module O = Jsont.Object

type error = Remote of Error.t | Local of string

(* A transport sends one serialized request and blocks until its response,
   handing request-scoped notifications to [on_note]. Cancelling the calling
   fiber cancels the request in the transport's own way. [post] sends a
   message that has no response, and [requests] takes the handler for
   requests the server sends outside any request's stream. *)
type transport = {
  send : headers:(string * string) list -> id:Id.t -> string -> on_note:(string -> unit) -> string;
  post : headers:(string * string) list -> string -> unit;
  requests : (string -> unit) -> unit;
}

let transport ?(post = fun ~headers:_ _ -> ()) ?(requests = ignore) send = { send; post; requests }

type protocol = [ `Modern | `Legacy | `Auto ]
type input_handler = string -> Proto.Input_request.t -> Json.t option
type cached = { expires : float; value : Json.t list }

type t = {
  transport : transport;
  protocol : protocol;
  info : Proto.Implementation.t option;
  capabilities : Proto.Client_caps.t;
  meta : (string * Json.t) list;  (** The [_meta] every 2026-07-28 request carries. *)
  on_input : input_handler;
  next_id : int Atomic.t;
  cache : (string, cached) Hashtbl.t;
  generations : (string, int) Hashtbl.t;
  mutable tools : Proto.Tool_info.t list;
  mutable version : string option;  (** The revision in use, once negotiated. *)
  negotiation : Eio.Mutex.t;
}

let encode codec v = Result.get_ok (Jsont.Json.encode codec v)

let pp_error ppf = function
  | Remote e -> Error.pp ppf e
  | Local msg -> Format.fprintf ppf "local error: %s" msg

(* Request headers for Streamable HTTP. Line transports ignore them. *)

let header_safe v =
  v <> ""
  && v.[0] <> ' '
  && v.[0] <> '\t'
  && v.[String.length v - 1] <> ' '
  && v.[String.length v - 1] <> '\t'
  && String.for_all (fun c -> c = '\t' || (c >= ' ' && c <= '~')) v
  && not (String.starts_with ~prefix:"=?base64?" v && String.ends_with ~suffix:"?=" v)

let header_value v = if header_safe v then v else "=?base64?" ^ Base64.encode_string v ^ "?="

(* [x-mcp-header] annotations of a tool: (property path, header name).
   [None] when an annotation breaks the spec rules, in which case the tool
   must not be offered at all. *)
let header_params (tool : Proto.Tool_info.t) =
  let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None in
  let rec walk path schema =
    List.concat_map
      (fun (name, prop) ->
        let path = path @ [ name ] in
        let here =
          match str (Json.find "x-mcp-header" prop) with
          | None -> []
          | Some h -> [ (path, h, str (Json.find "type" prop)) ]
        in
        here @ walk path prop)
      (Option.fold ~none:[] ~some:Json.mems (Json.find "properties" schema))
  in
  (* Annotations off the chain of [properties] (under items, oneOf, $defs...)
     are not reachable statically and make the whole tool invalid. *)
  let rec count = function
    | Jsont.Object (mems, _) ->
        List.fold_left
          (fun n ((k, _), v) -> n + (if k = "x-mcp-header" then 1 else 0) + count v)
          0 mems
    | Jsont.Array (l, _) -> List.fold_left (fun n v -> n + count v) 0 l
    | _ -> 0
  in
  let found = walk [] tool.input_schema in
  let names = List.map (fun (_, h, _) -> String.lowercase_ascii h) found in
  let valid (_, h, ty) =
    h <> "" && String.for_all Schema.is_tchar h
    && List.mem ty [ Some "string"; Some "integer"; Some "boolean" ]
  in
  if
    List.for_all valid found
    && List.length (List.sort_uniq compare names) = List.length names
    && count tool.input_schema = List.length found
  then Some (List.map (fun (path, h, _) -> (path, h)) found)
  else None

let param_headers t name args =
  match List.find_opt (fun (tool : Proto.Tool_info.t) -> tool.name = name) t.tools with
  | None -> []
  | Some tool ->
      Option.value (header_params tool) ~default:[]
      |> List.filter_map (fun (path, h) ->
          let value =
            match Json.find_path path args with
            | Some (Jsont.String (s, _)) -> Some s
            | Some (Jsont.Number (f, _)) when Float.is_integer f -> Some (Printf.sprintf "%.0f" f)
            | Some (Jsont.Bool (b, _)) -> Some (string_of_bool b)
            | _ -> None
          in
          Option.map (fun v -> ("Mcp-Param-" ^ h, header_value v)) value)

let headers t method_ params =
  let name key =
    match Json.find key params with
    | Some (Jsont.String (s, _)) -> [ ("Mcp-Name", header_value s) ]
    | _ -> []
  in
  let base = [ ("MCP-Protocol-Version", Proto.version); ("Mcp-Method", method_) ] in
  match method_ with
  | "tools/call" ->
      let args = Option.value (Json.find "arguments" params) ~default:Json.empty in
      let tool = match Json.find "name" params with Some (Jsont.String (s, _)) -> s | _ -> "" in
      base @ name "name" @ param_headers t tool args
  | "prompts/get" -> base @ name "name"
  | "resources/read" -> base @ name "uri"
  | _ -> base

(* Responses *)

type response = { jsonrpc : string; id : Id.t; result : Json.t option; error : Error.t option }

let response_codec =
  O.map (fun jsonrpc id result error -> { jsonrpc; id; result; error })
  |> O.mem "jsonrpc" Jsont.string |> O.mem "id" Id.codec |> O.opt_mem "result" Jsont.json
  |> O.opt_mem "error" Error.codec |> O.finish

let is_legacy version = version <> Proto.version
let legacy_headers version = [ ("MCP-Protocol-Version", version) ]

let server_request =
  O.map (fun id method_ params -> (id, { Proto.Input_request.method_; params }))
  |> O.mem "id" Id.codec |> O.mem "method" Jsont.string
  |> O.mem "params" Jsont.json ~dec_absent:Json.empty
  |> O.finish

(* Earlier revisions put input requests to the client as requests of their
   own, on the stream of the request that needs them or, over stdio, on
   their own. They are answered through [on_input], keyed by request id. *)
let answer t ~version (id, (q : Proto.Input_request.t)) =
  let input =
    match q.method_ with
    | "ping" -> Some Json.empty
    | _ -> (
        match t.on_input (Id.to_wire id) q with
        | a -> a
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception _ -> None)
  in
  let reply =
    match input with
    | Some a -> Wire.result id (Json.to_string a)
    | None -> Wire.error ~id (Error.internal_error ("No answer for " ^ q.method_))
  in
  t.transport.post ~headers:(legacy_headers version) (Wire.to_string reply)

let answer_raw t raw =
  match Json.decode server_request raw with
  | Ok r ->
      let version = Option.value t.version ~default:(List.hd Proto.legacy_versions) in
      answer t ~version r
  | Error _ -> ()

(* One request at a given revision. A 2026-07-28 request describes itself in
   [_meta]; one of an earlier revision leaves that to its session. *)
let exchange t ~version ?on_note method_ params =
  let id = Id.Int (Atomic.fetch_and_add t.next_id 1) in
  let progress = if on_note <> None then [ ("progressToken", Id.to_json id) ] else [] in
  let meta = if is_legacy version then progress else progress @ t.meta in
  let params = Json.obj (if meta = [] then params else ("_meta", Json.obj meta) :: params) in
  let body =
    String.concat ""
      [
        "{\"jsonrpc\":\"2.0\",\"id\":";
        Id.to_wire id;
        ",\"method\":";
        Json.quote method_;
        ",\"params\":";
        Json.to_string params;
        "}";
      ]
  in
  let on_note = Option.value on_note ~default:ignore in
  let on_note, headers =
    if is_legacy version then
      ( (fun raw ->
          match Json.decode server_request raw with
          | Ok r -> answer t ~version r
          | Error _ -> on_note raw),
        legacy_headers version )
    else (on_note, headers t method_ params)
  in
  match t.transport.send ~headers ~id body ~on_note with
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e -> Error (Local (Printexc.to_string e))
  | raw -> (
      match Json.decode response_codec raw with
      | Error msg -> Error (Local ("Malformed response: " ^ msg))
      | Ok r when r.jsonrpc <> "2.0" -> Error (Local "Invalid response JSON-RPC version")
      | Ok r when r.id <> id -> Error (Local "Response id does not match request")
      | Ok { error = Some e; result = None; _ } -> Error (Remote e)
      | Ok { result = Some r; error = None; _ } -> Ok r
      | Ok _ -> Error (Local "Response must have exactly one of result or error"))

let typed codec = function
  | Error _ as e -> e
  | Ok json -> Result.map_error (fun m -> Local m) (Json.decode_json codec json)

let default_info = Proto.Implementation.v ~version:"0.1.0" "mcp-sdk"

let initialize t =
  let requested = List.hd Proto.legacy_versions in
  let params =
    [
      ("protocolVersion", Json.str requested);
      ("capabilities", encode Proto.Client_caps.codec t.capabilities);
      ("clientInfo", encode Proto.Implementation.codec (Option.value t.info ~default:default_info));
    ]
  in
  match typed Proto.Initialize_result.codec (exchange t ~version:requested "initialize" params) with
  | Error e -> Error e
  | Ok r when List.mem r.protocol_version Proto.legacy_versions -> (
      match
        t.transport.post
          ~headers:(legacy_headers r.protocol_version)
          {|{"jsonrpc":"2.0","method":"notifications/initialized"}|}
      with
      | () -> Ok r.protocol_version
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e -> Error (Local (Printexc.to_string e)))
  | Ok r -> Error (Local ("Unsupported protocol version " ^ r.protocol_version))

(* [`Auto] asks with 2026-07-28 first: a server of an earlier revision
   answers [server/discover] with an error, as it knows neither the method
   nor a request without a session. *)
let negotiate t =
  match t.protocol with
  | `Modern -> Ok Proto.version
  | `Legacy -> initialize t
  | `Auto -> (
      match exchange t ~version:Proto.version "server/discover" [] with
      | Ok _ -> Ok Proto.version
      | Error (Remote _) -> initialize t
      | Error e -> Error e)

(* Negotiated once, on first use. A failure is not kept, so a later request
   tries again. *)
let protocol_version t =
  match t.version with
  | Some v -> Ok v
  | None ->
      Eio.Mutex.use_ro t.negotiation (fun () ->
          match t.version with
          | Some v -> Ok v
          | None ->
              let r = negotiate t in
              Result.iter (fun v -> t.version <- Some v) r;
              r)

let send t ?on_note method_ params =
  Result.bind (protocol_version t) (fun version -> exchange t ~version ?on_note method_ params)

let connect ?info ?(capabilities = Proto.Client_caps.none) ?(on_input = fun _ _ -> None)
    ?(protocol = `Modern) transport =
  let meta =
    (Proto.key_protocol_version, Json.str Proto.version)
    :: (Proto.key_client_capabilities, encode Proto.Client_caps.codec capabilities)
    :: Option.fold info ~none:[] ~some:(fun i ->
        [ (Proto.key_client_info, encode Proto.Implementation.codec i) ])
  in
  let t =
    {
      transport;
      protocol;
      info;
      capabilities;
      meta;
      on_input;
      next_id = Atomic.make 1;
      cache = Hashtbl.create 8;
      generations = Hashtbl.create 8;
      tools = [];
      version = (if protocol = `Modern then Some Proto.version else None);
      negotiation = Eio.Mutex.create ();
    }
  in
  transport.requests (answer_raw t);
  t

let result_type r =
  match Json.find "resultType" r with Some (Jsont.String (s, _)) -> s | _ -> "complete"

(* Multi round-trip requests: fulfil input requests and retry the original
   request with the responses and the exact requestState echoed back. *)
let rec request t ?on_note ?(rounds = 8) method_ params =
  match send t ?on_note method_ params with
  | Error _ as e -> e
  | Ok r -> (
      match result_type r with
      | "complete" -> Ok r
      | "input_required" when rounds > 0 -> (
          match Json.decode_json Proto.Input_required.codec r with
          | Error msg -> Error (Local msg)
          | Ok ir -> (
              let answers = List.map (fun (key, req) -> (key, t.on_input key req)) ir.requests in
              match List.find_opt (fun (_, a) -> a = None) answers with
              | Some (key, _) -> Error (Local ("No answer for input request " ^ key))
              | None ->
                  let responses = Json.obj (List.map (fun (k, a) -> (k, Option.get a)) answers) in
                  let params =
                    List.filter (fun (k, _) -> k <> "inputResponses" && k <> "requestState") params
                    @ [ ("inputResponses", responses) ]
                    @ Option.fold ~none:[]
                        ~some:(fun s -> [ ("requestState", Json.str s) ])
                        ir.state
                  in
                  request t ?on_note ~rounds:(rounds - 1) method_ params))
      | "input_required" -> Error (Local "Too many input rounds")
      | other -> Error (Local ("Unknown resultType " ^ other)))

(* Cacheable lists: every page, following cursors until exhausted (a cursor
   seen twice is a server bug, not a reason to loop), cached for the
   smallest ttlMs among the pages. *)
let generation t method_ = Option.value (Hashtbl.find_opt t.generations method_) ~default:0

let fetch_all t method_ key =
  let now = Unix.gettimeofday () in
  match Hashtbl.find_opt t.cache method_ with
  | Some c when c.expires > now -> Ok c.value
  | _ ->
      let started = generation t method_ in
      let page = Proto.Page.codec ~items:key Jsont.json in
      let rec go seen acc ttl cursor =
        let params = Option.fold ~none:[] ~some:(fun c -> [ ("cursor", Json.str c) ]) cursor in
        match typed page (send t method_ params) with
        | Error _ as e -> e
        | Ok (p : Json.t Proto.Page.t) -> (
            let acc = List.rev_append p.items acc and ttl = min ttl p.ttl_ms in
            match p.next_cursor with
            | Some c when not (List.mem c seen) -> go (c :: seen) acc ttl (Some c)
            | Some _ -> Error (Local "Pagination cursor loop")
            | None ->
                let value = List.rev acc in
                if ttl > 0 && generation t method_ = started then
                  Hashtbl.replace t.cache method_ { expires = now +. (float ttl /. 1000.); value };
                Ok value)
      in
      go [] [] max_int None

let list_all t method_ key item =
  Result.bind (fetch_all t method_ key) (fun items ->
      List.fold_right
        (fun j acc ->
          Result.bind acc (fun l ->
              Result.map
                (fun x -> x :: l)
                (Result.map_error (fun m -> Local m) (Json.decode_json item j))))
        items (Ok []))

let invalidate t method_ =
  Hashtbl.replace t.generations method_ (generation t method_ + 1);
  Hashtbl.remove t.cache method_

let discover t = typed Proto.Discover_result.codec (send t "server/discover" [])

let list_tools t =
  Result.map
    (fun tools ->
      let tools = List.filter (fun tool -> header_params tool <> None) tools in
      t.tools <- tools;
      tools)
    (list_all t "tools/list" "tools" Proto.Tool_info.codec)

let progress_note on_progress raw =
  match Json.decode Jsont.json raw with
  | Ok note -> (
      match (Json.find "method" note, Json.find_path [ "params"; "progress" ] note) with
      | Some (Jsont.String ("notifications/progress", _)), Some (Jsont.Number (p, _)) ->
          let num = function Some (Jsont.Number (f, _)) -> Some f | _ -> None in
          let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None in
          on_progress
            ?total:(num (Json.find_path [ "params"; "total" ] note))
            ?message:(str (Json.find_path [ "params"; "message" ] note))
            p
      | _ -> ())
  | Error _ -> ()

(* A header mismatch means our view of the tool's [x-mcp-header]
   annotations is stale: refresh it and retry once. *)
let call_tool t ?on_progress name arguments =
  let on_note = Option.map progress_note on_progress in
  let call () =
    request t ?on_note "tools/call" [ ("name", Json.str name); ("arguments", arguments) ]
  in
  let result =
    match call () with
    | Error (Remote { code; _ }) when code = Error.header_mismatch_code ->
        invalidate t "tools/list";
        Result.bind (list_tools t) (fun _ -> call ())
    | r -> r
  in
  typed Proto.Call_result.codec result

let list_resources t = list_all t "resources/list" "resources" Proto.Resource.codec

let list_templates t =
  list_all t "resources/templates/list" "resourceTemplates" Proto.Resource_template.codec

let read_resource t uri =
  typed Proto.Read_result.codec (request t "resources/read" [ ("uri", Json.str uri) ])

let list_prompts t = list_all t "prompts/list" "prompts" Proto.Prompt_info.codec

let get_prompt t ?(args = []) name =
  typed Proto.Prompt_result.codec
    (request t "prompts/get"
       [
         ("name", Json.str name);
         ("arguments", Json.obj (List.map (fun (k, v) -> (k, Json.str v)) args));
       ])

let complete t ~ref_ ~arg ~value =
  let ref_ =
    match ref_ with
    | `Prompt name -> Json.obj [ ("type", Json.str "ref/prompt"); ("name", Json.str name) ]
    | `Template uri -> Json.obj [ ("type", Json.str "ref/resource"); ("uri", Json.str uri) ]
  in
  typed Proto.Completion.codec
    (send t "completion/complete"
       [
         ("ref", ref_); ("argument", Json.obj [ ("name", Json.str arg); ("value", Json.str value) ]);
       ])
  |> Result.map fst

(* Blocks delivering notifications until the server ends the subscription
   or the calling fiber is cancelled. List notifications also invalidate the
   matching cached lists. *)
let listen t (filter : Proto.Filter.t) f =
  let on_note raw =
    match Json.decode Jsont.json raw with
    | Error _ -> ()
    | Ok note ->
        let method_ =
          match Json.find "method" note with Some (Jsont.String (m, _)) -> m | _ -> ""
        in
        (match method_ with
        | "notifications/tools/list_changed" -> invalidate t "tools/list"
        | "notifications/prompts/list_changed" -> invalidate t "prompts/list"
        | "notifications/resources/list_changed" ->
            invalidate t "resources/list";
            invalidate t "resources/templates/list"
        | _ -> ());
        f method_ (Option.value (Json.find "params" note) ~default:Json.empty)
  in
  let notifications = encode Proto.Filter.codec filter in
  match protocol_version t with
  | Ok v when is_legacy v -> Error (Local ("subscriptions/listen needs revision " ^ Proto.version))
  | _ ->
      Result.map ignore
        (send t ~on_note "subscriptions/listen" [ ("notifications", notifications) ])

(* What a transport's reader hands to the fiber waiting on a request: its
   notifications, for that fiber to run as callbacks, then its response. The
   reader never waits on it, so it can always route the responses callbacks
   are waiting for. The stream is unbounded, but holds at most [max_queued]
   bytes of notifications (any single one is admitted) and one response.
   Past that, a request fails without running further callbacks, while
   notifications for no request are dropped. *)
module Inbox = struct
  type event = Note of string | Response of (string, string) result

  type t = {
    events : event Eio.Stream.t;
    max_queued : int;
    overflow : [ `Fail | `Drop ];
    mutable queued : int;
    mutable answered : bool;
    mutable overflowed : bool;
  }

  let overflow_message = "Notification backlog exceeded"

  let create ~max_queued ~overflow =
    {
      events = Eio.Stream.create max_int;
      max_queued;
      overflow;
      queued = 0;
      answered = false;
      overflowed = false;
    }

  let respond t result =
    if not t.answered then (
      t.answered <- true;
      Eio.Stream.add t.events (Response result))

  let note t raw =
    let size = String.length raw in
    if t.answered then ()
    else if t.queued > 0 && t.queued + size > t.max_queued then
      match t.overflow with
      | `Fail ->
          t.overflowed <- true;
          respond t (Error overflow_message)
      | `Drop -> ()
    else (
      t.queued <- t.queued + size;
      Eio.Stream.add t.events (Note raw))

  let rec receive t ~on_note =
    match Eio.Stream.take t.events with
    | Note _ when t.overflowed -> failwith overflow_message
    | Note raw ->
        t.queued <- t.queued - String.length raw;
        on_note raw;
        receive t ~on_note
    | Response (Ok raw) -> raw
    | Response (Error msg) -> failwith msg
end

(* Newline-delimited transport over a byte stream, e.g. a server's stdio.
   Requests are multiplexed: one reader fiber routes responses by id and
   notifications by progress token or subscription id. *)

let route_probe =
  let meta = O.map Fun.id |> O.opt_mem Proto.key_subscription_id Id.codec |> O.finish in
  let params =
    O.map (fun token sub -> match token with Some _ -> token | None -> Option.join sub)
    |> O.opt_mem "progressToken" Id.codec
    |> O.opt_mem "_meta" meta |> O.finish
  in
  O.map (fun id method_ target -> (id, method_, Option.join target))
  |> O.opt_mem "id" Id.codec |> O.opt_mem "method" Jsont.string |> O.opt_mem "params" params
  |> O.finish

let is_response raw =
  match Json.decode route_probe raw with Ok (Some _, None, _) -> true | _ -> false

let line_transport ~sw ?(on_notification = ignore) ?(max_message = 1 lsl 26)
    ?(max_queued = 1 lsl 22) source sink =
  let pending : (Id.t, Inbox.t) Hashtbl.t = Hashtbl.create 64 in
  let closed = ref None in
  (* Unbounded, yet it holds at most a request and a cancellation per send
     in flight, and a bound could hold up the protected cancellation. *)
  let out = Eio.Stream.create max_int in
  let unsolicited = Inbox.create ~max_queued ~overflow:`Drop in
  let on_request = ref ignore in
  let close message =
    if !closed = None then (
      closed := Some message;
      Hashtbl.iter (fun _ inbox -> Inbox.respond inbox (Error message)) pending;
      Hashtbl.reset pending)
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      ignore (Inbox.receive unsolicited ~on_note:on_notification);
      `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      match
        Eio.Buf_write.with_flow sink (fun w ->
            let rec loop () =
              Eio.Buf_write.string w (Eio.Stream.take out);
              Eio.Buf_write.char w '\n';
              loop ()
            in
            loop ())
      with
      | result -> result
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e ->
          close (Printexc.to_string e);
          `Stop_daemon);
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let input = Eio.Buf_read.of_flow source ~max_size:max_message in
      let rec loop () =
        let line = Eio.Buf_read.line input in
        (match Json.decode route_probe line with
        | Ok (Some id, None, _) ->
            Option.iter
              (fun inbox ->
                Hashtbl.remove pending id;
                Inbox.respond inbox (Ok line))
              (Hashtbl.find_opt pending id)
        | Ok (None, Some _, Some id) when Hashtbl.mem pending id ->
            Inbox.note (Hashtbl.find pending id) line
        | Ok (None, Some _, _) -> Inbox.note unsolicited line
        | Ok (Some _, Some _, _) ->
            (* A request of the server's own, answered on a fiber of its own
               so the reader never waits for it. *)
            Eio.Fiber.fork ~sw (fun () ->
                try !on_request line with Eio.Cancel.Cancelled _ as e -> raise e | _ -> ())
        | _ -> ());
        loop ()
      in
      match loop () with
      | () -> `Stop_daemon
      | exception End_of_file ->
          close "Connection closed";
          `Stop_daemon
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e ->
          close (Printexc.to_string e);
          `Stop_daemon);
  let send ~headers:_ ~id body ~on_note =
    Option.iter failwith !closed;
    let inbox = Inbox.create ~max_queued ~overflow:`Fail in
    Hashtbl.replace pending id inbox;
    match
      Eio.Stream.add out body;
      Inbox.receive inbox ~on_note
    with
    | raw -> raw
    | exception e ->
        if Hashtbl.mem pending id then (
          Hashtbl.remove pending id;
          Eio.Cancel.protect (fun () ->
              Eio.Stream.add out
                ("{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\",\"params\":{\"requestId\":"
               ^ Id.to_wire id ^ "}}")));
        raise e
  in
  let post ~headers:_ body =
    Option.iter failwith !closed;
    Eio.Stream.add out body
  in
  { send; post; requests = (fun f -> on_request := f) }
