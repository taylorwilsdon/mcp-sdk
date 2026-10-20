(* Streamable HTTP (2026-07-28) request handling. [Http1] presents each
   httpun request as an exchange, which keeps this module free of httpun and
   lets an application present requests from any other HTTP server. *)

open Mcp

type writer = {
  write : string -> unit;
  flush : unit -> [ `Written | `Closed ];
  close : unit -> unit;
}

type t = {
  meth : string;
  header : string -> string option;
  read_body : max:int -> string option;  (** [None] when over [max]. *)
  respond : status:int -> (string * string) list -> Wire.t -> unit;
  stream : status:int -> (string * string) list -> writer;
}

type config = {
  allow_origin : string -> bool;
  authenticate : (string -> string option) -> (string option, string) result;
  max_body : int;
  keepalive : float;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
}

(* Without an explicit policy only loopback origins may call us, which is
   what defeats DNS rebinding against local servers. *)
let loopback_origin origin =
  match String.index_opt origin ':' with
  | None -> false
  | Some i ->
      let rest = String.sub origin (i + 1) (String.length origin - i - 1) in
      let host =
        if String.starts_with ~prefix:"//" rest then String.sub rest 2 (String.length rest - 2)
        else rest
      in
      let host =
        if String.starts_with ~prefix:"[" host then
          match String.index_opt host ']' with Some j -> String.sub host 0 (j + 1) | None -> host
        else match String.index_opt host ':' with Some j -> String.sub host 0 j | None -> host
      in
      List.mem host [ "localhost"; "127.0.0.1"; "[::1]" ]

let config ?(allow_origin = loopback_origin) ?(authenticate = fun _ -> Ok None)
    ?(max_body = 1 lsl 22) ?(keepalive = 15.0) ~clock () =
  {
    allow_origin;
    authenticate;
    max_body;
    keepalive;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
  }

(* Collect a request body delivered through httpun style callbacks. *)
let read_body ~schedule_read ~close ~max =
  let buf = Buffer.create 4096 in
  let result, resolve = Eio.Promise.create () in
  let rec next () =
    schedule_read
      ~on_eof:(fun () -> Eio.Promise.resolve resolve (Some (Buffer.contents buf)))
      ~on_read:(fun bs ~off ~len ->
        if Buffer.length buf + len > max then (
          close ();
          Eio.Promise.resolve resolve None)
        else (
          Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
          next ()))
  in
  next ();
  Eio.Promise.await result

let json_headers = [ ("content-type", "application/json") ]

let sse_headers =
  [
    ("content-type", "text/event-stream"); ("cache-control", "no-cache"); ("x-accel-buffering", "no");
  ]

let reply x ?id e = x.respond ~status:(Error.http_status e) json_headers (Wire.error ?id e)

(* Header values may carry UTF-8 or unsafe bytes as =?base64?...?= *)
let decode_header_value v =
  let n = String.length v in
  if n >= 11 && String.starts_with ~prefix:"=?base64?" v && String.ends_with ~suffix:"?=" v then
    Result.to_option (Base64.decode (String.sub v 9 (n - 11)))
  else if String.for_all (fun c -> c = '\t' || (c >= ' ' && c <= '~')) v then Some v
  else None

let same_value ~numeric header body =
  header = body
  || numeric
     &&
     match (float_of_string_opt header, float_of_string_opt body) with
     | Some a, Some b -> a = b
     | _ -> false

let validate_headers x req =
  let open Server.Request in
  let check name expected ~numeric =
    match Option.map decode_header_value (x.header name) with
    | None -> Error (Error.header_mismatch ("missing " ^ name))
    | Some None -> Error (Error.header_mismatch ("invalid characters in " ^ name))
    | Some (Some v) when same_value ~numeric v expected -> Ok ()
    | Some (Some v) ->
        Error
          (Error.header_mismatch
             (Printf.sprintf "%s header value %S does not match body value %S" name v expected))
  in
  let ( let* ) = Result.bind in
  let* () = check "mcp-protocol-version" (protocol_version req) ~numeric:false in
  let* () = check "mcp-method" (method_ req) ~numeric:false in
  let* () =
    match method_ req with
    | "tools/call" | "resources/read" | "prompts/get" ->
        check "mcp-name" (target req) ~numeric:false
    | _ -> Ok ()
  in
  List.fold_left
    (fun acc (name, value, numeric) ->
      let* () = acc in
      check ("mcp-param-" ^ String.lowercase_ascii name) value ~numeric)
    (Ok ()) (header_args req)

(* A 2025-11-25 session is the client's own [initialize] parameters, so its
   id is simply their encoding: the client sends them back with every request
   and the server keeps nothing. *)
let b64 = Base64.uri_safe_alphabet

let session_id meta =
  Base64.encode_string ~pad:false ~alphabet:b64 (Json.encode Proto.Meta.codec meta)

let session_of_id id =
  match Base64.decode ~pad:false ~alphabet:b64 id with
  | Ok s -> Result.to_option (Json.decode Proto.Meta.codec s)
  | Error _ -> None

let event w wire =
  w.write "data: ";
  Wire.iter w.write wire;
  w.write "\n\n"

(* Respond with JSON unless the handler emits before finishing, in which case
   switch to an SSE stream scoped to this request. Each event waits for its
   flush, so a peer that reads slowly holds back the handler instead of
   growing the output buffer. The stream closing is the cancellation signal,
   noticed on every flush and by periodic keepalives. *)
let execute cfg server ~scope x ~headers ?principal req =
  let gone, set_gone = Eio.Promise.create () in
  let flush w = if w.flush () = `Closed then ignore (Eio.Promise.try_resolve set_gone ()) in
  Eio.Switch.run @@ fun sw ->
  let sse = ref None in
  let open_stream () =
    match !sse with
    | Some w -> w
    | None ->
        let w = x.stream ~status:200 (headers @ sse_headers) in
        sse := Some w;
        Eio.Fiber.fork_daemon ~sw (fun () ->
            let rec beat () =
              Eio.Time.sleep cfg.clock cfg.keepalive;
              w.write ":\n\n";
              flush w;
              beat ()
            in
            beat ());
        w
  in
  let emit wire =
    if not (Eio.Promise.is_resolved gone) then begin
      let w = open_stream () in
      event w wire;
      flush w
    end
  in
  Eio.Fiber.first
    (fun () ->
      let wire, status = Server.execute server ?principal ~scope ~emit req in
      match !sse with
      | None -> x.respond ~status (headers @ json_headers) wire
      | Some w ->
          event w wire;
          w.close ())
    (fun () -> Eio.Promise.await gone);
  Option.iter (fun w -> if Eio.Promise.is_resolved gone then w.close ()) !sse

(* A body version the [MCP-Protocol-Version] header does not repeat is a
   header mismatch, whether or not the server supports either. *)
let version_mismatch x (e : Error.t) =
  match (Option.bind e.data (Json.find "requested"), x.header "mcp-protocol-version") with
  | Some (Jsont.String (requested, _)), Some header
    when e.code = Error.unsupported_version_code && requested <> header ->
      Error.header_mismatch
        (Printf.sprintf "mcp-protocol-version header value %S does not match body value %S" header
           requested)
  | _ -> e

let dispatch cfg server ~scope x ?principal ?session body =
  match Server.decode server ?session body with
  | Server.Reject (id, e) -> reply x ?id (version_mismatch x e)
  | Ignore | Cancel _ -> x.respond ~status:202 [] []
  | Request req -> (
      (* Earlier revisions mirror nothing into headers. *)
      let legacy = Server.Request.protocol_version req <> Proto.version in
      match if legacy then Ok () else validate_headers x req with
      | Error e -> reply x ~id:(Server.Request.id req) e
      | Ok () ->
          let headers =
            Option.fold (Server.Request.session req) ~none:[] ~some:(fun s ->
                [ ("mcp-session-id", session_id s) ])
          in
          execute cfg server ~scope x ~headers ?principal req)

let post cfg server ~scope x =
  match Option.map cfg.allow_origin (x.header "origin") with
  | Some false ->
      x.respond ~status:403 json_headers (Wire.error (Error.invalid_request "Origin not allowed"))
  | _ -> (
      match cfg.authenticate x.header with
      | Error challenge -> x.respond ~status:401 [ ("www-authenticate", challenge) ] []
      | Ok principal -> (
          match x.read_body ~max:cfg.max_body with
          | None ->
              x.respond ~status:413 json_headers
                (Wire.error (Error.invalid_request "Request body too large"))
          | Some body -> (
              match Option.map session_of_id (x.header "mcp-session-id") with
              | Some None -> reply x (Error.invalid_request "Invalid Mcp-Session-Id")
              | session ->
                  dispatch cfg server ~scope x ?principal ?session:(Option.join session) body)))

exception Connection_closed

(* Requests are fibers of their connection's switch. When the connection
   ends, whatever is still running belongs to a peer that has gone away, so
   it is cancelled rather than left to finish into a dead socket. *)
let with_connection serve =
  match
    Eio.Switch.run @@ fun sw ->
    serve sw;
    Eio.Switch.fail sw Connection_closed
  with
  | () | (exception Connection_closed) -> ()

let handle cfg server ~scope x =
  match x.meth with
  | "POST" -> post cfg server ~scope x
  | _ -> x.respond ~status:405 [ ("allow", "POST") ] []
