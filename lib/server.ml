module O = Jsont.Object
module Smap = Map.Make (String)

(* Requests *)

type completion_ref = Ref_prompt of string | Ref_template of string

type op =
  | Discover
  | List_tools of string option
  | List_resources of string option
  | List_templates of string option
  | List_prompts of string option
  | Call of Tool.call
  | Read of string
  | Get_prompt of string * Prompt.args
  | Complete of { ref_ : completion_ref; arg : string; value : string; context : Prompt.args }
  | Listen of Proto.Filter.t

type params = {
  meta : Proto.Meta.t;
  inputs : (string * Json.t) list;
  state : string option;
  op : op;
}

type body = Request_body of string * params | Cancel_body of Id.t
type envelope = { jsonrpc : string; id : Id.t option; body : body }

(* Registry snapshots. Everything a request needs is precomputed here and
   published atomically, so readers on any domain never lock. *)

type features = { tools : bool; resources : bool; prompts : bool }

type registry = {
  features : features;
  tools : Tool.t list;
  resources : Resource.t list;
  templates : Resource.template list;
  prompts : Prompt.t list;
}

type pages = {
  tool_pages : string array;
  resource_pages : string array;
  template_pages : string array;
  prompt_pages : string array;
}

type snapshot = {
  reg : registry;
  resource_map : Resource.t Smap.t;
  prompt_map : Prompt.t Smap.t;
  pages : pages;
  discover : string;
  methods : string list;
  codec : envelope Jsont.t;
}

type config = {
  info : Proto.Implementation.t;
  server_info : Jsont.mem;
  server_meta : Json.t;
  instructions : string option;
  extensions : Json.t option;
  page_size : int option;
  list_ttl_ms : int;
  list_scope : Proto.cache_scope;
  state_key : string;
  state_ttl_ms : int;
}

(* Subscription hub: monotonically increasing change counters. Listeners
   compare against the last values they saw, so producers never block and
   bursts of changes coalesce into one notification per listener. *)
type hub = { tools_v : int; prompts_v : int; resources_v : int; uris : int Smap.t; closed : bool }

type t = {
  cfg : config;
  snapshot : snapshot Atomic.t;
  hub : hub Atomic.t;
  changed : Eio.Condition.t;
}

let supported_versions = [ Proto.version ]

(* Request codec *)

let meta_mem map = O.mem "_meta" Proto.Meta.codec map
let inputs_codec = Jsont.map ~dec:Smap.bindings (O.as_string_map Jsont.json)

let mrtr_mems map =
  map |> O.mem "inputResponses" inputs_codec ~dec_absent:[] |> O.opt_mem "requestState" Jsont.string

let plain op meta = { meta; inputs = []; state = None; op }

let listing op =
  O.map (fun meta cursor -> plain (op cursor) meta)
  |> meta_mem |> O.opt_mem "cursor" Jsont.string |> O.finish

let string_args = Jsont.map ~dec:Smap.bindings (O.as_string_map Jsont.string)

let completion_ref =
  let prompt = O.map (fun name -> Ref_prompt name) |> O.mem "name" Jsont.string |> O.finish in
  let template = O.map (fun uri -> Ref_template uri) |> O.mem "uri" Jsont.string |> O.finish in
  O.map Fun.id
  |> O.case_mem "type" Jsont.string ~tag_to_string:Fun.id
       O.Case.
         [
           make (map ~dec:Fun.id "ref/prompt" prompt);
           make (map ~dec:Fun.id "ref/resource" template);
         ]
  |> O.finish

let argument =
  O.map (fun name value -> (name, value))
  |> O.mem "name" Jsont.string |> O.mem "value" Jsont.string |> O.finish

let context = O.map Fun.id |> O.mem "arguments" string_args ~dec_absent:[] |> O.finish

let params_codecs reg ~completions =
  let discover = O.map (plain Discover) |> meta_mem |> O.finish in
  let call =
    O.map (fun meta inputs state call -> { meta; inputs; state; op = Call call })
    |> meta_mem |> mrtr_mems
    |> O.case_mem "name" Jsont.string ~tag_to_string:Fun.id (List.map Tool.case reg.tools)
    |> O.finish
  in
  let read =
    O.map (fun meta inputs state uri -> { meta; inputs; state; op = Read uri })
    |> meta_mem |> mrtr_mems |> O.mem "uri" Jsont.string |> O.finish
  in
  let get =
    O.map (fun meta inputs state name args -> { meta; inputs; state; op = Get_prompt (name, args) })
    |> meta_mem |> mrtr_mems |> O.mem "name" Jsont.string
    |> O.mem "arguments" string_args ~dec_absent:[]
    |> O.finish
  in
  let complete =
    O.map (fun meta ref_ (arg, value) context ->
        plain (Complete { ref_; arg; value; context }) meta)
    |> meta_mem |> O.mem "ref" completion_ref |> O.mem "argument" argument
    |> O.mem "context" context ~dec_absent:[]
    |> O.finish
  in
  let listen =
    O.map (fun meta filter -> plain (Listen filter) meta)
    |> meta_mem
    |> O.mem "notifications" Proto.Filter.codec
    |> O.finish
  in
  let when_ b l = if b then l else [] in
  List.concat
    [
      [ ("server/discover", discover); ("subscriptions/listen", listen) ];
      when_ reg.features.tools
        [ ("tools/list", listing (fun c -> List_tools c)); ("tools/call", call) ];
      when_ reg.features.resources
        [
          ("resources/list", listing (fun c -> List_resources c));
          ("resources/templates/list", listing (fun c -> List_templates c));
          ("resources/read", read);
        ];
      when_ reg.features.prompts
        [ ("prompts/list", listing (fun c -> List_prompts c)); ("prompts/get", get) ];
      when_ completions [ ("completion/complete", complete) ];
    ]

let cancelled = O.map Fun.id |> O.mem "requestId" Id.codec |> O.finish

let envelope_codec methods =
  let request (name, params) =
    O.Case.make
      (O.Case.map name
         ~dec:(fun p -> Request_body (name, p))
         (O.map Fun.id |> O.mem "params" params |> O.finish))
  in
  let cancel =
    O.Case.make
      (O.Case.map "notifications/cancelled"
         ~dec:(fun id -> Cancel_body id)
         (O.map Fun.id |> O.mem "params" cancelled |> O.finish))
  in
  O.map (fun jsonrpc id body -> { jsonrpc; id; body })
  |> O.mem "jsonrpc" Jsont.string |> O.opt_mem "id" Id.codec
  |> O.case_mem "method" Jsont.string ~tag_to_string:Fun.id (cancel :: List.map request methods)
  |> O.finish

(* Snapshot construction *)

let rec chunk n l =
  if List.length l <= n then [ l ]
  else List.filteri (fun i _ -> i < n) l :: chunk n (List.filteri (fun i _ -> i >= n) l)

let pages cfg ~key codec infos =
  let pages = match cfg.page_size with None -> [ infos ] | Some n -> chunk n infos in
  let last = List.length pages - 1 in
  let page i items : _ Proto.Page.t =
    {
      items;
      next_cursor = (if i < last then Some (string_of_int (i + 1)) else None);
      ttl_ms = cfg.list_ttl_ms;
      cache_scope = cfg.list_scope;
      meta = Some cfg.server_meta;
    }
  in
  Array.of_list
    (List.mapi
       (fun i items -> Json.encode (Proto.Page.codec ~items:key codec) (page i items))
       pages)

let sort what key l =
  let l = List.sort (fun a b -> String.compare (key a) (key b)) l in
  let rec check = function
    | a :: (b :: _ as rest) ->
        if key a = key b then invalid_arg (Printf.sprintf "Server: duplicate %s %S" what (key a));
        check rest
    | _ -> ()
  in
  check l;
  l

let has_completions reg =
  List.exists (fun p -> Prompt.completer p <> None) reg.prompts
  || List.exists (fun t -> Resource.completer t <> None) reg.templates

let build cfg reg =
  (* Deterministic order keeps list payloads byte-stable for client caches. *)
  let reg =
    {
      reg with
      tools = sort "tool" Tool.name reg.tools;
      resources = sort "resource" Resource.uri reg.resources;
      templates = sort "template" (fun t -> (Resource.template_info t).uri_template) reg.templates;
      prompts = sort "prompt" Prompt.name reg.prompts;
    }
  in
  let completions = has_completions reg in
  let methods = params_codecs reg ~completions in
  let capabilities : Proto.Server_caps.t =
    {
      tools = reg.features.tools;
      resources = reg.features.resources;
      prompts = reg.features.prompts;
      completions;
      extensions = cfg.extensions;
    }
  in
  let discover : Proto.Discover_result.t =
    {
      supported_versions;
      capabilities;
      instructions = cfg.instructions;
      ttl_ms = cfg.list_ttl_ms;
      cache_scope = cfg.list_scope;
      meta = Some cfg.server_meta;
    }
  in
  {
    reg;
    resource_map = Smap.of_list (List.map (fun r -> (Resource.uri r, r)) reg.resources);
    prompt_map = Smap.of_list (List.map (fun p -> (Prompt.name p, p)) reg.prompts);
    pages =
      {
        tool_pages = pages cfg ~key:"tools" Proto.Tool_info.codec (List.map Tool.info reg.tools);
        resource_pages =
          pages cfg ~key:"resources" Proto.Resource.codec (List.map Resource.info reg.resources);
        template_pages =
          pages cfg ~key:"resourceTemplates" Proto.Resource_template.codec
            (List.map Resource.template_info reg.templates);
        prompt_pages =
          pages cfg ~key:"prompts" Proto.Prompt_info.codec (List.map Prompt.info reg.prompts);
      };
    discover = Json.encode Proto.Discover_result.codec discover;
    methods = List.map fst methods;
    codec = envelope_codec methods;
  }

let v ?title ?description ?website_url ?(icons = []) ?instructions ?extensions ?tools ?resources
    ?templates ?prompts ?page_size ?(list_ttl_ms = 60_000) ?(list_scope = Proto.Public) ?state_key
    ?(state_ttl_ms = 600_000) ~name ~version () =
  if Option.fold page_size ~none:false ~some:(fun n -> n <= 0) then
    invalid_arg "Server.v: page_size must be positive";
  let info = Proto.Implementation.v ?title ?description ?website_url ~icons ~version name in
  let info_json = Result.get_ok (Jsont.Json.encode Proto.Implementation.codec info) in
  let server_info = Jsont.Json.mem (Jsont.Json.name Proto.key_server_info) info_json in
  let cfg =
    {
      info;
      server_info;
      server_meta = Jsont.Json.object' [ server_info ];
      instructions;
      extensions;
      page_size;
      list_ttl_ms;
      list_scope;
      state_key = (match state_key with Some k -> k | None -> State.random_key ());
      state_ttl_ms;
    }
  in
  let some = Option.value ~default:[] in
  let reg =
    {
      features =
        {
          tools = tools <> None;
          resources = resources <> None || templates <> None;
          prompts = prompts <> None;
        };
      tools = some tools;
      resources = some resources;
      templates = some templates;
      prompts = some prompts;
    }
  in
  {
    cfg;
    snapshot = Atomic.make (build cfg reg);
    hub =
      Atomic.make { tools_v = 0; prompts_v = 0; resources_v = 0; uris = Smap.empty; closed = false };
    changed = Eio.Condition.create ();
  }

let info t = t.cfg.info

(* Mutation *)

let rec update_hub t f =
  let h = Atomic.get t.hub in
  if Atomic.compare_and_set t.hub h (f h) then Eio.Condition.broadcast t.changed else update_hub t f

let rec update t f =
  let s = Atomic.get t.snapshot in
  if not (Atomic.compare_and_set t.snapshot s (build t.cfg (f s.reg))) then update t f

let change_tools t f =
  update t (fun r -> { r with tools = f r.tools; features = { r.features with tools = true } });
  update_hub t (fun h -> { h with tools_v = h.tools_v + 1 })

let set_tools t tools = change_tools t (fun _ -> tools)

let add_tool t tool =
  change_tools t (fun l -> tool :: List.filter (fun x -> Tool.name x <> Tool.name tool) l)

let remove_tool t name = change_tools t (List.filter (fun x -> Tool.name x <> name))

let set_prompts t prompts =
  update t (fun r -> { r with prompts; features = { r.features with prompts = true } });
  update_hub t (fun h -> { h with prompts_v = h.prompts_v + 1 })

let set_resources t ?templates resources =
  update t (fun r ->
      {
        r with
        resources;
        templates = Option.value templates ~default:r.templates;
        features = { r.features with resources = true };
      });
  update_hub t (fun h -> { h with resources_v = h.resources_v + 1 })

let resource_updated t uri =
  update_hub t (fun h ->
      { h with uris = Smap.update uri (fun v -> Some (1 + Option.value v ~default:0)) h.uris })

let shutdown t = update_hub t (fun h -> { h with closed = true })

(* Decoding *)

type request = { id : Id.t; method_ : string; params : params; snap : snapshot }
type incoming = Request of request | Cancel of Id.t | Ignore | Reject of Id.t option * Error.t

let version_probe =
  let meta = O.map Fun.id |> O.opt_mem Proto.key_protocol_version Jsont.string |> O.finish in
  O.map Option.join |> O.opt_mem "_meta" meta |> O.finish

let probe =
  O.map (fun id m v -> (id, m, Option.join v))
  |> O.opt_mem "id" Id.codec |> O.opt_mem "method" Jsont.string |> O.opt_mem "params" version_probe
  |> O.finish

let not_found m =
  if m = "initialize" then
    Error.v Error.method_not_found_code
      ("initialize is not supported: this server speaks MCP "
      ^ String.concat ", " supported_versions
      ^ " and expects per-request _meta")
  else Error.method_not_found m

(* Cold path: the typed decode failed, so find out why with cheap probes to
   pick the right JSON-RPC error and recover the id when possible. *)
let classify snap s msg =
  match Json.decode probe s with
  | Error _ -> (
      match Json.decode Jsont.ignore s with
      | Error _ -> Reject (None, Error.parse_error "Parse error")
      | Ok () -> Reject (None, Error.invalid_request "Invalid JSON-RPC message"))
  | Ok (None, _, _) -> Ignore
  | Ok (Some id, None, _) -> Reject (Some id, Error.invalid_request "Missing method")
  | Ok (Some id, Some m, _) when not (List.mem m snap.methods) -> Reject (Some id, not_found m)
  | Ok (Some id, Some _, Some v) when not (List.mem v supported_versions) ->
      Reject (Some id, Error.unsupported_version ~supported:supported_versions ~requested:v)
  | Ok (Some id, Some _, None) ->
      Reject
        ( Some id,
          Error.invalid_params ("Missing required _meta field " ^ Proto.key_protocol_version) )
  | Ok (Some id, Some _, Some _) -> Reject (Some id, Error.invalid_params msg)

let decode t s =
  let snap = Atomic.get t.snapshot in
  match Json.decode snap.codec s with
  | Ok { jsonrpc = "2.0"; id = Some id; body = Request_body (method_, params) } ->
      Request { id; method_; params; snap }
  | Ok { jsonrpc = "2.0"; id = None; body = Cancel_body id } -> Cancel id
  | Ok { jsonrpc = "2.0"; id = None; body = Request_body _ } -> Ignore
  | Ok { id; _ } -> Reject (id, Error.invalid_request "Invalid JSON-RPC message")
  | Error msg -> classify snap s msg

module Request = struct
  type t = request

  let id r = r.id
  let method_ r = r.method_
  let protocol_version r = r.params.meta.protocol_version

  let target r =
    match r.params.op with
    | Call c -> c.name
    | Read uri -> uri
    | Get_prompt (name, _) -> name
    | _ -> ""

  let header_args r = match r.params.op with Call c -> c.headers | _ -> []
end

(* Execution *)

let guard f =
  match f () with
  | r -> r
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e -> Error (Error.internal_error (Printexc.to_string e))

let with_server_info cfg = function
  | Some (Jsont.Object (mems, m)) -> Some (Jsont.Object (cfg.server_info :: mems, m))
  | _ -> Some cfg.server_meta

let step cfg ctx ~binding codec with_meta = function
  | Error e -> Error e
  | Ok (Input.Complete v) -> Ok (Json.encode codec (with_meta v))
  | Ok (Input.Required (r : Input.required)) -> (
      let caps = Ctx.client_caps ctx in
      match
        List.sort_uniq compare
          (List.filter_map (fun (_, q) -> Input.missing_capability caps q) r.requests)
      with
      | [] ->
          let state =
            Option.map (State.seal ~key:cfg.state_key ~binding ~ttl_ms:cfg.state_ttl_ms) r.state
          in
          Ok (Json.encode Proto.Input_required.codec { r with state })
      | missing -> Error (Error.missing_capability (Json.obj missing)))

let page pages = function
  | None -> Ok pages.(0)
  | Some c -> (
      match int_of_string_opt c with
      | Some i when i > 0 && i < Array.length pages -> Ok pages.(i)
      | _ -> Error (Error.invalid_params "Invalid cursor"))

let subscription_note sub method_ rest =
  Wire.notification method_
    ("{\"_meta\":{\"" ^ Proto.key_subscription_id ^ "\":" ^ sub ^ "}" ^ rest ^ "}")

let listen t ctx (filter : Proto.Filter.t) =
  let features = (Atomic.get t.snapshot).reg.features in
  let accepted : Proto.Filter.t =
    {
      tools = filter.tools && features.tools;
      prompts = filter.prompts && features.prompts;
      resources = filter.resources && features.resources;
      uris = (if features.resources then filter.uris else []);
    }
  in
  let sub = Id.to_wire (Ctx.id ctx) in
  let note = subscription_note sub in
  let emit = Ctx.emit ctx in
  (* Take the baseline before acknowledging so no change can fall between. *)
  let seen = ref (Atomic.get t.hub) in
  emit
    (note "notifications/subscriptions/acknowledged"
       (",\"notifications\":" ^ Json.encode Proto.Filter.codec accepted));
  let uri_v h uri = Smap.find_opt uri h.uris in
  Eio.Condition.loop_no_mutex t.changed (fun () ->
      let h = Atomic.get t.hub and s = !seen in
      seen := h;
      if accepted.tools && h.tools_v <> s.tools_v then
        emit (note "notifications/tools/list_changed" "");
      if accepted.prompts && h.prompts_v <> s.prompts_v then
        emit (note "notifications/prompts/list_changed" "");
      if accepted.resources && h.resources_v <> s.resources_v then
        emit (note "notifications/resources/list_changed" "");
      if h.uris != s.uris then
        List.iter
          (fun uri ->
            if uri_v h uri <> uri_v s uri then
              emit (note "notifications/resources/updated" (",\"uri\":" ^ Json.quote uri)))
          accepted.uris;
      if h.closed then Some () else None);
  "{\"resultType\":\"complete\",\"_meta\":{\"" ^ Proto.key_subscription_id ^ "\":" ^ sub ^ "}}"

let complete cfg snap ref_ arg value context =
  let completer =
    match ref_ with
    | Ref_prompt name ->
        Option.map
          (fun p -> Option.map (fun f -> f arg value context) (Prompt.completer p))
          (Smap.find_opt name snap.prompt_map)
    | Ref_template uri ->
        Option.map
          (fun t -> Option.map (fun f -> f arg value) (Resource.completer t))
          (List.find_opt
             (fun t -> (Resource.template_info t).uri_template = uri)
             snap.reg.templates)
  in
  match completer with
  | None -> Error (Error.invalid_params "Unknown completion reference")
  | Some values ->
      let values = Option.value values ~default:[] in
      let total = List.length values in
      let shown = List.filteri (fun i _ -> i < 100) values in
      Ok
        (Json.encode Proto.Completion.codec
           (Proto.Completion.v ~total ~has_more:(total > 100) shown, Some cfg.server_meta))

let run t snap ctx ~binding op =
  let cfg = t.cfg in
  let step codec with_meta f = step cfg ctx ~binding codec with_meta (guard f) in
  match op with
  | Discover -> Ok snap.discover
  | List_tools c -> page snap.pages.tool_pages c
  | List_resources c -> page snap.pages.resource_pages c
  | List_templates c -> page snap.pages.template_pages c
  | List_prompts c -> page snap.pages.prompt_pages c
  | Call c ->
      step Proto.Call_result.codec
        (fun (r : Proto.Call_result.t) -> { r with meta = with_server_info cfg r.meta })
        (fun () -> c.run ctx)
  | Read uri -> (
      let read_meta (r : Proto.Read_result.t) = { r with meta = with_server_info cfg r.meta } in
      match Smap.find_opt uri snap.resource_map with
      | Some r -> step Proto.Read_result.codec read_meta (fun () -> Resource.read r ctx)
      | None -> (
          let matching =
            List.find_map
              (fun tpl -> Option.map (fun vars -> (tpl, vars)) (Resource.match_template tpl uri))
              snap.reg.templates
          in
          match matching with
          | Some (tpl, vars) ->
              step Proto.Read_result.codec read_meta (fun () ->
                  Resource.read_template tpl ctx uri vars)
          | None ->
              Error
                (Error.invalid_params
                   ~data:(Json.obj [ ("uri", Json.str uri) ])
                   "Resource not found")))
  | Get_prompt (name, args) -> (
      match Smap.find_opt name snap.prompt_map with
      | None -> Error (Error.invalid_params ("Unknown prompt: " ^ name))
      | Some p -> (
          match Prompt.missing_args p args with
          | [] ->
              step Proto.Prompt_result.codec
                (fun (r : Proto.Prompt_result.t) -> { r with meta = with_server_info cfg r.meta })
                (fun () -> Prompt.get p ctx args)
          | missing ->
              Error
                (Error.invalid_params ("Missing required arguments: " ^ String.concat ", " missing))
          ))
  | Complete { ref_; arg; value; context } -> complete cfg snap ref_ arg value context
  | Listen filter -> Ok (listen t ctx filter)

let execute t ?principal ~emit req =
  let p = req.params in
  let fail e = (Wire.error ~id:req.id e, Error.http_status e) in
  if not (List.mem p.meta.protocol_version supported_versions) then
    fail
      (Error.unsupported_version ~supported:supported_versions ~requested:p.meta.protocol_version)
  else
    let binding = State.binding ~method_:req.method_ ~target:(Request.target req) ~principal in
    let verified =
      match p.state with
      | None -> Ok None
      | Some token -> Result.map Option.some (State.open_ ~key:t.cfg.state_key ~binding token)
    in
    match verified with
    | Error msg -> fail (Error.invalid_params msg)
    | Ok state -> (
        let ctx = Ctx.make ~id:req.id ~meta:p.meta ~inputs:p.inputs ~state ~principal ~emit in
        match run t req.snap ctx ~binding p.op with
        | Ok body -> (Wire.result req.id body, 200)
        | Error e -> fail e)
