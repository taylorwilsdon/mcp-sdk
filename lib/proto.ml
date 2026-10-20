(* Wire types of MCP 2026-07-28 with codecs in both directions. Unknown
   members are skipped everywhere so newer peers never break decoding. *)

module O = Jsont.Object

let version = "2026-07-28"

(* Revisions served through an [initialize] handshake, latest first. *)
let legacy_versions = [ "2025-11-25"; "2025-06-18" ]
let key_protocol_version = "io.modelcontextprotocol/protocolVersion"
let key_client_info = "io.modelcontextprotocol/clientInfo"
let key_client_capabilities = "io.modelcontextprotocol/clientCapabilities"
let key_log_level = "io.modelcontextprotocol/logLevel"
let key_server_info = "io.modelcontextprotocol/serverInfo"
let key_subscription_id = "io.modelcontextprotocol/subscriptionId"

(* Members shared by many objects. *)

let meta_mem enc map = O.opt_mem "_meta" Jsont.json ~enc map
let opt_string name enc map = O.opt_mem name Jsont.string ~enc map

let list_mem name codec enc map =
  O.mem name (Jsont.list codec) ~dec_absent:[] ~enc ~enc_omit:(( = ) []) map

let flag_mem name enc map = O.mem name Jsont.bool ~dec_absent:false ~enc ~enc_omit:not map

(* [resultType] is encoded as "complete" and accepted absent on decode, as
   required for results from earlier protocol revisions. *)
let complete map =
  O.mem "resultType" Jsont.string ~dec_absent:"complete" ~enc:(fun _ -> "complete") map

(* A capability that is only meaningful by presence, e.g. ["roots": {}]. *)
let presence = Jsont.map ~dec:(fun () -> true) ~enc:(fun _ -> ()) O.zero
let presence_mem name enc map = O.mem name presence ~dec_absent:false ~enc ~enc_omit:not map

type role = User | Assistant

let role = Jsont.enum ~kind:"role" [ ("user", User); ("assistant", Assistant) ]

type cache_scope = Public | Private

let cache_scope = Jsont.enum ~kind:"cache scope" [ ("public", Public); ("private", Private) ]

module Icon = struct
  type t = { src : string; mime_type : string option; sizes : string list; theme : string option }

  let v ?mime_type ?(sizes = []) ?theme src = { src; mime_type; sizes; theme }

  let codec =
    O.map ~kind:"icon" (fun src mime_type sizes theme -> { src; mime_type; sizes; theme })
    |> O.mem "src" Jsont.string ~enc:(fun i -> i.src)
    |> opt_string "mimeType" (fun i -> i.mime_type)
    |> list_mem "sizes" Jsont.string (fun i -> i.sizes)
    |> opt_string "theme" (fun i -> i.theme)
    |> O.finish
end

let icons_mem enc map = list_mem "icons" Icon.codec enc map

module Implementation = struct
  type t = {
    name : string;
    version : string;
    title : string option;
    description : string option;
    website_url : string option;
    icons : Icon.t list;
  }

  let v ?title ?description ?website_url ?(icons = []) ~version name =
    { name; version; title; description; website_url; icons }

  let codec =
    O.map ~kind:"implementation" (fun name version title description website_url icons ->
        { name; version; title; description; website_url; icons })
    |> O.mem "name" Jsont.string ~enc:(fun i -> i.name)
    |> O.mem "version" Jsont.string ~enc:(fun i -> i.version)
    |> opt_string "title" (fun i -> i.title)
    |> opt_string "description" (fun i -> i.description)
    |> opt_string "websiteUrl" (fun i -> i.website_url)
    |> icons_mem (fun i -> i.icons)
    |> O.finish
end

module Annotations = struct
  type t = { audience : role list; priority : float option; last_modified : string option }

  let codec =
    O.map ~kind:"annotations" (fun audience priority last_modified ->
        { audience; priority; last_modified })
    |> list_mem "audience" role (fun a -> a.audience)
    |> O.opt_mem "priority" Jsont.number ~enc:(fun a -> a.priority)
    |> opt_string "lastModified" (fun a -> a.last_modified)
    |> O.finish
end

let annotations_mem enc map = O.opt_mem "annotations" Annotations.codec ~enc map

module Contents = struct
  type body = Text of string | Blob of string
  type t = { uri : string; mime_type : string option; body : body; meta : Json.t option }

  let text ?mime_type uri text = { uri; mime_type; body = Text text; meta = None }
  let blob ?mime_type uri blob = { uri; mime_type; body = Blob blob; meta = None }

  let codec =
    O.map ~kind:"resource contents" (fun uri mime_type text blob meta ->
        let body =
          match (text, blob) with
          | Some t, _ -> Text t
          | None, Some b -> Blob b
          | None, None ->
              Jsont.Error.msg Jsont.Meta.none "Resource contents need a text or blob member"
        in
        { uri; mime_type; body; meta })
    |> O.mem "uri" Jsont.string ~enc:(fun c -> c.uri)
    |> opt_string "mimeType" (fun c -> c.mime_type)
    |> opt_string "text" (fun c -> match c.body with Text t -> Some t | Blob _ -> None)
    |> opt_string "blob" (fun c -> match c.body with Blob b -> Some b | Text _ -> None)
    |> meta_mem (fun c -> c.meta)
    |> O.finish
end

module Resource = struct
  type t = {
    uri : string;
    name : string;
    title : string option;
    description : string option;
    mime_type : string option;
    size : int option;
    icons : Icon.t list;
    annotations : Annotations.t option;
    meta : Json.t option;
  }

  let codec =
    O.map ~kind:"resource" (fun uri name title description mime_type size icons annotations meta ->
        { uri; name; title; description; mime_type; size; icons; annotations; meta })
    |> O.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
    |> O.mem "name" Jsont.string ~enc:(fun r -> r.name)
    |> opt_string "title" (fun r -> r.title)
    |> opt_string "description" (fun r -> r.description)
    |> opt_string "mimeType" (fun r -> r.mime_type)
    |> O.opt_mem "size" Jsont.int ~enc:(fun r -> r.size)
    |> icons_mem (fun r -> r.icons)
    |> annotations_mem (fun r -> r.annotations)
    |> meta_mem (fun r -> r.meta)
    |> O.finish
end

module Resource_template = struct
  type t = {
    uri_template : string;
    name : string;
    title : string option;
    description : string option;
    mime_type : string option;
    icons : Icon.t list;
    annotations : Annotations.t option;
  }

  let codec =
    O.map ~kind:"resource template"
      (fun uri_template name title description mime_type icons annotations ->
        { uri_template; name; title; description; mime_type; icons; annotations })
    |> O.mem "uriTemplate" Jsont.string ~enc:(fun r -> r.uri_template)
    |> O.mem "name" Jsont.string ~enc:(fun r -> r.name)
    |> opt_string "title" (fun r -> r.title)
    |> opt_string "description" (fun r -> r.description)
    |> opt_string "mimeType" (fun r -> r.mime_type)
    |> icons_mem (fun r -> r.icons)
    |> annotations_mem (fun r -> r.annotations)
    |> O.finish
end

module Content = struct
  type text = { text : string; annotations : Annotations.t option }
  type media = { data : string; mime_type : string; annotations : Annotations.t option }
  type embedded = { resource : Contents.t; annotations : Annotations.t option }

  type t =
    | Text of text
    | Image of media
    | Audio of media
    | Resource_link of Resource.t
    | Resource of embedded

  let text ?annotations text = Text { text; annotations }
  let image ?annotations ~mime_type data = Image { data; mime_type; annotations }
  let audio ?annotations ~mime_type data = Audio { data; mime_type; annotations }

  let text_codec =
    O.map ~kind:"text content" (fun text annotations -> { text; annotations })
    |> O.mem "text" Jsont.string ~enc:(fun (t : text) -> t.text)
    |> annotations_mem (fun (t : text) -> t.annotations)
    |> O.finish

  let media_codec kind =
    O.map ~kind (fun data mime_type annotations -> { data; mime_type; annotations })
    |> O.mem "data" Jsont.string ~enc:(fun m -> m.data)
    |> O.mem "mimeType" Jsont.string ~enc:(fun m -> m.mime_type)
    |> annotations_mem (fun (m : media) -> m.annotations)
    |> O.finish

  let embedded_codec =
    O.map ~kind:"embedded resource" (fun resource annotations -> { resource; annotations })
    |> O.mem "resource" Contents.codec ~enc:(fun e -> e.resource)
    |> annotations_mem (fun (e : embedded) -> e.annotations)
    |> O.finish

  let case_text = O.Case.map ~dec:(fun t -> Text t) "text" text_codec
  let case_image = O.Case.map ~dec:(fun m -> Image m) "image" (media_codec "image content")
  let case_audio = O.Case.map ~dec:(fun m -> Audio m) "audio" (media_codec "audio content")
  let case_link = O.Case.map ~dec:(fun r -> Resource_link r) "resource_link" Resource.codec
  let case_resource = O.Case.map ~dec:(fun e -> Resource e) "resource" embedded_codec

  let codec =
    let enc_case = function
      | Text t -> O.Case.value case_text t
      | Image m -> O.Case.value case_image m
      | Audio m -> O.Case.value case_audio m
      | Resource_link r -> O.Case.value case_link r
      | Resource e -> O.Case.value case_resource e
    in
    let cases =
      O.Case.
        [ make case_text; make case_image; make case_audio; make case_link; make case_resource ]
    in
    O.map ~kind:"content block" Fun.id
    |> O.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> O.finish
end

module Tool_annotations = struct
  type t = {
    title : string option;
    read_only : bool option;
    destructive : bool option;
    idempotent : bool option;
    open_world : bool option;
  }

  let v ?title ?read_only ?destructive ?idempotent ?open_world () =
    { title; read_only; destructive; idempotent; open_world }

  let codec =
    O.map ~kind:"tool annotations" (fun title read_only destructive idempotent open_world ->
        { title; read_only; destructive; idempotent; open_world })
    |> opt_string "title" (fun a -> a.title)
    |> O.opt_mem "readOnlyHint" Jsont.bool ~enc:(fun a -> a.read_only)
    |> O.opt_mem "destructiveHint" Jsont.bool ~enc:(fun a -> a.destructive)
    |> O.opt_mem "idempotentHint" Jsont.bool ~enc:(fun a -> a.idempotent)
    |> O.opt_mem "openWorldHint" Jsont.bool ~enc:(fun a -> a.open_world)
    |> O.finish
end

module Tool_info = struct
  type t = {
    name : string;
    title : string option;
    description : string option;
    input_schema : Json.t;
    output_schema : Json.t option;
    annotations : Tool_annotations.t option;
    icons : Icon.t list;
    meta : Json.t option;
  }

  let codec =
    O.map ~kind:"tool"
      (fun name title description input_schema output_schema annotations icons meta ->
        { name; title; description; input_schema; output_schema; annotations; icons; meta })
    |> O.mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> opt_string "title" (fun t -> t.title)
    |> opt_string "description" (fun t -> t.description)
    |> O.mem "inputSchema" Jsont.json ~enc:(fun t -> t.input_schema)
    |> O.opt_mem "outputSchema" Jsont.json ~enc:(fun t -> t.output_schema)
    |> O.opt_mem "annotations" Tool_annotations.codec ~enc:(fun t -> t.annotations)
    |> icons_mem (fun t -> t.icons)
    |> meta_mem (fun t -> t.meta)
    |> O.finish
end

module Prompt_info = struct
  type argument = {
    name : string;
    title : string option;
    description : string option;
    required : bool;
  }

  type t = {
    name : string;
    title : string option;
    description : string option;
    arguments : argument list;
    icons : Icon.t list;
  }

  let argument_codec =
    O.map ~kind:"prompt argument" (fun name title description required ->
        { name; title; description; required })
    |> O.mem "name" Jsont.string ~enc:(fun (a : argument) -> a.name)
    |> opt_string "title" (fun (a : argument) -> a.title)
    |> opt_string "description" (fun (a : argument) -> a.description)
    |> flag_mem "required" (fun a -> a.required)
    |> O.finish

  let codec =
    O.map ~kind:"prompt" (fun name title description arguments icons ->
        { name; title; description; arguments; icons })
    |> O.mem "name" Jsont.string ~enc:(fun (p : t) -> p.name)
    |> opt_string "title" (fun (p : t) -> p.title)
    |> opt_string "description" (fun (p : t) -> p.description)
    |> list_mem "arguments" argument_codec (fun p -> p.arguments)
    |> icons_mem (fun (p : t) -> p.icons)
    |> O.finish
end

module Prompt_message = struct
  type t = { role : role; content : Content.t }

  let user content = { role = User; content }
  let assistant content = { role = Assistant; content }

  let codec =
    O.map ~kind:"prompt message" (fun role content -> { role; content })
    |> O.mem "role" role ~enc:(fun m -> m.role)
    |> O.mem "content" Content.codec ~enc:(fun m -> m.content)
    |> O.finish
end

module Client_caps = struct
  type t = {
    elicit_form : bool;
    elicit_url : bool;
    sampling : bool;
    roots : bool;
    extensions : Json.t option;
  }

  let none =
    { elicit_form = false; elicit_url = false; sampling = false; roots = false; extensions = None }

  (* [elicitation: {}] means form mode only. *)
  let elicitation =
    O.map ~kind:"elicitation capability" (fun form url ->
        if form || url then (form, url) else (true, false))
    |> presence_mem "form" fst |> presence_mem "url" snd |> O.finish

  let codec =
    O.map ~kind:"client capabilities" (fun elicitation sampling roots extensions ->
        let elicit_form, elicit_url = Option.value elicitation ~default:(false, false) in
        { elicit_form; elicit_url; sampling; roots; extensions })
    |> O.opt_mem "elicitation" elicitation ~enc:(fun c ->
        if c.elicit_form || c.elicit_url then Some (c.elicit_form, c.elicit_url) else None)
    |> presence_mem "sampling" (fun c -> c.sampling)
    |> presence_mem "roots" (fun c -> c.roots)
    |> O.opt_mem "extensions" Jsont.json ~enc:(fun c -> c.extensions)
    |> O.finish
end

module Server_caps = struct
  type t = {
    tools : bool;
    resources : bool;
    prompts : bool;
    completions : bool;
    extensions : Json.t option;
  }

  let list_changed = Json.obj [ ("listChanged", Json.bool true) ]
  let resources_caps = Json.obj [ ("subscribe", Json.bool true); ("listChanged", Json.bool true) ]

  (* Every feature this SDK serves supports change notifications through
     subscriptions/listen, so the sub-objects are fixed. *)
  let feature value = Jsont.map ~dec:(fun _ -> true) ~enc:(fun _ -> value) Jsont.json

  let make ~list_changed ~resources_caps =
    O.map ~kind:"server capabilities" (fun tools resources prompts completions extensions ->
        { tools; resources; prompts; completions; extensions })
    |> O.mem "tools" (feature list_changed) ~dec_absent:false ~enc:(fun c -> c.tools) ~enc_omit:not
    |> O.mem "resources" (feature resources_caps) ~dec_absent:false
         ~enc:(fun c -> c.resources)
         ~enc_omit:not
    |> O.mem "prompts" (feature list_changed) ~dec_absent:false
         ~enc:(fun c -> c.prompts)
         ~enc_omit:not
    |> presence_mem "completions" (fun c -> c.completions)
    |> O.opt_mem "extensions" Jsont.json ~enc:(fun c -> c.extensions)
    |> O.finish

  let codec = make ~list_changed ~resources_caps

  (* Earlier revisions only notify outside a request, which a stateless
     server cannot do, so they are offered no notifications. *)
  let legacy_codec = make ~list_changed:Json.empty ~resources_caps:Json.empty
end

module Meta = struct
  type t = {
    protocol_version : string;
    client_info : Implementation.t option;
    client_caps : Client_caps.t;
    progress_token : Id.t option;
    log_level : string option;
    extra : Json.t;  (** Every other [_meta] key, e.g. trace context. *)
  }

  (* The [_meta] of a request from an earlier revision: the protocol version
     comes from its session instead. *)
  let none =
    {
      protocol_version = "";
      client_info = None;
      client_caps = Client_caps.none;
      progress_token = None;
      log_level = None;
      extra = Json.empty;
    }

  let codec =
    O.map ~kind:"request _meta"
      (fun protocol_version client_info client_caps progress_token log_level extra ->
        let client_caps =
          match (protocol_version, client_caps) with
          | _, Some caps -> caps
          | "", None -> Client_caps.none
          | _, None -> Jsont.Error.msg Jsont.Meta.none ("Missing " ^ key_client_capabilities)
        in
        { protocol_version; client_info; client_caps; progress_token; log_level; extra })
    |> O.mem key_protocol_version Jsont.string ~dec_absent:"" ~enc:(fun m -> m.protocol_version)
    |> O.opt_mem key_client_info Implementation.codec ~enc:(fun m -> m.client_info)
    |> O.opt_mem key_client_capabilities Client_caps.codec ~enc:(fun m -> Some m.client_caps)
    |> O.opt_mem "progressToken" Id.codec ~enc:(fun m -> m.progress_token)
    |> O.opt_mem key_log_level Jsont.string ~enc:(fun m -> m.log_level)
    |> O.keep_unknown Jsont.json_mems ~enc:(fun m -> m.extra)
    |> O.finish
end

module Filter = struct
  type t = { tools : bool; prompts : bool; resources : bool; uris : string list }

  let none = { tools = false; prompts = false; resources = false; uris = [] }

  let codec =
    O.map ~kind:"subscription filter" (fun tools prompts resources uris ->
        { tools; prompts; resources; uris })
    |> flag_mem "toolsListChanged" (fun f -> f.tools)
    |> flag_mem "promptsListChanged" (fun f -> f.prompts)
    |> flag_mem "resourcesListChanged" (fun f -> f.resources)
    |> list_mem "resourceSubscriptions" Jsont.string (fun f -> f.uris)
    |> O.finish
end

module Call_result = struct
  type t = {
    content : Content.t list;
    structured : Json.t option;
    is_error : bool;
    meta : Json.t option;
  }

  let codec =
    O.map ~kind:"tool result" (fun content structured is_error meta _ ->
        { content; structured; is_error; meta })
    |> O.mem "content" (Jsont.list Content.codec) ~dec_absent:[] ~enc:(fun r -> r.content)
    |> O.opt_mem "structuredContent" Jsont.json ~enc:(fun r -> r.structured)
    |> flag_mem "isError" (fun r -> r.is_error)
    |> meta_mem (fun r -> r.meta)
    |> complete |> O.finish
end

module Read_result = struct
  type t = {
    contents : Contents.t list;
    ttl_ms : int;
    cache_scope : cache_scope;
    meta : Json.t option;
  }

  let codec =
    O.map ~kind:"read resource result" (fun contents ttl_ms cache_scope meta _ ->
        { contents; ttl_ms; cache_scope; meta })
    |> O.mem "contents" (Jsont.list Contents.codec) ~enc:(fun r -> r.contents)
    |> O.mem "ttlMs" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.ttl_ms)
    |> O.mem "cacheScope" cache_scope ~dec_absent:Private ~enc:(fun r -> r.cache_scope)
    |> meta_mem (fun r -> r.meta)
    |> complete |> O.finish
end

module Prompt_result = struct
  type t = { description : string option; messages : Prompt_message.t list; meta : Json.t option }

  let codec =
    O.map ~kind:"get prompt result" (fun description messages meta _ ->
        { description; messages; meta })
    |> opt_string "description" (fun r -> r.description)
    |> O.mem "messages" (Jsont.list Prompt_message.codec) ~enc:(fun r -> r.messages)
    |> meta_mem (fun r -> r.meta)
    |> complete |> O.finish
end

module Completion = struct
  type t = { values : string list; total : int option; has_more : bool }

  let v ?total ?(has_more = false) values = { values; total; has_more }

  let completion_codec =
    O.map ~kind:"completion" (fun values total has_more -> { values; total; has_more })
    |> O.mem "values" (Jsont.list Jsont.string) ~enc:(fun c -> c.values)
    |> O.opt_mem "total" Jsont.int ~enc:(fun c -> c.total)
    |> flag_mem "hasMore" (fun c -> c.has_more)
    |> O.finish

  let codec =
    O.map ~kind:"complete result" (fun completion meta _ -> (completion, meta))
    |> O.mem "completion" completion_codec ~enc:fst
    |> meta_mem snd |> complete |> O.finish
end

module Discover_result = struct
  type t = {
    supported_versions : string list;
    capabilities : Server_caps.t;
    instructions : string option;
    ttl_ms : int;
    cache_scope : cache_scope;
    meta : Json.t option;
  }

  let codec =
    O.map ~kind:"discover result"
      (fun supported_versions capabilities instructions ttl_ms cache_scope meta _ ->
        { supported_versions; capabilities; instructions; ttl_ms; cache_scope; meta })
    |> O.mem "supportedVersions" (Jsont.list Jsont.string) ~enc:(fun r -> r.supported_versions)
    |> O.mem "capabilities" Server_caps.codec ~enc:(fun r -> r.capabilities)
    |> opt_string "instructions" (fun r -> r.instructions)
    |> O.mem "ttlMs" Jsont.int ~dec_absent:0 ~enc:(fun r -> r.ttl_ms)
    |> O.mem "cacheScope" cache_scope ~dec_absent:Private ~enc:(fun r -> r.cache_scope)
    |> meta_mem (fun r -> r.meta)
    |> complete |> O.finish
end

module Initialize_result = struct
  type t = {
    protocol_version : string;
    capabilities : Server_caps.t;
    server_info : Implementation.t;
    instructions : string option;
  }

  let codec =
    O.map ~kind:"initialize result" (fun protocol_version capabilities server_info instructions ->
        { protocol_version; capabilities; server_info; instructions })
    |> O.mem "protocolVersion" Jsont.string ~enc:(fun r -> r.protocol_version)
    |> O.mem "capabilities" Server_caps.legacy_codec ~enc:(fun r -> r.capabilities)
    |> O.mem "serverInfo" Implementation.codec ~enc:(fun r -> r.server_info)
    |> opt_string "instructions" (fun r -> r.instructions)
    |> O.finish
end

module Page = struct
  type 'a t = {
    items : 'a list;
    next_cursor : string option;
    ttl_ms : int;
    cache_scope : cache_scope;
    meta : Json.t option;
  }

  let codec ~items:name item =
    O.map
      ~kind:("list " ^ name ^ " result")
      (fun items next_cursor ttl_ms cache_scope meta _ ->
        { items; next_cursor; ttl_ms; cache_scope; meta })
    |> O.mem name (Jsont.list item) ~enc:(fun p -> p.items)
    |> opt_string "nextCursor" (fun p -> p.next_cursor)
    |> O.mem "ttlMs" Jsont.int ~dec_absent:0 ~enc:(fun p -> p.ttl_ms)
    |> O.mem "cacheScope" cache_scope ~dec_absent:Private ~enc:(fun p -> p.cache_scope)
    |> meta_mem (fun p -> p.meta)
    |> complete |> O.finish
end

module Input_request = struct
  type t = { method_ : string; params : Json.t }

  let codec =
    O.map ~kind:"input request" (fun method_ params -> { method_; params })
    |> O.mem "method" Jsont.string ~enc:(fun r -> r.method_)
    |> O.mem "params" Jsont.json ~dec_absent:Json.empty ~enc:(fun r -> r.params)
    |> O.finish
end

module Input_required = struct
  type t = { requests : (string * Input_request.t) list; state : string option }

  module Smap = Map.Make (String)

  let requests_codec =
    Jsont.map
      ~dec:(fun m -> Smap.bindings m)
      ~enc:(fun l -> Smap.of_list l)
      (O.as_string_map Input_request.codec)

  let codec =
    O.map ~kind:"input required result" (fun _ requests state -> { requests; state })
    |> O.mem "resultType" Jsont.string ~enc:(fun _ -> "input_required")
    |> O.mem "inputRequests" requests_codec ~dec_absent:[]
         ~enc:(fun r -> r.requests)
         ~enc_omit:(( = ) [])
    |> opt_string "requestState" (fun r -> r.state)
    |> O.finish
end

module Elicit_result = struct
  type action = Accept | Decline | Cancel
  type t = { action : action; content : Json.t option }

  let action =
    Jsont.enum ~kind:"elicitation action"
      [ ("accept", Accept); ("decline", Decline); ("cancel", Cancel) ]

  let codec =
    O.map ~kind:"elicit result" (fun action content -> { action; content })
    |> O.mem "action" action ~enc:(fun r -> r.action)
    |> O.opt_mem "content" Jsont.json ~enc:(fun r -> r.content)
    |> O.finish
end
