(** Resources and resource templates. *)

type result = Proto.Read_result.t Input.step
type t

val v :
  ?title:string ->
  ?description:string ->
  ?mime_type:string ->
  ?size:int ->
  ?icons:Proto.Icon.t list ->
  ?annotations:Proto.Annotations.t ->
  ?meta:Json.t ->
  uri:string ->
  string ->
  (Ctx.t -> (result, Error.t) Stdlib.result) ->
  t

val uri : t -> string
val info : t -> Proto.Resource.t

type template

val template :
  ?title:string ->
  ?description:string ->
  ?mime_type:string ->
  ?icons:Proto.Icon.t list ->
  ?annotations:Proto.Annotations.t ->
  ?complete:(string -> string -> string list) ->
  uri_template:string ->
  string ->
  (Ctx.t -> string -> (string * string) list -> (result, Error.t) Stdlib.result) ->
  template
(** [template ~uri_template name read] serves every URI matching an RFC 6570 template: [{var}]
    matches one path segment and [{+var}] anything. [read ctx uri vars] receives the decoded
    variables. [complete arg prefix] answers [completion/complete] for a variable. *)

val template_info : template -> Proto.Resource_template.t

val match_template : template -> string -> (string * string) list option
(** The variables of a URI matching the template. *)

val contents : ?ttl_ms:int -> ?cache_scope:Proto.cache_scope -> Proto.Contents.t list -> result
(** A read result. Caching defaults to immediately stale and private. *)

val text :
  ?mime_type:string -> ?ttl_ms:int -> ?cache_scope:Proto.cache_scope -> string -> string -> result
(** [text uri s] *)

val blob :
  ?mime_type:string -> ?ttl_ms:int -> ?cache_scope:Proto.cache_scope -> string -> string -> result
(** [blob uri base64] *)

(**/**)

val read : t -> Ctx.t -> (result, Error.t) Stdlib.result

val read_template :
  template -> Ctx.t -> string -> (string * string) list -> (result, Error.t) Stdlib.result

val completer : template -> (string -> string -> string list) option
