(** MCP servers.

    A server is an immutable registry snapshot published through an [Atomic.t]: requests on any
    fiber or domain read it without locking, and updates swap in a new snapshot whose list results
    are serialized once. Every request is handled on its own, as the stateless 2026-07-28 protocol
    intends. *)

type t

val supported_versions : string list

val v :
  ?title:string ->
  ?description:string ->
  ?website_url:string ->
  ?icons:Proto.Icon.t list ->
  ?instructions:string ->
  ?extensions:Json.t ->
  ?tools:Tool.t list ->
  ?resources:Resource.t list ->
  ?templates:Resource.template list ->
  ?prompts:Prompt.t list ->
  ?page_size:int ->
  ?list_ttl_ms:int ->
  ?list_scope:Proto.cache_scope ->
  ?state_key:string ->
  ?state_ttl_ms:int ->
  name:string ->
  version:string ->
  unit ->
  t
(** A server advertising a feature for each of [tools], [resources] or [templates], and [prompts]
    that is given, even empty.

    - [page_size] paginates list results (default: one page).
    - [list_ttl_ms] and [list_scope] are the caching hints of list results and [server/discover]
      (default: 60s, public).
    - [state_key] is the MRTR [requestState] HMAC key. It defaults to a random key, so set it when
      several processes serve the same clients.
    - [state_ttl_ms] is how long sealed state stays valid (default: 10min). *)

val info : t -> Proto.Implementation.t

(** {1 Changing the registry}

    Each change notifies [subscriptions/listen] streams that asked for it. *)

val set_tools : t -> Tool.t list -> unit
val add_tool : t -> Tool.t -> unit
val remove_tool : t -> string -> unit
val set_prompts : t -> Prompt.t list -> unit
val set_resources : t -> ?templates:Resource.template list -> Resource.t list -> unit

val resource_updated : t -> string -> unit
(** Notify subscribers of a resource URI that it changed. *)

val shutdown : t -> unit
(** End every subscription stream gracefully. *)

(** {1 Transports}

    What a transport needs to serve requests. *)

type request

type incoming =
  | Request of request
  | Cancel of Id.t  (** [notifications/cancelled] for a request id. *)
  | Ignore  (** A notification needing no action. *)
  | Reject of Id.t option * Error.t  (** Answer with this error. *)

val decode : t -> string -> incoming
(** Decode one JSON-RPC message: envelope, method parameters and typed tool arguments in a single
    pass. *)

val execute : t -> ?principal:string -> emit:(Wire.t -> unit) -> request -> Wire.t * int
(** Run a request, returning its response and the HTTP status it implies. Request-scoped
    notifications go to [emit] before the response. *)

module Request : sig
  type t = request

  val id : t -> Id.t
  val method_ : t -> string
  val protocol_version : t -> string

  val target : t -> string
  (** The tool or prompt name, or resource URI, of the request. *)

  val header_args : t -> Schema.header list
end
