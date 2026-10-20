(** MCP servers.

    A server is an immutable registry snapshot published through an [Atomic.t]: requests on any
    fiber or domain read it without locking, and updates swap in a new snapshot whose list results
    are serialized once. Every request is handled on its own, as the stateless 2026-07-28 protocol
    intends.

    Clients of revisions 2025-11-25 and 2025-06-18 are served too. Their [initialize] opens a
    session: the negotiated version, client info and capabilities that later requests of the session
    would otherwise carry in [_meta]. The transport keeps it, so the server still holds no state per
    client. Handlers need no changes: when one returns {!Input.required}, the server puts each input
    request to the client itself and runs the handler again with the answers. Change notifications
    and resource subscriptions are not offered to these clients, since earlier revisions only
    deliver them outside a request.

    One server serves every domain: registry changes, subscription notifications and the
    [requestState] key are shared by all of them. Handlers run on whichever domain serves the
    request, so anything they capture must be safe to use from any domain; keep resources bound to
    one domain in {!Local} values. *)

type t

val supported_versions : string list
(** The revisions served without a session. {!Proto.legacy_versions} are served through
    [initialize]. *)

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

    - [page_size] paginates list results (default: one page). Cursors are opaque and tied to the
      listed contents and page size. If either changes, a stale cursor returns [-32602]; restart the
      listing without a cursor.
    - [list_ttl_ms] and [list_scope] are the caching hints of list results and [server/discover]
      (default: 60s, public).
    - [state_key] is the MRTR [requestState] HMAC key. It defaults to a random key, so set it when
      several processes serve the same clients. State is bound to the method, target, principal and
      original arguments. Object member ordering may change on retry; argument values may not.
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

val decode : t -> ?session:Proto.Meta.t -> string -> incoming
(** Decode one JSON-RPC message: envelope, method parameters and typed tool arguments in a single
    pass. It runs no application code beyond the codecs given to {!Schema.of_jsont}: conversions
    such as {!Schema.map} and {!Schema.refine} run in {!execute}, so a transport can decode on its
    reader without handlers stalling it.

    [session] is the one {!Request.session} opened for the peer, if any. A client's answer to a
    request the server put to it is taken here and comes back as [Ignore]. *)

val execute :
  t -> ?principal:string -> scope:Local.scope -> emit:(Wire.t -> unit) -> request -> Wire.t * int
(** Run a request, returning its response and the HTTP status it implies. [scope] holds the {!Local}
    values of the calling domain. Request-scoped notifications go to [emit] before the response;
    [emit] may block until the peer accepts them, which is how a slow client holds back a handler
    that keeps emitting. *)

module Request : sig
  type t = request

  val id : t -> Id.t
  val method_ : t -> string
  val protocol_version : t -> string

  val session : t -> Proto.Meta.t option
  (** The session an [initialize] request opens, for the transport to pass to {!decode} with the
      peer's later messages. *)

  val target : t -> string
  (** The tool or prompt name, or resource URI, of the request. *)

  val header_args : t -> Schema.header list
end
