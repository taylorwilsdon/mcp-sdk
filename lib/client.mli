(** MCP clients.

    Every request carries the protocol version, client info and capabilities in [_meta]; there is no
    handshake. Multi round-trip requests are driven to completion through [on_input], list results
    are cached for the [ttlMs] the server allows, and [x-mcp-header] arguments are mirrored into
    HTTP headers for tools seen in {!list_tools}. A client may be shared by any number of fibers,
    within one domain. *)

type error =
  | Remote of Error.t  (** An error response from the server. *)
  | Local of string  (** A failure on this side, never sent by the server. *)

val pp_error : Format.formatter -> error -> unit

type transport = {
  send : headers:(string * string) list -> id:Id.t -> string -> on_note:(string -> unit) -> string;
}
(** [send ~headers ~id body ~on_note] sends one request and blocks until its response, passing
    request-scoped notifications to [on_note]. Cancelling the calling fiber cancels the request.

    The built-in transports share one contract. Their reader never waits on application code, so
    responses keep flowing whatever callbacks do. Callbacks run in the calling fiber, in wire order,
    before the response is returned, and may make other requests (over HTTP, given a free pool
    connection). Notifications wait for their callbacks in a queue of at most [max_queued] bytes per
    request; a request whose queue would exceed it fails with a [Local] error and is cancelled,
    rather than letting a slow callback grow memory without bound. *)

type input_handler = string -> Proto.Input_request.t -> Json.t option
(** Answer an input request (by key), e.g. an elicitation with an [ElicitResult] object. [None]
    gives up on the request. *)

type t

val connect :
  ?info:Proto.Implementation.t ->
  ?capabilities:Proto.Client_caps.t ->
  ?on_input:input_handler ->
  transport ->
  t

val discover : t -> (Proto.Discover_result.t, error) result

val list_tools : t -> (Proto.Tool_info.t list, error) result
(** All pages. Tools with invalid [x-mcp-header] annotations are dropped, as the transport rules
    require. *)

val call_tool :
  t ->
  ?on_progress:(?total:float -> ?message:string -> float -> unit) ->
  string ->
  Json.t ->
  (Proto.Call_result.t, error) result

val list_resources : t -> (Proto.Resource.t list, error) result
val list_templates : t -> (Proto.Resource_template.t list, error) result
val read_resource : t -> string -> (Proto.Read_result.t, error) result
val list_prompts : t -> (Proto.Prompt_info.t list, error) result

val get_prompt :
  t -> ?args:(string * string) list -> string -> (Proto.Prompt_result.t, error) result

val complete :
  t ->
  ref_:[ `Prompt of string | `Template of string ] ->
  arg:string ->
  value:string ->
  (Proto.Completion.t, error) result

val listen : t -> Proto.Filter.t -> (string -> Json.t -> unit) -> (unit, error) result
(** [listen t filter f] opens a [subscriptions/listen] stream and calls [f method params] for each
    notification until the server ends it or the calling fiber is cancelled. List notifications
    invalidate cached lists. *)

val request :
  t ->
  ?on_note:(string -> unit) ->
  ?rounds:int ->
  string ->
  (string * Json.t) list ->
  (Json.t, error) result
(** A raw request with the given parameters, retried through at most [rounds] input rounds (default
    8). *)

val invalidate : t -> string -> unit
(** Drop the cached result of a list method. Fetches already in progress may finish, but cannot
    repopulate the cache with a result obtained across this invalidation. *)

val line_transport :
  sw:Eio.Switch.t ->
  ?on_notification:(string -> unit) ->
  ?max_message:int ->
  ?max_queued:int ->
  _ Eio.Flow.source ->
  _ Eio.Flow.sink ->
  transport
(** Newline-delimited JSON-RPC, e.g. to a server's stdio. Requests are multiplexed by id; cancelling
    one sends [notifications/cancelled]. Notifications for no in-flight request go to
    [on_notification], on a separate fiber, and are dropped while more than [max_queued] bytes of
    them wait for it. [max_message] bounds one incoming message (default 64MiB) and [max_queued]
    each notification queue (default 4MiB). EOF or an I/O failure rejects both pending and future
    sends. *)

(**/**)

val is_response : string -> bool

module Inbox : sig
  type t

  val create : max_queued:int -> overflow:[ `Fail | `Drop ] -> t
  val note : t -> string -> unit
  val respond : t -> (string, string) result -> unit
  val receive : t -> on_note:(string -> unit) -> string
end

val header_params : Proto.Tool_info.t -> (string list * string) list option
