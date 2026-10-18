(** The context of one request, handed to every handler.

    The protocol is stateless: everything here comes from the request itself. Functions that emit
    messages must be called from the handler's own fiber. *)

type t

val id : t -> Id.t
val protocol_version : t -> string
val client_info : t -> Proto.Implementation.t option
val client_caps : t -> Proto.Client_caps.t

val principal : t -> string option
(** The authenticated principal, as established by the HTTP transport's [authenticate] hook. *)

val meta : t -> string -> Json.t option
(** Any other [_meta] key of the request, e.g. ["traceparent"]. *)

val progress : t -> ?total:float -> ?message:string -> float -> unit
(** Report progress if the client asked for it. Values that do not increase are dropped, as the
    protocol requires. *)

val state : t -> string option
(** The verified [requestState] of a retried request. *)

val input : t -> string -> Json.t option
(** The raw input response for a key. *)

type 'a elicited = Accepted of 'a | Declined | Cancelled

val elicited : t -> string -> 'a Schema.t -> 'a elicited option
(** The answer to an {!Input.elicit} request. [None] when absent or not matching the schema, in
    which case the handler should ask again. *)

(**/**)

val make :
  id:Id.t ->
  meta:Proto.Meta.t ->
  inputs:(string * Json.t) list ->
  state:string option ->
  principal:string option ->
  emit:(Wire.t -> unit) ->
  t

val emit : t -> Wire.t -> unit
