(** Multi round-trip requests (MRTR).

    A handler that needs more from the client returns {!required} with the requests to fulfil. The
    client retries the original request with the answers, which the handler reads with
    {!Ctx.elicited} or {!Ctx.input}. Any [state] is sealed by the server (HMAC-SHA256, bound to the
    method, target and principal, with an expiry) and handed back verified through {!Ctx.state}, so
    handlers stay stateless. The state is signed, not encrypted: do not put secrets in it. *)

type request = Proto.Input_request.t
type required = Proto.Input_required.t

type 'a step =
  | Complete of 'a
  | Required of required  (** The client must provide input and retry. *)

val elicit : message:string -> 'a Schema.t -> request
(** Ask the user to fill in a form described by a flat schema. *)

val elicit_url : message:string -> url:string -> request
(** Send the user to [url] for an out-of-band interaction. *)

val raw : method_:string -> Json.t -> request
(** Any other input request, e.g. the deprecated [sampling/createMessage]. *)

val required : ?state:string -> (string * request) list -> 'a step
(** Input requests keyed by names unique within the request.
    @raise Invalid_argument when there are neither requests nor state. *)

(**/**)

val missing_capability : Proto.Client_caps.t -> request -> (string * Json.t) option
