(** Serialized JSON-RPC messages as lists of fragments.

    Responses are assembled from shared, preserialized pieces (a cached [tools/list] result, a
    request id) and written fragment by fragment, so large cached payloads are never copied into a
    fresh string per request. *)

type t = string list

val result : Id.t -> string -> t
(** A result response around a serialized result object. *)

val error : ?id:Id.t -> Error.t -> t
val notification : string -> string -> t

val request : Id.t -> string -> string -> t
(** A request from the server, as earlier revisions use for elicitation and sampling. *)

val iter : (string -> unit) -> t -> unit
val length : t -> int
val to_string : t -> string
