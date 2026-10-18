(** JSON-RPC request ids: strings or integers, never [null]. *)

type t = Int of int | String of string

val codec : t Jsont.t
val to_json : t -> Json.t

val to_wire : t -> string
(** The id as JSON text. *)

val pp : Format.formatter -> t -> unit
