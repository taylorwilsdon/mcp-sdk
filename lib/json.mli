(** Generic JSON values and codec helpers over {!Jsont}. *)

type t = Jsont.json

val str : string -> t
val int : int -> t
val num : float -> t
val bool : bool -> t
val list : t list -> t
val null : t
val obj : (string * t) list -> t
val empty : t

val mems : t -> (string * t) list
(** The members of an object, [[]] for anything else. *)

val find : string -> t -> t option
val find_path : string list -> t -> t option

val encode : 'a Jsont.t -> 'a -> string
(** Minified JSON.
    @raise Invalid_argument if the codec cannot encode. *)

val decode : 'a Jsont.t -> string -> ('a, string) result
val decode_json : 'a Jsont.t -> t -> ('a, string) result
val to_string : t -> string

val quote : string -> string
(** A JSON string literal. *)
