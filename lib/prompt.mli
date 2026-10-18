(** Prompts. *)

type result = Proto.Prompt_result.t Input.step
type args = (string * string) list
type t

val arg :
  ?title:string -> ?description:string -> ?required:bool -> string -> Proto.Prompt_info.argument

val v :
  ?title:string ->
  ?description:string ->
  ?icons:Proto.Icon.t list ->
  ?args:Proto.Prompt_info.argument list ->
  ?complete:(string -> string -> args -> string list) ->
  string ->
  (Ctx.t -> args -> (result, Error.t) Stdlib.result) ->
  t
(** [v name get]. Required arguments are checked before [get] runs. [complete arg prefix args]
    answers [completion/complete]. *)

val name : t -> string
val info : t -> Proto.Prompt_info.t
val messages : ?description:string -> Proto.Prompt_message.t list -> result

(**/**)

val get : t -> Ctx.t -> args -> (result, Error.t) Stdlib.result
val completer : t -> (string -> string -> args -> string list) option
val missing_args : t -> args -> string list
