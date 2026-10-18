(** Tools. *)

type result = Proto.Call_result.t Input.step
type t

val v :
  ?title:string ->
  ?description:string ->
  ?annotations:Proto.Tool_annotations.t ->
  ?icons:Proto.Icon.t list ->
  ?output_schema:Json.t ->
  ?meta:Json.t ->
  string ->
  'a Schema.t ->
  (Ctx.t -> 'a -> (result, Error.t) Stdlib.result) ->
  t
(** [v name args handler] is a tool whose arguments are decoded with [args]. Report failures of the
    tool itself with {!error} so the model sees them; return [Error] only for protocol errors. *)

val name : t -> string
val info : t -> Proto.Tool_info.t
val content : ?structured:Json.t -> ?is_error:bool -> Proto.Content.t list -> result
val text : ?is_error:bool -> string -> result
val error : string -> result
val structured : ?content:Proto.Content.t list -> Json.t -> result

(**/**)

type call = private {
  name : string;
  run : Ctx.t -> (result, Error.t) Stdlib.result;
  headers : Schema.header list;
}

val case : t -> (call, string) Jsont.Object.Case.t
