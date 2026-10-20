(** Tools. *)

type result = Proto.Call_result.t Input.step
type t

type 'b output
(** The typed structured result of a tool: the [outputSchema] it advertises and the codec its values
    are encoded with, so the two cannot drift apart. *)

val output : schema:Json.t -> 'b Jsont.t -> 'b output
(** [output ~schema codec]. As with {!Schema.of_jsont}, keep [schema] in agreement with [codec]; the
    SDK does not validate results against it. [output ~schema Jsont.json] advertises a schema for
    results built with {!structured}. *)

val v :
  ?title:string ->
  ?description:string ->
  ?annotations:Proto.Tool_annotations.t ->
  ?icons:Proto.Icon.t list ->
  ?output:'b output ->
  ?meta:Json.t ->
  string ->
  'a Schema.t ->
  (Ctx.t -> 'a -> (result, Error.t) Stdlib.result) ->
  t
(** [v name args handler] is a tool whose arguments are decoded with [args]. Report failures of the
    tool itself with {!error} so the model sees them; return [Error] only for protocol errors.
    [output] advertises the [outputSchema] that results built with {!return} satisfy.

    Revisions before 2026-07-28 require structured results and output schemas to be objects. For
    their clients, a non-object output schema [S] is advertised as
    [{"type":"object","properties":{"result":S},"required":["result"]}] and each result [v] is sent
    as [{"result": v}]. A tool without an output schema has its non-object results wrapped the same
    way. *)

val name : t -> string
val info : t -> Proto.Tool_info.t
val content : ?structured:Json.t -> ?is_error:bool -> Proto.Content.t list -> result
val text : ?is_error:bool -> string -> result
val error : string -> result
val structured : ?content:Proto.Content.t list -> Json.t -> result

val return : ?content:Proto.Content.t list -> 'b output -> 'b -> result
(** [return output v] is [v] as structured content. [content] defaults to one text block holding the
    same JSON, for clients that only read content.
    @raise Invalid_argument if the codec cannot encode [v]. *)

(**/**)

type call = private {
  name : string;
  run : Ctx.t -> (result, Error.t) Stdlib.result;
  headers : Schema.header list;
  legacy : Proto.Call_result.t -> Proto.Call_result.t;
}

val legacy_info : t -> Proto.Tool_info.t
val case : t -> (call, string) Jsont.Object.Case.t
