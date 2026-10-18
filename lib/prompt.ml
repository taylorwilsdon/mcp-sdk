type result = Proto.Prompt_result.t Input.step
type args = (string * string) list

type t = {
  info : Proto.Prompt_info.t;
  get : Ctx.t -> args -> (result, Error.t) Stdlib.result;
  complete : (string -> string -> args -> string list) option;
}

let arg ?title ?description ?(required = false) name : Proto.Prompt_info.argument =
  { name; title; description; required }

let v ?title ?description ?(icons = []) ?(args = []) ?complete name get =
  { info = { name; title; description; arguments = args; icons }; get; complete }

let name t = t.info.name
let info t = t.info
let get t = t.get
let completer t = t.complete
let messages ?description messages : result = Input.Complete { description; messages; meta = None }

let missing_args t args =
  List.filter_map
    (fun (a : Proto.Prompt_info.argument) ->
      if a.required && not (List.mem_assoc a.name args) then Some a.name else None)
    t.info.arguments
