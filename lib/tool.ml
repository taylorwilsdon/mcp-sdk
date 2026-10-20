module O = Jsont.Object

type result = Proto.Call_result.t Input.step

(* A decoded [tools/call]: the handler already applied to its typed
   arguments, plus the [x-mcp-header] values the transport must check. *)
type call = {
  name : string;
  run : Ctx.t -> (result, Error.t) Stdlib.result;
  headers : Schema.header list;
}

type t = { info : Proto.Tool_info.t; case : (call, string) O.Case.t }

let name t = t.info.name
let info t = t.info
let case t = t.case

let v ?title ?description ?annotations ?(icons = []) ?output_schema ?meta name schema handler =
  let decoder = Schema.decoder schema in
  (* Arguments are converted when the call executes, not as it is decoded. *)
  let call convert headers =
    let run ctx =
      match convert () with
      | Ok args -> handler ctx args
      | Error msg -> Error (Error.invalid_params msg)
    in
    { name; run; headers }
  in
  (* An absent [arguments] member means the empty object. *)
  let empty =
    match Json.decode decoder "{}" with
    | Ok (convert, _) -> call convert []
    | Error e -> { name; run = (fun _ -> Error (Error.invalid_params e)); headers = [] }
  in
  let call =
    O.map
      ~kind:("tools/call " ^ name ^ " params")
      (function Some (convert, headers) -> call convert headers | None -> empty)
    |> O.opt_mem "arguments" decoder |> O.finish
  in
  let info : Proto.Tool_info.t =
    {
      name;
      title;
      description;
      input_schema = Schema.json_schema schema;
      output_schema;
      annotations;
      icons;
      meta;
    }
  in
  { info; case = O.Case.make (O.Case.map ~dec:Fun.id name call) }

let content ?structured ?(is_error = false) content : result =
  Input.Complete { content; structured; is_error; meta = None }

let text ?is_error s = content ?is_error [ Proto.Content.text s ]
let error s = text ~is_error:true s

let structured ?(content = []) json =
  Input.Complete
    { Proto.Call_result.content; structured = Some json; is_error = false; meta = None }
