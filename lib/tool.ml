module O = Jsont.Object

type result = Proto.Call_result.t Input.step
type 'b output = { schema : Json.t; codec : 'b Jsont.t }

let output ~schema codec = { schema; codec }

(* A decoded [tools/call]: the handler already applied to its typed
   arguments, plus the [x-mcp-header] values the transport must check and
   the result's form for earlier revisions. *)
type call = {
  name : string;
  run : Ctx.t -> (result, Error.t) Stdlib.result;
  headers : Schema.header list;
  legacy : Proto.Call_result.t -> Proto.Call_result.t;
}

type t = {
  info : Proto.Tool_info.t;
  legacy_info : Proto.Tool_info.t;
  case : (call, string) O.Case.t;
}

let name t = t.info.name
let info t = t.info
let legacy_info t = t.legacy_info
let case t = t.case

(* Earlier revisions only allow objects as [structuredContent] and
   [outputSchema], so any other value is wrapped as [{"result": value}], as
   other SDKs do. A tool's schema decides this for all its results. *)
let is_object = function Jsont.Object _ -> true | _ -> false

let object_schema s =
  match Json.find "type" s with Some (Jsont.String ("object", _)) -> true | _ -> false

let wrapped schema =
  Json.obj
    [
      ("type", Json.str "object");
      ("properties", Json.obj [ ("result", schema) ]);
      ("required", Json.list [ Json.str "result" ]);
    ]

let legacy output_schema (r : Proto.Call_result.t) =
  let wraps v =
    match output_schema with Some s -> not (object_schema s) | None -> not (is_object v)
  in
  match r.structured with
  | Some v when wraps v -> { r with structured = Some (Json.obj [ ("result", v) ]) }
  | _ -> r

let v ?title ?description ?annotations ?(icons = []) ?output ?meta name schema handler =
  let output_schema = Option.map (fun o -> o.schema) output in
  let legacy = legacy output_schema in
  let decoder = Schema.decoder schema in
  (* Arguments are converted when the call executes, not as it is decoded. *)
  let call convert headers =
    let run ctx =
      match convert () with
      | Ok args -> handler ctx args
      | Error msg -> Error (Error.invalid_params msg)
    in
    { name; run; headers; legacy }
  in
  (* An absent [arguments] member means the empty object. *)
  let empty =
    match Json.decode decoder "{}" with
    | Ok (convert, _) -> call convert []
    | Error e -> { name; run = (fun _ -> Error (Error.invalid_params e)); headers = []; legacy }
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
  let legacy_info =
    match output_schema with
    | Some s when not (object_schema s) -> { info with output_schema = Some (wrapped s) }
    | _ -> info
  in
  { info; legacy_info; case = O.Case.make (O.Case.map ~dec:Fun.id name call) }

let content ?structured ?(is_error = false) content : result =
  Input.Complete { content; structured; is_error; meta = None }

let text ?is_error s = content ?is_error [ Proto.Content.text s ]
let error s = text ~is_error:true s

let structured ?(content = []) json =
  Input.Complete
    { Proto.Call_result.content; structured = Some json; is_error = false; meta = None }

let return ?content o value =
  match Jsont.Json.encode o.codec value with
  | Error e -> invalid_arg ("Tool.return: " ^ e)
  | Ok json ->
      let content = Option.value content ~default:[ Proto.Content.text (Json.to_string json) ] in
      structured ~content json
