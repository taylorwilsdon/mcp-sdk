(* Example server exercising every feature of the SDK. *)

open Mcp
open Mcp_http

let echo =
  Tool.v "echo" ~description:"Echo a message back"
    Schema.(
      let+ message = req "message" string ~doc:"Text to echo" in
      message)
    (fun _ctx message -> Ok (Tool.text message))

let add =
  Tool.v "add" ~description:"Add two integers"
    ~annotations:(Proto.Tool_annotations.v ~read_only:true ())
    Schema.(
      let+ a = req "a" int and+ b = req "b" int in
      (a, b))
    (fun _ctx (a, b) ->
      Ok
        (Tool.structured
           ~content:[ Proto.Content.text (string_of_int (a + b)) ]
           (Json.obj [ ("sum", Json.int (a + b)) ])))

let countdown clock =
  Tool.v "countdown" ~description:"Count down, reporting progress"
    Schema.(
      let+ from = dft "from" int 3 ~doc:"Starting number" in
      from)
    (fun ctx from ->
      for i = 1 to from do
        Eio.Time.sleep clock 0.1;
        Ctx.progress ctx ~total:(float from) (float i)
      done;
      Ok (Tool.text "liftoff"))

(* Multi round-trip: ask the user to confirm through elicitation, carrying
   the original request in sealed requestState across the retry. *)
let delete =
  let confirm = Schema.(req "confirm" bool ~doc:"Really delete?") in
  Tool.v "delete_file" ~description:"Delete a file after confirmation"
    ~annotations:(Proto.Tool_annotations.v ~destructive:true ())
    Schema.(
      let+ path = req "path" string in
      path)
    (fun ctx path ->
      match Ctx.elicited ctx "confirm" confirm with
      | Some (Ctx.Accepted true) -> Ok (Tool.text ("deleted " ^ path))
      | Some (Accepted false | Declined | Cancelled) -> Ok (Tool.text "kept")
      | None ->
          Ok
            (Input.required ~state:path
               [ ("confirm", Input.elicit ~message:("Delete " ^ path ^ "?") confirm) ]))

let readme =
  Resource.v ~uri:"file:///README.md" "README" ~mime_type:"text/markdown" (fun _ ->
      Ok
        (Resource.text ~mime_type:"text/markdown" ~ttl_ms:60_000 ~cache_scope:Proto.Public
           "file:///README.md" "# Hello"))

let greeting =
  Resource.template ~uri_template:"greeting://{name}" "greeting"
    ~complete:(fun _ prefix -> List.filter (String.starts_with ~prefix) [ "alice"; "bob"; "carol" ])
    (fun _ uri vars -> Ok (Resource.text uri ("Hello, " ^ List.assoc "name" vars)))

let review =
  Prompt.v "review" ~description:"Review some code"
    ~args:[ Prompt.arg "code" ~required:true ]
    (fun _ args ->
      Ok
        (Prompt.messages
           [ Proto.Prompt_message.user (Proto.Content.text ("Review:\n" ^ List.assoc "code" args)) ]))

let server clock =
  Server.v ~name:"mcp-sdk-example" ~version:"0.1.0"
    ~instructions:"Example server for the OCaml MCP SDK"
    ~tools:[ echo; add; countdown clock; delete ]
    ~resources:[ readme ] ~templates:[ greeting ] ~prompts:[ review ] ()

let () =
  let http_port = ref None and domains = ref 0 in
  Arg.parse
    [
      ("--http", Arg.Int (fun p -> http_port := Some p), "PORT  Serve Streamable HTTP/1.1 on PORT");
      ("--domains", Arg.Set_int domains, "N  Extra domains accepting HTTP connections");
    ]
    (fun _ -> ())
    "mcp-sdk [--http PORT] [--domains N]  (stdio when no port is given)";
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env and net = Eio.Stdenv.net env in
  let server = server clock in
  match !http_port with
  | None -> Stdio.run server env
  | Some port ->
      let domains = if !domains > 0 then Some (Eio.Stdenv.domain_mgr env, !domains) else None in
      Mcp_http.serve ?domains ~net ~clock ~port server
