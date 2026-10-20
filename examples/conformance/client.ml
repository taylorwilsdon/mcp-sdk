(* The client the official conformance suite runs against its scenario
   servers: `client --command "client.exe"`. The suite passes the server URL
   as the last argument and the revision under test in
   MCP_CONFORMANCE_PROTOCOL_VERSION. Dated revisions through 2025-11-25 open
   a session with initialize; 2026-07-28 is stateless.

   Every scenario gets the same treatment: list and call each tool
   concurrently, read each resource, get each prompt. A scenario that names
   its tool calls in MCP_CONFORMANCE_CONTEXT gets exactly those instead.
   Input requests are answered from the defaults of the schema they carry,
   as a user accepting a prefilled form would. *)

open Mcp

let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None

(* A value a schema accepts: its default, else the simplest of its type. *)
let rec example schema =
  match Json.find "default" schema with
  | Some d -> d
  | None -> (
      match str (Json.find "type" schema) with
      | Some "integer" -> Json.int 1
      | Some "number" -> Json.num 1.
      | Some "boolean" -> Json.bool true
      | Some "array" -> Json.list []
      | Some "object" -> arguments schema
      | _ -> Json.str "test")

and arguments schema =
  Json.obj
    (List.map
       (fun (name, s) -> (name, example s))
       (Option.fold ~none:[] ~some:Json.mems (Json.find "properties" schema)))

let on_input _ (r : Proto.Input_request.t) =
  match r.method_ with
  | "elicitation/create" ->
      let content =
        Option.fold ~none:Json.empty ~some:arguments (Json.find "requestedSchema" r.params)
      in
      Some (Json.obj [ ("action", Json.str "accept"); ("content", content) ])
  | "sampling/createMessage" ->
      Some
        (Json.obj
           [
             ("role", Json.str "assistant");
             ("content", Json.obj [ ("type", Json.str "text"); ("text", Json.str "Paris") ]);
             ("model", Json.str "conformance");
           ])
  | "roots/list" -> Some (Json.obj [ ("roots", Json.list []) ])
  | _ -> None

let protocol () =
  match Sys.getenv_opt "MCP_CONFORMANCE_PROTOCOL_VERSION" with
  | Some v when v <> Proto.version && List.mem v Proto.legacy_versions -> `Legacy
  | Some v when v = Proto.version -> `Modern
  | _ -> `Auto

(* [http://host:port/path] *)
let parse_url url =
  let rest =
    match String.index_opt url ':' with
    | Some i when String.sub url i 3 = "://" -> String.sub url (i + 3) (String.length url - i - 3)
    | _ -> url
  in
  let authority, path =
    match String.index_opt rest '/' with
    | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
    | None -> (rest, "/")
  in
  let host, port =
    match String.rindex_opt authority ':' with
    | Some i ->
        (String.sub authority 0 i, String.sub authority (i + 1) (String.length authority - i - 1))
    | None -> (authority, "80")
  in
  (authority, host, port, path)

let report what = function
  | Ok _ -> ()
  | Error e -> Format.eprintf "%s: %a@." what Client.pp_error e

(* [toolCalls] of the scenario's context: (name, arguments). *)
let context_calls () =
  let call c =
    Option.map
      (fun n -> (n, Option.value (Json.find "arguments" c) ~default:Json.empty))
      (str (Json.find "name" c))
  in
  match
    Option.bind (Sys.getenv_opt "MCP_CONFORMANCE_CONTEXT") (fun c ->
        Result.to_option (Json.decode Jsont.json c))
    |> Option.map (Json.find "toolCalls")
  with
  | Some (Some (Jsont.Array (calls, _))) -> Some (List.filter_map call calls)
  | _ -> None

let exercise client =
  report "protocol" (Client.protocol_version client);
  (match Client.list_tools client with
  | Error e -> report "tools/list" (Error e)
  | Ok tools ->
      let calls =
        match context_calls () with
        | Some calls -> calls
        | None ->
            List.map
              (fun (tool : Proto.Tool_info.t) -> (tool.name, arguments tool.input_schema))
              tools
      in
      Eio.Fiber.List.iter
        (fun (name, args) -> report name (Client.call_tool client name args))
        calls);
  (match Client.list_resources client with
  | Error e -> report "resources/list" (Error e)
  | Ok resources ->
      List.iter
        (fun (r : Proto.Resource.t) -> report r.uri (Client.read_resource client r.uri))
        resources);
  match Client.list_prompts client with
  | Error e -> report "prompts/list" (Error e)
  | Ok prompts ->
      List.iter
        (fun (p : Proto.Prompt_info.t) ->
          let args =
            List.map (fun (a : Proto.Prompt_info.argument) -> (a.name, "test")) p.arguments
          in
          report p.name (Client.get_prompt client ~args p.name))
        prompts

let () =
  let url = Sys.argv.(Array.length Sys.argv - 1) in
  let authority, host, port, path = parse_url url in
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let connect sw =
    match Eio.Net.getaddrinfo_stream net host ~service:port with
    | addr :: _ -> Eio.Net.connect ~sw net addr
    | [] -> failwith ("Cannot resolve " ^ host)
  in
  let transport = Mcp_http.Client.v ~sw ~path ~authority connect in
  let capabilities =
    { Proto.Client_caps.none with elicit_form = true; sampling = true; roots = true }
  in
  exercise
    (Client.connect ~capabilities ~on_input ~protocol:(protocol ())
       ~info:(Proto.Implementation.v ~version:"1.0.0" "mcp-sdk-conformance")
       transport)
