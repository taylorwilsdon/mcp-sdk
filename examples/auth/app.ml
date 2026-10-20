(* A notes server for authenticated clients. The MCP endpoint is mounted in
   the application's own httpun-eio server, next to the protected resource
   metadata that tells clients where to get a token. *)

open Mcp

let scopes = [ "notes:read"; "notes:write" ]
let metadata_path = "/.well-known/oauth-protected-resource"

(* Authorization happens per tool, from the principal the bearer hook
   established: a missing scope is a tool error the model can explain. *)
let requiring scope ctx f =
  match Bearer.Principal.of_ctx ctx with
  | Some p when Bearer.Principal.allows p scope -> Ok (f p)
  | _ -> Ok (Tool.error ("insufficient scope: " ^ scope ^ " is required"))

let server () =
  let notes = Atomic.make [] in
  let rec add note =
    let l = Atomic.get notes in
    if not (Atomic.compare_and_set notes l (note :: l)) then add note
  in
  Server.v ~name:"notes" ~version:"1.0.0"
    ~tools:
      [
        Tool.v "whoami" ~description:"The authenticated caller" Schema.empty (fun ctx () ->
            Ok
              (Tool.text
                 (Option.fold (Bearer.Principal.of_ctx ctx) ~none:"anonymous" ~some:(fun p ->
                      p.Bearer.Principal.sub))));
        Tool.v "list_notes" ~description:"Notes written so far (needs notes:read)" Schema.empty
          (fun ctx () ->
            requiring "notes:read" ctx (fun _ ->
                Tool.text (String.concat "\n" (List.rev (Atomic.get notes)))));
        Tool.v "add_note" ~description:"Write a note (needs notes:write)"
          Schema.(req "text" string ~doc:"The note")
          (fun ctx text ->
            requiring "notes:write" ctx (fun p ->
                add (p.sub ^ ": " ^ text);
                Tool.text "saved"));
      ]
    ()

let respond reqd ?(headers = []) status body =
  let headers =
    Httpun.Headers.of_list (("content-length", string_of_int (String.length body)) :: headers)
  in
  Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers status) body

(* Serve connections accepted on [socket] until [sw] ends. *)
let run ~sw ~clock ~socket (cfg : Bearer.config) =
  let mcp = server () in
  let config = Mcp_http.config ~clock ~authenticate:(Bearer.authenticate cfg) () in
  let metadata = Json.to_string (Bearer.metadata cfg ~scopes) in
  let scope = Local.scope sw in
  let connection flow addr =
    Eio.Switch.run @@ fun sw ->
    let request_handler _ ({ reqd; _ } : Httpun.Reqd.t Gluten.reqd) =
      let request = Httpun.Reqd.request reqd in
      let path = List.hd (String.split_on_char '?' request.target) in
      match (request.meth, path) with
      | `GET, p when p = metadata_path ->
          respond reqd ~headers:[ ("content-type", "application/json") ] `OK metadata
      | _, "/mcp" ->
          Eio.Fiber.fork ~sw (fun () ->
              Mcp_http.handle config mcp ~scope (Mcp_http.Http1.exchange reqd))
      | _ -> respond reqd `Not_found ""
    in
    let error_handler _ ?request:_ _ start =
      Httpun.Body.Writer.close (start Httpun.Headers.empty)
    in
    Httpun_eio.Server.create_connection_handler ~sw ~request_handler ~error_handler addr flow
  in
  Eio.Net.run_server ~on_error:ignore socket connection
