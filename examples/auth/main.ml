(* The notes server behind bearer authentication.

     MCP_AUTH_SECRET=... dune exec examples/auth/main.exe -- --port 8080
     MCP_AUTH_SECRET=... dune exec examples/auth/main.exe -- --mint alice --scope notes:read

   Serve it over HTTPS by terminating TLS in front of it, and set --resource
   to the public URL clients use. *)

open Auth_example

let () =
  let port = ref 8080 and issuer = ref "https://auth.example.com" and resource = ref "" in
  let mint = ref "" and scope = ref "" in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "PORT  Listen on PORT (default 8080)");
      ("--issuer", Arg.Set_string issuer, "URL  The authorization server tokens come from");
      ("--resource", Arg.Set_string resource, "URL  This server's public URL (default localhost)");
      ("--mint", Arg.Set_string mint, "SUB  Print a token for SUB instead of serving");
      ("--scope", Arg.Set_string scope, "SCOPES  Space-separated scopes of the minted token");
    ]
    ignore "main [--port PORT] [--issuer URL] [--resource URL] [--mint SUB --scope SCOPES]";
  let secret =
    match Sys.getenv_opt "MCP_AUTH_SECRET" with
    | Some s when String.length s >= 32 -> s
    | _ -> failwith "Set MCP_AUTH_SECRET to the token signing secret, at least 32 bytes"
  in
  let base = if !resource = "" then Printf.sprintf "http://localhost:%d" !port else !resource in
  let cfg : Bearer.config =
    { secret; issuer = !issuer; resource = base ^ "/mcp"; metadata_url = base ^ App.metadata_path }
  in
  if !mint <> "" then print_endline (Bearer.sign cfg ~scope:!scope !mint)
  else
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let socket =
      Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
    in
    App.run ~sw ~clock:(Eio.Stdenv.clock env) ~socket cfg
