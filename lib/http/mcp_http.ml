(* Streamable HTTP transport for MCP 2026-07-28 over HTTP/1.1. *)

type config = Exchange.config

let loopback_origin = Exchange.loopback_origin
let config = Exchange.config

module Exchange = Exchange

let handle = Exchange.handle

module Http1 = Http1

let serve ?(host = Eio.Net.Ipaddr.V4.loopback) ?(backlog = 4096) ?domains ?(max_connections = 1024)
    ?allow_origin ?authenticate ?max_body ?keepalive ~net ~clock ~port server =
  let cfg = config ?allow_origin ?authenticate ?max_body ?keepalive ~clock () in
  let connections = Eio.Semaphore.make max_connections in
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen ~sw ~backlog ~reuse_addr:true net (`Tcp (host, port)) in
  (* One accept loop per domain, each with a scope for that domain's Local
     values. As in Eio.Net.run_server, [connections] limits them together. *)
  let accept () =
    Eio.Switch.run @@ fun sw ->
    let handler = Http1.connection_handler cfg server ~scope:(Mcp.Local.scope sw) in
    let rec loop () =
      Eio.Semaphore.acquire connections;
      Eio.Net.accept_fork ~sw socket ~on_error:ignore (fun flow addr ->
          Fun.protect
            ~finally:(fun () -> Eio.Semaphore.release connections)
            (fun () -> handler flow addr));
      loop ()
    in
    loop ()
  in
  Option.iter
    (fun (mgr, n) ->
      for _ = 1 to n do
        Eio.Fiber.fork ~sw (fun () -> Eio.Domain_manager.run mgr accept)
      done)
    domains;
  accept ()

module Client = Http_client
