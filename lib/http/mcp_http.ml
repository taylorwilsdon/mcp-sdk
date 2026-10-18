(* Streamable HTTP transport for MCP 2026-07-28 over HTTP/1.1. *)

let loopback_origin = Exchange.loopback_origin

let serve ?(host = Eio.Net.Ipaddr.V4.loopback) ?(backlog = 4096) ?domains
    ?(allow_origin = loopback_origin) ?(authenticate = fun _ -> Ok None) ?(max_body = 1 lsl 22)
    ?(keepalive = 15.0) ~net ~clock ~port server =
  let cfg : Exchange.config =
    {
      allow_origin;
      authenticate;
      max_body;
      keepalive;
      clock :> float Eio.Time.clock_ty Eio.Resource.t;
    }
  in
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen ~sw ~backlog ~reuse_addr:true net (`Tcp (host, port)) in
  Eio.Net.run_server ?additional_domains:domains ~on_error:ignore socket
    (Http1.connection_handler cfg server)

module Client = Http_client
