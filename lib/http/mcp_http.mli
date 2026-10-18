(** Streamable HTTP for MCP 2026-07-28, over HTTP/1.1.

    A single POST endpoint. Each request is answered with JSON, or with an SSE stream scoped to it
    as soon as the handler emits a notification (progress, subscription events). Closing that stream
    cancels the request. There are no sessions, no GET stream and no resumption: GET and DELETE get
    405. Header mirrors ([MCP-Protocol-Version], [Mcp-Method], [Mcp-Name], [Mcp-Param-*]) are
    checked against the body, with -32020 on mismatch. *)

val loopback_origin : string -> bool
(** The default [Origin] policy: only loopback origins, against DNS rebinding. Requests without
    [Origin] are always accepted. *)

val serve :
  ?host:Eio.Net.Ipaddr.v4v6 ->
  ?backlog:int ->
  ?domains:_ Eio.Domain_manager.t * int ->
  ?allow_origin:(string -> bool) ->
  ?authenticate:((string -> string option) -> (string option, string) result) ->
  ?max_body:int ->
  ?keepalive:float ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  port:int ->
  Mcp.Server.t ->
  'a
(** Serve on [host] (default loopback) and [port] until cancelled.

    - [domains] adds accept loops on that many extra domains; connections are independent, so this
      scales across cores.
    - [authenticate header] returns the principal for a request (exposed to handlers as
      {!Mcp.Ctx.principal}, and binding MRTR state), or the [WWW-Authenticate] challenge of a 401.
    - [max_body] bounds request bodies (default 4MiB); [keepalive] is the SSE comment interval in
      seconds (default 15). *)

(** Client transport for {!Mcp.Client.connect}. *)
module Client : sig
  val http1 :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    ?host:Eio.Net.Ipaddr.v4v6 ->
    ?path:string ->
    ?connections:int ->
    port:int ->
    unit ->
    Mcp.Client.transport
  (** A pool of up to [connections] keep-alive connections (default 64), one request in flight on
      each. Cancelling a request drops its connection. *)
end
