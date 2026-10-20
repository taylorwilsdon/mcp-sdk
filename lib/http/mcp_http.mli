(** Streamable HTTP for MCP 2026-07-28, over HTTP/1.1.

    A single POST endpoint. Each request is answered with JSON, or with an SSE stream scoped to it
    as soon as the handler emits a notification (progress, subscription events). Closing that stream
    cancels the request. There is no GET stream and no resumption: GET and DELETE get 405. Header
    mirrors ([MCP-Protocol-Version], [Mcp-Method], [Mcp-Name], [Mcp-Param-*]) are checked against
    the body, with -32020 on mismatch.

    Clients of 2025-11-25 and 2025-06-18 get an [Mcp-Session-Id] from [initialize]. It encodes the
    session itself, so the server stores nothing. Their answers to elicitation and sampling requests
    are POSTed back, get 202, and must reach the process that asked.

    {!serve} hosts the endpoint on its own. An application with an HTTP server of its own mounts it
    with {!handle} instead, next to its other routes and behind its own middleware. *)

type config
(** The endpoint's request policy. *)

val loopback_origin : string -> bool
(** The default [Origin] policy: only loopback origins, against DNS rebinding. Requests without
    [Origin] are always accepted. *)

val config :
  ?allow_origin:(string -> bool) ->
  ?authenticate:((string -> string option) -> (string option, string) result) ->
  ?max_body:int ->
  ?keepalive:float ->
  clock:_ Eio.Time.clock ->
  unit ->
  config
(** - [allow_origin] decides which [Origin]s may call (default {!loopback_origin}).
    - [authenticate header] returns the principal for a request (exposed to handlers as
      {!Mcp.Ctx.principal}, and binding MRTR state), or the [WWW-Authenticate] challenge of a 401.
    - [max_body] bounds request bodies (default 4MiB); [keepalive] is the SSE comment interval in
      seconds (default 15). *)

val serve :
  ?host:Eio.Net.Ipaddr.v4v6 ->
  ?backlog:int ->
  ?domains:_ Eio.Domain_manager.t * int ->
  ?max_connections:int ->
  ?allow_origin:(string -> bool) ->
  ?authenticate:((string -> string option) -> (string option, string) result) ->
  ?max_body:int ->
  ?keepalive:float ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  port:int ->
  Mcp.Server.t ->
  'a
(** Serve on [host] (default loopback) and [port] until cancelled, with the policy of {!config}.

    - [domains] adds accept loops on that many extra domains. They all serve the same server, so
      registry changes, subscriptions and [requestState] keys are shared, while each domain makes
      its own {!Mcp.Local} values, which last as long as [serve].
    - [max_connections] bounds the connections open at once, over all domains (default 1024). Since
      each connection carries one request at a time, it also bounds the requests in progress;
      further connections wait in the listen backlog. *)

(** {1 Embedding} *)

(** One HTTP request, as an HTTP server presents it. {!Http1.exchange} adapts httpun-eio; an adapter
    for another server fills in the same fields. *)
module Exchange : sig
  type writer = {
    write : string -> unit;
    flush : unit -> [ `Written | `Closed ];
        (** Wait until what was written has been passed on, or the peer has gone away. *)
    close : unit -> unit;
  }

  type t = {
    meth : string;
    header : string -> string option;  (** Case-insensitive. *)
    read_body : max:int -> string option;  (** [None] when over [max] bytes. *)
    respond : status:int -> (string * string) list -> Mcp.Wire.t -> unit;
    stream : status:int -> (string * string) list -> writer;
        (** Start a streamed response, e.g. with chunked encoding. *)
  }
end

val handle : config -> Mcp.Server.t -> scope:Mcp.Local.scope -> Exchange.t -> unit
(** [handle config server ~scope x] answers one request to the MCP endpoint, returning once the
    response is complete. [scope] holds the {!Mcp.Local} values of the calling domain; make one per
    domain with {!Mcp.Local.scope}. Run each request on a fiber of its own: cancelling it cancels
    the MCP request, so running it under the connection's switch makes a dropped connection cancel
    it. *)

module Http1 : sig
  val exchange : Httpun.Reqd.t -> Exchange.t
  (** An httpun-eio request as an exchange, for an httpun-eio [request_handler]. *)
end

(** Client transport for {!Mcp.Client.connect}. *)
module Client : sig
  val v :
    sw:Eio.Switch.t ->
    ?path:string ->
    ?connections:int ->
    ?max_queued:int ->
    authority:string ->
    (Eio.Switch.t -> _ Eio.Net.stream_socket) ->
    Mcp.Client.transport
  (** [v ~sw ~authority connect] keeps a pool of up to [connections] keep-alive connections (default
      64), one request in flight on each, each opened with [connect sw]. That decides how to reach
      the server: resolving a host name, a Unix domain socket, a proxy, socket options. [authority]
      is the [Host] header and [path] the endpoint (default ["/mcp"]). Cancelling a request drops
      its connection. [max_queued] bounds each request's queue of notifications (default 4MiB), as
      {!Mcp.Client.transport} describes. *)

  val http1 :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    ?host:Eio.Net.Ipaddr.v4v6 ->
    ?path:string ->
    ?connections:int ->
    ?max_queued:int ->
    port:int ->
    unit ->
    Mcp.Client.transport
  (** {!v} over TCP to [host] (default loopback) and [port]. *)
end
