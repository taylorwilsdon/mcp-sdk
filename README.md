# mcp-sdk

A Model Context Protocol SDK for OCaml 5, built on Eio, targeting the latest MCP server protocol - **2026-07-28** as of writing. Full support for true stateless with no `initialize` handshake, no sessions, multi
round-trip requests (MRTR), `subscriptions/listen` and cacheable results. Clients still on
2025-11-25 or 2025-06-18 are served through `initialize` with the same handlers, and the server
still keeps no state per client.

```ocaml
open Mcp

let weather =
  Tool.v "get_weather" ~description:"Current weather for a city"
    Schema.(
      let+ city = req "city" string ~doc:"City name"
      and+ units = dft "units" (enum [ ("c", `C); ("f", `F) ]) `C in
      (city, units))
    (fun _ctx (city, _units) -> Ok (Tool.text ("Sunny in " ^ city)))

let () =
  Eio_main.run @@ fun env ->
  Stdio.run (Server.v ~name:"weather" ~version:"1.0.0" ~tools:[ weather ] ()) env
```

The same `Schema` value produces the `inputSchema` advertised in `tools/list` and the decoder
that turns `arguments` straight into ``(string * [`C | `F])``. There is no ppx and no
intermediate JSON tree.

## Libraries

| Library | Contents |
| --- | --- |
| `mcp-sdk` (`Mcp`) | Protocol types and codecs, `Schema`, `Tool`/`Resource`/`Prompt`, `Server`, the stdio transport, `Client` |
| `mcp-sdk.http` (`Mcp_http`) | Streamable HTTP server and client transports over HTTP/1.1 (httpun) |

## Design

Revision 2026-07-28 made every request self-describing: version, client capabilities and
identity travel in `_meta`. The SDK is built around that.

- **No shared mutable state.** A server is an immutable registry snapshot behind an
  `Atomic.t`. Requests on any fiber or domain read it without locks; `Server.add_tool` and
  the other setters swap in a new snapshot with compare-and-set. One server serves every
  domain, so registry changes, subscriptions and the `requestState` key are shared by
  construction.
- **Domain-local resources are declared, not captured.** A handler's closure must be safe on
  any domain. A resource tied to one, such as a pool of upstream connections (Eio switches
  belong to a domain), is a `Local.v (fun sw -> ...)` read with `Ctx.local`: each serving
  domain makes its own on first use, under a switch that lasts as long as it serves. Adding
  domains is a deployment decision that changes nothing else.
- **Decode is dispatch.** Each snapshot compiles a single jsont codec for the whole
  JSON-RPC envelope. The `method` selects the parameter codec, and for `tools/call` the
  tool `name` selects that tool's typed argument decoder. One streaming pass yields the
  handler ready to run on its arguments. Decoding only runs jsont codecs; conversions
  (`Schema.map`, `Schema.refine`) run when the request executes, on its fiber and under its
  cancellation, so the reader never waits on application code. Errors take a cold path
  that probes the message to pick the right JSON-RPC code (-32700, -32600, -32601, -32602,
  -32022) and recover the id.
- **Preserialized cacheable results.** `tools/list`, `prompts/list`, `resources/list`,
  `resources/templates/list` (one string per page) and `server/discover` are encoded once
  per snapshot, in deterministic order with `ttlMs`/`cacheScope` and `serverInfo`.
  Responses are written as fragments around the request id, so a cached payload is never
  copied into a per-request string.
- **MRTR without server state.** Handlers return `Input.required`. Any `requestState` is
  sealed with HMAC-SHA256 over the payload, an expiry, and the method, target,
  authenticated principal and a canonical digest of the original arguments. It comes back verified through `Ctx.state`, and a tampered,
  expired or replayed-elsewhere state is rejected with -32602. Input requests the client
  has not declared a capability for are refused with -32021.
- **Subscriptions that never block producers.** Changes bump counters in one atomic hub
  record and broadcast an `Eio.Condition`. Each `subscriptions/listen` stream diffs
  against what it last saw, so bursts coalesce and a slow listener cannot stall anyone.
- **Consistent pagination.** Opaque cursors identify the listed contents and page size.
  A registry change that affects the listing invalidates old cursors with -32602;
  clients can restart without a cursor instead of silently skipping entries.
- **Bounded by policy, not by luck.** Readers never wait, so responses and cancellations
  always get through. Admission is explicit: stdio refuses requests beyond `max_requests` at
  once, and HTTP accepts at most `max_connections`, one request each. Output waits for the
  peer, through stdio's 1MiB high-water mark and per-event SSE flushes. Subscriptions coalesce
  changes rather than queue them. On the client, notifications wait for their callbacks in a
  queue of at most `max_queued` bytes per request; a request that overflows it fails and is
  cancelled.
- **Transports stay thin.** stdio (or any byte stream) runs one fiber per request and
  maps `notifications/cancelled` to fiber cancellation. Streamable HTTP answers with
  JSON, or upgrades lazily to an SSE stream scoped to the request the first time the
  handler emits. Closing the stream cancels the request. Header mirrors
  (`MCP-Protocol-Version`, `Mcp-Method`, `Mcp-Name`, `Mcp-Param-*` with `=?base64?…?=`)
  are validated against the body (-32020). Requests are checked for allowed origins
  (loopback by default) and an optional bearer hook, and GET or DELETE gets 405.
  `Mcp_http.serve ~domains` runs accept loops on extra domains. stdio serves its single
  peer on one domain; a CPU-bound handler can hand its work to an `Eio.Executor_pool`,
  bearing in mind that cancellation does not reach work already running there.
- **Embeddable.** `Mcp_http.serve` is a convenience. An application with its own HTTP server
  mounts the endpoint with `Mcp_http.handle` next to its routes and middleware, adapting each
  request to an `Exchange.t` (`Mcp_http.Http1.exchange` does so for httpun-eio). The client
  side takes a `connect` function, so name resolution, Unix sockets or proxies need no new
  transport.
- **Earlier revisions without server state.** `initialize` negotiates 2025-11-25 or
  2025-06-18 and opens a session that is just the client's version, info and capabilities:
  what a 2026-07-28 request carries in `_meta`. stdio keeps it for the connection; over HTTP
  it is the `Mcp-Session-Id`, which the client sends back with every request, so nothing is
  stored. When a handler returns `Input.required`, the server puts each input request to the
  client as a server-to-client request on that request's stream and runs the handler again
  with the answers. Handlers are written once, for MRTR. Only the answers, POSTed separately,
  must reach the process that asked.
- **An open schema.** `Schema` covers ordinary arguments, `Schema.refine` adds checks and
  conversions that are advertised alongside (`minimum`, `pattern`...), and `Schema.of_jsont`
  brings in any jsont codec (unions, recursive values, existing types) with the schema it
  satisfies.

## Spec coverage

| Feature | Status |
| --- | --- |
| Per-request `_meta` (protocol version, client info/capabilities, progress token, trace context) | ✓ |
| `server/discover`, `UnsupportedProtocolVersionError` | ✓ |
| Tools (typed arguments, annotations, `outputSchema`, structured content, icons) | ✓ |
| Resources, RFC 6570 templates (`{var}`, `{+var}`), not found as -32602 | ✓ |
| Prompts with required argument checks | ✓ |
| Completion for prompts and templates | ✓ |
| MRTR: `InputRequiredResult`, `inputResponses`, sealed `requestState`, form and URL elicitation | ✓ |
| `subscriptions/listen` with acknowledgement, list changes, resource updates, graceful end | ✓ |
| Cacheable results (`ttlMs`, `cacheScope`), pagination | ✓ |
| Progress, cancellation (stdio notification, HTTP stream close) | ✓ |
| Streamable HTTP header validation, `x-mcp-header`, Origin checks | ✓ |
| Client: `_meta`, MRTR loop, TTL cache, cursor loop detection, `x-mcp-header` mirroring and validation | ✓ |
| Deprecated: logging, sampling and roots helpers | Not provided (sampling and roots can still be sent with `Input.raw`) |
| 2025-11-25 and 2025-06-18 clients: `initialize`, `ping`, stateless sessions, elicitation and sampling bridged from MRTR | ✓ |
| 2025-11-25 and 2025-06-18 clients: logging, list change notifications, resource subscriptions | Not provided |
| Tasks extension, OAuth flows | Not provided (an `authenticate` hook supplies the principal) |

The test suite round-trips hand-written samples of every wire type through its codec
without losing data, decodes a sample of every request method, and runs end to end over
stdio pipes and HTTP/1.1.

## Performance

`dune exec bench/bench.exe -- -n 50000 -d 3` on a 14-core Apple Silicon laptop, OCaml
5.4.1 without flambda. The client runs in the same process, on one domain:

```
core (decode + dispatch + encode, 50 tools)
  server/discover                      543348 req/s
  tools/list (50 tools, cached)        542117 req/s
  tools/call add                       321770 req/s
  tools/call tool_42 (3 fields)        269112 req/s
transports (50000 calls, 64 in flight, client in-process)
  stdio (pipes)                         84078 req/s   p50   752.9 us   p99   931.0 us
  http/1.1 (keep-alive pool)            34534 req/s   p50  1751.2 us   p99  3478.1 us
```

A cached `tools/list` of 50 tools costs the same as `server/discover`.

## Limitations

- Streamable HTTP is served over HTTP/1.1 only, as the official SDKs do. Terminate TLS
  and HTTP/2 at a reverse proxy; one that closes the upstream request when its client goes
  away (nginx's default) carries cancellation through.
- The official `@modelcontextprotocol/conformance` suite (0.1.16) speaks 2025-11-25. Against
  `examples/conformance` it passes every server scenario except the four in
  `examples/conformance/baseline.yml` (logging and resource subscriptions). The suite
  needs Node 22 or newer:

  ```
  dune exec examples/conformance/conformance.exe -- --port 3000 &
  npx @modelcontextprotocol/conformance@0.1.16 server --url http://localhost:3000/mcp \
    --expected-failures examples/conformance/baseline.yml
  ```
- Tool arguments are validated by their `Schema` decoder. There is no general JSON Schema
  2020-12 validator, so a schema given to `Schema.of_jsont` or `Schema.refine` must agree
  with the codec or check it accompanies.
- `Mcp_http.Client.v` connects through httpun-eio, which needs an Eio stream socket; a TLS
  library has to present its connection as one.

## Development

```sh
dune build
dune test
dune exec mcp-sdk -- --http 8080   # example server; stdio without flags
dune exec bench/bench.exe
```
