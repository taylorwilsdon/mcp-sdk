# mcp-sdk

A Model Context Protocol SDK for OCaml 5, built on Eio, targeting protocol revision
**2026-07-28**: the stateless revision with no `initialize` handshake, no sessions, multi
round-trip requests (MRTR), `subscriptions/listen` and cacheable results.

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
that turns `arguments` straight into `(string * [`C | `F])`. There is no ppx and no
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
  the other setters swap in a new snapshot with compare-and-set.
- **Decode is dispatch.** Each snapshot compiles a single jsont codec for the whole
  JSON-RPC envelope. The `method` selects the parameter codec, and for `tools/call` the
  tool `name` selects that tool's typed argument decoder. One streaming pass yields the
  handler already applied to its arguments. Errors take a cold path that probes the
  message to pick the right JSON-RPC code (-32700, -32600, -32601, -32602, -32022) and
  recover the id.
- **Preserialized cacheable results.** `tools/list`, `prompts/list`, `resources/list`,
  `resources/templates/list` (one string per page) and `server/discover` are encoded once
  per snapshot, in deterministic order with `ttlMs`/`cacheScope` and `serverInfo`.
  Responses are written as fragments around the request id, so a cached payload is never
  copied into a per-request string.
- **MRTR without server state.** Handlers return `Input.required`. Any `requestState` is
  sealed with HMAC-SHA256 over the payload, an expiry, and the method, target and
  authenticated principal. It comes back verified through `Ctx.state`, and a tampered,
  expired or replayed-elsewhere state is rejected with -32602. Input requests the client
  has not declared a capability for are refused with -32021.
- **Subscriptions that never block producers.** Changes bump counters in one atomic hub
  record and broadcast an `Eio.Condition`. Each `subscriptions/listen` stream diffs
  against what it last saw, so bursts coalesce and a slow listener cannot stall anyone.
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

## Spec coverage

| Feature | Status |
| --- | --- |
| Per-request `_meta` (protocol version, client info/capabilities, progress token, trace context) | ✓ |
| `server/discover`, `UnsupportedProtocolVersionError`, `initialize` answered with the supported versions | ✓ |
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
| Legacy `initialize`/session protocol (2025-11-25 and earlier) | Not provided |
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
- The official `@modelcontextprotocol/conformance` suite (0.1.16) still speaks 2025-11-25
  only, so it cannot drive a modern-only server yet.
- Tool arguments are validated by their `Schema` decoder. There is no general JSON Schema
  2020-12 validator.

## Development

```sh
dune build
dune test
dune exec mcp-sdk -- --http 8080   # example server; stdio without flags
dune exec bench/bench.exe
```
