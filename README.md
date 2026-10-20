# mcp-sdk: MCP SDK for OCaml 5

A [Model Context Protocol](https://modelcontextprotocol.io) server and client library for
OCaml 5 and Eio. It targets revision **2026-07-28**: stateless requests with no `initialize`
handshake and no sessions, multi round-trip requests (MRTR), `subscriptions/listen` and
cacheable results. Clients and servers still on 2025-11-25 or 2025-06-18 are supported too,
with the same handlers.

It is for OCaml developers who want to expose tools, resources and prompts to MCP hosts
(Claude, IDEs, agents), or call MCP servers from OCaml. Compared with the official Python,
TypeScript, Go and Rust SDKs it is small: no OAuth client, no task extension, no ppx. In
exchange it offers:

- **One schema per tool.** A single `Schema` value produces the advertised `inputSchema` and
  the decoder that hands your handler typed arguments. `Tool.output` does the same for results.
- **A stateless core.** Every request is handled on its own from an immutable registry
  snapshot, so one server can be served from many Eio domains or processes.
- **Bounded resource use.** Readers never wait on application code, admission limits are
  explicit, and per-request buffers are capped.
- **Measured efficiency.** See [Performance](#performance) for a comparison with the official
  Go SDK and FastMCP, including the rows where OCaml is not ahead.

[tmattio/ocaml-mcp](https://github.com/tmattio/ocaml-mcp) is another Eio-based OCaml
implementation, targeting 2025-06-18. This SDK differs in implementing 2026-07-28 natively,
decoding a request straight into a tool's typed arguments in one pass, mapping cancellation onto
Eio fibers, and serving earlier revisions without storing per-client state.

## Installation

You need OCaml 5.2 or newer, opam and dune 3.24 or newer. The package is not on the opam
repository yet; pin it from git:

```sh
opam pin add mcp-sdk git+https://github.com/taylorwilsdon/mcp-sdk.git
```

Then depend on it from your `dune` file. `mcp-sdk.http` is only needed for Streamable HTTP:

```
(executable
 (name server)
 (libraries mcp-sdk mcp-sdk.http eio_main))
```

## A first server

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
intermediate JSON tree. Invalid arguments are answered with -32602 before the handler runs.

Structured results are typed the same way. The output schema and the encoding come from one
value, so they cannot drift apart:

```ocaml
let sum =
  Tool.output
    ~schema:(Json.obj [ ("type", Json.str "object") (* ... *) ])
    Jsont.Object.(map Fun.id |> mem "sum" Jsont.int ~enc:Fun.id |> finish)

let add =
  Tool.v "add" ~output:sum
    Schema.(let+ a = req "a" int and+ b = req "b" int in (a, b))
    (fun _ctx (a, b) -> Ok (Tool.return sum (a + b)))
```

[examples/weather](examples/weather) is a complete server with an upstream HTTP API, connection
pools and a typed result. [bin/main.ml](bin/main.ml) exercises every feature: progress, MRTR
elicitation, resources, templates, prompts and completion.

## Connecting a host

Build, then register the executable as a stdio server:

```sh
dune build
claude mcp add weather -- "$PWD/_build/default/examples/weather/ocaml/weather.exe"
```

Hosts configured with JSON, such as Claude Desktop, take the same command:

```json
{ "mcpServers": { "weather": { "command": "/path/to/_build/default/examples/weather/ocaml/weather.exe" } } }
```

Over HTTP, serve with `Mcp_http.serve ~port` (the examples take `--http PORT`) and register
`http://localhost:PORT/mcp`. To inspect a server interactively, use the
[MCP Inspector](https://github.com/modelcontextprotocol/inspector):

```sh
npx @modelcontextprotocol/inspector ./_build/default/examples/weather/ocaml/weather.exe
```

## A client

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let transport = Mcp_http.Client.http1 ~sw ~net:(Eio.Stdenv.net env) ~port:8080 () in
  (* `Auto speaks 2026-07-28, and falls back to initialize for earlier servers. *)
  let client = Mcp.Client.connect ~protocol:`Auto transport in
  match Mcp.Client.call_tool client "get_weather" (Mcp.Json.obj [ ("city", Mcp.Json.str "Berlin") ]) with
  | Ok result -> List.iter (function Mcp.Proto.Content.Text t -> print_endline t.text | _ -> ()) result.content
  | Error e -> Format.eprintf "%a@." Mcp.Client.pp_error e
```

Over stdio, `Mcp.Client.line_transport ~sw source sink` connects to a child process's pipes.
Multi round-trip requests are driven through `~on_input`, which answers elicitation, sampling
and roots requests whichever revision the server speaks.

## Compatibility

| Revision | Server | Client |
| --- | --- | --- |
| 2026-07-28 | Native: stateless, `_meta` per request | `~protocol:`Modern` (default) |
| 2025-11-25 | Through `initialize`, with a session carried by the client | `~protocol:`Legacy` or `` `Auto `` |
| 2025-06-18 | As 2025-11-25 (no URL elicitation) | As 2025-11-25 |

Earlier revisions only deliver log messages, list changes and resource updates outside a
request. This server never sends anything outside a request, so those features, and
`subscriptions/listen` on the client, are 2026-07-28 only. For earlier clients, handlers are
still written once for MRTR: the server puts each input request to the client itself. Structured
results that are not objects are wrapped as `{"result": value}`, with the advertised output
schema wrapped to match, since earlier revisions require objects.

### Conformance

[examples/conformance](examples/conformance) runs the official
[conformance suite](https://github.com/modelcontextprotocol/conformance) (0.2.0-alpha.12) against
this SDK. It tests the server and the client, each at each revision's own wire version, and
checks every message against that revision's JSON schema. The server is exercised by the suite's
own independent client. The client is exercised by the suite's scenario servers, including one
built on the official TypeScript SDK. CI runs it on every push. These are the scenarios each
revision's frozen requirement set includes:

| Revision | Role | Required | Passed | Not passed, by design |
| --- | --- | ---: | ---: | --- |
| 2026-07-28 | server | 37 | 37 | |
| 2026-07-28 | client | 32 | 7 | 25 OAuth scenarios |
| 2025-11-25 | server | 30 | 26 | 4: logging and resource subscriptions, which need messages outside a request |
| 2025-11-25 | client | 18 | 3 | 14 OAuth scenarios, SSE stream resumption |

Every scenario that does not pass is listed with its reason in
`examples/conformance/baseline-<revision>.yml`. The suite also runs scenarios that a revision
does not score: the tasks extension (not provided) and a pending JSON Schema 2020-12 keyword
scenario fail, and the rest pass. To reproduce, with Node 22 or newer:

```sh
examples/conformance/run.sh     # writes _conformance/ and prints the table above
```

## Performance

[examples/weather](examples/weather/README.md#benchmarks) compares the same `get_weather` server in this
SDK, the official Go SDK and FastMCP. Each is measured both in its default configuration and in
one matched to the OCaml server (one core, no access log). The table there reports medians of
repeated runs and links the raw samples. In short:

- **Concurrency and cost per call favour OCaml.** At 64 calls in flight, the OCaml server serves
  1.4 to 2.6 times as many requests per second as Go on one core or on all cores, at a lower p99
  and less CPU per call. Against FastMCP it serves 15 to 24 times as many.
- **One call at a time, Go is faster.** Its per-request latency at one in flight is lower than
  OCaml's over both stdio and HTTP. Startup (about 5 ms) and memory (about 10 MB) are on a par
  with Go's.
- **Extra domains do not speed this workload up yet.** The server is domain-safe and serves from
  several domains, but on this benchmark `--domains 4` gives less throughput than one domain.
  Treat multicore as supported, not as a measured speed-up.

The core benchmark measures decode, dispatch and encode without a transport:

```sh
dune exec bench/bench.exe -- -n 50000 -d 3
```

## Deployment

[examples/auth](examples/auth) is a tested resource server behind bearer authentication. It
serves protected resource metadata (RFC 9728), answers missing or invalid tokens with a 401
whose `WWW-Authenticate` challenge points at that metadata, verifies JWTs for issuer, audience
and expiry, and checks scopes per tool. Its `authenticate` hook is the
`Mcp_http.config ~authenticate` seam: replace its token verification with your identity
provider's.

Streamable HTTP is served over HTTP/1.1. Terminate TLS and HTTP/2 at a reverse proxy that
closes the upstream request when its client goes away, so cancellation carries through:

```
# Caddy
mcp.example.com {
  reverse_proxy localhost:8080 {
    flush_interval -1
  }
}
```

```
# nginx
location /mcp {
  proxy_pass http://127.0.0.1:8080;
  proxy_http_version 1.1;
  proxy_buffering off;          # stream SSE events as they are written
  proxy_read_timeout 1h;        # long-running tools and subscriptions
}
```

Behind a proxy, Origin checks still apply: pass `~allow_origin` for the origins your clients
use. Set `~state_key` when several processes serve the same clients, so the MRTR state one
process seals verifies in another.

## Design

Revision 2026-07-28 made every request self-describing: version, client capabilities and
identity travel in `_meta`. The SDK is built around that.

- **Immutable snapshots, atomic updates.** A server's registry is an immutable snapshot behind
  an `Atomic.t`. Requests on any fiber or domain read it without locks; `Server.add_tool` and
  the other setters swap in a new snapshot with compare-and-set. Some state is deliberately
  shared by every domain, always through atomics: the snapshot, the subscription hub, the
  answers awaited from clients of earlier revisions, and the `requestState` key. Nothing is kept
  per client: an earlier revision's session travels with the client.
- **Domain-local resources are declared, not captured.** A handler's closure must be safe on
  any domain. A resource tied to one, such as a pool of upstream connections (Eio switches
  belong to a domain), is a `Local.v (fun sw -> ...)` read with `Ctx.local`: each serving
  domain makes its own on first use, under a switch that lasts as long as it serves.
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
  sealed with HMAC-SHA256 over the payload, an expiry, and the method, target, authenticated
  principal and a canonical digest of the original arguments. It comes back verified through
  `Ctx.state`, and a tampered, expired or replayed-elsewhere state is rejected with -32602.
  Input requests the client has not declared a capability for are refused with -32021.
- **Subscriptions that never block producers.** Changes bump counters in one atomic hub
  record and broadcast an `Eio.Condition`. Each `subscriptions/listen` stream diffs against
  what it last saw, so bursts coalesce and a slow listener cannot stall anyone. Only URIs an
  open subscription watches are tracked.
- **Bounded by policy, not by luck.** Readers never wait, so responses and cancellations
  always get through. Admission is explicit: stdio refuses requests beyond `max_requests` at
  once, and HTTP accepts at most `max_connections`, one request each. Output waits for the
  peer, through stdio's 1MiB high-water mark and per-event SSE flushes. On the client,
  notifications wait for their callbacks in a queue of at most `max_queued` bytes per
  request, and an HTTP response or SSE event may hold at most `max_response` bytes.
- **Failures stay on the server.** An exception a handler raises reaches the
  `Server.v ~on_exception` hook (stderr by default); the client sees a generic -32603.
- **Transports stay thin.** stdio (or any byte stream) runs one fiber per request and
  maps `notifications/cancelled` to fiber cancellation. Streamable HTTP answers with
  JSON, or upgrades lazily to an SSE stream scoped to the request the first time the
  handler emits. Closing the stream cancels the request. Header mirrors
  (`MCP-Protocol-Version`, `Mcp-Method`, `Mcp-Name`, `Mcp-Param-*` with `=?base64?…?=`)
  are validated against the body (-32020). Requests are checked for allowed origins
  (loopback by default) and an optional bearer hook, and GET or DELETE gets 405.
  `Mcp_http.serve ~domains` runs accept loops on extra domains. A CPU-bound handler can hand
  its work to an `Eio.Executor_pool`, bearing in mind that cancellation does not reach work
  already running there.
- **Embeddable.** `Mcp_http.serve` is a convenience. An application with its own HTTP server
  mounts the endpoint with `Mcp_http.handle` next to its routes and middleware, adapting each
  request to an `Exchange.t` (`Mcp_http.Http1.exchange` does so for httpun-eio). The client
  side takes a `connect` function, so name resolution, Unix sockets or proxies need no new
  transport.
- **Earlier revisions without server state.** `initialize` negotiates 2025-11-25 or
  2025-06-18 and opens a session that is just the client's version, info and capabilities:
  what a 2026-07-28 request carries in `_meta`. stdio keeps it for the connection; over HTTP
  it is the `Mcp-Session-Id`, which the client sends back with every request, so nothing is
  stored. Only the answers to the server's own input requests, POSTed separately, must reach
  the process that asked.
- **An open schema.** `Schema` covers ordinary arguments, `Schema.refine` adds checks and
  conversions that are advertised alongside (`minimum`, `pattern`...), and `Schema.of_jsont`
  brings in any jsont codec (unions, recursive values, existing types) with the schema it
  satisfies.

## Spec coverage

| Feature | Status |
| --- | --- |
| Per-request `_meta` (protocol version, client info/capabilities, progress token, trace context) | ✓ |
| `server/discover`, `UnsupportedProtocolVersionError` | ✓ |
| Tools (typed arguments and results, annotations, `outputSchema`, structured content, icons) | ✓ |
| Resources, RFC 6570 templates (`{var}`, `{+var}`), not found as -32602 | ✓ |
| Prompts with required argument checks | ✓ |
| Completion for prompts and templates | ✓ |
| MRTR: `InputRequiredResult`, `inputResponses`, sealed `requestState`, form and URL elicitation | ✓ |
| `subscriptions/listen` with acknowledgement, list changes, resource updates, graceful end | ✓ |
| Cacheable results (`ttlMs`, `cacheScope`), pagination | ✓ |
| Progress, cancellation (stdio notification, HTTP stream close) | ✓ |
| Streamable HTTP header validation, `x-mcp-header`, Origin checks, bearer authentication hook | ✓ |
| Client: `_meta`, MRTR loop, TTL cache, cursor loop detection, `x-mcp-header` mirroring and validation | ✓ |
| Client of 2025-11-25 and 2025-06-18 servers: `initialize`, sessions, server requests on the request stream or a GET stream | ✓ |
| 2025-11-25 and 2025-06-18 clients: `initialize`, `ping`, stateless sessions, elicitation and sampling bridged from MRTR | ✓ |
| 2025-11-25 and 2025-06-18 clients: logging, list change notifications, resource subscriptions | Not provided |
| Deprecated: logging, sampling and roots helpers | Not provided (sampling and roots can still be sent with `Input.raw`) |
| SSE stream resumption (`Last-Event-ID`) | Not provided |
| Tasks extension, OAuth client flows | Not provided (an `authenticate` hook supplies the principal; `Mcp_http.Client.v ~headers` sends a token) |

The test suite round-trips hand-written samples of every wire type through its codec without
losing data, decodes a sample of every request method, and runs the client end to end against
the server over stdio pipes and HTTP/1.1, in each revision.

## Limitations

- Streamable HTTP is served over HTTP/1.1 only, as the official SDKs do; see
  [Deployment](#deployment).
- Tool arguments are validated by their `Schema` decoder and results are encoded by their
  `Tool.output` codec. There is no general JSON Schema 2020-12 validator, so a schema given to
  `Schema.of_jsont`, `Schema.refine` or `Tool.output` must agree with the codec it accompanies.
- `Mcp_http.Client.v` connects through httpun-eio, which needs an Eio stream socket; a TLS
  library has to present its connection as one.

## Stability

The API is not stable before 1.0. A minor release may change it, and every change that breaks
code is listed in [CHANGES.md](CHANGES.md) with what to do instead. API documentation is built
from the `.mli` files with `dune build @doc`.

## Development

```sh
dune build
dune test
dune exec mcp-sdk -- --http 8080   # example server; stdio without flags
dune exec bench/bench.exe
examples/conformance/run.sh        # needs Node 22
```
