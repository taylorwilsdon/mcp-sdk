# Weather quickstart

A complete MCP server in one file: a `get_weather` tool that looks up a city with the
[Open-Meteo](https://open-meteo.com) geocoding API and returns its current conditions, as text
for the model and as structured content.

```
ocaml/weather.ml     the server, built with this SDK
python/weather.py    the same server in FastMCP, for comparison
bench/               the load driver behind the numbers below
```

## Run it

Commands here run from the repository root. `dune exec` resolves the path against the
current directory, so from `examples/weather` it is `./ocaml/weather.exe`.

```sh
dune exec examples/weather/ocaml/weather.exe                 # stdio
dune exec examples/weather/ocaml/weather.exe -- --http 8080  # Streamable HTTP at /mcp
```

To use it from Claude Code, over stdio or over HTTP with the server above running:

```sh
dune build
claude mcp add weather -- "$PWD/_build/default/examples/weather/ocaml/weather.exe"
claude mcp add --transport http weather http://localhost:8080/mcp
```

Or talk to it directly. There is no `initialize` handshake in protocol 2026-07-28: every
request carries its own `_meta`.

```sh
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}},"name":"get_weather","arguments":{"city":"Berlin"}}}' \
  | (cat; sleep 2) | ./_build/default/examples/weather/ocaml/weather.exe
```

```
Berlin, Germany: 9.3°C (feels like 6.1°C), Mainly clear, humidity 70%, wind 12.7 km/h
```

Over HTTP the same request also mirrors the method, tool name and version in headers, which
the server checks against the body (-32020 when one is missing or disagrees):

```sh
curl -s http://localhost:8080/mcp \
  -H 'content-type: application/json' -H 'accept: application/json, text/event-stream' \
  -H 'mcp-protocol-version: 2026-07-28' -H 'mcp-method: tools/call' -H 'mcp-name: get_weather' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}},"name":"get_weather","arguments":{"city":"Berlin"}}}'
```

## How it works

The tool's arguments are a `Schema` value. The same value produces the `inputSchema` clients
see in `tools/list` and the decoder that hands the handler a typed
``(string * [`Celsius | `Fahrenheit])``:

```ocaml
Schema.(
  let+ city = req "city" string ~doc:"City name, e.g. Berlin"
  and+ units = dft "units" (enum [ ("celsius", `Celsius); ("fahrenheit", `Fahrenheit) ]) `Celsius in
  (city, units))
```

A bad `units` value never reaches the handler; it is answered with -32602. The Open-Meteo
responses are decoded with the same combinators, straight into records. Failures the model
should see (unknown city, upstream down) are returned with `Tool.error`.

Upstream requests reuse keep-alive connections from an `Eio.Pool`. Eio ties a connection to the
switch it was opened under, and a switch to one domain, so the pools are `Local` values: one
server serves every domain, and each domain makes pools of its own the first time a request
there needs them.

Set `OPEN_METEO_GEOCODING_URL` and `OPEN_METEO_FORECAST_URL` to point the server at another
host, as the benchmark does.

## OCaml vs FastMCP

Both servers implement the same contract, and both speak MCP 2026-07-28 statelessly. That means
one driver, this SDK's own client, loads them over the same code path. The Python side is
written the way a FastMCP user would write it: FastMCP 4.1.1 on CPython 3.12.9, one pooled
`httpx.AsyncClient` from the lifespan, and stateless JSON-response HTTP. Both servers reuse
upstream connections.

Upstream calls go to a mock Open-Meteo server running in the driver, so the numbers measure
the servers and not the network. Each call makes two upstream GETs. Apple M4 Max with 14
cores, macOS 15.5, OCaml 5.4.1. Each throughput cell is one 2-second run after a short
warm-up, so treat the ratios as indicative, not precise.

| | OCaml | FastMCP | |
| --- | ---: | ---: | ---: |
| Cold start (spawn to first `tools/list`) | 4.4 ms | 404.5 ms | 92x |
| Idle memory (RSS) | 10.9 MB | 92.7 MB | 8.5x |
| stdio `get_weather`, 1 in flight | 1029 req/s, p50 0.95 ms | 529 req/s, p50 1.87 ms | 1.9x |
| stdio `get_weather`, 64 in flight | 13167 req/s, p99 6.91 ms | 689 req/s, p99 152.92 ms | 19x |
| HTTP `get_weather`, 1 in flight | 2038 req/s, p50 0.49 ms | 556 req/s, p50 1.78 ms | 3.7x |
| HTTP `get_weather`, 64 in flight | 9337 req/s, p99 9.08 ms | 213 req/s, p99 743.01 ms | 44x |
| HTTP `get_weather`, 64 in flight, `--domains 4` | 7377 req/s, p99 10.29 ms | n/a | |
| stdio `tools/list`, 64 in flight (no upstream) | 86483 req/s, p99 0.94 ms | 2598 req/s, p99 37.20 ms | 33x |
| Server CPU per `get_weather` (stdio, 64 in flight) | 73 us | 1263 us | 17x |
| Server CPU per `tools/list` | 5 us | 333 us | 63x |

The driver stops at the first failed call, so every call in every cell succeeded.

### What this means

- **One call at a time over a real network, the gap disappears.** OCaml saves about 1 ms per
  call, and a real Open-Meteo round trip takes 50 to 200 ms. A single user chatting with one
  tool will not notice the difference.
- **Startup and memory are where you will notice it.** A 5 ms start and 11 MB footprint make
  per-invocation stdio servers, serverless deployments and many servers per host cheap.
  FastMCP spends 400 ms importing before it can answer anything.
- **Under concurrency OCaml keeps its latency.** At 64 in flight, FastMCP's p99 grows to
  150 ms on stdio and 740 ms on HTTP, while OCaml stays near 10 ms. Eio fibers make blocking
  I/O cheap, and the work after decoding involves no interpreter.
- **Cost per request is 17 to 63 times lower,** which shows up directly as fewer cores at a
  given load. `tools/list` is the extreme case, because the SDK serializes it once per
  registry snapshot.
- **Extra domains do not help this workload yet.** With `--domains 4` the server uses about 4.7
  cores for fewer calls per second than one domain manages on one core. CPU per call rises the
  same way for `tools/list`, which makes no upstream calls, and equally under Eio's own
  `run_server`, so the cost is not specific to this example. A profile shows the domains in
  Eio's `poll` loop and in OCaml 5's stop-the-world minor collections, but the root cause is not
  pinned down yet. One domain already handles 9,000 to 13,000 calls per second, well past what a
  FastMCP process reaches, and a FastMCP process that scales out needs more workers, each with
  its own 90 MB.

### Caveats

- **The comparison uses FastMCP's defaults.** For example, uvicorn's per-request access log
  stays on. Tuning the Python side, for example with uvloop or multiple workers, would narrow
  some throughput rows, but not cold start or memory per process.

### Reproduce

```sh
dune build
cd examples/weather/python && uv sync && cd -
./_build/default/examples/weather/bench/bench.exe   # -seconds 2 -cold 5 by default
```
