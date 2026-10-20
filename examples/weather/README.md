# Weather quickstart

A complete MCP server in one file: a `get_weather` tool that looks up a city with the
[Open-Meteo](https://open-meteo.com) geocoding API and returns its current conditions, as text
for the model and as structured content.

```
ocaml/weather.ml     the server, built with this SDK
go/main.go           the same server on the official Go SDK, for comparison
python/weather.py    the same server in FastMCP, for comparison
bench/               the load driver behind the numbers below
results/             the tables below and every sample behind them
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

A bad `units` value never reaches the handler; it is answered with -32602. The result is typed
the same way: `Tool.output` pairs a record codec with the `outputSchema` it satisfies, and
`Tool.return` encodes the record as structured content. The Open-Meteo
responses are decoded with the same combinators, straight into records. Failures the model
should see (unknown city, upstream down) are returned with `Tool.error`.

Upstream requests reuse keep-alive connections from an `Eio.Pool`. Eio ties a connection to the
switch it was opened under, and a switch to one domain, so the pools are `Local` values: one
server serves every domain, and each domain makes pools of its own the first time a request
there needs them.

Set `OPEN_METEO_GEOCODING_URL` and `OPEN_METEO_FORECAST_URL` to point the server at another
host, as the benchmark does.

## Benchmarks

The same contract is implemented three times: with this SDK, with the official
[Go SDK](https://github.com/modelcontextprotocol/go-sdk) 1.8.0 on Go 1.25, and with FastMCP
4.1.1 on CPython 3.12.9. Each is written the way a user of that SDK would write it: typed
arguments, a typed result with its output schema (a record codec, a Go struct, a Pydantic
model), one pooled upstream HTTP client, and stateless HTTP answering with JSON. All three speak
MCP 2026-07-28 statelessly, so one driver, this SDK's own client, loads them over the same code
path.

Each SDK runs in two configurations:

- **Default**, as it ships: Go uses every core, and FastMCP keeps uvicorn's per-request access
  log.
- **Matched** to the OCaml server, which serves on one domain and logs nothing per request: Go
  with `GOMAXPROCS=1`, FastMCP with `--no-access-log`.

The validation is not identical. Pydantic and the Go SDK check results against their schema at
run time; the OCaml result conforms by construction, as it is encoded by the codec its schema
describes.

Upstream calls go to a mock Open-Meteo running in the driver on domains of its own, so the
numbers measure the servers and not the network. Each call makes two upstream GETs. Every
throughput cell is five 2-second samples on one server process after a warm-up: the median
first, then the range, with the median latency. Cold start is the median of five spawns. Apple
M4 Max with 14 cores, macOS 15.5, OCaml 5.4.1 without flambda. The driver stops at the first
failed call, so every call in every cell succeeded. Every sample is in
[results/samples.csv](results/samples.csv).

| | OCaml | Go | FastMCP | Go, 1 core | FastMCP, no access log |
| --- | ---: | ---: | ---: | ---: | ---: |
| Cold start (spawn to first `tools/list`) | 4.2 ms | 5.8 ms | 399.1 ms | 4.6 ms | 397.6 ms |
| Idle memory (RSS) | 11.0 MB | 10.2 MB | 91.3 MB | 9.9 MB | 91.6 MB |
| stdio `get_weather`, 1 in flight | 1021 req/s (1017-1031), p50 0.94 ms | 2651 req/s (2607-2671), p50 0.34 ms | 581 req/s (518-585), p50 1.69 ms | 2046 req/s (2008-2054), p50 0.47 ms | 597 req/s (532-600), p50 1.67 ms |
| stdio `get_weather`, 64 in flight | 10331 req/s (8952-13218), p99 8.47 ms | 7198 req/s (6631-7308), p99 9.96 ms | 671 req/s (660-684), p99 156.26 ms | 3952 req/s (3817-4044), p99 33.73 ms | 685 req/s (683-688), p99 154.84 ms |
| HTTP `get_weather`, 1 in flight | 1963 req/s (1937-2026), p50 0.50 ms | 2416 req/s (2363-2436), p50 0.37 ms | 552 req/s (548-554), p50 1.79 ms | 2792 req/s (2758-2812), p50 0.34 ms | 558 req/s (552-561), p50 1.77 ms |
| HTTP `get_weather`, 64 in flight | 7963 req/s (6872-9345), p99 10.89 ms | 5372 req/s (5247-6475), p99 22.44 ms | 335 req/s (219-346), p99 537.43 ms | 4713 req/s (4683-4731), p99 55.60 ms | 346 req/s (220-356), p99 484.54 ms |
| HTTP `get_weather`, 64 in flight, `--domains 4` | 7504 req/s (7484-7565), p99 10.40 ms | n/a | n/a | n/a | n/a |
| stdio `tools/list`, 64 in flight (no upstream) | 44810 req/s (44498-44947), p99 1.62 ms | 15225 req/s (15131-15386), p99 4.74 ms | 2522 req/s (2512-2524), p99 37.39 ms | 9999 req/s (9888-10281), p99 15.35 ms | 2557 req/s (2541-2570), p99 36.35 ms |
| Server CPU per `get_weather` (stdio, 64 in flight) | 94 us | 660 us | 1319 us | 130 us | 1285 us |
| Server CPU per `tools/list` | 8 us | 279 us | 343 us | 68 us | 338 us |

| OCaml vs | Go | FastMCP | Go, 1 core | FastMCP, no access log |
| --- | ---: | ---: | ---: | ---: |
| Cold start (spawn to first `tools/list`) | 1.4x | 94x | 1.1x | 94x |
| Idle memory (RSS) | 0.93x | 8.3x | 0.9x | 8.4x |
| stdio `get_weather`, 1 in flight | 0.39x | 1.8x | 0.5x | 1.7x |
| stdio `get_weather`, 64 in flight | 1.4x | 15x | 2.6x | 15x |
| HTTP `get_weather`, 1 in flight | 0.81x | 3.6x | 0.7x | 3.5x |
| HTTP `get_weather`, 64 in flight | 1.5x | 24x | 1.7x | 23x |
| HTTP `get_weather`, 64 in flight, `--domains 4` | n/a | n/a | n/a | n/a |
| stdio `tools/list`, 64 in flight (no upstream) | 2.9x | 18x | 4.5x | 18x |
| Server CPU per `get_weather` (stdio, 64 in flight) | 7x | 14x | 1.4x | 14x |
| Server CPU per `tools/list` | 35x | 43x | 8.6x | 43x |

The second table is how many times better OCaml is in each row (above 1x) or worse (below 1x).

### What this means

- **One call at a time, Go is faster.** Its per-call latency is about a third of OCaml's over
  stdio and two thirds over HTTP, on one core or many. Over a real network neither difference
  matters: an Open-Meteo round trip takes 50 to 200 ms.
- **Under concurrency, OCaml serves the most calls and keeps its latency.** At 64 in flight it
  serves 1.4 to 2.6 times what Go does on stdio, at a lower p99, and FastMCP's p99 grows to
  150 ms on stdio and about 500 ms on HTTP. OCaml's own throughput varies most between samples
  in these rows, so read its medians with their ranges.
- **Cost per call is lowest in OCaml.** Server CPU per `get_weather` is 94 us, against 130 us for
  Go on one core, 660 us for Go on all cores, and about 1300 us for FastMCP. `tools/list` is the
  extreme case, because the SDK serializes it once per registry snapshot.
- **Startup and memory are on a par with Go**, at about 5 ms and 10 MB. FastMCP spends 400 ms
  importing before it can answer anything and holds 90 MB per process.
- **Turning off FastMCP's access log changes little.** Its matched and default results are
  within noise of each other, so the log was not what set it apart.
- **Extra domains do not help this workload yet.** With `--domains 4` the server serves fewer
  calls per second than one domain does. CPU per call rises the same way for `tools/list`,
  which makes no upstream calls, and equally under Eio's own `run_server`, so the cost is not
  specific to this example. A profile shows the domains in Eio's `poll` loop and in OCaml 5's
  stop-the-world minor collections, but the root cause is not pinned down yet. The SDK is
  domain-safe; a speed-up from more domains is not demonstrated.

### Caveats

- One mock and one machine. A workload with real CPU work in the handler, such as parsing,
  search or policy evaluation, would say more about where the language matters than this
  I/O-bound one; it is not measured here.
- Tuning beyond the matched configurations, for example uvloop or several workers for FastMCP,
  or more domains for a CPU-bound OCaml handler, would change some throughput rows, but not cold
  start or memory per process.

### Reproduce

```sh
dune build
(cd examples/weather/python && uv sync)
(cd examples/weather/go && go build -o weather .)
./_build/default/examples/weather/bench/bench.exe -raw examples/weather/results/samples.csv
```

The defaults are `-seconds 2 -repeat 5 -cold 5`.
