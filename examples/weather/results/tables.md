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
