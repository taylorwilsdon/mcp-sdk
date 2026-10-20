(* Load driver for the OCaml vs FastMCP table in ../README.md.

   Both servers run as child processes against a mock Open-Meteo, served by
   this process on domains of their own, so the numbers measure the servers
   and not the network. Any failed call aborts the run.

   bench [-seconds S] [-cold N]   (from the repository root) *)

open Mcp

let now = Unix.gettimeofday

(* Mock Open-Meteo: canned responses for Berlin. *)

let geocoding_body =
  {|{"results":[{"id":2950159,"name":"Berlin","latitude":52.52437,"longitude":13.41053,"country":"Germany"}],"generationtime_ms":0.59}|}

let forecast_body =
  {|{"latitude":52.52,"longitude":13.419998,"current":{"time":"2026-10-18T18:00","interval":900,"temperature_2m":9.3,"relative_humidity_2m":81,"apparent_temperature":7.1,"weather_code":3,"wind_speed_10m":11.2}}|}

let mock_request _addr (gluten : Httpun.Reqd.t Gluten.reqd) =
  let reqd = gluten.reqd in
  let body =
    if String.starts_with ~prefix:"/v1/search" (Httpun.Reqd.request reqd).target then geocoding_body
    else forecast_body
  in
  let headers =
    Httpun.Headers.of_list
      [
        ("content-type", "application/json"); ("content-length", string_of_int (String.length body));
      ]
  in
  Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers `OK) body

let mock_connection socket addr =
  let error_handler _ ?request:_ _ respond =
    Httpun.Body.Writer.close (respond Httpun.Headers.empty)
  in
  try
    Eio.Switch.run @@ fun sw ->
    Httpun_eio.Server.create_connection_handler ~sw ~request_handler:mock_request ~error_handler
      addr socket;
    Eio.Switch.fail sw Exit
  with Exit -> ()

(* Serve the mock on three domains of its own; its base URL. *)
let mock ~sw env =
  let dm = Eio.Stdenv.domain_mgr env in
  let socket =
    Eio.Net.listen ~sw ~backlog:4096 ~reuse_addr:true (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Domain_manager.run dm (fun () ->
          Eio.Net.run_server ~additional_domains:(dm, 2) ~on_error:ignore socket mock_connection));
  match Eio.Net.listening_addr socket with
  | `Tcp (_, port) -> Printf.sprintf "http://127.0.0.1:%d" port
  | `Unix _ -> assert false

(* Server processes *)

let devnull ~sw env = Eio.Path.(open_out ~sw ~create:`Never (Eio.Stdenv.fs env / "/dev/null"))

(* [argv] against the mock, with its logs discarded. *)
let spawn ~sw env ~upstream ?stdin ~stdout argv =
  let mgr = Eio.Stdenv.process_mgr env and stderr = devnull ~sw env in
  let env =
    Eio.Process.Env.override
      [ ("OPEN_METEO_GEOCODING_URL", Some upstream); ("OPEN_METEO_FORECAST_URL", Some upstream) ]
      (Eio.Process.environment mgr)
  in
  Eio.Process.spawn ~sw mgr ~env ?stdin ~stdout ~stderr argv

(* RSS in bytes and user+system CPU seconds of [proc], from ps. *)
let ps proc =
  let pid = string_of_int (Eio.Process.pid proc) in
  let ic = Unix.open_process_args_in "ps" [| "ps"; "-o"; "rss=,time="; "-p"; pid |] in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  Scanf.sscanf out " %d %d:%f" (fun rss min sec -> (1024. *. float rss, (60. *. float min) +. sec))

let free_port () =
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind socket (ADDR_INET (Unix.inet_addr_loopback, 0));
  let port = match Unix.getsockname socket with ADDR_INET (_, port) -> port | _ -> assert false in
  Unix.close socket;
  port

let rec wait_port env port =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  try Eio.Switch.run (fun sw -> ignore (Eio.Net.connect ~sw (Eio.Stdenv.net env) addr))
  with Eio.Io _ ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    wait_port env port

(* [f proc client] with [argv] serving [client] over stdio, or over HTTP with
   the extra arguments [http]. The server is killed afterwards. *)
let with_server env ~upstream ?http argv f =
  Eio.Switch.run @@ fun sw ->
  let proc, client =
    match http with
    | None ->
        let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
        let proc = spawn ~sw env ~upstream ~stdin:c2s_r ~stdout:s2c_w argv in
        Eio.Flow.close c2s_r;
        Eio.Flow.close s2c_w;
        (proc, Client.connect (Client.line_transport ~sw s2c_r c2s_w))
    | Some extra ->
        let port = free_port () in
        let argv = argv @ ("--http" :: string_of_int port :: extra) in
        let proc = spawn ~sw env ~upstream ~stdout:(devnull ~sw env) argv in
        wait_port env port;
        let net = Eio.Stdenv.net env in
        (proc, Client.connect (Mcp_http.Client.http1 ~sw ~net ~port ()))
  in
  Fun.protect ~finally:(fun () -> Eio.Process.signal proc Sys.sigkill) (fun () -> f proc client)

(* Calls *)

let fail e = Format.kasprintf failwith "%a" Client.pp_error e

let get_weather client =
  match Client.call_tool client "get_weather" (Json.obj [ ("city", Json.str "Berlin") ]) with
  | Ok r when not r.is_error -> ()
  | Ok r -> failwith (Json.encode Proto.Call_result.codec r)
  | Error e -> fail e

(* A raw request, so the client's list cache is never consulted. *)
let list_tools client =
  match Client.request client "tools/list" [] with Ok _ -> () | Error e -> fail e

type load = { rate : float; p50 : float; p99 : float; cpu : float }

(* [op] from [concurrency] fibers for [seconds], after a warm-up. [cpu] is
   the server's CPU time per call. *)
let load ~seconds proc client ~concurrency op =
  let run seconds =
    let deadline = now () +. seconds in
    let rec fiber acc =
      if now () >= deadline then acc
      else
        let t = now () in
        op client;
        fiber ((now () -. t) :: acc)
    in
    List.concat (Eio.Fiber.List.map fiber (List.init concurrency (Fun.const [])))
  in
  ignore (run 0.5);
  let _, cpu0 = ps proc in
  let t0 = now () in
  let latencies = Array.of_list (run seconds) in
  let elapsed = now () -. t0 in
  let _, cpu1 = ps proc in
  Array.sort Float.compare latencies;
  let calls = float (Array.length latencies) in
  let pct p = latencies.(int_of_float (p *. (calls -. 1.))) in
  { rate = calls /. elapsed; p50 = pct 0.5; p99 = pct 0.99; cpu = (cpu1 -. cpu0) /. calls }

(* Table *)

(* One cell: the value compared, whether higher is better, and its text. *)
type cell = { value : float; higher : bool; text : string }

let lower value text = { value; higher = false; text }

let rate (l : load) concurrency =
  let name, v = if concurrency = 1 then ("p50", l.p50) else ("p99", l.p99) in
  {
    value = l.rate;
    higher = true;
    text = Printf.sprintf "%.0f req/s, %s %.2f ms" l.rate name (1e3 *. v);
  }

let cpu (l : load) = lower l.cpu (Printf.sprintf "%.0f us" (1e6 *. l.cpu))
let median l = List.nth (List.sort Float.compare l) (List.length l / 2)

(* The table's rows for the server [argv]; [domains] adds the multicore HTTP
   row. *)
let measure env ~upstream ~seconds ~cold ~domains argv =
  let load = load ~seconds in
  let cold_start () =
    let t0 = now () in
    with_server env ~upstream argv (fun _ client ->
        list_tools client;
        now () -. t0)
  in
  let cold = median (List.init cold (fun _ -> cold_start ())) in
  let rss, c1, c64, list =
    with_server env ~upstream argv @@ fun proc client ->
    list_tools client;
    Eio.Time.sleep (Eio.Stdenv.clock env) 1.;
    let rss, _ = ps proc in
    ( rss,
      load proc client ~concurrency:1 get_weather,
      load proc client ~concurrency:64 get_weather,
      load proc client ~concurrency:64 list_tools )
  in
  let http extra concurrency =
    with_server env ~upstream ~http:extra argv @@ fun proc client ->
    rate (load proc client ~concurrency get_weather) concurrency
  in
  [
    ("Cold start (spawn to first `tools/list`)", lower cold (Printf.sprintf "%.1f ms" (1e3 *. cold)));
    ("Idle memory (RSS)", lower rss (Printf.sprintf "%.1f MB" (rss /. 1e6)));
    ("stdio `get_weather`, 1 in flight", rate c1 1);
    ("stdio `get_weather`, 64 in flight", rate c64 64);
    ("HTTP `get_weather`, 1 in flight", http [] 1);
    ("HTTP `get_weather`, 64 in flight", http [] 64);
  ]
  @ (if domains = 0 then []
     else
       [
         ( Printf.sprintf "HTTP `get_weather`, 64 in flight, `--domains %d`" domains,
           http [ "--domains"; string_of_int domains ] 64 );
       ])
  @ [
      ("stdio `tools/list`, 64 in flight (no upstream)", rate list 64);
      ("Server CPU per `get_weather` (stdio, 64 in flight)", cpu c64);
      ("Server CPU per `tools/list`", cpu list);
    ]

let () =
  let seconds = ref 2. and cold = ref 5 in
  Arg.parse
    [
      ("-seconds", Arg.Set_float seconds, "S  Duration of each throughput cell (default 2)");
      ("-cold", Arg.Set_int cold, "N  Cold starts per server, of which the median (default 5)");
    ]
    (fun _ -> ())
    "bench [-seconds S] [-cold N]  (from the repository root)";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let measure = measure env ~upstream:(mock ~sw env) ~seconds:!seconds ~cold:!cold in
  let ocaml = measure ~domains:4 [ "_build/default/examples/weather/ocaml/weather.exe" ] in
  let python =
    measure ~domains:0
      [ "examples/weather/python/.venv/bin/python"; "examples/weather/python/weather.py" ]
  in
  print_endline "| | OCaml | FastMCP | |\n| --- | ---: | ---: | ---: |";
  List.iter
    (fun (label, o) ->
      match List.assoc_opt label python with
      | Some p ->
          let ratio = if o.higher then o.value /. p.value else p.value /. o.value in
          Printf.printf "| %s | %s | %s | %.2gx |\n" label o.text p.text ratio
      | None -> Printf.printf "| %s | %s | n/a | |\n" label o.text)
    ocaml
