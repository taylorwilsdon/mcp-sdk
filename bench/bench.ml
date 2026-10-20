(* Throughput and latency of the protocol core and of each transport.

   dune exec bench/bench.exe -- [-n CALLS] [-c CONCURRENCY] [-d DOMAINS] *)

open Mcp

let meta =
  {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}|}

let tools =
  Tool.v "add"
    Schema.(
      let+ a = req "a" int and+ b = req "b" int in
      (a, b))
    (fun _ (a, b) -> Ok (Tool.text (string_of_int (a + b))))
  :: List.init 49 (fun i ->
      Tool.v (Printf.sprintf "tool_%02d" i) ~description:"A tool with a realistic schema"
        Schema.(
          let+ q = req "query" string ~doc:"Search query"
          and+ limit = dft "limit" int 10 ~doc:"Maximum results"
          and+ tags = opt "tags" (list string) in
          (q, limit, tags))
        (fun _ (q, _, _) -> Ok (Tool.text q)))

let server () = Server.v ~name:"bench" ~version:"1" ~tools ()

let report name n seconds latencies =
  let rate = float n /. seconds in
  match latencies with
  | None -> Printf.printf "%-34s %10.0f req/s\n%!" name rate
  | Some l ->
      Array.sort compare l;
      let pct p =
        1e6 *. l.(min (Array.length l - 1) (int_of_float (p *. float (Array.length l))))
      in
      Printf.printf "%-34s %10.0f req/s   p50 %7.1f us   p99 %7.1f us\n%!" name rate (pct 0.5)
        (pct 0.99)

let core name ~n server line =
  Eio.Switch.run @@ fun sw ->
  let scope = Local.scope sw in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    match Server.decode server line with
    | Server.Request r -> ignore (Server.execute server ~scope ~emit:ignore r)
    | _ -> failwith "bench request did not decode"
  done;
  report name n (Unix.gettimeofday () -. t0) None

(* [concurrency] fibers share [n] calls; each call is timed. *)
let drive name ~n ~concurrency client =
  let next = Atomic.make 0 and latencies = Array.make n 0. in
  let args = Json.obj [ ("a", Json.int 1); ("b", Json.int 2) ] in
  let t0 = Unix.gettimeofday () in
  let worker () =
    let rec loop () =
      let i = Atomic.fetch_and_add next 1 in
      if i < n then begin
        let t = Unix.gettimeofday () in
        (match Client.call_tool client "add" args with
        | Ok _ -> ()
        | Error e -> Format.kasprintf failwith "%a" Client.pp_error e);
        latencies.(i) <- Unix.gettimeofday () -. t;
        loop ()
      end
    in
    loop ()
  in
  Eio.Fiber.all (List.init concurrency (fun _ -> worker));
  report name n (Unix.gettimeofday () -. t0) (Some latencies)

let () =
  let n = ref 100_000 and concurrency = ref 64 and domains = ref 0 in
  Arg.parse
    [
      ("-n", Arg.Set_int n, "CALLS  Calls per transport benchmark");
      ("-c", Arg.Set_int concurrency, "N  Concurrent in-flight calls");
      ("-d", Arg.Set_int domains, "N  Extra server domains for HTTP");
    ]
    (fun _ -> ())
    "bench [-n CALLS] [-c CONCURRENCY] [-d DOMAINS]";
  let n = !n and concurrency = !concurrency in
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env and net = Eio.Stdenv.net env in
  let s = server () in
  Printf.printf "core (decode + dispatch + encode, %d tools)\n" (List.length tools);
  core "  server/discover" ~n:(10 * n) s
    (Printf.sprintf {|{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{%s}}|} meta);
  core "  tools/list (50 tools, cached)" ~n:(10 * n) s
    (Printf.sprintf {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{%s}}|} meta);
  core "  tools/call add" ~n:(10 * n) s
    (Printf.sprintf
       {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{%s,"name":"add","arguments":{"a":1,"b":2}}}|}
       meta);
  core "  tools/call tool_42 (3 fields)" ~n:(10 * n) s
    (Printf.sprintf
       {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{%s,"name":"tool_42","arguments":{"query":"ocaml","limit":5,"tags":["a","b"]}}}|}
       meta);
  Printf.printf "transports (%d calls, %d in flight, client in-process)\n%!" n concurrency;
  Eio.Switch.run (fun sw ->
      let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Stdio.serve s c2s_r s2c_w;
          `Stop_daemon);
      drive "  stdio (pipes)" ~n ~concurrency
        (Client.connect (Client.line_transport ~sw s2c_r c2s_w));
      Eio.Resource.close c2s_w);
  let domains = if !domains > 0 then Some (Eio.Stdenv.domain_mgr env, !domains) else None in
  let port = 18901 in
  Eio.Fiber.first
    (fun () -> Mcp_http.serve ?domains ~backlog:4096 ~net ~clock ~port s)
    (fun () ->
      try
        Eio.Switch.run @@ fun sw ->
        Eio.Time.sleep clock 0.05;
        drive "  http/1.1 (keep-alive pool)" ~n ~concurrency
          (Client.connect (Mcp_http.Client.http1 ~sw ~net ~connections:concurrency ~port ()));
        Eio.Switch.fail sw Exit
      with Exit -> ())
