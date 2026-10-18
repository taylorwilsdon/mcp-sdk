open Mcp

let parse s = Result.get_ok (Json.decode Jsont.json s)

let meta =
  Json.obj
    [
      (Proto.key_protocol_version, Json.str Proto.version);
      (Proto.key_client_capabilities, Json.empty);
    ]

let request ?(id = 1) method_ params =
  Json.to_string
    (Json.obj
       [
         ("jsonrpc", Json.str "2.0");
         ("id", Json.int id);
         ("method", Json.str method_);
         ("params", Json.obj (("_meta", meta) :: params));
       ])

let execute server ?principal raw =
  match Server.decode server raw with
  | Server.Request req ->
      fst (Server.execute server ?principal ~emit:ignore req) |> Wire.to_string |> parse
  | Server.Reject (id, error) -> Wire.error ?id error |> Wire.to_string |> parse
  | _ -> Alcotest.fail "expected a request or rejection"

let field path value = Option.get (Json.find_path path value)
let string = function Jsont.String (s, _) -> s | _ -> Alcotest.fail "expected string"

let check_error code response =
  Alcotest.(check string)
    "error code" (string_of_int code)
    (Json.to_string (field [ "error"; "code" ] response))

let check_success response =
  Alcotest.(check bool) "has result" true (Json.find "result" response <> None)

let ok = function Ok v -> v | Error e -> Alcotest.failf "%a" Client.pp_error e
let local = function Error (Client.Local _) -> () | _ -> Alcotest.fail "expected local error"
let tool name = Tool.v name Schema.empty (fun _ () -> Ok (Tool.text name))
let server tools = Server.v ~name:"regression" ~version:"1" ~tools ()

let call ?id name arguments extra =
  request ?id "tools/call" (("name", Json.str name) :: ("arguments", arguments) :: extra)

let nested_schema = Schema.(req "account" (obj (req "tenant" string ~header:"Tenant")))
let nested_tool = Tool.v "nested" nested_schema (fun _ tenant -> Ok (Tool.text tenant))

let nested_headers () =
  let _, headers =
    Result.get_ok (Json.decode (Schema.decoder nested_schema) {|{"account":{"tenant":"victim"}}|})
  in
  Alcotest.(check (list (triple string string bool)))
    "nested capture"
    [ ("Tenant", "victim", false) ]
    headers;
  let optional = Schema.(opt "account" (obj (dft "tenant" string ~header:"Tenant" "default"))) in
  List.iter
    (fun raw ->
      let _, headers = Result.get_ok (Json.decode (Schema.decoder optional) raw) in
      Alcotest.(check int) "absent values have no header" 0 (List.length headers))
    [ "{}"; {|{"account":{}}|} ]

let templates () =
  let template pattern =
    Resource.template ~uri_template:pattern "t" (fun _ uri _ -> Ok (Resource.text uri "ok"))
  in
  let check pattern uri expected =
    Alcotest.(check (option (list (pair string string))))
      pattern expected
      (Resource.match_template (template pattern) uri)
  in
  check "x://{name}" "x://a%20b" (Some [ ("name", "a b") ]);
  check "x://{name}" "x://a/b" None;
  check "x://{+path}/end" "x://a/b/end" (Some [ ("path", "a/b") ]);
  check "x://{a}{b}" "x://abc" (Some [ ("a", "a"); ("b", "bc") ]);
  check "x://{name}" "x://" None;
  check "x://literal" "x://literal" (Some []);
  let start = Unix.gettimeofday () in
  let value = String.make 40_000 'a' in
  check "x://{name}" ("x://" ^ value) (Some [ ("name", value) ]);
  check "x://{+a}{+b}{+c}Z" ("x://" ^ String.make 1000 'a') None;
  Alcotest.(check bool) "bounded matching time" true (Unix.gettimeofday () -. start < 1.)

let state_binding () =
  let resume ctx _ =
    match Ctx.state ctx with
    | None -> Ok (Input.required ~state:"approved" [])
    | Some _ -> Ok (Tool.text "resumed")
  in
  let s = server [ Tool.v "resume" Schema.(req "data" (json ())) resume ] in
  let original = parse {|{"data":{"a":1,"b":[2,3]}}|} in
  let first = execute s ~principal:"alice" (call "resume" original []) in
  let state = field [ "result"; "requestState" ] first in
  let retry arguments = call ~id:2 "resume" arguments [ ("requestState", state) ] in
  check_success (execute s ~principal:"alice" (retry (parse {|{"data":{"b":[2,3],"a":1}}|})));
  check_error (-32602)
    (execute s ~principal:"alice" (retry (parse {|{"data":{"a":1000000,"b":[2,3]}}|})));
  check_error (-32602) (execute s ~principal:"alice" (retry (parse {|{"data":{"a":1,"b":[3,2]}}|})));
  check_error (-32602) (execute s ~principal:"bob" (retry original));
  let s = server [ Tool.v "resume" Schema.empty resume ] in
  let first = execute s (request "tools/call" [ ("name", Json.str "resume") ]) in
  check_success
    (execute s
       (call "resume" Json.empty [ ("requestState", field [ "result"; "requestState" ] first) ]))

let prompt_state_binding () =
  let p =
    Prompt.v "resume" (fun ctx _ ->
        match Ctx.state ctx with
        | None -> Ok (Input.required ~state:"approved" [])
        | Some _ -> Ok (Prompt.messages []))
  in
  let s = Server.v ~name:"t" ~version:"1" ~prompts:[ p ] () in
  let params value =
    [ ("name", Json.str "resume"); ("arguments", Json.obj [ ("a", Json.str value) ]) ]
  in
  let first = execute s (request "prompts/get" (params "original")) in
  let state = ("requestState", field [ "result"; "requestState" ] first) in
  check_success (execute s (request ~id:2 "prompts/get" (state :: params "original")));
  check_error (-32602) (execute s (request ~id:2 "prompts/get" (state :: params "changed")))

let exception_server () =
  let schema = Schema.(map int_of_string (req "n" string)) in
  let convert = Tool.v "convert" schema (fun _ n -> Ok (Tool.text (string_of_int n))) in
  let prompt =
    Prompt.v "broken"
      ~complete:(fun _ _ _ -> failwith "backend unavailable")
      (fun _ _ -> Ok (Prompt.messages []))
  in
  Server.v ~name:"t" ~version:"1" ~tools:[ convert; tool "ok" ] ~prompts:[ prompt ] ()

let completion_params =
  [
    ("ref", parse {|{"type":"ref/prompt","name":"broken"}|});
    ("argument", parse {|{"name":"a","value":"x"}|});
  ]

let exceptions () =
  let s = exception_server () in
  check_error (-32602) (execute s (call "convert" (parse {|{"n":"bad"}|}) []));
  check_error (-32603) (execute s (request "completion/complete" completion_params));
  check_success (execute s (call "convert" (parse {|{"n":"42"}|}) []))

let response_envelopes () =
  List.iter
    (fun raw ->
      let client = Client.connect { send = (fun ~headers:_ ~id:_ _ ~on_note:_ -> raw) } in
      local (Client.call_tool client "t" Json.empty))
    [
      {|{"jsonrpc":"1.0","id":1,"result":{"content":[]}}|};
      {|{"jsonrpc":"2.0","id":999,"result":{"content":[]}}|};
      {|{"jsonrpc":"2.0","id":"1","result":{"content":[]}}|};
      {|{"jsonrpc":"2.0","result":{"content":[]}}|};
      {|{"id":1,"result":{"content":[]}}|};
      {|{"jsonrpc":"2.0","id":1,"result":{},"error":{"code":-32603,"message":"bad"}}|};
      {|{"jsonrpc":"2.0","id":1}|};
    ];
  let client =
    Client.connect
      {
        send =
          (fun ~headers:_ ~id _ ~on_note:_ -> Wire.to_string (Wire.result id {|{"content":[]}|}));
      }
  in
  ignore (ok (Client.call_tool client "t" Json.empty));
  ignore (ok (Client.call_tool client "t" Json.empty));
  let client =
    Client.connect
      {
        send =
          (fun ~headers:_ ~id _ ~on_note:_ ->
            Wire.to_string (Wire.error ~id (Error.invalid_params "bad")));
      }
  in
  match Client.call_tool client "t" Json.empty with
  | Error (Client.Remote e) -> Alcotest.(check int) "remote error preserved" (-32602) e.code
  | _ -> Alcotest.fail "expected remote error"

let invalidation () =
  let sends = ref 0 and client_ref = ref None in
  let s = server [ tool "old" ] in
  let transport : Client.transport =
    {
      send =
        (fun ~headers:_ ~id:_ raw ~on_note:_ ->
          incr sends;
          let response = execute s raw in
          if !sends = 1 then (
            Server.set_tools s [ tool "new" ];
            Client.invalidate (Option.get !client_ref) "tools/list");
          Json.to_string response);
    }
  in
  let client = Client.connect transport in
  client_ref := Some client;
  ignore (ok (Client.list_tools client));
  let names () = List.map (fun (t : Proto.Tool_info.t) -> t.name) (ok (Client.list_tools client)) in
  Alcotest.(check (list string)) "fresh list after invalidation" [ "new" ] (names ());
  Alcotest.(check (list string)) "fresh result is cached" [ "new" ] (names ());
  Alcotest.(check int) "two fetches" 2 !sends

let pagination () =
  let make () =
    Server.v ~name:"t" ~version:"1" ~page_size:1 ~tools:[ tool "a"; tool "b"; tool "c" ] ()
  in
  let s = make () in
  let first = execute s (request "tools/list" []) in
  let cursor = [ ("cursor", field [ "result"; "nextCursor" ] first) ] in
  check_success (execute (make ()) (request "tools/list" cursor));
  Server.remove_tool s "a";
  check_error (-32602) (execute s (request "tools/list" cursor));
  let restarted = execute s (request "tools/list" []) in
  let tools = field [ "result"; "tools" ] restarted in
  match tools with
  | Jsont.Array (first :: _, _) ->
      Alcotest.(check string) "restart includes b" "b" (string (field [ "name" ] first))
  | _ -> Alcotest.fail "expected tools"

let with_stdio env server f =
  try
    Eio.Switch.run @@ fun sw ->
    let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
    Eio.Fiber.fork ~sw (fun () -> Stdio.serve server c2s_r s2c_w);
    let client = Client.connect (Client.line_transport ~sw s2c_r c2s_w) in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () -> f client);
    Eio.Switch.fail sw Exit
  with Exit -> ()

let callback env =
  let progress =
    Tool.v "progress" Schema.empty (fun ctx () ->
        Ctx.progress ctx 1.;
        Ctx.progress ctx 2.;
        Ok (Tool.text "done"))
  in
  let s = server [ progress; tool "other" ] in
  with_stdio env s (fun client ->
      let seen = ref [] in
      ignore
        (ok
           (Client.call_tool client "progress" Json.empty
              ~on_progress:(fun ?total:_ ?message:_ step ->
                ignore (ok (Client.list_tools client));
                seen := step :: !seen)));
      Alcotest.(check (list (float 0.)))
        "callbacks finish in order before return" [ 1.; 2. ] (List.rev !seen);
      local
        (Client.call_tool client "progress" Json.empty ~on_progress:(fun ?total:_ ?message:_ _ ->
             failwith "callback failed"));
      ignore (ok (Client.call_tool client "other" Json.empty));
      ignore
        (ok
           (Client.listen client { Proto.Filter.none with tools = true } (fun method_ _ ->
                if method_ = "notifications/subscriptions/acknowledged" then (
                  ignore (ok (Client.list_tools client));
                  Server.shutdown s)))))

let stdio_exceptions env =
  with_stdio env (exception_server ()) (fun client ->
      (match Client.call_tool client "convert" (parse {|{"n":"bad"}|}) with
      | Error (Client.Remote e) -> Alcotest.(check int) "decode error" (-32602) e.code
      | _ -> Alcotest.fail "expected decode error");
      (match Client.request client "completion/complete" completion_params with
      | Error (Client.Remote e) -> Alcotest.(check int) "completion error" (-32603) e.code
      | _ -> Alcotest.fail "expected completion error");
      ignore (ok (Client.call_tool client "ok" Json.empty)))

let eof env =
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw and _, sink = Eio_unix.pipe sw in
  let client = Client.connect (Client.line_transport ~sw r sink) in
  Eio.Resource.close w;
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
      local (Client.discover client);
      local (Client.discover client))

let with_http env server f =
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
  let port = 20000 + Random.int 30000 in
  Eio.Fiber.first
    (fun () -> Mcp_http.serve ~net ~clock ~port ~keepalive:0.01 server)
    (fun () ->
      try
        Eio.Switch.run @@ fun sw ->
        Eio.Time.sleep clock 0.02;
        Eio.Time.with_timeout_exn clock 5. (fun () -> f ~sw ~net ~port);
        Eio.Switch.fail sw Exit
      with Exit -> ())

let http_headers env =
  with_http env (server [ nested_tool ]) (fun ~sw ~net ~port ->
      let transport = Mcp_http.Client.http1 ~sw ~net ~port () in
      let raw = call "nested" (parse {|{"account":{"tenant":"victim"}}|}) [] in
      let base =
        [
          ("MCP-Protocol-Version", Proto.version);
          ("Mcp-Method", "tools/call");
          ("Mcp-Name", "nested");
        ]
      in
      let send extra =
        transport.send ~headers:(base @ extra) ~id:(Id.Int 1) raw ~on_note:ignore |> parse
      in
      check_error (-32020) (send []);
      check_error (-32020) (send [ ("Mcp-Param-Tenant", "attacker") ]);
      check_success (send [ ("Mcp-Param-Tenant", "victim") ]);
      let client = Client.connect transport in
      ignore (ok (Client.list_tools client));
      ignore (ok (Client.call_tool client "nested" (parse {|{"account":{"tenant":"日本"}}|}))))

let http_cleanup env =
  let clock = Eio.Stdenv.clock env in
  let active = ref 0 in
  let slow =
    Tool.v "slow" Schema.empty (fun ctx () ->
        incr active;
        Fun.protect
          ~finally:(fun () -> decr active)
          (fun () ->
            Ctx.progress ctx 1.;
            Eio.Time.sleep clock 10.;
            Ok (Tool.text "done")))
  in
  with_http env
    (server [ slow; tool "ok" ])
    (fun ~sw ~net ~port ->
      let client = Client.connect (Mcp_http.Client.http1 ~sw ~net ~port ~connections:1 ()) in
      ignore (ok (Client.discover client));
      let fd_dir = if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd" in
      let count () = Array.length (Sys.readdir fd_dir) in
      let before = count () in
      for _ = 1 to 20 do
        let started, signal = Eio.Promise.create () in
        Eio.Fiber.first
          (fun () ->
            ignore
              (Client.call_tool client "slow" Json.empty ~on_progress:(fun ?total:_ ?message:_ _ ->
                   Eio.Promise.resolve signal ())))
          (fun () -> Eio.Promise.await started)
      done;
      let rec wait () =
        if !active <> 0 then (
          Eio.Time.sleep clock 0.01;
          wait ())
      in
      wait ();
      ignore (ok (Client.call_tool client "ok" Json.empty));
      Alcotest.(check bool) "descriptor count remains bounded" true (count () <= before + 2))

let http_callbacks env =
  let progress =
    Tool.v "progress" Schema.empty (fun ctx () ->
        Ctx.progress ctx 1.;
        Ctx.progress ctx 2.;
        Ok (Tool.text "done"))
  in
  with_http env
    (server [ progress; tool "ok" ])
    (fun ~sw ~net ~port ->
      let client = Client.connect (Mcp_http.Client.http1 ~sw ~net ~port ~connections:2 ()) in
      let seen = ref [] in
      ignore
        (ok
           (Client.call_tool client "progress" Json.empty
              ~on_progress:(fun ?total:_ ?message:_ step ->
                ignore (ok (Client.list_tools client));
                seen := step :: !seen)));
      Alcotest.(check (list (float 0.))) "callback order" [ 1.; 2. ] (List.rev !seen);
      local
        (Client.call_tool client "progress" Json.empty ~on_progress:(fun ?total:_ ?message:_ _ ->
             failwith "callback failed"));
      ignore (ok (Client.call_tool client "ok" Json.empty)))

let test name f = Alcotest.test_case name `Quick f
let io name f = test name (fun () -> Eio_main.run f)

let tests =
  [
    test "nested header captures" nested_headers;
    test "bounded URI template matching" templates;
    test "state binds canonical tool arguments" state_binding;
    test "state binds prompt arguments" prompt_state_binding;
    test "request exception boundaries" exceptions;
    test "response envelope validation" response_envelopes;
    test "invalidation during list fetch" invalidation;
    test "pagination rejects stale generations" pagination;
    io "callbacks can reenter the client" callback;
    io "stdio survives decoder and completion errors" stdio_exceptions;
    io "calls after EOF fail promptly" eof;
    io "HTTP validates nested headers" http_headers;
    io "HTTP cancellation closes descriptors" http_cleanup;
    io "HTTP callbacks can reenter and fail safely" http_callbacks;
  ]
