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

(* Requests executed directly, on a scope of a mock event loop. *)
let with_scope f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw -> f (Local.scope sw)

let execute server ?principal raw =
  match Server.decode server raw with
  | Server.Request req ->
      with_scope (fun scope -> fst (Server.execute server ?principal ~scope ~emit:ignore req))
      |> Wire.to_string |> parse
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
  let s = server [ Tool.v "resume" Schema.(req "data" json) resume ] in
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

let with_stdio ?max_requests ?max_queued env server f =
  try
    Eio.Switch.run @@ fun sw ->
    let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
    Eio.Fiber.fork ~sw (fun () -> Stdio.serve ?max_requests server c2s_r s2c_w);
    let client = Client.connect (Client.line_transport ~sw ?max_queued s2c_r c2s_w) in
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

let with_http ?domains env server f =
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
  let port = 20000 + Random.int 30000 in
  Eio.Fiber.first
    (fun () -> Mcp_http.serve ?domains ~net ~clock ~port ~keepalive:0.01 server)
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

(* One server over two domains: each domain makes its own Local value, on
   which the handler can fork, and a tool added later is served everywhere. *)
let http_domains env =
  let made = ref [] and lock = Mutex.create () in
  let local =
    Local.v (fun sw ->
        Mutex.protect lock (fun () -> made := Domain.self () :: !made);
        sw)
  in
  let fork =
    Tool.v "fork" Schema.empty (fun ctx () ->
        Eio.Fiber.fork ~sw:(Ctx.local ctx local) ignore;
        Ok (Tool.text "ok"))
  in
  let s = server [ fork ] in
  with_http
    ~domains:(Eio.Stdenv.domain_mgr env, 1)
    env s
    (fun ~sw ~net ~port ->
      let client = Client.connect (Mcp_http.Client.http1 ~sw ~net ~port ~connections:8 ()) in
      let calls name =
        Eio.Fiber.List.iter
          (fun () -> ignore (ok (Client.call_tool client name Json.empty)))
          (List.init 32 ignore)
      in
      calls "fork";
      let domains = List.sort_uniq compare !made in
      Alcotest.(check int) "one value per domain" (List.length domains) (List.length !made);
      Server.add_tool s (tool "added");
      calls "added")

(* Values are made once per scope, again after a failure, and only on the
   scope's own domain. *)
let local_values env =
  let attempts = ref 0 in
  let flaky =
    Local.v (fun _ ->
        incr attempts;
        if !attempts = 1 then failwith "first attempt";
        !attempts)
  in
  Eio.Switch.run @@ fun sw ->
  let scope = Local.scope sw in
  Alcotest.check_raises "failure reaches the caller" (Failure "first attempt") (fun () ->
      ignore (Local.get scope flaky));
  Alcotest.(check int) "made again" 2 (Local.get scope flaky);
  Alcotest.(check int) "then kept" 2 (Local.get scope flaky);
  Alcotest.check_raises "other domains are refused"
    (Invalid_argument "Local.get: scope used from another domain") (fun () ->
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () -> ignore (Local.get scope flaky)))

let text response =
  match field [ "result"; "content" ] response with
  | Jsont.Array ([ content ], _) -> string (field [ "text" ] content)
  | _ -> Alcotest.fail "expected one content item"

(* Conversions run when a request executes, never while it is decoded;
   refinements and imported codecs advertise what they check. *)
let conversions () =
  let converted = ref 0 in
  let positive n = if n > 0 then Ok n else Error "must be positive" in
  let count =
    Schema.(refine ~schema:(Json.obj [ ("minimum", Json.int 1) ]) ~enc:Fun.id positive int)
  in
  let shape =
    Schema.of_jsont
      ~schema:(parse {|{"type":["string","integer"]}|})
      Jsont.(
        any
          ~dec_string:(map ~dec:(fun s -> `Name s) string)
          ~dec_number:(map ~dec:(fun n -> `Id (Float.to_int n)) number)
          ())
  in
  let args =
    Schema.(
      let+ n = dft "n" count 3 and+ shape = req "shape" shape and+ _ = opt "any" json in
      incr converted;
      (n, shape))
  in
  let echo =
    Tool.v "echo" args (fun _ (n, shape) ->
        Ok
          (Tool.text
             (match shape with `Name s -> s ^ string_of_int n | `Id i -> string_of_int (i + n))))
  in
  let s = server [ echo ] in
  let raw arguments = call "echo" (parse arguments) [] in
  (match Server.decode s (raw {|{"shape":"a"}|}) with
  | Server.Request req ->
      Alcotest.(check int) "decoding converts nothing" 0 !converted;
      ignore (with_scope (fun scope -> Server.execute s ~scope ~emit:ignore req));
      Alcotest.(check int) "execution converts" 1 !converted
  | _ -> Alcotest.fail "expected a request");
  Alcotest.(check string) "default" "a3" (text (execute s (raw {|{"shape":"a"}|})));
  Alcotest.(check string) "union" "7" (text (execute s (raw {|{"shape":2,"n":5}|})));
  let refused = execute s (raw {|{"shape":"a","n":0}|}) in
  check_error (-32602) refused;
  Alcotest.(check string)
    "names the property" "n: must be positive"
    (string (field [ "error"; "message" ] refused));
  check_error (-32602) (execute s (raw {|{"shape":true}|}));
  Alcotest.(check string)
    "advertised schema"
    {|{"n":{"type":"integer","minimum":1,"default":3},"shape":{"type":["string","integer"]},"any":{}}|}
    (Json.to_string (field [ "properties" ] (Schema.json_schema args)))

(* With one request admitted, another is refused at once, while the reader
   still delivers the cancellation that frees the slot. *)
let stdio_admission env =
  let clock = Eio.Stdenv.clock env in
  let started, signal = Eio.Promise.create () in
  let slow =
    Tool.v "slow" Schema.empty (fun _ () ->
        Eio.Promise.resolve signal ();
        Eio.Time.sleep clock 10.;
        Ok (Tool.text "done"))
  in
  with_stdio ~max_requests:1 env
    (server [ slow; tool "ok" ])
    (fun client ->
      Eio.Fiber.first
        (fun () -> ignore (Client.call_tool client "slow" Json.empty))
        (fun () ->
          Eio.Promise.await started;
          match Client.call_tool client "ok" Json.empty with
          | Error (Client.Remote e) ->
              Alcotest.(check string) "busy" "Too many requests in flight" e.message
          | _ -> Alcotest.fail "expected a busy error");
      (* The slot frees once the cancelled request has unwound. *)
      let rec retry n =
        match Client.call_tool client "ok" Json.empty with
        | Error (Client.Remote { code = -32603; _ }) when n > 0 ->
            Eio.Time.sleep clock 0.01;
            retry (n - 1)
        | result -> ignore (ok result)
      in
      retry 100)

(* A callback slower than the notifications it receives fails its request
   once [max_queued] bytes are waiting, and the server stops the handler. *)
let chatty_server clock active =
  let chatty =
    Tool.v "chatty" Schema.empty (fun ctx () ->
        incr active;
        Fun.protect
          ~finally:(fun () -> decr active)
          (fun () ->
            for i = 1 to 1000 do
              Ctx.progress ctx (float i)
            done;
            Eio.Time.sleep clock 10.;
            Ok (Tool.text "done")))
  in
  server [ chatty; tool "ok" ]

let check_backlog clock active client =
  let slow_callback ?total:_ ?message:_ _ = Eio.Time.sleep clock 0.01 in
  local (Client.call_tool client "chatty" Json.empty ~on_progress:slow_callback);
  let rec settled n =
    !active = 0
    || n > 0
       &&
       (Eio.Time.sleep clock 0.01;
        settled (n - 1))
  in
  Alcotest.(check bool) "handler cancelled" true (settled 100);
  ignore (ok (Client.call_tool client "ok" Json.empty))

let stdio_backlog env =
  let clock = Eio.Stdenv.clock env and active = ref 0 in
  with_stdio ~max_queued:256 env (chatty_server clock active) (check_backlog clock active)

let http_backlog env =
  let clock = Eio.Stdenv.clock env and active = ref 0 in
  with_http env (chatty_server clock active) (fun ~sw ~net ~port ->
      check_backlog clock active
        (Client.connect (Mcp_http.Client.http1 ~sw ~net ~port ~max_queued:256 ())))

(* An application's own httpun-eio server mounts the endpoint next to its
   other routes, and the client reaches it through its own [connect]. *)
let http_embedded env =
  let net = Eio.Stdenv.net env in
  let config = Mcp_http.config ~clock:(Eio.Stdenv.clock env) () in
  let s = server [ tool "ok" ] in
  try
    Eio.Switch.run @@ fun sw ->
    let loopback = Eio.Net.Ipaddr.V4.loopback in
    let socket = Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net (`Tcp (loopback, 0)) in
    let port = match Eio.Net.listening_addr socket with `Tcp (_, p) -> p | _ -> assert false in
    let scope = Local.scope sw in
    let connection flow addr =
      Eio.Switch.run @@ fun sw ->
      let request_handler _ ({ reqd; _ } : Httpun.Reqd.t Gluten.reqd) =
        match (Httpun.Reqd.request reqd).target with
        | "/mcp" ->
            Eio.Fiber.fork ~sw (fun () ->
                Mcp_http.handle config s ~scope (Mcp_http.Http1.exchange reqd))
        | _ ->
            let headers = Httpun.Headers.of_list [ ("content-length", "0") ] in
            Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers `Not_found) ""
      in
      let error_handler _ ?request:_ _ start =
        Httpun.Body.Writer.close (start Httpun.Headers.empty)
      in
      Httpun_eio.Server.create_connection_handler ~sw ~request_handler ~error_handler addr flow
    in
    Eio.Fiber.fork_daemon ~sw (fun () -> Eio.Net.run_server ~on_error:ignore socket connection);
    let connects = ref 0 in
    let transport ?path () =
      Mcp_http.Client.v ~sw ?path ~authority:"localhost" (fun sw ->
          incr connects;
          Eio.Net.connect ~sw net (`Tcp (loopback, port)))
    in
    let client = Client.connect (transport ()) in
    ignore (ok (Client.call_tool client "ok" Json.empty));
    ignore (ok (Client.call_tool client "ok" Json.empty));
    Alcotest.(check int) "one kept-alive connection" 1 !connects;
    local (Client.discover (Client.connect (transport ~path:"/elsewhere" ())));
    Eio.Switch.fail sw Exit
  with Exit -> ()

let shortest_numbers () =
  let check expected json = Alcotest.(check string) expected expected (Json.to_string json) in
  check "9.3" (Json.num 9.3);
  check "0.30000000000000004" (Json.num (0.1 +. 0.2));
  check "-1.5e-300" (Json.num (-1.5e-300));
  check "12345678901234568" (Json.num 12345678901234567.);
  check {|{"9.3000000000000007":"\"1.0000000000000002"}|}
    (Json.obj [ ("9.3000000000000007", Json.str {|"1.0000000000000002|}) ]);
  check "[52.52437,13.41053,null]"
    (Json.list [ Json.num 52.52437; Json.num 13.41053; Json.num nan ]);
  Random.init 7;
  for _ = 1 to 10_000 do
    let f = Int64.float_of_bits (Random.bits64 ()) in
    if Float.is_finite f then
      match parse (Json.to_string (Json.num f)) with
      | Jsont.Number (g, _) when Int64.equal (Int64.bits_of_float g) (Int64.bits_of_float f) -> ()
      | _ -> Alcotest.failf "%h does not round-trip" f
  done

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
    test "numbers use their shortest exact form" shortest_numbers;
    test "conversions run at execution" conversions;
    io "callbacks can reenter the client" callback;
    io "stdio survives decoder and completion errors" stdio_exceptions;
    io "calls after EOF fail promptly" eof;
    io "HTTP validates nested headers" http_headers;
    io "HTTP cancellation closes descriptors" http_cleanup;
    io "HTTP callbacks can reenter and fail safely" http_callbacks;
    io "HTTP shares one server across domains" http_domains;
    io "Local values are made per scope" local_values;
    io "stdio refuses requests beyond its limit" stdio_admission;
    io "stdio client bounds notification backlog" stdio_backlog;
    io "HTTP client bounds notification backlog" http_backlog;
    io "HTTP endpoint mounts in another server" http_embedded;
  ]
