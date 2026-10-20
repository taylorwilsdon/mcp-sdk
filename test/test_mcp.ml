open Mcp

let meta =
  {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{"elicitation":{}}}|}

let req ?(id = "1") method_ params =
  Printf.sprintf {|{"jsonrpc":"2.0","id":%s,"method":"%s","params":{%s%s}}|} id method_ meta params

let parse s = Result.get_ok (Json.decode Jsont.json s)
let get path s = Json.find_path path (parse s)
let str = function Some (Jsont.String (s, _)) -> s | _ -> Alcotest.fail "expected a string"
let num = function Some (Jsont.Number (f, _)) -> f | _ -> Alcotest.fail "expected a number"
let code s = int_of_float (num (get [ "error"; "code" ] s))
let check_code expected s = Alcotest.(check int) "error code" expected (code s)
let ok = function Ok v -> v | Error e -> Alcotest.failf "unexpected error: %a" Client.pp_error e

(* Wire samples. Each codec must decode its samples and, unless marked
   decode only, encode them again without losing data. *)

(* Every member of [a] must survive in [b], except members holding the
   default value ([false] or [[]]) that encoders omit. *)
let rec subset a b =
  match (a, b) with
  | Jsont.Object (ma, _), Jsont.Object _ ->
      List.for_all
        (fun ((k, _), v) ->
          match (Json.find k b, v) with
          | Some v', _ -> subset v v'
          | None, (Jsont.Bool (false, _) | Jsont.Array ([], _)) -> true
          | None, _ -> false)
        ma
  | Jsont.Array (la, _), Jsont.Array (lb, _) ->
      List.length la = List.length lb && List.for_all2 subset la lb
  | Jsont.Number (x, _), Jsont.Number (y, _) -> x = y
  | Jsont.String (x, _), Jsont.String (y, _) -> x = y
  | Jsont.Bool (x, _), Jsont.Bool (y, _) -> x = y
  | Jsont.Null _, Jsont.Null _ -> true
  | _ -> false

type codec = Codec : string * 'a Jsont.t * bool * string list -> codec

let note = {|{"uri":"notes://42","name":"note-42"}|}

(* [true]: lossless round trip. [false]: decode only, for types where the
   SDK deliberately keeps less than the wire carries. *)
let codecs =
  Proto.
    [
      Codec
        ( "tool",
          Tool_info.codec,
          true,
          [
            {|{"name":"ping","inputSchema":{"type":"object"}}|};
            {|{"name":"search","title":"Search notes","description":"Full-text search","inputSchema":{"type":"object","properties":{"q":{"type":"string"}},"required":["q"]},"outputSchema":{"type":"object","properties":{"hits":{"type":"array","items":{"type":"string"}}}},"annotations":{"title":"Search","readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":false},"icons":[{"src":"https://example.org/s.svg","mimeType":"image/svg+xml","sizes":["any"],"theme":"dark"}],"_meta":{"example.org/tier":"free"}}|};
          ] );
      Codec
        ( "resource",
          Resource.codec,
          true,
          [
            note;
            {|{"uri":"notes://42","name":"note-42","title":"Groceries","description":"Weekly list","mimeType":"text/markdown","size":118,"icons":[{"src":"https://example.org/n.png"}],"annotations":{"audience":["user","assistant"],"priority":0.5,"lastModified":"2026-03-14T09:26:53Z"},"_meta":{"example.org/owner":"ada"}}|};
          ] );
      Codec
        ( "content",
          Content.codec,
          true,
          [
            {|{"type":"text","text":"Build passed"}|};
            {|{"type":"text","text":"internal","annotations":{"audience":["assistant"],"priority":0.2}}|};
            {|{"type":"image","data":"R0lGODlhAQABAAAAACw=","mimeType":"image/gif","annotations":{"priority":1}}|};
            {|{"type":"audio","data":"T2dnUwACAAAAAAAA","mimeType":"audio/ogg"}|};
            {|{"type":"resource_link","uri":"notes://42","name":"note-42","mimeType":"text/markdown"}|};
            {|{"type":"resource","resource":{"uri":"notes://42","text":"eggs"},"annotations":{"audience":["user"]}}|};
          ] );
      Codec
        ( "resource contents",
          Contents.codec,
          true,
          [
            {|{"uri":"notes://42","mimeType":"text/plain","text":"eggs, rice"}|};
            {|{"uri":"blob://logo","mimeType":"image/png","blob":"iVBORw0KGgo=","_meta":{"example.org/etag":"a1"}}|};
          ] );
      Codec
        ( "call result",
          Call_result.codec,
          true,
          [
            {|{"content":[{"type":"text","text":"3 matches"}],"structuredContent":{"hits":["a","b","c"]},"_meta":{"example.org/ms":12},"resultType":"complete"}|};
            {|{"content":[{"type":"text","text":"q must not be empty"}],"isError":true}|};
            {|{"content":[],"structuredContent":[1,2,3]}|};
          ] );
      Codec
        ( "read result",
          Read_result.codec,
          true,
          [
            {|{"contents":[{"uri":"notes://42","text":"eggs"}],"ttlMs":60000,"cacheScope":"public"}|};
          ] );
      Codec
        ( "prompt result",
          Prompt_result.codec,
          true,
          [
            {|{"description":"Summarise a note","messages":[{"role":"user","content":{"type":"text","text":"Summarise note 42"}},{"role":"assistant","content":{"type":"resource","resource":{"uri":"notes://42","text":"eggs"}}}]}|};
          ] );
      Codec
        ( "complete result",
          Completion.codec,
          true,
          [
            {|{"completion":{"values":["notes://41","notes://42"],"total":17,"hasMore":true}}|};
            {|{"completion":{"values":[]}}|};
          ] );
      Codec
        ( "input required",
          Input_required.codec,
          true,
          [
            {|{"resultType":"input_required","inputRequests":{"ok":{"method":"elicitation/create","params":{"mode":"form","message":"Delete 3 notes?","requestedSchema":{"type":"object","properties":{"ok":{"type":"boolean"}}}}},"draft":{"method":"sampling/createMessage","params":{"messages":[],"maxTokens":64}}},"requestState":"opaque"}|};
            {|{"resultType":"input_required","requestState":"step-2"}|};
          ] );
      Codec
        ( "elicit result",
          Elicit_result.codec,
          true,
          [
            {|{"action":"accept","content":{"ok":true,"reason":"tidy up"}}|};
            {|{"action":"decline"}|};
            {|{"action":"cancel"}|};
          ] );
      Codec
        ( "tools page",
          Page.codec ~items:"tools" Tool_info.codec,
          true,
          [
            {|{"tools":[{"name":"ping","inputSchema":{"type":"object"}}],"nextCursor":"2","ttlMs":30000,"cacheScope":"public"}|};
          ] );
      Codec
        ( "prompts page",
          Page.codec ~items:"prompts" Prompt_info.codec,
          true,
          [
            {|{"prompts":[{"name":"summarise","title":"Summarise","description":"Summarise a note","arguments":[{"name":"id","required":true},{"name":"style","title":"Style","description":"brief or full"}],"icons":[{"src":"https://example.org/p.png"}]}]}|};
          ] );
      Codec
        ( "resources page",
          Page.codec ~items:"resources" Resource.codec,
          true,
          [ Printf.sprintf {|{"resources":[%s],"nextCursor":"2"}|} note ] );
      Codec
        ( "templates page",
          Page.codec ~items:"resourceTemplates" Resource_template.codec,
          true,
          [
            {|{"resourceTemplates":[{"uriTemplate":"notes://{id}","name":"note","title":"Note","description":"A note by id","mimeType":"text/markdown","icons":[{"src":"https://example.org/n.svg"}],"annotations":{"audience":["user"]}}],"cacheScope":"private"}|};
          ] );
      Codec
        ( "error",
          Error.codec,
          true,
          [
            {|{"code":-32700,"message":"Parse error"}|};
            {|{"code":-32602,"message":"Unknown tool","data":{"name":"frobnicate"}}|};
            {|{"code":-32603,"message":"Internal error","data":"disk full"}|};
          ] );
      Codec
        ( "discover result",
          Discover_result.codec,
          false,
          [
            {|{"supportedVersions":["2026-07-28"],"capabilities":{"tools":{"listChanged":true},"resources":{"subscribe":true},"prompts":{},"completions":{},"extensions":{"example.org/beta":{}}},"instructions":"Notes live under notes://","ttlMs":3600000,"cacheScope":"public"}|};
          ] );
      Codec
        ( "client capabilities",
          Client_caps.codec,
          false,
          [
            {|{}|};
            {|{"elicitation":{}}|};
            {|{"elicitation":{"form":{},"url":{}},"sampling":{"tools":{}},"roots":{"listChanged":true},"extensions":{"example.org/ui":{"mimeTypes":["text/html"]}}}|};
          ] );
      Codec
        ( "server capabilities",
          Server_caps.codec,
          false,
          [
            {|{}|};
            {|{"tools":{},"resources":{"subscribe":true,"listChanged":true},"prompts":{"listChanged":true},"completions":{},"logging":{}}|};
          ] );
    ]

let wire_tests =
  List.concat_map
    (fun (Codec (ty, codec, lossless, samples)) ->
      List.mapi
        (fun i text ->
          Alcotest.test_case (Printf.sprintf "%s %d" ty i) `Quick (fun () ->
              match Json.decode codec text with
              | Error e -> Alcotest.failf "decode: %s" e
              | Ok v when lossless ->
                  let encoded = parse (Json.encode codec v) in
                  if not (subset (parse text) encoded) then
                    Alcotest.failf "lost data:\n%s\nre-encoded as\n%s" text (Json.to_string encoded)
              | Ok _ -> ()))
        samples)
    codecs

let any_tool name = Tool.v name Schema.empty (fun _ () -> Ok (Tool.text "ok"))

let request_tests =
  let server =
    Server.v ~name:"requests" ~version:"1"
      ~tools:[ any_tool "ping" ]
      ~resources:[]
      ~prompts:[ Prompt.v "summarise" (fun _ _ -> Ok (Prompt.messages [])) ]
      ~templates:
        [
          Resource.template ~uri_template:"notes://{id}" "note"
            ~complete:(fun _ _ -> [])
            (fun _ uri _ -> Ok (Resource.text uri ""));
        ]
      ()
  in
  List.map
    (fun (name, text) ->
      Alcotest.test_case name `Quick (fun () ->
          match Server.decode server text with
          | Server.Request _ | Cancel _ -> ()
          | Ignore -> Alcotest.fail "ignored"
          | Reject (_, e) -> Alcotest.failf "rejected: %a" Error.pp e))
    [
      ("discover", req "server/discover" "");
      ("list tools", req "tools/list" {|,"cursor":"2"|});
      ("call tool", req "tools/call" {|,"name":"ping","arguments":{}|});
      ( "call tool with input responses",
        req "tools/call"
          {|,"name":"ping","arguments":{},"inputResponses":{"ok":{"action":"accept","content":{"ok":true}}},"requestState":"opaque"|}
      );
      ("list resources", req "resources/list" "");
      ("list templates", req "resources/templates/list" "");
      ("read resource", req "resources/read" {|,"uri":"notes://42"|});
      ("list prompts", req "prompts/list" "");
      ("get prompt", req "prompts/get" {|,"name":"summarise","arguments":{"style":"brief"}|});
      ( "complete template",
        req "completion/complete"
          {|,"ref":{"type":"ref/resource","uri":"notes://{id}"},"argument":{"name":"id","value":"4"}|}
      );
      ( "complete prompt",
        req "completion/complete"
          {|,"ref":{"type":"ref/prompt","name":"summarise"},"argument":{"name":"style","value":"b"},"context":{"arguments":{"id":"42"}}|}
      );
      ( "listen",
        req "subscriptions/listen"
          {|,"notifications":{"toolsListChanged":true,"resourceSubscriptions":["notes://42"]}|} );
      ( "cancel",
        {|{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":7,"reason":"user abort"}}|}
      );
    ]

(* Schema *)

let args =
  Schema.(
    let+ name = req "name" string ~doc:"Who" ~header:"Name"
    and+ count = dft "count" int 1
    and+ tags = opt "tags" (list string)
    and+ mode = req "mode" (enum [ ("fast", `Fast); ("slow", `Slow) ]) in
    (name, count, tags, mode))

let schema_tests =
  let decode s =
    Result.bind
      (Json.decode (Schema.decoder args) s)
      (fun (convert, headers) -> Result.map (fun v -> (v, headers)) (convert ()))
  in
  [
    Alcotest.test_case "json schema" `Quick (fun () ->
        let s = Json.to_string (Schema.json_schema args) in
        Alcotest.(check string)
          "schema"
          {|{"type":"object","properties":{"name":{"type":"string","description":"Who","x-mcp-header":"Name"},"count":{"type":"integer","default":1},"tags":{"type":"array","items":{"type":"string"}},"mode":{"type":"string","enum":["fast","slow"]}},"required":["name","mode"]}|}
          s);
    Alcotest.test_case "decode with defaults" `Quick (fun () ->
        match decode {|{"mode":"slow","extra":true,"name":"x"}|} with
        | Ok (("x", 1, None, `Slow), [ ("Name", "x", false) ]) -> ()
        | _ -> Alcotest.fail "unexpected decode");
    Alcotest.test_case "decode all fields" `Quick (fun () ->
        match decode {|{"name":"y","count":3,"tags":["a","b"],"mode":"fast"}|} with
        | Ok (("y", 3, Some [ "a"; "b" ], `Fast), _) -> ()
        | _ -> Alcotest.fail "unexpected decode");
    Alcotest.test_case "missing required" `Quick (fun () ->
        Alcotest.(check bool) "error" true (Result.is_error (decode {|{"name":"y"}|})));
    Alcotest.test_case "wrong type" `Quick (fun () ->
        Alcotest.(check bool) "error" true (Result.is_error (decode {|{"name":1,"mode":"fast"}|})));
    Alcotest.test_case "header on number rejected" `Quick (fun () ->
        Alcotest.check_raises "invalid"
          (Invalid_argument {|Schema: x-mcp-header on "x" needs a string, integer or boolean|})
          (fun () -> ignore Schema.(req "x" float ~header:"X")));
    Alcotest.test_case "header must be a token" `Quick (fun () ->
        Alcotest.(check bool)
          "raises" true
          (match Schema.(req "x" string ~header:"Bad Header") with
          | _ -> false
          | exception Invalid_argument _ -> true));
  ]

(* Server dispatch *)

let echo =
  Tool.v "echo"
    Schema.(
      let+ m = req "message" string in
      m)
    (fun _ m -> Ok (Tool.text m))

let boom = Tool.v "boom" Schema.empty (fun _ () -> failwith "kaboom")
let confirm_schema = Schema.(req "ok" bool)

let confirm =
  Tool.v "confirm"
    Schema.(
      let+ what = req "what" string in
      what)
    (fun ctx what ->
      match (Ctx.elicited ctx "ask" confirm_schema, Ctx.state ctx) with
      | Some (Ctx.Accepted true), Some state -> Ok (Tool.text ("confirmed " ^ state))
      | Some _, _ -> Ok (Tool.text "denied")
      | None, _ ->
          Ok (Input.required ~state:what [ ("ask", Input.elicit ~message:"Sure?" confirm_schema) ]))

let make_server ?page_size ?state_key () =
  Server.v ~name:"test" ~version:"1.0" ?page_size ?state_key
    ~tools:[ echo; boom; confirm; any_tool "zeta"; any_tool "alpha" ]
    ~resources:[ Resource.v ~uri:"mem://a" "a" (fun _ -> Ok (Resource.text "mem://a" "A")) ]
    ~templates:
      [
        Resource.template ~uri_template:"user://{id}/profile" "user" (fun _ uri vars ->
            Ok (Resource.text uri (List.assoc "id" vars)));
      ]
    ~prompts:
      [
        Prompt.v "greet"
          ~args:[ Prompt.arg "who" ~required:true ]
          (fun _ args ->
            Ok
              (Prompt.messages
                 [ Proto.Prompt_message.user (Proto.Content.text ("hi " ^ List.assoc "who" args)) ]));
      ]
    ()

let exchange ?session server s =
  let notes = ref [] in
  let reply =
    match Server.decode server ?session s with
    | Server.Request r ->
        let wire, _ =
          Eio_mock.Backend.run @@ fun () ->
          Eio.Switch.run @@ fun sw ->
          Server.execute server ~scope:(Local.scope sw)
            ~emit:(fun w -> notes := Wire.to_string w :: !notes)
            r
        in
        Some (Wire.to_string wire)
    | Reject (id, e) -> Some (Wire.to_string (Wire.error ?id e))
    | Ignore | Cancel _ -> None
  in
  (reply, List.rev !notes)

let call ?session server s = Option.get (fst (exchange ?session server s))

let server_tests =
  let s = make_server () in
  [
    Alcotest.test_case "discover" `Quick (fun () ->
        let r = call s (req "server/discover" "") in
        Alcotest.(check string)
          "version" "2026-07-28"
          (str
             ( Json.find_path [ "result"; "supportedVersions" ] (parse r) |> function
               | Some (Jsont.Array (x :: _, _)) -> Some x
               | _ -> None ));
        Alcotest.(check string)
          "server info" "test"
          (str (get [ "result"; "_meta"; "io.modelcontextprotocol/serverInfo"; "name" ] r));
        Alcotest.(check string) "cache scope" "public" (str (get [ "result"; "cacheScope" ] r)));
    Alcotest.test_case "tools are sorted" `Quick (fun () ->
        let r = call s (req "tools/list" "") in
        let names =
          match get [ "result"; "tools" ] r with
          | Some (Jsont.Array (l, _)) -> List.map (fun t -> str (Json.find "name" t)) l
          | _ -> []
        in
        Alcotest.(check (list string)) "order" [ "alpha"; "boom"; "confirm"; "echo"; "zeta" ] names);
    Alcotest.test_case "pagination" `Quick (fun () ->
        let s = make_server ~page_size:2 () in
        let r = call s (req "tools/list" "") in
        let next r =
          call s
            (req "tools/list"
               (",\"cursor\":" ^ Json.quote (str (get [ "result"; "nextCursor" ] r))))
        in
        let r = next (next r) in
        Alcotest.(check bool) "last page" true (get [ "result"; "nextCursor" ] r = None);
        check_code (-32602) (call s (req "tools/list" {|,"cursor":"9"|})));
    Alcotest.test_case "call" `Quick (fun () ->
        let r = call s (req "tools/call" {|,"name":"echo","arguments":{"message":"hey"}|}) in
        Alcotest.(check string)
          "text" "hey"
          (str
             ( Json.find_path [ "result"; "content" ] (parse r) |> function
               | Some (Jsont.Array (c :: _, _)) -> Json.find "text" c
               | _ -> None )));
    Alcotest.test_case "id preserved" `Quick (fun () ->
        let r =
          call s (req ~id:{|"abc"|} "tools/call" {|,"name":"echo","arguments":{"message":"x"}|})
        in
        Alcotest.(check string) "id" "abc" (str (get [ "id" ] r)));
    Alcotest.test_case "unknown tool" `Quick (fun () ->
        check_code (-32602) (call s (req "tools/call" {|,"name":"nope"|})));
    Alcotest.test_case "bad arguments" `Quick (fun () ->
        check_code (-32602) (call s (req "tools/call" {|,"name":"echo","arguments":{"message":3}|})));
    Alcotest.test_case "handler exception" `Quick (fun () ->
        check_code (-32603) (call s (req "tools/call" {|,"name":"boom"|})));
    Alcotest.test_case "missing meta" `Quick (fun () ->
        check_code (-32602) (call s {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}|}));
    Alcotest.test_case "missing capabilities" `Quick (fun () ->
        check_code (-32602)
          (call s
             {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}|}));
    Alcotest.test_case "unsupported version" `Quick (fun () ->
        let r =
          call s
            {|{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-11-25","io.modelcontextprotocol/clientCapabilities":{}}}}|}
        in
        check_code (-32022) r;
        Alcotest.(check string)
          "requested" "2025-11-25"
          (str (get [ "error"; "data"; "requested" ] r)));
    Alcotest.test_case "unknown method" `Quick (fun () ->
        check_code (-32601) (call s (req "nope/nope" "")));
    Alcotest.test_case "initialize needs its parameters" `Quick (fun () ->
        check_code (-32602) (call s {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|}));
    Alcotest.test_case "removed ping" `Quick (fun () ->
        check_code (-32601) (call s (req "ping" "")));
    Alcotest.test_case "parse error" `Quick (fun () ->
        let r = call s "{oops" in
        check_code (-32700) r;
        Alcotest.(check bool) "no id" true (get [ "id" ] r = None));
    Alcotest.test_case "null id" `Quick (fun () ->
        check_code (-32600)
          (call s {|{"jsonrpc":"2.0","id":null,"method":"tools/list","params":{}}|}));
    Alcotest.test_case "batch" `Quick (fun () -> check_code (-32600) (call s "[]"));
    Alcotest.test_case "wrong jsonrpc" `Quick (fun () ->
        check_code (-32600)
          (call s
             (Printf.sprintf {|{"jsonrpc":"1.0","id":1,"method":"server/discover","params":{%s}}|}
                meta)));
    Alcotest.test_case "notification gets no reply" `Quick (fun () ->
        Alcotest.(check bool)
          "none" true
          (fst (exchange s {|{"jsonrpc":"2.0","method":"notifications/whatever"}|}) = None));
    Alcotest.test_case "read resource" `Quick (fun () ->
        Alcotest.(check string)
          "text" "A"
          (str
             ( Json.find_path [ "result"; "contents" ]
                 (parse (call s (req "resources/read" {|,"uri":"mem://a"|})))
             |> function
               | Some (Jsont.Array (c :: _, _)) -> Json.find "text" c
               | _ -> None )));
    Alcotest.test_case "read template" `Quick (fun () ->
        let r = call s (req "resources/read" {|,"uri":"user://42/profile"|}) in
        Alcotest.(check string)
          "id" "42"
          (str
             ( Json.find_path [ "result"; "contents" ] (parse r) |> function
               | Some (Jsont.Array (c :: _, _)) -> Json.find "text" c
               | _ -> None ));
        Alcotest.(check int) "ttl" 0 (int_of_float (num (get [ "result"; "ttlMs" ] r))));
    Alcotest.test_case "resource not found is -32602" `Quick (fun () ->
        check_code (-32602) (call s (req "resources/read" {|,"uri":"user://42/x/profile"|})));
    Alcotest.test_case "prompt" `Quick (fun () ->
        ignore
          (get [ "result"; "messages" ]
             (call s (req "prompts/get" {|,"name":"greet","arguments":{"who":"bob"}|})));
        check_code (-32602) (call s (req "prompts/get" {|,"name":"greet"|})));
    Alcotest.test_case "elicitation requires capability" `Quick (fun () ->
        let r =
          call s
            {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}},"name":"confirm","arguments":{"what":"x"}}}|}
        in
        check_code (-32021) r;
        ignore (get [ "error"; "data"; "requiredCapabilities"; "elicitation" ] r));
    Alcotest.test_case "disabled features are not found" `Quick (fun () ->
        let s = Server.v ~name:"bare" ~version:"1" () in
        check_code (-32601) (call s (req "tools/list" ""));
        ignore (get [ "result" ] (call s (req "server/discover" ""))));
    Alcotest.test_case "list changes are visible" `Quick (fun () ->
        let s = make_server () in
        Server.add_tool s (any_tool "beta");
        let r = call s (req "tools/call" {|,"name":"beta"|}) in
        ignore (get [ "result" ] r);
        Server.remove_tool s "beta";
        check_code (-32602) (call s (req "tools/call" {|,"name":"beta"|})));
  ]

(* MRTR *)

let state_of r = str (get [ "result"; "requestState" ] r)

let retry ?(name = "confirm") ?(what = "x") ~state answer =
  req "tools/call"
    (Printf.sprintf
       {|,"name":"%s","arguments":{"what":"%s"},"requestState":"%s","inputResponses":{"ask":%s}|}
       name what state answer)

let mrtr_tests =
  let s = make_server () in
  let first () = call s (req "tools/call" {|,"name":"confirm","arguments":{"what":"x"}|}) in
  [
    Alcotest.test_case "input required" `Quick (fun () ->
        let r = first () in
        Alcotest.(check string) "type" "input_required" (str (get [ "result"; "resultType" ] r));
        Alcotest.(check string)
          "method" "elicitation/create"
          (str (get [ "result"; "inputRequests"; "ask"; "method" ] r)));
    Alcotest.test_case "retry completes" `Quick (fun () ->
        let r =
          call s (retry ~state:(state_of (first ())) {|{"action":"accept","content":{"ok":true}}|})
        in
        Alcotest.(check string)
          "text" "confirmed x"
          (str
             ( Json.find_path [ "result"; "content" ] (parse r) |> function
               | Some (Jsont.Array (c :: _, _)) -> Json.find "text" c
               | _ -> None )));
    Alcotest.test_case "tampered state rejected" `Quick (fun () ->
        let st = state_of (first ()) in
        let tampered =
          String.mapi (fun i c -> if i = 3 then if c = 'A' then 'B' else 'A' else c) st
        in
        check_code (-32602)
          (call s (retry ~state:tampered {|{"action":"accept","content":{"ok":true}}|})));
    Alcotest.test_case "state bound to the tool" `Quick (fun () ->
        let st = state_of (first ()) in
        check_code (-32602)
          (call s
             (req "tools/call"
                (Printf.sprintf {|,"name":"echo","arguments":{"message":"m"},"requestState":"%s"|}
                   st))));
    Alcotest.test_case "state bound to the server key" `Quick (fun () ->
        let other = make_server ~state_key:"another key" () in
        check_code (-32602)
          (call other
             (retry ~state:(state_of (first ())) {|{"action":"accept","content":{"ok":true}}|})));
    Alcotest.test_case "malformed answer asks again" `Quick (fun () ->
        let r =
          call s (retry ~state:(state_of (first ())) {|{"action":"accept","content":{"ok":"yes"}}|})
        in
        Alcotest.(check string) "type" "input_required" (str (get [ "result"; "resultType" ] r)));
    Alcotest.test_case "expired state rejected" `Quick (fun () ->
        let s = Server.v ~name:"t" ~version:"1" ~state_ttl_ms:(-1) ~tools:[ confirm ] () in
        let st =
          state_of (call s (req "tools/call" {|,"name":"confirm","arguments":{"what":"x"}|}))
        in
        check_code (-32602) (call s (retry ~state:st {|{"action":"accept","content":{"ok":true}}|})));
  ]

(* End to end over stdio pipes and HTTP *)

let slow_cancelled = Atomic.make 0

let slow clock =
  Tool.v "slow" Schema.empty (fun ctx () ->
      Ctx.progress ctx 1.;
      match Eio.Time.sleep clock 10. with
      | () -> Ok (Tool.text "done")
      | exception (Eio.Cancel.Cancelled _ as e) ->
          Atomic.incr slow_cancelled;
          raise e)

let progress clock =
  Tool.v "progress" Schema.empty (fun ctx () ->
      for i = 1 to 3 do
        Eio.Time.sleep clock 0.01;
        Ctx.progress ctx ~total:3. ~message:"step" (float i)
      done;
      Ok (Tool.text "done"))

let headers =
  Tool.v "region"
    Schema.(
      let+ r = req "region" string ~header:"Region" and+ n = req "n" int ~header:"N" in
      (r, n))
    (fun _ (r, n) -> Ok (Tool.text (Printf.sprintf "%s/%d" r n)))

let e2e_server clock =
  Server.v ~name:"e2e" ~version:"1"
    ~tools:[ echo; confirm; slow clock; progress clock; headers ]
    ~resources:[ Resource.v ~uri:"mem://a" "a" (fun _ -> Ok (Resource.text "mem://a" "A")) ]
    ~list_ttl_ms:60_000 ()

let caps = { Proto.Client_caps.none with elicit_form = true }

let on_input _ (r : Proto.Input_request.t) =
  if r.method_ = "elicitation/create" then
    Some
      (Json.obj [ ("action", Json.str "accept"); ("content", Json.obj [ ("ok", Json.bool true) ]) ])
  else None

let first_text (r : Proto.Call_result.t) =
  match r.content with Proto.Content.Text t :: _ -> t.text | _ -> ""

let client_suite ~clock server client =
  let call name args = ok (Client.call_tool client name (parse args)) in
  Alcotest.(check string) "echo" "hi" (first_text (call "echo" {|{"message":"hi"}|}));
  Alcotest.(check string) "mrtr" "confirmed x" (first_text (call "confirm" {|{"what":"x"}|}));
  let steps = ref [] in
  let r =
    ok
      (Client.call_tool client "progress" Json.empty ~on_progress:(fun ?total:_ ?message:_ p ->
           steps := p :: !steps))
  in
  Alcotest.(check string) "progress result" "done" (first_text r);
  Alcotest.(check (list (float 0.))) "progress" [ 1.; 2.; 3. ] (List.rev !steps);
  let tools = ok (Client.list_tools client) in
  Alcotest.(check int) "tools" 5 (List.length tools);
  Alcotest.(check string) "headers" "eu/7" (first_text (call "region" {|{"region":"eu","n":7}|}));
  Alcotest.(check string)
    "non-ascii header" "日本/1"
    (first_text (call "region" {|{"region":"日本","n":1}|}));
  (* Concurrency: many in-flight requests on one client. *)
  let results =
    List.init 50 (fun i -> i)
    |> Eio.Fiber.List.map (fun i ->
        first_text (call "echo" (Printf.sprintf {|{"message":"m%d"}|} i)))
  in
  Alcotest.(check (list string)) "concurrent" (List.init 50 (Printf.sprintf "m%d")) results;
  (* Cancellation reaches the handler. *)
  let before = Atomic.get slow_cancelled in
  Eio.Fiber.first
    (fun () ->
      ignore
        (Client.call_tool client "slow" Json.empty ~on_progress:(fun ?total:_ ?message:_ _ -> ())))
    (fun () -> Eio.Time.sleep clock 0.2);
  let rec wait n =
    if Atomic.get slow_cancelled = before && n > 0 then (
      Eio.Time.sleep clock 0.05;
      wait (n - 1))
  in
  wait 40;
  Alcotest.(check int) "handler cancelled" (before + 1) (Atomic.get slow_cancelled);
  (* Subscriptions. *)
  let seen = ref [] in
  let filter = { Proto.Filter.none with tools = true; uris = [ "mem://a" ] } in
  Eio.Fiber.both
    (fun () -> ok (Client.listen client filter (fun m _ -> seen := m :: !seen)))
    (fun () ->
      let rec until_acked n =
        if !seen = [] && n > 0 then (
          Eio.Time.sleep clock 0.02;
          until_acked (n - 1))
      in
      until_acked 100;
      Server.add_tool server (any_tool "late");
      Server.resource_updated server "mem://a";
      Server.resource_updated server "mem://other";
      Eio.Time.sleep clock 0.1;
      Server.shutdown server);
  Alcotest.(check (list string))
    "notifications"
    [
      "notifications/subscriptions/acknowledged";
      "notifications/tools/list_changed";
      "notifications/resources/updated";
    ]
    (List.rev !seen);
  Alcotest.(check int) "cache invalidated" 6 (List.length (ok (Client.list_tools client)))

let with_env f () = Eio_main.run @@ fun env -> f env

let stdio_test =
  with_env @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
  let server = e2e_server clock in
  Eio.Fiber.both
    (fun () ->
      Stdio.serve server c2s_r s2c_w;
      Eio.Resource.close s2c_w)
    (fun () ->
      let transport = Client.line_transport ~sw s2c_r c2s_w in
      client_suite ~clock server (Client.connect ~capabilities:caps ~on_input transport);
      Eio.Resource.close c2s_w)

let port =
  ref
    (Random.self_init ();
     20000 + Random.int 20000)

let http_test =
  with_env @@ fun env ->
  let clock = Eio.Stdenv.clock env and net = Eio.Stdenv.net env in
  let server = e2e_server clock in
  incr port;
  let port = !port in
  Eio.Fiber.first
    (fun () -> Mcp_http.serve ~keepalive:0.05 ~net ~clock ~port server)
    (fun () ->
      (* Failing the switch tears down the client's pooled connections. *)
      try
        Eio.Switch.run @@ fun sw ->
        Eio.Time.sleep clock 0.05;
        let transport = Mcp_http.Client.http1 ~sw ~net ~port () in
        let client = Client.connect ~capabilities:caps ~on_input transport in
        ignore (ok (Client.discover client));
        client_suite ~clock server client;
        Eio.Switch.fail sw Exit
      with Exit -> ())

(* Revisions 2025-11-25 and 2025-06-18 *)

let initialize ?(caps = "{}") version =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"%s","capabilities":%s,"clientInfo":{"name":"old","version":"1"}}}|}
    version caps

let confirm_call =
  {|{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"confirm","arguments":{"what":"x"}}}|}

let accept id =
  Printf.sprintf {|{"jsonrpc":"2.0","id":%s,"result":{"action":"accept","content":{"ok":true}}}|}
    (Json.to_string (Option.get id))

let text_of r =
  str
    ( Json.find_path [ "result"; "content" ] (parse r) |> function
      | Some (Jsont.Array (c :: _, _)) -> Json.find "text" c
      | _ -> None )

let session_of s =
  match Server.decode s (initialize "2025-11-25") with
  | Server.Request r -> Server.Request.session r
  | _ -> Alcotest.fail "initialize was not decoded"

(* One POST to the HTTP endpoint, answered in memory. [on_flush] sees the
   SSE stream so far each time an event is flushed. *)
let post ~clock server ?session ?(on_flush = ignore) body =
  let status = ref 0 and headers = ref [] and out = Buffer.create 256 in
  let x : Mcp_http.Exchange.t =
    {
      meth = "POST";
      header = (function "mcp-session-id" -> session | _ -> None);
      read_body = (fun ~max:_ -> Some body);
      respond =
        (fun ~status:code h wire ->
          status := code;
          headers := h;
          Buffer.add_string out (Wire.to_string wire));
      stream =
        (fun ~status:code h ->
          status := code;
          headers := h;
          {
            write = Buffer.add_string out;
            flush =
              (fun () ->
                on_flush (Buffer.contents out);
                `Written);
            close = ignore;
          });
    }
  in
  Eio.Switch.run (fun sw ->
      Mcp_http.handle (Mcp_http.config ~clock ()) server ~scope:(Local.scope sw) x);
  (!status, List.assoc_opt "mcp-session-id" !headers, Buffer.contents out)

let last_event stream =
  let events = String.split_on_char '\n' stream in
  List.fold_left
    (fun last l ->
      if String.starts_with ~prefix:"data: " l then String.sub l 6 (String.length l - 6) else last)
    "" events

let legacy_tests =
  let s = make_server () in
  [
    Alcotest.test_case "initialize negotiates" `Quick (fun () ->
        let version v = str (get [ "result"; "protocolVersion" ] (call s (initialize v))) in
        Alcotest.(check string) "supported" "2025-06-18" (version "2025-06-18");
        Alcotest.(check string) "latest otherwise" "2025-11-25" (version "2024-11-05");
        let r = call s (initialize "2025-11-25") in
        Alcotest.(check string)
          "server info" "test"
          (str (get [ "result"; "serverInfo"; "name" ] r));
        Alcotest.(check bool)
          "no list changes" true
          (get [ "result"; "capabilities"; "tools"; "listChanged" ] r = None));
    Alcotest.test_case "session stands in for _meta" `Quick (fun () ->
        let session = session_of s in
        ignore
          (get [ "result"; "tools" ]
             (call ?session s {|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|}));
        Alcotest.(check string)
          "ping" "{}"
          (Json.to_string
             (Option.get
                (get [ "result" ] (call ?session s {|{"jsonrpc":"2.0","id":1,"method":"ping"}|}))));
        check_code (-32602) (call s {|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|});
        check_code (-32021) (call ?session s confirm_call));
    Alcotest.test_case "stdio asks the client" `Quick
      ( with_env @@ fun _ ->
        Eio.Switch.run @@ fun sw ->
        let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
        Eio.Fiber.both
          (fun () ->
            Stdio.serve (make_server ()) c2s_r s2c_w;
            Eio.Resource.close s2c_w)
          (fun () ->
            let input = Eio.Buf_read.of_flow s2c_r ~max_size:1_000_000 in
            let send s = Eio.Flow.copy_string (s ^ "\n") c2s_w in
            send (initialize ~caps:{|{"elicitation":{}}|} "2025-11-25");
            ignore (get [ "result"; "protocolVersion" ] (Eio.Buf_read.line input));
            send {|{"jsonrpc":"2.0","method":"notifications/initialized"}|};
            send confirm_call;
            let ask = Eio.Buf_read.line input in
            Alcotest.(check string) "asks" "elicitation/create" (str (get [ "method" ] ask));
            send (accept (get [ "id" ] ask));
            Alcotest.(check string) "answer" "confirmed x" (text_of (Eio.Buf_read.line input));
            Eio.Resource.close c2s_w) );
    Alcotest.test_case "http session" `Quick
      ( with_env @@ fun env ->
        let clock = Eio.Stdenv.clock env and server = make_server () in
        let post = post ~clock server in
        let status, session, _ = post (initialize ~caps:{|{"elicitation":{}}|} "2025-11-25") in
        Alcotest.(check int) "initialized" 200 status;
        Alcotest.(check bool) "session id" true (session <> None);
        let _, _, r = post ?session {|{"jsonrpc":"2.0","id":1,"method":"tools/list"}|} in
        ignore (get [ "result"; "tools" ] r);
        let answered = ref false in
        let on_flush stream =
          let ask = last_event stream in
          if (not !answered) && get [ "method" ] ask <> None then begin
            answered := true;
            let status, _, _ = post ?session (accept (get [ "id" ] ask)) in
            Alcotest.(check int) "answer accepted" 202 status
          end
        in
        let _, _, stream = post ?session ~on_flush confirm_call in
        Alcotest.(check string) "answer" "confirmed x" (text_of (last_event stream));
        let status, _, _ = post ~session:"%%" {|{"jsonrpc":"2.0","id":1,"method":"ping"}|} in
        Alcotest.(check int) "bad session" 400 status );
  ]

(* Robustness *)

let tool_info schema : Proto.Tool_info.t =
  {
    name = "t";
    title = None;
    description = None;
    input_schema = parse schema;
    output_schema = None;
    annotations = None;
    icons = [];
    meta = None;
  }

let robustness_tests =
  [
    Alcotest.test_case "oversized stdio message is skipped" `Quick
      ( with_env @@ fun _ ->
        Eio.Switch.run @@ fun sw ->
        let c2s_r, c2s_w = Eio_unix.pipe sw and s2c_r, s2c_w = Eio_unix.pipe sw in
        Eio.Fiber.both
          (fun () ->
            Stdio.serve ~max_message:256 (make_server ()) c2s_r s2c_w;
            Eio.Resource.close s2c_w)
          (fun () ->
            Eio.Flow.copy_string
              (String.make 1000 'x' ^ "\n" ^ req "server/discover" "" ^ "\n")
              c2s_w;
            Eio.Resource.close c2s_w;
            let input = Eio.Buf_read.of_flow s2c_r ~max_size:1_000_000 in
            check_code (-32700) (Eio.Buf_read.line input);
            ignore (get [ "result"; "supportedVersions" ] (Eio.Buf_read.line input))) );
    Alcotest.test_case "pending requests fail when the server goes away" `Quick
      ( with_env @@ fun _ ->
        Eio.Switch.run @@ fun sw ->
        let s2c_r, s2c_w = Eio_unix.pipe sw and _, c2s_w = Eio_unix.pipe sw in
        let client = Client.connect (Client.line_transport ~sw s2c_r c2s_w) in
        Eio.Fiber.both
          (fun () ->
            match Client.discover client with
            | Error (Client.Local _) -> ()
            | _ -> Alcotest.fail "expected a local error")
          (fun () -> Eio.Resource.close s2c_w) );
    Alcotest.test_case "x-mcp-header rules" `Quick (fun () ->
        let valid s = Client.header_params (tool_info s) <> None in
        Alcotest.(check bool)
          "nested properties" true
          (valid
             {|{"type":"object","properties":{"a":{"type":"object","properties":{"b":{"type":"string","x-mcp-header":"B"}}}}}|});
        Alcotest.(check bool)
          "under items" false
          (valid
             {|{"type":"object","properties":{"a":{"type":"array","items":{"type":"string","x-mcp-header":"A"}}}}|});
        Alcotest.(check bool)
          "on a number" false
          (valid {|{"type":"object","properties":{"a":{"type":"number","x-mcp-header":"A"}}}|});
        Alcotest.(check bool)
          "duplicate names" false
          (valid
             {|{"type":"object","properties":{"a":{"type":"string","x-mcp-header":"X"},"b":{"type":"string","x-mcp-header":"x"}}}|}));
    Alcotest.test_case "duplicate tool names are rejected" `Quick (fun () ->
        Alcotest.check_raises "duplicate" (Invalid_argument {|Server: duplicate tool "echo"|})
          (fun () -> ignore (Server.v ~name:"d" ~version:"1" ~tools:[ echo; echo ] ())));
  ]

let () =
  Alcotest.run "mcp"
    [
      ("wire", wire_tests);
      ("requests", request_tests);
      ("schema", schema_tests);
      ("server", server_tests);
      ("mrtr", mrtr_tests);
      ("legacy", legacy_tests);
      ("robustness", robustness_tests);
      ("regressions", Regression.tests);
      ( "e2e",
        [
          Alcotest.test_case "stdio" `Quick stdio_test;
          Alcotest.test_case "http/1.1" `Quick http_test;
        ] );
    ]
