(* The notes server end to end: metadata, challenges, and calls with tokens
   of different scopes, through the SDK's own HTTP client. *)

open Mcp
open Auth_example

let cfg port : Bearer.config =
  let base = Printf.sprintf "http://localhost:%d" port in
  {
    secret = String.make 32 's';
    issuer = "https://auth.example.com";
    resource = base ^ "/mcp";
    metadata_url = base ^ App.metadata_path;
  }

let parse s = Result.get_ok (Json.decode Jsont.json s)

let text = function
  | Ok ({ content = Proto.Content.Text t :: _; is_error; _ } : Proto.Call_result.t) ->
      (t.text, is_error)
  | Ok _ -> Alcotest.fail "expected text"
  | Error e -> Alcotest.failf "%a" Client.pp_error e

(* One raw HTTP/1.1 exchange: status line, headers and body as text. *)
let raw ~net ~port request =
  Eio.Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string request flow;
  Eio.Flow.shutdown flow `Send;
  Eio.Buf_read.(parse_exn take_all) flow ~max_size:1_000_000

let contains ~sub s =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

let with_server f =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let socket =
      Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port = match Eio.Net.listening_addr socket with `Tcp (_, p) -> p | _ -> assert false in
    let cfg = cfg port in
    Eio.Fiber.fork_daemon ~sw (fun () -> App.run ~sw ~clock ~socket cfg);
    Eio.Time.with_timeout_exn clock 5. (fun () -> f ~sw ~net ~port cfg);
    Eio.Switch.fail sw Exit
  with Exit -> ()

let client ~sw ~net ~port token =
  Client.connect
    (Mcp_http.Client.http1 ~sw ~net ~port
       ~headers:(fun () -> [ ("authorization", "Bearer " ^ token) ])
       ())

let metadata () =
  with_server @@ fun ~sw:_ ~net ~port cfg ->
  let response =
    raw ~net ~port
      (Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
         App.metadata_path)
  in
  let lines = String.split_on_char '\n' response in
  let m = parse (List.nth lines (List.length lines - 1)) in
  Alcotest.(check (option string))
    "resource" (Some cfg.resource)
    (match Json.find "resource" m with Some (Jsont.String (s, _)) -> Some s | _ -> None);
  Alcotest.(check bool)
    "names its authorization server" true
    (contains ~sub:cfg.issuer (Json.to_string (Option.get (Json.find "authorization_servers" m))))

let challenges () =
  with_server @@ fun ~sw:_ ~net ~port cfg ->
  let post ?token () =
    let auth = Option.fold token ~none:"" ~some:(Printf.sprintf "Authorization: Bearer %s\r\n") in
    raw ~net ~port
      (Printf.sprintf
         "POST /mcp HTTP/1.1\r\n\
          Host: localhost\r\n\
          %sContent-Type: application/json\r\n\
          Content-Length: 2\r\n\
          Connection: close\r\n\
          \r\n\
          {}"
         auth)
  in
  let anonymous = post () in
  Alcotest.(check bool) "401" true (String.starts_with ~prefix:"HTTP/1.1 401" anonymous);
  Alcotest.(check bool)
    "points at the metadata" true
    (contains ~sub:(Printf.sprintf "resource_metadata=%S" cfg.metadata_url) anonymous);
  let expired = post ~token:(Bearer.sign cfg ~ttl:(-1.) "alice") () in
  Alcotest.(check bool) "expired" true (contains ~sub:{|error="invalid_token"|} expired);
  let elsewhere = post ~token:(Bearer.sign cfg ~audience:"https://other.example/mcp" "alice") () in
  Alcotest.(check bool)
    "wrong audience" true
    (contains ~sub:"token not issued for this resource" elsewhere);
  let forged = post ~token:(Bearer.sign { cfg with secret = String.make 32 'x' } "alice") () in
  Alcotest.(check bool) "forged" true (contains ~sub:"invalid signature" forged)

let scopes () =
  with_server @@ fun ~sw ~net ~port cfg ->
  let reader = client ~sw ~net ~port (Bearer.sign cfg ~scope:"notes:read" "bob") in
  let writer = client ~sw ~net ~port (Bearer.sign cfg ~scope:"notes:read notes:write" "alice") in
  let call c name args = text (Client.call_tool c name (parse args)) in
  Alcotest.(check (pair string bool)) "principal" ("alice", false) (call writer "whoami" "{}");
  Alcotest.(check (pair string bool))
    "write" ("saved", false)
    (call writer "add_note" {|{"text":"hello"}|});
  Alcotest.(check (pair string bool))
    "missing scope"
    ("insufficient scope: notes:write is required", true)
    (call reader "add_note" {|{"text":"nope"}|});
  Alcotest.(check (pair string bool)) "read" ("alice: hello", false) (call reader "list_notes" "{}");
  match Client.call_tool (client ~sw ~net ~port "garbage") "whoami" Json.empty with
  | Error (Client.Local _) -> ()
  | _ -> Alcotest.fail "expected an unauthenticated call to fail"

let () =
  Alcotest.run "auth"
    [
      ( "bearer",
        [
          Alcotest.test_case "protected resource metadata" `Quick metadata;
          Alcotest.test_case "challenges" `Quick challenges;
          Alcotest.test_case "scopes" `Quick scopes;
        ] );
    ]
