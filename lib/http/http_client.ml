(* Client transport for Streamable HTTP over HTTP/1.1: a pool of keep-alive
   connections, one in-flight request each. *)

let rec event_end s i =
  match String.index_from_opt s i '\n' with
  | Some j when j + 1 < String.length s && s.[j + 1] = '\n' -> Some j
  | Some j -> event_end s (j + 1)
  | None -> None

let event_data block =
  String.split_on_char '\n' block
  |> List.filter_map (fun line ->
      if String.starts_with ~prefix:"data:" line then
        let v = String.sub line 5 (String.length line - 5) in
        Some (if String.starts_with ~prefix:" " v then String.sub v 1 (String.length v - 1) else v)
      else None)
  |> String.concat "\n"

(* Read a body that is either one JSON object or an SSE stream of
   request-scoped notifications terminated by the response. At most [max]
   bytes are held: the whole of a JSON body, or the unfinished event of a
   stream. Past that, reading stops and the request fails, which drops the
   connection. *)
let read_response ~max ~status ~sse ~schedule_read ~on_note ~finish =
  let buf = Buffer.create 4096 in
  let feed chunk =
    if not sse then Buffer.add_string buf chunk
    else begin
      String.iter (fun c -> if c <> '\r' then Buffer.add_char buf c) chunk;
      let s = Buffer.contents buf in
      let rec go start =
        match event_end s start with
        | Some i ->
            let data = event_data (String.sub s start (i - start)) in
            if Mcp.Client.is_response data then finish (Ok data)
            else if data <> "" then on_note data;
            go (i + 2)
        | None ->
            Buffer.clear buf;
            Buffer.add_substring buf s start (String.length s - start)
      in
      go 0
    end
  in
  let rec next () =
    schedule_read
      ~on_eof:(fun () ->
        if sse then finish (Error "Stream closed before the response")
        else if Buffer.length buf = 0 && status >= 300 then
          finish (Error ("HTTP " ^ string_of_int status))
        else finish (Ok (Buffer.contents buf)))
      ~on_read:(fun bs ~off ~len ->
        feed (Bigstringaf.substring bs ~off ~len);
        if Buffer.length buf > max then finish (Error "Response exceeds max_response bytes")
        else next ())
  in
  next ()

let request_headers body extra =
  ("content-type", "application/json")
  :: ("accept", "application/json, text/event-stream")
  :: ("content-length", string_of_int (String.length body))
  :: List.map (fun (k, v) -> (String.lowercase_ascii k, v)) extra

let is_sse content_type =
  Option.fold ~none:false ~some:(String.starts_with ~prefix:"text/event-stream") content_type

(* The connection reader queues events; the requesting fiber runs callbacks
   in order, allowing them to make requests without blocking that reader. *)
let exchange ~max_queued ~on_note start =
  let inbox = Mcp.Client.Inbox.create ~max_queued ~overflow:`Fail in
  let abort = start ~on_note:(Mcp.Client.Inbox.note inbox) (Mcp.Client.Inbox.respond inbox) in
  match Mcp.Client.Inbox.receive inbox ~on_note with
  | body -> body
  | exception e ->
      Eio.Cancel.protect abort;
      raise e

let v ~sw ?(path = "/mcp") ?(connections = 64) ?(max_queued = 1 lsl 22) ?(max_response = 1 lsl 22)
    ?(headers = fun () -> []) ~authority connect : Mcp.Client.transport =
  (* The [Mcp-Session-Id] a server of an earlier revision issued, if any,
     and the revision its requests carry. *)
  let session = Atomic.make None and version = Atomic.make None in
  let on_request = ref ignore and listening = Atomic.make false in
  let session_header () =
    Option.fold (Atomic.get session) ~none:[] ~some:(fun s -> [ ("mcp-session-id", s) ])
  in
  let open_connection sw =
    let socket = connect sw in
    (Httpun_eio.Client.create_connection ~sw socket, socket, ref false)
  in
  let dispose (conn, socket, disposed) =
    Eio.Cancel.protect (fun () ->
        if not !disposed then (
          disposed := true;
          Fun.protect
            ~finally:(fun () -> Eio.Resource.close socket)
            (fun () ->
              let stopped = Httpun_eio.Client.shutdown conn in
              Eio.Flow.shutdown socket `All;
              Eio.Promise.await stopped)))
  in
  let pool =
    Eio.Pool.create connections
      (fun () -> open_connection sw)
      ~validate:(fun (c, _, disposed) -> (not !disposed) && not (Httpun_eio.Client.is_closed c))
      ~dispose
  in
  (* A server of an earlier revision may put requests to the client on a
     stream of its own, which the client opens with GET once it has a
     session. Each message is handled on a fiber of its own, so the stream is
     never held up. A server without such a stream refuses the GET and is not
     asked again; one that ends it has it reopened by the next response that
     carries the session. *)
  let listen () =
    Eio.Switch.run @@ fun lsw ->
    let ((conn, _, _) as connection) = open_connection lsw in
    Fun.protect ~finally:(fun () -> dispose connection) @@ fun () ->
    let ended, resolve = Eio.Promise.create () in
    let finish status _ = ignore (Eio.Promise.try_resolve resolve status) in
    let deliver raw =
      Eio.Fiber.fork ~sw (fun () ->
          try !on_request raw with Eio.Cancel.Cancelled _ as e -> raise e | _ -> ())
    in
    let response_handler (resp : Httpun.Response.t) reader =
      let status = Httpun.Status.to_code resp.status in
      read_response ~max:max_response ~status
        ~sse:(is_sse (Httpun.Headers.get resp.headers "content-type"))
        ~schedule_read:(Httpun.Body.Reader.schedule_read reader)
        ~on_note:deliver ~finish:(finish status)
    in
    let hs =
      ("host", authority) :: ("accept", "text/event-stream")
      :: (headers () @ session_header ()
         @ Option.fold (Atomic.get version) ~none:[] ~some:(fun v ->
             [ ("mcp-protocol-version", v) ]))
    in
    let writer =
      Httpun_eio.Client.request conn
        (Httpun.Request.create ~headers:(Httpun.Headers.of_list hs) `GET path)
        ~error_handler:(fun _ -> finish 0 ())
        ~response_handler
    in
    Httpun.Body.Writer.close writer;
    Eio.Promise.await ended
  in
  let start_listening () =
    if not (Atomic.exchange listening true) then
      Eio.Fiber.fork ~sw (fun () ->
          match listen () with
          | 200 -> Atomic.set listening false
          | _ -> ()
          | exception (Eio.Cancel.Cancelled _ as e) -> raise e
          | exception _ -> Atomic.set listening false)
  in
  (* One POST on [connection], answered through [finish]. *)
  let request ((conn, _, _) as connection) extra body ~on_note finish =
    Option.iter (fun v -> Atomic.set version (Some v)) (List.assoc_opt "MCP-Protocol-Version" extra);
    let hs = request_headers body (headers () @ session_header () @ extra) in
    let response_handler (resp : Httpun.Response.t) reader =
      Option.iter
        (fun s ->
          Atomic.set session (Some s);
          start_listening ())
        (Httpun.Headers.get resp.headers "mcp-session-id");
      read_response ~max:max_response
        ~status:(Httpun.Status.to_code resp.status)
        ~sse:(is_sse (Httpun.Headers.get resp.headers "content-type"))
        ~schedule_read:(Httpun.Body.Reader.schedule_read reader)
        ~on_note ~finish
    in
    let error_handler _ = finish (Error "HTTP/1.1 response error") in
    let writer =
      Httpun_eio.Client.request conn
        (Httpun.Request.create
           ~headers:(Httpun.Headers.of_list (("host", authority) :: hs))
           `POST path)
        ~error_handler ~response_handler
    in
    Httpun.Body.Writer.write_string writer body;
    Httpun.Body.Writer.close writer;
    (* HTTP/1.1 can only abandon a response by dropping the connection. *)
    fun () -> dispose connection
  in
  let send ~headers:extra ~id:_ body ~on_note =
    Eio.Pool.use pool @@ fun connection ->
    exchange ~max_queued ~on_note (request connection extra body)
  in
  (* A message without a response takes a connection of its own: an answer
     sent from a callback must not wait for the connection its request holds. *)
  let post ~headers:extra body =
    Eio.Switch.run @@ fun sw ->
    let connection = open_connection sw in
    Fun.protect ~finally:(fun () -> dispose connection) @@ fun () ->
    ignore (exchange ~max_queued ~on_note:ignore (request connection extra body))
  in
  { send; post; requests = (fun f -> on_request := f) }

let http1 ~sw ~net ?(host = Eio.Net.Ipaddr.V4.loopback) ?path ?connections ?max_queued ?max_response
    ?headers ~port () =
  let authority = Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp host port in
  v ~sw ?path ?connections ?max_queued ?max_response ?headers ~authority (fun sw ->
      Eio.Net.connect ~sw net (`Tcp (host, port)))
