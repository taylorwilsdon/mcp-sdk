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
   request-scoped notifications terminated by the response. *)
let read_response ~status ~sse ~schedule_read ~on_note ~finish =
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
        next ())
  in
  next ()

let headers body extra =
  ("content-type", "application/json")
  :: ("accept", "application/json, text/event-stream")
  :: ("content-length", string_of_int (String.length body))
  :: List.map (fun (k, v) -> (String.lowercase_ascii k, v)) extra

let is_sse content_type =
  Option.fold ~none:false ~some:(String.starts_with ~prefix:"text/event-stream") content_type

(* The connection reader queues events; the requesting fiber runs callbacks
   in order, allowing them to make requests without blocking that reader. *)
let exchange ~on_note start =
  let events = Eio.Stream.create max_int and finished = ref false in
  let finish result =
    if not !finished then (
      finished := true;
      Eio.Stream.add events (`Response result))
  in
  let note raw = if not !finished then Eio.Stream.add events (`Note raw) in
  let abort = start ~on_note:note finish in
  let rec receive () =
    match Eio.Stream.take events with
    | `Note raw ->
        on_note raw;
        receive ()
    | `Response (Ok body) -> body
    | `Response (Error msg) -> failwith msg
  in
  match receive () with
  | body -> body
  | exception e ->
      Eio.Cancel.protect abort;
      raise e

let authority host port = Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp host port

let http1 ~sw ~net ?(host = Eio.Net.Ipaddr.V4.loopback) ?(path = "/mcp") ?(connections = 64) ~port
    () : Mcp.Client.transport =
  let authority = authority host port in
  let connect () =
    let socket = Eio.Net.connect ~sw net (`Tcp (host, port)) in
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
    Eio.Pool.create connections connect
      ~validate:(fun (c, _, disposed) -> (not !disposed) && not (Httpun_eio.Client.is_closed c))
      ~dispose
  in
  let send ~headers:extra ~id:_ body ~on_note =
    Eio.Pool.use pool @@ fun ((conn, _, _) as connection) ->
    exchange ~on_note @@ fun ~on_note finish ->
    let headers = Httpun.Headers.of_list (("host", authority) :: headers body extra) in
    let response_handler (resp : Httpun.Response.t) reader =
      read_response
        ~status:(Httpun.Status.to_code resp.status)
        ~sse:(is_sse (Httpun.Headers.get resp.headers "content-type"))
        ~schedule_read:(Httpun.Body.Reader.schedule_read reader)
        ~on_note ~finish
    in
    let error_handler _ = finish (Error "HTTP/1.1 response error") in
    let writer =
      Httpun_eio.Client.request conn
        (Httpun.Request.create ~headers `POST path)
        ~error_handler ~response_handler
    in
    Httpun.Body.Writer.write_string writer body;
    Httpun.Body.Writer.close writer;
    (* HTTP/1.1 can only abandon a response by dropping the connection. *)
    fun () -> dispose connection
  in
  { send }
