(* Newline-delimited JSON-RPC over any byte stream: the stdio transport, but
   equally a Unix socket or TCP connection. One reader loop, a fiber per
   request, and a shared buffered writer that coalesces concurrent replies
   into as few writes as the kernel will take.

   The reader never waits: requests beyond [max_requests] are refused at
   once, so cancellations always get through. Requests do wait when more than
   [high_water] bytes of output are unwritten, so a peer that stops reading
   stops handlers rather than growing the buffer. *)

let high_water = 1 lsl 20

let serve ?(max_message = 1 lsl 26) ?(max_requests = 1024) server source sink =
  Eio.Switch.run @@ fun sw ->
  Eio.Buf_write.with_flow sink @@ fun w ->
  let send wire =
    Wire.iter (Eio.Buf_write.string w) wire;
    Eio.Buf_write.char w '\n'
  in
  let send_paced wire =
    send wire;
    if Eio.Buf_write.pending_bytes w > high_water then Eio.Buf_write.flush w
  in
  let scope = Local.scope sw in
  let inflight : (Id.t, unit Eio.Promise.u) Hashtbl.t = Hashtbl.create 64 in
  let cancel u = ignore (Eio.Promise.try_resolve u ()) in
  let run req =
    let id = Server.Request.id req in
    let cancelled, resolver = Eio.Promise.create () in
    Hashtbl.replace inflight id resolver;
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Fiber.first
          (fun () ->
            let wire, _status = Server.execute server ~scope ~emit:send_paced req in
            send_paced wire)
          (fun () -> Eio.Promise.await cancelled);
        match Hashtbl.find_opt inflight id with
        | Some r when r == resolver -> Hashtbl.remove inflight id
        | _ -> ())
  in
  (* A 2025-11-25 session lasts as long as the connection. *)
  let session = ref None in
  let handle line =
    match Server.decode server ?session:!session line with
    | Server.Request req when Hashtbl.length inflight >= max_requests ->
        send
          (Wire.error ~id:(Server.Request.id req)
             (Error.internal_error "Too many requests in flight"))
    | Request req ->
        Option.iter (fun s -> session := Some s) (Server.Request.session req);
        run req
    | Cancel id -> Option.iter cancel (Hashtbl.find_opt inflight id)
    | Ignore -> ()
    | Reject (id, e) -> send (Wire.error ?id e)
  in
  let input = Eio.Buf_read.of_flow source ~max_size:max_message in
  let rec loop () =
    match Eio.Buf_read.line input with
    | "" -> loop ()
    | line ->
        handle line;
        loop ()
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        (* Drop the oversized message without buffering it and carry on. *)
        Eio.Buf_read.skip_while (( <> ) '\n') input;
        send (Wire.error (Error.parse_error "Message too large"));
        loop ()
    | exception End_of_file -> ()
  in
  loop ();
  (* End of input is the shutdown signal: abandon whatever is still running. *)
  Hashtbl.iter (fun _ u -> cancel u) inflight

let run server env = serve server (Eio.Stdenv.stdin env) (Eio.Stdenv.stdout env)
