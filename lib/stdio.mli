(** The stdio transport: newline-delimited JSON-RPC over a byte stream.

    Works over any flow pair, so it also serves Unix domain sockets and TCP connections. Requests
    run concurrently, one fiber each, and replies share a buffered writer. Its {!Local} values live
    as long as [serve]. *)

val serve :
  ?max_message:int -> ?max_requests:int -> Server.t -> _ Eio.Flow.source -> _ Eio.Flow.sink -> unit
(** Serve until the input ends, then cancel whatever is still running. [notifications/cancelled]
    cancels the referenced request's fiber.

    - [max_message] bounds one message in bytes (default 64MiB); longer ones are skipped and
      answered with a parse error.
    - [max_requests] bounds the requests running at once (default 1024). Beyond it, a request is
      answered with an internal error straight away, so input keeps flowing and cancellations are
      never held up.
    - Once more than 1MiB of output is waiting for the peer, requests wait for it to be written
      before emitting more, so a peer that stops reading stops handlers rather than growing the
      buffer. *)

val run : Server.t -> < stdin : _ Eio.Flow.source ; stdout : _ Eio.Flow.sink ; .. > -> unit
(** [run server env] serves the process's standard streams. *)
