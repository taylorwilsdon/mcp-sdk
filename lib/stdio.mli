(** The stdio transport: newline-delimited JSON-RPC over a byte stream.

    Works over any flow pair, so it also serves Unix domain sockets and TCP connections. Requests
    run concurrently, one fiber each, and replies share a buffered writer. *)

val serve : ?max_message:int -> Server.t -> _ Eio.Flow.source -> _ Eio.Flow.sink -> unit
(** Serve until the input ends, then cancel whatever is still running. [notifications/cancelled]
    cancels the referenced request's fiber. [max_message] bounds one message in bytes (default
    64MiB); longer ones are skipped and answered with a parse error. *)

val run : Server.t -> < stdin : _ Eio.Flow.source ; stdout : _ Eio.Flow.sink ; .. > -> unit
(** [run server env] serves the process's standard streams. *)
