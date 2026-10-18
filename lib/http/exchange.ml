(* Streamable HTTP (2026-07-28) independent of the HTTP version. The
   HTTP/1.1 adapter presents each request as an [exchange]. *)

open Mcp

type writer = {
  write : string -> unit;
  flush : (bool -> unit) -> unit;  (** Argument: the peer has gone away. *)
  close : unit -> unit;
}

type t = {
  meth : string;
  header : string -> string option;
  read_body : max:int -> string option;  (** [None] when over [max]. *)
  respond : status:int -> (string * string) list -> Wire.t -> unit;
  stream : status:int -> (string * string) list -> writer;
}

type config = {
  allow_origin : string -> bool;
  authenticate : (string -> string option) -> (string option, string) result;
  max_body : int;
  keepalive : float;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
}

(* Without an explicit policy only loopback origins may call us, which is
   what defeats DNS rebinding against local servers. *)
let loopback_origin origin =
  match String.index_opt origin ':' with
  | None -> false
  | Some i ->
      let rest = String.sub origin (i + 1) (String.length origin - i - 1) in
      let host =
        if String.starts_with ~prefix:"//" rest then String.sub rest 2 (String.length rest - 2)
        else rest
      in
      let host =
        if String.starts_with ~prefix:"[" host then
          match String.index_opt host ']' with Some j -> String.sub host 0 (j + 1) | None -> host
        else match String.index_opt host ':' with Some j -> String.sub host 0 j | None -> host
      in
      List.mem host [ "localhost"; "127.0.0.1"; "[::1]" ]

(* Collect a request body delivered through httpun style callbacks. *)
let read_body ~schedule_read ~close ~max =
  let buf = Buffer.create 4096 in
  let result, resolve = Eio.Promise.create () in
  let rec next () =
    schedule_read
      ~on_eof:(fun () -> Eio.Promise.resolve resolve (Some (Buffer.contents buf)))
      ~on_read:(fun bs ~off ~len ->
        if Buffer.length buf + len > max then (
          close ();
          Eio.Promise.resolve resolve None)
        else (
          Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
          next ()))
  in
  next ();
  Eio.Promise.await result

let json_headers = [ ("content-type", "application/json") ]

let sse_headers =
  [
    ("content-type", "text/event-stream"); ("cache-control", "no-cache"); ("x-accel-buffering", "no");
  ]

let reply x ?id e = x.respond ~status:(Error.http_status e) json_headers (Wire.error ?id e)

(* Header values may carry UTF-8 or unsafe bytes as =?base64?...?= *)
let decode_header_value v =
  let n = String.length v in
  if n >= 11 && String.starts_with ~prefix:"=?base64?" v && String.ends_with ~suffix:"?=" v then
    Result.to_option (Base64.decode (String.sub v 9 (n - 11)))
  else if String.for_all (fun c -> c = '\t' || (c >= ' ' && c <= '~')) v then Some v
  else None

let same_value ~numeric header body =
  header = body
  || numeric
     &&
     match (float_of_string_opt header, float_of_string_opt body) with
     | Some a, Some b -> a = b
     | _ -> false

let validate_headers x req =
  let open Server.Request in
  let check name expected ~numeric =
    match Option.map decode_header_value (x.header name) with
    | None -> Error (Error.header_mismatch ("missing " ^ name))
    | Some None -> Error (Error.header_mismatch ("invalid characters in " ^ name))
    | Some (Some v) when same_value ~numeric v expected -> Ok ()
    | Some (Some v) ->
        Error
          (Error.header_mismatch
             (Printf.sprintf "%s header value %S does not match body value %S" name v expected))
  in
  let ( let* ) = Result.bind in
  let* () = check "mcp-protocol-version" (protocol_version req) ~numeric:false in
  let* () = check "mcp-method" (method_ req) ~numeric:false in
  let* () =
    match method_ req with
    | "tools/call" | "resources/read" | "prompts/get" ->
        check "mcp-name" (target req) ~numeric:false
    | _ -> Ok ()
  in
  List.fold_left
    (fun acc (name, value, numeric) ->
      let* () = acc in
      check ("mcp-param-" ^ String.lowercase_ascii name) value ~numeric)
    (Ok ()) (header_args req)

let event w wire =
  w.write "data: ";
  Wire.iter w.write wire;
  w.write "\n\n"

(* Respond with JSON unless the handler emits before finishing, in which case
   switch to an SSE stream scoped to this request. The stream closing is the
   cancellation signal, noticed on every flush and by periodic keepalives. *)
let execute cfg server x ?principal req =
  let gone, set_gone = Eio.Promise.create () in
  let mark_gone closed = if closed then ignore (Eio.Promise.try_resolve set_gone ()) in
  Eio.Switch.run @@ fun sw ->
  let sse = ref None in
  let open_stream () =
    match !sse with
    | Some w -> w
    | None ->
        let w = x.stream ~status:200 sse_headers in
        sse := Some w;
        Eio.Fiber.fork_daemon ~sw (fun () ->
            let rec beat () =
              Eio.Time.sleep cfg.clock cfg.keepalive;
              w.write ":\n\n";
              w.flush mark_gone;
              beat ()
            in
            beat ());
        w
  in
  let emit wire =
    if not (Eio.Promise.is_resolved gone) then begin
      let w = open_stream () in
      event w wire;
      w.flush mark_gone
    end
  in
  Eio.Fiber.first
    (fun () ->
      let wire, status = Server.execute server ?principal ~emit req in
      match !sse with
      | None -> x.respond ~status json_headers wire
      | Some w ->
          event w wire;
          w.close ())
    (fun () -> Eio.Promise.await gone);
  Option.iter (fun w -> if Eio.Promise.is_resolved gone then w.close ()) !sse

let post cfg server x =
  match Option.map cfg.allow_origin (x.header "origin") with
  | Some false ->
      x.respond ~status:403 json_headers (Wire.error (Error.invalid_request "Origin not allowed"))
  | _ -> (
      match cfg.authenticate x.header with
      | Error challenge -> x.respond ~status:401 [ ("www-authenticate", challenge) ] []
      | Ok principal -> (
          match x.read_body ~max:cfg.max_body with
          | None ->
              x.respond ~status:413 json_headers
                (Wire.error (Error.invalid_request "Request body too large"))
          | Some body -> (
              match Server.decode server body with
              | Server.Reject (id, e) -> reply x ?id e
              | Ignore | Cancel _ -> x.respond ~status:202 [] []
              | Request req -> (
                  match validate_headers x req with
                  | Error e -> reply x ~id:(Server.Request.id req) e
                  | Ok () -> execute cfg server x ?principal req))))

exception Connection_closed

(* Requests are fibers of their connection's switch. When the connection
   ends, whatever is still running belongs to a peer that has gone away, so
   it is cancelled rather than left to finish into a dead socket. *)
let with_connection serve =
  match
    Eio.Switch.run @@ fun sw ->
    serve sw;
    Eio.Switch.fail sw Connection_closed
  with
  | () | (exception Connection_closed) -> ()

let handle cfg server x =
  match x.meth with
  | "POST" -> post cfg server x
  | _ -> x.respond ~status:405 [ ("allow", "POST") ] []
