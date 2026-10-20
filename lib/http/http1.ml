open Httpun

let response status headers =
  Response.create ~headers:(Headers.of_list headers) (Status.of_code status)

let exchange reqd : Exchange.t =
  let req = Reqd.request reqd in
  {
    meth = Method.to_string req.meth;
    header = Headers.get req.headers;
    read_body =
      (let body = Reqd.request_body reqd in
       Exchange.read_body ~schedule_read:(Body.Reader.schedule_read body) ~close:(fun () ->
           Body.Reader.close body));
    respond =
      (fun ~status headers wire ->
        let headers = ("content-length", string_of_int (Mcp.Wire.length wire)) :: headers in
        let w = Reqd.respond_with_streaming reqd (response status headers) in
        Mcp.Wire.iter (Body.Writer.write_string w) wire;
        Body.Writer.close w);
    stream =
      (fun ~status headers ->
        let headers = ("transfer-encoding", "chunked") :: headers in
        let w =
          Reqd.respond_with_streaming ~flush_headers_immediately:true reqd (response status headers)
        in
        {
          write = Body.Writer.write_string w;
          flush =
            (fun () ->
              let flushed, resolve = Eio.Promise.create () in
              Body.Writer.flush w (Eio.Promise.resolve resolve);
              Eio.Promise.await flushed);
          close = (fun () -> Body.Writer.close w);
        });
  }

let connection_handler cfg server ~scope =
 fun socket addr ->
  Exchange.with_connection @@ fun sw ->
  let request_handler _addr (gluten : Reqd.t Gluten.reqd) =
    let reqd = gluten.reqd in
    Eio.Fiber.fork ~sw (fun () ->
        match Exchange.handle cfg server ~scope (exchange reqd) with
        | () -> ()
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception e -> Reqd.report_exn reqd e)
  in
  let error_handler _addr ?request:_ _error start_response =
    Body.Writer.close (start_response Headers.empty)
  in
  Httpun_eio.Server.create_connection_handler ~sw ~request_handler ~error_handler addr socket
