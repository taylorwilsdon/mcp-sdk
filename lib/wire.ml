type t = string list

let result_head = "{\"jsonrpc\":\"2.0\",\"id\":"
let error_head = "{\"jsonrpc\":\"2.0\",\"error\":"
let result id body = [ result_head; Id.to_wire id; ",\"result\":"; body; "}" ]

let error ?id e =
  let err = Json.encode Error.codec e in
  match id with
  | None -> [ error_head; err; "}" ]
  | Some id -> [ result_head; Id.to_wire id; ",\"error\":"; err; "}" ]

let notification method_ params =
  [ "{\"jsonrpc\":\"2.0\",\"method\":"; Json.quote method_; ",\"params\":"; params; "}" ]

let request id method_ params =
  [ result_head; Id.to_wire id; ",\"method\":"; Json.quote method_; ",\"params\":"; params; "}" ]

let iter = List.iter
let length t = List.fold_left (fun n s -> n + String.length s) 0 t
let to_string t = String.concat "" t
