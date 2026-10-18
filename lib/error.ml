type t = { code : int; message : string; data : Json.t option }

let v ?data code message = { code; message; data }
let parse_error_code = -32700
let invalid_request_code = -32600
let method_not_found_code = -32601
let invalid_params_code = -32602
let internal_error_code = -32603
let header_mismatch_code = -32020
let missing_capability_code = -32021
let unsupported_version_code = -32022
let parse_error message = v parse_error_code message
let invalid_request message = v invalid_request_code message
let method_not_found name = v method_not_found_code ("Method not found: " ^ name)
let invalid_params ?data message = v ?data invalid_params_code message
let internal_error message = v internal_error_code message
let header_mismatch message = v header_mismatch_code ("Header mismatch: " ^ message)

let missing_capability required =
  v missing_capability_code "Missing required client capability"
    ~data:(Json.obj [ ("requiredCapabilities", required) ])

let unsupported_version ~supported ~requested =
  v unsupported_version_code "Unsupported protocol version"
    ~data:
      (Json.obj
         [
           ("supported", Json.list (List.map Json.str supported)); ("requested", Json.str requested);
         ])

let http_status e =
  match e.code with
  | -32601 -> 404
  | -32700 | -32600 | -32602 | -32020 | -32021 | -32022 -> 400
  | _ -> 200

let codec =
  Jsont.Object.map ~kind:"JSON-RPC error" (fun code message data -> { code; message; data })
  |> Jsont.Object.mem "code" Jsont.int ~enc:(fun e -> e.code)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun e -> e.message)
  |> Jsont.Object.opt_mem "data" Jsont.json ~enc:(fun e -> e.data)
  |> Jsont.Object.finish

let pp ppf e = Format.fprintf ppf "%d: %s" e.code e.message
