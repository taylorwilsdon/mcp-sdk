type t = Int of int | String of string

let of_json = function
  | Jsont.String (s, _) -> String s
  | Jsont.Number (f, _) when Float.is_integer f && Float.abs f <= 0x1p53 -> Int (Float.to_int f)
  | j -> Jsont.Error.msg (Jsont.Json.meta j) "Request id must be a string or integer"

let to_json = function Int i -> Json.int i | String s -> Json.str s
let codec = Jsont.map ~kind:"request id" ~dec:of_json ~enc:to_json Jsont.json
let to_wire = function Int i -> string_of_int i | String s -> Json.quote s
let pp ppf = function Int i -> Format.pp_print_int ppf i | String s -> Format.fprintf ppf "%S" s
