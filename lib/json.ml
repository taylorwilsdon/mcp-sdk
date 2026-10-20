type t = Jsont.json

let str s = Jsont.Json.string s
let int i = Jsont.Json.int i
let num f = Jsont.Json.number f
let bool b = Jsont.Json.bool b
let list l = Jsont.Json.list l
let null = Jsont.Json.null ()

let obj mems =
  Jsont.Json.object' (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) mems)

let empty = obj []

let mems = function
  | Jsont.Object (mems, _) -> List.map (fun ((name, _), v) -> (name, v)) mems
  | _ -> []

let find key = function
  | Jsont.Object (mems, _) -> Option.map snd (Jsont.Json.find_mem key mems)
  | _ -> None

let rec find_path path json =
  match path with [] -> Some json | key :: rest -> Option.bind (find key json) (find_path rest)

(* jsont otherwise allocates a 64KiB slice buffer per encode, which costs more
   than encoding a typical result. Encoding never yields, so one scratch
   buffer per domain is never shared by two encodes. *)
let scratch = Domain.DLS.new_key (fun () -> Bytes.create 4096)

(* The fewest significant digits that read back as [f]. *)
let shortest f =
  let rec go digits =
    let s = Printf.sprintf "%.*g" digits f in
    if digits = 17 || Float.of_string s = f then s else go (digits + 1)
  in
  go 15

(* jsont prints numbers with %.17g, so 9.3 comes out as 9.3000000000000007.
   Only a number token of 17 significant digits can have a shorter exact form,
   so only those are reprinted, and [s] is returned as is when there are none. *)
let shorten_numbers s =
  let n = String.length s in
  let out = ref None and copied = ref 0 in
  let rewrite i j =
    let token = String.sub s i (j - i) in
    let short = shortest (Float.of_string token) in
    if String.length short < j - i then (
      let b = match !out with Some b -> b | None -> Buffer.create n in
      out := Some b;
      Buffer.add_substring b s !copied (i - !copied);
      Buffer.add_string b short;
      copied := j)
  in
  let rec number_end j =
    match if j < n then s.[j] else ' ' with
    | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> number_end (j + 1)
    | _ -> j
  in
  let rec string_end i =
    match s.[i] with '"' -> i + 1 | '\\' -> string_end (i + 2) | _ -> string_end (i + 1)
  in
  let rec scan i =
    if i < n then
      match s.[i] with
      | '"' -> scan (string_end (i + 1))
      | '-' | '0' .. '9' ->
          let j = number_end i in
          if j - i >= 17 then rewrite i j;
          scan j
      | _ -> scan (i + 1)
  in
  scan 0;
  match !out with
  | None -> s
  | Some b ->
      Buffer.add_substring b s !copied (n - !copied);
      Buffer.contents b

let encode codec v =
  match Jsont_bytesrw.encode_string ~buf:(Domain.DLS.get scratch) codec v with
  | Ok s -> shorten_numbers s
  | Error e -> invalid_arg e

(* jsont renders errors for terminals; protocol messages want one plain line. *)
let error_message (_, _, kind) =
  let s = Jsont.Error.kind_to_string kind in
  let b = Buffer.create (String.length s) in
  let rec text i =
    if i < String.length s then
      match s.[i] with
      | '\027' -> escape (i + 1)
      | '\n' ->
          Buffer.add_char b ' ';
          text (i + 1)
      | c ->
          Buffer.add_char b c;
          text (i + 1)
  and escape i = if i < String.length s then if s.[i] = 'm' then text (i + 1) else escape (i + 1) in
  text 0;
  Buffer.contents b

let decode codec s = Result.map_error error_message (Jsont_bytesrw.decode_string' codec s)
let decode_json codec json = Result.map_error error_message (Jsont.Json.decode' codec json)
let to_string json = encode Jsont.json json
let quote s = encode Jsont.string s
