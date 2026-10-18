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

let encode codec v =
  match Jsont_bytesrw.encode_string ~buf:(Domain.DLS.get scratch) codec v with
  | Ok s -> s
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
