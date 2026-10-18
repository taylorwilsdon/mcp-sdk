type result = Proto.Read_result.t Input.step
type handler = Ctx.t -> (result, Error.t) Stdlib.result
type t = { info : Proto.Resource.t; read : handler }

let v ?title ?description ?mime_type ?size ?(icons = []) ?annotations ?meta ~uri name read =
  { info = { uri; name; title; description; mime_type; size; icons; annotations; meta }; read }

let uri t = t.info.uri
let info t = t.info
let read t = t.read

let contents ?(ttl_ms = 0) ?(cache_scope = Proto.Private) contents : result =
  Input.Complete { contents; ttl_ms; cache_scope; meta = None }

let text ?mime_type ?ttl_ms ?cache_scope uri s =
  contents ?ttl_ms ?cache_scope [ Proto.Contents.text ?mime_type uri s ]

let blob ?mime_type ?ttl_ms ?cache_scope uri b =
  contents ?ttl_ms ?cache_scope [ Proto.Contents.blob ?mime_type uri b ]

(* RFC 6570 level 1 and 2 templates: [{var}] matches one path segment and
   [{+var}] matches anything, both decoding percent escapes. *)
module Uri_template = struct
  type segment = Lit of string | Var of { name : string; reserved : bool }

  let parse s =
    let n = String.length s in
    let literal_until i = Option.value (String.index_from_opt s i '{') ~default:n in
    let rec go i acc =
      if i >= n then List.rev acc
      else
        match (s.[i], String.index_from_opt s i '}') with
        | '{', Some j ->
            let body = String.sub s (i + 1) (j - i - 1) in
            let reserved = String.starts_with ~prefix:"+" body in
            let name = if reserved then String.sub body 1 (String.length body - 1) else body in
            go (j + 1) (Var { name; reserved } :: acc)
        | '{', None -> List.rev (Lit (String.sub s i (n - i)) :: acc)
        | _ ->
            let j = literal_until i in
            go j (Lit (String.sub s i (j - i)) :: acc)
    in
    go 0 []

  let hex = function
    | '0' .. '9' as c -> Some (Char.code c - 48)
    | 'a' .. 'f' as c -> Some (Char.code c - 87)
    | 'A' .. 'F' as c -> Some (Char.code c - 55)
    | _ -> None

  let pct_decode s =
    let n = String.length s in
    let b = Buffer.create n in
    let rec go i =
      if i < n then
        match (s.[i], if i + 2 < n then (hex s.[i + 1], hex s.[i + 2]) else (None, None)) with
        | '%', (Some h, Some l) ->
            Buffer.add_char b (Char.chr ((h lsl 4) lor l));
            go (i + 3)
        | c, _ ->
            Buffer.add_char b c;
            go (i + 1)
    in
    go 0;
    Buffer.contents b

  let starts_at s pos lit =
    let l = String.length lit in
    pos + l <= String.length s && String.sub s pos l = lit

  (* Variables match minimally and never empty; literals anchor them. *)
  let matches segments uri =
    let n = String.length uri in
    let rec go segs pos acc =
      match segs with
      | [] -> if pos = n then Some (List.rev acc) else None
      | Lit l :: rest -> if starts_at uri pos l then go rest (pos + String.length l) acc else None
      | Var { name; reserved } :: rest ->
          let rec try_end e =
            if e > n || ((not reserved) && uri.[e - 1] = '/') then None
            else
              let value = pct_decode (String.sub uri pos (e - pos)) in
              match go rest e ((name, value) :: acc) with
              | Some _ as r -> r
              | None -> try_end (e + 1)
          in
          try_end (pos + 1)
    in
    go segments 0 []
end

type template = {
  tinfo : Proto.Resource_template.t;
  segments : Uri_template.segment list;
  tread : Ctx.t -> string -> (string * string) list -> (result, Error.t) Stdlib.result;
  complete : (string -> string -> string list) option;
}

let template ?title ?description ?mime_type ?(icons = []) ?annotations ?complete ~uri_template name
    read =
  {
    tinfo = { uri_template; name; title; description; mime_type; icons; annotations };
    segments = Uri_template.parse uri_template;
    tread = read;
    complete;
  }

let template_info t = t.tinfo
let read_template t = t.tread
let completer t = t.complete
let match_template t uri = Uri_template.matches t.segments uri
