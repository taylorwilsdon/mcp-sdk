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

  (* Work backwards to compute which suffixes match. Each variable remembers
     the shortest successful end at each offset, avoiding backtracking and
     repeated percent-decoding. Work is bounded by template length * URI length. *)
  let matches segments uri =
    let n = String.length uri in
    let terminal = Array.make (n + 1) false in
    terminal.(n) <- true;
    let reachable, plan =
      List.fold_left
        (fun (suffix, plan) segment ->
          match segment with
          | Lit literal ->
              let len = String.length literal in
              let reachable =
                Array.init (n + 1) (fun pos ->
                    pos + len <= n && suffix.(pos + len) && starts_at uri pos literal)
              in
              (reachable, (segment, [||]) :: plan)
          | Var { reserved; _ } ->
              let ends = Array.make (n + 1) (-1) in
              for pos = n - 1 downto 0 do
                if reserved || uri.[pos] <> '/' then
                  ends.(pos) <- (if suffix.(pos + 1) then pos + 1 else ends.(pos + 1))
              done;
              (Array.map (( <= ) 0) ends, (segment, ends) :: plan))
        (terminal, []) (List.rev segments)
    in
    let rec capture pos acc = function
      | [] -> Some (List.rev acc)
      | (Lit literal, _) :: rest -> capture (pos + String.length literal) acc rest
      | (Var { name; _ }, ends) :: rest ->
          let last = ends.(pos) in
          let value = pct_decode (String.sub uri pos (last - pos)) in
          capture last ((name, value) :: acc) rest
    in
    if reachable.(0) then capture 0 [] plan else None
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
