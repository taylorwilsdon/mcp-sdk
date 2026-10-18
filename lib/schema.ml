(* An applicative description of a JSON object that yields both its JSON
   Schema and a jsont codec decoding straight to a typed OCaml value.

   Each field decodes to an existential [binding] keyed by a fresh type
   witness. The compiled object map collects the bindings in field order and
   [eval] consumes them in the same order, so no intermediate JSON tree is
   built and no lookup by name happens at decode time. *)

(* A header capture is (x-mcp-header name, value, value is an integer). *)
type header = string * string * bool
type binding = B : 'a Type.Id.t * 'a * header list -> binding

type 'a ty = {
  codec : 'a Jsont.t;
  schema : (string * Json.t) list;
  header : (bool * ('a -> string)) option;
  captures : ('a * header list) Jsont.t option;
}

type field = {
  name : string;
  property : Json.t;
  required : bool;
  decode : binding Jsont.t;
  absent : binding option;
}

type 'a t = { fields : field list; eval : binding list -> 'a * binding list }

let string =
  {
    codec = Jsont.string;
    schema = [ ("type", Json.str "string") ];
    header = Some (false, Fun.id);
    captures = None;
  }

let int =
  {
    codec = Jsont.int;
    schema = [ ("type", Json.str "integer") ];
    header = Some (true, string_of_int);
    captures = None;
  }

let float =
  { codec = Jsont.number; schema = [ ("type", Json.str "number") ]; header = None; captures = None }

let bool =
  {
    codec = Jsont.bool;
    schema = [ ("type", Json.str "boolean") ];
    header = Some (false, string_of_bool);
    captures = None;
  }

let enum cases =
  let label v = fst (List.find (fun (_, v') -> v' = v) cases) in
  {
    codec = Jsont.enum cases;
    schema =
      [
        ("type", Json.str "string"); ("enum", Json.list (List.map (fun (s, _) -> Json.str s) cases));
      ];
    header = Some (false, label);
    captures = None;
  }

let list ty =
  {
    codec = Jsont.list ty.codec;
    schema = [ ("type", Json.str "array"); ("items", Json.obj ty.schema) ];
    header = None;
    captures = None;
  }

let json ?(schema = Json.empty) () =
  { codec = Jsont.json; schema = Json.mems schema; header = None; captures = None }

let is_tchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

let header_repr name header ty =
  match (header, ty.header) with
  | None, _ -> None
  | Some h, _ when h = "" || not (String.for_all is_tchar h) ->
      invalid_arg (Printf.sprintf "Schema: x-mcp-header %S on %S is not an HTTP token" h name)
  | Some _, None ->
      invalid_arg
        (Printf.sprintf "Schema: x-mcp-header on %S needs a string, integer or boolean" name)
  | Some h, Some (numeric, repr) -> Some (h, numeric, repr)

let property ?doc ?header ?default ty =
  let extra =
    List.concat
      [
        Option.fold doc ~none:[] ~some:(fun d -> [ ("description", Json.str d) ]);
        Option.fold header ~none:[] ~some:(fun h -> [ ("x-mcp-header", Json.str h) ]);
        Option.fold default ~none:[] ~some:(fun d -> [ ("default", d) ]);
      ]
  in
  Json.obj (ty.schema @ extra)

(* Bindings are produced and consumed in field order, so the witness check in
   [take] can only fail if that invariant is broken inside this module. *)
let take (type a) (id : a Type.Id.t) = function
  | B (id', v, _) :: rest -> (
      match Type.Id.provably_equal id id' with
      | Some Type.Equal -> ((v : a), rest)
      | None -> invalid_arg "Schema: binding order invariant broken")
  | [] -> invalid_arg "Schema: binding order invariant broken"

let field_of (type a b) ~name ~property ~required ~(absent : b option) (ty : a ty) (inj : a -> b)
    hdr : b t =
  let id : b Type.Id.t = Type.Id.make () in
  let codec =
    match ty.captures with
    | Some codec -> codec
    | None -> Jsont.map ty.codec ~dec:(fun v -> (v, []))
  in
  let decode =
    Jsont.map codec ~dec:(fun (v, headers) ->
        let headers =
          match hdr with
          | None -> headers
          | Some (h, numeric, repr) -> (h, repr v, numeric) :: headers
        in
        B (id, inj v, headers))
  in
  let absent = Option.map (fun v -> B (id, v, [])) absent in
  { fields = [ { name; property; required; decode; absent } ]; eval = take id }

let req ?doc ?header name ty =
  let hdr = header_repr name header ty in
  field_of ~name ~property:(property ?doc ?header ty) ~required:true ~absent:None ty Fun.id hdr

let opt ?doc ?header name ty =
  let hdr = header_repr name header ty in
  field_of ~name ~property:(property ?doc ?header ty) ~required:false ~absent:(Some None) ty
    Option.some hdr

let dft ?doc ?header name ty default =
  let hdr = header_repr name header ty in
  let default_json = Result.to_option (Jsont.Json.encode ty.codec default) in
  field_of ~name
    ~property:(property ?doc ?header ?default:default_json ty)
    ~required:false ~absent:(Some default) ty Fun.id hdr

let const v = { fields = []; eval = (fun bs -> (v, bs)) }
let empty = const ()

let map f t =
  {
    t with
    eval =
      (fun bs ->
        let v, bs = t.eval bs in
        (f v, bs));
  }

let both a b =
  {
    fields = a.fields @ b.fields;
    eval =
      (fun bs ->
        let va, bs = a.eval bs in
        let vb, bs = b.eval bs in
        ((va, vb), bs));
  }

let ( let+ ) t f = map f t
let ( and+ ) = both

let json_schema t =
  let properties = Json.obj (List.map (fun f -> (f.name, f.property)) t.fields) in
  let required =
    List.filter_map (fun f -> if f.required then Some (Json.str f.name) else None) t.fields
  in
  Json.obj
    ((("type", Json.str "object") :: [ ("properties", properties) ])
    @ if required = [] then [] else [ ("required", Json.list required) ])

(* A field list as a GADT so the object map's constructor gets exactly one
   curried [binding] argument per field. *)
type _ chain = Nil : binding list chain | Cons : field * 'd chain -> (binding -> 'd) chain
type packed = Chain : 'd chain -> packed

let rec chain = function
  | [] -> Chain Nil
  | f :: fs ->
      let (Chain c) = chain fs in
      Chain (Cons (f, c))

let rec curry : type d. d chain -> binding list -> d =
 fun c acc -> match c with Nil -> List.rev acc | Cons (_, c) -> fun b -> curry c (b :: acc)

let rec add_fields : type d.
    (binding list, d) Jsont.Object.map -> d chain -> (binding list, binding list) Jsont.Object.map =
 fun map -> function
  | Nil -> map
  | Cons (f, c) -> add_fields (Jsont.Object.mem f.name f.decode ?dec_absent:f.absent map) c

let headers bindings = List.concat_map (fun (B (_, _, h)) -> h) bindings

let decoder ?(kind = "arguments") t =
  let (Chain c) = chain t.fields in
  add_fields (Jsont.Object.map ~kind (curry c [])) c
  |> Jsont.Object.finish
  |> Jsont.map ~dec:(fun bindings -> (fst (t.eval bindings), headers bindings))

let codec ?kind t = Jsont.map ~dec:fst (decoder ?kind t)

let obj t =
  let captures = decoder ~kind:"object" t in
  {
    codec = Jsont.map ~dec:fst captures;
    schema = Json.mems (json_schema t);
    header = None;
    captures = Some captures;
  }
