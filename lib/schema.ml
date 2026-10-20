(* An applicative description of a JSON object that yields both its JSON
   Schema and a jsont codec decoding straight to a typed OCaml value.

   A property type pairs a pure jsont decoder, from JSON to a representation
   ['r], with a conversion from ['r] to the property's type, much as jsont's
   own maps do. Decoding a message only runs the decoders: each field becomes
   an existential [binding] keyed by a fresh type witness, collected in field
   order with no intermediate JSON tree and no lookup by name. Conversions,
   which are application code, run later when the request executes, by
   consuming the bindings in that same order. *)

(* A header capture is (x-mcp-header name, value, value is an integer). *)
type header = string * string * bool

type 'a ty =
  | Ty : {
      codec : 'r Jsont.t;
      convert : 'r -> 'a;
      captures : 'r -> header list;  (** Header values of nested properties. *)
      header : (bool * ('r -> string)) option;  (** May mirror an [x-mcp-header]. *)
      encode : 'a -> Json.t option;  (** For advertised defaults. *)
      schema : (string * Json.t) list;
    }
      -> 'a ty

(* A field's representation, [None] when absent, and its header captures. *)
type binding = B : 'r Type.Id.t * 'r option * header list -> binding

type field = {
  name : string;
  property : Json.t;
  required : bool;
  decode : binding Jsont.t;
  absent : binding option;
}

type 'a t = { fields : field list; eval : binding list -> 'a * binding list }

(* A rejected conversion: the property path and why. *)
exception Invalid of string list * string

let no_captures _ = []

let prim ?header codec schema =
  Ty
    {
      codec;
      convert = Fun.id;
      captures = no_captures;
      header;
      encode = (fun v -> Result.to_option (Jsont.Json.encode codec v));
      schema;
    }

let string = prim Jsont.string [ ("type", Json.str "string") ] ~header:(false, Fun.id)
let int = prim Jsont.int [ ("type", Json.str "integer") ] ~header:(true, string_of_int)
let float = prim Jsont.number [ ("type", Json.str "number") ]
let bool = prim Jsont.bool [ ("type", Json.str "boolean") ] ~header:(false, string_of_bool)

let enum cases =
  let label v = fst (List.find (fun (_, v') -> v' = v) cases) in
  prim (Jsont.enum cases)
    [ ("type", Json.str "string"); ("enum", Json.list (List.map (fun (s, _) -> Json.str s) cases)) ]
    ~header:(false, label)

let list (Ty t) =
  let encode l =
    let items = List.filter_map t.encode l in
    if List.compare_lengths items l = 0 then Some (Json.list items) else None
  in
  Ty
    {
      codec = Jsont.list t.codec;
      convert = List.map t.convert;
      captures = no_captures;
      header = None;
      encode;
      schema = [ ("type", Json.str "array"); ("items", Json.obj t.schema) ];
    }

let json = prim Jsont.json []
let of_jsont ~schema codec = prim codec (Json.mems schema)

let refine ?schema ?enc f (Ty t) =
  let convert r = match f (t.convert r) with Ok v -> v | Error msg -> raise (Invalid ([], msg)) in
  let encode = match enc with Some enc -> fun v -> t.encode (enc v) | None -> fun _ -> None in
  Ty
    {
      codec = t.codec;
      convert;
      captures = t.captures;
      header = t.header;
      encode;
      schema = t.schema @ Option.fold schema ~none:[] ~some:Json.mems;
    }

let is_tchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

let property ?doc ?header ?default schema =
  let extra =
    List.concat
      [
        Option.fold doc ~none:[] ~some:(fun d -> [ ("description", Json.str d) ]);
        Option.fold header ~none:[] ~some:(fun h -> [ ("x-mcp-header", Json.str h) ]);
        Option.fold default ~none:[] ~some:(fun d -> [ ("default", d) ]);
      ]
  in
  Json.obj (schema @ extra)

(* Bindings are produced and consumed in field order, so the witness check in
   [take] can only fail if that invariant is broken inside this module. *)
let take (type r) (id : r Type.Id.t) = function
  | B (id', v, _) :: rest -> (
      match Type.Id.provably_equal id id' with
      | Some Type.Equal -> ((v : r option), rest)
      | None -> invalid_arg "Schema: binding order invariant broken")
  | [] -> invalid_arg "Schema: binding order invariant broken"

(* A property [name] of type [ty], injected with [inj]; [absent] is its value
   when missing, [None] if it is required. *)
let field ?doc ?header ?default name (Ty t) inj absent =
  let capture =
    match (header, t.header) with
    | None, _ -> None
    | Some h, _ when h = "" || not (String.for_all is_tchar h) ->
        invalid_arg (Printf.sprintf "Schema: x-mcp-header %S on %S is not an HTTP token" h name)
    | Some _, None ->
        invalid_arg
          (Printf.sprintf "Schema: x-mcp-header on %S needs a string, integer or boolean" name)
    | Some h, Some (numeric, repr) -> Some (fun r -> (h, repr r, numeric))
  in
  let id = Type.Id.make () in
  let decode =
    Jsont.map t.codec ~dec:(fun r ->
        let nested = t.captures r in
        B (id, Some r, match capture with Some c -> c r :: nested | None -> nested))
  in
  let convert r =
    try t.convert r with Invalid (path, msg) -> raise (Invalid (name :: path, msg))
  in
  let eval bindings =
    let r, rest = take id bindings in
    ((match r with Some r -> inj (convert r) | None -> Option.get absent), rest)
  in
  let property = property ?doc ?header ?default:(Option.bind default t.encode) t.schema in
  let field =
    {
      name;
      property;
      required = Option.is_none absent;
      decode;
      absent = Option.map (fun _ -> B (id, None, [])) absent;
    }
  in
  { fields = [ field ]; eval }

let req ?doc ?header name ty = field ?doc ?header name ty Fun.id None
let opt ?doc ?header name ty = field ?doc ?header name ty Option.some (Some None)
let dft ?doc ?header name ty default = field ?doc ?header ~default name ty Fun.id (Some default)
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

let bindings ~kind t =
  let (Chain c) = chain t.fields in
  add_fields (Jsont.Object.map ~kind (curry c [])) c |> Jsont.Object.finish

let headers bindings = List.concat_map (fun (B (_, _, h)) -> h) bindings

(* Conversions are application code: any failure but cancellation rejects the
   arguments. *)
let convert t bindings =
  match fst (t.eval bindings) with
  | v -> Ok v
  | exception Invalid (path, msg) -> Error (String.concat "." path ^ ": " ^ msg)
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e -> Error (Printexc.to_string e)

let decoder ?(kind = "arguments") t =
  Jsont.map (bindings ~kind t) ~dec:(fun bs -> ((fun () -> convert t bs), headers bs))

let codec ?(kind = "arguments") t =
  Jsont.map (bindings ~kind t) ~dec:(fun bs ->
      match convert t bs with Ok v -> v | Error msg -> Jsont.Error.msg Jsont.Meta.none msg)

let obj t =
  Ty
    {
      codec = bindings ~kind:"object" t;
      convert = (fun bs -> fst (t.eval bs));
      captures = headers;
      header = None;
      encode = (fun _ -> None);
      schema = Json.mems (json_schema t);
    }
