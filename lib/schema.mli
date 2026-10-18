(** Typed tool arguments.

    A ['a t] describes a JSON object. The same value yields the JSON Schema advertised in
    [tools/list] and a decoder that turns the [arguments] of a [tools/call] straight into an ['a],
    in one streaming pass with no intermediate JSON tree.

    {[
    let args =
      Schema.(
        let+ city = req "city" string ~doc:"City name" and+ days = dft "days" int 3 in
        (city, days))
    ]} *)

type 'a ty
(** The type of a property's value. *)

val string : string ty
val int : int ty
val float : float ty
val bool : bool ty

val enum : (string * 'a) list -> 'a ty
(** A string restricted to the given labels. *)

val list : 'a ty -> 'a list ty

val json : ?schema:Json.t -> unit -> Json.t ty
(** Any JSON value, optionally described by an explicit [schema]. *)

type 'a t
(** An object schema producing ['a]. *)

val obj : 'a t -> 'a ty
(** A nested object. *)

val req : ?doc:string -> ?header:string -> string -> 'a ty -> 'a t
(** A required property. [header] mirrors the value into the [Mcp-Param-{header}] HTTP header
    ([x-mcp-header]); it is only allowed on string, integer, boolean and enum properties, and must
    be an HTTP token.
    @raise Invalid_argument otherwise. *)

val opt : ?doc:string -> ?header:string -> string -> 'a ty -> 'a option t
(** An optional property. *)

val dft : ?doc:string -> ?header:string -> string -> 'a ty -> 'a -> 'a t
(** An optional property with a default, advertised as ["default"]. *)

val const : 'a -> 'a t
val empty : unit t
val map : ('a -> 'b) -> 'a t -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

val json_schema : 'a t -> Json.t
(** The JSON Schema (2020-12) of the object. *)

val codec : ?kind:string -> 'a t -> 'a Jsont.t
(** A decode-only codec for the object. *)

type header = string * string * bool
(** A captured [x-mcp-header] value: header name, value as it must appear on the wire, and whether
    it is an integer (compared numerically). *)

val decoder : ?kind:string -> 'a t -> ('a * header list) Jsont.t
(** Like {!codec}, also returning the header values present. *)

val is_tchar : char -> bool
(** Whether a character may appear in an HTTP token (RFC 9110). *)
