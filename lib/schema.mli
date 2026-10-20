(** Typed tool arguments.

    A ['a t] describes a JSON object. The same value yields the JSON Schema advertised in
    [tools/list] and a decoder that turns the [arguments] of a [tools/call] straight into an ['a],
    in one streaming pass with no intermediate JSON tree.

    Decoding and conversion are separate steps. Decoding only runs jsont codecs, as the message is
    read; conversions given to {!map} and {!refine} run when the request executes, on its own fiber
    and under its cancellation. A failed conversion rejects the arguments with [-32602].

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

val json : Json.t ty
(** Any JSON value, advertised as the empty schema. *)

val of_jsont : schema:Json.t -> 'a Jsont.t -> 'a ty
(** A property decoded by a jsont codec and advertised as [schema]: for unions ({!Jsont.any},
    {!Jsont.Object.case_mem}), recursive values ({!Jsont.rec'}) or a codec the application already
    has. The codec is the validation and [schema] only describes it, so keep the two in agreement.
    The codec runs while the message is decoded: keep it pure, and leave conversions to {!refine}.
*)

val refine : ?schema:Json.t -> ?enc:('b -> 'a) -> ('a -> ('b, string) result) -> 'a ty -> 'b ty
(** [refine f ty] checks or converts a value of [ty] with [f]; [Error msg] rejects the arguments,
    naming the property. The members of [schema], e.g. [{"minimum": 1}], are added to the advertised
    schema, so that it states what [f] checks. [enc] lets {!dft} advertise its default.

    {[
    let port =
      refine
        ~schema:(Json.obj [ ("minimum", Json.int 1) ])
        (fun n -> if n >= 1 then Ok n else Error "must be at least 1")
        int
    ]} *)

type 'a t
(** An object schema producing ['a]. *)

val obj : 'a t -> 'a ty
(** A nested object, preserving captures of header-annotated properties inside it. *)

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
(** [map f t] converts with [f]. An exception raised by [f] rejects the arguments. *)

val both : 'a t -> 'b t -> ('a * 'b) t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

val json_schema : 'a t -> Json.t
(** The JSON Schema (2020-12) of the object. *)

val codec : ?kind:string -> 'a t -> 'a Jsont.t
(** A decode-only codec for the object that also runs its conversions, for decoding outside a
    request, e.g. an elicitation answer or an upstream response. *)

type header = string * string * bool
(** A captured [x-mcp-header] value: header name, value as it must appear on the wire, and whether
    it is an integer (compared numerically). *)

val decoder : ?kind:string -> 'a t -> ((unit -> ('a, string) result) * header list) Jsont.t
(** Decodes the object without converting it: the conversion to run when the request executes, and
    the header values present. *)

val is_tchar : char -> bool
(** Whether a character may appear in an HTTP token (RFC 9110). *)
