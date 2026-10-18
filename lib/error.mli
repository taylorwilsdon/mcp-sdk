(** JSON-RPC errors, with the MCP codes of revision 2026-07-28. *)

type t = { code : int; message : string; data : Json.t option }

val v : ?data:Json.t -> int -> string -> t
val parse_error_code : int
val invalid_request_code : int
val method_not_found_code : int
val invalid_params_code : int
val internal_error_code : int

val header_mismatch_code : int
(** [-32020] *)

val missing_capability_code : int
(** [-32021] *)

val unsupported_version_code : int
(** [-32022] *)

val parse_error : string -> t
val invalid_request : string -> t
val method_not_found : string -> t
val invalid_params : ?data:Json.t -> string -> t
val internal_error : string -> t
val header_mismatch : string -> t

val missing_capability : Json.t -> t
(** From the [requiredCapabilities] the client did not declare. *)

val unsupported_version : supported:string list -> requested:string -> t

val http_status : t -> int
(** The status Streamable HTTP pairs with the error: 404 for an unknown method, 400 for malformed
    requests and the MCP codes, 200 otherwise. *)

val codec : t Jsont.t
val pp : Format.formatter -> t -> unit
