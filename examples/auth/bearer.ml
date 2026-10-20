(* Bearer tokens for an MCP server acting as an OAuth 2.1 resource server.

   Tokens are HS256 JWTs issued by an authorization server that shares the
   secret. A production deployment would verify asymmetric signatures
   against the issuer's published keys, or introspect opaque tokens; what
   changes is [verify], not how its result reaches the handlers. *)

open Mcp

type config = {
  secret : string;
  issuer : string;  (** The authorization server, [iss]. *)
  resource : string;  (** This server's canonical URI, the [aud] tokens must name. *)
  metadata_url : string;  (** Where the protected resource metadata is served. *)
}

let b64 = Base64.uri_safe_alphabet
let encode s = Base64.encode_string ~pad:false ~alphabet:b64 s
let decode s = Result.to_option (Base64.decode ~pad:false ~alphabet:b64 s)
let json s = Option.bind (decode s) (fun s -> Result.to_option (Json.decode Jsont.json s))
let str = function Some (Jsont.String (s, _)) -> Some s | _ -> None
let mac cfg signed = Digestif.SHA256.hmac_string ~key:cfg.secret signed

let sign cfg ?(scope = "") ?(ttl = 3600.) ?(now = Unix.gettimeofday ()) ?audience sub =
  let claims =
    Json.obj
      [
        ("iss", Json.str cfg.issuer);
        ("aud", Json.str (Option.value audience ~default:cfg.resource));
        ("sub", Json.str sub);
        ("scope", Json.str scope);
        ("exp", Json.num (now +. ttl));
      ]
  in
  let signed = encode {|{"alg":"HS256","typ":"JWT"}|} ^ "." ^ encode (Json.to_string claims) in
  signed ^ "." ^ encode (Digestif.SHA256.to_raw_string (mac cfg signed))

(* The claims of a token this server accepts. *)
let verify cfg ~now token =
  let ( let* ) = Result.bind in
  let* header, payload, signature =
    match String.split_on_char '.' token with
    | [ h; p; s ] -> Ok (h, p, s)
    | _ -> Error "malformed token"
  in
  let* () =
    match Option.bind (decode signature) Digestif.SHA256.of_raw_string_opt with
    | Some s when Digestif.SHA256.equal s (mac cfg (header ^ "." ^ payload)) -> Ok ()
    | _ -> Error "invalid signature"
  in
  let* () =
    match Option.bind (json header) (fun h -> str (Json.find "alg" h)) with
    | Some "HS256" -> Ok ()
    | _ -> Error "unsupported algorithm"
  in
  let* claims = Option.to_result ~none:"malformed claims" (json payload) in
  let audiences =
    match Json.find "aud" claims with
    | Some (Jsont.Array (l, _)) -> List.filter_map (fun a -> str (Some a)) l
    | aud -> Option.to_list (str aud)
  in
  match (str (Json.find "iss" claims), Json.find "exp" claims) with
  | Some iss, _ when iss <> cfg.issuer -> Error "wrong issuer"
  | _ when not (List.mem cfg.resource audiences) -> Error "token not issued for this resource"
  | _, Some (Jsont.Number (exp, _)) when exp > now -> Ok claims
  | _ -> Error "token expired"

(* What handlers learn of the caller, through [Ctx.principal]. It is also
   what MRTR state is bound to, so state minted for one caller is refused
   for another. *)
module Principal = struct
  type t = { sub : string; scopes : string list }

  let to_string p =
    Json.to_string
      (Json.obj [ ("sub", Json.str p.sub); ("scope", Json.str (String.concat " " p.scopes)) ])

  let of_claims claims =
    {
      sub = Option.value (str (Json.find "sub" claims)) ~default:"";
      scopes =
        String.split_on_char ' ' (Option.value (str (Json.find "scope" claims)) ~default:"")
        |> List.filter (( <> ) "");
    }

  let of_ctx ctx =
    Option.bind (Ctx.principal ctx) (fun p -> Result.to_option (Json.decode Jsont.json p))
    |> Option.map of_claims

  let allows p scope = List.mem scope p.scopes
end

(* RFC 6750 challenges, pointing clients at the metadata (RFC 9728) that
   names the authorization server to get a token from. *)
let challenge cfg ?error () =
  let params =
    Option.fold error ~none:[] ~some:(fun (code, description) ->
        [ ("error", code); ("error_description", description) ])
    @ [ ("resource_metadata", cfg.metadata_url) ]
  in
  "Bearer " ^ String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s=%S" k v) params)

let authenticate cfg ?(now = Unix.gettimeofday) header =
  match Option.map (String.split_on_char ' ') (header "authorization") with
  | None -> Error (challenge cfg ())
  | Some [ scheme; token ] when String.lowercase_ascii scheme = "bearer" -> (
      match verify cfg ~now:(now ()) token with
      | Ok claims -> Ok (Some (Principal.to_string (Principal.of_claims claims)))
      | Error reason -> Error (challenge cfg ~error:("invalid_token", reason) ()))
  | Some _ -> Error (challenge cfg ~error:("invalid_request", "expected a bearer token") ())

let metadata cfg ~scopes =
  Json.obj
    [
      ("resource", Json.str cfg.resource);
      ("authorization_servers", Json.list [ Json.str cfg.issuer ]);
      ("scopes_supported", Json.list (List.map Json.str scopes));
      ("bearer_methods_supported", Json.list [ Json.str "header" ]);
    ]
