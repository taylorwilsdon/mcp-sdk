(* Integrity-protected MRTR [requestState]:

     base64url(expiry_ms_be64 ^ payload) "." base64url(hmac_sha256)

   The MAC covers the payload, its expiry and a binding of the originating
   method, target, original arguments and authenticated principal,
   so state cannot be replayed late, on another request, or by another user. *)

module H = Digestif.SHA256

let b64 = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
let unb64 s = Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s
let now_ms () = Int64.of_float (Unix.gettimeofday () *. 1000.)

(* Only evaluated for requests that issue or consume state. Sorting object
   members makes insignificant ordering differences between retries harmless. *)
let rec canonical = function
  | Jsont.Object (members, _) ->
      members
      |> List.map (fun ((key, _), value) -> (key, canonical value))
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      |> Json.obj
  | Jsont.Array (items, _) -> Json.list (List.map canonical items)
  | value -> value

let arguments_codec =
  let open Jsont.Object in
  let params = map Fun.id |> mem "arguments" Jsont.json ~dec_absent:Json.empty |> finish in
  map Fun.id |> mem "params" params |> finish

let arguments_digest raw =
  match Json.decode arguments_codec raw with
  | Ok arguments -> H.to_hex (H.digest_string (Json.to_string (canonical arguments)))
  | Error message -> invalid_arg message

let binding ~method_ ~target ~principal ~arguments =
  String.concat ""
    (List.map
       (fun s -> string_of_int (String.length s) ^ ":" ^ s)
       [ method_; target; Option.value principal ~default:""; arguments ])

let mac ~key ~binding body = H.hmac_string ~key (binding ^ body)

let seal ~key ~binding ~ttl_ms payload =
  let body = Bytes.create (8 + String.length payload) in
  Bytes.set_int64_be body 0 (Int64.add (now_ms ()) (Int64.of_int ttl_ms));
  Bytes.blit_string payload 0 body 8 (String.length payload);
  let body = Bytes.unsafe_to_string body in
  b64 body ^ "." ^ b64 (H.to_raw_string (mac ~key ~binding body))

let open_ ~key ~binding token =
  let invalid = Error "Invalid requestState" in
  match String.rindex_opt token '.' with
  | None -> invalid
  | Some i -> (
      let part off len = unb64 (String.sub token off len) in
      match (part 0 i, part (i + 1) (String.length token - i - 1)) with
      | Ok body, Ok tag when String.length body >= 8 -> (
          match H.of_raw_string_opt tag with
          | Some tag when H.equal tag (mac ~key ~binding body) ->
              if Int64.compare (String.get_int64_be body 0) (now_ms ()) < 0 then
                Error "Expired requestState"
              else Ok (String.sub body 8 (String.length body - 8))
          | _ -> invalid)
      | _ -> invalid)

let random_key () = In_channel.with_open_bin "/dev/urandom" (fun ic -> really_input_string ic 32)
