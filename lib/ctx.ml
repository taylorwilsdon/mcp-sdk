type t = {
  id : Id.t;
  meta : Proto.Meta.t;
  inputs : (string * Json.t) list;
  state : string option;
  principal : string option;
  emit : Wire.t -> unit;
  scope : Local.scope;
  mutable last_progress : float;
}

let make ~id ~meta ~inputs ~state ~principal ~emit ~scope =
  { id; meta; inputs; state; principal; emit; scope; last_progress = neg_infinity }

let id t = t.id
let emit t = t.emit
let protocol_version t = t.meta.protocol_version
let client_info t = t.meta.client_info
let client_caps t = t.meta.client_caps
let principal t = t.principal
let state t = t.state
let meta t key = Json.find key t.meta.extra
let input t key = List.assoc_opt key t.inputs
let local t l = Local.get t.scope l

let progress t ?total ?message value =
  match t.meta.progress_token with
  | Some token when value > t.last_progress ->
      t.last_progress <- value;
      let opt name f = Option.fold ~none:[] ~some:(fun v -> [ (name, f v) ]) in
      let params =
        Json.obj
          ((("progressToken", Id.to_json token) :: [ ("progress", Json.num value) ])
          @ opt "total" Json.num total @ opt "message" Json.str message)
      in
      t.emit (Wire.notification "notifications/progress" (Json.to_string params))
  | _ -> ()

type 'a elicited = Accepted of 'a | Declined | Cancelled

(* Malformed responses read as absent, so the handler simply asks again, as
   the MRTR error-handling rules recommend. *)
let elicited t key schema =
  Option.bind (input t key) (fun json ->
      match Json.decode_json Proto.Elicit_result.codec json with
      | Error _ -> None
      | Ok { action = Decline; _ } -> Some Declined
      | Ok { action = Cancel; _ } -> Some Cancelled
      | Ok { action = Accept; content } ->
          let content = Option.value content ~default:Json.empty in
          Result.to_option (Json.decode_json (Schema.codec schema) content)
          |> Option.map (fun v -> Accepted v))
