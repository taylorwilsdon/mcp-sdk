type request = Proto.Input_request.t
type required = Proto.Input_required.t
type 'a step = Complete of 'a | Required of required

let elicit ~message schema : request =
  {
    method_ = "elicitation/create";
    params =
      Json.obj
        [
          ("mode", Json.str "form");
          ("message", Json.str message);
          ("requestedSchema", Schema.json_schema schema);
        ];
  }

let elicit_url ~message ~url : request =
  {
    method_ = "elicitation/create";
    params =
      Json.obj [ ("mode", Json.str "url"); ("message", Json.str message); ("url", Json.str url) ];
  }

let raw ~method_ params : request = { method_; params }

let required ?state requests =
  if requests = [] && state = None then
    invalid_arg "Input.required: needs input requests or a request state";
  Required { requests; state }

(* The client capability each input request depends on, as the JSON object
   to report in a MissingRequiredClientCapability error. *)
let missing_capability (caps : Proto.Client_caps.t) (r : request) =
  let mode =
    Option.bind (Json.find "mode" r.params) (function Jsont.String (s, _) -> Some s | _ -> None)
  in
  let cap name sub = Some (name, Json.obj [ (sub, Json.empty) ]) in
  match (r.method_, mode) with
  | "elicitation/create", Some "url" when not caps.elicit_url -> cap "elicitation" "url"
  | "elicitation/create", (Some "form" | None) when not caps.elicit_form -> cap "elicitation" "form"
  | "sampling/createMessage", _ when not caps.sampling -> Some ("sampling", Json.empty)
  | "roots/list", _ when not caps.roots -> Some ("roots", Json.empty)
  | _ -> None
