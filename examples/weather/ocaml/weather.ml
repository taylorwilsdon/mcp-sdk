(* Weather server: one tool backed by the Open-Meteo HTTP API. *)

open Mcp

(* An upstream base URL "http://host[:port]", as its authority. *)
let authority var default =
  match String.split_on_char '/' (Option.value (Sys.getenv_opt var) ~default) with
  | [ "http:"; ""; authority ] -> authority
  | _ -> invalid_arg (var ^ ": expected http://host[:port]")

let geocoding_api = authority "OPEN_METEO_GEOCODING_URL" "http://geocoding-api.open-meteo.com"
let forecast_api = authority "OPEN_METEO_FORECAST_URL" "http://api.open-meteo.com"

(* RFC 3986 percent-encoding of everything but unreserved characters. *)
let percent_encode s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~') as c -> Buffer.add_char buf c
      | c -> Printf.bprintf buf "%%%02X" (Char.code c))
    s;
  Buffer.contents buf

(* Connect to the first of [addrs] that accepts. *)
let rec connect ~sw net = function
  | [] -> assert false (* getaddrinfo_stream raises rather than find nothing *)
  | [ addr ] -> Eio.Net.connect ~sw net addr
  | addr :: addrs -> ( try Eio.Net.connect ~sw net addr with Eio.Io _ -> connect ~sw net addrs)

(* Keep-alive connections to [authority], one request in flight on each. A
   connection whose request failed or was cancelled may still have a response
   coming, so it is marked broken and replaced on its next use. Connections
   belong to the switch they were opened under, and so to its domain: each
   domain serving requests gets pools of its own. *)
let upstream net authority =
  Local.v @@ fun sw ->
  let host, service =
    match String.split_on_char ':' authority with
    | [ host; port ] -> (host, port)
    | _ -> (authority, "80")
  in
  let open_connection () =
    let socket = connect ~sw net (Eio.Net.getaddrinfo_stream ~service net host) in
    (Httpun_eio.Client.create_connection ~sw socket, socket, ref false)
  in
  let close (conn, socket, _) =
    Eio.Cancel.protect @@ fun () ->
    let stopped = Httpun_eio.Client.shutdown conn in
    (try Eio.Flow.shutdown socket `All with Eio.Io _ -> ());
    Eio.Promise.await stopped;
    Eio.Resource.close socket
  in
  ( authority,
    Eio.Pool.create 64 open_connection ~dispose:close ~validate:(fun (conn, _, broken) ->
        not (!broken || Httpun_eio.Client.is_closed conn)) )

(* GET [target] from [upstream] and decode the JSON body with [codec].
   Failures are returned as a message for the model. *)
let get (authority, pool) target codec =
  match
    Eio.Pool.use pool @@ fun (conn, _, broken) ->
    let body, resolve = Eio.Promise.create () in
    let finish result = ignore (Eio.Promise.try_resolve resolve result) in
    let response_handler (resp : Httpun.Response.t) reader =
      let buf = Buffer.create 1024 in
      let rec read () =
        Httpun.Body.Reader.schedule_read reader
          ~on_eof:(fun () ->
            finish
              (if resp.status = `OK then Ok (Buffer.contents buf)
               else Error ("HTTP " ^ Httpun.Status.to_string resp.status)))
          ~on_read:(fun bs ~off ~len ->
            Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
            read ())
      in
      read ()
    in
    let error_handler _ =
      broken := true;
      finish (Error "invalid response")
    in
    let headers = Httpun.Headers.of_list [ ("host", authority) ] in
    Httpun_eio.Client.request conn
      (Httpun.Request.create ~headers `GET target)
      ~error_handler ~response_handler
    |> Httpun.Body.Writer.close;
    try Eio.Promise.await body
    with e ->
      broken := true;
      raise e
  with
  | Ok body -> Json.decode codec body
  | Error msg -> Error ("Open-Meteo " ^ authority ^ ": " ^ msg)
  | exception Eio.Io _ -> Error ("Open-Meteo " ^ authority ^ " is unreachable")

(* Upstream responses are decoded with the same Schema combinators as tool
   arguments, straight into records; members not listed are skipped. *)
type place = { name : string; country : string; latitude : float; longitude : float }

let places =
  Schema.(
    codec
      (let+ results =
         dft "results"
           (list
              (obj
                 (let+ name = req "name" string
                  and+ country = dft "country" string ""
                  and+ latitude = req "latitude" float
                  and+ longitude = req "longitude" float in
                  { name; country; latitude; longitude })))
           []
       in
       results))

type current = { temperature : float; apparent : float; humidity : float; wind : float; code : int }

let current =
  Schema.(
    codec
      (let+ current =
         req "current"
           (obj
              (let+ temperature = req "temperature_2m" float
               and+ humidity = req "relative_humidity_2m" float
               and+ apparent = req "apparent_temperature" float
               and+ code = req "weather_code" int
               and+ wind = req "wind_speed_10m" float in
               { temperature; apparent; humidity; wind; code }))
       in
       current))

(* WMO weather interpretation codes. *)
let conditions = function
  | 0 -> "Clear sky"
  | 1 -> "Mainly clear"
  | 2 -> "Partly cloudy"
  | 3 -> "Overcast"
  | 45 | 48 -> "Fog"
  | 51 | 53 | 55 -> "Drizzle"
  | 56 | 57 -> "Freezing drizzle"
  | 61 | 63 | 65 -> "Rain"
  | 66 | 67 -> "Freezing rain"
  | 71 | 73 | 75 -> "Snow"
  | 77 -> "Snow grains"
  | 80 | 81 | 82 -> "Rain showers"
  | 85 | 86 -> "Snow showers"
  | 95 -> "Thunderstorm"
  | 96 | 99 -> "Thunderstorm with hail"
  | _ -> "Unknown"

(* The tool's structured result. Its codec and advertised schema are one
   value, the counterpart of the Pydantic model in ../python. *)
type weather = {
  city : string;
  country : string;
  latitude : float;
  longitude : float;
  temperature : float;
  apparent_temperature : float;
  humidity : float;
  wind_speed_kmh : float;
  conditions : string;
  units : string;
}

let weather =
  let fields =
    [
      ("city", "string");
      ("country", "string");
      ("latitude", "number");
      ("longitude", "number");
      ("temperature", "number");
      ("apparent_temperature", "number");
      ("humidity", "number");
      ("wind_speed_kmh", "number");
      ("conditions", "string");
    ]
  in
  let units =
    Json.obj
      [
        ("type", Json.str "string");
        ("enum", Json.list [ Json.str "celsius"; Json.str "fahrenheit" ]);
      ]
  in
  let schema =
    Json.obj
      [
        ("type", Json.str "object");
        ( "properties",
          Json.obj
            (List.map (fun (name, ty) -> (name, Json.obj [ ("type", Json.str ty) ])) fields
            @ [ ("units", units) ]) );
        ("required", Json.list (List.map Json.str (List.map fst fields @ [ "units" ])));
      ]
  in
  let codec =
    Jsont.Object.(
      map
        (fun
          city
          country
          latitude
          longitude
          temperature
          apparent_temperature
          humidity
          wind_speed_kmh
          conditions
          units
        ->
          {
            city;
            country;
            latitude;
            longitude;
            temperature;
            apparent_temperature;
            humidity;
            wind_speed_kmh;
            conditions;
            units;
          })
      |> mem "city" Jsont.string ~enc:(fun w -> w.city)
      |> mem "country" Jsont.string ~enc:(fun w -> w.country)
      |> mem "latitude" Jsont.number ~enc:(fun w -> w.latitude)
      |> mem "longitude" Jsont.number ~enc:(fun w -> w.longitude)
      |> mem "temperature" Jsont.number ~enc:(fun w -> w.temperature)
      |> mem "apparent_temperature" Jsont.number ~enc:(fun w -> w.apparent_temperature)
      |> mem "humidity" Jsont.number ~enc:(fun w -> w.humidity)
      |> mem "wind_speed_kmh" Jsont.number ~enc:(fun w -> w.wind_speed_kmh)
      |> mem "conditions" Jsont.string ~enc:(fun w -> w.conditions)
      |> mem "units" Jsont.string ~enc:(fun w -> w.units)
      |> finish)
  in
  Tool.output ~schema codec

let report (place : place) units symbol (c : current) =
  let w =
    {
      city = place.name;
      country = place.country;
      latitude = place.latitude;
      longitude = place.longitude;
      temperature = c.temperature;
      apparent_temperature = c.apparent;
      humidity = c.humidity;
      wind_speed_kmh = c.wind;
      conditions = conditions c.code;
      units;
    }
  in
  Tool.return weather
    ~content:
      [
        Proto.Content.text
          (Printf.sprintf "%s, %s: %g%s (feels like %g%s), %s, humidity %g%%, wind %g km/h" w.city
             w.country w.temperature symbol w.apparent_temperature symbol w.conditions w.humidity
             w.wind_speed_kmh);
      ]
    w

let get_weather net =
  let geocoding = upstream net geocoding_api and forecast = upstream net forecast_api in
  Tool.v "get_weather" ~description:"Current weather for a city, from Open-Meteo" ~output:weather
    ~annotations:(Proto.Tool_annotations.v ~read_only:true ())
    Schema.(
      let+ city = req "city" string ~doc:"City name, e.g. Berlin"
      and+ units =
        dft "units" (enum [ ("celsius", `Celsius); ("fahrenheit", `Fahrenheit) ]) `Celsius
      in
      (city, units))
    (fun ctx (city, units) ->
      let geocoding = Ctx.local ctx geocoding and forecast = Ctx.local ctx forecast in
      let units, symbol =
        match units with `Celsius -> ("celsius", "°C") | `Fahrenheit -> ("fahrenheit", "°F")
      in
      let geocode =
        get geocoding
          ("/v1/search?name=" ^ percent_encode city ^ "&count=1&language=en&format=json")
          places
      in
      match geocode with
      | Error msg -> Ok (Tool.error msg)
      | Ok [] -> Ok (Tool.error ("No location found for '" ^ city ^ "'"))
      | Ok (place :: _) -> (
          let target =
            Printf.sprintf
              "/v1/forecast?latitude=%.15g&longitude=%.15g&current=temperature_2m,relative_humidity_2m,apparent_temperature,weather_code,wind_speed_10m&temperature_unit=%s&wind_speed_unit=kmh"
              place.latitude place.longitude units
          in
          match get forecast target current with
          | Error msg -> Ok (Tool.error msg)
          | Ok c -> Ok (report place units symbol c)))

let server net =
  Server.v ~name:"weather" ~version:"1.0.0"
    ~instructions:"Use get_weather to look up the current weather for a city."
    ~tools:[ get_weather net ]
    ()

let () =
  let http_port = ref None and domains = ref 0 in
  Arg.parse
    [
      ("--http", Arg.Int (fun p -> http_port := Some p), "PORT  Serve Streamable HTTP/1.1 on PORT");
      ("--domains", Arg.Set_int domains, "N  Extra domains accepting HTTP connections");
    ]
    (fun _ -> ())
    "weather [--http PORT] [--domains N]  (stdio when no port is given)";
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env and net = Eio.Stdenv.net env in
  match !http_port with
  | None -> Stdio.run (server net) env
  | Some port ->
      let domains = if !domains > 0 then Some (Eio.Stdenv.domain_mgr env, !domains) else None in
      Mcp_http.serve ?domains ~net ~clock ~port (server net)
