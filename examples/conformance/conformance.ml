(* The fixtures that the official conformance suite
   (@modelcontextprotocol/conformance) expects of a server, served over
   Streamable HTTP. The suite speaks 2025-11-25, so every scenario goes
   through the initialize handshake. *)

open Mcp

let json s = Result.get_ok (Json.decode Jsont.json s)

(* A 1x1 PNG and an empty WAV. *)
let png =
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg=="

let wav = "UklGRiQAAABXQVZFZm10IBAAAAABAAEAQB8AAEAfAAABAAgAZGF0YQAAAAA="

let embedded ?(mime_type = "text/plain") uri text =
  Proto.Content.Resource { resource = Proto.Contents.text ~mime_type uri text; annotations = None }

let fixed name description content =
  Tool.v name ~description Schema.empty (fun _ () -> Ok (Tool.content content))

(* Ask the client once and report its answer. Handlers are written for MRTR;
   for this revision the server puts the request to the client itself. *)
let asking name description args request ~reply =
  Tool.v name ~description args (fun ctx a ->
      match Ctx.input ctx "answer" with
      | Some answer -> Ok (Tool.text (reply ^ Json.to_string answer))
      | None -> Ok (Input.required [ ("answer", request a) ]))

let elicit message properties =
  Input.raw ~method_:"elicitation/create"
    (Json.obj
       [
         ("message", Json.str message);
         ( "requestedSchema",
           Json.obj [ ("type", Json.str "object"); ("properties", json properties) ] );
       ])

let progress clock =
  Tool.v "test_tool_with_progress" ~description:"Reports progress" Schema.empty (fun ctx () ->
      List.iteri
        (fun i value ->
          if i > 0 then Eio.Time.sleep clock 0.05;
          Ctx.progress ctx ~total:100. value)
        [ 0.; 50.; 100. ];
      Ok (Tool.text "Progress complete"))

let tools clock =
  [
    fixed "test_simple_text" "Returns text"
      [ Proto.Content.text "This is a simple text response for testing." ];
    fixed "test_image_content" "Returns an image" [ Proto.Content.image ~mime_type:"image/png" png ];
    fixed "test_audio_content" "Returns audio" [ Proto.Content.audio ~mime_type:"audio/wav" wav ];
    fixed "test_embedded_resource" "Returns an embedded resource"
      [ embedded "test://embedded-resource" "This is an embedded resource content." ];
    fixed "test_multiple_content_types" "Returns text, an image and a resource"
      [
        Proto.Content.text "Multiple content types test:";
        Proto.Content.image ~mime_type:"image/png" png;
        embedded ~mime_type:"application/json" "test://mixed-content-resource"
          {|{"test":"data","value":123}|};
      ];
    Tool.v "test_error_handling" ~description:"Always fails" Schema.empty (fun _ () ->
        Ok (Tool.error "This tool intentionally returns an error for testing"));
    progress clock;
    asking "test_sampling" "Asks the client's model"
      Schema.(req "prompt" string)
      (fun prompt ->
        Input.raw ~method_:"sampling/createMessage"
          (Json.obj
             [
               ( "messages",
                 Json.list
                   [
                     Json.obj
                       [
                         ("role", Json.str "user");
                         ( "content",
                           Json.obj [ ("type", Json.str "text"); ("text", Json.str prompt) ] );
                       ];
                   ] );
               ("maxTokens", Json.int 100);
             ]))
      ~reply:"LLM response: ";
    asking "test_elicitation" "Asks the user"
      Schema.(req "message" string)
      (fun message ->
        elicit message
          {|{"username":{"type":"string","description":"User's response"},"email":{"type":"string","description":"User's email address"}}|})
      ~reply:"User response: ";
    asking "test_elicitation_sep1034_defaults" "Asks with defaults" Schema.empty
      (fun () ->
        elicit "Please review the defaults"
          {|{"name":{"type":"string","default":"John Doe"},"age":{"type":"integer","default":30},"score":{"type":"number","default":95.5},"status":{"type":"string","enum":["active","inactive","pending"],"default":"active"},"verified":{"type":"boolean","default":true}}|})
      ~reply:"Elicitation completed: ";
    asking "test_elicitation_sep1330_enums" "Asks with every kind of enum" Schema.empty
      (fun () ->
        elicit "Please choose"
          {|{"untitledSingle":{"type":"string","enum":["option1","option2","option3"]},"titledSingle":{"type":"string","oneOf":[{"const":"value1","title":"First Option"},{"const":"value2","title":"Second Option"},{"const":"value3","title":"Third Option"}]},"legacyEnum":{"type":"string","enum":["opt1","opt2","opt3"],"enumNames":["Option One","Option Two","Option Three"]},"untitledMulti":{"type":"array","items":{"type":"string","enum":["option1","option2","option3"]}},"titledMulti":{"type":"array","items":{"anyOf":[{"const":"value1","title":"First Choice"},{"const":"value2","title":"Second Choice"},{"const":"value3","title":"Third Choice"}]}}}|})
      ~reply:"Elicitation completed: ";
  ]

let resources =
  [
    Resource.v ~uri:"test://static-text" ~description:"Static text" ~mime_type:"text/plain"
      "static-text" (fun _ ->
        Ok
          (Resource.text ~mime_type:"text/plain" "test://static-text"
             "This is the content of the static text resource."));
    Resource.v ~uri:"test://static-binary" ~description:"Static PNG" ~mime_type:"image/png"
      "static-binary" (fun _ ->
        Ok (Resource.blob ~mime_type:"image/png" "test://static-binary" png));
    Resource.v ~uri:"test://watched-resource" ~description:"Watched text" "watched-resource"
      (fun _ -> Ok (Resource.text "test://watched-resource" "Watched"));
  ]

let templates =
  [
    Resource.template ~uri_template:"test://template/{id}/data" ~description:"Data by id"
      ~mime_type:"application/json" "template-data" (fun _ uri vars ->
        let id = List.assoc "id" vars in
        Ok
          (Resource.text ~mime_type:"application/json" uri
             (Printf.sprintf {|{"id":%s,"templateTest":true,"data":"Data for ID: %s"}|}
                (Json.quote id) id)));
  ]

let user_text s = Proto.Prompt_message.user (Proto.Content.text s)

let prompts =
  [
    Prompt.v "test_simple_prompt" ~description:"A simple prompt" (fun _ _ ->
        Ok (Prompt.messages [ user_text "This is a simple prompt for testing." ]));
    Prompt.v "test_prompt_with_arguments" ~description:"A prompt with arguments"
      ~args:[ Prompt.arg "arg1" ~required:true; Prompt.arg "arg2" ~required:true ]
      ~complete:(fun _ prefix _ ->
        List.filter (String.starts_with ~prefix) [ "paris"; "park"; "party" ])
      (fun _ args ->
        Ok
          (Prompt.messages
             [
               user_text
                 (Printf.sprintf "Prompt with arguments: arg1='%s', arg2='%s'"
                    (List.assoc "arg1" args) (List.assoc "arg2" args));
             ]));
    Prompt.v "test_prompt_with_embedded_resource" ~description:"A prompt embedding a resource"
      ~args:[ Prompt.arg "resourceUri" ~required:true ]
      (fun _ args ->
        Ok
          (Prompt.messages
             [
               Proto.Prompt_message.user
                 (embedded (List.assoc "resourceUri" args) "Embedded resource content for testing.");
               user_text "Please process the embedded resource above.";
             ]));
    Prompt.v "test_prompt_with_image" ~description:"A prompt with an image" (fun _ _ ->
        Ok
          (Prompt.messages
             [
               Proto.Prompt_message.user (Proto.Content.image ~mime_type:"image/png" png);
               user_text "Please analyze the image above.";
             ]));
  ]

let () =
  let port = ref 3000 in
  Arg.parse
    [ ("--port", Arg.Set_int port, "PORT  Listen on PORT (default 3000)") ]
    ignore "conformance [--port PORT]";
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env and net = Eio.Stdenv.net env in
  Mcp_http.serve ~net ~clock ~port:!port
    (Server.v ~name:"conformance" ~version:"1.0.0" ~tools:(tools clock) ~resources ~templates
       ~prompts ())
