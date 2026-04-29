(* Cookbook: Custom Tool Registration
   Demonstrates defining and registering tools that the Copilot agent
   can invoke during a session. *)

open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in

     (* Define a weather tool using the helpers *)
     let params =
       Tools.make_object_schema
         ~properties:
           [ Tools.make_string_param ~name:"location" ~description:"City name" ()
           ; Tools.make_string_param ~name:"units" ~description:"Temperature units (celsius/fahrenheit)" ()
           ]
         ~required:["location"]
         ()
     in
     let weather_def, weather_handler =
       Tools.define_tool
         ~name:"get_weather"
         ~description:"Get current weather for a location"
         ~parameters:params
         (fun invocation ->
           let open Yojson.Safe.Util in
           let location =
             try invocation.arguments |> member "location" |> to_string
             with _ -> "Unknown"
           in
           let result =
             Printf.sprintf
               "{\"location\": \"%s\", \"temperature\": 22, \"condition\": \"sunny\"}"
               location
           in
           Lwt.return (Types.make_tool_result result))
     in

     (* Define a calculator tool *)
     let calc_def, calc_handler =
       Tools.define_tool
         ~name:"calculate"
         ~description:"Evaluate a math expression"
         ~parameters:
           (Tools.make_object_schema
              ~properties:
                [ Tools.make_string_param
                    ~name:"expression"
                    ~description:"Math expression to evaluate"
                    ()
                ]
              ~required:["expression"]
              ())
         (fun invocation ->
           let open Yojson.Safe.Util in
           let expr =
             try invocation.arguments |> member "expression" |> to_string
             with _ -> "0"
           in
           (* Simple evaluation placeholder *)
           let result = Printf.sprintf "Result of '%s' = 42" expr in
           Lwt.return (Types.make_tool_result result))
     in

     (* Create session with both tools *)
     let config =
       { (Types.default_session_config ()) with
         tools = [ weather_def; calc_def ]
       }
     in
     let* session = Client.create_session ~config client in

     (* Register handlers *)
     Tools.register_tools session [ (weather_def, weather_handler); (calc_def, calc_handler) ];
     Session.register_permission_handler session Tools.approve_all;

     (* Use the tools *)
     let msg = Types.make_message "What is the weather in Paris? Also calculate 2+2." in
     let* _response = Session.send_and_wait session msg in

     let* () = Session.destroy session in
     let* () = Client.stop client in
     Lwt.return_unit)
