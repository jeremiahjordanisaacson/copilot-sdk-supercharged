(* Cookbook: Model Selection
   Demonstrates listing available models and choosing one for a session. *)

open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in

     (* List available models *)
     let* models = Client.list_models client in
     Printf.printf "Available models (%d):\n" (List.length models);
     List.iter
       (fun (m : Types.model_info) ->
         let name =
           match m.model_name with
           | Some n -> n
           | None -> "(unnamed)"
         in
         Printf.printf "  - %s (%s)\n" m.model_id name)
       models;

     (* Pick a model -- prefer gpt-4o, fall back to first available *)
     let model_id =
       match
         List.find_opt
           (fun (m : Types.model_info) ->
             String.lowercase_ascii m.model_id = "gpt-4o")
           models
       with
       | Some m -> m.model_id
       | None ->
         (match models with
          | m :: _ -> m.model_id
          | [] -> failwith "No models available")
     in
     Printf.printf "\nUsing model: %s\n" model_id;

     (* Create session with chosen model and high reasoning *)
     let config =
       { (Types.default_session_config ()) with
         model = Some model_id
       ; reasoning_effort = Some High
       }
     in
     let* session = Client.create_session ~config client in
     Session.register_permission_handler session Tools.approve_all;

     Session.on session (fun event ->
       (match event.event_type with
        | Types.AssistantMessage ->
          let open Yojson.Safe.Util in
          let content =
            try event.data |> member "content" |> to_string
            with _ -> ""
          in
          Printf.printf "Response: %s\n" content
        | _ -> ());
       Lwt.return_unit);

     let msg = Types.make_message "What model are you running on?" in
     let* _response = Session.send_and_wait session msg in

     let* () = Session.destroy session in
     let* () = Client.stop client in
     Lwt.return_unit)
