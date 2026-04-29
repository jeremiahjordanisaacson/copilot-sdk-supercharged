(* Cookbook: Streaming Responses
   Demonstrates handling streaming delta events from the assistant. *)

open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in

     let config =
       { (Types.default_session_config ()) with streaming = true }
     in
     let* session = Client.create_session ~config client in
     Session.register_permission_handler session Tools.approve_all;

     (* Collect streaming deltas into a buffer *)
     let buf = Buffer.create 256 in
     Session.on session (fun event ->
       (match event.event_type with
        | Types.AssistantMessageDelta ->
          let open Yojson.Safe.Util in
          let delta =
            try event.data |> member "content" |> to_string
            with _ -> ""
          in
          Buffer.add_string buf delta;
          print_string delta;
          flush stdout
        | Types.AssistantMessage ->
          Printf.printf "\n--- Stream complete (%d chars) ---\n"
            (Buffer.length buf)
        | Types.AssistantReasoningDelta ->
          let open Yojson.Safe.Util in
          let delta =
            try event.data |> member "content" |> to_string
            with _ -> ""
          in
          Printf.printf "[reasoning] %s" delta
        | _ -> ());
       Lwt.return_unit);

     let msg = Types.make_message "Write a haiku about OCaml." in
     let* _response = Session.send_and_wait session msg in

     let* () = Session.destroy session in
     let* () = Client.stop client in
     Lwt.return_unit)
