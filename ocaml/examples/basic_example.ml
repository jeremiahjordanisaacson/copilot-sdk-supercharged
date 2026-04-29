(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Basic example: create a client, start a session, send a message. *)

open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     (* Create client with default options *)
     let client = Client.create () in

     (* Start the client (connects to CLI) *)
     let* () = Client.start client in

     (* Check server status *)
     let* status = Client.get_status client in
     Printf.printf "Connected! Protocol version: %d\n" status.protocol_version;

     (* Create a session *)
     let config =
       { (Types.default_session_config ()) with
         model = Some "gpt-4o"
       }
     in
     let* session = Client.create_session ~config client in
     Printf.printf "Session created: %s\n" (Session.session_id session);

     (* Register an event handler *)
     Session.on session (fun event ->
       (match event.event_type with
        | Types.AssistantMessage ->
          let open Yojson.Safe.Util in
          let content =
            try event.data |> member "content" |> to_string
            with _ -> "(no content)"
          in
          Printf.printf "Assistant: %s\n" content
        | Types.AssistantMessageDelta ->
          let open Yojson.Safe.Util in
          let delta =
            try event.data |> member "content" |> to_string
            with _ -> ""
          in
          print_string delta
        | Types.SessionIdle ->
          Printf.printf "\n[Session idle]\n"
        | Types.SessionError ->
          let open Yojson.Safe.Util in
          let msg =
            try event.data |> member "message" |> to_string
            with _ -> "unknown error"
          in
          Printf.eprintf "Error: %s\n" msg
        | _ -> ());
       Lwt.return_unit);

     (* Register a permission handler that approves everything *)
     Session.register_permission_handler session Tools.approve_all;

     (* Send a message and wait for the response *)
     let msg = Types.make_message "What is OCaml? Explain in two sentences." in
     let* response = Session.send_and_wait session msg in
     (match response with
      | Some ev ->
        let open Yojson.Safe.Util in
        let content =
          try ev.data |> member "content" |> to_string
          with _ -> "(no content in final event)"
        in
        Printf.printf "\nFinal response: %s\n" content
      | None -> Printf.printf "Timed out waiting for response\n");

     (* Clean up *)
     let* () = Session.destroy session in
     let* () = Client.stop client in
     Printf.printf "Done!\n";
     Lwt.return_unit)
