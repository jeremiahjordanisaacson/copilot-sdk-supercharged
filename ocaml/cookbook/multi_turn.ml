(* Cookbook: Multi-turn Conversation
   Demonstrates sending multiple messages in sequence with a shared session. *)

open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in

     let config =
       { (Types.default_session_config ()) with
         model = Some "gpt-4o"
       ; system_prompt =
           Some "You are a helpful coding assistant specializing in OCaml."
       }
     in
     let* session = Client.create_session ~config client in
     Session.register_permission_handler session Tools.approve_all;

     (* Helper to send and print response *)
     let ask prompt =
       Printf.printf "\nUser: %s\n" prompt;
       let msg = Types.make_message prompt in
       let* response = Session.send_and_wait session msg in
       (match response with
        | Some ev ->
          let open Yojson.Safe.Util in
          let content =
            try ev.data |> member "content" |> to_string
            with _ -> "(no content)"
          in
          Printf.printf "Assistant: %s\n" content
        | None ->
          Printf.printf "(timed out)\n");
       Lwt.return_unit
     in

     (* Multi-turn conversation *)
     let* () = ask "What is a functor in OCaml?" in
     let* () = ask "Can you give me a simple example?" in
     let* () = ask "How does that differ from a Haskell functor?" in

     let* () = Session.destroy session in
     let* () = Client.stop client in
     Lwt.return_unit)
