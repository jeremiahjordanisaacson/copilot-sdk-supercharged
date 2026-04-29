(* Cookbook: Error Handling
   Demonstrates robust error handling with the Result type and Lwt. *)

open Copilot_sdk

(** Wrap client operations with proper error recovery. *)
let safe_send session prompt =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let msg = Types.make_message prompt in
      let* response = Session.send_and_wait ~timeout:30.0 session msg in
      match response with
      | Some ev -> Lwt.return_ok ev
      | None -> Lwt.return_error "Request timed out")
    (fun exn ->
      match exn with
      | Jsonrpc.Jsonrpc_error err ->
        Lwt.return_error
          (Printf.sprintf "JSON-RPC error %d: %s" err.code err.message)
      | Jsonrpc.Process_exited msg ->
        Lwt.return_error (Printf.sprintf "CLI process exited: %s" msg)
      | exn ->
        Lwt.return_error (Printexc.to_string exn))

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     (* Attempt connection with error handling *)
     let result =
       Lwt.catch
         (fun () ->
           let opts =
             { (Types.default_client_options ()) with
               cli_path = Some "/nonexistent/path"
             }
           in
           let client = Client.create ~options:opts () in
           let* () = Client.start client in
           Lwt.return_ok client)
         (fun exn ->
           Printf.printf "Expected error: %s\n" (Printexc.to_string exn);
           Lwt.return_error (Printexc.to_string exn))
     in
     let* client_result = result in
     match client_result with
     | Error msg ->
       Printf.printf "Could not connect: %s\n" msg;
       Printf.printf "Falling back to default CLI path...\n";

       (* Retry with defaults *)
       let client = Client.create () in
       let connect_result =
         Lwt.catch
           (fun () ->
             let* () = Client.start client in
             Lwt.return_ok ())
           (fun exn -> Lwt.return_error (Printexc.to_string exn))
       in
       let* _ = connect_result in
       Lwt.return_unit
     | Ok client ->
       let* session = Client.create_session client in
       Session.register_permission_handler session Tools.approve_all;

       (* Send with error handling *)
       let* result = safe_send session "Hello!" in
       (match result with
        | Ok ev ->
          Printf.printf "Success: got event type %s\n"
            (match ev.event_type with
             | Types.AssistantMessage -> "assistant.message"
             | Types.SessionIdle -> "session.idle"
             | _ -> "other")
        | Error msg ->
          Printf.printf "Failed: %s\n" msg);

       let* () = Session.destroy session in
       let* () = Client.stop client in
       Lwt.return_unit)
