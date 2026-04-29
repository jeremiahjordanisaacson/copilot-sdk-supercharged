(* Cookbook: Permission Handling
   Demonstrates custom permission handling with selective approval. *)

open Copilot_sdk

(** A permission handler that approves read operations but
    denies write operations. *)
let selective_handler : Session.permission_handler =
  fun request _session_id ->
    let open Yojson.Safe.Util in
    let kind =
      try request |> member "kind" |> to_string with _ -> "unknown"
    in
    Printf.printf "Permission request: %s\n" kind;
    if kind = "read" || kind = "fileRead" then
      Lwt.return Types.{ decision = Approved; rules = None }
    else (
      Printf.printf "Denied permission: %s\n" kind;
      Lwt.return Types.{ decision = DeniedByUser; rules = None })

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in
     let* session = Client.create_session client in

     (* Use the selective handler *)
     Session.register_permission_handler session selective_handler;

     Session.on session (fun event ->
       (match event.event_type with
        | Types.AssistantMessage ->
          let open Yojson.Safe.Util in
          let content =
            try event.data |> member "content" |> to_string
            with _ -> ""
          in
          Printf.printf "Assistant: %s\n" content
        | Types.PermissionRequested ->
          Printf.printf "[Permission event received]\n"
        | _ -> ());
       Lwt.return_unit);

     let msg = Types.make_message "Read the file README.md and summarize it." in
     let* _response = Session.send_and_wait session msg in

     let* () = Session.destroy session in
     let* () = Client.stop client in
     Lwt.return_unit)
