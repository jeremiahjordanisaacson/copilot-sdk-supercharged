(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Copilot CLI SDK Client -- main entry point for the Copilot SDK. *)

(* ========================================================================== *)
(* Internal state                                                             *)
(* ========================================================================== *)

type t = {
  options : Types.client_options;
  (* "coding-agent" or "empty" (validated by [create]). *)
  mode : string;
  mutable rpc : Jsonrpc.t option;
  mutable process : Lwt_process.process_full option;
  mutable state : Types.connection_state;
  mutable negotiated_protocol_version : int option;
  (* Sessions that receive server-to-client requests, keyed by session id. *)
  sessions : (string, Session.t) Hashtbl.t;
  (* GitHub token providers keyed by registration id, with their session id. *)
  token_providers : (string, Types.github_token_provider * string option ref) Hashtbl.t;
}

(* ========================================================================== *)
(* JSON helpers                                                               *)
(* ========================================================================== *)

let json_member (json : Yojson.Safe.t) (key : string) : Yojson.Safe.t option =
  match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let json_string (json : Yojson.Safe.t) (key : string) : string option =
  match json_member json key with Some (`String s) -> Some s | _ -> None

let json_int (json : Yojson.Safe.t) (key : string) : int option =
  match json_member json key with Some (`Int n) -> Some n | _ -> None

(* ========================================================================== *)
(* CLI resolution                                                             *)
(* ========================================================================== *)

let find_cli_path (opts : Types.client_options) : string =
  match opts.cli_path with
  | Some p -> p
  | None ->
    (* Check COPILOT_CLI_PATH env var *)
    (match Sys.getenv_opt "COPILOT_CLI_PATH" with
     | Some p -> p
     | None ->
       (* Try to locate via which/where *)
       let cmd =
         if Sys.win32 then "where copilot-cli 2>NUL"
         else "which copilot-cli 2>/dev/null"
       in
       let ic = Unix.open_process_in cmd in
       let result =
         try String.trim (input_line ic) with End_of_file -> ""
       in
       ignore (Unix.close_process_in ic);
       if result <> "" then result
       else failwith "Cannot find copilot-cli. Set cli_path or COPILOT_CLI_PATH.")

(* ========================================================================== *)
(* Construction                                                               *)
(* ========================================================================== *)

let create ?(options = Types.default_client_options ()) () : t =
  { options
  ; mode = Types.normalize_client_mode options.client_mode
  ; rpc = None
  ; process = None
  ; state = Disconnected
  ; negotiated_protocol_version = None
  ; sessions = Hashtbl.create 8
  ; token_providers = Hashtbl.create 4
  }

(* ========================================================================== *)
(* Spawn CLI subprocess                                                       *)
(* ========================================================================== *)

let spawn_cli (t : t) : (Jsonrpc.t * Lwt_process.process_full) Lwt.t =
  let open Lwt.Syntax in
  let cli_path = find_cli_path t.options in
  (* JavaScript entry points (for example npm-loader.js) run with node;
     anything else runs directly. *)
  let program, args =
    if Filename.check_suffix cli_path ".js" then ("node", [| "node"; cli_path; "--stdio" |])
    else (cli_path, [| cli_path; "--stdio" |])
  in
  let args =
    match t.options.log_level with
    | Some level -> Array.append args [| "--log-level"; level |]
    | None -> args
  in
  let args =
    match t.options.remote with
    | Some true -> Array.append args [| "--remote" |]
    | _ -> args
  in
  let cmd = (program, args) in
  let process =
    new Lwt_process.process_full cmd
  in
  let ic = process#stdout in
  let oc = process#stdin in
  let rpc = Jsonrpc.create ic oc in
  let* () = Jsonrpc.start rpc in
  Lwt.return (rpc, process)

(* ========================================================================== *)
(* TCP connection                                                             *)
(* ========================================================================== *)

let connect_tcp (url : string) : Jsonrpc.t Lwt.t =
  let open Lwt.Syntax in
  (* Parse host:port *)
  let host, port =
    match String.split_on_char ':' url with
    | [ h; p ] -> (h, int_of_string p)
    | _ -> (url, 3000)
  in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect fd addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let rpc = Jsonrpc.create ic oc in
  let* () = Jsonrpc.start rpc in
  Lwt.return rpc

(* ========================================================================== *)
(* Server-to-client requests                                                  *)
(* ========================================================================== *)

let register_server_handlers (t : t) (rpc : Jsonrpc.t) : unit =
  Jsonrpc.register_handler rpc "gitHubToken.getToken" (fun _method params ->
    let open Lwt.Syntax in
    let registration_id = Option.value (json_string params "registrationId") ~default:"" in
    match Hashtbl.find_opt t.token_providers registration_id with
    | None ->
      Lwt.fail_with
        (Printf.sprintf "No GitHub token provider registered for registration ID %S"
           registration_id)
    | Some (provider, session_ref) ->
      let request : Types.github_token_request =
        { Types.gtr_registration_id = registration_id
        ; gtr_host = Option.value (json_string params "host") ~default:""
        ; gtr_session_id =
            (match json_string params "sessionId" with
             | Some sid -> Some sid
             | None -> !session_ref)
        ; gtr_reason = Option.value (json_string params "reason") ~default:"initial"
        }
      in
      let* result = provider request in
      Lwt.return (Types.github_token_result_to_yojson result));
  Jsonrpc.register_handler rpc "hooks.invoke" (fun _method params ->
    let open Lwt.Syntax in
    let session_id = Option.value (json_string params "sessionId") ~default:"" in
    match Hashtbl.find_opt t.sessions session_id with
    | None -> Lwt.fail_with (Printf.sprintf "Session not found: %s" session_id)
    | Some session ->
      let hook_type = Option.value (json_string params "hookType") ~default:"" in
      let input = Option.value (json_member params "input") ~default:`Null in
      let* output = Session.handle_hooks_invoke session hook_type input in
      Lwt.return
        (match output with
         | Some value -> `Assoc [ ("output", value) ]
         | None -> `Assoc []));
  Jsonrpc.register_handler rpc "shellAuthentication.request" (fun _method params ->
    let open Lwt.Syntax in
    let cancel = Types.shell_authentication_response_to_yojson Types.Shell_auth_cancel in
    let valid_kinds =
      [ Types.shell_input_kind_secret; Types.shell_input_kind_text; Types.shell_input_kind_confirm ]
    in
    match
      ( json_string params "sessionId"
      , json_string params "requestId"
      , json_string params "program"
      , json_string params "prompt"
      , json_string params "inputKind" )
    with
    | Some session_id, Some request_id, Some program, Some prompt, Some input_kind
      when List.mem input_kind valid_kinds ->
      (match Hashtbl.find_opt t.sessions session_id with
       | None -> Lwt.return cancel
       | Some session ->
         let request : Types.shell_authentication_request =
           { Types.sar_session_id = session_id
           ; sar_request_id = request_id
           ; sar_program = program
           ; sar_prompt = prompt
           ; sar_input_kind = input_kind
           ; sar_command = Option.value (json_string params "command") ~default:""
           ; sar_cwd = Option.value (json_string params "cwd") ~default:""
           }
         in
         let* response = Session.handle_shell_authentication session request in
         Lwt.return (Types.shell_authentication_response_to_yojson response))
    | _ -> Lwt.return cancel);
  Jsonrpc.register_handler rpc "shellAuthentication.cancel" (fun _method params ->
    (match json_string params "sessionId", json_string params "requestId" with
     | Some session_id, Some request_id ->
       (match Hashtbl.find_opt t.sessions session_id with
        | Some session -> Session.cancel_shell_authentication session request_id
        | None -> ())
     | _ -> ());
    Lwt.return (`Assoc []));
  Jsonrpc.register_handler rpc "memoryStore.store" (fun _method params ->
    let open Lwt.Syntax in
    let session_id = Option.value (json_string params "sessionId") ~default:"" in
    match Hashtbl.find_opt t.sessions session_id with
    | None -> Lwt.fail_with "Memory store owner is unavailable"
    | Some session ->
      let field key = Option.value (json_string params key) ~default:"" in
      let memory : Types.stored_memory =
        { Types.sm_session_id = session_id
        ; sm_subject = field "subject"
        ; sm_fact = field "fact"
        ; sm_citations = field "citations"
        ; sm_reason = field "reason"
        }
      in
      let* () = Session.handle_memory_store session memory in
      Lwt.return `Null);
  match t.options.network with
  | None -> ()
  | Some network ->
    (match network.Types.resolve_proxy with
     | Some resolve ->
       Jsonrpc.register_handler rpc "network.resolveProxy" (fun _method params ->
         let open Lwt.Syntax in
         let* proxy = resolve (Option.value (json_string params "url") ~default:"") in
         Lwt.return
           (match proxy with
            | Some value when value <> "" -> `Assoc [ ("proxy", `String value) ]
            | _ -> `Assoc []))
     | None -> ());
    (* Tokens and credentials are passed through and never logged. *)
    (match network.Types.authenticate_negotiate with
     | Some authenticate ->
       Jsonrpc.register_handler rpc "network.authenticateNegotiate" (fun _method params ->
         let open Lwt.Syntax in
         let* token = authenticate (Option.value (json_string params "spn") ~default:"") in
         Lwt.return
           (match token with
            | Some value when value <> "" -> `Assoc [ ("token", `String value) ]
            | _ -> `Assoc []))
     | None -> ());
    (match network.Types.authenticate_basic with
     | Some authenticate ->
       Jsonrpc.register_handler rpc "network.authenticateBasic" (fun _method params ->
         let open Lwt.Syntax in
         let request : Types.network_basic_auth_request =
           { Types.nba_proxy = Option.value (json_string params "proxy") ~default:""
           ; nba_realm = json_string params "realm"
           ; nba_previous_rejected =
               (match json_member params "previousRejected" with
                | Some (`Bool b) -> b
                | _ -> false)
           }
         in
         let* credentials = authenticate request in
         let fields =
           match credentials with
           | None -> []
           | Some c ->
             (match c.Types.nbc_username with
              | Some u -> [ ("username", `String u) ]
              | None -> [])
             @ (match c.Types.nbc_password with
                | Some p -> [ ("password", `String p) ]
                | None -> [])
         in
         Lwt.return (`Assoc fields))
     | None -> ())

(* ========================================================================== *)
(* Handshake                                                                  *)
(* ========================================================================== *)

(* Sends [connect] (with clientInfo when set) and falls back to [ping] on
   servers without [connect]; the server must report exactly
   Version.sdk_protocol_version. *)
let perform_handshake (t : t) (rpc : Jsonrpc.t) : unit Lwt.t =
  let open Lwt.Syntax in
  let connect_params =
    match Option.bind t.options.client_info Types.client_info_to_yojson with
    | Some info -> `Assoc [ ("clientInfo", info) ]
    | None -> `Assoc []
  in
  let* server_version =
    Lwt.catch
      (fun () ->
        let* result = Jsonrpc.send_request rpc "connect" connect_params in
        Lwt.return (json_int result "protocolVersion"))
      (function
        | Jsonrpc.Jsonrpc_error err
          when err.Jsonrpc.code = -32601 || err.Jsonrpc.message = "Unhandled method connect" ->
          let* pong = Jsonrpc.send_request rpc "ping" (`Assoc [ ("message", `String "ping") ]) in
          Lwt.return (json_int pong "protocolVersion")
        | exn -> Lwt.fail exn)
  in
  let expected = Version.sdk_protocol_version in
  match server_version with
  | None ->
    Lwt.fail_with
      (Printf.sprintf
         "SDK protocol version mismatch: SDK supports version %d, but server does not report a protocol version. Please update your SDK or server to ensure compatibility."
         expected)
  | Some actual when actual < Version.min_protocol_version || actual > Version.max_protocol_version ->
    Lwt.fail_with
      (Printf.sprintf
         "SDK protocol version mismatch: SDK supports version %d, but server reports version %d. Please update your SDK or server to ensure compatibility."
         expected actual)
  | Some actual ->
    t.negotiated_protocol_version <- Some actual;
    Lwt.return_unit

(* Registers the host network callbacks with network.setProvider. *)
let register_network_provider (t : t) (rpc : Jsonrpc.t) : unit Lwt.t =
  let open Lwt.Syntax in
  match Option.bind t.options.network Types.network_set_provider_params with
  | Some params ->
    let* _result = Jsonrpc.send_request rpc "network.setProvider" params in
    Lwt.return_unit
  | None -> Lwt.return_unit

let set_session_fs_provider (t : t) (config : Types.session_fs_config)
    : Yojson.Safe.t Lwt.t =
  let rpc = match t.rpc with
    | Some rpc -> rpc
    | None -> failwith "Client is not connected. Call start() first."
  in
  let params = Types.session_fs_config_to_yojson config in
  Jsonrpc.send_request rpc "sessionFs.setProvider" params

(* ========================================================================== *)
(* Lifecycle                                                                  *)
(* ========================================================================== *)

let start (t : t) : unit Lwt.t =
  let open Lwt.Syntax in
  t.state <- Connecting;
  Lwt.catch
    (fun () ->
      let* rpc =
        match t.options.cli_url with
        | Some url ->
          connect_tcp url
        | None ->
          let* rpc, process = spawn_cli t in
          t.process <- Some process;
          Lwt.return rpc
      in
      t.rpc <- Some rpc;
      register_server_handlers t rpc;
      let* () = perform_handshake t rpc in
      let* () = register_network_provider t rpc in
      t.state <- Connected;
      let* () =
        match t.options.session_fs with
        | Some fs_config ->
          let* _result = set_session_fs_provider t fs_config in
          Lwt.return_unit
        | None -> Lwt.return_unit
      in
      Lwt.return_unit)
    (fun exn ->
      t.state <- Error;
      Lwt.fail exn)

let stop (t : t) : unit Lwt.t =
  let open Lwt.Syntax in
  let* () =
    match t.rpc with
    | Some rpc -> Jsonrpc.stop rpc
    | None -> Lwt.return_unit
  in
  t.rpc <- None;
  let* () =
    match t.process with
    | Some p ->
      p#terminate;
      let* _status = p#close in
      Lwt.return_unit
    | None -> Lwt.return_unit
  in
  t.process <- None;
  t.state <- Disconnected;
  Lwt.return_unit

(* ========================================================================== *)
(* Session creation                                                           *)
(* ========================================================================== *)

let get_rpc (t : t) : Jsonrpc.t =
  match t.rpc with
  | Some rpc -> rpc
  | None -> failwith "Client is not connected. Call start() first."

let empty_mode_error =
  "CopilotClient is in mode 'empty' but the session config did not specify \
   available_tools. Empty mode requires every session to opt into the tools it wants."

(* Private random state for registration ids (does not touch the global one). *)
let registration_rng = lazy (Random.State.make_self_init ())

let registration_counter = ref 0

let new_registration_id () : string =
  let rng = Lazy.force registration_rng in
  incr registration_counter;
  Printf.sprintf "%08x-%04x-4%03x-%04x-%012x"
    (Random.State.bits rng)
    (Random.State.bits rng land 0xffff)
    (Random.State.bits rng land 0xfff)
    (!registration_counter land 0xffff)
    (Unix.getpid ())

(* Stores a GitHub token provider and returns its registration id. *)
let register_token_provider (t : t) (provider : Types.github_token_provider option)
    (session_id : string option) : string option =
  match provider with
  | None -> None
  | Some p ->
    let registration_id = new_registration_id () in
    Hashtbl.replace t.token_providers registration_id (p, ref session_id);
    Some registration_id

let remove_token_provider (t : t) (registration_id : string option) : unit =
  match registration_id with
  | Some id -> Hashtbl.remove t.token_providers id
  | None -> ()

let with_registration_id (flat : Yojson.Safe.t) (registration_id : string option)
    : Yojson.Safe.t =
  match flat, registration_id with
  | `Assoc fields, Some id ->
    `Assoc (fields @ [ ("gitHubTokenProviderRegistrationId", `String id) ])
  | _ -> flat

(* Translates the flat payload into the SDK protocol 4 grouped payload and
   applies the client-mode defaults (called right before sending). *)
let finalize_session_params (t : t) ~resume (flat : Yojson.Safe.t) : Yojson.Safe.t =
  let grouped = Session_wire.group_session_params ~resume flat in
  if t.mode = Types.client_mode_empty then Session_wire.apply_empty_mode_defaults grouped
  else grouped

(* Tracks a created/resumed session for server-to-client requests. *)
let track_session (t : t) (session : Session.t) (registration_id : string option) : unit =
  let session_id = Session.session_id session in
  (match registration_id with
   | Some id ->
     (match Hashtbl.find_opt t.token_providers id with
      | Some (_, session_ref) -> session_ref := Some session_id
      | None -> ())
   | None -> ());
  Hashtbl.replace t.sessions session_id session

let create_session ?(config = Types.default_session_config ()) (t : t)
    : Session.t Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  if Option.is_some config.github_token && Option.is_some config.github_token_provider then
    Lwt.fail_with "github_token and github_token_provider are mutually exclusive"
  else if t.mode = Types.client_mode_empty && Option.is_none config.available_tools then
    Lwt.fail_with empty_mode_error
  else begin
    let registration_id = register_token_provider t config.github_token_provider None in
    let flat = with_registration_id (Types.session_config_to_yojson config) registration_id in
    let params = finalize_session_params t ~resume:false flat in
    let* result =
      Lwt.catch
        (fun () -> Jsonrpc.send_request rpc "session.create" params)
        (fun exn ->
          remove_token_provider t registration_id;
          Lwt.fail exn)
    in
    let open Yojson.Safe.Util in
    let session_id = result |> member "sessionId" |> to_string in
    let session = Session.create ~rpc ~session_id ~config in
    track_session t session registration_id;
    (match t.options.on_get_trace_context with
     | Some provider -> Session.register_trace_context_provider session provider
     | None -> ());
    Session.setup_notifications session;
    Lwt.return session
  end

(* Detaches a session (session.detach) and stops tracking it. *)
let disconnect_session (t : t) (session : Session.t) : unit Lwt.t =
  let open Lwt.Syntax in
  let* () = Session.detach session in
  let session_id = Session.session_id session in
  (match Hashtbl.find_opt t.sessions session_id with
   | Some tracked when tracked == session -> Hashtbl.remove t.sessions session_id
   | _ -> ());
  Hashtbl.filter_map_inplace
    (fun _registration_id ((_, session_ref) as registration) ->
      if !session_ref = Some session_id then None else Some registration)
    t.token_providers;
  Lwt.return_unit

(* Clears the server's managed-settings cache (managedSettings.clearCache). *)
let clear_managed_settings_cache (t : t) : unit Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* _result = Jsonrpc.send_request rpc "managedSettings.clearCache" (`Assoc []) in
  Lwt.return_unit

(* Policy Helper failure carried by a session.create / session.resume error
   (error.data.policyHelperFailure). *)
let policy_helper_failure (exn : exn) : Types.policy_helper_failure option =
  match exn with
  | Jsonrpc.Jsonrpc_error err -> Types.policy_helper_failure_of_error_data err.Jsonrpc.data
  | _ -> None

let negotiated_protocol_version (t : t) : int option = t.negotiated_protocol_version

let mode (t : t) : string = t.mode

(* ========================================================================== *)
(* Server queries                                                             *)
(* ========================================================================== *)

let get_status (t : t) : Types.get_status_response Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* result = Jsonrpc.send_request rpc "status.get" `Null in
  match Types.get_status_response_of_yojson result with
  | Ok resp -> Lwt.return resp
  | Error msg -> Lwt.fail_with msg

let list_models (t : t) : Types.model_info list Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* result = Jsonrpc.send_request rpc "models.list" `Null in
  let open Yojson.Safe.Util in
  let models = result |> to_list in
  Lwt.return
    (List.filter_map
       (fun j ->
         match Types.model_info_of_yojson j with Ok m -> Some m | Error _ -> None)
       models)

let get_foreground_session_id (t : t) : string Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* result = Jsonrpc.send_request rpc "session.getForeground" (`Assoc []) in
  let open Yojson.Safe.Util in
  Lwt.return (result |> member "sessionId" |> to_string)

let set_foreground_session_id (t : t) (session_id : string) : unit Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let params = `Assoc [ ("sessionId", `String session_id) ] in
  let* result = Jsonrpc.send_request rpc "session.setForeground" params in
  let open Yojson.Safe.Util in
  let success = try result |> member "success" |> to_bool with _ -> false in
  if not success then
    let err =
      try result |> member "error" |> to_string with _ -> "Unknown"
    in
    Lwt.fail_with (Printf.sprintf "Failed to set foreground session: %s" err)
  else Lwt.return_unit

(* ========================================================================== *)
(* State inspection                                                           *)
(* ========================================================================== *)

let get_last_session_id (t : t) : string option Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* result = Jsonrpc.send_request rpc "session.getLastId" (`Assoc []) in
  let open Yojson.Safe.Util in
  let sid = try Some (result |> member "sessionId" |> to_string) with _ -> None in
  Lwt.return sid

let get_session_metadata (t : t) (session_id : string) : Yojson.Safe.t Lwt.t =
  let rpc = get_rpc t in
  let params = `Assoc [ ("sessionId", `String session_id) ] in
  Jsonrpc.send_request rpc "session.getMetadata" params

let ping ?(message = "ping") (t : t) : Yojson.Safe.t Lwt.t =
  let rpc = get_rpc t in
  let params = `Assoc [ ("message", `String message) ] in
  Jsonrpc.send_request rpc "ping" params

let resume_session (t : t) (session_id : string) ?(config = Types.default_session_config ()) ()
    : Session.t Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  if t.mode = Types.client_mode_empty && Option.is_none config.available_tools then
    Lwt.fail_with empty_mode_error
  else begin
    let registration_id =
      register_token_provider t config.github_token_provider (Some session_id)
    in
    let base_params = Types.session_resume_config_to_yojson config in
    let flat =
      match base_params with
      | `Assoc fields -> `Assoc (("sessionId", `String session_id) :: fields)
      | _ -> `Assoc [ ("sessionId", `String session_id) ]
    in
    let params = finalize_session_params t ~resume:true (with_registration_id flat registration_id) in
    let* result =
      Lwt.catch
        (fun () -> Jsonrpc.send_request rpc "session.resume" params)
        (fun exn ->
          remove_token_provider t registration_id;
          Lwt.fail exn)
    in
    let open Yojson.Safe.Util in
    let sid = try result |> member "sessionId" |> to_string with _ -> session_id in
    let session = Session.create ~rpc ~session_id:sid ~config in
    track_session t session registration_id;
    (match t.options.on_get_trace_context with
     | Some provider -> Session.register_trace_context_provider session provider
     | None -> ());
    Session.setup_notifications session;
    Lwt.return session
  end

let delete_session (t : t) (session_id : string) : unit Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let params = `Assoc [ ("sessionId", `String session_id) ] in
  let* result = Jsonrpc.send_request rpc "session.delete" params in
  Hashtbl.remove t.sessions session_id;
  match json_member result "success" with
  | Some (`Bool false) ->
    let err = Option.value (json_string result "error") ~default:"Unknown error" in
    Lwt.fail_with (Printf.sprintf "Failed to delete session %s: %s" session_id err)
  | _ -> Lwt.return_unit

let list_sessions (t : t) : Yojson.Safe.t Lwt.t =
  let rpc = get_rpc t in
  Jsonrpc.send_request rpc "session.list" (`Assoc [])

let get_auth_status (t : t) : Yojson.Safe.t Lwt.t =
  let rpc = get_rpc t in
  Jsonrpc.send_request rpc "auth.getStatus" (`Assoc [])

(* ========================================================================== *)
(* State inspection                                                           *)
(* ========================================================================== *)

let connection_state (t : t) : Types.connection_state = t.state
