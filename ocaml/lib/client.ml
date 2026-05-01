(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Copilot CLI SDK Client -- main entry point for the Copilot SDK. *)

(* ========================================================================== *)
(* Internal state                                                             *)
(* ========================================================================== *)

type t = {
  options : Types.client_options;
  mutable rpc : Jsonrpc.t option;
  mutable process : Lwt_process.process_full option;
  mutable state : Types.connection_state;
}

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
  { options; rpc = None; process = None; state = Disconnected }

(* ========================================================================== *)
(* Spawn CLI subprocess                                                       *)
(* ========================================================================== *)

let spawn_cli (t : t) : (Jsonrpc.t * Lwt_process.process_full) Lwt.t =
  let open Lwt.Syntax in
  let cli_path = find_cli_path t.options in
  let args = [| cli_path; "--stdio" |] in
  let args =
    match t.options.log_level with
    | Some level -> Array.append args [| "--log-level"; level |]
    | None -> args
  in
  let cmd = (cli_path, args) in
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
(* Handshake                                                                  *)
(* ========================================================================== *)

let perform_handshake (rpc : Jsonrpc.t) : unit Lwt.t =
  let open Lwt.Syntax in
  let params =
    `Assoc
      [ ("protocolVersion", `Int Version.sdk_protocol_version)
      ; ("sdkName", `String Version.sdk_name)
      ; ("sdkVersion", `String Version.sdk_version)
      ]
  in
  let* result = Jsonrpc.send_request rpc "initialize" params in
  let open Yojson.Safe.Util in
  let server_version =
    try result |> member "protocolVersion" |> to_int with _ -> 0
  in
  if server_version < Version.min_protocol_version then
    Lwt.fail_with
      (Printf.sprintf
         "Server protocol version %d is below minimum %d"
         server_version Version.min_protocol_version)
  else Lwt.return_unit

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
      let* () = perform_handshake rpc in
      t.state <- Connected;
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

let create_session ?(config = Types.default_session_config ()) (t : t)
    : Session.t Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let params = Types.session_config_to_yojson config in
  let* result = Jsonrpc.send_request rpc "session/create" params in
  let open Yojson.Safe.Util in
  let session_id = result |> member "sessionId" |> to_string in
  let session = Session.create ~rpc ~session_id ~config in
  Session.setup_notifications session;
  Lwt.return session

(* ========================================================================== *)
(* Server queries                                                             *)
(* ========================================================================== *)

let get_status (t : t) : Types.get_status_response Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* result = Jsonrpc.send_request rpc "getStatus" `Null in
  match Types.get_status_response_of_yojson result with
  | Ok resp -> Lwt.return resp
  | Error msg -> Lwt.fail_with msg

let list_models (t : t) : Types.model_info list Lwt.t =
  let open Lwt.Syntax in
  let rpc = get_rpc t in
  let* result = Jsonrpc.send_request rpc "models/list" `Null in
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

let connection_state (t : t) : Types.connection_state = t.state
