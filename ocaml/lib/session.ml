(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Copilot session -- manages a single conversation. *)

(* ========================================================================== *)
(* Types                                                                      *)
(* ========================================================================== *)

type event_handler = Types.session_event -> unit Lwt.t

type tool_handler =
  Types.tool_invocation -> Types.tool_result Lwt.t

type permission_handler =
  Yojson.Safe.t -> string -> Types.permission_result Lwt.t

type t = {
  rpc : Jsonrpc.t;
  session_id : string;
  config : Types.session_config;
  mutable event_handlers : event_handler list;
  tool_handlers : (string, tool_handler) Hashtbl.t;
  mutable permission_handler : permission_handler option;
  mutable idle : bool;
}

(* ========================================================================== *)
(* Construction                                                               *)
(* ========================================================================== *)

let create ~rpc ~session_id ~config =
  let t =
    { rpc
    ; session_id
    ; config
    ; event_handlers = []
    ; tool_handlers = Hashtbl.create 8
    ; permission_handler = None
    ; idle = false
    }
  in
  (* Register JSON-RPC handlers for tool calls and permissions *)
  Jsonrpc.register_handler rpc "tools/handlePendingToolCall"
    (fun _method params ->
      let open Lwt.Syntax in
      match Types.tool_invocation_of_yojson params with
      | Error msg ->
        Lwt.return
          (Types.tool_result_to_yojson
             (Types.make_tool_result ~result_type:Failure msg))
      | Ok invocation ->
        (match Hashtbl.find_opt t.tool_handlers invocation.tool_name with
         | None ->
           Lwt.return
             (Types.tool_result_to_yojson
                (Types.make_tool_result ~result_type:Failure
                   (Printf.sprintf "No handler for tool: %s"
                      invocation.tool_name)))
         | Some handler ->
           let* result =
             Lwt.catch
               (fun () -> handler invocation)
               (fun exn ->
                 Lwt.return
                   (Types.make_tool_result ~result_type:Failure
                      (Printexc.to_string exn)))
           in
           Lwt.return (Types.tool_result_to_yojson result)));
  Jsonrpc.register_handler rpc "permission/handlePermissionRequest"
    (fun _method params ->
      let open Lwt.Syntax in
      match t.permission_handler with
      | None ->
        (* Default: deny *)
        Lwt.return
          (Types.permission_result_to_yojson
             { decision = DeniedByUser; rules = None })
      | Some handler ->
        let* result =
          Lwt.catch
            (fun () -> handler params t.session_id)
            (fun _exn ->
              Lwt.return
                Types.{ decision = DeniedByUser; rules = None })
        in
        Lwt.return (Types.permission_result_to_yojson result));
  t

(* ========================================================================== *)
(* Event dispatching                                                          *)
(* ========================================================================== *)

let dispatch_event (t : t) (event : Types.session_event) : unit Lwt.t =
  let open Lwt.Syntax in
  (match event.event_type with
   | SessionIdle -> t.idle <- true
   | _ -> t.idle <- false);
  Lwt_list.iter_s (fun handler -> handler event) t.event_handlers

let setup_notifications (t : t) : unit =
  Jsonrpc.set_notification_handler t.rpc
    (fun method_ params ->
      if method_ = "session/event" then
        match Types.session_event_of_yojson params with
        | Ok event -> dispatch_event t event
        | Error _ -> Lwt.return_unit
      else Lwt.return_unit)

(* ========================================================================== *)
(* Public API                                                                 *)
(* ========================================================================== *)

let session_id (t : t) : string = t.session_id

let on (t : t) (handler : event_handler) : unit =
  t.event_handlers <- handler :: t.event_handlers

let send (t : t) (msg : Types.message_options) : unit Lwt.t =
  let params =
    `Assoc
      [ ("sessionId", `String t.session_id)
      ; ("message", Types.message_options_to_yojson msg)
      ]
  in
  let open Lwt.Syntax in
  let* _result = Jsonrpc.send_request t.rpc "session/send" params in
  Lwt.return_unit

let send_and_wait ?(timeout = 60.0) (t : t) (msg : Types.message_options)
    : Types.session_event option Lwt.t =
  let open Lwt.Syntax in
  let waiter, resolver = Lwt.wait () in
  let resolved = ref false in
  let handler (event : Types.session_event) : unit Lwt.t =
    if not !resolved then
      (match event.event_type with
       | AssistantMessage | SessionIdle | SessionError ->
         resolved := true;
         Lwt.wakeup resolver (Some event);
         Lwt.return_unit
       | _ -> Lwt.return_unit)
    else Lwt.return_unit
  in
  on t handler;
  let* () = send t msg in
  let timeout_thread =
    let* () = Lwt_unix.sleep timeout in
    if not !resolved then (
      resolved := true;
      Lwt.wakeup resolver None);
    Lwt.return_unit
  in
  Lwt.async (fun () -> timeout_thread);
  waiter

let register_tool (t : t) (name : string) (handler : tool_handler) : unit =
  Hashtbl.replace t.tool_handlers name handler

let register_permission_handler (t : t) (handler : permission_handler) : unit =
  t.permission_handler <- Some handler

let destroy (t : t) : unit Lwt.t =
  let params = `Assoc [ ("sessionId", `String t.session_id) ] in
  let open Lwt.Syntax in
  let* _result = Jsonrpc.send_request t.rpc "session/destroy" params in
  Lwt.return_unit

let is_idle (t : t) : bool = t.idle
