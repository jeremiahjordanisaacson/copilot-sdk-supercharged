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

type exit_plan_mode_handler =
  Types.exit_plan_mode_request -> Types.exit_plan_mode_response Lwt.t

type t = {
  rpc : Jsonrpc.t;
  session_id : string;
  config : Types.session_config;
  mutable event_handlers : event_handler list;
  tool_handlers : (string, tool_handler) Hashtbl.t;
  mutable permission_handler : permission_handler option;
  mutable exit_plan_mode_handler : exit_plan_mode_handler option;
  mutable trace_context_provider : (unit -> Types.trace_context) option;
  mutable idle : bool;
  (* Pending shell authentication prompts, keyed by request id. *)
  pending_shell_auth : (string, unit Lwt.u) Hashtbl.t;
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
    ; exit_plan_mode_handler = None
    ; trace_context_provider = None
    ; idle = false
    ; pending_shell_auth = Hashtbl.create 4
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
  Jsonrpc.register_handler rpc "exitPlanMode.request"
    (fun _method params ->
      let open Lwt.Syntax in
      match t.exit_plan_mode_handler with
      | None ->
        Lwt.return
          (Types.exit_plan_mode_response_to_yojson
             (Types.default_exit_plan_mode_response ()))
      | Some handler ->
        (match Types.exit_plan_mode_request_of_yojson params with
         | Error _ ->
           Lwt.return
             (Types.exit_plan_mode_response_to_yojson
                (Types.default_exit_plan_mode_response ()))
         | Ok req ->
           let* result =
             Lwt.catch
               (fun () -> handler req)
               (fun _exn ->
                 Lwt.return (Types.default_exit_plan_mode_response ()))
           in
           Lwt.return (Types.exit_plan_mode_response_to_yojson result)));
  t

(* ========================================================================== *)
(* Event dispatching                                                          *)
(* ========================================================================== *)

let dispatch_event (t : t) (event : Types.session_event) : unit Lwt.t =
  (match event.event_type with
   | SessionIdle -> t.idle <- true
   | _ -> t.idle <- false);
  Lwt_list.iter_s (fun handler -> handler event) t.event_handlers

let setup_notifications (t : t) : unit =
  Jsonrpc.set_notification_handler t.rpc
    (fun method_ params ->
      if method_ = "session.event" || method_ = "session/event" then begin
        (* Protocol 4 wraps the event as {sessionId, event}; older payloads
           carry the event itself. *)
        let event_json =
          match params with
          | `Assoc fields ->
            (match List.assoc_opt "event" fields with
             | Some event -> (match event with `Assoc _ -> event | _ -> params)
             | None -> params)
          | _ -> params
        in
        match Types.session_event_of_yojson event_json with
        | Ok event -> dispatch_event t event
        | Error _ -> Lwt.return_unit
      end
      else Lwt.return_unit)

(* ========================================================================== *)
(* Public API                                                                 *)
(* ========================================================================== *)

let session_id (t : t) : string = t.session_id

let on (t : t) (handler : event_handler) : unit =
  t.event_handlers <- handler :: t.event_handlers

let inject_trace_context (t : t) (params : Yojson.Safe.t) : Yojson.Safe.t =
  match t.trace_context_provider with
  | None -> params
  | Some provider ->
    (try
       let ctx = provider () in
       let fields = match params with `Assoc fs -> fs | _ -> [] in
       let fields = match ctx.traceparent with
         | Some tp -> ("traceparent", `String tp) :: fields
         | None -> fields
       in
       let fields = match ctx.tracestate with
         | Some ts -> ("tracestate", `String ts) :: fields
         | None -> fields
       in
       `Assoc fields
     with _ -> params)

let send (t : t) (msg : Types.message_options) : unit Lwt.t =
  (* session.send takes the message fields (prompt, source, responseFormat,
     clientTurnId, ...) next to the session id. *)
  let message_fields =
    match Types.message_options_to_yojson msg with
    | `Assoc fields -> fields
    | _ -> []
  in
  let params = `Assoc (("sessionId", `String t.session_id) :: message_fields) in
  let params = inject_trace_context t params in
  let open Lwt.Syntax in
  let* _result = Jsonrpc.send_request t.rpc "session.send" params in
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

let register_exit_plan_mode_handler (t : t) (handler : exit_plan_mode_handler) : unit =
  t.exit_plan_mode_handler <- Some handler

let register_trace_context_provider (t : t) (provider : unit -> Types.trace_context) : unit =
  t.trace_context_provider <- Some provider

let destroy (t : t) : unit Lwt.t =
  let params = `Assoc [ ("sessionId", `String t.session_id) ] in
  let open Lwt.Syntax in
  let* _result = Jsonrpc.send_request t.rpc "session/destroy" params in
  Lwt.return_unit

let is_idle (t : t) : bool = t.idle

(* ========================================================================== *)
(* v2.6.0 / v3.0.0 session RPCs and host callbacks                            *)
(* ========================================================================== *)

let request_with_session (t : t) (method_ : string) (extra : (string * Yojson.Safe.t) list)
    : Yojson.Safe.t Lwt.t =
  Jsonrpc.send_request t.rpc method_ (`Assoc (("sessionId", `String t.session_id) :: extra))

let switch_auto_tier (t : t) (auto_tier : string option) : Yojson.Safe.t Lwt.t =
  let tier = match auto_tier with Some value -> `String value | None -> `Null in
  request_with_session t "session.model.switchAutoTier" [ ("autoTier", tier) ]

let detach (t : t) : unit Lwt.t =
  let open Lwt.Syntax in
  let* _result = request_with_session t "session.detach" [] in
  Hashtbl.iter (fun _request_id cancel -> Lwt.wakeup_later cancel ()) t.pending_shell_auth;
  Hashtbl.reset t.pending_shell_auth;
  Lwt.return_unit

let get_managed_settings (t : t) : Yojson.Safe.t Lwt.t =
  request_with_session t "session.managedSettings.get" []

let recheck_policy_helper (t : t) : Yojson.Safe.t Lwt.t =
  request_with_session t "session.managedSettings.recheckPolicyHelper" []

let set_permission_mode (t : t) (mode : string) : Types.permission_mode_set_result Lwt.t =
  let open Lwt.Syntax in
  let* result = request_with_session t "session.permissions.setMode" [ ("mode", `String mode) ] in
  Lwt.return (Types.permission_mode_set_result_of_yojson result)

let handle_hooks_invoke (t : t) (hook_type : string) (input : Yojson.Safe.t)
    : Yojson.Safe.t option Lwt.t =
  let handler =
    match hook_type with
    | "subagentStart" -> t.config.Types.on_subagent_start
    | "subagentStop" -> t.config.Types.on_subagent_stop
    | _ -> None
  in
  match handler with
  | Some hook -> hook input
  | None -> Lwt.return_none

let handle_shell_authentication (t : t) (request : Types.shell_authentication_request)
    : Types.shell_authentication_response Lwt.t =
  let open Lwt.Syntax in
  let request_id = request.Types.sar_request_id in
  match t.config.Types.on_shell_authentication_request with
  | None -> Lwt.return Types.Shell_auth_cancel
  | Some _ when Hashtbl.mem t.pending_shell_auth request_id ->
    Lwt.return Types.Shell_auth_cancel
  | Some handler ->
    let cancelled, cancel = Lwt.wait () in
    Hashtbl.replace t.pending_shell_auth request_id cancel;
    let answer =
      Lwt.catch (fun () -> handler request) (fun _exn -> Lwt.return Types.Shell_auth_cancel)
    in
    let on_cancel =
      let* () = cancelled in
      Lwt.return Types.Shell_auth_cancel
    in
    let* response = Lwt.pick [ answer; on_cancel ] in
    Hashtbl.remove t.pending_shell_auth request_id;
    Lwt.return response

let cancel_shell_authentication (t : t) (request_id : string) : unit =
  match Hashtbl.find_opt t.pending_shell_auth request_id with
  | Some cancel ->
    Hashtbl.remove t.pending_shell_auth request_id;
    Lwt.wakeup_later cancel ()
  | None -> ()

let handle_memory_store (t : t) (memory : Types.stored_memory) : unit Lwt.t =
  match t.config.Types.on_store_memory with
  | Some store -> store memory
  | None -> Lwt.fail_with "No memory store is registered for this session"
