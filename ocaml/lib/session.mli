(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Copilot session -- a single conversation with the Copilot CLI.

    Each session manages its own event stream, tool handlers, and
    permission handling. *)

(** {1 Types} *)

type event_handler = Types.session_event -> unit Lwt.t
(** Callback invoked for every session event. *)

type tool_handler =
  Types.tool_invocation -> Types.tool_result Lwt.t
(** Callback invoked when the agent calls a registered tool. *)

type permission_handler =
  Yojson.Safe.t -> string -> Types.permission_result Lwt.t
(** [permission_handler request session_id] decides whether a permission
    request should be approved or denied. *)

type exit_plan_mode_handler =
  Types.exit_plan_mode_request -> Types.exit_plan_mode_response Lwt.t
(** Callback invoked when the agent requests to exit plan mode. *)

(** {1 Session handle} *)

type t

val create :
  rpc:Jsonrpc.t ->
  session_id:string ->
  config:Types.session_config ->
  t
(** [create ~rpc ~session_id ~config] builds a new session handle. The
    underlying RPC connection must already be started. *)

val session_id : t -> string
(** [session_id t] returns the unique identifier of this session. *)

val on : t -> event_handler -> unit
(** [on t handler] registers an event listener. *)

val send : t -> Types.message_options -> unit Lwt.t
(** [send t msg] sends a user message into the session. *)

val send_and_wait :
  ?timeout:float -> t -> Types.message_options -> Types.session_event option Lwt.t
(** [send_and_wait ?timeout t msg] sends a message and waits for the
    terminal [assistant.message] or [session.idle] event. Returns [None]
    on timeout (default 60 s). *)

val register_tool : t -> string -> tool_handler -> unit
(** [register_tool t name handler] registers a handler for a named tool. *)

val register_permission_handler : t -> permission_handler -> unit
(** [register_permission_handler t handler] sets the permission callback. *)

val register_exit_plan_mode_handler : t -> exit_plan_mode_handler -> unit
(** [register_exit_plan_mode_handler t handler] sets the exit plan mode callback. *)

val register_trace_context_provider : t -> (unit -> Types.trace_context) -> unit
(** [register_trace_context_provider t provider] sets a trace context provider
    that injects traceparent/tracestate into outgoing requests. *)

val destroy : t -> unit Lwt.t
(** [destroy t] terminates the session on the server side. *)

val setup_notifications : t -> unit
(** [setup_notifications t] wires the session's notification handler to
    the underlying JSON-RPC connection. Called automatically by
    {!Client.create_session}. *)

val is_idle : t -> bool
(** [is_idle t] is [true] when the last event was [session.idle]. *)

(** {1 v2.6.0 / v3.0.0 session operations} *)

val switch_auto_tier : t -> string option -> Yojson.Safe.t Lwt.t
(** [switch_auto_tier t tier] changes the Auto routing preference
    ([session.model.switchAutoTier]); [None] returns to the default
    routing. Experimental. *)

val detach : t -> unit Lwt.t
(** [detach t] sends [session.detach]: the session is released from this
    connection and can be resumed later. *)

val get_managed_settings : t -> Yojson.Safe.t Lwt.t
(** [get_managed_settings t] returns the managed settings in effect. *)

val recheck_policy_helper : t -> Yojson.Safe.t Lwt.t
(** [recheck_policy_helper t] re-runs the managed-settings policy helper
    ([session.managedSettings.recheckPolicyHelper]). *)

val set_permission_mode : t -> string -> Types.permission_mode_set_result Lwt.t
(** [set_permission_mode t mode] calls [session.permissions.setMode]. A
    refused change reports [pms_success = false] and may carry the reason
    {!Types.permission_mode_refusal_managed_policy}. *)

val handle_hooks_invoke : t -> string -> Yojson.Safe.t -> Yojson.Safe.t option Lwt.t
(** Dispatches a [hooks.invoke] request ("subagentStart" / "subagentStop")
    to the session's hook handlers. Used by {!Client}. *)

val handle_shell_authentication :
  t -> Types.shell_authentication_request -> Types.shell_authentication_response Lwt.t
(** Answers a [shellAuthentication.request] with the session's handler;
    cancels when no handler is set or the request is cancelled first. *)

val cancel_shell_authentication : t -> string -> unit
(** Cancels a pending shell authentication prompt by request id. *)

val handle_memory_store : t -> Types.stored_memory -> unit Lwt.t
(** Delivers a [memoryStore.store] request to the session's callback. *)
