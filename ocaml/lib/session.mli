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
