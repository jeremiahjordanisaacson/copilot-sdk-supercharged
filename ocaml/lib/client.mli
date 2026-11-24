(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Copilot CLI SDK Client -- main entry point for the Copilot SDK.

    This module provides the {!CopilotClient} for managing the connection
    to the Copilot CLI server and creating conversation sessions. *)

(** {1 Client handle} *)

type t

(** {1 Lifecycle} *)

val create : ?options:Types.client_options -> unit -> t
(** [create ?options ()] creates a new client. The client is not connected
    until {!start} is called. *)

val start : t -> unit Lwt.t
(** [start t] spawns the CLI subprocess (or connects to an existing server
    if [cli_url] was provided) and performs the initial handshake. *)

val stop : t -> unit Lwt.t
(** [stop t] terminates the connection and kills the subprocess if one
    was spawned. *)

(** {1 Sessions} *)

val create_session :
  ?config:Types.session_config -> t -> Session.t Lwt.t
(** [create_session ?config t] creates a new conversation session on the
    connected CLI server. *)

(** {1 Server queries} *)

val get_status : t -> Types.get_status_response Lwt.t
(** [get_status t] queries the CLI server status including protocol
    version. *)

val list_models : t -> Types.model_info list Lwt.t
(** [list_models t] returns the models available on the connected CLI
    server. *)

(** {1 State inspection} *)

val connection_state : t -> Types.connection_state
(** [connection_state t] returns the current connection state. *)

val get_foreground_session_id : t -> string Lwt.t
(** [get_foreground_session_id t] returns the session ID of the current
    foreground session. *)

val set_foreground_session_id : t -> string -> unit Lwt.t
(** [set_foreground_session_id t session_id] sets the foreground session
    to the given session ID. *)

val get_last_session_id : t -> string option Lwt.t
(** [get_last_session_id t] returns the last-used session ID, or [None]. *)

val get_session_metadata : t -> string -> Yojson.Safe.t Lwt.t
(** [get_session_metadata t session_id] retrieves metadata for a session. *)

val ping : ?message:string -> t -> Yojson.Safe.t Lwt.t
(** [ping ?message t] pings the server and returns the response. *)

val resume_session : t -> string -> ?config:Types.session_config -> unit -> Session.t Lwt.t
(** [resume_session t session_id ?config ()] resumes a session by ID. *)

val delete_session : t -> string -> unit Lwt.t
(** [delete_session t session_id] permanently deletes a session. *)

val list_sessions : t -> Yojson.Safe.t Lwt.t
(** [list_sessions t] lists all sessions. *)

val get_auth_status : t -> Yojson.Safe.t Lwt.t
(** [get_auth_status t] returns authentication status. *)

val set_session_fs_provider : t -> Types.session_fs_config -> Yojson.Safe.t Lwt.t
(** [set_session_fs_provider t config] registers a session filesystem provider. *)

(** {1 v2.6.0 / v3.0.0 (SDK protocol 4)} *)

val disconnect_session : t -> Session.t -> unit Lwt.t
(** [disconnect_session t session] sends [session.detach] and stops routing
    server requests to the session. Its state on disk is kept, so it can be
    resumed later. *)

val clear_managed_settings_cache : t -> unit Lwt.t
(** [clear_managed_settings_cache t] calls [managedSettings.clearCache]. *)

val policy_helper_failure : exn -> Types.policy_helper_failure option
(** [policy_helper_failure exn] reads [error.data.policyHelperFailure] from a
    [Jsonrpc.Jsonrpc_error] raised by [session.create] / [session.resume]. *)

val negotiated_protocol_version : t -> int option
(** [negotiated_protocol_version t] is the protocol version reported by the
    server during {!start} (always 4 once started). *)

val mode : t -> string
(** [mode t] is the client mode in effect: ["coding-agent"] or ["empty"]. *)

(** {1 v3.1.0: MCP secret store and plugin extension approvals}

    Server-scoped requests that require Copilot CLI 1.0.116 or newer. An older
    runtime answers with a method-not-found [Jsonrpc.Jsonrpc_error], raised like
    any other RPC error. *)

val mcp_secrets_save :
  ?config_dir:string -> t -> secret_id:string -> value:string ->
  Types.mcp_secrets_save_result Lwt.t
(** [mcp_secrets_save ?config_dir t ~secret_id ~value] saves a legacy MCP
    configuration credential in the runtime-owned secret store
    ([mcp.secrets.save]). [value] is a credential: it is sent to the runtime
    and never logged. [config_dir] overrides the runtime's configuration root.
    [mss_used_keychain] is [true] when the OS keychain stored the credential
    and [false] for the file backend. *)

val mcp_secrets_delete : ?config_dir:string -> t -> secret_id:string -> unit Lwt.t
(** [mcp_secrets_delete ?config_dir t ~secret_id] deletes a legacy MCP
    configuration credential ([mcp.secrets.delete]). *)

val mcp_secrets_delete_server : ?config_dir:string -> t -> server_name:string -> unit Lwt.t
(** [mcp_secrets_delete_server ?config_dir t ~server_name] deletes the legacy
    credentials indexed for an MCP server ([mcp.secrets.deleteServer]). *)

val extension_approvals_grant :
  t -> session_id:string -> request_id:string ->
  Types.permissions_extension_approvals_grant_result Lwt.t
(** [extension_approvals_grant t ~session_id ~request_id] persists the pending
    plugin extension permission request [request_id] of session [session_id]
    as a user-scoped approval, before you answer that request normally
    ([permissions.extensionApprovals.grant]). *)

val extension_approvals_list :
  ?session_id:string -> t -> Types.permissions_extension_approvals_list_result Lwt.t
(** [extension_approvals_list ?session_id t] lists the user-scoped plugin
    extension approvals ([permissions.extensionApprovals.list]); [session_id]
    selects an attached session's configuration profile. *)

val extension_approvals_revoke :
  ?session_id:string -> t -> extension_name:string -> installation_key:string ->
  Types.permissions_extension_approvals_revoke_result Lwt.t
(** [extension_approvals_revoke ?session_id t ~extension_name ~installation_key]
    revokes one approval ([permissions.extensionApprovals.revoke]). Pass the
    approval's [pea_extension_name] and [pea_installation_key] unchanged. *)
