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
