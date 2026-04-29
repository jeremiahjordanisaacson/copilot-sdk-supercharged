(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** JSON-RPC 2.0 transport layer for communicating with the Copilot CLI.

    Implements Content-Length header framing (LSP-style) over stdio or TCP. *)

(** {1 Errors} *)

type jsonrpc_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}

exception Jsonrpc_error of jsonrpc_error
exception Process_exited of string

(** {1 Request Handler} *)

type request_handler = string -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t

(** {1 Client} *)

type t

val create : Lwt_io.input_channel -> Lwt_io.output_channel -> t
(** [create ic oc] creates a JSON-RPC client using the given input/output
    channels. Typically these come from a spawned subprocess or TCP socket. *)

val start : t -> unit Lwt.t
(** [start t] begins reading messages from the input channel in the
    background. Must be called before [send_request] or
    [send_notification]. *)

val stop : t -> unit Lwt.t
(** [stop t] terminates the background reader and closes channels. *)

val send_request :
  t -> string -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t
(** [send_request t method_ params] sends a JSON-RPC request and returns
    the result. Raises [Jsonrpc_error] on error responses. *)

val send_notification : t -> string -> Yojson.Safe.t -> unit Lwt.t
(** [send_notification t method_ params] sends a one-way JSON-RPC
    notification. *)

val register_handler : t -> string -> request_handler -> unit
(** [register_handler t method_ handler] registers a handler for incoming
    requests with the given method name. *)

val set_notification_handler :
  t -> (string -> Yojson.Safe.t -> unit Lwt.t) -> unit
(** [set_notification_handler t f] sets the callback for incoming
    notifications. *)

val is_running : t -> bool
(** [is_running t] returns [true] while the background reader is active. *)
