(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Tool definition helpers for the Copilot SDK.

    Provides a functional API for declaring tools with JSON Schema
    parameters that the Copilot agent can invoke during a session. *)

open Types

(** [define_tool ~name ~description ?parameters handler] builds a
    {!Types.tool_definition} and pairs it with its handler function.
    This is the primary way to declare tools in the OCaml SDK. *)
let define_tool ~name ~description ?parameters
    (handler : tool_invocation -> tool_result Lwt.t)
    : tool_definition * Session.tool_handler =
  let defn = make_tool_definition ?parameters name description in
  (defn, handler)

(** [make_string_param ~name ~description ()] creates a JSON Schema
    property object of type [string]. *)
let make_string_param ~name ~description () : string * Yojson.Safe.t =
  ( name
  , `Assoc [ ("type", `String "string"); ("description", `String description) ]
  )

(** [make_number_param ~name ~description ()] creates a JSON Schema
    property object of type [number]. *)
let make_number_param ~name ~description () : string * Yojson.Safe.t =
  ( name
  , `Assoc [ ("type", `String "number"); ("description", `String description) ]
  )

(** [make_bool_param ~name ~description ()] creates a JSON Schema
    property object of type [boolean]. *)
let make_bool_param ~name ~description () : string * Yojson.Safe.t =
  ( name
  , `Assoc [ ("type", `String "boolean"); ("description", `String description) ]
  )

(** [make_object_schema ~properties ~required ()] builds a complete JSON
    Schema object suitable for passing as [?parameters] to
    {!define_tool}. *)
let make_object_schema ~properties ~required () : Yojson.Safe.t =
  `Assoc
    [ ("type", `String "object")
    ; ("properties", `Assoc properties)
    ; ("required", `List (List.map (fun s -> `String s) required))
    ]

(** [register_tools session tool_pairs] registers a list of
    [(definition, handler)] pairs on the given session. *)
let register_tools (session : Session.t) (tools : (tool_definition * Session.tool_handler) list)
    : unit =
  List.iter
    (fun (defn, handler) ->
      Session.register_tool session defn.tool_name handler)
    tools

(** [approve_all] is a convenience permission handler that approves
    every incoming permission request. Useful for development and
    testing. *)
let approve_all : Session.permission_handler =
  fun _request _session_id ->
    Lwt.return Types.{ decision = Approved; rules = None }
