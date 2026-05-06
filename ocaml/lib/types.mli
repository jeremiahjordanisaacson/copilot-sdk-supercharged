(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Core types for the Copilot SDK.

    Records, variants, and conversion functions used throughout the SDK. *)

(** {1 Connection State} *)

type connection_state =
  | Disconnected
  | Connecting
  | Connected
  | Error

val connection_state_to_string : connection_state -> string

(** {1 Tool Types} *)

type tool_result_type =
  | Success
  | Failure
  | Rejected
  | Denied
  | Timeout

val tool_result_type_to_string : tool_result_type -> string
val tool_result_type_of_string : string -> tool_result_type

type tool_result = {
  text_result : string;
  result_type : tool_result_type;
  error : string option;
  session_log : string option;
}

val make_tool_result :
  ?result_type:tool_result_type ->
  ?error:string ->
  ?session_log:string ->
  string ->
  tool_result

val tool_result_to_yojson : tool_result -> Yojson.Safe.t

type tool_invocation = {
  session_id : string;
  tool_call_id : string;
  tool_name : string;
  arguments : Yojson.Safe.t;
}

val tool_invocation_of_yojson : Yojson.Safe.t -> (tool_invocation, string) result

(** {1 Session Filesystem Configuration} *)

type session_fs_config = {
  initial_cwd : string;
  session_state_path : string;
  conventions : string;
}

val default_session_fs_config : unit -> session_fs_config
val session_fs_config_to_yojson : session_fs_config -> Yojson.Safe.t

(** {1 MCP Server Configuration} *)

type mcp_server_type = Stdio | Http

val mcp_server_type_to_string : mcp_server_type -> string

type mcp_server_config = {
  mcp_type : mcp_server_type;
  command : string option;
  args : string list;
  url : string option;
  env : (string * string) list;
  headers : (string * string) list;
}

val mcp_server_config_to_yojson : mcp_server_config -> Yojson.Safe.t

(** {1 Command Definition} *)

type command_definition = {
  cmd_name : string;
  cmd_description : string;
}

val command_definition_to_yojson : command_definition -> Yojson.Safe.t

(** {1 Image Response Format} *)

type image_response_format = FormatText | FormatImage | FormatJsonObject

val image_response_format_to_string : image_response_format -> string

(** {1 Session Configuration} *)

type reasoning_effort =
  | Low
  | Medium
  | High
  | Xhigh

val reasoning_effort_to_string : reasoning_effort -> string

type message_options = {
  prompt : string;
  mode : string option;
}

val message_options_to_yojson : message_options -> Yojson.Safe.t

type tool_definition = {
  tool_name : string;
  tool_description : string;
  tool_parameters : Yojson.Safe.t option;
}

type session_config = {
  model : string option;
  system_prompt : string option;
  reasoning_effort : reasoning_effort option;
  streaming : bool;
  tools : tool_definition list;
  excluded_tools : string list;
  mcp_servers : (string * mcp_server_config) list;
  model_capabilities : (string * Yojson.Safe.t) list;
  enable_config_discovery : bool;
  include_sub_agent_streaming_events : bool;
  commands : command_definition list;
  skill_directories : string list;
  disabled_skills : string list;
  working_directory : string option;
  github_token : string option;
  response_format : image_response_format option;
  request_headers : (string * string) list;
  on_elicitation_request : bool;
  instruction_directories : string list;
}

val default_session_config : unit -> session_config
val session_config_to_yojson : session_config -> Yojson.Safe.t
val tool_definition_to_yojson : tool_definition -> Yojson.Safe.t

(** {1 Session Events} *)

type session_event_type =
  | AssistantMessage
  | AssistantMessageDelta
  | AssistantReasoning
  | AssistantReasoningDelta
  | SessionIdle
  | SessionError
  | PermissionRequested
  | ToolCalled
  | Unknown of string

val session_event_type_of_string : string -> session_event_type

type session_event = {
  event_type : session_event_type;
  id : string;
  timestamp : string;
  parent_id : string option;
  agent_id : string option;
  ephemeral : bool option;
  data : Yojson.Safe.t;
}

val session_event_of_yojson : Yojson.Safe.t -> (session_event, string) result

(** {1 Permission Types} *)

type permission_decision =
  | Approved
  | DeniedByUser
  | DeniedByPolicy

val permission_decision_to_string : permission_decision -> string

type permission_result = {
  decision : permission_decision;
  rules : Yojson.Safe.t option;
}

val permission_result_to_yojson : permission_result -> Yojson.Safe.t

(** {1 Status Types} *)

type get_status_response = {
  protocol_version : int;
  server_version : string option;
}

val get_status_response_of_yojson : Yojson.Safe.t -> (get_status_response, string) result

type model_info = {
  model_id : string;
  model_name : string option;
}

val model_info_of_yojson : Yojson.Safe.t -> (model_info, string) result

(** {1 Client Options} *)

type client_options = {
  cli_path : string option;
  cli_url : string option;
  log_level : string option;
  github_token : string option;
  use_logged_in_user : bool option;
  session_idle_timeout_seconds : int option;
  session_fs : session_fs_config option;
  copilot_home : string option;
  tcp_connection_token : string option;
}

val default_client_options : unit -> client_options

(** {1 Helper Constructors} *)

val make_message : ?mode:string -> string -> message_options
val make_tool_definition : ?parameters:Yojson.Safe.t -> string -> string -> tool_definition
