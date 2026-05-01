(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** Core types for the Copilot SDK. *)

open Yojson.Safe.Util

(* ========================================================================== *)
(* Connection State                                                           *)
(* ========================================================================== *)

type connection_state =
  | Disconnected
  | Connecting
  | Connected
  | Error

let connection_state_to_string = function
  | Disconnected -> "disconnected"
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Error -> "error"

(* ========================================================================== *)
(* Tool Types                                                                 *)
(* ========================================================================== *)

type tool_result_type =
  | Success
  | Failure
  | Rejected
  | Denied
  | Timeout

let tool_result_type_to_string = function
  | Success -> "success"
  | Failure -> "failure"
  | Rejected -> "rejected"
  | Denied -> "denied"
  | Timeout -> "timeout"

let tool_result_type_of_string = function
  | "success" -> Success
  | "failure" -> Failure
  | "rejected" -> Rejected
  | "denied" -> Denied
  | "timeout" -> Timeout
  | _ -> Failure

type tool_result = {
  text_result : string;
  result_type : tool_result_type;
  error : string option;
  session_log : string option;
}

let make_tool_result ?(result_type = Success) ?error ?session_log text =
  { text_result = text; result_type; error; session_log }

let tool_result_to_yojson (r : tool_result) : Yojson.Safe.t =
  let fields =
    [ ("textResultForLlm", `String r.text_result)
    ; ("resultType", `String (tool_result_type_to_string r.result_type))
    ]
  in
  let fields =
    match r.error with
    | Some e -> ("error", `String e) :: fields
    | None -> fields
  in
  let fields =
    match r.session_log with
    | Some s -> ("sessionLog", `String s) :: fields
    | None -> fields
  in
  `Assoc fields

type tool_invocation = {
  session_id : string;
  tool_call_id : string;
  tool_name : string;
  arguments : Yojson.Safe.t;
}

let tool_invocation_of_yojson (json : Yojson.Safe.t)
    : (tool_invocation, string) result =
  try
    Ok
      { session_id = json |> member "sessionId" |> to_string
      ; tool_call_id = json |> member "toolCallId" |> to_string
      ; tool_name = json |> member "toolName" |> to_string
      ; arguments =
          (try json |> member "arguments" with _ -> `Null)
      }
  with exn -> Error (Printexc.to_string exn)

(* ========================================================================== *)
(* Session Filesystem Configuration                                           *)
(* ========================================================================== *)

type session_fs_config = {
  initial_cwd : string;
  session_state_path : string;
  conventions : string;  (** "windows" or "posix" *)
}

let default_session_fs_config () =
  { initial_cwd = ""; session_state_path = ""; conventions = "posix" }

let session_fs_config_to_yojson (c : session_fs_config) : Yojson.Safe.t =
  `Assoc
    [ ("initialCwd", `String c.initial_cwd)
    ; ("sessionStatePath", `String c.session_state_path)
    ; ("conventions", `String c.conventions)
    ]

(* ========================================================================== *)
(* MCP Server Configuration                                                   *)
(* ========================================================================== *)

type mcp_server_type = Stdio | Http

let mcp_server_type_to_string = function
  | Stdio -> "stdio"
  | Http -> "http"

type mcp_server_config = {
  mcp_type : mcp_server_type;
  command : string option;
  args : string list;
  url : string option;
  env : (string * string) list;
  headers : (string * string) list;
}

let mcp_server_config_to_yojson (c : mcp_server_config) : Yojson.Safe.t =
  let fields =
    [ ("type", `String (mcp_server_type_to_string c.mcp_type)) ]
  in
  let fields =
    match c.command with
    | Some cmd -> ("command", `String cmd) :: fields
    | None -> fields
  in
  let fields =
    match c.args with
    | [] -> fields
    | args -> ("args", `List (List.map (fun s -> `String s) args)) :: fields
  in
  let fields =
    match c.url with
    | Some u -> ("url", `String u) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Command Definition                                                         *)
(* ========================================================================== *)

type command_definition = {
  cmd_name : string;
  cmd_description : string;
}

let command_definition_to_yojson (c : command_definition) : Yojson.Safe.t =
  `Assoc
    [ ("name", `String c.cmd_name)
    ; ("description", `String c.cmd_description)
    ]

(* ========================================================================== *)
(* Image Response Format                                                      *)
(* ========================================================================== *)

type image_response_format = FormatText | FormatImage | FormatJsonObject

let image_response_format_to_string = function
  | FormatText -> "text"
  | FormatImage -> "image"
  | FormatJsonObject -> "json_object"

(* ========================================================================== *)
(* Session Configuration                                                      *)
(* ========================================================================== *)

type reasoning_effort =
  | Low
  | Medium
  | High
  | Xhigh

let reasoning_effort_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Xhigh -> "xhigh"

type message_options = {
  prompt : string;
  mode : string option;
}

let message_options_to_yojson (m : message_options) : Yojson.Safe.t =
  let fields = [ ("prompt", `String m.prompt) ] in
  let fields =
    match m.mode with
    | Some v -> ("mode", `String v) :: fields
    | None -> fields
  in
  `Assoc fields

type tool_definition = {
  tool_name : string;
  tool_description : string;
  tool_parameters : Yojson.Safe.t option;
}

let tool_definition_to_yojson (td : tool_definition) : Yojson.Safe.t =
  let fields =
    [ ("name", `String td.tool_name)
    ; ("description", `String td.tool_description)
    ]
  in
  let fields =
    match td.tool_parameters with
    | Some p -> ("parameters", p) :: fields
    | None -> fields
  in
  `Assoc fields

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
}

let default_session_config () =
  { model = None
  ; system_prompt = None
  ; reasoning_effort = None
  ; streaming = true
  ; tools = []
  ; excluded_tools = []
  ; mcp_servers = []
  ; model_capabilities = []
  ; enable_config_discovery = false
  ; include_sub_agent_streaming_events = false
  ; commands = []
  ; skill_directories = []
  ; disabled_skills = []
  ; working_directory = None
  ; github_token = None
  ; response_format = None
  ; request_headers = []
  ; on_elicitation_request = false
  }

let session_config_to_yojson (c : session_config) : Yojson.Safe.t =
  let fields = [ ("streaming", `Bool c.streaming) ] in
  let fields =
    match c.model with
    | Some m -> ("model", `String m) :: fields
    | None -> fields
  in
  let fields =
    match c.system_prompt with
    | Some p -> ("systemPrompt", `String p) :: fields
    | None -> fields
  in
  let fields =
    match c.reasoning_effort with
    | Some e ->
      ("reasoningEffort", `String (reasoning_effort_to_string e)) :: fields
    | None -> fields
  in
  let fields =
    match c.tools with
    | [] -> fields
    | ts ->
      let tools_json = `List (List.map tool_definition_to_yojson ts) in
      ("tools", tools_json) :: fields
  in
  let fields =
    match c.excluded_tools with
    | [] -> fields
    | et -> ("excludedTools", `List (List.map (fun s -> `String s) et)) :: fields
  in
  let fields =
    match c.mcp_servers with
    | [] -> fields
    | servers ->
      let assocs = List.map (fun (name, cfg) -> (name, mcp_server_config_to_yojson cfg)) servers in
      ("mcpServers", `Assoc assocs) :: fields
  in
  let fields =
    match c.model_capabilities with
    | [] -> fields
    | caps -> ("modelCapabilities", `Assoc caps) :: fields
  in
  let fields =
    if c.enable_config_discovery then
      ("enableConfigDiscovery", `Bool true) :: fields
    else fields
  in
  let fields =
    if c.include_sub_agent_streaming_events then
      ("includeSubAgentStreamingEvents", `Bool true) :: fields
    else fields
  in
  let fields =
    match c.commands with
    | [] -> fields
    | cmds ->
      ("commands", `List (List.map command_definition_to_yojson cmds)) :: fields
  in
  let fields =
    match c.skill_directories with
    | [] -> fields
    | dirs -> ("skillDirectories", `List (List.map (fun s -> `String s) dirs)) :: fields
  in
  let fields =
    match c.disabled_skills with
    | [] -> fields
    | sk -> ("disabledSkills", `List (List.map (fun s -> `String s) sk)) :: fields
  in
  let fields =
    match c.working_directory with
    | Some d -> ("workingDirectory", `String d) :: fields
    | None -> fields
  in
  let fields =
    match c.github_token with
    | Some t -> ("gitHubToken", `String t) :: fields
    | None -> fields
  in
  let fields =
    match c.response_format with
    | Some f -> ("responseFormat", `String (image_response_format_to_string f)) :: fields
    | None -> fields
  in
  let fields =
    match c.request_headers with
    | [] -> fields
    | hdrs ->
      ("requestHeaders",
       `Assoc (List.map (fun (k, v) -> (k, `String v)) hdrs))
      :: fields
  in
  let fields =
    if c.on_elicitation_request then
      ("requestElicitation", `Bool true) :: fields
    else fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Session Events                                                             *)
(* ========================================================================== *)

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

let session_event_type_of_string = function
  | "assistant.message" -> AssistantMessage
  | "assistant.message_delta" -> AssistantMessageDelta
  | "assistant.reasoning" -> AssistantReasoning
  | "assistant.reasoning_delta" -> AssistantReasoningDelta
  | "session.idle" -> SessionIdle
  | "session.error" -> SessionError
  | "permission.requested" -> PermissionRequested
  | "tool.called" -> ToolCalled
  | s -> Unknown s

type session_event = {
  event_type : session_event_type;
  data : Yojson.Safe.t;
}

let session_event_of_yojson (json : Yojson.Safe.t) : (session_event, string) result =
  try
    let etype = json |> member "type" |> to_string in
    let data = try json |> member "data" with _ -> `Null in
    Ok { event_type = session_event_type_of_string etype; data }
  with exn -> Error (Printexc.to_string exn)

(* ========================================================================== *)
(* Permission Types                                                           *)
(* ========================================================================== *)

type permission_decision =
  | Approved
  | DeniedByUser
  | DeniedByPolicy

let permission_decision_to_string = function
  | Approved -> "approved"
  | DeniedByUser -> "deniedInteractivelyByUser"
  | DeniedByPolicy -> "deniedByPolicy"

type permission_result = {
  decision : permission_decision;
  rules : Yojson.Safe.t option;
}

let permission_result_to_yojson (r : permission_result) : Yojson.Safe.t =
  let fields =
    [ ("kind", `String (permission_decision_to_string r.decision)) ]
  in
  let fields =
    match r.rules with
    | Some v -> ("rules", v) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Status Types                                                               *)
(* ========================================================================== *)

type get_status_response = {
  protocol_version : int;
  server_version : string option;
}

let get_status_response_of_yojson (json : Yojson.Safe.t)
    : (get_status_response, string) result =
  try
    Ok
      { protocol_version = json |> member "protocolVersion" |> to_int
      ; server_version =
          (try Some (json |> member "serverVersion" |> to_string)
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

type model_info = {
  model_id : string;
  model_name : string option;
}

let model_info_of_yojson (json : Yojson.Safe.t) : (model_info, string) result =
  try
    Ok
      { model_id = json |> member "modelId" |> to_string
      ; model_name =
          (try Some (json |> member "modelName" |> to_string)
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

(* ========================================================================== *)
(* Client Options                                                             *)
(* ========================================================================== *)

type client_options = {
  cli_path : string option;
  cli_url : string option;
  log_level : string option;
  github_token : string option;
  use_logged_in_user : bool option;
  session_idle_timeout_seconds : int option;
  session_fs : session_fs_config option;
}

let default_client_options () =
  { cli_path = None
  ; cli_url = None
  ; log_level = None
  ; github_token = None
  ; use_logged_in_user = None
  ; session_idle_timeout_seconds = None
  ; session_fs = None
  }

(* ========================================================================== *)
(* Helper Constructors                                                        *)
(* ========================================================================== *)

let make_message ?mode prompt = { prompt; mode }

let make_tool_definition ?parameters name description =
  { tool_name = name; tool_description = description; tool_parameters = parameters }
