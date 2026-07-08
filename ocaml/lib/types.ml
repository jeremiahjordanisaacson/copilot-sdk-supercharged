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
  agent_mode : string option;
  display_prompt : string option;
  request_headers : (string * string) list;
}

let message_options_to_yojson (m : message_options) : Yojson.Safe.t =
  let fields = [ ("prompt", `String m.prompt) ] in
  let fields =
    match m.mode with
    | Some v -> ("mode", `String v) :: fields
    | None -> fields
  in
  let fields =
    match m.agent_mode with
    | Some v -> ("agentMode", `String v) :: fields
    | None -> fields
  in
  let fields =
    match m.display_prompt with
    | Some v -> ("displayPrompt", `String v) :: fields
    | None -> fields
  in
  let fields =
    match m.request_headers with
    | [] -> fields
    | hdrs ->
      ("requestHeaders", `Assoc (List.map (fun (k, v) -> (k, `String v)) hdrs))
      :: fields
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

(* ========================================================================== *)
(* Upstream-sync Feature Types & Constants                                    *)
(* ========================================================================== *)

(** Per-session AI-credit budget; set [max_ai_credits] to cap spend. *)
type session_limits_config = { max_ai_credits : float option }

let session_limits_config_to_yojson (s : session_limits_config) : Yojson.Safe.t =
  match s.max_ai_credits with
  | Some c -> `Assoc [ ("maxAiCredits", `Float c) ]
  | None -> `Assoc []

(** Opt-in persistent session memory. *)
type memory_configuration = { memory_enabled : bool }

let memory_configuration_to_yojson (m : memory_configuration) : Yojson.Safe.t =
  `Assoc [ ("enabled", `Bool m.memory_enabled) ]

(** Arguments passed to a BYOK bearer-token provider (per-session scoping). *)
type provider_token_args = { pta_session_id : string }

let provider_token_args_session_id (a : provider_token_args) : string =
  a.pta_session_id

(** BYOK bearer-token provider: mints a fresh bearer token per session. *)
type bearer_token_provider = provider_token_args -> string

(** Intercepts outbound LLM inference HTTP/WebSocket requests. Receives the
    request and a context value and returns the (possibly mutated) request. *)
type copilot_request_handler = Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t

(** Tool "defer" loading policy: lazy via search ([DeferAuto]) or eager
    pre-load ([DeferNever]). *)
type tool_defer = DeferAuto | DeferNever

let tool_defer_to_string = function
  | DeferAuto -> "auto"
  | DeferNever -> "never"

(** System-message section identifiers. The [preamble] section targets only the
    identity preamble; [preserve] protects an addressable section from removal. *)
let system_message_section_preamble = "preamble"
let system_message_section_identity = "identity"
let system_message_section_tool_instructions = "tool_instructions"
let system_message_section_preserve = "preserve"

(** GitHub-anchored attachment variants. *)
let attachment_github_commit = "GitHubCommit"
let attachment_github_release = "GitHubRelease"
let attachment_github_actions_job = "GitHubActionsJob"
let attachment_github_repository = "GitHubRepository"
let attachment_github_file_diff = "GitHubFileDiff"
let attachment_github_tree_comparison = "GitHubTreeComparison"
let attachment_github_url = "GitHubUrl"
let attachment_github_file = "GitHubFile"
let attachment_github_snippet = "GitHubSnippet"

(** Hook names (include [on_post_tool_use] and [on_pre_mcp_tool_call]). *)
let hook_on_pre_tool_use = "onPreToolUse"
let hook_on_post_tool_use = "onPostToolUse"
let hook_on_pre_mcp_tool_call = "onPreMcpToolCall"

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
  (* Upstream-sync session options (parity with @github/copilot-sdk) *)
  enable_citations : bool;
  excluded_builtin_agents : string list;
  session_limits : session_limits_config option;
  memory : memory_configuration option;
  otlp_protocol : string option;
  enable_web_socket_responses : bool;
  exp_assignments : (string * Yojson.Safe.t) list;
  on_mcp_auth_request : bool;
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
  ; instruction_directories = []
  ; enable_citations = false
  ; excluded_builtin_agents = []
  ; session_limits = None
  ; memory = None
  ; otlp_protocol = None
  ; enable_web_socket_responses = false
  ; exp_assignments = []
  ; on_mcp_auth_request = false
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
  let fields =
    match c.instruction_directories with
    | [] -> fields
    | dirs -> ("instructionDirectories", `List (List.map (fun s -> `String s) dirs)) :: fields
  in
  let fields =
    if c.enable_citations then ("enableCitations", `Bool true) :: fields
    else fields
  in
  let fields =
    match c.excluded_builtin_agents with
    | [] -> fields
    | agents ->
      ("excludedBuiltinAgents", `List (List.map (fun s -> `String s) agents)) :: fields
  in
  let fields =
    match c.session_limits with
    | Some sl -> ("sessionLimits", session_limits_config_to_yojson sl) :: fields
    | None -> fields
  in
  let fields =
    match c.memory with
    | Some mem -> ("memory", memory_configuration_to_yojson mem) :: fields
    | None -> fields
  in
  let fields =
    match c.otlp_protocol with
    | Some p -> ("otlpProtocol", `String p) :: fields
    | None -> fields
  in
  let fields =
    if c.enable_web_socket_responses then
      ("enableWebSocketResponses", `Bool true) :: fields
    else fields
  in
  let fields =
    match c.exp_assignments with
    | [] -> fields
    | assigns -> ("expAssignments", `Assoc assigns) :: fields
  in
  let fields =
    if c.on_mcp_auth_request then ("mcpAuthHandler", `Bool true) :: fields
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
  id : string;
  timestamp : string;
  parent_id : string option;
  agent_id : string option;
  ephemeral : bool option;
  data : Yojson.Safe.t;
}

let session_event_of_yojson (json : Yojson.Safe.t) : (session_event, string) result =
  try
    let etype = json |> member "type" |> to_string in
    let data = try json |> member "data" with _ -> `Null in
    let id = try json |> member "id" |> to_string with _ -> "" in
    let timestamp = try json |> member "timestamp" |> to_string with _ -> "" in
    let parent_id = try Some (json |> member "parentId" |> to_string) with _ -> None in
    let agent_id = try Some (json |> member "agentId" |> to_string) with _ -> None in
    let ephemeral = try Some (json |> member "ephemeral" |> to_bool) with _ -> None in
    Ok { event_type = session_event_type_of_string etype; id; timestamp; parent_id; agent_id; ephemeral; data }
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
  copilot_home : string option;
  tcp_connection_token : string option;
  request_handler : copilot_request_handler option;
  bearer_token_provider : bearer_token_provider option;
}

let default_client_options () =
  { cli_path = None
  ; cli_url = None
  ; log_level = None
  ; github_token = None
  ; use_logged_in_user = None
  ; session_idle_timeout_seconds = None
  ; session_fs = None
  ; copilot_home = None
  ; tcp_connection_token = None
  ; request_handler = None
  ; bearer_token_provider = None
  }

(* ========================================================================== *)
(* Helper Constructors                                                        *)
(* ========================================================================== *)

let make_message ?mode ?agent_mode ?display_prompt ?(request_headers = []) prompt =
  { prompt; mode; agent_mode; display_prompt; request_headers }

let make_tool_definition ?parameters name description =
  { tool_name = name; tool_description = description; tool_parameters = parameters }
