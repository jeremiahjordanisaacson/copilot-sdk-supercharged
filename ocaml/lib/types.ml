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
  instruction_directories : string list;
  enable_session_telemetry : bool;
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
  ; enable_session_telemetry = false
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
    if c.enable_session_telemetry then
      ("enableSessionTelemetry", `Bool true) :: fields
    else fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Slash Command Types                                                        *)
(* ========================================================================== *)

type slash_command_input_completion = SCICDirectory

let slash_command_input_completion_to_string = function
  | SCICDirectory -> "directory"

let slash_command_input_completion_of_string = function
  | "directory" -> SCICDirectory
  | _ -> SCICDirectory

type slash_command_kind = SCKBuiltin | SCKClient | SCKSkill

let slash_command_kind_to_string = function
  | SCKBuiltin -> "builtin"
  | SCKClient -> "client"
  | SCKSkill -> "skill"

let slash_command_kind_of_string = function
  | "builtin" -> SCKBuiltin
  | "client" -> SCKClient
  | "skill" -> SCKSkill
  | _ -> SCKBuiltin

type model_picker_price_category = MPPCHigh | MPPCLow | MPPCMedium | MPPCVeryHigh

let model_picker_price_category_to_string = function
  | MPPCHigh -> "high"
  | MPPCLow -> "low"
  | MPPCMedium -> "medium"
  | MPPCVeryHigh -> "very_high"

let model_picker_price_category_of_string = function
  | "high" -> MPPCHigh
  | "low" -> MPPCLow
  | "medium" -> MPPCMedium
  | "very_high" -> MPPCVeryHigh
  | _ -> MPPCMedium

type slash_command_input = {
  sci_hint : string;
  sci_completion : slash_command_input_completion option;
}

let slash_command_input_of_yojson (json : Yojson.Safe.t)
    : (slash_command_input, string) result =
  try
    Ok
      { sci_hint = json |> member "hint" |> to_string
      ; sci_completion =
          (try Some (slash_command_input_completion_of_string
                       (json |> member "completion" |> to_string))
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let slash_command_input_to_yojson (s : slash_command_input) : Yojson.Safe.t =
  let fields = [ ("hint", `String s.sci_hint) ] in
  let fields =
    match s.sci_completion with
    | Some c -> ("completion", `String (slash_command_input_completion_to_string c)) :: fields
    | None -> fields
  in
  `Assoc fields

type slash_command_info = {
  sc_allow_during_agent_execution : bool;
  sc_description : string;
  sc_kind : slash_command_kind;
  sc_name : string;
  sc_aliases : string list option;
  sc_experimental : bool option;
  sc_input : slash_command_input option;
}

let slash_command_info_of_yojson (json : Yojson.Safe.t)
    : (slash_command_info, string) result =
  try
    Ok
      { sc_allow_during_agent_execution =
          json |> member "allowDuringAgentExecution" |> to_bool
      ; sc_description = json |> member "description" |> to_string
      ; sc_kind =
          slash_command_kind_of_string (json |> member "kind" |> to_string)
      ; sc_name = json |> member "name" |> to_string
      ; sc_aliases =
          (try Some (json |> member "aliases" |> to_list |> List.map to_string)
           with _ -> None)
      ; sc_experimental =
          (try Some (json |> member "experimental" |> to_bool)
           with _ -> None)
      ; sc_input =
          (try
             match slash_command_input_of_yojson (json |> member "input") with
             | Ok v -> Some v
             | Error _ -> None
           with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let slash_command_info_to_yojson (s : slash_command_info) : Yojson.Safe.t =
  let fields =
    [ ("allowDuringAgentExecution", `Bool s.sc_allow_during_agent_execution)
    ; ("description", `String s.sc_description)
    ; ("kind", `String (slash_command_kind_to_string s.sc_kind))
    ; ("name", `String s.sc_name)
    ]
  in
  let fields =
    match s.sc_aliases with
    | Some a -> ("aliases", `List (List.map (fun s -> `String s) a)) :: fields
    | None -> fields
  in
  let fields =
    match s.sc_experimental with
    | Some e -> ("experimental", `Bool e) :: fields
    | None -> fields
  in
  let fields =
    match s.sc_input with
    | Some i -> ("input", slash_command_input_to_yojson i) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Command Request Types                                                      *)
(* ========================================================================== *)

type commands_invoke_request = {
  cir_name : string;
  cir_input : string option;
}

let commands_invoke_request_of_yojson (json : Yojson.Safe.t)
    : (commands_invoke_request, string) result =
  try
    Ok
      { cir_name = json |> member "name" |> to_string
      ; cir_input =
          (try Some (json |> member "input" |> to_string) with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let commands_invoke_request_to_yojson (r : commands_invoke_request) : Yojson.Safe.t =
  let fields = [ ("name", `String r.cir_name) ] in
  let fields =
    match r.cir_input with
    | Some i -> ("input", `String i) :: fields
    | None -> fields
  in
  `Assoc fields

type commands_list_request = {
  clr_include_builtins : bool option;
  clr_include_client_commands : bool option;
  clr_include_skills : bool option;
}

let commands_list_request_of_yojson (json : Yojson.Safe.t)
    : (commands_list_request, string) result =
  try
    Ok
      { clr_include_builtins =
          (try Some (json |> member "includeBuiltins" |> to_bool) with _ -> None)
      ; clr_include_client_commands =
          (try Some (json |> member "includeClientCommands" |> to_bool) with _ -> None)
      ; clr_include_skills =
          (try Some (json |> member "includeSkills" |> to_bool) with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let commands_list_request_to_yojson (r : commands_list_request) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match r.clr_include_builtins with
    | Some b -> ("includeBuiltins", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match r.clr_include_client_commands with
    | Some b -> ("includeClientCommands", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match r.clr_include_skills with
    | Some b -> ("includeSkills", `Bool b) :: fields
    | None -> fields
  in
  `Assoc fields

(* ========================================================================== *)
(* Model Billing Types                                                        *)
(* ========================================================================== *)

type model_billing_token_prices = {
  mbtp_batch_size : int option;
  mbtp_cache_price : int option;
  mbtp_input_price : int option;
  mbtp_output_price : int option;
}

let model_billing_token_prices_of_yojson (json : Yojson.Safe.t)
    : (model_billing_token_prices, string) result =
  try
    Ok
      { mbtp_batch_size =
          (try Some (json |> member "batchSize" |> to_int) with _ -> None)
      ; mbtp_cache_price =
          (try Some (json |> member "cachePrice" |> to_int) with _ -> None)
      ; mbtp_input_price =
          (try Some (json |> member "inputPrice" |> to_int) with _ -> None)
      ; mbtp_output_price =
          (try Some (json |> member "outputPrice" |> to_int) with _ -> None)
      }
  with exn -> Error (Printexc.to_string exn)

let model_billing_token_prices_to_yojson (p : model_billing_token_prices) : Yojson.Safe.t =
  let fields = [] in
  let fields =
    match p.mbtp_batch_size with
    | Some v -> ("batchSize", `Int v) :: fields
    | None -> fields
  in
  let fields =
    match p.mbtp_cache_price with
    | Some v -> ("cachePrice", `Int v) :: fields
    | None -> fields
  in
  let fields =
    match p.mbtp_input_price with
    | Some v -> ("inputPrice", `Int v) :: fields
    | None -> fields
  in
  let fields =
    match p.mbtp_output_price with
    | Some v -> ("outputPrice", `Int v) :: fields
    | None -> fields
  in
  `Assoc fields

(* Experimental *)
(* Diagnostics from loading skills. *)

type skills_load_diagnostics = {
  sld_errors : string list;
  sld_warnings : string list;
}

let skills_load_diagnostics_of_yojson (json : Yojson.Safe.t)
    : (skills_load_diagnostics, string) result =
  try
    Ok
      { sld_errors = json |> member "errors" |> to_list |> List.map to_string
      ; sld_warnings = json |> member "warnings" |> to_list |> List.map to_string
      }
  with exn -> Error (Printexc.to_string exn)

let skills_load_diagnostics_to_yojson (d : skills_load_diagnostics) : Yojson.Safe.t =
  `Assoc
    [ ("errors", `List (List.map (fun s -> `String s) d.sld_errors))
    ; ("warnings", `List (List.map (fun s -> `String s) d.sld_warnings))
    ]

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
(* Exit Plan Mode                                                             *)
(* ========================================================================== *)

type exit_plan_mode_request = {
  epm_session_id : string;
}

let exit_plan_mode_request_of_yojson (json : Yojson.Safe.t)
    : (exit_plan_mode_request, string) result =
  try
    Ok { epm_session_id = json |> member "sessionId" |> to_string }
  with exn -> Error (Printexc.to_string exn)

type exit_plan_mode_response = {
  epm_approved : bool;
}

let exit_plan_mode_response_to_yojson (r : exit_plan_mode_response) : Yojson.Safe.t =
  `Assoc [ ("approved", `Bool r.epm_approved) ]

let default_exit_plan_mode_response () = { epm_approved = true }

(* ========================================================================== *)
(* Trace Context                                                              *)
(* ========================================================================== *)

type trace_context = {
  traceparent : string option;
  tracestate : string option;
}

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
  remote : bool option;
  on_get_trace_context : (unit -> trace_context) option;
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
  ; remote = None
  ; on_get_trace_context = None
  }

(* ========================================================================== *)
(* Helper Constructors                                                        *)
(* ========================================================================== *)

let make_message ?mode prompt = { prompt; mode }

let make_tool_definition ?parameters name description =
  { tool_name = name; tool_description = description; tool_parameters = parameters }
