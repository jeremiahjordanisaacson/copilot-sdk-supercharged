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
  args_schema : Yojson.Safe.t option;
}

let command_definition_to_yojson (c : command_definition) : Yojson.Safe.t =
  let fields =
    [ ("name", `String c.cmd_name)
    ; ("description", `String c.cmd_description)
    ]
  in
  let fields =
    match c.args_schema with
    | Some s -> ("argsSchema", s) :: fields
    | None -> fields
  in
  `Assoc fields

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
  (* Message origin: "user", "system", or "agent-<id>" (JSON key "source"). *)
  source : string option;
  (* JSON Schema for structured output, sent as a json_schema "responseFormat". *)
  response_schema : Yojson.Safe.t option;
  (* Opaque caller-chosen turn identifier (JSON key "clientTurnId"). *)
  client_turn_id : string option;
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
  let fields =
    match m.source with
    | Some v -> ("source", `String v) :: fields
    | None -> fields
  in
  let fields =
    match m.response_schema with
    | Some schema ->
      ( "responseFormat"
      , `Assoc
          [ ("type", `String "json_schema")
          ; ( "jsonSchema"
            , `Assoc
                [ ("name", `String "response")
                ; ("strict", `Bool true)
                ; ("schema", schema)
                ] )
          ] )
      :: fields
    | None -> fields
  in
  let fields =
    match m.client_turn_id with
    | Some v -> ("clientTurnId", `String v) :: fields
    | None -> fields
  in
  `Assoc fields

type tool_definition = {
  tool_name : string;
  tool_description : string;
  tool_parameters : Yojson.Safe.t option;
  (* Safe to repeat with the same arguments (JSON key "idempotent"). *)
  tool_idempotent : bool option;
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
  let fields =
    match td.tool_idempotent with
    | Some b -> ("idempotent", `Bool b) :: fields
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
let hook_on_user_prompt_transformed = "userPromptTransformed"

(* ========================================================================== *)
(* v2.6.0 / v3.0.0 Feature Types (SDK protocol 4)                             *)
(* ========================================================================== *)

(* Small builders for optional JSON members (fields are accumulated in
   reverse and reversed by the caller). *)
let add_bool key value fields =
  match value with Some b -> (key, `Bool b) :: fields | None -> fields

let add_int key value fields =
  match value with Some n -> (key, `Int n) :: fields | None -> fields

let add_string key value fields =
  match value with Some s -> (key, `String s) :: fields | None -> fields

let add_json key value fields =
  match value with Some j -> (key, j) :: fields | None -> fields

let string_list items = `List (List.map (fun s -> `String s) items)

let json_field (json : Yojson.Safe.t) (key : string) : Yojson.Safe.t option =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt key fields with Some `Null -> None | value -> value)
  | _ -> None

let json_string_field (json : Yojson.Safe.t) (key : string) : string option =
  match json_field json key with Some (`String s) -> Some s | _ -> None

(** Identity of the application embedding the SDK, sent with [connect]. *)
type client_info = {
  editor_name : string option;
  editor_version : string option;
  extension_name : string option;
  extension_version : string option;
}

let client_info_to_yojson (c : client_info) : Yojson.Safe.t option =
  let add key value fields =
    match value with
    | Some v when v <> "" -> (key, `String v) :: fields
    | _ -> fields
  in
  let fields =
    []
    |> add "extensionVersion" c.extension_version
    |> add "extensionName" c.extension_name
    |> add "editorVersion" c.editor_version
    |> add "editorName" c.editor_name
  in
  match fields with
  | [] -> None
  | _ -> Some (`Assoc fields)

(** Auto routing tiers (experimental). *)
let auto_tier_fast = "fast"
let auto_tier_efficiency = "efficiency"
let auto_tier_balance = "balance"
let auto_tier_intelligence = "intelligence"

(** Ask-user tool variants. *)
let ask_user_variant_legacy = "legacy"
let ask_user_variant_elicitation = "elicitation"

(** Working-directory trust scopes (create only). *)
let working_directory_trust_session = "session"
let working_directory_trust_user = "user"

(** Message sources for [session.send]. *)
let message_source_user = "user"
let message_source_system = "system"
let message_source_agent agent_id = "agent-" ^ agent_id

(** A [gitHubToken.getToken] request from the runtime. *)
type github_token_request = {
  gtr_registration_id : string;
  gtr_host : string;
  gtr_session_id : string option;
  gtr_reason : string;
}

(** Answer to a [gitHubToken.getToken] request. *)
type github_token_result =
  | Github_token of {
      access_token : string;
      token_type : string option;
      expires_in : int;
    }
  | Github_token_cancelled

let github_token_result_to_yojson (r : github_token_result) : Yojson.Safe.t =
  match r with
  | Github_token { access_token; token_type; expires_in } ->
    `Assoc
      (List.rev
         ([ ("expiresIn", `Int expires_in)
          ; ("accessToken", `String access_token)
          ; ("kind", `String "token")
          ]
          |> add_string "tokenType" token_type))
  | Github_token_cancelled -> `Assoc [ ("kind", `String "cancelled") ]

(** Supplies GitHub tokens for one session (mutually exclusive with
    [github_token]). *)
type github_token_provider = github_token_request -> github_token_result Lwt.t

(** Sandbox configuration (sent as permissions.sandbox). *)
type sandbox_config = {
  sandbox_enabled : bool;
  sandbox_user_policy : Yojson.Safe.t option;
  sandbox_add_current_working_directory : bool option;
  sandbox_mcp_servers : bool option;
  sandbox_lsp_servers : bool option;
  sandbox_extensions : bool option;
  allow_bypass : bool option;
}

let default_sandbox_config () : sandbox_config =
  { sandbox_enabled = true
  ; sandbox_user_policy = None
  ; sandbox_add_current_working_directory = None
  ; sandbox_mcp_servers = None
  ; sandbox_lsp_servers = None
  ; sandbox_extensions = None
  ; allow_bypass = None
  }

let sandbox_config_to_yojson (s : sandbox_config) : Yojson.Safe.t =
  `Assoc
    (List.rev
       ([ ("enabled", `Bool s.sandbox_enabled) ]
        |> add_json "userPolicy" s.sandbox_user_policy
        |> add_bool "addCurrentWorkingDirectory" s.sandbox_add_current_working_directory
        |> add_bool "sandboxMcpServers" s.sandbox_mcp_servers
        |> add_bool "sandboxLspServers" s.sandbox_lsp_servers
        |> add_bool "sandboxExtensions" s.sandbox_extensions
        |> add_bool "allowBypass" s.allow_bypass))

(** Session-level image generation (sent as tools.imageGeneration). *)
type image_generation_config = {
  image_enabled : bool option;
  image_watermark : bool option;
  save_to_disk : bool option;
}

let image_generation_config_to_yojson (c : image_generation_config) : Yojson.Safe.t =
  `Assoc
    (List.rev
       ([]
        |> add_bool "enabled" c.image_enabled
        |> add_bool "watermark" c.image_watermark
        |> add_bool "saveToDisk" c.save_to_disk))

(** Autopilot continuation options (sent as execution.autopilot). *)
type autopilot_options = {
  continuation_enabled : bool option;
  max_continues : int option;
}

let autopilot_options_to_yojson (a : autopilot_options) : Yojson.Safe.t =
  `Assoc
    (List.rev
       ([]
        |> add_bool "continuationEnabled" a.continuation_enabled
        |> add_int "maxContinues" a.max_continues))

(** Token limits for a model capabilities override. *)
type model_limits = {
  max_prompt_tokens : int option;
  max_output_tokens : int option;
  max_context_window_tokens : int option;
}

let model_limits_to_capabilities (l : model_limits) : (string * Yojson.Safe.t) list =
  [ ( "limits"
    , `Assoc
        (List.rev
           ([]
            |> add_int "max_prompt_tokens" l.max_prompt_tokens
            |> add_int "max_output_tokens" l.max_output_tokens
            |> add_int "max_context_window_tokens" l.max_context_window_tokens)) )
  ]

let azure_json version = `Assoc [ ("apiVersion", `String version) ]

(** Legacy single BYOK provider. Sent as a named provider called "default". *)
type provider_config = {
  prov_type : string option;
  prov_base_url : string;
  prov_api_key : string option;
  prov_api_key_command : string option;
  prov_bearer_token : string option;
  prov_wire_api : string option;
  prov_model_provider : string option;
  prov_azure_api_version : string option;
}

let make_provider_config ?provider_type ?api_key ?api_key_command ?bearer_token
    ?wire_api ?model_provider ?azure_api_version base_url : provider_config =
  { prov_type = provider_type
  ; prov_base_url = base_url
  ; prov_api_key = api_key
  ; prov_api_key_command = api_key_command
  ; prov_bearer_token = bearer_token
  ; prov_wire_api = wire_api
  ; prov_model_provider = model_provider
  ; prov_azure_api_version = azure_api_version
  }

let provider_config_to_yojson (p : provider_config) : Yojson.Safe.t =
  `Assoc
    (List.rev
       ([ ("baseUrl", `String p.prov_base_url) ]
        |> add_string "type" p.prov_type
        |> add_string "apiKey" p.prov_api_key
        |> add_string "apiKeyCommand" p.prov_api_key_command
        |> add_string "bearerToken" p.prov_bearer_token
        |> add_string "wireApi" p.prov_wire_api
        |> add_string "modelProvider" p.prov_model_provider
        |> add_json "azure" (Option.map azure_json p.prov_azure_api_version)))

(** A named BYOK provider connection (inference.providers). *)
type named_provider_config = {
  np_name : string;
  np_type : string option;
  np_base_url : string;
  np_api_key : string option;
  np_api_key_command : string option;
  np_bearer_token : string option;
  np_wire_api : string option;
  np_transport : string option;
  np_azure_api_version : string option;
  np_headers : (string * string) list;
  np_model_provider : string option;
}

let make_named_provider ?provider_type ?api_key ?api_key_command ?bearer_token
    ?wire_api ?transport ?azure_api_version ?(headers = []) ?model_provider ~name
    base_url : named_provider_config =
  { np_name = name
  ; np_type = provider_type
  ; np_base_url = base_url
  ; np_api_key = api_key
  ; np_api_key_command = api_key_command
  ; np_bearer_token = bearer_token
  ; np_wire_api = wire_api
  ; np_transport = transport
  ; np_azure_api_version = azure_api_version
  ; np_headers = headers
  ; np_model_provider = model_provider
  }

let named_provider_config_to_yojson (p : named_provider_config) : Yojson.Safe.t =
  let headers =
    match p.np_headers with
    | [] -> None
    | hdrs -> Some (`Assoc (List.map (fun (k, v) -> (k, `String v)) hdrs))
  in
  `Assoc
    (List.rev
       ([ ("baseUrl", `String p.np_base_url); ("name", `String p.np_name) ]
        |> add_string "type" p.np_type
        |> add_string "apiKey" p.np_api_key
        |> add_string "apiKeyCommand" p.np_api_key_command
        |> add_string "bearerToken" p.np_bearer_token
        |> add_string "wireApi" p.np_wire_api
        |> add_string "transport" p.np_transport
        |> add_json "azure" (Option.map azure_json p.np_azure_api_version)
        |> add_json "headers" headers
        |> add_string "modelProvider" p.np_model_provider))

(** A model served by a named provider (inference.models). Select it with
    the session model "provider/id". *)
type provider_model_config = {
  pm_provider : string;
  pm_id : string;
  pm_model_id : string option;
  pm_wire_model : string option;
  pm_name : string option;
  pm_max_prompt_tokens : int option;
  pm_max_context_window_tokens : int option;
  pm_max_output_tokens : int option;
  pm_capabilities : Yojson.Safe.t option;
}

let make_provider_model ?model_id ?wire_model ?name ?max_prompt_tokens
    ?max_context_window_tokens ?max_output_tokens ?capabilities ~provider id
    : provider_model_config =
  { pm_provider = provider
  ; pm_id = id
  ; pm_model_id = model_id
  ; pm_wire_model = wire_model
  ; pm_name = name
  ; pm_max_prompt_tokens = max_prompt_tokens
  ; pm_max_context_window_tokens = max_context_window_tokens
  ; pm_max_output_tokens = max_output_tokens
  ; pm_capabilities = capabilities
  }

let provider_model_config_to_yojson (m : provider_model_config) : Yojson.Safe.t =
  `Assoc
    (List.rev
       ([ ("id", `String m.pm_id); ("provider", `String m.pm_provider) ]
        |> add_string "modelId" m.pm_model_id
        |> add_string "wireModel" m.pm_wire_model
        |> add_string "name" m.pm_name
        |> add_int "maxPromptTokens" m.pm_max_prompt_tokens
        |> add_int "maxContextWindowTokens" m.pm_max_context_window_tokens
        |> add_int "maxOutputTokens" m.pm_max_output_tokens
        |> add_json "capabilities" m.pm_capabilities))

(** Shell authentication input kinds. *)
let shell_input_kind_secret = "secret"
let shell_input_kind_text = "text"
let shell_input_kind_confirm = "confirm"

(** A [shellAuthentication.request] from a program (sudo, ssh, git). *)
type shell_authentication_request = {
  sar_session_id : string;
  sar_request_id : string;
  sar_program : string;
  sar_prompt : string;
  sar_input_kind : string;
  sar_command : string;
  sar_cwd : string;
}

(** Answer to a shell authentication request. *)
type shell_authentication_response =
  | Shell_auth_accept of string
  | Shell_auth_cancel

let shell_authentication_response_to_yojson (r : shell_authentication_response)
    : Yojson.Safe.t =
  match r with
  | Shell_auth_accept value ->
    `Assoc [ ("action", `String "accept"); ("value", `String value) ]
  | Shell_auth_cancel -> `Assoc [ ("action", `String "cancel") ]

(** Answers shell authentication prompts. The promise is cancelled when the
    runtime sends [shellAuthentication.cancel]. *)
type shell_authentication_handler =
  shell_authentication_request -> shell_authentication_response Lwt.t

(** A fact the agent asked the host to remember ([memoryStore.store]). *)
type stored_memory = {
  sm_session_id : string;
  sm_subject : string;
  sm_fact : string;
  sm_citations : string;
  sm_reason : string;
}

(** Persists memories (sends state.hasMemoryStore). *)
type store_memory_handler = stored_memory -> unit Lwt.t

(** Sub-agent lifecycle hook: receives the raw hook input and returns the
    hook output object, if any. *)
type subagent_hook_handler = Yojson.Safe.t -> Yojson.Safe.t option Lwt.t

(** Network callback scopes. *)
let network_scope_connection = "connection"
let network_scope_runtime = "runtime"

(** Payload of a [network.authenticateBasic] request. *)
type network_basic_auth_request = {
  nba_proxy : string;
  nba_realm : string option;
  nba_previous_rejected : bool;
}

(** Proxy Basic credentials. Never log or persist them. *)
type network_basic_credentials = {
  nbc_username : string option;
  nbc_password : string option;
}

(** Host network callbacks, registered with [network.setProvider]. *)
type network_options = {
  network_scope : string option;
  resolve_proxy : (string -> string option Lwt.t) option;
  authenticate_negotiate : (string -> string option Lwt.t) option;
  authenticate_basic :
    (network_basic_auth_request -> network_basic_credentials option Lwt.t) option;
}

let default_network_options () : network_options =
  { network_scope = None
  ; resolve_proxy = None
  ; authenticate_negotiate = None
  ; authenticate_basic = None
  }

let network_set_provider_params (n : network_options) : Yojson.Safe.t option =
  let marker callback = Option.map (fun _ -> `Assoc []) callback in
  let callbacks =
    []
    |> add_json "authenticateBasic" (marker n.authenticate_basic)
    |> add_json "authenticateNegotiate" (marker n.authenticate_negotiate)
    |> add_json "resolveProxy" (marker n.resolve_proxy)
  in
  match callbacks with
  | [] -> None
  | _ -> Some (`Assoc (add_string "scope" n.network_scope callbacks))

(** Client modes. *)
let client_mode_coding_agent = "coding-agent"
let client_mode_empty = "empty"
let client_mode_copilot_cli = "copilot-cli"

let normalize_client_mode (mode : string option) : string =
  match mode with
  | None -> client_mode_coding_agent
  | Some m when m = client_mode_coding_agent || m = client_mode_copilot_cli ->
    client_mode_coding_agent
  | Some m when m = client_mode_empty -> client_mode_empty
  | Some m ->
    invalid_arg
      (Printf.sprintf "mode must be %S or %S, got %S" client_mode_coding_agent
         client_mode_empty m)

(** The latest unresolved Policy Helper failure. Helper-authored text is
    untrusted: render it as plain text and never execute it. *)
type policy_helper_failure = {
  phf_category : string;
  phf_failure_mode : string;
  phf_message : string;
  phf_previous_policy_retained : bool;
  phf_reason : string;
  phf_authority : string option;
  phf_code : string option;
  phf_diagnostic : string option;
  phf_remediation : string option;
  phf_severity : string option;
  phf_help_link : Yojson.Safe.t option;
  phf_details : Yojson.Safe.t option;
}

let policy_helper_failure_of_yojson (json : Yojson.Safe.t)
    : policy_helper_failure option =
  match json with
  | `Assoc _ ->
    let required key = Option.value (json_string_field json key) ~default:"" in
    Some
      { phf_category = required "category"
      ; phf_failure_mode = required "failureMode"
      ; phf_message = required "message"
      ; phf_previous_policy_retained =
          (match json_field json "previousPolicyRetained" with
           | Some (`Bool b) -> b
           | _ -> false)
      ; phf_reason = required "reason"
      ; phf_authority = json_string_field json "authority"
      ; phf_code = json_string_field json "code"
      ; phf_diagnostic = json_string_field json "diagnostic"
      ; phf_remediation = json_string_field json "remediation"
      ; phf_severity = json_string_field json "severity"
      ; phf_help_link = json_field json "helpLink"
      ; phf_details = json_field json "details"
      }
  | _ -> None

let policy_helper_failure_of_error_data (data : Yojson.Safe.t option)
    : policy_helper_failure option =
  match data with
  | Some d -> Option.bind (json_field d "policyHelperFailure") policy_helper_failure_of_yojson
  | None -> None

(** Permission modes and the known refusal reason. *)
let permission_mode_manual = "manual"
let permission_mode_assisted = "assisted"
let permission_mode_allow_all = "allow-all"
let permission_mode_refusal_managed_policy = "managed_policy"

(** Result of [session.permissions.setMode]. *)
type permission_mode_set_result = {
  pms_success : bool;
  pms_mode : string;
  pms_reason : string option;
}

let permission_mode_set_result_of_yojson (json : Yojson.Safe.t)
    : permission_mode_set_result =
  { pms_success =
      (match json_field json "success" with Some (`Bool b) -> b | _ -> false)
  ; pms_mode = Option.value (json_string_field json "mode") ~default:""
  ; pms_reason = json_string_field json "reason"
  }

(** Data of a "tool.shell_output" event: live, append-only shell output. *)
type tool_shell_output = {
  so_tool_call_id : string;
  so_text : string;
  so_sequence : int;
  so_stream : string option;
}

let tool_shell_output_of_yojson (data : Yojson.Safe.t) : tool_shell_output option =
  match json_string_field data "toolCallId", json_string_field data "text" with
  | Some tool_call_id, Some text ->
    Some
      { so_tool_call_id = tool_call_id
      ; so_text = text
      ; so_sequence =
          (match json_field data "sequence" with Some (`Int n) -> n | _ -> 0)
      ; so_stream = json_string_field data "stream"
      }
  | _ -> None

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
  rewind_enabled : bool option;
  additional_directories : string list option;
  disabled_mcp_servers : string list option;
  github_mcp_tool_config : (string * Yojson.Safe.t) list;
  canvas_provider : (string * Yojson.Safe.t) list;
  custom_agents_local_only : bool option;
  tool_search : (string * Yojson.Safe.t) list;
  experimental_mode : bool option;
  content_exclusion : bool option;
  (* v2.6.0 / v3.0.0 session options (SDK protocol 4) *)
  session_name : string option;
  auto_tier : string option;
  ask_user_variant : string option;
  github_token_provider : github_token_provider option;
  sandbox_config : sandbox_config option;
  feature_flags : (string * bool) list option;
  allowed_models : string list option;
  auth_client_id_metadata_url : string option;
  agent_directories : string list option;
  hook_directories : string list option;
  interactive_user : bool option;
  excluded_managed_settings : string list option;
  enforce_managed_model_defaults : bool option;
  trust_working_directory : string option;
  refresh_custom_instructions : bool option;
  image_generation : image_generation_config option;
  autopilot : autopilot_options option;
  on_subagent_start : subagent_hook_handler option;
  on_subagent_stop : subagent_hook_handler option;
  provider : provider_config option;
  allow_transcript_recovery : bool option;
  suppress_resume_event : bool option;
  available_tools : string list option;
  providers : named_provider_config list option;
  models : provider_model_config list option;
  default_provider : string option;
  clear_default_provider : bool;
  capi_enabled : bool option;
  on_shell_authentication_request : shell_authentication_handler option;
  on_store_memory : store_memory_handler option;
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
  ; rewind_enabled = None
  ; additional_directories = None
  ; disabled_mcp_servers = None
  ; github_mcp_tool_config = []
  ; canvas_provider = []
  ; custom_agents_local_only = None
  ; tool_search = []
  ; experimental_mode = None
  ; content_exclusion = None
  ; session_name = None
  ; auto_tier = None
  ; ask_user_variant = None
  ; github_token_provider = None
  ; sandbox_config = None
  ; feature_flags = None
  ; allowed_models = None
  ; auth_client_id_metadata_url = None
  ; agent_directories = None
  ; hook_directories = None
  ; interactive_user = None
  ; excluded_managed_settings = None
  ; enforce_managed_model_defaults = None
  ; trust_working_directory = None
  ; refresh_custom_instructions = None
  ; image_generation = None
  ; autopilot = None
  ; on_subagent_start = None
  ; on_subagent_stop = None
  ; provider = None
  ; allow_transcript_recovery = None
  ; suppress_resume_event = None
  ; available_tools = None
  ; providers = None
  ; models = None
  ; default_provider = None
  ; clear_default_provider = false
  ; capi_enabled = None
  ; on_shell_authentication_request = None
  ; on_store_memory = None
  }

let session_base_fields (c : session_config) : (string * Yojson.Safe.t) list =
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
    match c.exp_assignments with
    | [] -> fields
    | assigns -> ("expAssignments", `Assoc assigns) :: fields
  in
  let fields =
    if c.on_mcp_auth_request then ("mcpAuthHandler", `Bool true) :: fields
    else fields
  in
  let fields =
    match c.rewind_enabled with
    | Some b -> ("rewindEnabled", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match c.additional_directories with
    | Some dirs ->
      ("additionalDirectories", `List (List.map (fun s -> `String s) dirs)) :: fields
    | None -> fields
  in
  let fields =
    match c.disabled_mcp_servers with
    | Some servers ->
      ("disabledMcpServers", `List (List.map (fun s -> `String s) servers)) :: fields
    | None -> fields
  in
  let fields =
    match c.github_mcp_tool_config with
    | [] -> fields
    | assigns -> ("githubMcpToolConfig", `Assoc assigns) :: fields
  in
  let fields =
    match c.canvas_provider with
    | [] -> fields
    | assigns -> ("canvasProvider", `Assoc assigns) :: fields
  in
  let fields =
    match c.custom_agents_local_only with
    | Some b -> ("customAgentsLocalOnly", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match c.tool_search with
    | [] -> fields
    | assigns -> ("toolSearch", `Assoc assigns) :: fields
  in
  let fields =
    match c.experimental_mode with
    | Some b -> ("enableExperimentalMode", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match c.content_exclusion with
    | Some b -> ("contentExclusion", `Bool b) :: fields
    | None -> fields
  in
  fields

(* Options added for v2.6.0 and v3.0.0. [resume] selects the create-only or
   resume-only keys. *)
let session_extra_fields ~resume (c : session_config) : (string * Yojson.Safe.t) list =
  let capi =
    []
    |> add_string "autoTier" c.auto_tier
    |> add_bool "enableWebSocketResponses"
         (if c.enable_web_socket_responses then Some true else None)
    |> add_bool "enabled" c.capi_enabled
  in
  let flag enabled = if enabled then Some true else None in
  let fields =
    []
    |> add_json "capi" (match capi with [] -> None | _ -> Some (`Assoc (List.rev capi)))
    |> add_string "askUserVariant" c.ask_user_variant
    |> add_json "sandboxConfig" (Option.map sandbox_config_to_yojson c.sandbox_config)
    |> add_json "featureFlags"
         (Option.map
            (fun flags -> `Assoc (List.map (fun (k, b) -> (k, `Bool b)) flags))
            c.feature_flags)
    |> add_json "allowedModels" (Option.map string_list c.allowed_models)
    |> add_string "authClientIdMetadataUrl" c.auth_client_id_metadata_url
    |> add_json "agentDirectories" (Option.map string_list c.agent_directories)
    |> add_json "hookDirectories" (Option.map string_list c.hook_directories)
    |> add_bool "interactiveUser" c.interactive_user
    |> add_json "excludedManagedSettings" (Option.map string_list c.excluded_managed_settings)
    |> add_bool "enforceManagedModelDefaults" c.enforce_managed_model_defaults
    |> add_json "imageGeneration" (Option.map image_generation_config_to_yojson c.image_generation)
    |> add_json "autopilot" (Option.map autopilot_options_to_yojson c.autopilot)
    |> add_json "provider" (Option.map provider_config_to_yojson c.provider)
    |> add_json "availableTools" (Option.map string_list c.available_tools)
    |> add_json "providers"
         (Option.map (fun ps -> `List (List.map named_provider_config_to_yojson ps)) c.providers)
    |> add_json "models"
         (Option.map (fun ms -> `List (List.map provider_model_config_to_yojson ms)) c.models)
    |> add_string "defaultProvider" c.default_provider
    |> add_bool "hooks"
         (flag (Option.is_some c.on_subagent_start || Option.is_some c.on_subagent_stop))
    |> add_bool "requestShellAuthentication"
         (flag (Option.is_some c.on_shell_authentication_request))
    |> add_bool "hasMemoryStore" (flag (Option.is_some c.on_store_memory))
  in
  let fields =
    if resume then
      fields
      |> add_bool "allowTranscriptRecovery" c.allow_transcript_recovery
      |> add_bool "suppressResumeEvent" c.suppress_resume_event
      |> add_json "inference"
           (* An explicit null clears the provider stored with the session. *)
           (if c.clear_default_provider then Some (`Assoc [ ("defaultProvider", `Null) ])
            else None)
    else
      fields
      |> add_string "name" c.session_name
      |> add_string "trustWorkingDirectory" c.trust_working_directory
      |> add_bool "refreshCustomInstructions" c.refresh_custom_instructions
  in
  List.rev fields

(* Flat session.create payload (translated to the grouped protocol 4 shape
   by Session_wire right before sending). *)
let session_config_to_yojson (c : session_config) : Yojson.Safe.t =
  `Assoc (List.rev (session_base_fields c) @ session_extra_fields ~resume:false c)

(* Flat session.resume payload (without the session id). *)
let session_resume_config_to_yojson (c : session_config) : Yojson.Safe.t =
  `Assoc (List.rev (session_base_fields c) @ session_extra_fields ~resume:true c)

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
  | ToolShellOutput
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
  | "tool.shell_output" -> ToolShellOutput
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

(** Serialize a permission reply together with an opaque decision context.
    The context is emitted under the JSON key "decisionContext". *)
let permission_result_to_yojson_with_context (r : permission_result)
    (decision_context : Yojson.Safe.t option) : Yojson.Safe.t =
  match decision_context, permission_result_to_yojson r with
  | Some ctx, `Assoc fields -> `Assoc (("decisionContext", ctx) :: fields)
  | _, json -> json

(* ========================================================================== *)
(* Exit Plan Mode                                                             *)
(* ========================================================================== *)

type exit_plan_mode_request = {
  session_id : string;
  summary : string;
  plan_content : string option;
  actions : string list;
  recommended_action : string;
}

let exit_plan_mode_request_of_yojson (json : Yojson.Safe.t)
    : (exit_plan_mode_request, string) result =
  try
    Ok
      { session_id = (try json |> member "sessionId" |> to_string with _ -> "")
      ; summary = (try json |> member "summary" |> to_string with _ -> "")
      ; plan_content =
          (try Some (json |> member "planContent" |> to_string) with _ -> None)
      ; actions =
          (try json |> member "actions" |> to_list |> List.map to_string
           with _ -> [])
      ; recommended_action =
          (try json |> member "recommendedAction" |> to_string with _ -> "")
      }
  with exn -> Error (Printexc.to_string exn)

type exit_plan_mode_response = {
  approved : bool;
  selected_action : string option;
  feedback : string option;
}

let exit_plan_mode_response_to_yojson (r : exit_plan_mode_response)
    : Yojson.Safe.t =
  let fields = [ ("approved", `Bool r.approved) ] in
  let fields =
    match r.selected_action with
    | Some v -> ("selectedAction", `String v) :: fields
    | None -> fields
  in
  let fields =
    match r.feedback with
    | Some v -> ("feedback", `String v) :: fields
    | None -> fields
  in
  `Assoc fields

let default_exit_plan_mode_response () : exit_plan_mode_response =
  { approved = true; selected_action = None; feedback = None }

(** W3C trace context propagated on outbound requests. *)
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
  request_handler : copilot_request_handler option;
  bearer_token_provider : bearer_token_provider option;
  (* JSON key: "builtinPluginDirectories" *)
  builtin_plugin_directories : string list option;
  (* JSON key: "inProcess" *)
  in_process : bool option;
  (* Enable remote session support (Mission Control); adds the --remote CLI flag. *)
  remote : bool option;
  (* Provider that supplies W3C trace context for outbound requests. *)
  on_get_trace_context : (unit -> trace_context) option;
  (* Identity of the embedding application, sent with "connect". *)
  client_info : client_info option;
  (* "coding-agent" (default when None) or "empty"; "copilot-cli" is a legacy alias. *)
  client_mode : string option;
  (* Host network callbacks, registered with "network.setProvider". *)
  network : network_options option;
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
  ; builtin_plugin_directories = None
  ; in_process = None
  ; remote = None
  ; on_get_trace_context = None
  ; client_info = None
  ; client_mode = None
  ; network = None
  }

(* ========================================================================== *)
(* Helper Constructors                                                        *)
(* ========================================================================== *)

let make_message ?mode ?agent_mode ?display_prompt ?(request_headers = []) ?source
    ?response_schema ?client_turn_id prompt =
  { prompt
  ; mode
  ; agent_mode
  ; display_prompt
  ; request_headers
  ; source
  ; response_schema
  ; client_turn_id
  }

let make_tool_definition ?parameters ?idempotent name description =
  { tool_name = name
  ; tool_description = description
  ; tool_parameters = parameters
  ; tool_idempotent = idempotent
  }
