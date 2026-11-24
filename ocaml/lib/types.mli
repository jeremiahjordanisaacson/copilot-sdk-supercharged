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
  args_schema : Yojson.Safe.t option;
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
  agent_mode : string option;
  display_prompt : string option;
  request_headers : (string * string) list;
  source : string option;
  response_schema : Yojson.Safe.t option;
  client_turn_id : string option;
}

val message_options_to_yojson : message_options -> Yojson.Safe.t

type tool_definition = {
  tool_name : string;
  tool_description : string;
  tool_parameters : Yojson.Safe.t option;
  tool_idempotent : bool option;
}

(** {1 Upstream-sync Feature Types & Constants} *)

type session_limits_config = { max_ai_credits : float option }

val session_limits_config_to_yojson : session_limits_config -> Yojson.Safe.t

type memory_configuration = { memory_enabled : bool }

val memory_configuration_to_yojson : memory_configuration -> Yojson.Safe.t

type provider_token_args = { pta_session_id : string }

val provider_token_args_session_id : provider_token_args -> string

type bearer_token_provider = provider_token_args -> string
type copilot_request_handler = Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t

type tool_defer = DeferAuto | DeferNever

val tool_defer_to_string : tool_defer -> string
val system_message_section_preamble : string
val system_message_section_identity : string
val system_message_section_tool_instructions : string
val system_message_section_preserve : string
val attachment_github_commit : string
val attachment_github_release : string
val attachment_github_actions_job : string
val attachment_github_repository : string
val attachment_github_file_diff : string
val attachment_github_tree_comparison : string
val attachment_github_url : string
val attachment_github_file : string
val attachment_github_snippet : string
val hook_on_pre_tool_use : string
val hook_on_post_tool_use : string
val hook_on_pre_mcp_tool_call : string
val hook_on_user_prompt_transformed : string

(** {1 v2.6.0 / v3.0.0 Feature Types (SDK protocol 4)} *)

type client_info = {
  editor_name : string option;
  editor_version : string option;
  extension_name : string option;
  extension_version : string option;
}
(** Identity of the embedding application, sent with [connect]. *)

val client_info_to_yojson : client_info -> Yojson.Safe.t option
(** Drops empty fields; [None] when every field is empty. *)

val auto_tier_fast : string
val auto_tier_efficiency : string
val auto_tier_balance : string
val auto_tier_intelligence : string
val ask_user_variant_legacy : string
val ask_user_variant_elicitation : string
val working_directory_trust_session : string
val working_directory_trust_user : string
val message_source_user : string
val message_source_system : string
val message_source_agent : string -> string

type github_token_request = {
  gtr_registration_id : string;
  gtr_host : string;
  gtr_session_id : string option;
  gtr_reason : string;
}

type github_token_result =
  | Github_token of {
      access_token : string;
      token_type : string option;
      expires_in : int;
    }
  | Github_token_cancelled

val github_token_result_to_yojson : github_token_result -> Yojson.Safe.t

type github_token_provider = github_token_request -> github_token_result Lwt.t

type sandbox_config = {
  sandbox_enabled : bool;
  sandbox_user_policy : Yojson.Safe.t option;
  sandbox_add_current_working_directory : bool option;
  sandbox_mcp_servers : bool option;
  sandbox_lsp_servers : bool option;
  sandbox_extensions : bool option;
  allow_bypass : bool option;
}

val default_sandbox_config : unit -> sandbox_config
val sandbox_config_to_yojson : sandbox_config -> Yojson.Safe.t

type image_generation_config = {
  image_enabled : bool option;
  image_watermark : bool option;
  save_to_disk : bool option;
}

val image_generation_config_to_yojson : image_generation_config -> Yojson.Safe.t

type autopilot_options = {
  continuation_enabled : bool option;
  max_continues : int option;
}

val autopilot_options_to_yojson : autopilot_options -> Yojson.Safe.t

type model_limits = {
  max_prompt_tokens : int option;
  max_output_tokens : int option;
  max_context_window_tokens : int option;
}

val model_limits_to_capabilities : model_limits -> (string * Yojson.Safe.t) list
(** [model_limits_to_capabilities l] is a [model_capabilities] override whose
    "limits" object uses the snake_case wire keys (for example
    [max_output_tokens]). *)

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

val make_provider_config :
  ?provider_type:string ->
  ?api_key:string ->
  ?api_key_command:string ->
  ?bearer_token:string ->
  ?wire_api:string ->
  ?model_provider:string ->
  ?azure_api_version:string ->
  string ->
  provider_config

val provider_config_to_yojson : provider_config -> Yojson.Safe.t

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

val make_named_provider :
  ?provider_type:string ->
  ?api_key:string ->
  ?api_key_command:string ->
  ?bearer_token:string ->
  ?wire_api:string ->
  ?transport:string ->
  ?azure_api_version:string ->
  ?headers:(string * string) list ->
  ?model_provider:string ->
  name:string ->
  string ->
  named_provider_config

val named_provider_config_to_yojson : named_provider_config -> Yojson.Safe.t

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

val make_provider_model :
  ?model_id:string ->
  ?wire_model:string ->
  ?name:string ->
  ?max_prompt_tokens:int ->
  ?max_context_window_tokens:int ->
  ?max_output_tokens:int ->
  ?capabilities:Yojson.Safe.t ->
  provider:string ->
  string ->
  provider_model_config

val provider_model_config_to_yojson : provider_model_config -> Yojson.Safe.t

val shell_input_kind_secret : string
val shell_input_kind_text : string
val shell_input_kind_confirm : string

type shell_authentication_request = {
  sar_session_id : string;
  sar_request_id : string;
  sar_program : string;
  sar_prompt : string;
  sar_input_kind : string;
  sar_command : string;
  sar_cwd : string;
}

type shell_authentication_response =
  | Shell_auth_accept of string
  | Shell_auth_cancel

val shell_authentication_response_to_yojson :
  shell_authentication_response -> Yojson.Safe.t

type shell_authentication_handler =
  shell_authentication_request -> shell_authentication_response Lwt.t

type stored_memory = {
  sm_session_id : string;
  sm_subject : string;
  sm_fact : string;
  sm_citations : string;
  sm_reason : string;
}

type store_memory_handler = stored_memory -> unit Lwt.t

type subagent_hook_handler = Yojson.Safe.t -> Yojson.Safe.t option Lwt.t

val network_scope_connection : string
val network_scope_runtime : string

type network_basic_auth_request = {
  nba_proxy : string;
  nba_realm : string option;
  nba_previous_rejected : bool;
}

type network_basic_credentials = {
  nbc_username : string option;
  nbc_password : string option;
}

type network_options = {
  network_scope : string option;
  resolve_proxy : (string -> string option Lwt.t) option;
  authenticate_negotiate : (string -> string option Lwt.t) option;
  authenticate_basic :
    (network_basic_auth_request -> network_basic_credentials option Lwt.t) option;
}

val default_network_options : unit -> network_options

val network_set_provider_params : network_options -> Yojson.Safe.t option
(** [None] when no callback is registered. *)

val client_mode_coding_agent : string
val client_mode_empty : string
val client_mode_copilot_cli : string

val normalize_client_mode : string option -> string
(** Maps [None] and "copilot-cli" to "coding-agent"; raises [Invalid_argument]
    for values other than "coding-agent" and "empty". *)

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

val policy_helper_failure_of_yojson : Yojson.Safe.t -> policy_helper_failure option

val policy_helper_failure_of_error_data :
  Yojson.Safe.t option -> policy_helper_failure option
(** Reads [policyHelperFailure] from JSON-RPC error data. *)

val permission_mode_manual : string
val permission_mode_assisted : string
val permission_mode_allow_all : string
val permission_mode_refusal_managed_policy : string

type permission_mode_set_result = {
  pms_success : bool;
  pms_mode : string;
  pms_reason : string option;
}

val permission_mode_set_result_of_yojson : Yojson.Safe.t -> permission_mode_set_result

type tool_shell_output = {
  so_tool_call_id : string;
  so_text : string;
  so_sequence : int;
  so_stream : string option;
}

val tool_shell_output_of_yojson : Yojson.Safe.t -> tool_shell_output option

(** {1 v3.1.0 Feature Types} *)

type user_input_image_attachment_constraints = {
  uiac_allowed_mime_types : string list;
  uiac_max_bytes_per_image : int;
  uiac_max_count : int;
}
(** Image support advertised by a [userInput.request] ([imageAttachmentConstraints]);
    absent when the runtime does not accept images in the answer. *)

val user_input_image_attachment_constraints_of_yojson :
  Yojson.Safe.t -> user_input_image_attachment_constraints option

type user_input_request = {
  uir_session_id : string;
  uir_question : string;
  uir_choices : string list option;
  uir_allow_freeform : bool option;
  uir_request_id : string option;  (** Pending request id, when the runtime sends it. *)
  uir_image_attachment_constraints : user_input_image_attachment_constraints option;
      (** Set when the runtime accepts image attachments in the answer. *)
  uir_meta : Yojson.Safe.t option;  (** The request's opaque [_meta] object. *)
}
(** A [userInput.request] (the ask_user flow), passed to
    [session_config.on_user_input_request]. *)

val user_input_request_of_yojson : Yojson.Safe.t -> user_input_request option
(** [None] when [sessionId] or [question] is missing. *)

type user_input_image_attachment = {
  uia_data : string;  (** Base64-encoded image bytes. *)
  uia_mime_type : string;
  uia_description : string option;
  uia_display_name : string option;
}
(** An inline image attached to an ask_user answer; sent with [type = "image"]. *)

val make_user_input_image_attachment :
  ?description:string ->
  ?display_name:string ->
  mime_type:string ->
  string ->
  user_input_image_attachment
(** [make_user_input_image_attachment ?description ?display_name ~mime_type data]
    builds an attachment from base64 [data]. *)

val user_input_image_attachment_to_yojson : user_input_image_attachment -> Yojson.Safe.t

type user_input_response = {
  answer : string;
  was_freeform : bool;
  attachments : user_input_image_attachment list option;
      (** Only valid when the request advertised
          [uir_image_attachment_constraints]; sent only when set. The runtime
          enforces the MIME type, size, and count limits. *)
}
(** Answer to a [userInput.request]. An image-only answer uses an empty
    [answer] and [was_freeform = true]. *)

val make_user_input_response :
  ?attachments:user_input_image_attachment list ->
  was_freeform:bool ->
  string ->
  user_input_response

val user_input_response_to_yojson : user_input_response -> Yojson.Safe.t
(** Emits [attachments] only when set. *)

type user_input_handler = user_input_request -> user_input_response Lwt.t
(** Answers ask_user questions; setting one sends [requestUserInput]. *)

type mcp_secrets_save_result = { mss_used_keychain : bool }
(** Result of [mcp.secrets.save]: [mss_used_keychain] is [true] when the OS
    keychain stored the credential and [false] for the file backend. *)

val mcp_secrets_save_result_of_yojson : Yojson.Safe.t -> mcp_secrets_save_result

type permissions_extension_approval = {
  pea_extension_name : string;
      (** Stable identity, for example ["plugin:plugin-name:extension-name"]. *)
  pea_plugin_name : string;
  pea_capabilities : string list;  (** Exact privileged capabilities the user approved. *)
  pea_granted_at : string;  (** RFC 3339 timestamp. *)
  pea_installation_key : string;  (** Opaque; pass it back unchanged to revoke. *)
  pea_installation_label : string option;
}
(** A user-scoped approval for one installed plugin extension. *)

val permissions_extension_approval_of_yojson :
  Yojson.Safe.t -> permissions_extension_approval option

type permissions_extension_approvals_grant_result = {
  peag_granted : bool;  (** Whether the pending request was eligible and persisted. *)
  peag_approval : permissions_extension_approval option;
}
(** Result of [permissions.extensionApprovals.grant]. *)

val permissions_extension_approvals_grant_result_of_yojson :
  Yojson.Safe.t -> permissions_extension_approvals_grant_result

type permissions_extension_approvals_list_result = {
  peal_approvals : permissions_extension_approval list;
}
(** Result of [permissions.extensionApprovals.list]. *)

val permissions_extension_approvals_list_result_of_yojson :
  Yojson.Safe.t -> permissions_extension_approvals_list_result

type permissions_extension_approvals_revoke_result = { pear_revoked : bool }
(** Result of [permissions.extensionApprovals.revoke]: whether a matching
    approval existed and was removed. *)

val permissions_extension_approvals_revoke_result_of_yojson :
  Yojson.Safe.t -> permissions_extension_approvals_revoke_result

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
  on_user_input_request : user_input_handler option;
}

val default_session_config : unit -> session_config

val session_config_to_yojson : session_config -> Yojson.Safe.t
(** Flat [session.create] payload; the client translates it to the grouped
    protocol 4 shape with {!Session_wire.group_session_params}. *)

val session_resume_config_to_yojson : session_config -> Yojson.Safe.t
(** Flat [session.resume] payload (without the session id). *)

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
  | ToolShellOutput
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

(** Serialize a permission reply together with an opaque decision context,
    emitted under the JSON key "decisionContext". *)
val permission_result_to_yojson_with_context :
  permission_result -> Yojson.Safe.t option -> Yojson.Safe.t

(** {1 Exit Plan Mode} *)

type exit_plan_mode_request = {
  session_id : string;
  summary : string;
  plan_content : string option;
  actions : string list;
  recommended_action : string;
}

val exit_plan_mode_request_of_yojson :
  Yojson.Safe.t -> (exit_plan_mode_request, string) result

type exit_plan_mode_response = {
  approved : bool;
  selected_action : string option;
  feedback : string option;
}

val exit_plan_mode_response_to_yojson : exit_plan_mode_response -> Yojson.Safe.t

val default_exit_plan_mode_response : unit -> exit_plan_mode_response

(** W3C trace context propagated on outbound requests. *)
type trace_context = {
  traceparent : string option;
  tracestate : string option;
}

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

val default_client_options : unit -> client_options

(** {1 Helper Constructors} *)

val make_message :
  ?mode:string ->
  ?agent_mode:string ->
  ?display_prompt:string ->
  ?request_headers:(string * string) list ->
  ?source:string ->
  ?response_schema:Yojson.Safe.t ->
  ?client_turn_id:string ->
  string ->
  message_options

val make_tool_definition :
  ?parameters:Yojson.Safe.t -> ?idempotent:bool -> string -> string -> tool_definition
