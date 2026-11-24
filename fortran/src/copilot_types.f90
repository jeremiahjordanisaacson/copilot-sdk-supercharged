! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Derived types for the Copilot SDK.
!! Defines options, configuration, metadata, events, tools, and permissions.
module copilot_types
  use copilot_json
  implicit none
  private

  ! Re-export all public types
  public :: copilot_client_options, session_config, session_metadata
  public :: session_event, send_options, send_and_wait_result
  public :: copilot_tool, tool_result, tool_invocation
  public :: permission_request, permission_result
  public :: elicitation_request, elicitation_result
  public :: exit_plan_mode_request_t, exit_plan_mode_response_t
  public :: session_fs_config, session_fs_entry
  public :: copilot_connection_state
  public :: mcp_server_config, command_definition, image_options
  public :: COPILOT_STATE_DISCONNECTED, COPILOT_STATE_CONNECTING
  public :: COPILOT_STATE_CONNECTED, COPILOT_STATE_ERROR
  public :: tool_callback_interface
  ! --- Upstream-sync feature surface (parity with @github/copilot-sdk) ---
  public :: session_limits_config, memory_configuration, provider_token_args
  public :: bearer_token_provider_interface, copilot_request_handler_interface
  public :: mcp_auth_handler_interface, post_tool_use_hook_interface
  public :: pre_mcp_tool_call_hook_interface
  public :: TOOL_DEFER_AUTO, TOOL_DEFER_NEVER
  public :: SECTION_PREAMBLE, SECTION_IDENTITY, SECTION_TOOL_INSTRUCTIONS, SECTION_PRESERVE
  public :: GITHUB_COMMIT, GITHUB_RELEASE, GITHUB_ACTIONS_JOB, GITHUB_REPOSITORY
  public :: GITHUB_FILE_DIFF, GITHUB_TREE_COMPARISON, GITHUB_URL, GITHUB_FILE, GITHUB_SNIPPET
  ! --- v2.6.0 upstream-sync surface (parity with @github/copilot-sdk) ---
  public :: copilot_client_info, sandbox_config, image_generation_config
  public :: autopilot_options, model_limits_override, byok_provider_config
  public :: github_token_provider_args, github_token_result
  public :: github_token_provider_interface
  public :: subagent_start_input, subagent_stop_input
  public :: subagent_start_output, subagent_stop_output
  public :: subagent_start_hook_interface, subagent_stop_hook_interface
  public :: MESSAGE_SOURCE_USER, MESSAGE_SOURCE_SYSTEM, message_source_agent
  public :: AUTO_TIER_EFFICIENCY, AUTO_TIER_BALANCE, AUTO_TIER_INTELLIGENCE, AUTO_TIER_FAST
  public :: ASK_USER_VARIANT_LEGACY, ASK_USER_VARIANT_ELICITATION
  public :: WORKING_DIRECTORY_TRUST_SESSION, WORKING_DIRECTORY_TRUST_USER
  ! --- v3.0.0 surface (SDK protocol 4, parity with @github/copilot-sdk 2.0) ---
  public :: named_provider_config, provider_model_config
  public :: CLIENT_MODE_CODING_AGENT, CLIENT_MODE_EMPTY, CLIENT_MODE_COPILOT_CLI
  public :: normalize_client_mode
  public :: EVENT_TOOL_SHELL_OUTPUT, tool_shell_output_data, parse_tool_shell_output
  public :: shell_authentication_request, shell_authentication_result
  public :: shell_authentication_handler_interface
  public :: SHELL_AUTH_INPUT_SECRET, SHELL_AUTH_INPUT_TEXT, SHELL_AUTH_INPUT_CONFIRM
  public :: stored_memory, store_memory_handler_interface
  public :: network_options, network_basic_auth_request, network_basic_credentials
  public :: network_resolve_proxy_interface, network_authenticate_negotiate_interface
  public :: network_authenticate_basic_interface
  public :: NETWORK_SCOPE_CONNECTION, NETWORK_SCOPE_RUNTIME
  public :: policy_helper_failure, parse_policy_helper_failure
  public :: PERMISSION_MODE_MANUAL, PERMISSION_MODE_ASSISTED, PERMISSION_MODE_ALLOW_ALL
  public :: PERMISSION_MODE_REFUSAL_MANAGED_POLICY, permission_mode_set_result
  public :: COPILOT_IOSTAT_RPC_ERROR, COPILOT_IOSTAT_PROTOCOL_MISMATCH
  public :: COPILOT_IOSTAT_INVALID_CONFIG
  ! --- v3.1.0 surface (parity with the Microsoft Copilot SDK 2.0.3-preview.0) ---
  public :: user_input_image_attachment_constraints, user_input_request
  public :: user_input_image_attachment, user_input_response, user_input_handler_interface
  public :: parse_user_input_request, user_input_response_json
  public :: mcp_secrets_save_result, permissions_extension_approval
  public :: permissions_extension_approvals_grant_result
  public :: permissions_extension_approvals_list_result
  public :: permissions_extension_approvals_revoke_result
  public :: parse_permissions_extension_approval

  ! --------------------------------------------------------------------------
  ! Connection state enum
  ! --------------------------------------------------------------------------
  integer, parameter :: COPILOT_STATE_DISCONNECTED = 0
  integer, parameter :: COPILOT_STATE_CONNECTING   = 1
  integer, parameter :: COPILOT_STATE_CONNECTED    = 2
  integer, parameter :: COPILOT_STATE_ERROR        = 3

  ! --------------------------------------------------------------------------
  ! Upstream-sync string constants
  ! --------------------------------------------------------------------------
  !> Tool "defer" loading policy (ToolDefer): eager pre-load or lazy search.
  character(len=*), parameter :: TOOL_DEFER_AUTO  = 'auto'   ! lazy: discovered on demand
  character(len=*), parameter :: TOOL_DEFER_NEVER = 'never'  ! eager: always pre-loaded

  !> System-message section identifiers ('preamble' targets the identity preamble;
  !! 'preserve' shields a section from a group-level remove).
  character(len=*), parameter :: SECTION_PREAMBLE          = 'preamble'
  character(len=*), parameter :: SECTION_IDENTITY          = 'identity'
  character(len=*), parameter :: SECTION_TOOL_INSTRUCTIONS = 'tool_instructions'
  character(len=*), parameter :: SECTION_PRESERVE          = 'preserve'

  !> GitHub-anchored attachment variants.
  character(len=*), parameter :: GITHUB_COMMIT          = 'GitHubCommit'
  character(len=*), parameter :: GITHUB_RELEASE         = 'GitHubRelease'
  character(len=*), parameter :: GITHUB_ACTIONS_JOB     = 'GitHubActionsJob'
  character(len=*), parameter :: GITHUB_REPOSITORY      = 'GitHubRepository'
  character(len=*), parameter :: GITHUB_FILE_DIFF       = 'GitHubFileDiff'
  character(len=*), parameter :: GITHUB_TREE_COMPARISON = 'GitHubTreeComparison'
  character(len=*), parameter :: GITHUB_URL             = 'GitHubUrl'
  character(len=*), parameter :: GITHUB_FILE            = 'GitHubFile'
  character(len=*), parameter :: GITHUB_SNIPPET         = 'GitHubSnippet'

  !> Message provenance for send_options%source ('agent-<id>' via message_source_agent).
  character(len=*), parameter :: MESSAGE_SOURCE_USER   = 'user'
  character(len=*), parameter :: MESSAGE_SOURCE_SYSTEM = 'system'

  !> Auto model routing preferences for session_config%auto_tier (experimental).
  character(len=*), parameter :: AUTO_TIER_EFFICIENCY   = 'efficiency'
  character(len=*), parameter :: AUTO_TIER_BALANCE      = 'balance'
  character(len=*), parameter :: AUTO_TIER_INTELLIGENCE = 'intelligence'
  character(len=*), parameter :: AUTO_TIER_FAST         = 'fast'

  !> Ask-user tool variants for session_config%ask_user_variant.
  character(len=*), parameter :: ASK_USER_VARIANT_LEGACY      = 'legacy'
  character(len=*), parameter :: ASK_USER_VARIANT_ELICITATION = 'elicitation'

  !> Working directory trust grants for session_config%trust_working_directory (create only).
  character(len=*), parameter :: WORKING_DIRECTORY_TRUST_SESSION = 'session'
  character(len=*), parameter :: WORKING_DIRECTORY_TRUST_USER    = 'user'

  !> Client modes for copilot_client_options%mode. 'copilot-cli' is a legacy alias of
  !! 'coding-agent' (the default). 'empty' requires session_config%available_tools.
  character(len=*), parameter :: CLIENT_MODE_CODING_AGENT = 'coding-agent'
  character(len=*), parameter :: CLIENT_MODE_EMPTY        = 'empty'
  character(len=*), parameter :: CLIENT_MODE_COPILOT_CLI  = 'copilot-cli'

  !> Streaming shell output event type (see parse_tool_shell_output).
  character(len=*), parameter :: EVENT_TOOL_SHELL_OUTPUT = 'tool.shell_output'

  !> Input kinds of a shellAuthentication.request.
  character(len=*), parameter :: SHELL_AUTH_INPUT_SECRET  = 'secret'
  character(len=*), parameter :: SHELL_AUTH_INPUT_TEXT    = 'text'
  character(len=*), parameter :: SHELL_AUTH_INPUT_CONFIRM = 'confirm'

  !> Scopes for network_options%scope.
  character(len=*), parameter :: NETWORK_SCOPE_CONNECTION = 'connection'
  character(len=*), parameter :: NETWORK_SCOPE_RUNTIME    = 'runtime'

  !> Permission modes for session%set_permission_mode and the known refusal reason.
  character(len=*), parameter :: PERMISSION_MODE_MANUAL    = 'manual'
  character(len=*), parameter :: PERMISSION_MODE_ASSISTED  = 'assisted'
  character(len=*), parameter :: PERMISSION_MODE_ALLOW_ALL = 'allow-all'
  character(len=*), parameter :: PERMISSION_MODE_REFUSAL_MANAGED_POLICY = 'managed_policy'

  !> iostat values reported by SDK calls (distinct from Fortran I/O status codes).
  integer, parameter :: COPILOT_IOSTAT_RPC_ERROR         = -101
  integer, parameter :: COPILOT_IOSTAT_PROTOCOL_MISMATCH = -102
  integer, parameter :: COPILOT_IOSTAT_INVALID_CONFIG    = -103

  type :: copilot_connection_state
    integer :: value = COPILOT_STATE_DISCONNECTED
  end type copilot_connection_state

  ! --------------------------------------------------------------------------
  ! Client identity sent as clientInfo on the connect handshake
  ! --------------------------------------------------------------------------
  type :: copilot_client_info
    character(len=:), allocatable :: editor_name
    character(len=:), allocatable :: editor_version
    character(len=:), allocatable :: extension_name
    character(len=:), allocatable :: extension_version
  end type copilot_client_info

  ! --------------------------------------------------------------------------
  ! Network callbacks (network.setProvider)
  ! --------------------------------------------------------------------------
  !> A proxy's Basic authentication challenge (network.authenticateBasic).
  type :: network_basic_auth_request
    character(len=:), allocatable :: proxy
    character(len=:), allocatable :: realm
    logical :: previous_rejected = .false.
  end type network_basic_auth_request

  !> Credentials answering a Basic challenge; leave unallocated when none are available.
  type :: network_basic_credentials
    character(len=:), allocatable :: username
    character(len=:), allocatable :: password
  end type network_basic_credentials

  !> Host network callbacks. Only the associated callbacks are registered.
  type :: network_options
    !> NETWORK_SCOPE_CONNECTION or NETWORK_SCOPE_RUNTIME (omitted when unallocated).
    character(len=:), allocatable :: scope
    procedure(network_resolve_proxy_interface), pointer, nopass :: resolve_proxy => null()
    procedure(network_authenticate_negotiate_interface), pointer, nopass :: authenticate_negotiate => null()
    procedure(network_authenticate_basic_interface), pointer, nopass :: authenticate_basic => null()
  end type network_options

  ! --------------------------------------------------------------------------
  ! Client options
  ! --------------------------------------------------------------------------
  type :: copilot_client_options
    character(len=:), allocatable :: cli_path
    character(len=:), allocatable :: cli_url
    character(len=:), allocatable :: cwd
    character(len=:), allocatable :: log_level
    character(len=:), allocatable :: github_token
    logical :: auto_start       = .true.
    logical :: auto_restart     = .false.
    logical :: use_logged_in_user = .false.
    logical :: use_stdio        = .true.
    integer :: session_idle_timeout_seconds = 0
    type(session_fs_config), allocatable :: session_fs
    character(len=:), allocatable :: copilot_home
    character(len=:), allocatable :: tcp_connection_token
    !> Enable remote session support (Mission Control); adds the --remote CLI flag.
    logical :: remote = .false.
    !> HTTP request handler to intercept/mutate outbound LLM inference requests
    !! (CopilotRequestHandler).
    procedure(copilot_request_handler_interface), pointer, nopass :: request_handler => null()
    !> BYOK bearer token provider (per-session token minting).
    procedure(bearer_token_provider_interface), pointer, nopass :: bearer_token_provider => null()
    !> Client identity sent as clientInfo on the connect handshake (empty fields are dropped).
    type(copilot_client_info) :: client_info
    !> Client mode: CLIENT_MODE_CODING_AGENT (default when unallocated) or CLIENT_MODE_EMPTY.
    character(len=:), allocatable :: mode
    !> Host network callbacks, registered with network.setProvider after connecting.
    type(network_options) :: network
  contains
    procedure :: set_defaults => client_options_set_defaults
  end type copilot_client_options

  ! --------------------------------------------------------------------------
  ! Session filesystem entry
  ! --------------------------------------------------------------------------
  type :: session_fs_entry
    character(len=:), allocatable :: path
    character(len=:), allocatable :: content
    logical :: is_directory = .false.
  end type session_fs_entry

  ! --------------------------------------------------------------------------
  ! Session filesystem configuration
  ! --------------------------------------------------------------------------
  type :: session_fs_config
    character(len=:), allocatable :: initial_cwd
    character(len=:), allocatable :: session_state_path
    character(len=:), allocatable :: conventions
  end type session_fs_config

  ! --------------------------------------------------------------------------
  ! MCP server configuration
  ! --------------------------------------------------------------------------
  type :: mcp_server_config
    character(len=:), allocatable :: command
    character(len=:), allocatable :: server_type
    character(len=:), allocatable :: url
    integer :: timeout = 0
  end type mcp_server_config

  ! --------------------------------------------------------------------------
  ! Command definition
  ! --------------------------------------------------------------------------
  type :: command_definition
    character(len=:), allocatable :: name
    character(len=:), allocatable :: description
  end type command_definition

  ! --------------------------------------------------------------------------
  ! Image options
  ! --------------------------------------------------------------------------
  type :: image_options
    character(len=:), allocatable :: size
    character(len=:), allocatable :: quality
    character(len=:), allocatable :: style
  end type image_options

  ! --------------------------------------------------------------------------
  ! Per-session AI-credit spending limits (SessionLimitsConfig)
  ! --------------------------------------------------------------------------
  type :: session_limits_config
    integer :: max_ai_credits = 0
  end type session_limits_config

  ! --------------------------------------------------------------------------
  ! Persistent session memory configuration (MemoryConfiguration)
  ! --------------------------------------------------------------------------
  type :: memory_configuration
    logical :: enabled = .false.
  end type memory_configuration

  ! --------------------------------------------------------------------------
  ! Arguments passed to a BYOK bearer_token_provider (per-session scoping)
  ! --------------------------------------------------------------------------
  type :: provider_token_args
    character(len=:), allocatable :: session_id
  end type provider_token_args

  ! --------------------------------------------------------------------------
  ! GitHub token provider (gitHubToken.getToken)
  ! --------------------------------------------------------------------------
  !> Arguments passed to session_config%github_token_provider.
  type :: github_token_provider_args
    !> Effective GitHub host the token is for.
    character(len=:), allocatable :: host
    character(len=:), allocatable :: session_id
    !> Why the runtime requests a credential.
    character(len=:), allocatable :: reason
  end type github_token_provider_args

  !> Token returned by a GitHub token provider. Set cancelled to decline.
  type :: github_token_result
    logical :: cancelled = .false.
    character(len=:), allocatable :: access_token
    !> OAuth token type (bearer when unallocated).
    character(len=:), allocatable :: token_type
    !> Remaining lifetime in seconds; must exceed one hour.
    integer :: expires_in = 28800
  end type github_token_result

  ! --------------------------------------------------------------------------
  ! Sub-agent lifecycle hooks (hooks.invoke subagentStart / subagentStop)
  ! --------------------------------------------------------------------------
  type :: subagent_start_input
    character(len=:), allocatable :: session_id
    !> Working directory (wire: cwd).
    character(len=:), allocatable :: working_directory
    character(len=:), allocatable :: transcript_path
    character(len=:), allocatable :: agent_name
    character(len=:), allocatable :: agent_display_name
    character(len=:), allocatable :: agent_description
    character(len=:), allocatable :: agent_id
    character(len=:), allocatable :: agent_type
    character(len=:), allocatable :: parent_agent_id
    !> The complete hook input as a JSON object string.
    character(len=:), allocatable :: input_json
  end type subagent_start_input

  type, extends(subagent_start_input) :: subagent_stop_input
    character(len=:), allocatable :: stop_reason
    !> The child's last assistant message.
    character(len=:), allocatable :: response
  end type subagent_stop_input

  !> Context prepended to the child's initial prompt.
  type :: subagent_start_output
    character(len=:), allocatable :: additional_context
  end type subagent_start_output

  !> decision 'block' with a reason continues the child; modified_response rewrites its answer.
  type :: subagent_stop_output
    character(len=:), allocatable :: decision
    character(len=:), allocatable :: reason
    character(len=:), allocatable :: modified_response
  end type subagent_stop_output

  ! --------------------------------------------------------------------------
  ! Sandbox, image generation, autopilot, and model limit options
  ! --------------------------------------------------------------------------
  !> Sandbox configuration (wire: sandboxConfig -> permissions.sandbox).
  type :: sandbox_config
    logical :: enabled = .true.
    !> Let the agent ask the host to approve a run outside the sandbox (wire: allowBypass).
    logical :: allow_bypass = .false.
    !> Optional user policy fragment as a JSON object string (wire: userPolicy).
    character(len=:), allocatable :: user_policy_json
  end type sandbox_config

  !> Image generation options (wire: imageGeneration); unallocated members are omitted.
  type :: image_generation_config
    logical, allocatable :: enabled
    logical, allocatable :: watermark
    logical, allocatable :: save_to_disk
  end type image_generation_config

  !> Autopilot continuation options (wire: autopilot); unallocated members are omitted.
  type :: autopilot_options
    logical, allocatable :: continuation_enabled
    integer, allocatable :: max_continues
  end type autopilot_options

  !> Model limit overrides sent as modelCapabilities.limits; zero means unset.
  type :: model_limits_override
    integer :: max_prompt_tokens = 0
    integer :: max_context_window_tokens = 0
    integer :: max_output_tokens = 0
  end type model_limits_override

  ! --------------------------------------------------------------------------
  ! BYOK providers
  ! --------------------------------------------------------------------------
  !> Legacy single BYOK provider (wire: provider, translated to a named "default" provider).
  type :: byok_provider_config
    !> Provider type, e.g. 'openai', 'azure', 'anthropic' (wire: type).
    character(len=:), allocatable :: provider_type
    character(len=:), allocatable :: base_url
    character(len=:), allocatable :: api_key
    !> Command whose output is used as the API key (wire: apiKeyCommand).
    character(len=:), allocatable :: api_key_command
    character(len=:), allocatable :: bearer_token
    character(len=:), allocatable :: wire_api
    character(len=:), allocatable :: azure_api_version
    !> Extra HTTP headers as a JSON object string.
    character(len=:), allocatable :: headers_json
    character(len=:), allocatable :: model_id
    character(len=:), allocatable :: wire_model
    integer :: max_prompt_tokens = 0
    integer :: max_output_tokens = 0
  end type byok_provider_config

  !> Named BYOK provider (wire: providers[] -> inference.providers).
  type :: named_provider_config
    character(len=:), allocatable :: name
    character(len=:), allocatable :: provider_type
    character(len=:), allocatable :: base_url
    character(len=:), allocatable :: api_key
    character(len=:), allocatable :: api_key_command
    character(len=:), allocatable :: bearer_token
    character(len=:), allocatable :: wire_api
    character(len=:), allocatable :: transport
    character(len=:), allocatable :: azure_api_version
    character(len=:), allocatable :: headers_json
    character(len=:), allocatable :: model_provider
  end type named_provider_config

  !> A model served by a named provider (wire: models[]); select it as 'provider/id'.
  type :: provider_model_config
    character(len=:), allocatable :: provider
    character(len=:), allocatable :: id
    character(len=:), allocatable :: model_id
    character(len=:), allocatable :: wire_model
    character(len=:), allocatable :: name
    integer :: max_prompt_tokens = 0
    integer :: max_context_window_tokens = 0
    integer :: max_output_tokens = 0
    !> Capability overrides as a JSON object string.
    character(len=:), allocatable :: capabilities_json
  end type provider_model_config

  ! --------------------------------------------------------------------------
  ! Shell authentication, memory store, and shell output
  ! --------------------------------------------------------------------------
  !> A shell command's interactive authentication prompt (shellAuthentication.request).
  type :: shell_authentication_request
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: request_id
    character(len=:), allocatable :: program
    character(len=:), allocatable :: prompt
    !> SHELL_AUTH_INPUT_SECRET, SHELL_AUTH_INPUT_TEXT, or SHELL_AUTH_INPUT_CONFIRM.
    character(len=:), allocatable :: input_kind
    character(len=:), allocatable :: command
    character(len=:), allocatable :: cwd
  end type shell_authentication_request

  !> Answer to a shell authentication prompt: accepted with a value, or cancelled.
  type :: shell_authentication_result
    logical :: accepted = .false.
    character(len=:), allocatable :: value
  end type shell_authentication_result

  !> A fact the agent asks the host to remember (memoryStore.store).
  type :: stored_memory
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: subject
    character(len=:), allocatable :: fact
    character(len=:), allocatable :: citations
    character(len=:), allocatable :: reason
  end type stored_memory

  !> Data of a tool.shell_output event.
  type :: tool_shell_output_data
    character(len=:), allocatable :: tool_call_id
    character(len=:), allocatable :: text
    integer :: sequence = 0
    !> 'stdout' or 'stderr'; unallocated when the runtime omits it.
    character(len=:), allocatable :: stream
  end type tool_shell_output_data

  ! --------------------------------------------------------------------------
  ! Managed policy
  ! --------------------------------------------------------------------------
  !> Policy Helper failure reported in error data (error.data.policyHelperFailure).
  !! Helper-authored text is untrusted: render it as plain text only.
  type :: policy_helper_failure
    character(len=:), allocatable :: category
    character(len=:), allocatable :: failure_mode
    character(len=:), allocatable :: message
    logical :: previous_policy_retained = .false.
    character(len=:), allocatable :: reason
    character(len=:), allocatable :: authority
    character(len=:), allocatable :: code
    !> Field-specific findings as a JSON array string.
    character(len=:), allocatable :: details_json
    character(len=:), allocatable :: diagnostic
    !> Help link object ({url, label}) as a JSON object string.
    character(len=:), allocatable :: help_link_json
    character(len=:), allocatable :: remediation
    character(len=:), allocatable :: severity
  end type policy_helper_failure

  !> Result of session%set_permission_mode.
  type :: permission_mode_set_result
    logical :: success = .false.
    character(len=:), allocatable :: mode
    !> Why the mode was refused, when known (PERMISSION_MODE_REFUSAL_MANAGED_POLICY).
    character(len=:), allocatable :: reason
  end type permission_mode_set_result

  ! --------------------------------------------------------------------------
  ! Ask-user requests and image attachments (userInput.request)
  ! --------------------------------------------------------------------------
  !> Image support advertised by a userInput.request (imageAttachmentConstraints).
  type :: user_input_image_attachment_constraints
    !> Accepted image MIME types (blank padded; use trim).
    character(len=:), allocatable :: allowed_mime_types(:)
    integer :: max_bytes_per_image = 0
    integer :: max_count = 0
  end type user_input_image_attachment_constraints

  !> A question from the agent's ask_user tool (userInput.request).
  type :: user_input_request
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: question
    !> Suggested answers (blank padded; use trim). Zero-size when the runtime sends none.
    character(len=:), allocatable :: choices(:)
    logical :: allow_freeform = .true.
    !> Pending request id; unallocated when the runtime omits it.
    character(len=:), allocatable :: request_id
    !> Allocated when the runtime accepts image attachments in the answer.
    type(user_input_image_attachment_constraints), allocatable :: image_attachment_constraints
    !> The request's opaque _meta object as JSON; unallocated when absent.
    character(len=:), allocatable :: meta_json
  end type user_input_request

  !> An inline image attached to an ask_user answer (sent with type "image").
  type :: user_input_image_attachment
    !> Base64-encoded image bytes.
    character(len=:), allocatable :: data
    character(len=:), allocatable :: mime_type
    character(len=:), allocatable :: description
    character(len=:), allocatable :: display_name
  end type user_input_image_attachment

  !> Answer to a userInput.request. attachments is only valid when the request advertised
  !! image_attachment_constraints and is sent only when allocated; an image-only answer uses
  !! an empty answer and was_freeform = .true.
  type :: user_input_response
    character(len=:), allocatable :: answer
    logical :: was_freeform = .false.
    type(user_input_image_attachment), allocatable :: attachments(:)
  end type user_input_response

  ! --------------------------------------------------------------------------
  ! MCP secret store and plugin extension approvals (Copilot CLI 1.0.116+)
  ! --------------------------------------------------------------------------
  !> Result of mcp.secrets.save.
  type :: mcp_secrets_save_result
    !> .true. when the OS keychain stored the credential, .false. for the file backend.
    logical :: used_keychain = .false.
  end type mcp_secrets_save_result

  !> A user-scoped approval for one installed plugin extension.
  type :: permissions_extension_approval
    !> Stable identity, for example 'plugin:plugin-name:extension-name'.
    character(len=:), allocatable :: extension_name
    character(len=:), allocatable :: plugin_name
    !> Exact privileged capabilities the user approved (blank padded; use trim).
    character(len=:), allocatable :: capabilities(:)
    !> RFC 3339 timestamp.
    character(len=:), allocatable :: granted_at
    !> Opaque installation identity; pass it back unchanged to revoke.
    character(len=:), allocatable :: installation_key
    !> Human-readable installation source; unallocated when absent.
    character(len=:), allocatable :: installation_label
  end type permissions_extension_approval

  !> Result of permissions.extensionApprovals.grant.
  type :: permissions_extension_approvals_grant_result
    !> Whether the pending request was eligible and its approval persisted.
    logical :: granted = .false.
    !> The persisted approval; allocated when granted.
    type(permissions_extension_approval), allocatable :: approval
  end type permissions_extension_approvals_grant_result

  !> Result of permissions.extensionApprovals.list.
  type :: permissions_extension_approvals_list_result
    type(permissions_extension_approval), allocatable :: approvals(:)
  end type permissions_extension_approvals_list_result

  !> Result of permissions.extensionApprovals.revoke.
  type :: permissions_extension_approvals_revoke_result
    !> Whether a matching approval existed and was removed.
    logical :: revoked = .false.
  end type permissions_extension_approvals_revoke_result

  ! --------------------------------------------------------------------------
  ! Session configuration
  ! --------------------------------------------------------------------------
  type :: session_config
    character(len=:), allocatable :: model
    character(len=:), allocatable :: system_prompt
    character(len=:), allocatable :: github_token
    character(len=:), allocatable :: workspace_path
    character(len=:), allocatable :: session_id
    type(copilot_tool), allocatable :: tools(:)
    type(session_fs_config) :: fs
    logical :: streaming = .true.
    integer :: session_idle_timeout_seconds = 0
    character(len=:), allocatable :: skill_directories(:)
    character(len=:), allocatable :: disabled_skills(:)
    character(len=:), allocatable :: excluded_tools(:)
    logical :: include_sub_agent_streaming_events = .false.
    logical :: enable_config_discovery = .false.
    character(len=:), allocatable :: model_capabilities_json
    character(len=:), allocatable :: request_headers_json
    character(len=:), allocatable :: mcp_servers_json
    type(command_definition), allocatable :: commands(:)
    type(image_options) :: img_options
    character(len=:), allocatable :: response_format
    character(len=:), allocatable :: instruction_directories(:)
    ! --- Upstream-sync session options (parity with @github/copilot-sdk) ---
    logical :: enable_citations = .false.
    character(len=:), allocatable :: excluded_builtin_agents(:)
    type(session_limits_config) :: session_limits
    type(memory_configuration) :: memory
    character(len=:), allocatable :: otlp_protocol
    logical :: enable_web_socket_responses = .false.
    character(len=:), allocatable :: exp_assignments_json
    !> Post-tool-use hook (inspect/override a tool result).
    procedure(post_tool_use_hook_interface), pointer, nopass :: on_post_tool_use => null()
    !> Pre-MCP-tool-call hook (gate an MCP server tool call).
    procedure(pre_mcp_tool_call_hook_interface), pointer, nopass :: on_pre_mcp_tool_call => null()
    !> MCP OAuth host token handler; when set, session.create signals mcpAuthHandler=true.
    procedure(mcp_auth_handler_interface), pointer, nopass :: on_mcp_auth_request => null()
    ! --- Newly-synced session options (parity with @github/copilot-sdk) ---
    !> Enable session telemetry (wire: enableSessionTelemetry).
    logical :: enable_session_telemetry = .false.
    !> Enable conversational rewind (restore an earlier turn) (wire: rewindEnabled).
    logical :: rewind_enabled = .false.
    !> Extra workspace directories to expose (wire: additionalDirectories).
    character(len=:), allocatable :: additional_directories(:)
    !> MCP servers to disable for this session (wire: disabledMcpServers).
    character(len=:), allocatable :: disabled_mcp_servers(:)
    !> GitHub MCP tool config as a JSON string (wire: githubMcpToolConfig).
    character(len=:), allocatable :: github_mcp_tool_config_json
    !> Canvas provider config as a JSON string (wire: canvasProvider).
    character(len=:), allocatable :: canvas_provider_json
    !> Restrict custom agents to locally-defined ones only (wire: customAgentsLocalOnly).
    logical :: custom_agents_local_only = .false.
    !> Permission-reply decision context as a JSON string (wire: decisionContext).
    character(len=:), allocatable :: decision_context_json
    !> Built-in plugin directories to load (wire: builtinPluginDirectories).
    character(len=:), allocatable :: builtin_plugin_directories(:)
    !> Agent-factory authoring arguments schema as a JSON string (wire: argsSchema).
    character(len=:), allocatable :: args_schema_json
    !> Reasoning effort control, e.g. 'low'/'medium'/'high' (wire: reasoningEffort).
    character(len=:), allocatable :: reasoning_effort
    !> Tool-search configuration as a JSON string (wire: toolSearch).
    character(len=:), allocatable :: tool_search_json
    !> Use the in-process FFI transport instead of a spawned CLI (wire: inProcess).
    logical :: in_process = .false.
    !> Enable experimental mode features (wire: experimentalMode).
    logical :: experimental_mode = .false.
    !> Enable content exclusion policy enforcement (wire: contentExclusion).
    logical :: content_exclusion = .false.
    !> User-prompt-transformed hook; when set, session.create signals userPromptTransformed=true.
    procedure(post_tool_use_hook_interface), pointer, nopass :: on_user_prompt_transformed => null()
    ! --- v2.6.0 session options (parity with @github/copilot-sdk) ---
    !> Auto routing preference for the 'auto' model (AUTO_TIER_* constants; wire: capi.autoTier).
    character(len=:), allocatable :: auto_tier
    !> Ask-user tool variant (ASK_USER_VARIANT_* constants; wire: askUserVariant).
    character(len=:), allocatable :: ask_user_variant
    !> GitHub token provider. The SDK registers it under a generated registration id and
    !! answers gitHubToken.getToken. Mutually exclusive with github_token.
    procedure(github_token_provider_interface), pointer, nopass :: github_token_provider => null()
    !> Sandbox configuration (wire: sandboxConfig).
    type(sandbox_config), allocatable :: sandbox
    !> Feature flag overrides as a JSON object string, e.g. '{"MY_FLAG":true}' (wire: featureFlags).
    character(len=:), allocatable :: feature_flags_json
    !> Exact model ids the host allows (wire: allowedModels).
    character(len=:), allocatable :: allowed_models(:)
    !> OAuth client id metadata document URL for MCP authorization (wire: authClientIdMetadataUrl).
    character(len=:), allocatable :: auth_client_id_metadata_url
    !> Model limit overrides merged into modelCapabilities.limits.
    type(model_limits_override) :: model_limits
    !> Extra agent definition directories (wire: agentDirectories).
    character(len=:), allocatable :: agent_directories(:)
    !> Extra hook definition directories (wire: hookDirectories).
    character(len=:), allocatable :: hook_directories(:)
    !> Create-only directory trust grant (WORKING_DIRECTORY_TRUST_* constants).
    character(len=:), allocatable :: trust_working_directory
    !> Resume-only: allow recovery of a damaged transcript (wire: allowTranscriptRecovery).
    logical, allocatable :: allow_transcript_recovery
    !> Image generation options (wire: imageGeneration).
    type(image_generation_config), allocatable :: image_generation
    !> Autopilot continuation options (wire: autopilot).
    type(autopilot_options), allocatable :: autopilot
    !> Legacy single BYOK provider (wire: provider).
    type(byok_provider_config), allocatable :: provider
    !> Sub-agent lifecycle hooks, invoked through hooks.invoke.
    procedure(subagent_start_hook_interface), pointer, nopass :: on_subagent_start => null()
    procedure(subagent_stop_hook_interface), pointer, nopass :: on_subagent_stop => null()
    ! --- v3.0.0 session options (SDK protocol 4) ---
    !> Named BYOK providers (wire: providers).
    type(named_provider_config), allocatable :: providers(:)
    !> Models served by named providers (wire: models).
    type(provider_model_config), allocatable :: models(:)
    !> Default named provider (wire: defaultProvider).
    character(len=:), allocatable :: default_provider
    !> Resume-only: clear a previously set default provider (sends defaultProvider: null).
    logical :: clear_default_provider = .false.
    !> Enable or disable Copilot API models independently of BYOK (wire: capi.enabled).
    logical, allocatable :: capi_enabled
    !> Tools the session may use (wire: availableTools; required in client mode 'empty').
    character(len=:), allocatable :: available_tools(:)
    !> Resume-only: do not emit the session.resume event (wire: suppressResumeEvent).
    logical :: suppress_resume_event = .false.
    !> Shell authentication prompt handler (enables interaction.requestShellAuthentication).
    procedure(shell_authentication_handler_interface), pointer, nopass :: on_shell_authentication_request => null()
    !> Memory store callback (enables state.hasMemoryStore).
    procedure(store_memory_handler_interface), pointer, nopass :: on_store_memory => null()
    ! --- v3.1.0 session options ---
    !> Ask-user handler answering userInput.request (enables interaction.requestUserInput).
    !! The request carries image_attachment_constraints when the runtime accepts images.
    procedure(user_input_handler_interface), pointer, nopass :: on_user_input_request => null()
  end type session_config

  ! --------------------------------------------------------------------------
  ! Session metadata
  ! --------------------------------------------------------------------------
  type :: session_metadata
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: model
    character(len=:), allocatable :: status
    character(len=:), allocatable :: created_at
    integer :: message_count = 0
  end type session_metadata

  ! --------------------------------------------------------------------------
  ! Session event
  ! --------------------------------------------------------------------------
  type :: session_event
    character(len=:), allocatable :: event_type
    character(len=:), allocatable :: id
    character(len=:), allocatable :: timestamp
    character(len=:), allocatable :: parent_id
    character(len=:), allocatable :: agent_id
    logical :: ephemeral = .false.
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: data_json
  end type session_event

  ! --------------------------------------------------------------------------
  ! Send options
  ! --------------------------------------------------------------------------
  type :: send_options
    character(len=:), allocatable :: message
    character(len=:), allocatable :: command
    character(len=:), allocatable :: model
    !> Per-send agent mode override (wire key: agentMode)
    character(len=:), allocatable :: agent_mode
    !> Prompt text shown to the user instead of the real prompt (wire key: displayPrompt)
    character(len=:), allocatable :: display_prompt
    !> Custom HTTP headers for this send as a JSON object string (wire key: requestHeaders)
    character(len=:), allocatable :: request_headers_json
    !> Message provenance: MESSAGE_SOURCE_USER, MESSAGE_SOURCE_SYSTEM, or
    !! message_source_agent(id) (wire key: source)
    character(len=:), allocatable :: source
    !> JSON schema (object string) for structured output (wire key: responseFormat)
    character(len=:), allocatable :: response_schema_json
    !> Opaque caller-chosen turn identifier (wire key: clientTurnId)
    character(len=:), allocatable :: client_turn_id
  end type send_options

  ! --------------------------------------------------------------------------
  ! Send-and-wait result
  ! --------------------------------------------------------------------------
  type :: send_and_wait_result
    character(len=:), allocatable :: message
    character(len=:), allocatable :: error_message
    logical :: is_error = .false.
  end type send_and_wait_result

  ! --------------------------------------------------------------------------
  ! Tool invocation context
  ! --------------------------------------------------------------------------
  type :: tool_invocation
    character(len=:), allocatable :: invocation_id
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: arguments_json
  end type tool_invocation

  ! --------------------------------------------------------------------------
  ! Tool result
  ! --------------------------------------------------------------------------
  type :: tool_result
    character(len=:), allocatable :: content
    character(len=:), allocatable :: error_text
    logical :: is_error = .false.
  end type tool_result

  ! --------------------------------------------------------------------------
  ! Tool definition
  ! --------------------------------------------------------------------------
  type :: copilot_tool
    character(len=:), allocatable :: name
    character(len=:), allocatable :: description
    character(len=:), allocatable :: input_schema_json
    !> Defer loading policy: 'auto' (lazy) or 'never' (eager). See TOOL_DEFER_* constants.
    character(len=:), allocatable :: defer
    !> The tool is safe to retry: repeated calls with the same arguments have no extra effect.
    logical :: idempotent = .false.
    procedure(tool_callback_interface), pointer, nopass :: callback => null()
  end type copilot_tool

  ! --------------------------------------------------------------------------
  ! Permission request / result
  ! --------------------------------------------------------------------------
  type :: permission_request
    character(len=:), allocatable :: kind
    character(len=:), allocatable :: description
    character(len=:), allocatable :: resource
    character(len=:), allocatable :: session_id
  end type permission_request

  type :: permission_result
    logical :: granted = .false.
    character(len=:), allocatable :: reason
  end type permission_result

  ! --------------------------------------------------------------------------
  ! Elicitation request / result
  ! --------------------------------------------------------------------------
  type :: elicitation_request
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: message
    character(len=:), allocatable :: mode
    character(len=:), allocatable :: schema_json
  end type elicitation_request

  type :: elicitation_result
    character(len=:), allocatable :: action
    character(len=:), allocatable :: content_json
  end type elicitation_result

  ! --------------------------------------------------------------------------
  ! Exit plan mode request / response
  ! --------------------------------------------------------------------------
  type :: exit_plan_mode_request_t
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: summary
    character(len=:), allocatable :: plan_content
    character(len=:), allocatable :: recommended_action
  end type exit_plan_mode_request_t

  type :: exit_plan_mode_response_t
    logical :: approved = .false.
    character(len=:), allocatable :: selected_action
    character(len=:), allocatable :: feedback
  end type exit_plan_mode_response_t

  ! --------------------------------------------------------------------------
  ! Abstract interfaces for callbacks
  ! --------------------------------------------------------------------------
  abstract interface
    function tool_callback_interface(args, invocation) result(res)
      import :: tool_invocation, tool_result
      character(len=*), intent(in) :: args
      type(tool_invocation), intent(in) :: invocation
      type(tool_result) :: res
    end function tool_callback_interface

    !> BYOK bearer token provider: mint a per-session bearer token.
    function bearer_token_provider_interface(args) result(token)
      import :: provider_token_args
      type(provider_token_args), intent(in) :: args
      character(len=:), allocatable :: token
    end function bearer_token_provider_interface

    !> HTTP request handler: intercept/mutate an outbound LLM inference request
    !! (JSON string in, possibly-modified JSON string out).
    function copilot_request_handler_interface(request_json) result(modified_json)
      character(len=*), intent(in) :: request_json
      character(len=:), allocatable :: modified_json
    end function copilot_request_handler_interface

    !> MCP OAuth host token handler: return an OAuth token for an MCP server request.
    function mcp_auth_handler_interface(session_id, server_url) result(token)
      character(len=*), intent(in) :: session_id, server_url
      character(len=:), allocatable :: token
    end function mcp_auth_handler_interface

    !> Post-tool-use hook: inspect a tool result and optionally return an override.
    function post_tool_use_hook_interface(tool_name, tool_result_json, session_id) result(override)
      character(len=*), intent(in) :: tool_name, tool_result_json, session_id
      character(len=:), allocatable :: override
    end function post_tool_use_hook_interface

    !> Pre-MCP-tool-call hook: gate an MCP server tool call, returning a decision.
    function pre_mcp_tool_call_hook_interface(server_name, tool_name, tool_args, session_id) result(decision)
      character(len=*), intent(in) :: server_name, tool_name, tool_args, session_id
      character(len=:), allocatable :: decision
    end function pre_mcp_tool_call_hook_interface

    !> GitHub token provider: return a token for args%host, or set cancelled.
    function github_token_provider_interface(args) result(token)
      import :: github_token_provider_args, github_token_result
      type(github_token_provider_args), intent(in) :: args
      type(github_token_result) :: token
    end function github_token_provider_interface

    !> Sub-agent start hook: optionally return context for the child's first prompt.
    function subagent_start_hook_interface(input) result(output)
      import :: subagent_start_input, subagent_start_output
      type(subagent_start_input), intent(in) :: input
      type(subagent_start_output) :: output
    end function subagent_start_hook_interface

    !> Sub-agent stop hook: optionally block (continue the child) or rewrite its response.
    function subagent_stop_hook_interface(input) result(output)
      import :: subagent_stop_input, subagent_stop_output
      type(subagent_stop_input), intent(in) :: input
      type(subagent_stop_output) :: output
    end function subagent_stop_hook_interface

    !> Shell authentication handler: accept with a value or cancel.
    function shell_authentication_handler_interface(request) result(response)
      import :: shell_authentication_request, shell_authentication_result
      type(shell_authentication_request), intent(in) :: request
      type(shell_authentication_result) :: response
    end function shell_authentication_handler_interface

    !> Memory store callback: persist a fact the agent wants remembered.
    subroutine store_memory_handler_interface(memory)
      import :: stored_memory
      type(stored_memory), intent(in) :: memory
    end subroutine store_memory_handler_interface

    !> Proxy resolution: return the proxy URL for url, or '' to connect directly.
    function network_resolve_proxy_interface(url) result(proxy)
      character(len=*), intent(in) :: url
      character(len=:), allocatable :: proxy
    end function network_resolve_proxy_interface

    !> Negotiate (Kerberos/NTLM) proxy authentication: return a token for spn, or ''.
    function network_authenticate_negotiate_interface(spn) result(token)
      character(len=*), intent(in) :: spn
      character(len=:), allocatable :: token
    end function network_authenticate_negotiate_interface

    !> Basic proxy authentication: return credentials, or leave them unallocated.
    function network_authenticate_basic_interface(request) result(credentials)
      import :: network_basic_auth_request, network_basic_credentials
      type(network_basic_auth_request), intent(in) :: request
      type(network_basic_credentials) :: credentials
    end function network_authenticate_basic_interface

    !> Ask-user handler: answer a userInput.request (see user_input_response).
    function user_input_handler_interface(request) result(response)
      import :: user_input_request, user_input_response
      type(user_input_request), intent(in) :: request
      type(user_input_response) :: response
    end function user_input_handler_interface
  end interface

contains

  !> Set sensible defaults for client options.
  subroutine client_options_set_defaults(self)
    class(copilot_client_options), intent(inout) :: self

    if (.not. allocated(self%log_level)) then
      self%log_level = 'info'
    end if
    if (.not. allocated(self%cwd)) then
      self%cwd = '.'
    end if
    self%auto_start = .true.
    self%use_stdio  = .true.
  end subroutine client_options_set_defaults

  !> Message source for a message sent on behalf of a sub-agent ('agent-<id>').
  function message_source_agent(agent_id) result(source)
    character(len=*), intent(in) :: agent_id
    character(len=:), allocatable :: source

    source = 'agent-' // trim(agent_id)
  end function message_source_agent

  !> Normalize a client mode: unset and the legacy 'copilot-cli' alias become 'coding-agent'.
  !! Returns '' for an unknown mode.
  function normalize_client_mode(mode) result(normalized)
    character(len=*), intent(in) :: mode
    character(len=:), allocatable :: normalized

    if (len_trim(mode) == 0 .or. mode == CLIENT_MODE_CODING_AGENT .or. &
        mode == CLIENT_MODE_COPILOT_CLI) then
      normalized = CLIENT_MODE_CODING_AGENT
    else if (mode == CLIENT_MODE_EMPTY) then
      normalized = CLIENT_MODE_EMPTY
    else
      normalized = ''
    end if
  end function normalize_client_mode

  !> Read the tool.shell_output data from a session.event notification or a bare event object.
  subroutine parse_tool_shell_output(event_json, output, found)
    character(len=*), intent(in) :: event_json
    type(tool_shell_output_data), intent(out) :: output
    logical, intent(out) :: found
    character(len=:), allocatable :: event, raw, text
    logical :: is_set

    found = .false.
    call json_get_path(event_json, 'params.event', event, is_set)
    if (.not. is_set) event = event_json
    if (json_get_string(event, 'type') /= EVENT_TOOL_SHELL_OUTPUT) return
    call json_get_raw(event, 'data', raw, is_set)
    if (.not. is_set) return
    output%tool_call_id = json_get_string(raw, 'toolCallId')
    output%text = json_get_string(raw, 'text')
    call json_get_integer(raw, 'sequence', output%sequence, is_set)
    text = json_get_string(raw, 'stream', is_set)
    if (is_set) output%stream = text
    found = .true.
  end subroutine parse_tool_shell_output

  !> Read the Policy Helper failure from a managed settings snapshot (policyHelperFailure),
  !! a JSON-RPC error object (data.policyHelperFailure), or a full error response
  !! (error.data.policyHelperFailure).
  subroutine parse_policy_helper_failure(error_json, failure, found)
    character(len=*), intent(in) :: error_json
    type(policy_helper_failure), intent(out) :: failure
    logical, intent(out) :: found
    character(len=:), allocatable :: raw, text
    logical :: is_set

    call json_get_raw(error_json, 'policyHelperFailure', raw, found)
    if (.not. found) call json_get_path(error_json, 'data.policyHelperFailure', raw, found)
    if (.not. found) call json_get_path(error_json, 'error.data.policyHelperFailure', raw, found)
    if (found) found = json_is_object(raw)
    if (.not. found) return

    failure%category = json_get_string(raw, 'category')
    failure%failure_mode = json_get_string(raw, 'failureMode')
    failure%message = json_get_string(raw, 'message')
    failure%previous_policy_retained = json_get_logical(raw, 'previousPolicyRetained', .false.)
    failure%reason = json_get_string(raw, 'reason')
    text = json_get_string(raw, 'authority', is_set)
    if (is_set) failure%authority = text
    text = json_get_string(raw, 'code', is_set)
    if (is_set) failure%code = text
    call json_get_raw(raw, 'details', text, is_set)
    if (is_set) failure%details_json = text
    text = json_get_string(raw, 'diagnostic', is_set)
    if (is_set) failure%diagnostic = text
    call json_get_raw(raw, 'helpLink', text, is_set)
    if (is_set) failure%help_link_json = text
    text = json_get_string(raw, 'remediation', is_set)
    if (is_set) failure%remediation = text
    text = json_get_string(raw, 'severity', is_set)
    if (is_set) failure%severity = text
  end subroutine parse_policy_helper_failure

  !> Read the params of a userInput.request, including imageAttachmentConstraints (only when
  !! the runtime accepts images), requestId, and the opaque _meta object.
  subroutine parse_user_input_request(params, request)
    character(len=*), intent(in) :: params
    type(user_input_request), intent(out) :: request
    character(len=:), allocatable :: raw, text
    logical :: found

    request%session_id = json_get_string(params, 'sessionId')
    request%question = json_get_string(params, 'question')
    call json_get_string_array(params, 'choices', request%choices)
    request%allow_freeform = json_get_logical(params, 'allowFreeform', .true.)
    text = json_get_string(params, 'requestId', found)
    if (found) request%request_id = text
    call json_get_raw(params, 'imageAttachmentConstraints', raw, found)
    if (found) found = json_is_object(raw)
    if (found) then
      allocate(request%image_attachment_constraints)
      call json_get_string_array(raw, 'allowedMimeTypes', &
                                 request%image_attachment_constraints%allowed_mime_types)
      call json_get_integer(raw, 'maxBytesPerImage', &
                            request%image_attachment_constraints%max_bytes_per_image, found)
      call json_get_integer(raw, 'maxCount', request%image_attachment_constraints%max_count, found)
    end if
    call json_get_raw(params, '_meta', raw, found)
    if (found) found = json_is_object(raw)
    if (found) request%meta_json = raw
  end subroutine parse_user_input_request

  !> Result of a userInput.request: answer and wasFreeform, plus attachments only when
  !! allocated (each image as {"type":"image","data","mimeType"} with optional description
  !! and displayName).
  function user_input_response_json(response) result(json)
    type(user_input_response), intent(in) :: response
    character(len=:), allocatable :: json
    integer :: i

    json = '{"answer":'
    if (allocated(response%answer)) then
      json = json // json_quote(response%answer)
    else
      json = json // '""'
    end if
    if (response%was_freeform) then
      json = json // ',"wasFreeform":true'
    else
      json = json // ',"wasFreeform":false'
    end if
    if (allocated(response%attachments)) then
      json = json // ',"attachments":['
      do i = 1, size(response%attachments)
        if (i > 1) json = json // ','
        json = json // user_input_image_attachment_json(response%attachments(i))
      end do
      json = json // ']'
    end if
    json = json // '}'
  end function user_input_response_json

  !> One image attachment of a user input response.
  function user_input_image_attachment_json(attachment) result(json)
    type(user_input_image_attachment), intent(in) :: attachment
    character(len=:), allocatable :: json

    json = '{"type":"image","data":'
    if (allocated(attachment%data)) then
      json = json // json_quote(attachment%data)
    else
      json = json // '""'
    end if
    json = json // ',"mimeType":'
    if (allocated(attachment%mime_type)) then
      json = json // json_quote(attachment%mime_type)
    else
      json = json // '""'
    end if
    if (allocated(attachment%description)) then
      json = json // ',"description":' // json_quote(attachment%description)
    end if
    if (allocated(attachment%display_name)) then
      json = json // ',"displayName":' // json_quote(attachment%display_name)
    end if
    json = json // '}'
  end function user_input_image_attachment_json

  !> Read a PermissionsExtensionApproval object.
  subroutine parse_permissions_extension_approval(raw, approval)
    character(len=*), intent(in) :: raw
    type(permissions_extension_approval), intent(out) :: approval
    character(len=:), allocatable :: text
    logical :: found

    approval%extension_name = json_get_string(raw, 'extensionName')
    approval%plugin_name = json_get_string(raw, 'pluginName')
    call json_get_string_array(raw, 'capabilities', approval%capabilities)
    approval%granted_at = json_get_string(raw, 'grantedAt')
    approval%installation_key = json_get_string(raw, 'installationKey')
    text = json_get_string(raw, 'installationLabel', found)
    if (found) approval%installation_label = text
  end subroutine parse_permissions_extension_approval

end module copilot_types
