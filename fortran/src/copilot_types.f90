! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Derived types for the Copilot SDK.
!! Defines options, configuration, metadata, events, tools, and permissions.
module copilot_types
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

  type :: copilot_connection_state
    integer :: value = COPILOT_STATE_DISCONNECTED
  end type copilot_connection_state

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

end module copilot_types
