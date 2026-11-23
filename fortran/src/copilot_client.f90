! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Main Copilot client module.
!! Manages the CLI process lifecycle, JSON-RPC transport, and session creation.
module copilot_client_module
  use iso_fortran_env, only: error_unit, int64
  use copilot_types
  use copilot_json
  use copilot_jsonrpc
  use copilot_session_module
  use copilot_session_wire
  use copilot_version
  use copilot_tools
  implicit none
  private

  public :: copilot_client, copilot_client_create, copilot_client_destroy
  public :: copilot_start, copilot_stop, copilot_ping
  public :: copilot_create_session, copilot_resume_session
  public :: copilot_get_session_metadata, copilot_set_foreground_session
  public :: copilot_get_foreground_session, copilot_delete_session
  public :: copilot_list_sessions, copilot_get_status
  public :: copilot_get_auth_status, copilot_list_models
  public :: copilot_get_last_session_id
  public :: copilot_clear_managed_settings_cache, copilot_handle_server_request
  public :: copilot_verify_protocol_version, copilot_build_session_params
  public :: copilot_network_provider_params

  integer, parameter :: MAX_SESSIONS = 32

  !> Main client type -- holds connection state, options, and sessions.
  type :: copilot_client
    type(copilot_client_options) :: options
    type(jsonrpc_client) :: rpc
    type(copilot_connection_state) :: state
    type(copilot_session_type) :: sessions(MAX_SESSIONS)
    integer :: session_count = 0
    logical :: cli_process_running = .false.
    integer :: cli_pid = -1
    !> Normalized client mode (CLIENT_MODE_CODING_AGENT or CLIENT_MODE_EMPTY).
    character(len=:), allocatable :: mode
    !> Protocol version reported by the server after a successful handshake (0 before).
    integer :: negotiated_protocol_version = 0
    !> Message of the last error reported by a client call.
    character(len=:), allocatable :: last_error
    !> Policy Helper failure from the last failed request, when the error carried one
    !! (for example a session.create rejected by managed policy).
    type(policy_helper_failure) :: last_policy_helper_failure
    logical :: has_policy_helper_failure = .false.
  contains
    procedure :: start           => client_start
    procedure :: stop            => client_stop
    procedure :: ping            => client_ping
    procedure :: create_session  => client_create_session
    procedure :: resume_session  => client_resume_session
    procedure :: get_status      => client_get_status
    procedure :: get_auth_status => client_get_auth_status
    procedure :: list_models     => client_list_models
    procedure :: list_sessions   => client_list_sessions
    procedure :: set_foreground     => client_set_foreground
    procedure :: get_foreground     => client_get_foreground
    procedure :: delete_session     => client_delete_session
    procedure :: get_last_session_id => client_get_last_session_id
    procedure :: clear_managed_settings_cache => client_clear_managed_settings_cache
    procedure :: handle_server_request => client_handle_server_request
  end type copilot_client

contains

  ! ==========================================================================
  ! Constructor / Destructor
  ! ==========================================================================

  !> Create a client with the supplied options.
  subroutine copilot_client_create(client, options)
    type(copilot_client), intent(out) :: client
    type(copilot_client_options), intent(in), optional :: options

    if (present(options)) then
      client%options = options
    end if
    call client%options%set_defaults()

    client%state%value = COPILOT_STATE_DISCONNECTED
    client%session_count = 0
    client%cli_process_running = .false.
    client%cli_pid = -1

    if (.not. resolve_client_mode(client)) then
      client%state%value = COPILOT_STATE_ERROR
      write(error_unit, '(A)') 'copilot_client: ' // client%last_error
      return
    end if

    ! Auto-start if requested
    if (client%options%auto_start) then
      call client%start()
    end if
  end subroutine copilot_client_create

  !> Destroy the client and stop the CLI process.
  subroutine copilot_client_destroy(client)
    type(copilot_client), intent(inout) :: client
    call client%stop()
  end subroutine copilot_client_destroy

  ! ==========================================================================
  ! Process management
  ! ==========================================================================

  !> Start the CLI process and establish the JSON-RPC connection.
  subroutine client_start(self)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable :: cli_cmd
    integer :: ios

    if (self%state%value == COPILOT_STATE_CONNECTED) return
    if (.not. resolve_client_mode(self)) then
      self%state%value = COPILOT_STATE_ERROR
      return
    end if

    self%state%value = COPILOT_STATE_CONNECTING

    ! Build the CLI command
    cli_cmd = build_cli_command(self%options)

    ! Spawn the CLI process using execute_command_line (blocking placeholder).
    ! In production a pipe-based spawn via ISO_C_BINDING would be used.
    ! For now, we set up the RPC client with placeholder units.
    call jsonrpc_create(self%rpc, 5, 6)  ! stdin=5, stdout=6 as placeholders

    self%state%value = COPILOT_STATE_CONNECTED
    self%cli_process_running = .true.

    ! Protocol handshake (connect or ping) and the SDK protocol version check
    call client_handshake(self, ios)
    if (ios /= 0) then
      self%state%value = COPILOT_STATE_ERROR
      write(error_unit, '(A)') 'copilot_client: handshake failed after start'
    else
      ! Register host network callbacks, if any
      call register_network_provider(self, ios)
      if (ios /= 0) write(error_unit, '(A)') 'copilot_client: network.setProvider failed'
    end if

    ! Set up session filesystem provider if configured
    if (allocated(self%options%session_fs)) then
      if (allocated(self%options%session_fs%initial_cwd)) then
        block
          character(len=:), allocatable :: fs_params
          integer :: fs_req_id, fs_ios
          fs_params = '{"initialCwd":"' // self%options%session_fs%initial_cwd // '",' // &
                      '"sessionStatePath":"' // self%options%session_fs%session_state_path // '",' // &
                      '"conventions":"' // self%options%session_fs%conventions // '"}'
          call self%rpc%send_request('sessionFs.setProvider', fs_params, fs_req_id, fs_ios)
        end block
      end if
    end if
  end subroutine client_start

  !> Stop the CLI process and close the JSON-RPC connection.
  subroutine client_stop(self)
    class(copilot_client), intent(inout) :: self
    integer :: i, ios

    ! Destroy all active sessions
    do i = 1, self%session_count
      if (self%sessions(i)%is_active) then
        call self%sessions(i)%destroy(ios)
      end if
    end do
    self%session_count = 0

    call jsonrpc_destroy(self%rpc)
    self%state%value = COPILOT_STATE_DISCONNECTED
    self%cli_process_running = .false.
  end subroutine client_stop

  ! ==========================================================================
  ! Connection verification
  ! ==========================================================================

  !> Ping the CLI server and verify that it speaks SDK protocol version 4.
  !! A version mismatch is reported as iostat = COPILOT_IOSTAT_PROTOCOL_MISMATCH with the
  !! message in client%last_error.
  subroutine client_ping(self, iostat)
    class(copilot_client), intent(inout) :: self
    integer, intent(out) :: iostat
    character(len=:), allocatable :: response

    call client_request(self, 'ping', '{}', response, iostat)
    if (iostat /= 0) return
    call check_protocol_version(self, response, iostat)
  end subroutine client_ping

  !> Handshake: send connect (carrying clientInfo) when a client identity is configured,
  !! falling back to ping for servers without connect; otherwise ping. Then require the
  !! server to report exactly SDK protocol version 4.
  subroutine client_handshake(self, iostat)
    class(copilot_client), intent(inout) :: self
    integer, intent(out) :: iostat
    character(len=:), allocatable :: info, response, error_json
    integer :: req_id
    logical :: is_error

    info = client_info_json(self%options%client_info)
    if (len(info) == 0) then
      call self%ping(iostat)
      return
    end if
    call self%rpc%send_request('connect', '{"clientInfo":' // info // '}', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(response, iostat)
    if (iostat /= 0) return
    call jsonrpc_response_error(response, error_json, is_error)
    if (is_error) then
      if (jsonrpc_error_code(error_json) == JSONRPC_METHOD_NOT_FOUND .or. &
          jsonrpc_error_message(error_json) == 'Unhandled method connect') then
        ! Legacy server without connect
        call self%ping(iostat)
      else
        call record_rpc_error(self, error_json, iostat)
      end if
      return
    end if
    call check_protocol_version(self, response, iostat)
  end subroutine client_handshake

  !> Check the protocolVersion of a connect or ping response (exactly SDK_PROTOCOL_VERSION).
  !! Returns .false. with the mismatch message in error_message.
  function copilot_verify_protocol_version(response_json, error_message) result(ok)
    character(len=*), intent(in) :: response_json
    character(len=:), allocatable, intent(out) :: error_message
    logical :: ok
    character(len=:), allocatable :: result_json
    character(len=20) :: buf
    integer :: version
    logical :: found

    ok = .false.
    call json_get_raw(response_json, 'result', result_json, found)
    if (.not. found) result_json = response_json
    call json_get_integer(result_json, 'protocolVersion', version, found)
    if (.not. found) then
      error_message = 'SDK protocol version mismatch: SDK supports version ' // SDK_PROTOCOL_VERSION_TEXT // &
                      ', but server does not report a protocol version. ' // &
                      'Please update your SDK or server to ensure compatibility.'
      return
    end if
    if (version /= SDK_PROTOCOL_VERSION) then
      write(buf, '(I0)') version
      error_message = 'SDK protocol version mismatch: SDK supports version ' // SDK_PROTOCOL_VERSION_TEXT // &
                      ', but server reports version ' // trim(buf) // &
                      '. Please update your SDK or server to ensure compatibility.'
      return
    end if
    error_message = ''
    ok = .true.
  end function copilot_verify_protocol_version

  !> Record the negotiated version or report a protocol mismatch.
  subroutine check_protocol_version(self, response, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: response
    integer, intent(out) :: iostat
    character(len=:), allocatable :: message

    iostat = 0
    if (copilot_verify_protocol_version(response, message)) then
      self%negotiated_protocol_version = SDK_PROTOCOL_VERSION
    else
      self%last_error = message
      write(error_unit, '(A)') 'copilot_client: ' // message
      iostat = COPILOT_IOSTAT_PROTOCOL_MISMATCH
    end if
  end subroutine check_protocol_version

  !> Module-level convenience for ping.
  subroutine copilot_ping(client, iostat)
    type(copilot_client), intent(inout) :: client
    integer, intent(out) :: iostat
    call client%ping(iostat)
  end subroutine copilot_ping

  ! ==========================================================================
  ! Session management
  ! ==========================================================================

  !> Create a new session on the server and return a session object.
  !! The flat options are translated to the grouped SDK protocol 4 payload before sending.
  !! A failure is reported through iostat: client%last_error holds the message and
  !! client%last_policy_helper_failure the Policy Helper failure when the error carried one.
  subroutine client_create_session(self, config, session, iostat)
    class(copilot_client), intent(inout) :: self
    type(session_config), intent(in), optional :: config
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    character(len=:), allocatable :: params, response, sid, registration_id

    call prepare_session_params(self, config, .false., '', registration_id, params, iostat)
    if (iostat /= 0) return

    call client_request(self, 'session.create', params, response, iostat)
    if (iostat /= 0) return

    sid = extract_session_id(response)
    if (len_trim(sid) == 0) then
      sid = 'session-' // char(ichar('0') + mod(self%rpc%next_id - 1, 10))
    end if

    call copilot_session_create(session, sid, self%rpc)
    call session%configure(config, self%options%network, registration_id)
    call track_session(self, session)
  end subroutine client_create_session

  !> Resume an existing session by ID. The optional config carries the options to apply on
  !! resume, including the resume-only suppress_resume_event, allow_transcript_recovery, and
  !! clear_default_provider.
  subroutine client_resume_session(self, session_id, session, iostat, config)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: session_id
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    type(session_config), intent(in), optional :: config
    character(len=:), allocatable :: params, response, registration_id

    call prepare_session_params(self, config, .true., session_id, registration_id, params, iostat)
    if (iostat /= 0) return

    call client_request(self, 'session.resume', params, response, iostat)
    if (iostat /= 0) return

    call copilot_session_create(session, session_id, self%rpc)
    call session%configure(config, self%options%network, registration_id)
    call track_session(self, session)
  end subroutine client_resume_session

  !> Validate the session options and build the grouped create / resume params.
  subroutine prepare_session_params(self, config, is_resume, session_id, registration_id, params, iostat)
    class(copilot_client), intent(inout) :: self
    type(session_config), intent(in), optional :: config
    logical, intent(in) :: is_resume
    character(len=*), intent(in) :: session_id
    character(len=:), allocatable, intent(out) :: registration_id, params
    integer, intent(out) :: iostat
    logical :: has_available_tools

    iostat = 0
    registration_id = ''
    params = ''
    self%has_policy_helper_failure = .false.
    if (.not. resolve_client_mode(self)) then
      iostat = COPILOT_IOSTAT_INVALID_CONFIG
      return
    end if

    has_available_tools = .false.
    if (present(config)) then
      has_available_tools = allocated(config%available_tools)
      if (associated(config%github_token_provider)) then
        if (allocated(config%github_token)) then
          call config_error(self, 'github_token and github_token_provider are mutually exclusive', iostat)
          return
        end if
        registration_id = new_registration_id()
      end if
    end if
    if (self%mode == CLIENT_MODE_EMPTY .and. .not. has_available_tools) then
      call config_error(self, 'Client mode "empty" requires session_config%available_tools ' // &
                        'on every create and resume', iostat)
      return
    end if

    params = copilot_build_session_params(config, is_resume, session_id, self%mode, registration_id)
  end subroutine prepare_session_params

  !> Build the session.create (is_resume = .false.) or session.resume params for a session
  !! configuration: the flat options the SDK has always sent, the client mode "empty"
  !! defaults (mode = CLIENT_MODE_EMPTY), then group_session_params for the protocol 4 shape.
  function copilot_build_session_params(config, is_resume, session_id, mode, &
                                        github_token_registration_id) result(params)
    type(session_config), intent(in), optional :: config
    logical, intent(in) :: is_resume
    character(len=*), intent(in) :: session_id
    character(len=*), intent(in), optional :: mode
    character(len=*), intent(in), optional :: github_token_registration_id
    character(len=:), allocatable :: params
    character(len=:), allocatable :: members, flat

    members = ''
    if (len(session_id) > 0) members = '"sessionId":' // json_quote(session_id) // ','
    if (present(config)) members = members // flat_session_options(config, is_resume)
    if (present(github_token_registration_id)) then
      if (len(github_token_registration_id) > 0) then
        members = members // '"gitHubTokenProviderRegistrationId":' // &
                  json_quote(github_token_registration_id) // ','
      end if
    end if
    flat = object_from_members(members)
    if (present(mode)) then
      if (mode == CLIENT_MODE_EMPTY) flat = apply_empty_mode_defaults(flat)
    end if
    params = group_session_params(flat, is_resume)
  end function copilot_build_session_params

  !> The flat session options of a configuration, each member followed by a comma.
  function flat_session_options(config, is_resume) result(members)
    type(session_config), intent(in) :: config
    logical, intent(in) :: is_resume
    character(len=:), allocatable :: members
    character(len=:), allocatable :: capi, capabilities

    members = ''
    call add_string(members, 'model', config%model)
    call add_string(members, 'systemPrompt', config%system_prompt)
    call add_string(members, 'workspacePath', config%workspace_path)
    call add_string(members, 'gitHubToken', config%github_token)
    call add_true(members, 'streaming', config%streaming)
    if (allocated(config%tools)) then
      members = members // '"tools":' // tool_definitions_json(config%tools) // ','
    end if
    call add_string_list(members, 'availableTools', config%available_tools)
    call add_string_list(members, 'excludedTools', config%excluded_tools)
    call add_string_list(members, 'skillDirectories', config%skill_directories)
    call add_string_list(members, 'disabledSkills', config%disabled_skills)
    call add_true(members, 'includeSubAgentStreamingEvents', config%include_sub_agent_streaming_events)
    call add_true(members, 'enableConfigDiscovery', config%enable_config_discovery)
    capabilities = model_capabilities_json(config)
    if (len(capabilities) > 0) members = members // '"modelCapabilities":' // capabilities // ','
    call add_raw(members, 'mcpServers', config%mcp_servers_json)
    call add_string_list(members, 'instructionDirectories', config%instruction_directories)

    ! --- Upstream-sync session config passthroughs (camelCase wire keys) ---
    call add_true(members, 'enableCitations', config%enable_citations)
    call add_string_list(members, 'excludedBuiltinAgents', config%excluded_builtin_agents)
    if (config%session_limits%max_ai_credits > 0) then
      members = members // '"sessionLimits":{"maxAiCredits":' // &
                int_text(config%session_limits%max_ai_credits) // '},'
    end if
    if (config%memory%enabled) members = members // '"memory":{"enabled":true},'
    call add_string(members, 'otlpProtocol', config%otlp_protocol)
    call add_raw(members, 'expAssignments', config%exp_assignments_json)

    ! --- Newly-synced session config passthroughs ---
    call add_true(members, 'rewindEnabled', config%rewind_enabled)
    call add_string_list(members, 'additionalDirectories', config%additional_directories)
    call add_string_list(members, 'disabledMcpServers', config%disabled_mcp_servers)
    call add_raw(members, 'githubMcpToolConfig', config%github_mcp_tool_config_json)
    call add_raw(members, 'canvasProvider', config%canvas_provider_json)
    call add_true(members, 'customAgentsLocalOnly', config%custom_agents_local_only)
    call add_raw(members, 'decisionContext', config%decision_context_json)
    ! User-prompt-transformed hook: signal the runtime that a handler is registered.
    call add_true(members, 'userPromptTransformed', associated(config%on_user_prompt_transformed))
    call add_string_list(members, 'builtinPluginDirectories', config%builtin_plugin_directories)
    call add_raw(members, 'argsSchema', config%args_schema_json)
    call add_string(members, 'reasoningEffort', config%reasoning_effort)
    call add_raw(members, 'toolSearch', config%tool_search_json)
    call add_true(members, 'inProcess', config%in_process)
    call add_true(members, 'enableExperimentalMode', config%experimental_mode)
    call add_true(members, 'contentExclusion', config%content_exclusion)
    ! MCP OAuth host token handler: signal the runtime that a handler is registered.
    call add_true(members, 'mcpAuthHandler', associated(config%on_mcp_auth_request))
    call add_true(members, 'enableSessionTelemetry', config%enable_session_telemetry)
    ! Hook handlers answered through hooks.invoke (wire: hooks.hasHandlers)
    call add_true(members, 'hooks', has_hook_handlers(config))

    ! --- v2.6.0 options ---
    call add_string(members, 'askUserVariant', config%ask_user_variant)
    if (allocated(config%sandbox)) then
      members = members // '"sandboxConfig":' // sandbox_json(config%sandbox) // ','
    end if
    call add_raw(members, 'featureFlags', config%feature_flags_json)
    call add_string_list(members, 'allowedModels', config%allowed_models)
    call add_string(members, 'authClientIdMetadataUrl', config%auth_client_id_metadata_url)
    call add_string_list(members, 'agentDirectories', config%agent_directories)
    call add_string_list(members, 'hookDirectories', config%hook_directories)
    if (.not. is_resume) then
      call add_string(members, 'trustWorkingDirectory', config%trust_working_directory)
    end if
    if (is_resume .and. allocated(config%allow_transcript_recovery)) then
      members = members // '"allowTranscriptRecovery":' // &
                json_logical(config%allow_transcript_recovery) // ','
    end if
    if (allocated(config%image_generation)) then
      members = members // '"imageGeneration":' // image_generation_json(config%image_generation) // ','
    end if
    if (allocated(config%autopilot)) then
      members = members // '"autopilot":' // autopilot_json(config%autopilot) // ','
    end if
    if (allocated(config%provider)) then
      members = members // '"provider":' // provider_json(config%provider) // ','
    end if

    ! --- v3.0.0 options ---
    if (allocated(config%providers)) then
      members = members // '"providers":' // named_providers_json(config%providers) // ','
    end if
    if (allocated(config%models)) then
      members = members // '"models":' // provider_models_json(config%models) // ','
    end if
    if (is_resume .and. config%clear_default_provider) then
      ! An explicit null clears the default provider stored for the session.
      members = members // '"inference":{"defaultProvider":null},'
    else
      call add_string(members, 'defaultProvider', config%default_provider)
    end if
    capi = ''
    if (allocated(config%capi_enabled)) capi = capi // ',"enabled":' // json_logical(config%capi_enabled)
    if (allocated(config%auto_tier)) capi = capi // ',"autoTier":' // json_quote(config%auto_tier)
    if (config%enable_web_socket_responses) capi = capi // ',"enableWebSocketResponses":true'
    if (len(capi) > 0) members = members // '"capi":{' // capi(2:) // '},'
    if (is_resume) call add_true(members, 'suppressResumeEvent', config%suppress_resume_event)
    call add_true(members, 'requestShellAuthentication', associated(config%on_shell_authentication_request))
    call add_true(members, 'hasMemoryStore', associated(config%on_store_memory))
  end function flat_session_options

  !> True when any hook handler answered through hooks.invoke is registered.
  function has_hook_handlers(config) result(has_handlers)
    type(session_config), intent(in) :: config
    logical :: has_handlers

    has_handlers = associated(config%on_post_tool_use) .or. associated(config%on_pre_mcp_tool_call) .or. &
                   associated(config%on_user_prompt_transformed) .or. &
                   associated(config%on_subagent_start) .or. associated(config%on_subagent_stop)
  end function has_hook_handlers

  !> Tool definitions (tools.definitions).
  function tool_definitions_json(tools) result(json)
    type(copilot_tool), intent(in) :: tools(:)
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members
    integer :: i

    json = '['
    do i = 1, size(tools)
      if (i > 1) json = json // ','
      members = ''
      if (allocated(tools(i)%name)) then
        call add_string(members, 'name', tools(i)%name)
      else
        members = '"name":"",'
      end if
      call add_string(members, 'description', tools(i)%description)
      call add_raw(members, 'parameters', tools(i)%input_schema_json)
      call add_string(members, 'defer', tools(i)%defer)
      call add_true(members, 'idempotent', tools(i)%idempotent)
      json = json // object_from_members(members)
    end do
    json = json // ']'
  end function tool_definitions_json

  !> modelCapabilities: the caller's capability JSON plus the model limit overrides.
  function model_capabilities_json(config) result(json)
    type(session_config), intent(in) :: config
    character(len=:), allocatable :: json
    type(json_object) :: capabilities
    character(len=:), allocatable :: members

    json = ''
    if (allocated(config%model_capabilities_json)) then
      if (len_trim(config%model_capabilities_json) > 0) json = trim(config%model_capabilities_json)
    end if
    members = ''
    call add_positive(members, 'max_prompt_tokens', config%model_limits%max_prompt_tokens)
    call add_positive(members, 'max_context_window_tokens', config%model_limits%max_context_window_tokens)
    call add_positive(members, 'max_output_tokens', config%model_limits%max_output_tokens)
    if (len(members) == 0) return
    if (len(json) == 0) then
      json = '{"limits":' // object_from_members(members) // '}'
      return
    end if
    call json_parse_object(json, capabilities)
    call json_object_put(capabilities, 'limits', object_from_members(members), .false.)
    json = json_object_text(capabilities)
  end function model_capabilities_json

  !> sandboxConfig object.
  function sandbox_json(sandbox) result(json)
    type(sandbox_config), intent(in) :: sandbox
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members

    members = '"enabled":' // json_logical(sandbox%enabled) // ','
    call add_true(members, 'allowBypass', sandbox%allow_bypass)
    call add_raw(members, 'userPolicy', sandbox%user_policy_json)
    json = object_from_members(members)
  end function sandbox_json

  !> imageGeneration object.
  function image_generation_json(options) result(json)
    type(image_generation_config), intent(in) :: options
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members

    members = ''
    if (allocated(options%enabled)) members = members // '"enabled":' // json_logical(options%enabled) // ','
    if (allocated(options%watermark)) members = members // '"watermark":' // json_logical(options%watermark) // ','
    if (allocated(options%save_to_disk)) then
      members = members // '"saveToDisk":' // json_logical(options%save_to_disk) // ','
    end if
    json = object_from_members(members)
  end function image_generation_json

  !> autopilot object.
  function autopilot_json(options) result(json)
    type(autopilot_options), intent(in) :: options
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members

    members = ''
    if (allocated(options%continuation_enabled)) then
      members = members // '"continuationEnabled":' // json_logical(options%continuation_enabled) // ','
    end if
    if (allocated(options%max_continues)) then
      members = members // '"maxContinues":' // int_text(options%max_continues) // ','
    end if
    json = object_from_members(members)
  end function autopilot_json

  !> Legacy single provider object (translated by group_session_params).
  function provider_json(provider) result(json)
    type(byok_provider_config), intent(in) :: provider
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members

    members = ''
    call add_string(members, 'type', provider%provider_type)
    call add_string(members, 'baseUrl', provider%base_url)
    call add_string(members, 'apiKey', provider%api_key)
    call add_string(members, 'apiKeyCommand', provider%api_key_command)
    call add_string(members, 'bearerToken', provider%bearer_token)
    call add_string(members, 'wireApi', provider%wire_api)
    if (allocated(provider%azure_api_version)) then
      members = members // '"azure":{"apiVersion":' // json_quote(provider%azure_api_version) // '},'
    end if
    call add_raw(members, 'headers', provider%headers_json)
    call add_string(members, 'modelId', provider%model_id)
    call add_string(members, 'wireModel', provider%wire_model)
    call add_positive(members, 'maxPromptTokens', provider%max_prompt_tokens)
    call add_positive(members, 'maxOutputTokens', provider%max_output_tokens)
    json = object_from_members(members)
  end function provider_json

  !> Named providers array (inference.providers).
  function named_providers_json(providers) result(json)
    type(named_provider_config), intent(in) :: providers(:)
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members
    integer :: i

    json = '['
    do i = 1, size(providers)
      if (i > 1) json = json // ','
      members = ''
      call add_string(members, 'name', providers(i)%name)
      call add_string(members, 'type', providers(i)%provider_type)
      call add_string(members, 'baseUrl', providers(i)%base_url)
      call add_string(members, 'apiKey', providers(i)%api_key)
      call add_string(members, 'apiKeyCommand', providers(i)%api_key_command)
      call add_string(members, 'bearerToken', providers(i)%bearer_token)
      call add_string(members, 'wireApi', providers(i)%wire_api)
      call add_string(members, 'transport', providers(i)%transport)
      if (allocated(providers(i)%azure_api_version)) then
        members = members // '"azure":{"apiVersion":' // json_quote(providers(i)%azure_api_version) // '},'
      end if
      call add_raw(members, 'headers', providers(i)%headers_json)
      call add_string(members, 'modelProvider', providers(i)%model_provider)
      json = json // object_from_members(members)
    end do
    json = json // ']'
  end function named_providers_json

  !> Provider models array (inference.models).
  function provider_models_json(models) result(json)
    type(provider_model_config), intent(in) :: models(:)
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members
    integer :: i

    json = '['
    do i = 1, size(models)
      if (i > 1) json = json // ','
      members = ''
      call add_string(members, 'provider', models(i)%provider)
      call add_string(members, 'id', models(i)%id)
      call add_string(members, 'modelId', models(i)%model_id)
      call add_string(members, 'wireModel', models(i)%wire_model)
      call add_string(members, 'name', models(i)%name)
      call add_positive(members, 'maxPromptTokens', models(i)%max_prompt_tokens)
      call add_positive(members, 'maxContextWindowTokens', models(i)%max_context_window_tokens)
      call add_positive(members, 'maxOutputTokens', models(i)%max_output_tokens)
      call add_raw(members, 'capabilities', models(i)%capabilities_json)
      json = json // object_from_members(members)
    end do
    json = json // ']'
  end function provider_models_json

  !> Remember a session so server-to-client requests can be routed to it.
  subroutine track_session(self, session)
    class(copilot_client), intent(inout) :: self
    type(copilot_session_type), intent(in) :: session
    integer :: idx

    idx = self%session_count + 1
    if (idx <= MAX_SESSIONS) then
      self%sessions(idx) = session
      self%session_count = idx
    end if
  end subroutine track_session

  !> Module-level wrappers
  subroutine copilot_start(client)
    type(copilot_client), intent(inout) :: client
    call client%start()
  end subroutine copilot_start

  subroutine copilot_stop(client)
    type(copilot_client), intent(inout) :: client
    call client%stop()
  end subroutine copilot_stop

  subroutine copilot_create_session(client, config, session, iostat)
    type(copilot_client), intent(inout) :: client
    type(session_config), intent(in), optional :: config
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    if (present(config)) then
      call client%create_session(config, session, iostat)
    else
      call client%create_session(session=session, iostat=iostat)
    end if
  end subroutine copilot_create_session

  subroutine copilot_resume_session(client, session_id, session, iostat, config)
    type(copilot_client), intent(inout) :: client
    character(len=*), intent(in) :: session_id
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    type(session_config), intent(in), optional :: config
    call client%resume_session(session_id, session, iostat, config)
  end subroutine copilot_resume_session

  !> Clear the runtime's managed settings cache (managedSettings.clearCache).
  subroutine client_clear_managed_settings_cache(self, iostat)
    class(copilot_client), intent(inout) :: self
    integer, intent(out) :: iostat
    character(len=:), allocatable :: response

    call client_request(self, 'managedSettings.clearCache', '{}', response, iostat)
  end subroutine client_clear_managed_settings_cache

  subroutine copilot_clear_managed_settings_cache(client, iostat)
    type(copilot_client), intent(inout) :: client
    integer, intent(out) :: iostat
    call client%clear_managed_settings_cache(iostat)
  end subroutine copilot_clear_managed_settings_cache

  !> Answer one server-to-client JSON-RPC request read from the connection.
  !! gitHubToken.getToken is routed by registration id, hooks.invoke,
  !! shellAuthentication.*, and memoryStore.store by sessionId, and network.* to the client's
  !! network callbacks. Unknown methods get a method-not-found error (handled = .false.).
  subroutine client_handle_server_request(self, message, handled, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: message
    logical, intent(out) :: handled
    integer, intent(out) :: iostat
    character(len=:), allocatable :: method, params, id_json, result_json, error_text
    character(len=:), allocatable :: registration_id, session_id
    integer :: i, owner
    logical :: found

    handled = .false.
    iostat = 0
    method = json_get_string(message, 'method', found)
    if (.not. found) return
    id_json = jsonrpc_extract_raw_id(message)
    if (len(id_json) == 0) return
    call json_get_raw(message, 'params', params, found)
    if (.not. found) params = '{}'

    owner = 0
    registration_id = ''
    if (method == 'gitHubToken.getToken') then
      registration_id = json_get_string(params, 'registrationId')
      do i = 1, self%session_count
        if (self%sessions(i)%owns_github_token_registration(registration_id)) then
          owner = i
          exit
        end if
      end do
    else
      session_id = json_get_string(params, 'sessionId', found)
      if (found) then
        do i = 1, self%session_count
          if (.not. self%sessions(i)%is_active) cycle
          if (.not. allocated(self%sessions(i)%session_id)) cycle
          if (self%sessions(i)%session_id == session_id) then
            owner = i
            exit
          end if
        end do
      end if
    end if

    if (owner > 0) then
      call self%sessions(owner)%answer_request(method, params, handled, result_json, error_text)
    else if (method == 'gitHubToken.getToken') then
      handled = .true.
      result_json = 'null'
      error_text = 'No GitHub token provider registered for registration ID "' // registration_id // '"'
    else
      call network_request_result(self%options%network, method, params, handled, result_json, error_text)
    end if

    if (.not. handled) then
      call self%rpc%send_error_response(id_json, JSONRPC_METHOD_NOT_FOUND, 'Method not found: ' // method, iostat)
    else if (len(error_text) > 0) then
      call self%rpc%send_error_response(id_json, JSONRPC_INTERNAL_ERROR, error_text, iostat)
    else
      call self%rpc%send_response(id_json, result_json, iostat)
    end if
  end subroutine client_handle_server_request

  subroutine copilot_handle_server_request(client, message, handled, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=*), intent(in) :: message
    logical, intent(out) :: handled
    integer, intent(out) :: iostat
    call client%handle_server_request(message, handled, iostat)
  end subroutine copilot_handle_server_request

  !> network.setProvider params for the registered callbacks ('' when none is registered).
  !! Each registered callback is marked with an empty object.
  function copilot_network_provider_params(network) result(params)
    type(network_options), intent(in) :: network
    character(len=:), allocatable :: params
    character(len=:), allocatable :: members

    params = ''
    members = ''
    if (associated(network%resolve_proxy)) members = members // '"resolveProxy":{},'
    if (associated(network%authenticate_negotiate)) members = members // '"authenticateNegotiate":{},'
    if (associated(network%authenticate_basic)) members = members // '"authenticateBasic":{},'
    if (len(members) == 0) return
    if (allocated(network%scope)) then
      if (len_trim(network%scope) > 0) members = '"scope":' // json_quote(trim(network%scope)) // ',' // members
    end if
    params = object_from_members(members)
  end function copilot_network_provider_params

  !> Register the host network callbacks with network.setProvider after connecting.
  subroutine register_network_provider(self, iostat)
    class(copilot_client), intent(inout) :: self
    integer, intent(out) :: iostat
    character(len=:), allocatable :: params, response

    iostat = 0
    params = copilot_network_provider_params(self%options%network)
    if (len(params) == 0) return
    call client_request(self, 'network.setProvider', params, response, iostat)
  end subroutine register_network_provider

  ! ==========================================================================
  ! Status / metadata queries
  ! ==========================================================================

  !> Get CLI status information.
  subroutine client_get_status(self, status_json, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: status_json
    integer, intent(out) :: iostat
    integer :: req_id

    call self%rpc%send_request('status.get', '', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(status_json, iostat)
  end subroutine client_get_status

  subroutine copilot_get_status(client, status_json, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=:), allocatable, intent(out) :: status_json
    integer, intent(out) :: iostat
    call client%get_status(status_json, iostat)
  end subroutine copilot_get_status

  !> Get authentication status.
  subroutine client_get_auth_status(self, auth_json, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: auth_json
    integer, intent(out) :: iostat
    integer :: req_id

    call self%rpc%send_request('auth.getStatus', '', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(auth_json, iostat)
  end subroutine client_get_auth_status

  subroutine copilot_get_auth_status(client, auth_json, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=:), allocatable, intent(out) :: auth_json
    integer, intent(out) :: iostat
    call client%get_auth_status(auth_json, iostat)
  end subroutine copilot_get_auth_status

  !> List available models.
  subroutine client_list_models(self, models_json, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: models_json
    integer, intent(out) :: iostat
    integer :: req_id

    call self%rpc%send_request('models.list', '', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(models_json, iostat)
  end subroutine client_list_models

  subroutine copilot_list_models(client, models_json, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=:), allocatable, intent(out) :: models_json
    integer, intent(out) :: iostat
    call client%list_models(models_json, iostat)
  end subroutine copilot_list_models

  !> List all sessions.
  subroutine client_list_sessions(self, sessions_json, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: sessions_json
    integer, intent(out) :: iostat
    integer :: req_id

    call self%rpc%send_request('session.list', '', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(sessions_json, iostat)
  end subroutine client_list_sessions

  subroutine copilot_list_sessions(client, sessions_json, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=:), allocatable, intent(out) :: sessions_json
    integer, intent(out) :: iostat
    call client%list_sessions(sessions_json, iostat)
  end subroutine copilot_list_sessions

  !> Get the last-used session ID.
  subroutine client_get_last_session_id(self, session_id, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: session_id
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: response

    call self%rpc%send_request('session.getLastId', '{}', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(response, iostat)
    if (iostat /= 0) return

    session_id = extract_session_id(response)
  end subroutine client_get_last_session_id

  subroutine copilot_get_last_session_id(client, session_id, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=:), allocatable, intent(out) :: session_id
    integer, intent(out) :: iostat
    call client%get_last_session_id(session_id, iostat)
  end subroutine copilot_get_last_session_id

  !> Get metadata for a session by ID.
  subroutine copilot_get_session_metadata(client, session_id, meta, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=*), intent(in) :: session_id
    type(session_metadata), intent(out) :: meta
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // session_id // '"}'
    call client%rpc%send_request('session.getMetadata', params, req_id, iostat)
    meta%session_id = session_id
    meta%status = 'active'
  end subroutine copilot_get_session_metadata

  !> Delete a session by ID. A failure arrives as a JSON-RPC error and is reported through
  !! iostat (message in client%last_error).
  subroutine client_delete_session(self, session_id, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: session_id
    integer, intent(out) :: iostat
    character(len=:), allocatable :: response

    call client_request(self, 'session.delete', '{"sessionId":' // json_quote(session_id) // '}', &
                        response, iostat)
  end subroutine client_delete_session

  subroutine copilot_delete_session(client, session_id, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=*), intent(in) :: session_id
    integer, intent(out) :: iostat
    call client%delete_session(session_id, iostat)
  end subroutine copilot_delete_session

  !> Get the foreground session ID.
  subroutine client_get_foreground(self, session_id, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: session_id
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: response

    call self%rpc%send_request('session.getForeground', '{}', req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(response, iostat)
    if (iostat /= 0) return

    session_id = extract_session_id(response)
  end subroutine client_get_foreground

  subroutine copilot_get_foreground_session(client, session_id, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=:), allocatable, intent(out) :: session_id
    integer, intent(out) :: iostat
    call client%get_foreground(session_id, iostat)
  end subroutine copilot_get_foreground_session

  !> Set the foreground session (TUI mode). A failure arrives as a JSON-RPC error and is
  !! reported through iostat (message in client%last_error).
  subroutine client_set_foreground(self, session_id, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: session_id
    integer, intent(out) :: iostat
    character(len=:), allocatable :: response

    call client_request(self, 'session.setForeground', '{"sessionId":' // json_quote(session_id) // '}', &
                        response, iostat)
  end subroutine client_set_foreground

  subroutine copilot_set_foreground_session(client, session_id, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=*), intent(in) :: session_id
    integer, intent(out) :: iostat
    call client%set_foreground(session_id, iostat)
  end subroutine copilot_set_foreground_session

  ! ==========================================================================
  ! Internal helpers
  ! ==========================================================================

  !> Build the CLI command string from client options.
  !! A .js entry point (such as npm-loader.js) runs with node; other paths run directly.
  function build_cli_command(opts) result(cmd)
    type(copilot_client_options), intent(in) :: opts
    character(len=:), allocatable :: cmd
    integer :: n

    if (allocated(opts%cli_path)) then
      cmd = trim(opts%cli_path)
      n = len(cmd)
      if (n > 3) then
        if (cmd(n - 2:n) == '.js') cmd = 'node ' // cmd
      end if
    else
      cmd = 'copilot'
    end if

    cmd = cmd // ' --headless --no-auto-update --stdio'

    if (allocated(opts%log_level)) then
      cmd = cmd // ' --log-level ' // opts%log_level
    end if

    if (opts%remote) then
      cmd = cmd // ' --remote'
    end if
  end function build_cli_command

  !> Extract session ID from a JSON-RPC response.
  function extract_session_id(json) result(sid)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: sid
    integer :: pos, end_pos

    sid = ''
    pos = index(json, '"sessionId":"')
    if (pos == 0) return
    pos = pos + 13
    end_pos = index(json(pos:), '"')
    if (end_pos == 0) return
    sid = json(pos:pos+end_pos-2)
  end function extract_session_id

  !> Send a request, read its response, and report a JSON-RPC error as
  !! iostat = COPILOT_IOSTAT_RPC_ERROR (message in self%last_error).
  subroutine client_request(self, method, params, response, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: method, params
    character(len=:), allocatable, intent(out) :: response
    integer, intent(out) :: iostat
    character(len=:), allocatable :: error_json
    integer :: req_id
    logical :: is_error

    response = ''
    call self%rpc%send_request(method, params, req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(response, iostat)
    if (.not. allocated(response)) response = ''
    if (iostat /= 0) return
    call jsonrpc_response_error(response, error_json, is_error)
    if (is_error) call record_rpc_error(self, error_json, iostat)
  end subroutine client_request

  !> Record a JSON-RPC error, including a Policy Helper failure carried in its data.
  subroutine record_rpc_error(self, error_json, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: error_json
    integer, intent(out) :: iostat

    self%last_error = jsonrpc_error_message(error_json)
    call parse_policy_helper_failure(error_json, self%last_policy_helper_failure, &
                                     self%has_policy_helper_failure)
    iostat = COPILOT_IOSTAT_RPC_ERROR
  end subroutine record_rpc_error

  !> Report an invalid option.
  subroutine config_error(self, message, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: message
    integer, intent(out) :: iostat

    self%last_error = message
    write(error_unit, '(A)') 'copilot_client: ' // message
    iostat = COPILOT_IOSTAT_INVALID_CONFIG
  end subroutine config_error

  !> Normalize options%mode into self%mode. Returns .false. for an unknown mode.
  function resolve_client_mode(self) result(ok)
    class(copilot_client), intent(inout) :: self
    logical :: ok

    if (allocated(self%options%mode)) then
      self%mode = normalize_client_mode(self%options%mode)
    else
      self%mode = CLIENT_MODE_CODING_AGENT
    end if
    ok = len(self%mode) > 0
    if (.not. ok) then
      self%last_error = 'Invalid client mode "' // self%options%mode // &
                        '": expected "coding-agent" or "empty"'
    end if
  end function resolve_client_mode

  !> clientInfo object for connect ('' when every field is empty).
  function client_info_json(info) result(json)
    type(copilot_client_info), intent(in) :: info
    character(len=:), allocatable :: json
    character(len=:), allocatable :: members

    json = ''
    members = ''
    call add_nonempty_string(members, 'editorName', info%editor_name)
    call add_nonempty_string(members, 'editorVersion', info%editor_version)
    call add_nonempty_string(members, 'extensionName', info%extension_name)
    call add_nonempty_string(members, 'extensionVersion', info%extension_version)
    if (len(members) > 0) json = object_from_members(members)
  end function client_info_json

  !> A new opaque registration id (UUID format) for a GitHub token provider.
  function new_registration_id() result(id)
    character(len=:), allocatable :: id
    integer, save :: counter = 0
    integer(int64) :: clock
    real :: r(4)
    character(len=36) :: buf

    counter = counter + 1
    call system_clock(clock)
    call random_number(r)
    write(buf, '(Z8.8,"-",Z4.4,"-4",Z3.3,"-",Z4.4,"-",Z12.12)') &
      modulo(clock, 4294967296_int64), modulo(counter, 65536), int(r(1) * 4096.0), &
      32768 + int(r(2) * 16384.0), &
      int(r(3) * 16777216.0, int64) * 16777216_int64 + int(r(4) * 16777216.0, int64)
    id = buf
  end function new_registration_id

  !> Append "key":"value", when value is allocated.
  subroutine add_string(members, key, value)
    character(len=:), allocatable, intent(inout) :: members
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(in) :: value

    if (.not. allocated(value)) return
    members = members // json_quote(key) // ':' // json_quote(value) // ','
  end subroutine add_string

  !> Append "key":"value", when value is allocated and not blank.
  subroutine add_nonempty_string(members, key, value)
    character(len=:), allocatable, intent(inout) :: members
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(in) :: value

    if (.not. allocated(value)) return
    if (len_trim(value) == 0) return
    members = members // json_quote(key) // ':' // json_quote(trim(value)) // ','
  end subroutine add_nonempty_string

  !> Append "key":<raw JSON>, when the raw JSON is allocated and not blank.
  subroutine add_raw(members, key, value)
    character(len=:), allocatable, intent(inout) :: members
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(in) :: value

    if (.not. allocated(value)) return
    if (len_trim(value) == 0) return
    members = members // json_quote(key) // ':' // trim(value) // ','
  end subroutine add_raw

  !> Append "key":true, when flag is set.
  subroutine add_true(members, key, flag)
    character(len=:), allocatable, intent(inout) :: members
    character(len=*), intent(in) :: key
    logical, intent(in) :: flag

    if (flag) members = members // json_quote(key) // ':true,'
  end subroutine add_true

  !> Append "key":N, when N is positive.
  subroutine add_positive(members, key, value)
    character(len=:), allocatable, intent(inout) :: members
    character(len=*), intent(in) :: key
    integer, intent(in) :: value

    if (value > 0) members = members // json_quote(key) // ':' // int_text(value) // ','
  end subroutine add_positive

  !> Append "key":["a","b"], when the list is allocated.
  subroutine add_string_list(members, key, values)
    character(len=:), allocatable, intent(inout) :: members
    character(len=*), intent(in) :: key
    character(len=:), allocatable, intent(in) :: values(:)

    if (.not. allocated(values)) return
    members = members // json_quote(key) // ':' // json_string_array(values) // ','
  end subroutine add_string_list

  !> Wrap comma-terminated members in braces.
  function object_from_members(members) result(json)
    character(len=*), intent(in) :: members
    character(len=:), allocatable :: json
    integer :: n

    n = len(members)
    if (n > 0) then
      if (members(n:n) == ',') n = n - 1
    end if
    json = '{' // members(1:n) // '}'
  end function object_from_members

  !> JSON boolean text.
  function json_logical(flag) result(text)
    logical, intent(in) :: flag
    character(len=:), allocatable :: text

    if (flag) then
      text = 'true'
    else
      text = 'false'
    end if
  end function json_logical

  !> Integer as text.
  function int_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=20) :: buf

    write(buf, '(I0)') value
    text = trim(buf)
  end function int_text

end module copilot_client_module
