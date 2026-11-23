! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Callbacks used by the unit tests.
module test_callbacks
  use copilot_types
  use copilot_tools
  implicit none
  private

  public :: accept_shell_auth, remember_fact, add_subagent_context, provide_token
  public :: lookup_tool, last_stored_fact

  !> Fact recorded by remember_fact.
  character(len=:), allocatable :: last_stored_fact

contains

  function accept_shell_auth(request) result(response)
    type(shell_authentication_request), intent(in) :: request
    type(shell_authentication_result) :: response

    response%accepted = request%input_kind == SHELL_AUTH_INPUT_SECRET
    response%value = 's3cret'
  end function accept_shell_auth

  subroutine remember_fact(memory)
    type(stored_memory), intent(in) :: memory

    last_stored_fact = memory%fact
  end subroutine remember_fact

  function add_subagent_context(input) result(output)
    type(subagent_start_input), intent(in) :: input
    type(subagent_start_output) :: output

    output%additional_context = 'agent ' // input%agent_name // ' in ' // input%working_directory
  end function add_subagent_context

  function provide_token(args) result(token)
    type(github_token_provider_args), intent(in) :: args
    type(github_token_result) :: token

    if (args%host == 'github.com') token%access_token = 'gho_test'
  end function provide_token

  function lookup_tool(args, invocation) result(res)
    character(len=*), intent(in) :: args
    type(tool_invocation), intent(in) :: invocation
    type(tool_result) :: res

    res = tool_success(args // invocation%session_id)
  end function lookup_tool

end module test_callbacks

!> Unit tests for the Copilot SDK Fortran modules.
program test_client
  use copilot_types
  use copilot_version
  use copilot_jsonrpc
  use copilot_tools
  use copilot_session_module
  use copilot_client_module
  use copilot_session_wire
  use test_callbacks
  implicit none

  integer :: failures

  failures = 0

  call test_protocol_version(failures)
  call test_client_options_defaults(failures)
  call test_connection_state(failures)
  call test_jsonrpc_build_request(failures)
  call test_jsonrpc_build_notification(failures)
  call test_jsonrpc_classification(failures)
  call test_jsonrpc_extract_method(failures)
  call test_tool_success(failures)
  call test_tool_failure(failures)
  call test_send_options(failures)
  call test_protocol_version_check(failures)
  call test_group_session_params(failures)
  call test_build_session_params(failures)
  call test_session_server_requests(failures)
  call test_v3_parsers(failures)

  if (failures > 0) then
    print '(A,I0,A)', 'FAILED: ', failures, ' test(s) failed'
    stop 1
  else
    print *, 'All tests passed'
  end if

contains

  ! Helper: assert equality of two strings
  subroutine assert_eq_str(label, expected, actual, failures)
    character(len=*), intent(in) :: label, expected, actual
    integer, intent(inout) :: failures

    if (trim(expected) /= trim(actual)) then
      print '(A,A,A,A,A,A)', 'FAIL: ', label, &
            ' expected="', trim(expected), '" got="', trim(actual) // '"'
      failures = failures + 1
    end if
  end subroutine assert_eq_str

  ! Helper: assert a logical condition
  subroutine assert_true(label, cond, failures)
    character(len=*), intent(in) :: label
    logical, intent(in) :: cond
    integer, intent(inout) :: failures

    if (.not. cond) then
      print '(A,A)', 'FAIL: ', label
      failures = failures + 1
    end if
  end subroutine assert_true

  ! ------------------------------------------------------------------
  subroutine test_protocol_version(failures)
    integer, intent(inout) :: failures
    call assert_true('protocol version is 4', SDK_PROTOCOL_VERSION == 4, failures)
    call assert_eq_str('protocol version text', '4', SDK_PROTOCOL_VERSION_TEXT, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_client_options_defaults(failures)
    integer, intent(inout) :: failures
    type(copilot_client_options) :: opts

    call opts%set_defaults()
    call assert_eq_str('default log_level', 'info', opts%log_level, failures)
    call assert_true('auto_start defaults to true', opts%auto_start, failures)
    call assert_true('use_stdio defaults to true', opts%use_stdio, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_connection_state(failures)
    integer, intent(inout) :: failures
    type(copilot_connection_state) :: st

    call assert_true('default state is disconnected', &
                     st%value == COPILOT_STATE_DISCONNECTED, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_jsonrpc_build_request(failures)
    integer, intent(inout) :: failures
    character(len=:), allocatable :: msg

    msg = jsonrpc_build_request(1, 'ping', '{"message":"hi"}')
    call assert_true('request contains jsonrpc', index(msg, '"jsonrpc":"2.0"') > 0, failures)
    call assert_true('request contains method', index(msg, '"method":"ping"') > 0, failures)
    call assert_true('request contains id', index(msg, '"id":1') > 0, failures)
    call assert_true('request contains params', &
                     index(msg, '"params":{"message":"hi"}') > 0, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_jsonrpc_build_notification(failures)
    integer, intent(inout) :: failures
    character(len=:), allocatable :: msg

    msg = jsonrpc_build_notification('session.event', '{"type":"idle"}')
    call assert_true('notification has no id', index(msg, '"id":') == 0, failures)
    call assert_true('notification has method', &
                     index(msg, '"method":"session.event"') > 0, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_jsonrpc_classification(failures)
    integer, intent(inout) :: failures
    character(len=:), allocatable :: resp, notif, req

    resp  = '{"jsonrpc":"2.0","id":1,"result":{"ok":true}}'
    notif = '{"jsonrpc":"2.0","method":"session.event","params":{}}'
    req   = '{"jsonrpc":"2.0","id":2,"method":"tool.call","params":{}}'

    call assert_true('is_response for response', jsonrpc_is_response(resp), failures)
    call assert_true('not notification for response', &
                     .not. jsonrpc_is_notification(resp), failures)
    call assert_true('is_notification for notification', &
                     jsonrpc_is_notification(notif), failures)
    call assert_true('is_request for request', jsonrpc_is_request(req), failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_jsonrpc_extract_method(failures)
    integer, intent(inout) :: failures
    character(len=:), allocatable :: m

    m = jsonrpc_extract_method('{"jsonrpc":"2.0","method":"ping","id":1}')
    call assert_eq_str('extract method', 'ping', m, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_tool_success(failures)
    integer, intent(inout) :: failures
    type(tool_result) :: r

    r = tool_success('42')
    call assert_true('tool_success is not error', .not. r%is_error, failures)
    call assert_eq_str('tool_success content', '42', r%content, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_tool_failure(failures)
    integer, intent(inout) :: failures
    type(tool_result) :: r

    r = tool_failure('oops')
    call assert_true('tool_failure is error', r%is_error, failures)
    call assert_eq_str('tool_failure text', 'oops', r%error_text, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_send_options(failures)
    integer, intent(inout) :: failures
    type(send_options) :: opts

    opts%message = 'hello'
    call assert_eq_str('send_options message', 'hello', opts%message, failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_protocol_version_check(failures)
    integer, intent(inout) :: failures
    character(len=:), allocatable :: message

    call assert_true('server version 4 accepted', copilot_verify_protocol_version( &
                     '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":4}}', message), failures)
    call assert_true('server version 3 rejected', .not. copilot_verify_protocol_version( &
                     '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":3}}', message), failures)
    call assert_eq_str('version mismatch message', 'SDK protocol version mismatch: SDK supports ' // &
                       'version 4, but server reports version 3. Please update your SDK or server ' // &
                       'to ensure compatibility.', message, failures)
    call assert_true('missing version rejected', .not. copilot_verify_protocol_version( &
                     '{"jsonrpc":"2.0","id":1,"result":{}}', message), failures)
    call assert_true('missing version message', &
                     index(message, 'server does not report a protocol version') > 0, failures)
  end subroutine

  ! ------------------------------------------------------------------
  ! group_session_params: flat payload -> grouped SDK protocol 4 payload
  subroutine test_group_session_params(failures)
    integer, intent(inout) :: failures
    character(len=:), allocatable :: grouped

    ! Resume: root keys, disableResume rename, unknown root key, null skipped, unknown group
    ! member dropped, create-only member dropped, mcp servers set envValueMode.
    grouped = group_session_params('{"model":"m","streaming":true,"requestPermission":true,' // &
      '"tools":[{"name":"t"}],"hooks":true,"disableResume":true,"customKey":1,"nullKey":null,' // &
      '"mcpServers":{"s":{"command":"x"}},"trustWorkingDirectory":"session",' // &
      '"events":{"bogusMember":true}}', .true.)
    call assert_eq_str('grouped resume payload', '{"model":"m","suppressResumeEvent":true,' // &
      '"customKey":1,"tools":{"definitions":[{"name":"t"}]},"permissions":{"request":true},' // &
      '"events":{"streaming":true},"mcp":{"servers":{"s":{"command":"x"}},"envValueMode":"direct"},' // &
      '"hooks":{"hasHandlers":true}}', grouped, failures)

    ! Create: legacy provider translation, resume-only member dropped, explicit member wins.
    grouped = group_session_params('{"model":"gpt-x","provider":{"type":"openai",' // &
      '"baseUrl":"https://h","apiKey":"k","maxOutputTokens":100},"openCanvases":["c"],' // &
      '"trustWorkingDirectory":"user","reasoningEffort":"low",' // &
      '"inference":{"reasoningEffort":"high"}}', .false.)
    call assert_eq_str('grouped create payload', '{"model":"gpt-x","inference":' // &
      '{"reasoningEffort":"high","providers":[{"name":"default","type":"openai",' // &
      '"baseUrl":"https://h","apiKey":"k"}],"defaultProvider":"default","models":' // &
      '[{"provider":"default","id":"gpt-x","maxOutputTokens":100}]},' // &
      '"github":{"capi":{"enabled":false}},"filesystem":{"trustWorkingDirectory":"user"}}', &
      grouped, failures)

    call assert_true('member table accepts events.streaming', &
                     session_member_allowed('events', 'streaming', .false.), failures)
    call assert_true('member table rejects unknown members', &
                     .not. session_member_allowed('events', 'bogusMember', .false.), failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_build_session_params(failures)
    integer, intent(inout) :: failures
    type(session_config) :: config, empty_config
    character(len=:), allocatable :: params

    config%model = 'gpt-5'
    config%streaming = .false.
    config%auto_tier = AUTO_TIER_FAST
    config%capi_enabled = .true.
    config%allowed_models = [character(len=6) :: 'gpt-5', 'claude']
    config%model_limits%max_output_tokens = 2048
    config%trust_working_directory = WORKING_DIRECTORY_TRUST_SESSION
    allocate(config%sandbox)
    config%sandbox%allow_bypass = .true.
    allocate(config%providers(1))
    config%providers(1)%name = 'azure'
    config%providers(1)%base_url = 'https://example'
    config%providers(1)%api_key_command = 'get-key'
    config%default_provider = 'azure'
    config%tools = [define_tool('lookup', 'Look up', '{}', lookup_tool, idempotent=.true.)]
    config%on_shell_authentication_request => accept_shell_auth
    config%on_store_memory => remember_fact

    params = copilot_build_session_params(config, .false., '', CLIENT_MODE_CODING_AGENT, 'reg-9')
    call assert_true('model stays at the root', index(params, '{"model":"gpt-5"') == 1, failures)
    call assert_true('streaming false is omitted', index(params, 'streaming') == 0, failures)
    call assert_true('capi carries enabled and autoTier', &
                     index(params, '"capi":{"enabled":true,"autoTier":"fast"}') > 0, failures)
    call assert_true('token provider registration id', &
                     index(params, '"tokenProviderRegistrationId":"reg-9"') > 0, failures)
    call assert_true('allowed models in policy', &
                     index(params, '"policy":{"allowedModels":["gpt-5","claude"]}') > 0, failures)
    call assert_true('max output tokens in capabilities', &
                     index(params, '"capabilities":{"limits":{"max_output_tokens":2048}}') > 0, failures)
    call assert_true('named provider', index(params, '"providers":[{"name":"azure",' // &
                     '"baseUrl":"https://example","apiKeyCommand":"get-key"}]') > 0, failures)
    call assert_true('default provider', index(params, '"defaultProvider":"azure"') > 0, failures)
    call assert_true('trust working directory on create', &
                     index(params, '"filesystem":{"trustWorkingDirectory":"session"}') > 0, failures)
    call assert_true('sandbox bypass', &
                     index(params, '"permissions":{"sandbox":{"enabled":true,"allowBypass":true}}') > 0, failures)
    call assert_true('idempotent tool definition', index(params, '"tools":{"definitions":' // &
                     '[{"name":"lookup","description":"Look up","parameters":{},"idempotent":true}]}') > 0, &
                     failures)
    call assert_true('shell authentication requested', &
                     index(params, '"interaction":{"requestShellAuthentication":true}') > 0, failures)
    call assert_true('memory store registered', &
                     index(params, '"hasMemoryStore":true') > 0, failures)

    config%clear_default_provider = .true.
    config%suppress_resume_event = .true.
    config%allow_transcript_recovery = .false.
    params = copilot_build_session_params(config, .true., 'sess-1')
    call assert_true('resume keeps the session id', index(params, '{"sessionId":"sess-1"') == 1, failures)
    call assert_true('resume clears the default provider', &
                     index(params, '"defaultProvider":null') > 0, failures)
    call assert_true('resume suppresses the resume event', &
                     index(params, '"suppressResumeEvent":true') > 0, failures)
    call assert_true('resume sends transcript recovery', &
                     index(params, '"allowTranscriptRecovery":false') > 0, failures)
    call assert_true('trust is create only', index(params, 'trustWorkingDirectory') == 0, failures)

    empty_config%available_tools = [character(len=4) :: 'view']
    params = copilot_build_session_params(empty_config, .false., '', CLIENT_MODE_EMPTY)
    call assert_true('empty mode available tools', &
                     index(params, '"availableTools":["view"]') > 0, failures)
    call assert_true('empty mode telemetry default', index(params, '"telemetryEnabled":false') > 0, failures)
    call assert_true('empty mode memory default', &
                     index(params, '"memory":{"enabled":false}') > 0, failures)
    call assert_true('empty mode repository hooks default', &
                     index(params, '"hooks":{"enableRepositoryHooks":false}') > 0, failures)
    call assert_eq_str('client mode alias', CLIENT_MODE_CODING_AGENT, &
                       normalize_client_mode(CLIENT_MODE_COPILOT_CLI), failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_session_server_requests(failures)
    integer, intent(inout) :: failures
    type(copilot_session_type) :: session
    type(session_config) :: config
    type(network_options) :: network
    character(len=:), allocatable :: result_json, error_text
    logical :: handled

    session%session_id = 'sess-1'
    session%is_active = .true.
    config%on_shell_authentication_request => accept_shell_auth
    config%on_store_memory => remember_fact
    config%on_subagent_start => add_subagent_context
    config%github_token_provider => provide_token
    call session%configure(config, network, 'reg-1')

    call session%answer_request('shellAuthentication.request', '{"sessionId":"sess-1",' // &
      '"requestId":"r1","program":"ssh","prompt":"Password:","inputKind":"secret"}', &
      handled, result_json, error_text)
    call assert_eq_str('shell auth accepted', '{"action":"accept","value":"s3cret"}', result_json, failures)

    call session%answer_request('shellAuthentication.cancel', '{"sessionId":"sess-1","requestId":"r2"}', &
      handled, result_json, error_text)
    call assert_eq_str('shell auth cancel reply', '{}', result_json, failures)
    call session%answer_request('shellAuthentication.request', '{"sessionId":"sess-1","requestId":"r2",' // &
      '"inputKind":"secret"}', handled, result_json, error_text)
    call assert_eq_str('cancelled shell auth declined', '{"action":"cancel"}', result_json, failures)

    call session%answer_request('memoryStore.store', '{"sessionId":"sess-1","subject":"build",' // &
      '"fact":"use fpm","citations":"README","reason":"setup"}', handled, result_json, error_text)
    call assert_eq_str('memory store reply', 'null', result_json, failures)
    call assert_eq_str('memory stored', 'use fpm', last_stored_fact, failures)

    call session%answer_request('hooks.invoke', '{"sessionId":"sess-1","hookType":"subagentStart",' // &
      '"input":{"agentName":"explore","cwd":"/w"}}', handled, result_json, error_text)
    call assert_eq_str('subagent start output', '{"output":{"additionalContext":"agent explore in /w"}}', &
                       result_json, failures)

    call session%answer_request('gitHubToken.getToken', '{"registrationId":"reg-1",' // &
      '"host":"github.com","reason":"sessionStart"}', handled, result_json, error_text)
    call assert_eq_str('github token', '{"kind":"token","accessToken":"gho_test","expiresIn":28800}', &
                       result_json, failures)
    call session%answer_request('gitHubToken.getToken', '{"registrationId":"other","host":"github.com"}', &
      handled, result_json, error_text)
    call assert_true('unknown token registration fails', len(error_text) > 0, failures)

    call session%answer_request('hooks.invoke', '{"sessionId":"other","hookType":"subagentStart"}', &
      handled, result_json, error_text)
    call assert_true('request for another session not handled', .not. handled, failures)
    call session%answer_request('network.resolveProxy', '{"url":"https://x"}', &
      handled, result_json, error_text)
    call assert_true('network callback missing is an error', handled .and. len(error_text) > 0, failures)
    call assert_eq_str('no network provider params', '', copilot_network_provider_params(network), failures)
  end subroutine

  ! ------------------------------------------------------------------
  subroutine test_v3_parsers(failures)
    integer, intent(inout) :: failures
    type(tool_shell_output_data) :: output
    type(policy_helper_failure) :: failure
    logical :: found

    call parse_tool_shell_output('{"jsonrpc":"2.0","method":"session.event","params":{"sessionId":"s",' // &
      '"event":{"type":"tool.shell_output","data":{"toolCallId":"c1","text":"hi\n","sequence":2,' // &
      '"stream":"stdout"}}}}', output, found)
    call assert_true('shell output parsed', found, failures)
    call assert_eq_str('shell output text', 'hi' // achar(10), output%text, failures)
    call assert_true('shell output sequence', output%sequence == 2, failures)
    call assert_eq_str('shell output stream', 'stdout', output%stream, failures)

    call parse_policy_helper_failure('{"code":-32000,"message":"blocked","data":{"policyHelperFailure":' // &
      '{"category":"timeout","failureMode":"failClosed","message":"helper timed out",' // &
      '"previousPolicyRetained":true,"reason":"sessionStart","severity":"error"}}}', failure, found)
    call assert_true('policy helper failure parsed', found, failures)
    call assert_eq_str('policy helper category', 'timeout', failure%category, failures)
    call assert_true('policy retained', failure%previous_policy_retained, failures)
    call assert_eq_str('message source agent', 'agent-42', message_source_agent('42'), failures)
  end subroutine

end program test_client
