! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Session management for the Copilot SDK.
!! Wraps a single conversation session with send, receive, and event handling.
module copilot_session_module
  use copilot_types
  use copilot_jsonrpc
  use copilot_json, only: json_get_raw, json_get_string, json_get_logical, json_quote
  implicit none
  private

  public :: copilot_session_type, copilot_session_create
  public :: session_event_handler
  public :: network_request_result

  integer, parameter :: MAX_HANDLERS = 64

  !> Procedure pointer interface for event handlers.
  abstract interface
    subroutine session_event_handler(event, user_data)
      import :: session_event
      type(session_event), intent(in) :: event
      class(*), intent(inout), optional :: user_data
    end subroutine session_event_handler
  end interface

  !> Procedure pointer interface for exit-plan-mode handlers.
  abstract interface
    subroutine exit_plan_mode_callback_interface(request, response, user_data)
      import :: exit_plan_mode_request_t, exit_plan_mode_response_t
      type(exit_plan_mode_request_t), intent(in) :: request
      type(exit_plan_mode_response_t), intent(out) :: response
      class(*), intent(inout), optional :: user_data
    end subroutine exit_plan_mode_callback_interface
  end interface

  !> Internal event handler entry.
  type :: handler_entry
    integer :: id = 0
    character(len=:), allocatable :: event_filter
    procedure(session_event_handler), pointer, nopass :: callback => null()
    logical :: active = .false.
  end type handler_entry

  !> Represents a single Copilot session.
  type :: copilot_session_type
    character(len=:), allocatable :: session_id
    character(len=:), allocatable :: workspace_path
    type(jsonrpc_client), pointer :: rpc => null()
    logical :: is_active = .false.
    integer :: handler_count = 0
    integer :: next_handler_id = 1
    type(handler_entry) :: handlers(MAX_HANDLERS)
    procedure(exit_plan_mode_callback_interface), pointer, nopass :: exit_plan_mode_handler => null()
    !> Message of the last JSON-RPC error returned for this session.
    character(len=:), allocatable :: last_error
    !> GitHub token provider registered for this session (answers gitHubToken.getToken).
    character(len=:), allocatable :: github_token_registration_id
    procedure(github_token_provider_interface), pointer, nopass :: github_token_provider => null()
    !> Sub-agent lifecycle hooks (hooks.invoke).
    procedure(subagent_start_hook_interface), pointer, nopass :: on_subagent_start => null()
    procedure(subagent_stop_hook_interface), pointer, nopass :: on_subagent_stop => null()
    !> Shell authentication prompt handler (shellAuthentication.request).
    procedure(shell_authentication_handler_interface), pointer, nopass :: on_shell_authentication_request => null()
    !> Memory store callback (memoryStore.store).
    procedure(store_memory_handler_interface), pointer, nopass :: on_store_memory => null()
    !> Ask-user handler (userInput.request).
    procedure(user_input_handler_interface), pointer, nopass :: on_user_input_request => null()
    !> The client's network callbacks, so network.* requests are answered while waiting.
    type(network_options) :: network
    !> Shell authentication request ids cancelled by the runtime (newline separated).
    character(len=:), allocatable :: cancelled_shell_auth_ids
  contains
    procedure :: send           => session_send
    procedure :: send_and_wait  => session_send_and_wait
    procedure :: abort           => session_abort
    procedure :: disconnect      => session_disconnect
    procedure :: detach          => session_detach
    procedure :: destroy         => session_destroy
    procedure :: on              => session_on
    procedure :: off             => session_off
    procedure :: dispatch_event  => session_dispatch_event
    procedure :: get_metadata    => session_get_metadata
    procedure :: register_exit_plan_mode => session_register_exit_plan_mode
    procedure :: configure       => session_configure
    procedure :: switch_auto_tier => session_switch_auto_tier
    procedure :: recheck_policy_helper => session_recheck_policy_helper
    procedure :: set_permission_mode => session_set_permission_mode
    procedure :: owns_github_token_registration => session_owns_github_token_registration
    procedure :: answer_request  => session_answer_request
    procedure :: handle_server_request => session_handle_server_request
  end type copilot_session_type

contains

  ! --------------------------------------------------------------------------
  ! Constructor
  ! --------------------------------------------------------------------------

  !> Create a new session object (does not make the RPC call).
  subroutine copilot_session_create(session, sid, rpc)
    type(copilot_session_type), intent(out) :: session
    character(len=*), intent(in) :: sid
    type(jsonrpc_client), target, intent(in) :: rpc

    session%session_id = sid
    session%rpc => rpc
    session%is_active = .true.
    session%handler_count = 0
    session%next_handler_id = 1
  end subroutine copilot_session_create

  ! --------------------------------------------------------------------------
  ! Event subscription
  ! --------------------------------------------------------------------------

  !> Subscribe to session events. Returns a handler ID for later removal.
  function session_on(self, event_type, callback) result(handler_id)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: event_type
    procedure(session_event_handler) :: callback
    integer :: handler_id

    handler_id = -1
    if (self%handler_count >= MAX_HANDLERS) return

    self%handler_count = self%handler_count + 1
    handler_id = self%next_handler_id
    self%next_handler_id = self%next_handler_id + 1

    self%handlers(self%handler_count)%id = handler_id
    self%handlers(self%handler_count)%event_filter = event_type
    self%handlers(self%handler_count)%callback => callback
    self%handlers(self%handler_count)%active = .true.
  end function session_on

  !> Unsubscribe from session events by handler ID.
  subroutine session_off(self, handler_id)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(in) :: handler_id
    integer :: i

    do i = 1, self%handler_count
      if (self%handlers(i)%id == handler_id) then
        self%handlers(i)%active = .false.
        self%handlers(i)%callback => null()
        exit
      end if
    end do
  end subroutine session_off

  !> Dispatch an event to all matching handlers.
  subroutine session_dispatch_event(self, event)
    class(copilot_session_type), intent(inout) :: self
    type(session_event), intent(in) :: event
    integer :: i

    do i = 1, self%handler_count
      if (.not. self%handlers(i)%active) cycle
      if (.not. associated(self%handlers(i)%callback)) cycle

      ! Match wildcard (empty filter) or exact event type
      if (.not. allocated(self%handlers(i)%event_filter)) then
        call self%handlers(i)%callback(event)
      else if (len_trim(self%handlers(i)%event_filter) == 0) then
        call self%handlers(i)%callback(event)
      else if (self%handlers(i)%event_filter == event%event_type) then
        call self%handlers(i)%callback(event)
      end if
    end do
  end subroutine session_dispatch_event

  ! --------------------------------------------------------------------------
  ! Messaging
  ! --------------------------------------------------------------------------

  !> Send a message to the session (fire-and-forget).
  subroutine session_send(self, opts, iostat)
    class(copilot_session_type), intent(inout) :: self
    type(send_options), intent(in) :: opts
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = build_send_params(self%session_id, opts)
    call self%rpc%send_request('session.send', params, req_id, iostat)
  end subroutine session_send

  !> Send a message and block until session.idle is received.
  !! Server-to-client requests that arrive while waiting (hooks, shell authentication,
  !! memory store, ask-user, GitHub token, network) are answered with the session's callbacks.
  subroutine session_send_and_wait(self, opts, result_out, timeout_ms)
    class(copilot_session_type), intent(inout) :: self
    type(send_options), intent(in) :: opts
    type(send_and_wait_result), intent(out) :: result_out
    integer, intent(in), optional :: timeout_ms
    integer :: req_id, ios, max_iterations, iteration
    character(len=:), allocatable :: msg, method, event_type
    type(session_event) :: evt
    logical :: handled

    max_iterations = 10000
    if (present(timeout_ms)) max_iterations = timeout_ms / 10

    ! Send the message
    call self%rpc%send_request('session.send', &
         build_send_params(self%session_id, opts), req_id, ios)
    if (ios /= 0) then
      result_out%is_error = .true.
      result_out%error_message = 'Failed to send message'
      return
    end if

    result_out%message = ''
    result_out%is_error = .false.

    ! Poll for session.idle
    iteration = 0
    do while (iteration < max_iterations)
      iteration = iteration + 1
      call self%rpc%read_message(msg, ios)
      if (ios /= 0) exit
      if (.not. allocated(msg)) cycle

      if (is_server_request(msg)) then
        call self%handle_server_request(msg, handled, ios)
        if (.not. handled) then
          call self%rpc%send_error_response(jsonrpc_extract_raw_id(msg), JSONRPC_METHOD_NOT_FOUND, &
               'Method not found: ' // json_get_string(msg, 'method'), ios)
        end if
        cycle
      end if

      method = jsonrpc_extract_method(msg)

      if (method == 'session.event') then
        event_type = extract_event_type(msg)
        evt%event_type = event_type
        evt%session_id = self%session_id
        evt%data_json = msg

        call self%dispatch_event(evt)

        if (event_type == 'assistant.message') then
          result_out%message = extract_content(msg)
        end if

        if (event_type == 'session.idle') exit

        if (event_type == 'session.error') then
          result_out%is_error = .true.
          result_out%error_message = extract_content(msg)
          exit
        end if
      end if
    end do
  end subroutine session_send_and_wait

  ! --------------------------------------------------------------------------
  ! Session lifecycle
  ! --------------------------------------------------------------------------

  !> Abort the current processing in this session.
  !! A failure arrives as a JSON-RPC error and is reported through iostat.
  subroutine session_abort(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    character(len=:), allocatable :: response

    call session_request(self, 'session.abort', session_id_params(self), response, iostat)
  end subroutine session_abort

  !> Disconnect this session: detach it from this connection (session.detach) and release
  !! its callbacks. Its state stays on disk, so it can be resumed later.
  subroutine session_disconnect(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    call self%detach(iostat)
  end subroutine session_disconnect

  !> Detach from the session (session.detach) and release its callbacks, including the
  !! GitHub token provider registration.
  subroutine session_detach(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    character(len=:), allocatable :: response, result_json
    logical :: found

    call session_request(self, 'session.detach', session_id_params(self), response, iostat)
    if (iostat /= 0) return
    call json_get_raw(response, 'result', result_json, found)
    if (found) then
      if (.not. json_get_logical(result_json, 'success', .true.)) then
        self%last_error = 'Failed to detach session ' // self%session_id // ': ' // &
                          json_get_string(result_json, 'error')
        iostat = COPILOT_IOSTAT_RPC_ERROR
        return
      end if
    end if
    call release_session(self)
  end subroutine session_detach

  !> Destroy the session on the server.
  subroutine session_destroy(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // self%session_id // '"}'
    call self%rpc%send_request('session.destroy', params, req_id, iostat)
    call release_session(self)
  end subroutine session_destroy

  !> Request an Auto routing tier for the 'auto' model (session.model.switchAutoTier).
  !! Pass an AUTO_TIER_* constant, or '' to return to provider-default routing.
  subroutine session_switch_auto_tier(self, tier, iostat)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: tier
    integer, intent(out) :: iostat
    character(len=:), allocatable :: params, response

    params = '{"sessionId":' // json_quote(self%session_id) // ',"autoTier":'
    if (len_trim(tier) == 0) then
      params = params // 'null}'
    else
      params = params // json_quote(trim(tier)) // '}'
    end if
    call session_request(self, 'session.model.switchAutoTier', params, response, iostat)
  end subroutine session_switch_auto_tier

  !> Re-run the managed-settings Policy Helper for this session
  !! (session.managedSettings.recheckPolicyHelper). Returns the resolved settings snapshot as
  !! JSON and, when present, the Policy Helper failure it reports.
  subroutine session_recheck_policy_helper(self, snapshot_json, iostat, failure, has_failure)
    class(copilot_session_type), intent(inout) :: self
    character(len=:), allocatable, intent(out) :: snapshot_json
    integer, intent(out) :: iostat
    type(policy_helper_failure), intent(out), optional :: failure
    logical, intent(out), optional :: has_failure
    type(policy_helper_failure) :: parsed
    character(len=:), allocatable :: response
    logical :: found

    snapshot_json = ''
    if (present(has_failure)) has_failure = .false.
    call session_request(self, 'session.managedSettings.recheckPolicyHelper', &
                         session_id_params(self), response, iostat)
    if (iostat /= 0) return
    call json_get_raw(response, 'result', snapshot_json, found)
    if (.not. found) snapshot_json = ''
    call parse_policy_helper_failure(snapshot_json, parsed, found)
    if (present(failure)) failure = parsed
    if (present(has_failure)) has_failure = found
  end subroutine session_recheck_policy_helper

  !> Set the session permission mode (session.permissions.setMode). outcome%reason explains a
  !! refusal when known (PERMISSION_MODE_REFUSAL_MANAGED_POLICY).
  subroutine session_set_permission_mode(self, mode, outcome, iostat)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: mode
    type(permission_mode_set_result), intent(out) :: outcome
    integer, intent(out) :: iostat
    character(len=:), allocatable :: params, response, result_json, text
    logical :: found

    params = '{"sessionId":' // json_quote(self%session_id) // ',"mode":' // json_quote(trim(mode)) // '}'
    call session_request(self, 'session.permissions.setMode', params, response, iostat)
    if (iostat /= 0) return
    call json_get_raw(response, 'result', result_json, found)
    if (.not. found) return
    outcome%success = json_get_logical(result_json, 'success', .false.)
    outcome%mode = json_get_string(result_json, 'mode')
    text = json_get_string(result_json, 'reason', found)
    if (found) outcome%reason = text
  end subroutine session_set_permission_mode

  !> Fetch metadata for this session from the server.
  subroutine session_get_metadata(self, meta, iostat)
    class(copilot_session_type), intent(inout) :: self
    type(session_metadata), intent(out) :: meta
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // self%session_id // '"}'
    call self%rpc%send_request('session.getMessages', params, req_id, iostat)
    meta%session_id = self%session_id
    meta%status = 'active'
  end subroutine session_get_metadata

  ! --------------------------------------------------------------------------
  ! Internal helpers
  ! --------------------------------------------------------------------------

  !> Build the JSON params for session.send.
  function build_send_params(session_id, opts) result(params)
    character(len=*), intent(in) :: session_id
    type(send_options), intent(in) :: opts
    character(len=:), allocatable :: params

    params = '{"sessionId":' // json_quote(session_id)
    if (allocated(opts%message)) then
      params = params // ',"prompt":' // json_quote(opts%message)
    end if
    if (allocated(opts%command)) then
      params = params // ',"command":"' // opts%command // '"'
    end if
    if (allocated(opts%agent_mode)) then
      params = params // ',"agentMode":"' // opts%agent_mode // '"'
    end if
    if (allocated(opts%display_prompt)) then
      params = params // ',"displayPrompt":"' // opts%display_prompt // '"'
    end if
    if (allocated(opts%request_headers_json)) then
      params = params // ',"requestHeaders":' // opts%request_headers_json
    end if
    if (allocated(opts%source)) then
      params = params // ',"source":' // json_quote(opts%source)
    end if
    if (allocated(opts%response_schema_json)) then
      params = params // ',"responseFormat":{"type":"json_schema","jsonSchema":' // &
               '{"name":"response","strict":true,"schema":' // opts%response_schema_json // '}}'
    end if
    if (allocated(opts%client_turn_id)) then
      params = params // ',"clientTurnId":' // json_quote(opts%client_turn_id)
    end if
    params = params // '}'
  end function build_send_params

  !> Extract event type from a session.event notification JSON.
  function extract_event_type(json) result(etype)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: etype
    integer :: pos, end_pos

    etype = ''
    pos = index(json, '"type":"')
    if (pos == 0) return
    pos = pos + 8
    end_pos = index(json(pos:), '"')
    if (end_pos == 0) return
    etype = json(pos:pos+end_pos-2)
  end function extract_event_type

  !> Extract "content" field from JSON.
  function extract_content(json) result(content)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: content
    integer :: pos, end_pos

    content = ''
    pos = index(json, '"content":"')
    if (pos == 0) return
    pos = pos + 11
    end_pos = index(json(pos:), '"')
    if (end_pos == 0) return
    content = json(pos:pos+end_pos-2)
  end function extract_content

  !> Register a handler for exit-plan-mode requests.
  subroutine session_register_exit_plan_mode(self, handler)
    class(copilot_session_type), intent(inout) :: self
    procedure(exit_plan_mode_callback_interface) :: handler
    self%exit_plan_mode_handler => handler
  end subroutine session_register_exit_plan_mode

  ! --------------------------------------------------------------------------
  ! Session callbacks and server-to-client requests
  ! --------------------------------------------------------------------------

  !> Attach the session-scoped callbacks of a session configuration, the client's network
  !! callbacks, and the GitHub token provider registration id generated for this session.
  subroutine session_configure(self, config, network, github_token_registration_id)
    class(copilot_session_type), intent(inout) :: self
    type(session_config), intent(in), optional :: config
    type(network_options), intent(in), optional :: network
    character(len=*), intent(in), optional :: github_token_registration_id

    if (present(config)) then
      self%github_token_provider => config%github_token_provider
      self%on_subagent_start => config%on_subagent_start
      self%on_subagent_stop => config%on_subagent_stop
      self%on_shell_authentication_request => config%on_shell_authentication_request
      self%on_store_memory => config%on_store_memory
      self%on_user_input_request => config%on_user_input_request
    end if
    if (present(network)) self%network = network
    if (present(github_token_registration_id)) then
      if (len(github_token_registration_id) > 0) then
        self%github_token_registration_id = github_token_registration_id
      end if
    end if
  end subroutine session_configure

  !> True when this active session registered the given GitHub token provider id.
  function session_owns_github_token_registration(self, registration_id) result(owns)
    class(copilot_session_type), intent(in) :: self
    character(len=*), intent(in) :: registration_id
    logical :: owns

    owns = .false.
    if (.not. self%is_active) return
    if (.not. associated(self%github_token_provider)) return
    if (.not. allocated(self%github_token_registration_id)) return
    owns = self%github_token_registration_id == registration_id .and. &
           len(self%github_token_registration_id) == len(registration_id)
  end function session_owns_github_token_registration

  !> Compute the reply to a server-to-client request addressed to this session:
  !! gitHubToken.getToken, hooks.invoke, shellAuthentication.request,
  !! shellAuthentication.cancel, memoryStore.store, userInput.request, and network.* callbacks.
  !! handled is .false. for other methods or requests for another session.
  !! On a callback failure error_text is set instead of result_json.
  subroutine session_answer_request(self, method, params, handled, result_json, error_text)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: method, params
    logical, intent(out) :: handled
    character(len=:), allocatable, intent(out) :: result_json, error_text
    character(len=:), allocatable :: target_session
    logical :: found

    handled = .true.
    result_json = 'null'
    error_text = ''

    select case (method)
    case ('hooks.invoke', 'shellAuthentication.request', 'shellAuthentication.cancel', &
          'memoryStore.store', 'userInput.request')
      target_session = json_get_string(params, 'sessionId', found)
      if (found .and. allocated(self%session_id)) then
        if (target_session /= self%session_id) then
          handled = .false.
          return
        end if
      end if
    end select

    select case (method)
    case ('gitHubToken.getToken')
      call answer_github_token(self, params, result_json, error_text)
    case ('hooks.invoke')
      call answer_hooks_invoke(self, params, result_json)
    case ('shellAuthentication.request')
      call answer_shell_authentication(self, params, result_json)
    case ('shellAuthentication.cancel')
      ! Requests are answered synchronously, so only a request that has not been read yet
      ! can still be pending: remember the id and decline it when it arrives.
      call remember_cancelled_shell_auth(self, json_get_string(params, 'requestId'))
      result_json = '{}'
    case ('memoryStore.store')
      call answer_memory_store(self, params, result_json, error_text)
    case ('userInput.request')
      call answer_user_input(self, params, result_json, error_text)
    case default
      call network_request_result(self%network, method, params, handled, result_json, error_text)
    end select
  end subroutine session_answer_request

  !> Answer a server-to-client JSON-RPC request with this session's callbacks and write the
  !! response. handled is .false. when the message is not a request this session answers.
  subroutine session_handle_server_request(self, message, handled, iostat)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: message
    logical, intent(out) :: handled
    integer, intent(out) :: iostat
    character(len=:), allocatable :: params, result_json, error_text, id_json
    logical :: found

    iostat = 0
    handled = .false.
    if (.not. is_server_request(message)) return
    call json_get_raw(message, 'params', params, found)
    if (.not. found) params = '{}'
    call self%answer_request(json_get_string(message, 'method'), params, handled, result_json, error_text)
    if (.not. handled) return
    if (.not. associated(self%rpc)) return
    id_json = jsonrpc_extract_raw_id(message)
    if (len(error_text) > 0) then
      call self%rpc%send_error_response(id_json, JSONRPC_INTERNAL_ERROR, error_text, iostat)
    else
      call self%rpc%send_response(id_json, result_json, iostat)
    end if
  end subroutine session_handle_server_request

  !> Compute the reply to a network.* callback request using the given network callbacks.
  subroutine network_request_result(network, method, params, handled, result_json, error_text)
    type(network_options), intent(in) :: network
    character(len=*), intent(in) :: method, params
    logical, intent(out) :: handled
    character(len=:), allocatable, intent(out) :: result_json, error_text
    type(network_basic_auth_request) :: basic_request
    type(network_basic_credentials) :: credentials
    character(len=:), allocatable :: text
    logical :: found

    handled = .true.
    result_json = '{}'
    error_text = ''
    select case (method)
    case ('network.resolveProxy')
      if (.not. associated(network%resolve_proxy)) then
        error_text = 'No network resolveProxy callback is registered'
        return
      end if
      text = network%resolve_proxy(json_get_string(params, 'url'))
      if (len(text) > 0) result_json = '{"proxy":' // json_quote(text) // '}'
    case ('network.authenticateNegotiate')
      if (.not. associated(network%authenticate_negotiate)) then
        error_text = 'No network authenticateNegotiate callback is registered'
        return
      end if
      text = network%authenticate_negotiate(json_get_string(params, 'spn'))
      if (len(text) > 0) result_json = '{"token":' // json_quote(text) // '}'
    case ('network.authenticateBasic')
      if (.not. associated(network%authenticate_basic)) then
        error_text = 'No network authenticateBasic callback is registered'
        return
      end if
      basic_request%proxy = json_get_string(params, 'proxy')
      text = json_get_string(params, 'realm', found)
      if (found) basic_request%realm = text
      basic_request%previous_rejected = json_get_logical(params, 'previousRejected', .false.)
      credentials = network%authenticate_basic(basic_request)
      text = ''
      if (allocated(credentials%username)) text = text // ',"username":' // json_quote(credentials%username)
      if (allocated(credentials%password)) text = text // ',"password":' // json_quote(credentials%password)
      if (len(text) > 0) result_json = '{' // text(2:) // '}'
    case default
      handled = .false.
    end select
  end subroutine network_request_result

  !> Answer gitHubToken.getToken with this session's provider.
  subroutine answer_github_token(self, params, result_json, error_text)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: params
    character(len=:), allocatable, intent(inout) :: result_json, error_text
    type(github_token_provider_args) :: args
    type(github_token_result) :: token
    character(len=:), allocatable :: registration_id, text
    character(len=20) :: buf
    logical :: found

    registration_id = json_get_string(params, 'registrationId')
    if (.not. self%owns_github_token_registration(registration_id)) then
      error_text = 'No GitHub token provider registered for registration ID "' // registration_id // '"'
      return
    end if
    args%host = json_get_string(params, 'host')
    text = json_get_string(params, 'sessionId', found)
    if (found) then
      args%session_id = text
    else
      args%session_id = self%session_id
    end if
    args%reason = json_get_string(params, 'reason')
    token = self%github_token_provider(args)
    if (token%cancelled .or. .not. allocated(token%access_token)) then
      result_json = '{"kind":"cancelled"}'
      return
    end if
    write(buf, '(I0)') token%expires_in
    result_json = '{"kind":"token","accessToken":' // json_quote(token%access_token)
    if (allocated(token%token_type)) then
      result_json = result_json // ',"tokenType":' // json_quote(token%token_type)
    end if
    result_json = result_json // ',"expiresIn":' // trim(buf) // '}'
  end subroutine answer_github_token

  !> Answer hooks.invoke for the sub-agent lifecycle hooks; other hook types reply {}.
  subroutine answer_hooks_invoke(self, params, result_json)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: params
    character(len=:), allocatable, intent(inout) :: result_json
    type(subagent_start_input) :: start_input
    type(subagent_stop_input) :: stop_input
    type(subagent_start_output) :: start_output
    type(subagent_stop_output) :: stop_output
    character(len=:), allocatable :: hook_type, input_json, members
    logical :: found

    result_json = '{}'
    hook_type = json_get_string(params, 'hookType')
    call json_get_raw(params, 'input', input_json, found)
    if (.not. found) input_json = '{}'

    select case (hook_type)
    case ('subagentStart')
      if (.not. associated(self%on_subagent_start)) return
      call read_subagent_input(input_json, self%session_id, start_input)
      start_output = self%on_subagent_start(start_input)
      if (allocated(start_output%additional_context)) then
        result_json = '{"output":{"additionalContext":' // &
                      json_quote(start_output%additional_context) // '}}'
      end if
    case ('subagentStop')
      if (.not. associated(self%on_subagent_stop)) return
      call read_subagent_input(input_json, self%session_id, stop_input)
      stop_input%stop_reason = json_get_string(input_json, 'stopReason')
      stop_input%response = json_get_string(input_json, 'response')
      stop_output = self%on_subagent_stop(stop_input)
      members = ''
      if (allocated(stop_output%decision)) then
        members = members // ',"decision":' // json_quote(stop_output%decision)
      end if
      if (allocated(stop_output%reason)) then
        members = members // ',"reason":' // json_quote(stop_output%reason)
      end if
      if (allocated(stop_output%modified_response)) then
        members = members // ',"modifiedResponse":' // json_quote(stop_output%modified_response)
      end if
      if (len(members) > 0) result_json = '{"output":{' // members(2:) // '}}'
    end select
  end subroutine answer_hooks_invoke

  !> Fill the shared sub-agent hook input fields from the wire input object.
  subroutine read_subagent_input(input_json, default_session_id, input)
    character(len=*), intent(in) :: input_json, default_session_id
    class(subagent_start_input), intent(inout) :: input
    character(len=:), allocatable :: text
    logical :: found

    input%input_json = input_json
    text = json_get_string(input_json, 'sessionId', found)
    if (found) then
      input%session_id = text
    else
      input%session_id = default_session_id
    end if
    input%working_directory = json_get_string(input_json, 'cwd')
    input%transcript_path = json_get_string(input_json, 'transcriptPath')
    input%agent_name = json_get_string(input_json, 'agentName')
    text = json_get_string(input_json, 'agentDisplayName', found)
    if (found) input%agent_display_name = text
    text = json_get_string(input_json, 'agentDescription', found)
    if (found) input%agent_description = text
    text = json_get_string(input_json, 'agentId', found)
    if (found) input%agent_id = text
    text = json_get_string(input_json, 'agentType', found)
    if (found) input%agent_type = text
    text = json_get_string(input_json, 'parentAgentId', found)
    if (found) input%parent_agent_id = text
  end subroutine read_subagent_input

  !> Answer shellAuthentication.request: {action:"accept", value} or {action:"cancel"}.
  subroutine answer_shell_authentication(self, params, result_json)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: params
    character(len=:), allocatable, intent(inout) :: result_json
    type(shell_authentication_request) :: request
    type(shell_authentication_result) :: answer

    result_json = '{"action":"cancel"}'
    request%request_id = json_get_string(params, 'requestId')
    if (shell_auth_was_cancelled(self, request%request_id)) return
    if (.not. associated(self%on_shell_authentication_request)) return
    request%session_id = json_get_string(params, 'sessionId')
    request%program = json_get_string(params, 'program')
    request%prompt = json_get_string(params, 'prompt')
    request%input_kind = json_get_string(params, 'inputKind')
    request%command = json_get_string(params, 'command')
    request%cwd = json_get_string(params, 'cwd')
    answer = self%on_shell_authentication_request(request)
    if (.not. answer%accepted) return
    if (allocated(answer%value)) then
      result_json = '{"action":"accept","value":' // json_quote(answer%value) // '}'
    else
      result_json = '{"action":"accept","value":""}'
    end if
  end subroutine answer_shell_authentication

  !> Remember a cancelled shell authentication request id.
  subroutine remember_cancelled_shell_auth(self, request_id)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: request_id

    if (len(request_id) == 0) return
    if (.not. allocated(self%cancelled_shell_auth_ids)) self%cancelled_shell_auth_ids = ''
    self%cancelled_shell_auth_ids = self%cancelled_shell_auth_ids // request_id // achar(10)
  end subroutine remember_cancelled_shell_auth

  !> True when the runtime already cancelled this shell authentication request.
  function shell_auth_was_cancelled(self, request_id) result(cancelled)
    class(copilot_session_type), intent(in) :: self
    character(len=*), intent(in) :: request_id
    logical :: cancelled

    cancelled = .false.
    if (len(request_id) == 0) return
    if (.not. allocated(self%cancelled_shell_auth_ids)) return
    cancelled = index(achar(10) // self%cancelled_shell_auth_ids, &
                      achar(10) // request_id // achar(10)) > 0
  end function shell_auth_was_cancelled

  !> Answer memoryStore.store by invoking the memory store callback (replies null).
  subroutine answer_memory_store(self, params, result_json, error_text)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: params
    character(len=:), allocatable, intent(inout) :: result_json, error_text
    type(stored_memory) :: memory

    if (.not. associated(self%on_store_memory)) then
      error_text = 'No memory store callback is registered for session ' // self%session_id
      return
    end if
    memory%session_id = json_get_string(params, 'sessionId')
    memory%subject = json_get_string(params, 'subject')
    memory%fact = json_get_string(params, 'fact')
    memory%citations = json_get_string(params, 'citations')
    memory%reason = json_get_string(params, 'reason')
    call self%on_store_memory(memory)
    result_json = 'null'
  end subroutine answer_memory_store

  !> Answer userInput.request (ask_user) with the session's handler. The request carries
  !! image_attachment_constraints (when the runtime accepts images), request_id, and meta_json.
  subroutine answer_user_input(self, params, result_json, error_text)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: params
    character(len=:), allocatable, intent(inout) :: result_json, error_text
    type(user_input_request) :: request
    type(user_input_response) :: response

    if (.not. associated(self%on_user_input_request)) then
      error_text = 'No user input handler is registered for session ' // self%session_id
      return
    end if
    call parse_user_input_request(params, request)
    response = self%on_user_input_request(request)
    result_json = user_input_response_json(response)
  end subroutine answer_user_input

  !> Send a request for this session and read its response; a JSON-RPC error is reported
  !! as iostat = COPILOT_IOSTAT_RPC_ERROR with the message in self%last_error.
  subroutine session_request(self, method, params, response, iostat)
    class(copilot_session_type), intent(inout) :: self
    character(len=*), intent(in) :: method, params
    character(len=:), allocatable, intent(out) :: response
    integer, intent(out) :: iostat
    character(len=:), allocatable :: error_json
    integer :: req_id
    logical :: is_error

    response = ''
    if (.not. associated(self%rpc)) then
      self%last_error = 'Session is not connected'
      iostat = COPILOT_IOSTAT_RPC_ERROR
      return
    end if
    call self%rpc%send_request(method, params, req_id, iostat)
    if (iostat /= 0) return
    call self%rpc%read_message(response, iostat)
    if (.not. allocated(response)) response = ''
    if (iostat /= 0) return
    call jsonrpc_response_error(response, error_json, is_error)
    if (is_error) then
      self%last_error = jsonrpc_error_message(error_json)
      iostat = COPILOT_IOSTAT_RPC_ERROR
    end if
  end subroutine session_request

  !> {"sessionId":"..."} for this session.
  function session_id_params(self) result(params)
    class(copilot_session_type), intent(in) :: self
    character(len=:), allocatable :: params

    params = '{"sessionId":' // json_quote(self%session_id) // '}'
  end function session_id_params

  !> Mark the session inactive and drop its callbacks and token provider registration.
  subroutine release_session(self)
    class(copilot_session_type), intent(inout) :: self

    self%is_active = .false.
    self%github_token_provider => null()
    if (allocated(self%github_token_registration_id)) deallocate(self%github_token_registration_id)
    self%on_subagent_start => null()
    self%on_subagent_stop => null()
    self%on_shell_authentication_request => null()
    self%on_store_memory => null()
    self%on_user_input_request => null()
  end subroutine release_session

  !> True for a JSON-RPC request (top-level method and id members).
  function is_server_request(message) result(is_request)
    character(len=*), intent(in) :: message
    logical :: is_request
    character(len=:), allocatable :: raw
    logical :: has_method, has_id

    call json_get_raw(message, 'method', raw, has_method)
    call json_get_raw(message, 'id', raw, has_id)
    is_request = has_method .and. has_id
  end function is_server_request

end module copilot_session_module
