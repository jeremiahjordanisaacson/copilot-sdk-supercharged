! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Main Copilot client module.
!! Manages the CLI process lifecycle, JSON-RPC transport, and session creation.
module copilot_client_module
  use iso_fortran_env, only: error_unit
  use copilot_types
  use copilot_jsonrpc
  use copilot_session_module
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

    self%state%value = COPILOT_STATE_CONNECTING

    ! Build the CLI command
    cli_cmd = build_cli_command(self%options)

    ! Spawn the CLI process using execute_command_line (blocking placeholder).
    ! In production a pipe-based spawn via ISO_C_BINDING would be used.
    ! For now, we set up the RPC client with placeholder units.
    call jsonrpc_create(self%rpc, 5, 6)  ! stdin=5, stdout=6 as placeholders

    self%state%value = COPILOT_STATE_CONNECTED
    self%cli_process_running = .true.

    ! Initial ping to verify the connection
    call self%ping(ios)
    if (ios /= 0) then
      self%state%value = COPILOT_STATE_ERROR
      write(error_unit, '(A)') 'copilot_client: ping failed after start'
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

  !> Ping the CLI server to verify connectivity.
  subroutine client_ping(self, iostat)
    class(copilot_client), intent(inout) :: self
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"protocolVersion":"' // SDK_PROTOCOL_VERSION // '"}'
    call self%rpc%send_request('ping', params, req_id, iostat)
  end subroutine client_ping

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
  subroutine client_create_session(self, config, session, iostat)
    class(copilot_client), intent(inout) :: self
    type(session_config), intent(in), optional :: config
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    integer :: req_id, idx
    character(len=:), allocatable :: params, response, sid

    ! Build params
    params = '{'
    if (present(config)) then
      if (allocated(config%model)) then
        params = params // '"model":"' // config%model // '",'
      end if
      if (allocated(config%system_prompt)) then
        params = params // '"systemPrompt":"' // config%system_prompt // '",'
      end if
      if (allocated(config%workspace_path)) then
        params = params // '"workspacePath":"' // config%workspace_path // '",'
      end if
      if (allocated(config%github_token)) then
        params = params // '"githubToken":"' // config%github_token // '",'
      end if
      if (config%session_idle_timeout_seconds > 0) then
        ! Would add timeout param here
      end if
    end if
    ! Remove trailing comma if present, close brace
    if (len(params) > 1 .and. params(len(params):len(params)) == ',') then
      params = params(1:len(params)-1) // '}'
    else
      params = params // '}'
    end if

    call self%rpc%send_request('session.create', params, req_id, iostat)
    if (iostat /= 0) return

    ! Read response to get session ID
    call self%rpc%read_message(response, iostat)
    if (iostat /= 0) return

    sid = extract_session_id(response)
    if (len_trim(sid) == 0) then
      sid = 'session-' // char(ichar('0') + mod(req_id, 10))
    end if

    call copilot_session_create(session, sid, self%rpc)

    ! Track the session
    idx = self%session_count + 1
    if (idx <= MAX_SESSIONS) then
      self%sessions(idx) = session
      self%session_count = idx
    end if
  end subroutine client_create_session

  !> Resume an existing session by ID.
  subroutine client_resume_session(self, session_id, session, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: session_id
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    integer :: req_id, idx
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // session_id // '"}'
    call self%rpc%send_request('session.resume', params, req_id, iostat)
    if (iostat /= 0) return

    call copilot_session_create(session, session_id, self%rpc)

    idx = self%session_count + 1
    if (idx <= MAX_SESSIONS) then
      self%sessions(idx) = session
      self%session_count = idx
    end if
  end subroutine client_resume_session

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

  subroutine copilot_resume_session(client, session_id, session, iostat)
    type(copilot_client), intent(inout) :: client
    character(len=*), intent(in) :: session_id
    type(copilot_session_type), intent(out) :: session
    integer, intent(out) :: iostat
    call client%resume_session(session_id, session, iostat)
  end subroutine copilot_resume_session

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

  !> Delete a session by ID.
  subroutine client_delete_session(self, session_id, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: session_id
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // session_id // '"}'
    call self%rpc%send_request('session.delete', params, req_id, iostat)
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

  !> Set the foreground session (TUI mode).
  subroutine client_set_foreground(self, session_id, iostat)
    class(copilot_client), intent(inout) :: self
    character(len=*), intent(in) :: session_id
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // session_id // '"}'
    call self%rpc%send_request('session.setForeground', params, req_id, iostat)
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
  function build_cli_command(opts) result(cmd)
    type(copilot_client_options), intent(in) :: opts
    character(len=:), allocatable :: cmd

    if (allocated(opts%cli_path)) then
      cmd = opts%cli_path
    else
      cmd = 'copilot'
    end if

    cmd = cmd // ' --headless --no-auto-update --stdio'

    if (allocated(opts%log_level)) then
      cmd = cmd // ' --log-level ' // opts%log_level
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

end module copilot_client_module
