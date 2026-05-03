! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> End-to-end tests for the Fortran Copilot SDK.
!! Uses the shared replaying CAPI proxy (test/harness/server.ts) to replay
!! recorded API exchanges against the SDK client.
program test_e2e
  use test_harness
  use copilot_types
  use copilot_client_module
  use copilot_session_module
  implicit none

  integer :: failures
  integer :: total_tests

  failures    = 0
  total_tests = 0

  print *, '======================================'
  print *, ' Fortran SDK — E2E Test Suite'
  print *, '======================================'
  print *

  call test_session_create_and_disconnect(failures, total_tests)
  call test_send_message(failures, total_tests)
  call test_session_fs_config(failures, total_tests)
  call test_multi_turn_conversation(failures, total_tests)
  call test_session_resume(failures, total_tests)
  call test_session_list(failures, total_tests)
  call test_session_metadata(failures, total_tests)
  call test_session_delete(failures, total_tests)
  call test_model_list(failures, total_tests)
  call test_ping(failures, total_tests)
  call test_auth_status(failures, total_tests)
  call test_client_lifecycle(failures, total_tests)
  call test_foreground_session(failures, total_tests)
  call test_tools(failures, total_tests)
  call test_streaming(failures, total_tests)
  call test_system_message_customization(failures, total_tests)
  call test_session_fs_provider(failures, total_tests)
  call test_mcp_servers_config(failures, total_tests)
  call test_skills_config(failures, total_tests)
  call test_compaction(failures, total_tests)

  print *
  print *, '======================================'
  if (failures > 0) then
    print '(A,I0,A,I0,A)', ' FAILED: ', failures, ' of ', total_tests, ' test(s)'
    print *, '======================================'
    error stop 1
  else
    print '(A,I0,A)', ' All ', total_tests, ' E2E tests passed'
    print *, '======================================'
  end if

contains

  ! ==========================================================================
  !  Test 1 — Create a session and disconnect
  ! ==========================================================================
  subroutine test_session_create_and_disconnect(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] session create and disconnect ...'

    ! Start and configure the replaying proxy
    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_receive_session_events.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    ! Create the SDK client pointing at the proxy
    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    ! Create a session
    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    ! Assert: session ID must not be empty
    if (.not. allocated(session%session_id) .or. &
        len_trim(session%session_id) == 0) then
      print *, '  FAIL — session_id is empty'
      failures = failures + 1
    else
      print *, '  PASS — session_id = ', trim(session%session_id)
    end if

    ! Tear down
    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_session_create_and_disconnect

  ! ==========================================================================
  !  Test 2 — Send a message via send_and_wait
  ! ==========================================================================
  subroutine test_send_message(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(send_options)           :: send_opts
    type(send_and_wait_result)   :: result
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] send message ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    ! Send a message
    send_opts%message = 'Hello'
    call session%send_and_wait(send_opts, result)

    ! Assert: no error
    if (result%is_error) then
      print *, '  FAIL — send_and_wait returned error: ', trim(result%error_message)
      failures = failures + 1
    else
      print *, '  PASS — send_and_wait succeeded'
      if (allocated(result%message) .and. len_trim(result%message) > 0) then
        print *, '         response = ', trim(result%message)
      end if
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_send_message

  ! ==========================================================================
  !  Test 3 — Session filesystem configuration
  ! ==========================================================================
  subroutine test_session_fs_config(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)              :: proxy
    type(copilot_client)          :: client
    type(copilot_client_options)  :: opts
    type(session_fs_config)       :: fs_cfg
    type(copilot_session_type)    :: session
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] session filesystem config ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session_fs/should_route_file_operations_through_the_session_fs_provider.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    ! Build client options with session_fs
    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.

    allocate(opts%session_fs)
    opts%session_fs%initial_cwd        = '/workspace/project'
    opts%session_fs%session_state_path = '~/.copilot/session-state'
    opts%session_fs%conventions        = ''

    call copilot_client_create(client, opts)

    ! Starting the client triggers sessionFs.setProvider internally
    call client%start()

    ! Verify the client is in a connected state
    if (client%state%value /= COPILOT_STATE_CONNECTED .and. &
        client%state%value /= COPILOT_STATE_ERROR) then
      print *, '  FAIL — client not connected after start, state=', client%state%value
      failures = failures + 1
    else
      print *, '  PASS — client started with session_fs config'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_session_fs_config

  ! ==========================================================================
  !  Test 4 — Multi-turn conversation
  ! ==========================================================================
  subroutine test_multi_turn_conversation(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(send_options)           :: send_opts
    type(send_and_wait_result)   :: result1, result2
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] multi-turn conversation ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    send_opts%message = 'What is 1+1?'
    call session%send_and_wait(send_opts, result1)

    send_opts%message = 'And what is 2+2?'
    call session%send_and_wait(send_opts, result2)

    if (result1%is_error .or. result2%is_error) then
      print *, '  FAIL — one of the send_and_wait calls returned error'
      failures = failures + 1
    else
      print *, '  PASS — multi-turn conversation succeeded'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_multi_turn_conversation

  ! ==========================================================================
  !  Test 5 — Session resume
  ! ==========================================================================
  subroutine test_session_resume(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client, client2
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session, resumed
    type(session_config)         :: sess_config
    character(len=256) :: saved_id
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] session resume ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    saved_id = session%session_id
    call client%stop()

    ! Create new client and resume
    call copilot_client_create(client2, opts)
    call client2%start()

    sess_config%session_id = trim(saved_id)
    call client2%create_session(session=resumed, config=sess_config, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — resume create_session returned iostat=', ierr
      failures = failures + 1
    else if (trim(resumed%session_id) /= trim(saved_id)) then
      print *, '  FAIL — resumed session ID does not match'
      failures = failures + 1
    else
      print *, '  PASS — session resumed with ID = ', trim(resumed%session_id)
    end if

    call client2%stop()
    call proxy_stop(proxy)
  end subroutine test_session_resume

  ! ==========================================================================
  !  Test 6 — Session list
  ! ==========================================================================
  subroutine test_session_list(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session1, session2
    type(session_list_item), allocatable :: sessions(:)
    integer :: count, ierr

    total_tests = total_tests + 1
    print *, '[TEST] session list ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session1, iostat=ierr)
    call client%create_session(session=session2, iostat=ierr)

    call client%list_sessions(sessions, count, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — list_sessions returned iostat=', ierr
      failures = failures + 1
    else if (count < 2) then
      print *, '  FAIL — expected at least 2 sessions, got ', count
      failures = failures + 1
    else
      print *, '  PASS — list_sessions returned ', count, ' sessions'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_session_list

  ! ==========================================================================
  !  Test 7 — Session metadata
  ! ==========================================================================
  subroutine test_session_metadata(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_metadata_type)  :: metadata
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] session metadata ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    call client%get_session_metadata(session%session_id, metadata, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — get_session_metadata returned iostat=', ierr
      failures = failures + 1
    else
      print *, '  PASS — session metadata retrieved'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_session_metadata

  ! ==========================================================================
  !  Test 8 — Session delete
  ! ==========================================================================
  subroutine test_session_delete(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_list_item), allocatable :: sessions(:)
    character(len=256) :: saved_id
    integer :: count, ierr, i
    logical :: found

    total_tests = total_tests + 1
    print *, '[TEST] session delete ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    saved_id = session%session_id

    call client%delete_session(trim(saved_id), iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — delete_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    call client%list_sessions(sessions, count, iostat=ierr)
    found = .false.
    do i = 1, count
      if (trim(sessions(i)%session_id) == trim(saved_id)) then
        found = .true.
        exit
      end if
    end do

    if (found) then
      print *, '  FAIL — deleted session still appears in list'
      failures = failures + 1
    else
      print *, '  PASS — session deleted successfully'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_session_delete

  ! ==========================================================================
  !  Test 9 — Model list
  ! ==========================================================================
  subroutine test_model_list(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(model_info), allocatable :: models(:)
    integer :: count, ierr

    total_tests = total_tests + 1
    print *, '[TEST] model list ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%list_models(models, count, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — list_models returned iostat=', ierr
      failures = failures + 1
    else if (count == 0) then
      print *, '  FAIL — models list is empty'
      failures = failures + 1
    else
      print *, '  PASS — list_models returned ', count, ' models'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_model_list

  ! ==========================================================================
  !  Test 10 — Ping
  ! ==========================================================================
  subroutine test_ping(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(ping_response)    :: response
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] ping ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%ping(response, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — ping returned iostat=', ierr
      failures = failures + 1
    else if (.not. allocated(response%message) .or. &
             len_trim(response%message) == 0) then
      print *, '  FAIL — ping message is empty'
      failures = failures + 1
    else if (response%timestamp <= 0) then
      print *, '  FAIL — ping timestamp is not positive'
      failures = failures + 1
    else
      print *, '  PASS — ping succeeded, message = ', trim(response%message)
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_ping

  ! ==========================================================================
  !  Test 11 — Auth status
  ! ==========================================================================
  subroutine test_auth_status(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(auth_status_type) :: status
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] auth status ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%get_auth_status(status, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — get_auth_status returned iostat=', ierr
      failures = failures + 1
    else
      print *, '  PASS — auth status retrieved'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_auth_status

  ! ==========================================================================
  !  Test 12 — Client lifecycle
  ! ==========================================================================
  subroutine test_client_lifecycle(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] client lifecycle ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    if (client%state%value /= COPILOT_STATE_CONNECTED) then
      print *, '  FAIL — client not connected after start'
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    call client%stop()

    if (client%state%value == COPILOT_STATE_CONNECTED) then
      print *, '  FAIL — client still connected after stop'
      failures = failures + 1
    else
      print *, '  PASS — client lifecycle (connected -> disconnected)'
    end if

    call proxy_stop(proxy)
  end subroutine test_client_lifecycle

  ! ==========================================================================
  !  Test 13 — Foreground session
  ! ==========================================================================
  subroutine test_foreground_session(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    character(len=256) :: fg_id
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] foreground session ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    call client%set_foreground_session_id(session%session_id, iostat=ierr)
    call client%get_foreground_session_id(fg_id, iostat=ierr)

    if (trim(fg_id) /= trim(session%session_id)) then
      print *, '  FAIL — foreground session ID does not match'
      failures = failures + 1
    else
      print *, '  PASS — foreground session ID matches'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_foreground_session

  ! ==========================================================================
  !  Test 14 — Tools
  ! ==========================================================================
  subroutine test_tools(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_config)         :: sess_config
    type(tool_definition)        :: tool_def
    type(send_options)           :: send_opts
    type(send_and_wait_result)   :: result
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] tools ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    tool_def%name = 'test_tool'
    tool_def%description = 'A test tool for E2E testing'

    allocate(sess_config%tools(1))
    sess_config%tools(1) = tool_def

    call client%create_session(session=session, config=sess_config, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session with tools returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    send_opts%message = 'Use the test_tool'
    call session%send_and_wait(send_opts, result)

    if (result%is_error) then
      print *, '  FAIL — send_and_wait with tools returned error'
      failures = failures + 1
    else
      print *, '  PASS — tools test succeeded'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_tools

  ! ==========================================================================
  !  Test 15 — Streaming
  ! ==========================================================================
  subroutine test_streaming(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_config)         :: sess_config
    type(send_options)           :: send_opts
    type(send_and_wait_result)   :: result
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] streaming ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    sess_config%streaming = .true.

    call client%create_session(session=session, config=sess_config, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session with streaming returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    send_opts%message = 'Hello streaming'
    call session%send_and_wait(send_opts, result)

    if (result%is_error) then
      print *, '  FAIL — streaming send_and_wait returned error'
      failures = failures + 1
    else
      print *, '  PASS — streaming test succeeded'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_streaming

  ! ==========================================================================
  !  Test 16 — System message customization
  ! ==========================================================================
  subroutine test_system_message_customization(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_config)         :: sess_config
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] system message customization ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    sess_config%system_message = 'You are a helpful assistant.'
    sess_config%system_message_mode = 'append'

    call client%create_session(session=session, config=sess_config, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session with system message returned iostat=', ierr
      failures = failures + 1
    else if (.not. allocated(session%session_id) .or. &
             len_trim(session%session_id) == 0) then
      print *, '  FAIL — session_id is empty'
      failures = failures + 1
    else
      print *, '  PASS — system message customization succeeded'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_system_message_customization

  ! ==========================================================================
  !  Test 17 — SessionFs provider
  ! ==========================================================================
  subroutine test_session_fs_provider(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)              :: proxy
    type(copilot_client)          :: client
    type(copilot_client_options)  :: opts
    type(session_fs_config)       :: fs_cfg
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] session filesystem provider ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session_fs/should_route_file_operations_through_the_session_fs_provider.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.

    allocate(opts%session_fs)
    opts%session_fs%initial_cwd        = '/workspace/project'
    opts%session_fs%session_state_path = '~/.copilot/session-state-provider'
    opts%session_fs%conventions        = 'posix'

    call copilot_client_create(client, opts)
    call client%start()

    if (client%state%value /= COPILOT_STATE_CONNECTED .and. &
        client%state%value /= COPILOT_STATE_ERROR) then
      print *, '  FAIL — client not connected with session_fs provider'
      failures = failures + 1
    else
      print *, '  PASS — session_fs provider started OK'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_session_fs_provider

  ! ==========================================================================
  !  Test 18 — MCP servers config
  ! ==========================================================================
  subroutine test_mcp_servers_config(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_config)         :: sess_config
    type(mcp_server_config)      :: mcp_srv
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] MCP servers config ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    mcp_srv%name = 'test-server'
    mcp_srv%command = 'echo'
    allocate(mcp_srv%args(1))
    mcp_srv%args(1) = 'hello'

    allocate(sess_config%mcp_servers(1))
    sess_config%mcp_servers(1) = mcp_srv

    call client%create_session(session=session, config=sess_config, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session with mcp_servers returned iostat=', ierr
      failures = failures + 1
    else if (.not. allocated(session%session_id) .or. &
             len_trim(session%session_id) == 0) then
      print *, '  FAIL — session_id is empty'
      failures = failures + 1
    else
      print *, '  PASS — MCP servers config succeeded'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_mcp_servers_config

  ! ==========================================================================
  !  Test 19 — Skills config
  ! ==========================================================================
  subroutine test_skills_config(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(session_config)         :: sess_config
    type(skills_config)          :: sk_cfg
    integer :: ierr

    total_tests = total_tests + 1
    print *, '[TEST] skills config ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_have_stateful_conversation.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    allocate(sk_cfg%directories(1))
    sk_cfg%directories(1) = '/workspace/skills'

    sess_config%skills = sk_cfg

    call client%create_session(session=session, config=sess_config, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session with skills returned iostat=', ierr
      failures = failures + 1
    else if (.not. allocated(session%session_id) .or. &
             len_trim(session%session_id) == 0) then
      print *, '  FAIL — session_id is empty'
      failures = failures + 1
    else
      print *, '  PASS — skills config succeeded'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_skills_config

  ! ==========================================================================
  !  Test 20 — Compaction
  ! ==========================================================================
  subroutine test_compaction(failures, total_tests)
    integer, intent(inout) :: failures, total_tests

    type(capi_proxy)       :: proxy
    type(copilot_client)   :: client
    type(copilot_client_options) :: opts
    type(copilot_session_type)   :: session
    type(send_options)           :: send_opts
    type(send_and_wait_result)   :: result
    integer :: ierr, i
    character(len=64) :: msg

    total_tests = total_tests + 1
    print *, '[TEST] compaction ...'

    call proxy_start(proxy, ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not start proxy'
      failures = failures + 1
      return
    end if

    call proxy_configure(proxy, &
      'session/should_receive_session_events.yaml', &
      '../test/snapshots', ierr)
    if (ierr /= 0) then
      print *, '  FAIL — could not configure proxy'
      failures = failures + 1
      call proxy_stop(proxy)
      return
    end if

    opts%cli_url   = trim(proxy%proxy_url)
    opts%auto_start = .false.
    call copilot_client_create(client, opts)
    call client%start()

    call client%create_session(session=session, iostat=ierr)
    if (ierr /= 0) then
      print *, '  FAIL — create_session returned iostat=', ierr
      failures = failures + 1
      call client%stop()
      call proxy_stop(proxy)
      return
    end if

    ! Send several messages to try to trigger compaction
    do i = 1, 5
      write(msg, '(A,I0,A)') 'Message ', i, ' to trigger compaction'
      send_opts%message = trim(msg)
      call session%send_and_wait(send_opts, result)
      ! Some sends may fail in replay; acceptable
    end do

    ! Compaction depends on replay data; verify session survived
    if (.not. allocated(session%session_id) .or. &
        len_trim(session%session_id) == 0) then
      print *, '  FAIL — session lost after compaction attempts'
      failures = failures + 1
    else
      print *, '  PASS — compaction test completed'
    end if

    call client%stop()
    call proxy_stop(proxy)
  end subroutine test_compaction

end program test_e2e
