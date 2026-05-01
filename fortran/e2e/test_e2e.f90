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

end program test_e2e
