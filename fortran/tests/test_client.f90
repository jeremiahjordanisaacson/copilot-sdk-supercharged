! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Unit tests for the Copilot SDK Fortran modules.
program test_client
  use copilot_types
  use copilot_version
  use copilot_jsonrpc
  use copilot_tools
  use copilot_session_module
  use copilot_client_module
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
    call assert_eq_str('protocol version', '3', SDK_PROTOCOL_VERSION, failures)
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

    msg = jsonrpc_build_request(1, 'ping', '{"protocolVersion":"3"}')
    call assert_true('request contains jsonrpc', index(msg, '"jsonrpc":"2.0"') > 0, failures)
    call assert_true('request contains method', index(msg, '"method":"ping"') > 0, failures)
    call assert_true('request contains id', index(msg, '"id":1') > 0, failures)
    call assert_true('request contains params', &
                     index(msg, '"params":{"protocolVersion":"3"}') > 0, failures)
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

end program test_client
