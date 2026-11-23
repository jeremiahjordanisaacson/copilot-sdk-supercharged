! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> JSON-RPC 2.0 transport layer.
!! Reads and writes Content-Length-framed JSON-RPC messages over stdio pipes.
module copilot_jsonrpc
  use iso_fortran_env, only: int32, int64, error_unit
  use copilot_json, only: json_get_raw, json_get_string, json_get_integer, json_quote
  implicit none
  private

  public :: jsonrpc_client, jsonrpc_create, jsonrpc_destroy
  public :: jsonrpc_send_request, jsonrpc_send_notification
  public :: jsonrpc_read_message, jsonrpc_build_request
  public :: jsonrpc_build_notification, jsonrpc_extract_id
  public :: jsonrpc_extract_method, jsonrpc_extract_result
  public :: jsonrpc_extract_error, jsonrpc_is_response
  public :: jsonrpc_is_notification, jsonrpc_is_request
  public :: jsonrpc_extract_raw_id, jsonrpc_build_response, jsonrpc_build_error_response
  public :: jsonrpc_response_error, jsonrpc_error_code, jsonrpc_error_message
  public :: JSONRPC_METHOD_NOT_FOUND, JSONRPC_INTERNAL_ERROR

  integer, parameter :: MAX_MSG_LEN = 1048576  ! 1 MB
  integer, parameter :: HEADER_BUF  = 256

  !> Standard JSON-RPC error codes used when answering server requests.
  integer, parameter :: JSONRPC_METHOD_NOT_FOUND = -32601
  integer, parameter :: JSONRPC_INTERNAL_ERROR   = -32603

  !> JSON-RPC client state
  type :: jsonrpc_client
    integer :: stdin_unit  = -1
    integer :: stdout_unit = -1
    integer :: next_id     = 1
    logical :: connected   = .false.
  contains
    procedure :: send_request    => jrpc_send_request
    procedure :: send_notification => jrpc_send_notification
    procedure :: send_response   => jrpc_send_response
    procedure :: send_error_response => jrpc_send_error_response
    procedure :: read_message    => jrpc_read_message
    procedure :: get_next_id     => jrpc_get_next_id
  end type jsonrpc_client

contains

  ! --------------------------------------------------------------------------
  ! Constructor / destructor
  ! --------------------------------------------------------------------------

  !> Create a JSON-RPC client with the given I/O units.
  subroutine jsonrpc_create(client, in_unit, out_unit)
    type(jsonrpc_client), intent(out) :: client
    integer, intent(in) :: in_unit, out_unit

    client%stdin_unit  = in_unit
    client%stdout_unit = out_unit
    client%next_id     = 1
    client%connected   = .true.
  end subroutine jsonrpc_create

  !> Destroy a JSON-RPC client.
  subroutine jsonrpc_destroy(client)
    type(jsonrpc_client), intent(inout) :: client
    client%connected = .false.
    client%stdin_unit  = -1
    client%stdout_unit = -1
  end subroutine jsonrpc_destroy

  ! --------------------------------------------------------------------------
  ! ID management
  ! --------------------------------------------------------------------------

  !> Return the next request ID and increment counter.
  function jrpc_get_next_id(self) result(id)
    class(jsonrpc_client), intent(inout) :: self
    integer :: id
    id = self%next_id
    self%next_id = self%next_id + 1
  end function jrpc_get_next_id

  ! --------------------------------------------------------------------------
  ! Message builders
  ! --------------------------------------------------------------------------

  !> Build a JSON-RPC 2.0 request string.
  function jsonrpc_build_request(id, method, params_json) result(msg)
    integer, intent(in) :: id
    character(len=*), intent(in) :: method
    character(len=*), intent(in) :: params_json
    character(len=:), allocatable :: msg
    character(len=20) :: id_str

    write(id_str, '(I0)') id

    if (len_trim(params_json) > 0) then
      msg = '{"jsonrpc":"2.0","id":' // trim(id_str) // &
            ',"method":"' // trim(method) // &
            '","params":' // trim(params_json) // '}'
    else
      msg = '{"jsonrpc":"2.0","id":' // trim(id_str) // &
            ',"method":"' // trim(method) // '"}'
    end if
  end function jsonrpc_build_request

  !> Build a JSON-RPC 2.0 notification string (no id).
  function jsonrpc_build_notification(method, params_json) result(msg)
    character(len=*), intent(in) :: method
    character(len=*), intent(in) :: params_json
    character(len=:), allocatable :: msg

    if (len_trim(params_json) > 0) then
      msg = '{"jsonrpc":"2.0","method":"' // trim(method) // &
            '","params":' // trim(params_json) // '}'
    else
      msg = '{"jsonrpc":"2.0","method":"' // trim(method) // '"}'
    end if
  end function jsonrpc_build_notification

  ! --------------------------------------------------------------------------
  ! Send helpers
  ! --------------------------------------------------------------------------

  !> Frame and write a JSON-RPC request to the output unit.
  subroutine jrpc_send_request(self, method, params_json, id, iostat)
    class(jsonrpc_client), intent(inout) :: self
    character(len=*), intent(in) :: method
    character(len=*), intent(in) :: params_json
    integer, intent(out) :: id
    integer, intent(out) :: iostat
    character(len=:), allocatable :: body

    id = self%get_next_id()
    body = jsonrpc_build_request(id, method, params_json)
    call write_framed_message(self%stdout_unit, body, iostat)
  end subroutine jrpc_send_request

  !> Convenience wrapper matching the module-level public name.
  subroutine jsonrpc_send_request(client, method, params_json, id, iostat)
    type(jsonrpc_client), intent(inout) :: client
    character(len=*), intent(in) :: method
    character(len=*), intent(in) :: params_json
    integer, intent(out) :: id
    integer, intent(out) :: iostat
    call client%send_request(method, params_json, id, iostat)
  end subroutine jsonrpc_send_request

  !> Frame and write a JSON-RPC notification to the output unit.
  subroutine jrpc_send_notification(self, method, params_json, iostat)
    class(jsonrpc_client), intent(inout) :: self
    character(len=*), intent(in) :: method
    character(len=*), intent(in) :: params_json
    integer, intent(out) :: iostat
    character(len=:), allocatable :: body

    body = jsonrpc_build_notification(method, params_json)
    call write_framed_message(self%stdout_unit, body, iostat)
  end subroutine jrpc_send_notification

  !> Convenience wrapper matching the module-level public name.
  subroutine jsonrpc_send_notification(client, method, params_json, iostat)
    type(jsonrpc_client), intent(inout) :: client
    character(len=*), intent(in) :: method
    character(len=*), intent(in) :: params_json
    integer, intent(out) :: iostat
    call client%send_notification(method, params_json, iostat)
  end subroutine jsonrpc_send_notification

  !> Build a JSON-RPC 2.0 success response. id_json is the raw request id ('7' or '"abc"').
  function jsonrpc_build_response(id_json, result_json) result(msg)
    character(len=*), intent(in) :: id_json
    character(len=*), intent(in) :: result_json
    character(len=:), allocatable :: msg

    if (len_trim(result_json) > 0) then
      msg = '{"jsonrpc":"2.0","id":' // trim(id_json) // ',"result":' // trim(result_json) // '}'
    else
      msg = '{"jsonrpc":"2.0","id":' // trim(id_json) // ',"result":null}'
    end if
  end function jsonrpc_build_response

  !> Build a JSON-RPC 2.0 error response.
  function jsonrpc_build_error_response(id_json, code, message) result(msg)
    character(len=*), intent(in) :: id_json
    integer, intent(in) :: code
    character(len=*), intent(in) :: message
    character(len=:), allocatable :: msg
    character(len=20) :: code_str

    write(code_str, '(I0)') code
    msg = '{"jsonrpc":"2.0","id":' // trim(id_json) // ',"error":{"code":' // trim(code_str) // &
          ',"message":' // json_quote(message) // '}}'
  end function jsonrpc_build_error_response

  !> Answer a server-to-client request with a result.
  subroutine jrpc_send_response(self, id_json, result_json, iostat)
    class(jsonrpc_client), intent(inout) :: self
    character(len=*), intent(in) :: id_json
    character(len=*), intent(in) :: result_json
    integer, intent(out) :: iostat

    call write_framed_message(self%stdout_unit, jsonrpc_build_response(id_json, result_json), iostat)
  end subroutine jrpc_send_response

  !> Answer a server-to-client request with an error.
  subroutine jrpc_send_error_response(self, id_json, code, message, iostat)
    class(jsonrpc_client), intent(inout) :: self
    character(len=*), intent(in) :: id_json
    integer, intent(in) :: code
    character(len=*), intent(in) :: message
    integer, intent(out) :: iostat

    call write_framed_message(self%stdout_unit, jsonrpc_build_error_response(id_json, code, message), iostat)
  end subroutine jrpc_send_error_response

  ! --------------------------------------------------------------------------
  ! Read
  ! --------------------------------------------------------------------------

  !> Read one Content-Length-framed message from the input unit.
  subroutine jrpc_read_message(self, message, iostat)
    class(jsonrpc_client), intent(in) :: self
    character(len=:), allocatable, intent(out) :: message
    integer, intent(out) :: iostat
    call read_framed_message(self%stdin_unit, message, iostat)
  end subroutine jrpc_read_message

  !> Convenience wrapper matching the module-level public name.
  subroutine jsonrpc_read_message(client, message, iostat)
    type(jsonrpc_client), intent(in) :: client
    character(len=:), allocatable, intent(out) :: message
    integer, intent(out) :: iostat
    call client%read_message(message, iostat)
  end subroutine jsonrpc_read_message

  ! --------------------------------------------------------------------------
  ! Message classification helpers
  ! --------------------------------------------------------------------------

  !> Extract the "id" integer from a JSON-RPC message (returns -1 if absent).
  function jsonrpc_extract_id(json) result(id)
    character(len=*), intent(in) :: json
    integer :: id
    integer :: pos, end_pos
    character(len=20) :: num_str

    id = -1
    pos = index(json, '"id":')
    if (pos == 0) return
    pos = pos + 5
    end_pos = pos
    do while (end_pos <= len(json))
      if (json(end_pos:end_pos) == ',' .or. json(end_pos:end_pos) == '}') exit
      end_pos = end_pos + 1
    end do
    num_str = json(pos:end_pos-1)
    read(num_str, *, iostat=pos) id
  end function jsonrpc_extract_id

  !> Extract the "method" string from a JSON-RPC message.
  function jsonrpc_extract_method(json) result(method)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: method
    method = extract_string_field(json, '"method":"')
  end function jsonrpc_extract_method

  !> Extract the "result" substring from a JSON-RPC response.
  function jsonrpc_extract_result(json) result(res)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: res
    integer :: pos

    pos = index(json, '"result":')
    if (pos == 0) then
      res = ''
      return
    end if
    pos = pos + 9
    res = json(pos:len_trim(json)-1)
  end function jsonrpc_extract_result

  !> Extract the "error" substring from a JSON-RPC response.
  function jsonrpc_extract_error(json) result(err)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: err
    integer :: pos

    pos = index(json, '"error":')
    if (pos == 0) then
      err = ''
      return
    end if
    pos = pos + 8
    err = json(pos:len_trim(json)-1)
  end function jsonrpc_extract_error

  !> Check if the message is a response (has "id" and "result" or "error").
  function jsonrpc_is_response(json) result(is_resp)
    character(len=*), intent(in) :: json
    logical :: is_resp
    is_resp = (index(json, '"id":') > 0) .and. &
              (index(json, '"result":') > 0 .or. index(json, '"error":') > 0)
  end function jsonrpc_is_response

  !> Check if the message is a notification (has "method" but no "id").
  function jsonrpc_is_notification(json) result(is_notif)
    character(len=*), intent(in) :: json
    logical :: is_notif
    is_notif = (index(json, '"method":') > 0) .and. (index(json, '"id":') == 0)
  end function jsonrpc_is_notification

  !> Check if the message is a request (has both "method" and "id").
  function jsonrpc_is_request(json) result(is_req)
    character(len=*), intent(in) :: json
    logical :: is_req
    is_req = (index(json, '"method":') > 0) .and. (index(json, '"id":') > 0) &
             .and. (index(json, '"result":') == 0)
  end function jsonrpc_is_request

  !> Raw JSON text of the top-level "id" member ('' when absent).
  function jsonrpc_extract_raw_id(json) result(id_json)
    character(len=*), intent(in) :: json
    character(len=:), allocatable :: id_json
    logical :: found

    call json_get_raw(json, 'id', id_json, found)
    if (.not. found) id_json = ''
  end function jsonrpc_extract_raw_id

  !> The top-level "error" object of a response, if any.
  subroutine jsonrpc_response_error(json, error_json, is_error)
    character(len=*), intent(in) :: json
    character(len=:), allocatable, intent(out) :: error_json
    logical, intent(out) :: is_error

    call json_get_raw(json, 'error', error_json, is_error)
    if (is_error) is_error = error_json /= 'null'
  end subroutine jsonrpc_response_error

  !> The "code" of a JSON-RPC error object (0 when absent).
  function jsonrpc_error_code(error_json) result(code)
    character(len=*), intent(in) :: error_json
    integer :: code
    logical :: found

    call json_get_integer(error_json, 'code', code, found)
  end function jsonrpc_error_code

  !> The "message" of a JSON-RPC error object.
  function jsonrpc_error_message(error_json) result(message)
    character(len=*), intent(in) :: error_json
    character(len=:), allocatable :: message

    message = json_get_string(error_json, 'message')
  end function jsonrpc_error_message

  ! --------------------------------------------------------------------------
  ! Internal framing helpers
  ! --------------------------------------------------------------------------

  !> Write a Content-Length-framed message.
  subroutine write_framed_message(unit_num, body, iostat)
    integer, intent(in) :: unit_num
    character(len=*), intent(in) :: body
    integer, intent(out) :: iostat
    character(len=HEADER_BUF) :: header
    integer :: body_len

    body_len = len_trim(body)
    write(header, '(A,I0,A,A)') 'Content-Length: ', body_len, &
          char(13) // char(10), char(13) // char(10)
    write(unit_num, '(A)', iostat=iostat, advance='no') trim(header)
    if (iostat /= 0) return
    write(unit_num, '(A)', iostat=iostat, advance='no') trim(body)
    flush(unit_num)
  end subroutine write_framed_message

  !> Read a Content-Length-framed message.
  subroutine read_framed_message(unit_num, message, iostat)
    integer, intent(in) :: unit_num
    character(len=:), allocatable, intent(out) :: message
    integer, intent(out) :: iostat
    character(len=HEADER_BUF) :: line
    integer :: content_length, pos

    content_length = -1
    iostat = 0

    ! Read headers until blank line
    do
      read(unit_num, '(A)', iostat=iostat) line
      if (iostat /= 0) return

      ! Strip trailing CR if present
      pos = len_trim(line)
      if (pos > 0 .and. ichar(line(pos:pos)) == 13) then
        line(pos:pos) = ' '
      end if

      ! Empty line signals end of headers
      if (len_trim(line) == 0) exit

      ! Parse Content-Length header
      pos = index(line, 'Content-Length:')
      if (pos > 0) then
        read(line(pos+15:), *, iostat=iostat) content_length
        if (iostat /= 0) return
      end if
    end do

    if (content_length <= 0 .or. content_length > MAX_MSG_LEN) then
      iostat = -1
      message = ''
      return
    end if

    ! Read the body
    allocate(character(len=content_length) :: message)
    read(unit_num, '(A)', iostat=iostat, advance='no', size=pos) message
  end subroutine read_framed_message

  !> Extract a simple quoted string value following a given key pattern.
  function extract_string_field(json, key) result(val)
    character(len=*), intent(in) :: json, key
    character(len=:), allocatable :: val
    integer :: pos, end_pos

    val = ''
    pos = index(json, key)
    if (pos == 0) return
    pos = pos + len(key)
    end_pos = index(json(pos:), '"')
    if (end_pos == 0) return
    val = json(pos:pos+end_pos-2)
  end function extract_string_field

end module copilot_jsonrpc
