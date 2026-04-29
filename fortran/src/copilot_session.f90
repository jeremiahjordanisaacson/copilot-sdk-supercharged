! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Session management for the Copilot SDK.
!! Wraps a single conversation session with send, receive, and event handling.
module copilot_session_module
  use copilot_types
  use copilot_jsonrpc
  implicit none
  private

  public :: copilot_session_type, copilot_session_create
  public :: session_event_handler

  integer, parameter :: MAX_HANDLERS = 64

  !> Procedure pointer interface for event handlers.
  abstract interface
    subroutine session_event_handler(event, user_data)
      import :: session_event
      type(session_event), intent(in) :: event
      class(*), intent(inout), optional :: user_data
    end subroutine session_event_handler
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
  contains
    procedure :: send           => session_send
    procedure :: send_and_wait  => session_send_and_wait
    procedure :: abort           => session_abort
    procedure :: disconnect      => session_disconnect
    procedure :: destroy         => session_destroy
    procedure :: on              => session_on
    procedure :: off             => session_off
    procedure :: dispatch_event  => session_dispatch_event
    procedure :: get_metadata    => session_get_metadata
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
  subroutine session_send_and_wait(self, opts, result_out, timeout_ms)
    class(copilot_session_type), intent(inout) :: self
    type(send_options), intent(in) :: opts
    type(send_and_wait_result), intent(out) :: result_out
    integer, intent(in), optional :: timeout_ms
    integer :: req_id, ios, max_iterations, iteration
    character(len=:), allocatable :: msg, method, event_type
    type(session_event) :: evt

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
  subroutine session_abort(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // self%session_id // '"}'
    call self%rpc%send_request('session.abort', params, req_id, iostat)
  end subroutine session_abort

  !> Disconnect (destroy) this session on the server.
  subroutine session_disconnect(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    call self%destroy(iostat)
  end subroutine session_disconnect

  !> Destroy the session on the server.
  subroutine session_destroy(self, iostat)
    class(copilot_session_type), intent(inout) :: self
    integer, intent(out) :: iostat
    integer :: req_id
    character(len=:), allocatable :: params

    params = '{"sessionId":"' // self%session_id // '"}'
    call self%rpc%send_request('session.destroy', params, req_id, iostat)
    self%is_active = .false.
  end subroutine session_destroy

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

    params = '{"sessionId":"' // session_id // '"'
    if (allocated(opts%message)) then
      params = params // ',"message":"' // opts%message // '"'
    end if
    if (allocated(opts%command)) then
      params = params // ',"command":"' // opts%command // '"'
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

end module copilot_session_module
