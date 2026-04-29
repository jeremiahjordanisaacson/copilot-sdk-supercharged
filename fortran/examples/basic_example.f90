! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Basic example: create a client, open a session, and send a message.
program basic_example
  use copilot_client_module
  use copilot_types
  use copilot_session_module
  implicit none

  type(copilot_client)        :: client
  type(copilot_client_options) :: opts
  type(session_config)        :: config
  type(copilot_session_type)  :: session
  type(send_options)          :: sopts
  type(send_and_wait_result)  :: result
  integer                     :: ios

  ! ------------------------------------------------------------------
  ! 1. Configure and create the client
  ! ------------------------------------------------------------------
  opts%log_level = 'info'
  ! opts%cli_path = '/usr/local/bin/copilot'   ! uncomment to override
  call copilot_client_create(client, opts)

  ! ------------------------------------------------------------------
  ! 2. Create a session
  ! ------------------------------------------------------------------
  config%model = 'gpt-4'
  config%streaming = .true.
  call copilot_create_session(client, config, session, ios)
  if (ios /= 0) then
    print *, 'ERROR: failed to create session'
    call copilot_stop(client)
    stop 1
  end if

  print *, 'Session created: ', session%session_id

  ! ------------------------------------------------------------------
  ! 3. Send a message and wait for the response
  ! ------------------------------------------------------------------
  sopts%message = 'What is 2 + 2?'
  call session%send_and_wait(sopts, result)

  if (result%is_error) then
    print *, 'ERROR: ', result%error_message
  else
    print *, 'Response: ', result%message
  end if

  ! ------------------------------------------------------------------
  ! 4. Clean up
  ! ------------------------------------------------------------------
  call session%destroy(ios)
  call copilot_stop(client)

  print *, 'Done.'
end program basic_example
