# Persisting Sessions in Fortran

Save and resume sessions across application restarts.

## Scenario

Your application needs to maintain long-running conversations that survive process restarts. The Copilot CLI supports infinite sessions that persist workspace state to `~/.copilot/session-state/{sessionId}`.

## Saving a Session ID

After creating a session, store the session ID for later use:

```fortran
program persist_session
  use copilot_client_module
  use copilot_types
  use copilot_session_module
  implicit none

  type(copilot_client)       :: client
  type(copilot_session_type) :: session
  type(send_options)         :: sopts
  type(send_and_wait_result) :: result
  integer                    :: ios, unit_num

  call copilot_client_create(client)
  call copilot_create_session(client, session=session, iostat=ios)

  ! Save session ID to a file
  open(newunit=unit_num, file='session_id.txt', status='replace', &
       action='write', iostat=ios)
  if (ios == 0) then
    write(unit_num, '(A)') session%session_id
    close(unit_num)
  end if

  ! Use the session
  sopts%message = 'Remember that my project uses Fortran 2018.'
  call session%send_and_wait(sopts, result)

  call copilot_stop(client)
end program
```

## Resuming a Saved Session

```fortran
program resume_saved
  use copilot_client_module
  use copilot_types
  use copilot_session_module
  implicit none

  type(copilot_client)       :: client
  type(copilot_session_type) :: session
  type(send_options)         :: sopts
  type(send_and_wait_result) :: result
  integer                    :: ios, unit_num
  character(len=256)         :: saved_id

  call copilot_client_create(client)

  ! Read saved session ID
  open(newunit=unit_num, file='session_id.txt', status='old', &
       action='read', iostat=ios)
  if (ios /= 0) then
    print *, 'No saved session found, creating new one'
    call copilot_create_session(client, session=session, iostat=ios)
  else
    read(unit_num, '(A)') saved_id
    close(unit_num)
    call copilot_resume_session(client, trim(saved_id), session, ios)
    if (ios /= 0) then
      print *, 'Resume failed, creating new session'
      call copilot_create_session(client, session=session, iostat=ios)
    end if
  end if

  ! The session remembers previous context
  sopts%message = 'What language is my project using?'
  call session%send_and_wait(sopts, result)
  print *, result%message  ! Should mention Fortran 2018

  call copilot_stop(client)
end program
```

## Notes

- Infinite sessions persist automatically to `~/.copilot/session-state/`.
- The CLI handles context compaction when the conversation grows long.
- Session IDs are stable strings that can be stored in files, databases, or environment variables.
