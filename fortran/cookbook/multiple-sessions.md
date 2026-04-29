# Multiple Sessions in Fortran

Managing multiple independent conversation sessions simultaneously with the GitHub Copilot SDK.

## Scenario

Your application needs to run several conversations in parallel, each with its own context and configuration.

## Creating Multiple Sessions

```fortran
program multi_session
  use copilot_client_module
  use copilot_types
  use copilot_session_module
  implicit none

  type(copilot_client)       :: client
  type(session_config)       :: config_code, config_docs
  type(copilot_session_type) :: session_code, session_docs
  type(send_options)         :: sopts
  type(send_and_wait_result) :: result
  integer                    :: ios

  call copilot_client_create(client)

  ! Session for code tasks
  config_code%system_prompt = 'You are a Fortran code assistant.'
  config_code%model = 'gpt-4'
  call client%create_session(config_code, session_code, ios)

  ! Session for documentation tasks
  config_docs%system_prompt = 'You write technical documentation.'
  config_docs%model = 'gpt-4'
  call client%create_session(config_docs, session_docs, ios)

  ! Use each session independently
  sopts%message = 'Write a quicksort subroutine'
  call session_code%send_and_wait(sopts, result)
  print *, 'Code:', result%message

  sopts%message = 'Document the quicksort subroutine'
  call session_docs%send_and_wait(sopts, result)
  print *, 'Docs:', result%message

  ! Clean up all sessions
  call session_code%destroy(ios)
  call session_docs%destroy(ios)
  call copilot_stop(client)
end program
```

## Session Isolation

Each session maintains its own conversation history. Messages sent to one session do not affect the others. This is useful for:

- Separating code generation from documentation tasks
- Running different models for different purposes
- Keeping context windows independent to avoid confusion

## Iterating Over Sessions

```fortran
integer :: i, ios

! List all sessions on the server
character(len=:), allocatable :: sessions_json
call client%list_sessions(sessions_json, ios)
if (ios == 0) then
  print *, 'Sessions: ', sessions_json
end if
```
