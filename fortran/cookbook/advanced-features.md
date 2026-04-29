# Advanced v2.0 Features in Fortran

Recipes for using advanced v2.0 features of the GitHub Copilot SDK in Fortran, including per-session auth, SessionFs, system prompts, streaming, and more.

## Scenario

Your application needs fine-grained control over authentication, filesystem access, prompt customization, and other advanced capabilities introduced in SDK v2.0.

## 1. Per-Session Authentication

Supply a GitHub token per session instead of globally.

```fortran
program per_session_auth
  use copilot_client_module
  use copilot_types
  use copilot_session_module
  implicit none

  type(copilot_client)       :: client
  type(session_config)       :: config
  type(copilot_session_type) :: session
  type(send_options)         :: sopts
  type(send_and_wait_result) :: result
  integer                    :: ios

  call copilot_client_create(client)

  config%github_token = 'ghu_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
  call client%create_session(config, session, ios)

  sopts%message = 'Who am I authenticated as?'
  call session%send_and_wait(sopts, result)
  print *, result%message

  call copilot_stop(client)
end program
```

## 2. Session Idle Timeout

Automatically expire sessions after a period of inactivity.

```fortran
type(session_config) :: config
config%session_idle_timeout_seconds = 600  ! 10-minute timeout
call client%create_session(config, session, ios)
```

## 3. SessionFs (Session Filesystem)

Configure a virtual filesystem for the session.

```fortran
type(session_config)   :: config
type(session_fs_config) :: fs
type(session_fs_entry) :: entries(2)

entries(1)%path    = '/project/main.f90'
entries(1)%content = 'program main; print *, "Hello"; end program'

entries(2)%path         = '/project/data'
entries(2)%is_directory = .true.

fs%cwd     = '/project'
fs%entries = entries
config%fs  = fs

call client%create_session(config, session, ios)
```

## 4. System Prompts

Customize the assistant behavior with a system prompt.

```fortran
config%system_prompt = 'You are an expert in computational fluid dynamics. ' // &
                       'Always provide Fortran 2018 code examples. ' // &
                       'Use coarray features when parallelism is needed.'

call client%create_session(config, session, ios)
```

## 5. Streaming Events

Subscribe to streaming delta events for real-time output.

```fortran
integer :: hid

hid = session%on('assistant.message_delta', print_delta)

! ... send a message ...

call session%off(hid)

! Event handler subroutine
subroutine print_delta(event, user_data)
  use copilot_types
  type(session_event), intent(in) :: event
  class(*), intent(inout), optional :: user_data

  ! event%data_json contains the delta content
  print '(A)', event%data_json
end subroutine
```

## 6. Model Selection

Specify which model to use for a session.

```fortran
config%model = 'gpt-4'
call client%create_session(config, session, ios)

! Or override per-message
sopts%model = 'claude-sonnet-4'
call session%send_and_wait(sopts, result)
```

## 7. Listing Models

Query which models are available.

```fortran
character(len=:), allocatable :: models_json
integer :: ios

call client%list_models(models_json, ios)
if (ios == 0) then
  print *, 'Available models: ', models_json
end if
```

## 8. Authentication Status

Check whether the user is signed in.

```fortran
character(len=:), allocatable :: auth_json
integer :: ios

call client%get_auth_status(auth_json, ios)
if (ios == 0) then
  print *, 'Auth: ', auth_json
end if
```

## 9. Foreground Session (TUI Mode)

Set a session as the foreground session for TUI integration.

```fortran
call client%set_foreground(session%session_id, ios)
```
