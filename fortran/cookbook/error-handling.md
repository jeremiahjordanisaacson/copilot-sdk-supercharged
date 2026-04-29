# Error Handling in Fortran

Patterns for handling errors gracefully in the Copilot SDK, including connection failures, timeouts, and resource cleanup.

## Scenario

Your application needs to handle network errors, CLI process failures, and timeouts without crashing.

## Checking Connection Health

```fortran
program check_connection
  use copilot_client_module
  use copilot_types
  implicit none

  type(copilot_client)        :: client
  type(copilot_client_options) :: opts
  integer                     :: ios

  opts%auto_start = .false.
  call copilot_client_create(client, opts)

  call copilot_start(client)
  call client%ping(ios)

  if (ios /= 0) then
    print *, 'ERROR: CLI server is not responding'
    call copilot_stop(client)
    stop 1
  end if

  print *, 'Connection is healthy'
  call copilot_stop(client)
end program
```

## Handling Session Errors

```fortran
type(send_and_wait_result) :: result
type(send_options)         :: sopts

sopts%message = 'Do something risky'
call session%send_and_wait(sopts, result, timeout_ms=30000)

if (result%is_error) then
  print *, 'Session error: ', result%error_message
  ! Optionally abort and retry
  call session%abort(ios)
else
  print *, 'Success: ', result%message
end if
```

## Cleanup on Failure

Always pair resource creation with destruction:

```fortran
type(copilot_client)       :: client
type(copilot_session_type) :: session
integer                    :: ios

call copilot_client_create(client)
call copilot_create_session(client, session=session, iostat=ios)

if (ios /= 0) then
  print *, 'Failed to create session'
  call copilot_stop(client)
  stop 1
end if

! ... do work ...

! Always destroy the session and stop the client
call session%destroy(ios)
call copilot_stop(client)
```

## Retry Pattern

```fortran
integer :: attempt, max_attempts, ios

max_attempts = 3
do attempt = 1, max_attempts
  call client%ping(ios)
  if (ios == 0) exit
  print '(A,I0,A,I0)', 'Retry ', attempt, ' of ', max_attempts
  call sleep(1)  ! Wait before retrying
end do

if (ios /= 0) then
  print *, 'All retry attempts failed'
  stop 1
end if
```
