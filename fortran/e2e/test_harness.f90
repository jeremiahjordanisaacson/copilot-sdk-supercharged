! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> Replaying CAPI proxy management for E2E tests.
!! Spawns the shared test harness server (test/harness/server.ts) and provides
!! helpers to configure and stop it.  Uses file-based IPC and curl system calls
!! since Fortran has no native HTTP or process-pipe support.
module test_harness
  use iso_fortran_env, only: error_unit
  implicit none
  private

  public :: capi_proxy, proxy_start, proxy_stop, proxy_configure

  integer, parameter :: URL_LEN  = 256
  integer, parameter :: PATH_LEN = 512

  !> State for a single replaying-proxy instance.
  type :: capi_proxy
    integer            :: pid       = -1
    character(len=URL_LEN) :: proxy_url = ''
    logical            :: running   = .false.
  end type capi_proxy

contains

  ! ==========================================================================
  !  Start the replay proxy
  ! ==========================================================================

  !> Launch `npx tsx server.ts` in the background, capture its URL from stdout.
  subroutine proxy_start(proxy, ierr)
    type(capi_proxy), intent(inout) :: proxy
    integer, intent(out)            :: ierr

    character(len=PATH_LEN) :: server_path, harness_dir, cmd
    character(len=URL_LEN)  :: line
    integer                 :: iu, ios, pos, end_pos, wait_count

    ierr = 0

    ! ---- Locate the harness server relative to this file's repo position ----
    ! Expected layout: <repo>/fortran/e2e/  ->  <repo>/test/harness/server.ts
    server_path = '../../../test/harness/server.ts'
    harness_dir = '../../../test/harness'

    ! ---- Build command: run in background, redirect stdout to temp file -----
    ! On Windows we use `start /B`, on Unix-like we append `&`.
#ifdef _WIN32
    cmd = 'cd /d ' // trim(harness_dir) // ' && start /B npx tsx server.ts > ' // &
          '"' // trim(harness_dir) // '/proxy_output.txt" 2>&1'
#else
    cmd = 'cd ' // trim(harness_dir) // ' && npx tsx server.ts > proxy_output.txt 2>&1 &'
#endif

    call execute_command_line(trim(cmd), wait=.false., exitstat=ios)
    if (ios /= 0) then
      write(error_unit, '(A,I0)') 'test_harness: failed to spawn proxy, exitstat=', ios
      ierr = ios
      return
    end if

    ! ---- Wait for the server to write its "Listening: http://..." line ------
    wait_count = 0
    do
      call sleep_ms(500)
      wait_count = wait_count + 1

      ! Try to open the output file
      open(newunit=iu, file=trim(harness_dir) // '/proxy_output.txt', &
           status='old', action='read', iostat=ios)
      if (ios /= 0) then
        if (wait_count > 20) then    ! 10 s total
          write(error_unit, '(A)') 'test_harness: timed out waiting for proxy output file'
          ierr = -1
          return
        end if
        cycle
      end if

      ! Read line(s) looking for the URL
      read(iu, '(A)', iostat=ios) line
      close(iu)

      if (ios == 0) then
        pos = index(line, 'Listening: http://')
        if (pos > 0) then
          ! Extract URL (from "http://" to end of non-space)
          pos = index(line, 'http://')
          end_pos = len_trim(line)
          proxy%proxy_url = line(pos:end_pos)
          proxy%running = .true.
          exit
        end if
      end if

      if (wait_count > 20) then
        write(error_unit, '(A)') 'test_harness: timed out waiting for Listening URL'
        ierr = -2
        return
      end if
    end do

    ! ---- Export the URL so the CLI can discover it --------------------------
    call set_env('COPILOT_API_URL', trim(proxy%proxy_url))

  end subroutine proxy_start

  ! ==========================================================================
  !  Stop the replay proxy
  ! ==========================================================================

  !> Gracefully stop the proxy by POSTing to /stop, then clean up.
  subroutine proxy_stop(proxy)
    type(capi_proxy), intent(inout) :: proxy
    character(len=PATH_LEN) :: cmd
    integer :: ios

    if (.not. proxy%running) return

    ! POST to /stop (best effort)
    cmd = 'curl -s -X POST "' // trim(proxy%proxy_url) // '/stop?skipWritingCache=true"'
    call execute_command_line(trim(cmd), wait=.true., exitstat=ios)

    ! Clean up the temp output file
    cmd = 'rm -f ../../../test/harness/proxy_output.txt 2>/dev/null'
    call execute_command_line(trim(cmd), wait=.true., exitstat=ios)

    proxy%running = .false.
    proxy%proxy_url = ''
    proxy%pid = -1
  end subroutine proxy_stop

  ! ==========================================================================
  !  Configure the proxy with a snapshot file
  ! ==========================================================================

  !> POST JSON to /config so the proxy replays the requested snapshot.
  subroutine proxy_configure(proxy, file_path, work_dir, ierr)
    type(capi_proxy), intent(in) :: proxy
    character(len=*), intent(in) :: file_path
    character(len=*), intent(in) :: work_dir
    integer, intent(out)         :: ierr

    character(len=1024) :: cmd
    integer :: ios

    ierr = 0

    if (.not. proxy%running) then
      write(error_unit, '(A)') 'test_harness: proxy not running, cannot configure'
      ierr = -1
      return
    end if

    ! Use curl to POST the config JSON
    cmd = 'curl -s -X POST -H "Content-Type: application/json" ' // &
          '-d ''{"filePath":"' // trim(file_path) // '","workDir":"' // trim(work_dir) // '"}'' ' // &
          '"' // trim(proxy%proxy_url) // '/config"'

    call execute_command_line(trim(cmd), wait=.true., exitstat=ios)
    if (ios /= 0) then
      write(error_unit, '(A,I0)') 'test_harness: configure failed, exitstat=', ios
      ierr = ios
    end if
  end subroutine proxy_configure

  ! ==========================================================================
  !  Internal helpers
  ! ==========================================================================

  !> Portable millisecond sleep using a system call.
  subroutine sleep_ms(ms)
    integer, intent(in) :: ms
    integer :: ios
    character(len=32) :: secs_str
    real :: secs

    secs = real(ms) / 1000.0
    write(secs_str, '(F6.3)') secs

#ifdef _WIN32
    call execute_command_line('ping -n 1 -w ' // trim(adjustl(secs_str)) // &
                              ' 127.0.0.1 >nul 2>&1', wait=.true., exitstat=ios)
#else
    call execute_command_line('sleep ' // trim(adjustl(secs_str)), wait=.true., exitstat=ios)
#endif
  end subroutine sleep_ms

  !> Set an environment variable (platform-dependent).
  subroutine set_env(name, value)
    character(len=*), intent(in) :: name, value
    character(len=512) :: cmd
    integer :: ios

#ifdef _WIN32
    cmd = 'set ' // trim(name) // '=' // trim(value)
#else
    cmd = 'export ' // trim(name) // '=' // trim(value)
#endif
    call execute_command_line(trim(cmd), wait=.true., exitstat=ios)
  end subroutine set_env

end module test_harness
