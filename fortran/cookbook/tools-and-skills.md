# Tools and Skills in Fortran

Patterns for defining custom tools and registering them with sessions.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools) so it can interact with your Fortran code, data, or system.

## Defining a Simple Tool

```fortran
program tool_example
  use copilot_client_module
  use copilot_types
  use copilot_tools
  use copilot_session_module
  implicit none

  type(copilot_client)       :: client
  type(session_config)       :: config
  type(copilot_session_type) :: session
  type(copilot_tool)         :: time_tool
  type(send_options)         :: sopts
  type(send_and_wait_result) :: result
  integer                    :: ios

  ! Define a tool
  time_tool = define_tool( &
    'get_time',                                       &
    'Returns the current system time',                &
    '{"type":"object","properties":{}}',              &
    get_time_callback                                 &
  )

  ! Attach to session config
  allocate(config%tools(1))
  config%tools(1) = time_tool

  call copilot_client_create(client)
  call client%create_session(config, session, ios)

  sopts%message = 'What time is it?'
  call session%send_and_wait(sopts, result)
  print *, result%message

  call session%destroy(ios)
  call copilot_stop(client)

contains

  function get_time_callback(args, invocation) result(res)
    character(len=*), intent(in) :: args
    type(tool_invocation), intent(in) :: invocation
    type(tool_result) :: res
    character(len=8) :: time_str

    call date_and_time(time=time_str)
    res = tool_success(time_str(1:2) // ':' // time_str(3:4) // ':' // time_str(5:6))
  end function

end program
```

## Defining a Tool with Parameters

```fortran
type(copilot_tool) :: calc_tool

calc_tool = define_tool( &
  'add_numbers',                                               &
  'Add two numbers and return the sum',                        &
  '{"type":"object","properties":{'                          // &
  '"a":{"type":"number","description":"First number"},'       // &
  '"b":{"type":"number","description":"Second number"}'       // &
  '},"required":["a","b"]}',                                   &
  add_callback                                                 &
)
```

## Tool Result Helpers

```fortran
! Success
res = tool_success('The answer is 42')

! Failure
res = tool_failure('File not found: config.dat')
```

## Multiple Tools

```fortran
allocate(config%tools(3))
config%tools(1) = time_tool
config%tools(2) = calc_tool
config%tools(3) = file_tool

call client%create_session(config, session, ios)
```

## Best Practices

- Keep tool descriptions clear and concise so the assistant knows when to use them.
- Validate tool arguments before processing.
- Return meaningful error messages from `tool_failure` so the assistant can retry.
- Avoid long-running operations inside tool callbacks; the CLI waits for a response.
