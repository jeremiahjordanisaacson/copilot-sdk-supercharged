# Copilot Supercharged SDK for Fortran

Fortran SDK for the GitHub Copilot CLI. Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio pipes using Content-Length header framing.

## Requirements

- Fortran 2008 compatible compiler (GFortran 8+, Intel Fortran 2019+, NAG 7+)
- CMake 3.16+ or [Fortran Package Manager (fpm)](https://fpm.fortran-lang.org/)
- [json-fortran](https://github.com/jacobwilliams/json-fortran) (fetched automatically by fpm)
- [GitHub Copilot CLI](https://github.com/github/copilot) installed and available on PATH (or specify path via `cli_path`)

## Installation

### With fpm (recommended)

Add the dependency to your `fpm.toml`:

```toml
[dependencies]
copilot-sdk = { path = "fortran" }
```

Build and run tests:

```bash
cd fortran
fpm build
fpm test
```

### With CMake

```bash
cd fortran
cmake -B build
cmake --build build
ctest --test-dir build
```

To disable examples or tests:

```bash
cmake -B build -DCOPILOT_SDK_BUILD_EXAMPLES=OFF -DCOPILOT_SDK_BUILD_TESTS=OFF
```

## Quick Start

```fortran
program hello_copilot
  use copilot_client_module
  use copilot_types
  use copilot_session_module
  implicit none

  type(copilot_client)        :: client
  type(copilot_client_options) :: opts
  type(copilot_session_type)  :: session
  type(send_options)          :: sopts
  type(send_and_wait_result)  :: result
  integer                     :: ios

  ! Create and start the client
  call copilot_client_create(client, opts)

  ! Create a session
  call copilot_create_session(client, session=session, iostat=ios)

  ! Send a message
  sopts%message = 'What is 2 + 2?'
  call session%send_and_wait(sopts, result)
  print *, 'Response: ', result%message

  ! Clean up
  call session%destroy(ios)
  call copilot_stop(client)
end program
```

## Architecture

The SDK mirrors the architecture of all other SDKs in this repository:

```
copilot_client_module
  |-- Spawns CLI process (copilot --headless --no-auto-update --stdio)
  |-- copilot_jsonrpc (Content-Length framed JSON-RPC 2.0 over pipes)
  |     |-- read_message / send_request / send_notification
  |     |-- Message builders and parsers
  |-- copilot_session_module
  |     |-- Event handlers (wildcard and typed)
  |     |-- send / send_and_wait / abort / destroy
  |-- copilot_types (derived types for all data structures)
  |-- copilot_tools (tool definition helpers)
  |-- copilot_version (protocol version constant)
```

### Transport Protocol

Messages are framed using the LSP base protocol:

```
Content-Length: <byte-count>\r\n
\r\n
<JSON-RPC message>
```

### JSON-RPC Methods

**Client to Server (requests):**

| Method | Description |
|---|---|
| `ping` | Verify connectivity and protocol version |
| `status.get` | Get CLI version info |
| `auth.getStatus` | Get authentication status |
| `models.list` | List available models |
| `session.create` | Create a new session |
| `session.resume` | Resume an existing session |
| `session.send` | Send a message to a session |
| `session.getMessages` | Get session history |
| `session.destroy` | Destroy a session |
| `session.abort` | Abort current processing |
| `session.delete` | Delete a session permanently |
| `session.list` | List all sessions |
| `session.setForeground` | Set foreground session (TUI mode) |

**Server to Client (notifications):**

| Method | Description |
|---|---|
| `session.event` | Session events (assistant.message, session.idle) |
| `session.lifecycle` | Session lifecycle events |

**Server to Client (requests):**

| Method | Description |
|---|---|
| `tool.call` | Execute a registered tool |
| `permission.request` | Request permission for an operation |

## API Reference

### copilot_client_options

```fortran
type(copilot_client_options) :: opts
opts%cli_path   = '/usr/local/bin/copilot'  ! Override CLI binary path
opts%cli_url    = 'http://localhost:3000'    ! Connect to external server
opts%log_level  = 'debug'                   ! Logging verbosity
opts%cwd        = '/my/project'             ! Working directory
opts%auto_start = .true.                    ! Auto-start CLI on create
```

### copilot_client

The main entry point. Manages the CLI server connection and session lifecycle.

```fortran
type(copilot_client) :: client

! Create with options
call copilot_client_create(client, opts)

! Or create with defaults
call copilot_client_create(client)

! Manual start/stop
call copilot_start(client)
call copilot_stop(client)
```

**Type-bound procedures:**

| Procedure | Description |
|---|---|
| `start()` | Start the CLI process |
| `stop()` | Stop the CLI process |
| `ping(iostat)` | Verify connectivity |
| `create_session(config, session, iostat)` | Create a new session |
| `resume_session(id, session, iostat)` | Resume an existing session |
| `get_status(json, iostat)` | Get CLI status |
| `get_auth_status(json, iostat)` | Get auth status |
| `list_models(json, iostat)` | List available models |
| `list_sessions(json, iostat)` | List sessions |
| `set_foreground(id, iostat)` | Set foreground session |

### copilot_session_type

Represents a conversation session.

```fortran
type(copilot_session_type) :: session
type(session_config)       :: config
type(send_options)         :: sopts
type(send_and_wait_result) :: result
integer                    :: ios

! Configure
config%model = 'gpt-4'
config%system_prompt = 'You are a Fortran expert.'
config%streaming = .true.

! Create
call client%create_session(config, session, ios)

! Send and wait
sopts%message = 'Explain array slicing in Fortran'
call session%send_and_wait(sopts, result)
print *, result%message

! Clean up
call session%destroy(ios)
```

**Type-bound procedures:**

| Procedure | Description |
|---|---|
| `send(opts, iostat)` | Send a message (fire-and-forget) |
| `send_and_wait(opts, result, timeout_ms)` | Send and block until idle |
| `abort(iostat)` | Abort current processing |
| `disconnect(iostat)` | Disconnect the session |
| `destroy(iostat)` | Destroy the session on the server |
| `on(event_type, callback)` | Subscribe to events (returns ID) |
| `off(handler_id)` | Unsubscribe from events |

### Tools

Define custom tools that the assistant can invoke:

```fortran
use copilot_tools

type(copilot_tool) :: my_tool

my_tool = define_tool( &
  'get_temperature',                           &
  'Returns the current temperature in Celsius', &
  '{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}', &
  get_temp_callback &
)

! Include the tool in session config
config%tools = [my_tool]
```

Implement the callback:

```fortran
function get_temp_callback(args, invocation) result(res)
  character(len=*), intent(in) :: args
  type(tool_invocation), intent(in) :: invocation
  type(tool_result) :: res

  ! Parse args, look up temperature...
  res = tool_success('22.5')
end function
```

Helper functions:

| Function | Description |
|---|---|
| `tool_success(content)` | Create a successful tool result |
| `tool_failure(error_text)` | Create a failed tool result |

### Permissions

Handle permission requests from the assistant:

```fortran
type(permission_request) :: req
type(permission_result)  :: pres

! In your permission handler callback:
if (req%kind == 'read') then
  pres%granted = .true.
else
  pres%granted = .false.
  pres%reason  = 'Write operations are not allowed'
end if
```

### Session Filesystem (SessionFs)

Configure a virtual filesystem for the session:

```fortran
type(session_fs_config) :: fs
type(session_fs_entry)  :: entries(2)

entries(1)%path    = '/data/input.txt'
entries(1)%content = 'Some input data'

entries(2)%path         = '/data/output'
entries(2)%is_directory = .true.

fs%cwd     = '/data'
fs%entries = entries
config%fs  = fs
```

### Events

The SDK emits standard session events:

| Event | Description |
|---|---|
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Final reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning chunk |
| `session.idle` | Session finished processing |
| `session.error` | An error occurred |
| `session.compaction_start` | Context compaction started |
| `session.compaction_complete` | Context compaction finished |

## Module Summary

| Module | File | Purpose |
|---|---|---|
| `copilot_client_module` | `src/copilot_client.f90` | Main client, process management |
| `copilot_jsonrpc` | `src/copilot_jsonrpc.f90` | JSON-RPC 2.0 transport |
| `copilot_session_module` | `src/copilot_session.f90` | Session management |
| `copilot_types` | `src/copilot_types.f90` | All derived types |
| `copilot_tools` | `src/copilot_tools.f90` | Tool definition helpers |
| `copilot_version` | `src/copilot_version.f90` | Protocol version constant |

## Error Handling

All I/O operations use Fortran's `iostat` mechanism. A non-zero `iostat` indicates an error:

```fortran
integer :: ios

call client%ping(ios)
if (ios /= 0) then
  print *, 'Connection failed'
  stop 1
end if
```

The `send_and_wait_result` type has an `is_error` flag:

```fortran
type(send_and_wait_result) :: result

call session%send_and_wait(sopts, result)
if (result%is_error) then
  print *, 'Error: ', result%error_message
end if
```

## Building from Source

```bash
cd fortran

# fpm
fpm build
fpm test
fpm run --example basic_example

# CMake
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
ctest --test-dir build
```

## Cookbook

See the [cookbook/](cookbook/) directory for practical recipes:

- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Tools and Skills](cookbook/tools-and-skills.md)
- [Advanced v2.0 Features](cookbook/advanced-features.md)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Ensure all tests pass (`fpm test`)
4. Submit a pull request

## License

MIT
