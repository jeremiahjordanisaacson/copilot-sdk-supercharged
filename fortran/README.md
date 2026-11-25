# Copilot Supercharged SDK for Fortran

Fortran SDK for the GitHub Copilot CLI. Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio pipes using Content-Length header framing.

## Requirements

- Fortran 2008 compatible compiler (GFortran 8+, Intel Fortran 2019+, NAG 7+)
- CMake 3.16+ or [Fortran Package Manager (fpm)](https://fpm.fortran-lang.org/)
- [json-fortran](https://github.com/jacobwilliams/json-fortran) (fetched automatically by fpm)
- [GitHub Copilot CLI](https://github.com/github/copilot) installed and available on PATH (or specify path via `cli_path`)

## Installation

### With fpm (recommended)

The SDK is not published to the fpm registry, so add it as a path dependency on a clone of this repository. The dependency name must match the package name in `fortran/fpm.toml` (`copilot-sdk-supercharged`). In your `fpm.toml`:

```toml
[dependencies]
copilot-sdk-supercharged = { path = "../copilot-sdk-supercharged/fortran" }
```

To build and test the SDK itself from a clone:

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

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0.

### MCP secret store and plugin extension approvals

Six server-scoped client methods. They require **Copilot CLI 1.0.116 or newer**; an older runtime
answers with a JSON-RPC method-not-found error, reported like any other through
`iostat = COPILOT_IOSTAT_RPC_ERROR` and `client%last_error`.

```fortran
type(mcp_secrets_save_result) :: saved
type(permissions_extension_approvals_grant_result) :: grant
type(permissions_extension_approvals_list_result) :: listed
type(permissions_extension_approvals_revoke_result) :: revoked
integer :: i

! MCP secret store. The value is a credential: it is sent to the runtime and never logged.
call client%mcp_secrets_save('my-server/api-key', api_key, saved, ios, config_dir='/path/to/config')
if (ios == 0 .and. .not. saved%used_keychain) print *, 'stored by the file backend'
call client%mcp_secrets_delete('my-server/api-key', ios)          ! mcp.secrets.delete
call client%mcp_secrets_delete_server('my-server', ios)           ! mcp.secrets.deleteServer

! Plugin extension approvals (user scoped). Grant persists a pending extension permission
! request before you answer it.
call client%extension_approvals_grant(session%session_id, pending_request_id, grant, ios)
if (ios == 0 .and. allocated(grant%approval)) print *, 'approved ', grant%approval%extension_name
call client%extension_approvals_list(listed, ios)                 ! optional session_id=
if (ios == 0) then
  do i = 1, size(listed%approvals)
    call client%extension_approvals_revoke(listed%approvals(i)%extension_name, &
                                           listed%approvals(i)%installation_key, revoked, ios)
  end do
end if
```

| Type-bound procedure (module wrapper) | Wire method | Result |
|---|---|---|
| `mcp_secrets_save(secret_id, value, outcome, iostat[, config_dir])` (`copilot_mcp_secrets_save`) | `mcp.secrets.save` | `mcp_secrets_save_result`: `used_keychain` (`.false.` means the file backend stored it) |
| `mcp_secrets_delete(secret_id, iostat[, config_dir])` (`copilot_mcp_secrets_delete`) | `mcp.secrets.delete` | none |
| `mcp_secrets_delete_server(server_name, iostat[, config_dir])` (`copilot_mcp_secrets_delete_server`) | `mcp.secrets.deleteServer` | none |
| `extension_approvals_grant(session_id, request_id, outcome, iostat)` (`copilot_extension_approvals_grant`) | `permissions.extensionApprovals.grant` | `permissions_extension_approvals_grant_result`: `granted`, `approval` (allocated when granted) |
| `extension_approvals_list(outcome, iostat[, session_id])` (`copilot_extension_approvals_list`) | `permissions.extensionApprovals.list` | `permissions_extension_approvals_list_result`: `approvals(:)` |
| `extension_approvals_revoke(extension_name, installation_key, outcome, iostat[, session_id])` (`copilot_extension_approvals_revoke`) | `permissions.extensionApprovals.revoke` | `permissions_extension_approvals_revoke_result`: `revoked` |

A `permissions_extension_approval` carries `extension_name` (stable identity, for example
`plugin:plugin-name:extension-name`), `plugin_name`, `capabilities(:)` (the exact privileged
capabilities the user approved, blank padded), `granted_at` (RFC 3339), `installation_key` (opaque;
pass it back unchanged to revoke), and `installation_label` (unallocated when absent).

### Ask-user image attachments

`config%on_user_input_request` answers the agent's ask_user questions (`userInput.request`; setting
it sends `interaction.requestUserInput`). When the runtime accepts images, the `user_input_request`
carries an allocated `image_attachment_constraints` (`allowed_mime_types(:)`,
`max_bytes_per_image`, `max_count`); it is unallocated otherwise. The request also carries
`request_id` and `meta_json` (the opaque `_meta` object) when the runtime sends them. Answer with
images through the optional `attachments(:)` of `user_input_response`:

```fortran
function ask_user(request) result(response)
  type(user_input_request), intent(in) :: request
  type(user_input_response) :: response
  response%was_freeform = .true.
  if (allocated(request%image_attachment_constraints)) then
    response%answer = ''                             ! image-only answer
    allocate(response%attachments(1))
    response%attachments(1)%data = screenshot_base64()
    response%attachments(1)%mime_type = 'image/png'
    response%attachments(1)%display_name = 'screenshot.png'
  else
    response%answer = 'yes'
  end if
end function ask_user

config%on_user_input_request => ask_user
```

`attachments` is sent only when allocated (each image as `{"type": "image", "data", "mimeType"}` plus
optional `description` and `displayName`) and is only valid when the request advertised
`image_attachment_constraints`; the runtime enforces the MIME type, size, and count limits.

## What's New in v3.0.0

- **SDK protocol v4.** The client now requires the server to report protocol version 4, so it
  needs Copilot CLI 1.0.113 or newer. Older CLIs are rejected at start with
  `SDK protocol version mismatch: SDK supports version 4, but server reports version N. ...`
  (`client%last_error`, `iostat = COPILOT_IOSTAT_PROTOCOL_MISMATCH`).
- **Existing options keep working.** Keep setting the same `session_config` fields. Right before
  `session.create` and `session.resume`, `group_session_params` (module `copilot_session_wire`)
  converts the flat payload into the grouped v4 shape (for example `streaming` becomes
  `events.streaming` and `reasoning_effort` becomes `inference.reasoningEffort`). Only members the
  runtime accepts are sent; create-only and resume-only members are honored. The legacy single
  `config%provider` is translated to a named `"default"` provider.
- String values in the session payload are now JSON-escaped, and `session.send` sends the
  message as `prompt`.

The examples below use these callbacks (all procedures are module procedures):

```fortran
module my_callbacks
  use copilot_types
  implicit none
contains
  function github_token(args) result(token)
    type(github_token_provider_args), intent(in) :: args
    type(github_token_result) :: token
    if (args%host == 'github.com') token%access_token = 'gho_example'
    token%expires_in = 28800                         ! seconds, must exceed one hour
  end function github_token

  function shell_auth(request) result(response)
    type(shell_authentication_request), intent(in) :: request
    type(shell_authentication_result) :: response
    response%accepted = request%input_kind == SHELL_AUTH_INPUT_CONFIRM
  end function shell_auth

  subroutine store_memory(memory)
    type(stored_memory), intent(in) :: memory
    print *, 'remember: ', memory%subject, ' = ', memory%fact
  end subroutine store_memory

  function on_subagent_start(input) result(output)
    type(subagent_start_input), intent(in) :: input
    type(subagent_start_output) :: output
    output%additional_context = 'You are helping ' // input%agent_name
  end function on_subagent_start

  function resolve_proxy(url) result(proxy)
    character(len=*), intent(in) :: url
    character(len=:), allocatable :: proxy
    proxy = ''                                       ! '' connects directly
    if (index(url, 'internal.example') > 0) proxy = 'http://proxy.example:8080'
  end function resolve_proxy

  function lookup(args, invocation) result(res)
    use copilot_tools
    character(len=*), intent(in) :: args
    type(tool_invocation), intent(in) :: invocation
    type(tool_result) :: res
    res = tool_success('record for ' // args // ' in ' // invocation%session_id)
  end function lookup
end module my_callbacks
```

### Client options: client info, client mode, network callbacks

```fortran
opts%client_info%editor_name    = 'my-editor'       ! sent as clientInfo on connect
opts%client_info%editor_version = '1.2.0'
opts%mode = CLIENT_MODE_CODING_AGENT                ! default ('copilot-cli' is an alias)
opts%network%scope = NETWORK_SCOPE_CONNECTION
opts%network%resolve_proxy => resolve_proxy        ! registered with network.setProvider
call copilot_client_create(client, opts)
if (client%state%value == COPILOT_STATE_ERROR) print *, client%last_error
```

`CLIENT_MODE_EMPTY` starts sessions without the coding-agent defaults: every create and resume
must set `config%available_tools`, and the SDK sends safe defaults (no telemetry, in-memory
storage, memory and repository hooks off) under your own values:

```fortran
opts%mode = CLIENT_MODE_EMPTY
config%available_tools = [character(len=4) :: 'view', 'grep']
```

### Session options

```fortran
config%model = 'gpt-5'
config%auto_tier = AUTO_TIER_BALANCE                ! Auto routing tier (capi.autoTier)
config%ask_user_variant = ASK_USER_VARIANT_ELICITATION
config%github_token_provider => github_token        ! answers gitHubToken.getToken
allocate(config%sandbox)
config%sandbox%allow_bypass = .true.                ! permissions.sandbox.allowBypass
config%feature_flags_json = '{"MY_FLAG":true}'      ! experiments.flags
config%allowed_models = [character(len=6) :: 'gpt-5', 'claude']
config%auth_client_id_metadata_url = 'https://example.com/oauth/client.json'
config%model_limits%max_output_tokens = 4096        ! modelCapabilities.limits.max_output_tokens
config%agent_directories = [character(len=8) :: './agents']
config%hook_directories = [character(len=7) :: './hooks']
config%trust_working_directory = WORKING_DIRECTORY_TRUST_SESSION   ! create only
allocate(config%image_generation)
config%image_generation%enabled = .true.
config%image_generation%save_to_disk = .false.
allocate(config%autopilot)
config%autopilot%max_continues = 3
config%on_subagent_start => on_subagent_start      ! hooks.invoke subagentStart
config%on_shell_authentication_request => shell_auth
config%on_store_memory => store_memory
call client%create_session(config, session, ios)
if (ios == COPILOT_IOSTAT_RPC_ERROR) then
  print *, client%last_error
  if (client%has_policy_helper_failure) print *, client%last_policy_helper_failure%message
end if
```

`github_token` and `github_token_provider` are mutually exclusive. The provider is registered
under a generated id and released when the session is detached or destroyed.

### Named BYOK providers

```fortran
allocate(config%providers(1))
config%providers(1)%name = 'azure'
config%providers(1)%provider_type = 'azure'
config%providers(1)%base_url = 'https://my-resource.openai.azure.com'
config%providers(1)%api_key_command = 'az account get-access-token --query accessToken -o tsv'
allocate(config%models(1))
config%models(1)%provider = 'azure'
config%models(1)%id = 'gpt-5'
config%models(1)%max_output_tokens = 8192
config%default_provider = 'azure'
config%capi_enabled = .false.                       ! Copilot API models off
config%model = 'azure/gpt-5'
```

The legacy single provider keeps working (it becomes the `"default"` provider):

```fortran
allocate(config%provider)
config%provider%provider_type = 'openai'
config%provider%base_url = 'https://api.openai.com/v1'
config%provider%api_key_command = 'pass show openai'
```

### Tools and messages

```fortran
config%tools = [define_tool('lookup', 'Look up a record', '{"type":"object"}', lookup, &
                            idempotent=.true.)]

sopts%message = 'Summarize the build log'
sopts%source = message_source_agent('reviewer')     ! or MESSAGE_SOURCE_USER / MESSAGE_SOURCE_SYSTEM
sopts%client_turn_id = 'turn-42'
sopts%response_schema_json = '{"type":"object","properties":{"summary":{"type":"string"}}}'
call session%send_and_wait(sopts, result)
```

### Session and client methods

```fortran
call session%switch_auto_tier(AUTO_TIER_FAST, ios)            ! session.model.switchAutoTier
call session%set_permission_mode(PERMISSION_MODE_ASSISTED, mode_result, ios)
if (.not. mode_result%success .and. allocated(mode_result%reason)) then
  if (mode_result%reason == PERMISSION_MODE_REFUSAL_MANAGED_POLICY) print *, 'blocked by policy'
end if
call session%recheck_policy_helper(snapshot, ios, failure, has_failure)
call client%clear_managed_settings_cache(ios)                 ! managedSettings.clearCache
call session%detach(ios)                                      ! session.detach

resume_config%suppress_resume_event = .true.
resume_config%allow_transcript_recovery = .true.
resume_config%clear_default_provider = .true.
call client%resume_session(saved_session_id, session, ios, resume_config)
```

`session%disconnect` now detaches (`session.detach`), so the session can be resumed later.
`abort`, `delete_session`, and `set_foreground` report a JSON-RPC error through `iostat`.

### Shell output events

```fortran
subroutine on_event(event, user_data)
  type(session_event), intent(in) :: event
  class(*), intent(inout), optional :: user_data
  type(tool_shell_output_data) :: output
  logical :: found
  call parse_tool_shell_output(event%data_json, output, found)
  if (found) write(*, '(A)', advance='no') output%text
end subroutine on_event

handler_id = session%on(EVENT_TOOL_SHELL_OUTPUT, on_event)
```

### Server-to-client requests

While `send_and_wait` waits, the session answers `gitHubToken.getToken`, `hooks.invoke`
(`subagentStart`, `subagentStop`), `shellAuthentication.request`, `shellAuthentication.cancel`,
`memoryStore.store`, and `network.resolveProxy` / `network.authenticateNegotiate` /
`network.authenticateBasic` with the callbacks above. Hosts that read messages themselves pass
each request to the client, which routes it to the owning session:

```fortran
call copilot_handle_server_request(client, message, handled, ios)
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not
  implemented: it is an experimental upstream surface that depends on the runtime's in-process
  hosting library.
- In-process native runtime hosting is not the default connection: the SDK keeps using the
  Copilot CLI (`cli_path`, or an external server through `cli_url`).
- Fortran transport: the client does not spawn the CLI process yet (it uses stdin/stdout units 5
  and 6 as placeholders) and reads each response as the next message, without request id
  correlation. Server-to-client requests are answered inside `send_and_wait`, or when the host
  passes them to `copilot_handle_server_request`.
- Callbacks run synchronously, so `shellAuthentication.cancel` can only decline a request that has
  not been read yet.
- Callbacks returning `character(len=:), allocatable` text (network callbacks) must always
  assign a value; return `''` for "no answer".

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
| `connect` | Handshake carrying `clientInfo` (falls back to `ping` on older servers) |
| `ping` | Verify connectivity and protocol version (must be 4) |
| `status.get` | Get CLI version info |
| `auth.getStatus` | Get authentication status |
| `models.list` | List available models |
| `session.create` | Create a new session (grouped v4 payload) |
| `session.resume` | Resume an existing session (grouped v4 payload) |
| `session.send` | Send a message to a session |
| `session.getMessages` | Get session history |
| `session.detach` | Detach a session (disconnect) |
| `session.destroy` | Destroy a session |
| `session.abort` | Abort current processing |
| `session.delete` | Delete a session permanently |
| `session.list` | List all sessions |
| `session.setForeground` | Set foreground session (TUI mode) |
| `session.model.switchAutoTier` | Request an Auto routing tier |
| `session.permissions.setMode` | Set the permission mode |
| `session.managedSettings.recheckPolicyHelper` | Re-run the managed-settings Policy Helper |
| `managedSettings.clearCache` | Clear the managed settings cache |
| `network.setProvider` | Register host network callbacks |
| `mcp.secrets.save` | Save an MCP secret (`mcp_secrets_save`, CLI 1.0.116+) |
| `mcp.secrets.delete` | Delete an MCP secret (`mcp_secrets_delete`, CLI 1.0.116+) |
| `mcp.secrets.deleteServer` | Delete an MCP server's secrets (`mcp_secrets_delete_server`, CLI 1.0.116+) |
| `permissions.extensionApprovals.grant` | Persist a pending plugin extension approval (`extension_approvals_grant`, CLI 1.0.116+) |
| `permissions.extensionApprovals.list` | List plugin extension approvals (`extension_approvals_list`, CLI 1.0.116+) |
| `permissions.extensionApprovals.revoke` | Revoke a plugin extension approval (`extension_approvals_revoke`, CLI 1.0.116+) |

**Server to Client (notifications):**

| Method | Description |
|---|---|
| `session.event` | Session events (assistant.message, session.idle, tool.shell_output) |
| `session.lifecycle` | Session lifecycle events |

**Server to Client (requests):**

| Method | Description |
|---|---|
| `tool.call` | Execute a registered tool |
| `permission.request` | Request permission for an operation |
| `gitHubToken.getToken` | Ask the session's GitHub token provider for a token |
| `hooks.invoke` | Run a hook handler (`subagentStart`, `subagentStop`) |
| `shellAuthentication.request` / `.cancel` | Answer or cancel a shell authentication prompt |
| `memoryStore.store` | Store a fact with the memory store callback |
| `userInput.request` | Answer an ask_user question with `on_user_input_request` (optional image attachments) |
| `network.resolveProxy` / `.authenticateNegotiate` / `.authenticateBasic` | Network callbacks |

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
| `resume_session(id, session, iostat[, config])` | Resume an existing session |
| `get_status(json, iostat)` | Get CLI status |
| `get_auth_status(json, iostat)` | Get auth status |
| `list_models(json, iostat)` | List available models |
| `list_sessions(json, iostat)` | List sessions |
| `set_foreground(id, iostat)` | Set foreground session |
| `clear_managed_settings_cache(iostat)` | Clear the managed settings cache |
| `mcp_secrets_save(id, value, outcome, iostat[, config_dir])` | Save an MCP secret (`mcp.secrets.save`) |
| `mcp_secrets_delete(id, iostat[, config_dir])` | Delete an MCP secret (`mcp.secrets.delete`) |
| `mcp_secrets_delete_server(name, iostat[, config_dir])` | Delete an MCP server's secrets (`mcp.secrets.deleteServer`) |
| `extension_approvals_grant(session_id, request_id, outcome, iostat)` | Persist a pending extension approval |
| `extension_approvals_list(outcome, iostat[, session_id])` | List plugin extension approvals |
| `extension_approvals_revoke(name, key, outcome, iostat[, session_id])` | Revoke a plugin extension approval |
| `handle_server_request(message, handled, iostat)` | Answer a server-to-client request |

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
| `disconnect(iostat)` | Disconnect (detach) the session |
| `detach(iostat)` | Detach the session (`session.detach`) |
| `destroy(iostat)` | Destroy the session on the server |
| `switch_auto_tier(tier, iostat)` | Request an Auto routing tier |
| `set_permission_mode(mode, outcome, iostat)` | Set the permission mode |
| `recheck_policy_helper(snapshot_json, iostat[, failure, has_failure])` | Re-run the Policy Helper |
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

## Recent Features (v2.4–v2.5)

Fields added in the v2.4 and v2.5 upstream syncs, set on `session_config` before
`create_session` (a few live on `copilot_client_options` or `send_options`).
JSON-valued fields (suffix `_json`) take a JSON string.

### v2.5 session options

- **Reasoning effort** — `config%reasoning_effort` (`'low'`/`'medium'`/`'high'`).
- **Tool search** — `config%tool_search_json`.
- **Session rewind** — `config%rewind_enabled`.
- **Content exclusion** — `config%content_exclusion`.
- **Additional directories** — `config%additional_directories`.
- **Disabled MCP servers** — `config%disabled_mcp_servers`.
- **GitHub MCP tool config** — `config%github_mcp_tool_config_json`.
- **Canvas provider** — `config%canvas_provider_json`.
- **Custom agents local-only** — `config%custom_agents_local_only`.
- **Permission decision context** — `config%decision_context_json`.
- **User-prompt-transformed hook** — `config%on_user_prompt_transformed`.
- **Built-in plugin directories** — `config%builtin_plugin_directories`.
- **Agent-factory args schema** — `config%args_schema_json`.
- **In-process FFI transport** — `config%in_process`.
- **Experimental mode** — `config%experimental_mode`.

### v2.4 session options

- **BYOK bearer token provider** — `opts%bearer_token_provider` (`copilot_client_options`).
- **HTTP request handler** — `opts%request_handler` (`copilot_client_options`).
- **MCP OAuth token handler** — `config%on_mcp_auth_request`.
- **Session citations** — `config%enable_citations`.
- **Excluded built-in agents** — `config%excluded_builtin_agents`.
- **Session spending limits** — `config%session_limits`.
- **Session memory** — `config%memory`.
- **OTLP protocol** — `config%otlp_protocol`.
- **WebSocket transport** — `config%enable_web_socket_responses`.
- **Experiment assignments** — `config%exp_assignments_json`.
- **Post/pre-tool hooks** — `config%on_post_tool_use`, `config%on_pre_mcp_tool_call`.
- **Per-message overrides** — `sopts%agent_mode`, `sopts%display_prompt`.
- **Constants** — `SECTION_PREAMBLE`/`SECTION_PRESERVE`, `GITHUB_COMMIT`/`GITHUB_REPOSITORY`.

### Reasoning effort and tool search

```fortran
type(session_config) :: config
config%reasoning_effort  = 'high'
config%tool_search_json  = '{"enabled":true}'
call client%create_session(config, session, ios)
```

### Session rewind and content exclusion

```fortran
config%rewind_enabled    = .true.
config%content_exclusion = .true.
call client%create_session(config, session, ios)
```

### Additional workspace directories

```fortran
config%additional_directories = [ character(len=16) :: &
  '/data/shared', '/data/reference' ]
call client%create_session(config, session, ios)
```

## Module Summary

| Module | File | Purpose |
|---|---|---|
| `copilot_client_module` | `src/copilot_client.f90` | Main client, process management |
| `copilot_jsonrpc` | `src/copilot_jsonrpc.f90` | JSON-RPC 2.0 transport |
| `copilot_session_module` | `src/copilot_session.f90` | Session management |
| `copilot_session_wire` | `src/copilot_session_wire.f90` | Flat to grouped v4 session payload translation |
| `copilot_json` | `src/copilot_json.f90` | JSON escaping and object member helpers |
| `copilot_types` | `src/copilot_types.f90` | All derived types |
| `copilot_tools` | `src/copilot_tools.f90` | Tool definition helpers |
| `copilot_version` | `src/copilot_version.f90` | Protocol version constant (4) |

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
