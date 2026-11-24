# Copilot Supercharged SDK for Tcl

A Tcl SDK for interacting with the GitHub Copilot CLI server. Communicates via JSON-RPC 2.0 over stdio using Content-Length header framing.

## Requirements

- **Tcl 8.6+** (for `try/on/finally`, coroutines, and `lmap`)
- The `copilot` CLI installed and available on `PATH` (or specify the path via `cli_path`)

### Checking Requirements

```tcl
# Check Tcl version
puts [info patchlevel]
# Should be 8.6 or newer

# Check copilot CLI is available
exec copilot --version
```

## Installation

### Manual

Add the `lib/` directory to your `auto_path`:

```tcl
lappend auto_path /path/to/copilot-sdk-supercharged/tcl/lib
package require copilot::client
```

### Using the Makefile

```bash
cd tcl
make install   # Copies packages into your Tcl auto_path
make test      # Runs the tcltest suite
```

## Quick Start

```tcl
#!/usr/bin/env tclsh
lappend auto_path /path/to/copilot-sdk-supercharged/tcl/lib

package require copilot::client

# Create and start the client
set client [::copilot::client::new]
::copilot::client::start $client

# Create a session
set session [::copilot::client::create_session $client \
    system_prompt "You are a helpful assistant. Be concise."]

# Send a message and wait for the response
set response [::copilot::client::send_and_wait $session \
    [dict create message "What is 2+2?"]]
puts "Response: [dict get $response content]"

# Clean up
::copilot::client::stop $client
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The MCP secret store and plugin extension
approval procs are server-scoped client calls that require Copilot CLI **1.0.116 or newer**; older
CLIs answer with a JSON-RPC method-not-found error, raised like any other RPC error.

- **MCP secret store**: `::copilot::client::mcp_secrets_save $client secret_id value ?config_dir?` returns a dict whose `used_keychain` is 1 when the OS keychain stored the credential (0 for the file backend). `::copilot::client::mcp_secrets_delete $client secret_id ?config_dir?` and `::copilot::client::mcp_secrets_delete_server $client server_name ?config_dir?` remove credentials. The value is a credential: the SDK never logs it.
- **Plugin extension approvals**: `::copilot::client::extension_approvals_grant $client session_id request_id` persists a pending extension permission request before you answer it and returns a dict with `granted` and `approval`. `::copilot::client::extension_approvals_list $client ?session_id?` returns a dict whose `approvals` is a list of approval dicts (`extension_name`, `plugin_name`, `capabilities`, `granted_at`, `installation_key`, `installation_label`; see `::copilot::types::make_permissions_extension_approval`). `::copilot::client::extension_approvals_revoke $client extension_name installation_key ?session_id?` returns a dict with `revoked`.
- **Ask-user image attachments**: the new `on_user_input_request` session option answers `userInput.request` (the `ask_user` tool). The handler gets a request dict (`question`, `choices`, `allow_freeform`, `request_id`, `image_attachment_constraints`, and `meta`, the request's opaque `_meta`) and a context dict (`session_id`). `image_attachment_constraints` is a dict with `allowed_mime_types`, `max_bytes_per_image`, and `max_count`, or `""` when the runtime does not accept images. Return a `::copilot::types::make_user_input_response` dict (`answer`, `was_freeform`, `attachments`) whose images are built with `::copilot::types::make_user_input_image_attachment` (`data` in base64, `mime_type`, optional `description` and `display_name`; the wire `type` is always `"image"`). `attachments` is sent only when non-empty, and the runtime enforces the limits. A plain string is sent as a freeform answer.
- Elicitation `requestId` / `_meta` passthrough is not available in this SDK: the `elicitation_handler` option only advertises elicitation support, and elicitation requests are not dispatched to it.

```tcl
proc answer_question {request context} {
    set limits [dict get $request image_attachment_constraints]
    if {$limits ne "" && "image/png" in [dict get $limits allowed_mime_types]} {
        set fh [open screenshot.png rb]
        set png [read $fh]
        close $fh
        return [::copilot::types::make_user_input_response answer "" was_freeform 1 \
            attachments [list [::copilot::types::make_user_input_image_attachment \
                data [binary encode base64 $png] mime_type image/png display_name screenshot.png]]]
    }
    return [::copilot::types::make_user_input_response answer "No screenshot available" was_freeform 1]
}
set session [::copilot::client::create_session $client on_user_input_request answer_question]

set saved [::copilot::client::mcp_secrets_save $client my-server-token $::env(MY_SERVER_TOKEN)]
puts "Stored in the OS keychain: [dict get $saved used_keychain]"
::copilot::client::mcp_secrets_delete_server $client my-server

# request_id is the requestId of a pending extension permission request (from its
# permission.requested event): persist it, then answer the request as usual.
set sid [dict get [::copilot::session::session_info $session] session_id]
set grant [::copilot::client::extension_approvals_grant $client $sid $request_id]
foreach approval [dict get [::copilot::client::extension_approvals_list $client] approvals] {
    ::copilot::client::extension_approvals_revoke $client \
        [dict get $approval extension_name] [dict get $approval installation_key]
}
```

## What's New in v3.0.0

This release speaks **SDK protocol v4**, so it requires Copilot CLI **1.0.113 or newer**; older
CLIs are rejected by `::copilot::client::start` with a protocol version mismatch error. The
client now performs the real handshake (`connect` with your `client_info`, then `ping`) and sends
`session.send`, `session.detach`, and the other Copilot JSON-RPC methods with exactly-typed JSON.
Every existing option keeps working: `create_session` and `resume_session` still take the same
flat options, and the SDK translates them into the grouped v4 `session.create` /
`session.resume` payload (for example `streaming` becomes `events.streaming` and
`excluded_tools` becomes `tools.excludedTools`). A legacy single `provider` is sent as a named
`"default"` connection with Copilot API models disabled, which preserves the v1 BYOK behavior.
`resume_session` now accepts the same options as `create_session` and returns the resume result
plus `handle`, the local session handle. `send_and_wait` returns a dict with `messageId`,
`content` (the last assistant message), and `events`.

New in this release:

- **Named BYOK providers**: `providers`, `models`, `default_provider` (`null` clears a saved default on resume), and `capi`, with `make_named_provider_config` and `make_provider_model_config`.
- **Idempotent tools**: `::copilot::tools::define_tool name description handler parameters 1`.
- **Client turn ID**: the `client_turn_id` send option.
- **Client mode**: the `mode` client option (`coding-agent`, the default, or `empty`).
- **Shell output events**: `tool.shell_output` events reach `on_event` listeners; read them with `::copilot::types::make_shell_output_event_data`.
- **Shell authentication**: the `on_shell_authentication_request` session option.
- **Memory store**: the `store_memory` session option.
- **Network callbacks**: the `network` client option (`scope`, `resolve_proxy`, `authenticate_negotiate`, `authenticate_basic`).
- **Policy helper failure**: `::copilot::client::policy_helper_failure $error_options` after a failed create/resume, and `::copilot::types::make_policy_helper_failure`.
- **Permission mode refusal reason**: `::copilot::session::set_permission_mode` returns `success`, `mode`, and `reason` (for example `managed_policy`).
- **Permission prompts**: the `on_permission_request` session option answers protocol 4 `permission.requested` events. Without it the runtime denies operations that need approval, including custom tools, unless a tool is defined with `skip_permission` (`define_tool name description handler parameters idempotent 1`).

```tcl
# The handler gets a dict (kind, tool_call_id, request_id, details) and returns a decision:
# approve, approve-for-session, deny, user-not-available, or a dict such as
# {kind reject feedback "Not now"}.
proc ::review {request} {
    if {[dict get $request kind] in {read custom-tool}} { return approve }
    return [dict create kind reject feedback "Only reads are allowed"]
}
set s [::copilot::client::create_session $c model "claude-sonnet-4.5" \
    on_permission_request ::review]
# Or approve everything: on_permission_request ::copilot::session::approve_all
```

Also new for this SDK (upstream v2.6.0 features): `client_info` (sent on `connect`), Auto routing
tier (`auto_tier` option, `::copilot::session::set_auto_tier`), `ask_user_variant`,
`github_token_provider` (answers `gitHubToken.getToken`), sandbox bypass
(`make_sandbox_config allow_bypass 1`), `feature_flags`, session detach
(`::copilot::session::detach_session`, also used by `destroy_session`), message source (the
`source` send option with `::copilot::types::message_source` / `message_source_agent`),
`::copilot::client::clear_managed_settings_cache`, `::copilot::session::get_managed_settings`,
`::copilot::session::recheck_policy_helper`, `allowed_models`, `auth_client_id_metadata_url`,
max output tokens (`make_model_capabilities_limits max_output_tokens 4096`), structured output
(the `response_schema` send option), `agent_directories` / `hook_directories`,
`trust_working_directory` (create only), `allow_transcript_recovery` (resume only),
`make_image_generation_config` (`save_to_disk`), `make_autopilot_options` (`max_continues`),
sub-agent hooks (`hooks [dict create on_subagent_start ... on_subagent_stop ...]`), and the BYOK
API key command (`make_provider_config ... api_key_command "..."`).

Tcl values are untyped, so build nested option values with the `make_*` builders, or pass exact
JSON with `::copilot::jsonrpc::json_raw` (for example tool parameter schemas):

```tcl
set client [::copilot::client::new mode "coding-agent" \
    client_info [::copilot::types::make_client_info editor_name "my-app" editor_version "1.2.0"]]
::copilot::client::start $client

# Idempotent tool with an exact JSON Schema
::copilot::tools::define_tool lookup_order "Look up an order by ID" [list apply {{params} {
    return "Order [dict get $params id] shipped"
}}] [::copilot::jsonrpc::json_raw {{"type":"object","properties":{"id":{"type":"string"}}}}] 1

# Named BYOK provider and model; Copilot API models disabled
set session [::copilot::client::create_session $client \
    model "gateway/chat" \
    providers [list [::copilot::types::make_named_provider_config \
        name gateway base_url https://models.example.com/v1 api_key $::env(MODEL_API_KEY)]] \
    models [list [::copilot::types::make_provider_model_config \
        provider gateway id chat model_id gpt-4o wire_model deployment]] \
    capi [dict create enabled false] \
    sandbox_config [::copilot::types::make_sandbox_config enabled 1 allow_bypass 1] \
    autopilot [::copilot::types::make_autopilot_options continuation_enabled 1 max_continues 5]]

# Turn ID, message source, and structured output
set response [::copilot::client::send_and_wait $session [::copilot::types::make_send_options \
    message "What is 2+2?" client_turn_id "turn-42" \
    source [dict get [::copilot::types::message_source] USER] \
    response_schema [::copilot::jsonrpc::json_raw {{"type":"object","properties":{"answer":{"type":"integer"}}}}]]]
puts [dict get $response content]
```

Shell authentication, memory store, shell output, and network callbacks:

```tcl
proc answer_sudo {request context} {
    if {[dict get $request input_kind] eq "confirm"} {
        return [dict create action cancel]
    }
    return [dict create action accept value $::env(SUDO_PASSWORD)]
}
proc store_memory {memory context} {
    set fh [open memories.txt a]
    puts $fh "[dict get $memory subject]: [dict get $memory fact]"
    close $fh
}
proc resolve_proxy {request} { return "PROXY proxy.example:8080; DIRECT" }

set client [::copilot::client::new network [dict create scope connection resolve_proxy resolve_proxy]]
::copilot::client::start $client   ;# registers the callback with network.setProvider

# Empty mode would also require: available_tools {view grep}
set session [::copilot::client::create_session $client \
    on_shell_authentication_request answer_sudo store_memory store_memory]
::copilot::session::on_event $session [list apply {{event} {
    set chunk [::copilot::types::make_shell_output_event_data $event]
    if {$chunk ne ""} { puts -nonewline [dict get $chunk text] }
}}]
```

Policy helper failures, permission modes, and resume:

```tcl
if {[catch {::copilot::client::create_session $client} err opts]} {
    set failure [::copilot::client::policy_helper_failure $opts]
    if {$failure ne ""} { puts "Blocked by policy: [dict get $failure message]" }
}

set result [::copilot::session::set_permission_mode $session "allow-all"]
if {![dict get $result success] && [dict get $result reason] eq "managed_policy"} {
    puts "Managed policy keeps mode [dict get $result mode]"
}

set sid [dict get [::copilot::session::session_info $session] session_id]
::copilot::session::detach_session $session
set resumed [::copilot::client::resume_session $client $sid \
    allow_transcript_recovery 1 capi [dict create enabled true] default_provider null]
set resumed_session [dict get $resumed handle]
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not available: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: this SDK keeps spawning the CLI over stdio (or connecting to `cli_url`).
- Handlers run synchronously while the SDK waits for a response, so a `shellAuthentication.cancel` only arrives after the handler for that request has returned; it is acknowledged without further action.
- Tcl values are untyped: plain Tcl dicts are encoded with best-effort JSON typing. Use the `make_*` builders or `::copilot::jsonrpc::json_raw` for nested values whose JSON types matter.

## Architecture

The SDK is composed of six packages, loaded via the `pkgIndex.tcl` package index:

```
tcl/
  lib/copilot/
    pkgIndex.tcl    # Package index (auto-loaded by Tcl)
    version.tcl     # SDK version constants
    types.tcl       # Type constructors and validators (dicts)
    jsonrpc.tcl     # JSON-RPC 2.0 transport layer
    tools.tcl       # Tool definition and registry
    session.tcl     # Session lifecycle and message handling
    client.tcl      # Client lifecycle and process management
  examples/
    basic_example.tcl
  tests/
    client_test.tcl
  cookbook/
    README.md
    error-handling.md
    multiple-sessions.md
    persisting-sessions.md
    tools-and-skills.md
    advanced-features.md
    async-io-patterns.md
  Makefile
```

### Module Responsibilities

| Package | Purpose |
|---------|---------|
| `copilot::version` | SDK version, name, and minimum protocol version constants |
| `copilot::types` | Dict-based type constructors (`make_client_options`, `make_session_config`, etc.), result types, JSON schema helpers |
| `copilot::jsonrpc` | Pure-Tcl JSON encoder/decoder, Content-Length framing, request/notification sending, pending request tracking |
| `copilot::tools` | Tool registry with `define_tool`, `invoke_tool`, `list_tools`, and wire-format conversion |
| `copilot::session` | Session creation/destruction, `send_and_wait`, event listener registration, tool call dispatch |
| `copilot::client` | CLI process spawning (`open \|...`), TCP connection, initialize handshake, session factory, status queries |

## API Reference

### Client

```tcl
# Create a client (returns a handle string)
set client [::copilot::client::new ?cli_path path? ?cli_url url? ?log_level level? ?timeout ms?]

# Start the client (spawns CLI or connects to URL)
::copilot::client::start $client

# Stop the client (shuts down CLI, closes connections)
::copilot::client::stop $client

# Get connection state
::copilot::client::get_state $client
# Returns: disconnected | connecting | connected | error
```

### Sessions

```tcl
# Create a session
set session [::copilot::client::create_session $client \
    ?system_prompt text? ?github_token token? ?model name? \
    ?streaming bool? ?session_idle_timeout_seconds secs?]

# Send a message and wait for the complete response
set response [::copilot::client::send_and_wait $session [dict create message "Hello"]]

# Register an event listener (returns an unsubscribe command)
set unsub [::copilot::session::on_event $session myCallback]

# Destroy a session
::copilot::client::destroy_session $client $session
```

### Tools

```tcl
# Define a tool (globally registered, available to all sessions)
::copilot::client::define_tool "tool_name" "Tool description" handlerProc ?parametersDict?

# List registered tools
::copilot::tools::list_tools

# Remove a tool
::copilot::tools::remove_tool "tool_name"
```

### Types

```tcl
# Create typed dicts with validation
set opts [::copilot::types::make_client_options cli_path "/usr/bin/copilot"]
set cfg  [::copilot::types::make_session_config system_prompt "Hello"]
set tool [::copilot::types::make_tool "name" "description" handler]

# Result types
set ok  [::copilot::types::make_ok "value"]
set err [::copilot::types::make_error "message"]
if {[::copilot::types::is_ok $result]} {
    set val [::copilot::types::unwrap $result]
}
```

## Recent Features (v2.4–v2.5)

Recent upstream syncs added a range of session options, passed as key/value pairs to `::copilot::client::create_session`. BYOK transport options are passed to `::copilot::client::new`. Constant and config helpers live in `::copilot::types`.

### New in v2.5

- **Reasoning effort** — `reasoning_effort` — set the model's reasoning budget (e.g. `"high"`).
- **Tool search** — `tool_search` — discover tools on demand.
- **Content exclusion** — `content_exclusion` — honor content-exclusion rules.
- **Session rewind** — `rewind_enabled` — allow rolling back to an earlier snapshot.
- **Additional directories** — `additional_directories` — extra workspace directories.
- **Disabled MCP servers** — `disabled_mcp_servers`.
- **GitHub MCP tool config** — `github_mcp_tool_config`.
- **Canvas provider** — `canvas_provider`.
- **Custom agents local-only** — `custom_agents_local_only`.
- **Built-in plugin directories** — `builtin_plugin_directories`.
- **Agent-factory authoring** — `args_schema`.
- **Permission decision context** — `decision_context`.
- **In-process transport** — `in_process`.
- **Experimental mode** — `experimental_mode`.
- **MCP OAuth handler** — `on_mcp_auth_request`.

### New in v2.4

- **Session citations** — `enable_citations`.
- **Excluded built-in agents** — `excluded_builtin_agents`.
- **Spending limits** — `session_limits` via `::copilot::types::make_session_limits_config max_ai_credits ...`.
- **Session memory** — `memory` via `::copilot::types::make_memory_configuration enabled ...`.
- **OTLP protocol** — `otlp_protocol`.
- **WebSocket responses** — `enable_web_socket_responses`.
- **Experiment assignments** — `exp_assignments`.
- **Hooks** — `on_post_tool_use`, `on_user_prompt_transformed`, `on_pre_mcp_tool_call`.
- **BYOK request handler / bearer token** — `request_handler`, `bearer_token_provider` on `client::new`.
- **Tool defer loading** — `::copilot::types::tool_defer`.
- **System-message sections** — `::copilot::types::system_message_section` (`PREAMBLE` / `PRESERVE`).
- **GitHub attachments** — `::copilot::types::github_attachment` (`GITHUB_COMMIT`, `GITHUB_REPOSITORY`).
- **Per-message overrides** — `agent_mode`, `display_prompt` on `send` / `send_and_wait`.

### Examples

```tcl
# Reasoning effort + content exclusion + session rewind
set session [::copilot::client::create_session $client \
    reasoning_effort "high" \
    content_exclusion 1 \
    rewind_enabled 1]
```

```tcl
# Grant the agent access to extra workspace directories
set session [::copilot::client::create_session $client \
    additional_directories {../shared-lib ../docs}]
```

```tcl
# Cap AI-credit spend and enable persistent memory
set limits [::copilot::types::make_session_limits_config max_ai_credits 500]
set mem    [::copilot::types::make_memory_configuration enabled 1]
set session [::copilot::client::create_session $client \
    session_limits $limits \
    memory $mem]
```

```tcl
# BYOK: supply bearer tokens dynamically
proc my_token_provider {args} {
    return $::env(MY_COPILOT_TOKEN)
}
set client [::copilot::client::new bearer_token_provider my_token_provider]
```

## Tcl Idioms Used

- **Namespaces** for module organization (`::copilot::client`, `::copilot::session`, etc.)
- **Dicts** for structured data (options, configs, events, JSON objects)
- **upvar/uplevel** for callback dispatch and variable binding
- **catch/try** for error handling (Tcl 8.6 `try/on/finally`)
- **Coroutines** (Tcl 8.6+) for cooperative multitasking in async patterns
- **fileevent** for non-blocking I/O integration with Tk or custom event loops
- **open |command** for spawning the CLI subprocess with bidirectional pipes
- **apply** for anonymous procs (lambdas) in tool handlers and callbacks

## Testing

Run the test suite:

```bash
cd tcl
tclsh tests/client_test.tcl
# Or:
make test
```

The tests use the built-in `tcltest` framework and cover:
- Package loading
- Version constants
- Type constructors and validators
- JSON encoding/decoding round-trips
- Tool registry operations
- Pending request lifecycle
- Notification handler dispatch
- Client handle creation

## Cookbook

See the [cookbook/](cookbook/README.md) for practical recipes:

- **Error Handling**: catch/try patterns, retry logic, cleanup wrappers
- **Multiple Sessions**: session registries, per-user sessions, round-robin pools
- **Persisting Sessions**: save/load to JSON, auto-save, metadata envelopes
- **Tools and Skills**: defining tools, file system tools, sub-agent orchestration
- **Advanced Features**: per-session auth, idle timeouts, streaming, model selection
- **Async I/O Patterns**: fileevent, coroutines, timer-based polling

## License

See the [LICENSE](../LICENSE) file in the repository root.
