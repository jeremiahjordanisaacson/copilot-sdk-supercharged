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
puts "Response: $response"

# Clean up
::copilot::client::stop $client
```

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
