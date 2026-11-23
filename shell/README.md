# Copilot Supercharged CLI SDK for Shell/Bash

A Bash library for communicating with the GitHub Copilot CLI server via JSON-RPC 2.0 over stdio. This SDK follows the same patterns as all other SDKs in this repository (see the [full list of 40 supported languages](../README.md#available-sdks)).

## Requirements

- **bash 4.0+** (for `coproc`, associative arrays, and other modern features)
- **jq** (for JSON parsing and construction)
- **copilot CLI** in your `PATH` (or provide the path explicitly)

### Checking Requirements

```bash
# Check bash version
bash --version

# Check jq is installed
jq --version

# Check copilot CLI is available
copilot --version
```

## Quick Start

```bash
#!/usr/bin/env bash
set -euo pipefail

# Source the SDK
source /path/to/copilot-sdk-supercharged/shell/lib/copilot_sdk.sh

# Start the client (spawns copilot CLI process)
copilot_client_start

# Create a session
copilot_client_create_session

# Send a message and wait for the response
copilot_session_send_and_wait "What is 2+2?"
echo "Response: $COPILOT_SESSION_LAST_RESPONSE"

# Clean up
copilot_session_destroy
copilot_client_stop
```

## What's New in v3.0.0

This release speaks **SDK protocol v4**, so it requires Copilot CLI **1.0.113 or newer**; older
CLIs are rejected by `copilot_client_start` with a protocol version mismatch error. Every
existing `COPILOT_*` variable keeps working: the SDK still builds the same flat options and then
translates them into the grouped v4 `session.create` / `session.resume` payload (for example
`"streaming"` becomes `events.streaming`, `COPILOT_EXCLUDED_TOOLS` becomes `tools.excludedTools`,
and the sub-agent hooks flag becomes `hooks.hasHandlers`). Fork-specific options that the runtime
does not group stay at the payload root. A legacy `COPILOT_PROVIDER` is sent as a named
`"default"` connection with Copilot API models disabled, which preserves the v1 BYOK behavior.

New in this release:

- **Named BYOK providers**: `COPILOT_PROVIDERS`, `COPILOT_MODELS`, `COPILOT_DEFAULT_PROVIDER` (the literal `null` clears a saved default on resume), and `COPILOT_CAPI`, with the `copilot_named_provider_config` and `copilot_provider_model_config` builders.
- **Custom tools and idempotent tools**: `copilot_register_tool <name> <description> <handler> [schema] [idempotent] [skip_permission]` (calls are answered during `copilot_session_send_and_wait`).
- **Permission prompts**: `COPILOT_PERMISSION_HANDLER` answers protocol 4 `permission.requested` events (`copilot_approve_all` and `copilot_deny_all` are ready-made handlers). Without a handler the runtime denies operations that need approval, including custom tools registered without `skip_permission`.

```bash
review_permission() {
    # $1 is the permission request JSON ({"kind":"write"|"shell"|"custom-tool"|..., ...})
    case "$(echo "$1" | jq -r '.kind')" in
        read|custom-tool) echo "approve" ;;
        *) echo '{"kind":"reject","feedback":"Only reads are allowed"}' ;;
    esac
}
COPILOT_PERMISSION_HANDLER="review_permission"
```
- **Client turn ID**: `COPILOT_CLIENT_TURN_ID` on `copilot_session_send*`.
- **Client mode**: `COPILOT_CLIENT_MODE` (`coding-agent`, the default, or `empty`).
- **Shell output events**: `COPILOT_EVENT_TOOL_SHELL_OUTPUT`, the `COPILOT_ON_SHELL_OUTPUT` handler, and `copilot_shell_output_event_data`.
- **Shell authentication**: `COPILOT_SHELL_AUTHENTICATION_HANDLER`.
- **Memory store**: `COPILOT_MEMORY_STORE_HANDLER`.
- **Network callbacks**: `COPILOT_NETWORK_SCOPE`, `COPILOT_NETWORK_RESOLVE_PROXY`, `COPILOT_NETWORK_AUTHENTICATE_NEGOTIATE`, and `COPILOT_NETWORK_AUTHENTICATE_BASIC`.
- **Policy helper failure**: `COPILOT_POLICY_HELPER_FAILURE` after a failed create/resume, plus `copilot_policy_helper_failure <json>`.
- **Permission mode refusal reason**: `copilot_session_set_permission_mode <mode>` sets `COPILOT_PERMISSION_MODE_RESULT`, whose `reason` can be `$COPILOT_PERMISSION_MODE_REFUSAL_MANAGED_POLICY`.

Also available (added in v2.6.0): client info (`COPILOT_CLIENT_INFO_APPLICATION_NAME` and
friends, sent on the `connect` handshake), Auto routing tier (`COPILOT_AUTO_TIER`,
`copilot_session_set_auto_tier`), ask-user variant (`COPILOT_ASK_USER_VARIANT`), GitHub token
provider (`COPILOT_GITHUB_TOKEN_PROVIDER`), sandbox bypass (`copilot_sandbox_config true true`),
feature flags (`COPILOT_FEATURE_FLAGS`), session detach (`copilot_session_destroy` sends
`session.detach`), message source (`COPILOT_MESSAGE_SOURCE`), managed settings
(`copilot_client_clear_managed_settings_cache`, `copilot_session_get_managed_settings`,
`copilot_session_recheck_policy_helper`), model allowlist (`COPILOT_ALLOWED_MODELS`), OAuth client
metadata URL (`COPILOT_AUTH_CLIENT_ID_METADATA_URL`), max output tokens
(`copilot_model_capabilities_limits "" "" 4096`), structured output (`COPILOT_RESPONSE_SCHEMA`,
`copilot_session_send_and_wait_structured`), agent and hook directories
(`COPILOT_AGENT_DIRECTORIES`, `COPILOT_HOOK_DIRECTORIES`), working directory trust
(`COPILOT_TRUST_WORKING_DIRECTORY`, create only), transcript recovery
(`COPILOT_ALLOW_TRANSCRIPT_RECOVERY`, resume only), image generation config
(`copilot_image_generation_config`), autopilot options (`copilot_autopilot_options`), sub-agent
hooks (`COPILOT_ON_SUBAGENT_START`, `COPILOT_ON_SUBAGENT_STOP`), and the BYOK API key command
(`PROVIDER_API_KEY_COMMAND` with `copilot_provider_config`).

### Named BYOK providers

```bash
COPILOT_PROVIDERS="[$(copilot_named_provider_config gateway https://models.example.com/v1 openai "$MODEL_API_KEY")]"
COPILOT_MODELS="[$(copilot_provider_model_config gateway chat gpt-4o deployment)]"
COPILOT_CAPI='{"enabled":false}'
copilot_client_create_session "gateway/chat"

# Later: re-enable Copilot API models and clear the saved default connection on resume
COPILOT_PROVIDERS="" COPILOT_MODELS=""
COPILOT_CAPI='{"enabled":true}'
COPILOT_DEFAULT_PROVIDER="null"
copilot_client_resume_session "$COPILOT_SESSION_ID" "gpt-5"
```

### Custom tools, turn IDs, and client mode

```bash
lookup_order() {
    local args="$1"
    echo "Order $(echo "$args" | jq -r '.id') shipped"
}
copilot_register_tool "lookup_order" "Look up an order by ID" lookup_order \
    '{"type":"object","properties":{"id":{"type":"string"}}}' true

# Empty mode: no ambient tools; every create/resume must set COPILOT_AVAILABLE_TOOLS
COPILOT_CLIENT_MODE="empty"
copilot_client_start
COPILOT_AVAILABLE_TOOLS='["lookup_order"]'
copilot_client_create_session

COPILOT_CLIENT_TURN_ID="turn-42"
copilot_session_send_and_wait "Where is order 42?"
```

### Shell output, shell authentication, and memory store

```bash
on_shell_output() { echo "$1" | jq -r '.text'; }
answer_sudo() {
    local request="$1"
    if [[ "$(echo "$request" | jq -r '.inputKind')" == "confirm" ]]; then
        echo '{"action":"cancel"}'
    else
        jq -c -n --arg v "$SUDO_PASSWORD" '{"action":"accept","value":$v}'
    fi
}
store_memory() { echo "$1" | jq -c '{subject, fact}' >> memories.jsonl; }

COPILOT_ON_SHELL_OUTPUT="on_shell_output"
COPILOT_SHELL_AUTHENTICATION_HANDLER="answer_sudo"
COPILOT_MEMORY_STORE_HANDLER="store_memory"
copilot_client_create_session
```

### Network callbacks

```bash
resolve_proxy() { echo "PROXY proxy.example:8080; DIRECT"; }
proxy_basic_auth() { jq -c -n --arg p "$PROXY_PASSWORD" '{"username":"svc-build","password":$p}'; }

COPILOT_NETWORK_SCOPE="connection"
COPILOT_NETWORK_RESOLVE_PROXY="resolve_proxy"
COPILOT_NETWORK_AUTHENTICATE_BASIC="proxy_basic_auth"
copilot_client_start   # registers the callbacks with network.setProvider
```

### Policy helper failures and permission modes

```bash
if ! copilot_client_create_session; then
    if [[ -n "$COPILOT_POLICY_HELPER_FAILURE" ]]; then
        echo "Blocked by policy: $(echo "$COPILOT_POLICY_HELPER_FAILURE" | jq -r '.message')" >&2
    fi
fi

if ! copilot_session_set_permission_mode "allow-all"; then
    reason=$(echo "$COPILOT_PERMISSION_MODE_RESULT" | jq -r '.reason // empty')
    if [[ "$reason" == "$COPILOT_PERMISSION_MODE_REFUSAL_MANAGED_POLICY" ]]; then
        echo "Managed policy keeps the current permission mode" >&2
    fi
fi
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not available: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: this SDK keeps spawning the CLI over stdio.
- Handlers run synchronously, so a `shellAuthentication.cancel` only arrives after the handler for that request has returned; it is acknowledged without further action.
- Custom tool calls, shell output events, and other session events are processed while `copilot_session_send_and_wait` is polling; `copilot_session_send` alone does not process them.

## Architecture

The SDK is composed of four modules, each sourced by the main `copilot_sdk.sh` entry point:

```
shell/
  lib/
    copilot_sdk.sh           # Main entry point (sources all modules)
    sdk_protocol_version.sh  # Protocol version constant
    json_rpc.sh              # JSON-RPC 2.0 transport layer
    client.sh                # Client lifecycle and session management
    session.sh               # Session message sending and events
  examples/
    basic_example.sh         # Full working example
```

### Module Overview

| Module | Purpose |
|--------|---------|
| `sdk_protocol_version.sh` | Defines `COPILOT_SDK_PROTOCOL_VERSION=4` matching the server expectation |
| `json_rpc.sh` | Low-level JSON-RPC 2.0 over stdio with Content-Length framing using `coproc` |
| `client.sh` | Client lifecycle: start/stop, ping, create/resume/delete sessions, list models |
| `session.sh` | Session operations: send, send_and_wait, get_messages, abort, destroy |

## API Reference

### Client Functions

#### `copilot_client_start [cli_path] [log_level]`

Start the Copilot CLI client. Spawns the CLI server process and verifies the protocol version.

- `cli_path` - Path to the copilot CLI binary (default: `"copilot"`)
- `log_level` - Log level (default: `"info"`)

```bash
copilot_client_start                        # Use default copilot from PATH
copilot_client_start "/usr/local/bin/copilot" "debug"  # Custom path and log level
```

#### `copilot_client_stop`

Stop the CLI server and clean up all resources.

```bash
copilot_client_stop
```

#### `copilot_client_ping [message]`

Send a ping to verify server connectivity. Sets `COPILOT_JSONRPC_LAST_RESPONSE`.

```bash
copilot_client_ping "health check"
protocol_version=$(copilot_jsonrpc_get_result_field '.protocolVersion')
```

#### `copilot_client_create_session [model] [system_message]`

Create a new conversation session. Sets `COPILOT_SESSION_ID`.

```bash
copilot_client_create_session                    # Default model
copilot_client_create_session "gpt-4"            # Specific model
copilot_client_create_session "" "You are helpful"  # With system message
```

#### `copilot_client_resume_session <session_id> [model]`

Resume an existing session. Sets `COPILOT_SESSION_ID`.

```bash
copilot_client_resume_session "abc-123"
```

#### `copilot_client_delete_session <session_id>`

Delete a session permanently.

```bash
copilot_client_delete_session "abc-123"
```

#### `copilot_client_list_models`

List available models. Results in `COPILOT_JSONRPC_LAST_RESPONSE`.

```bash
copilot_client_list_models
copilot_jsonrpc_get_result_field '.models' | jq -r '.[].id'
```

#### `copilot_client_list_sessions`

List all sessions. Results in `COPILOT_JSONRPC_LAST_RESPONSE`.

```bash
copilot_client_list_sessions
copilot_jsonrpc_get_result_field '.sessions' | jq -r '.[].sessionId'
```

### Session Functions

All session functions default to using `COPILOT_SESSION_ID` but accept an optional session ID parameter.

#### `copilot_session_send <prompt> [session_id]`

Send a message (fire-and-forget). Returns immediately.

```bash
copilot_session_send "Hello, world!"
message_id=$(copilot_jsonrpc_get_result_field '.messageId')
```

#### `copilot_session_send_and_wait <prompt> [session_id] [timeout] [poll_interval]`

Send a message and block until the session becomes idle. Sets `COPILOT_SESSION_LAST_RESPONSE` to the assistant's reply.

```bash
copilot_session_send_and_wait "What is 2+2?"
echo "$COPILOT_SESSION_LAST_RESPONSE"

# With custom timeout (120 seconds)
copilot_session_send_and_wait "Write a long story" "" 120
```

#### `copilot_session_get_messages [session_id]`

Retrieve all events from session history. Sets `COPILOT_SESSION_MESSAGES` (JSON array).

```bash
copilot_session_get_messages
echo "$COPILOT_SESSION_MESSAGES" | jq '.[] | select(.type == "assistant.message") | .data.content'
```

#### `copilot_session_abort [session_id]`

Abort the currently processing message.

```bash
copilot_session_abort
```

#### `copilot_session_destroy [session_id]`

Destroy the session and release resources.

```bash
copilot_session_destroy
```

### JSON-RPC Functions (Low-Level)

These are used internally but available for advanced use cases.

#### `copilot_jsonrpc_start <cli_path> [extra_args...]`

Start the CLI process as a coproc.

#### `copilot_jsonrpc_stop`

Stop the CLI process and clean up.

#### `copilot_jsonrpc_request <method> [params_json] [timeout]`

Send a JSON-RPC request and wait for the response.

#### `copilot_jsonrpc_send_message <json_body>`

Send a raw Content-Length framed message.

#### `copilot_jsonrpc_read_message`

Read a Content-Length framed message. Sets `COPILOT_JSONRPC_LAST_RESPONSE`.

#### `copilot_jsonrpc_get_result`

Extract the `.result` from the last response.

#### `copilot_jsonrpc_get_result_field <jq_path>`

Extract a specific field from `.result`.

#### `copilot_jsonrpc_is_connected`

Check if the CLI process is still running.

## Global Variables

| Variable | Description |
|----------|-------------|
| `COPILOT_SDK_PROTOCOL_VERSION` | Protocol version (currently 4) |
| `COPILOT_SDK_NAME` | SDK name (`copilot-sdk-shell`) |
| `COPILOT_SDK_VERSION` | SDK version |
| `COPILOT_CLIENT_STATE` | Connection state: `disconnected`, `connecting`, `connected`, `error` |
| `COPILOT_SESSION_ID` | Active session ID (set by create/resume) |
| `COPILOT_SESSION_LAST_RESPONSE` | Last assistant response text from `send_and_wait` |
| `COPILOT_SESSION_MESSAGES` | JSON array from `get_messages` |
| `COPILOT_JSONRPC_LAST_RESPONSE` | Raw JSON of last JSON-RPC response |
| `COPILOT_JSONRPC_LAST_ERROR` | Raw JSON of last JSON-RPC error |
| `COPILOT_JSONRPC_PID` | PID of the CLI coproc process |

## Error Handling

All functions return 0 on success and 1 on failure, with error messages written to stderr. Use standard bash error handling patterns:

```bash
# Check return codes
if ! copilot_client_start; then
    echo "Failed to start client" >&2
    exit 1
fi

# Or use set -e (enabled by copilot_sdk.sh)
set -e
copilot_client_start
copilot_client_create_session
```

## Protocol Details

The SDK communicates with the Copilot CLI using JSON-RPC 2.0 over stdio with Content-Length header framing, identical to the LSP (Language Server Protocol) transport:

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","id":1,"method":"ping","params":{}}
```

The CLI process is launched with:
```
copilot --headless --no-auto-update --log-level info --stdio
```

## Examples

See `examples/basic_example.sh` for a complete working example that demonstrates all major SDK features.

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```bash
copilot_init --session-idle-timeout 300
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```bash
copilot_init --session-fs-cwd "/repo" --session-fs-state-path "/tmp/state"
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```bash
meta=$(copilot_get_session_metadata "$SESSION_ID")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```bash
copilot_create_session --skill-directories "./skills" --disabled-skills "test-skill"
```

- `--skill-directories` - directories to scan for skill definitions
- `--disabled-skills` - skills to exclude from the session
- `--include-sub-agent-streaming-events` - receive streaming events from sub-agents

## Image Generation

Request image responses using response format constants:

```bash
copilot_session_send_and_wait \
    "Generate a sunset over mountains" \
    "" 60 1 \
    "$COPILOT_RESPONSE_FORMAT_IMAGE" \
    '{"size":"1024x1024","quality":"hd","style":"natural"}'
```

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk`. Configure these by setting the
`COPILOT_*` variables before `copilot_client_create_session` (or, for per-message
options, before `copilot_session_send*`). Array/object values are JSON strings.

### v2.5

- **Reasoning effort** — `COPILOT_REASONING_EFFORT` (`minimal`|`low`|`medium`|`high`|`max`).
- **Session rewind** — `COPILOT_REWIND_ENABLED` (`"true"`/`"false"`).
- **Additional directories** — `COPILOT_ADDITIONAL_DIRECTORIES` (JSON array).
- **Content exclusion** — `COPILOT_CONTENT_EXCLUSION`.
- **Tool search** — `COPILOT_TOOL_SEARCH` (JSON object or boolean).
- **Disabled MCP servers** — `COPILOT_DISABLED_MCP_SERVERS` (JSON array).
- **GitHub MCP tool config** — `COPILOT_GITHUB_MCP_TOOL_CONFIG` (JSON object).
- **Canvas provider** — `COPILOT_CANVAS_PROVIDER`.
- **Custom agents local-only** — `COPILOT_CUSTOM_AGENTS_LOCAL_ONLY`.
- **Experimental mode / in-process FFI** — `COPILOT_EXPERIMENTAL_MODE`, `COPILOT_IN_PROCESS`.
- **User-prompt-transformed hook** — `COPILOT_ON_USER_PROMPT_TRANSFORMED` (handler function name).
- **Permission decision context** — `COPILOT_DECISION_CONTEXT` (JSON object).
- **Agent-factory args schema** — `COPILOT_ARGS_SCHEMA` (JSON object).
- **Built-in plugin directories** — `COPILOT_BUILTIN_PLUGIN_DIRECTORIES` (JSON array).

### v2.4

- **BYOK bearer token provider** — `COPILOT_BEARER_TOKEN_PROVIDER` (handler function name).
- **MCP OAuth token handler** — `COPILOT_ON_MCP_AUTH_REQUEST` (handler function name).
- **Session citations** — `COPILOT_ENABLE_CITATIONS`.
- **Excluded built-in agents** — `COPILOT_EXCLUDED_BUILTIN_AGENTS` (JSON array).
- **Spending limits** — `COPILOT_SESSION_LIMITS` (JSON object) or `COPILOT_MAX_AI_CREDITS`.
- **Session memory** — `COPILOT_MEMORY_ENABLED`.
- **OTLP protocol / WebSocket transport** — `COPILOT_OTLP_PROTOCOL`, `COPILOT_ENABLE_WEB_SOCKET_RESPONSES`.
- **Post-tool-use hook** — `COPILOT_ON_POST_TOOL_USE` (handler function name).
- **Message agent mode / display prompt** — `COPILOT_AGENT_MODE`, `COPILOT_DISPLAY_PROMPT`.
- **Section / attachment constants** — `COPILOT_SECTION_PREAMBLE`, `COPILOT_OVERRIDE_PRESERVE`, `COPILOT_GITHUB_COMMIT`, `COPILOT_GITHUB_REPOSITORY`.
- **Experiment assignments** — `COPILOT_EXP_ASSIGNMENTS` (JSON object).

```bash
# Reasoning effort + session rewind + extra workspace dirs
COPILOT_REASONING_EFFORT="high"
COPILOT_REWIND_ENABLED="true"
COPILOT_ADDITIONAL_DIRECTORIES='["/repo/pkg-a", "/repo/pkg-b"]'
copilot_client_create_session
```

```bash
# Honor content exclusion and enable on-demand tool search
COPILOT_CONTENT_EXCLUSION="true"
COPILOT_TOOL_SEARCH='{"mode":"auto"}'
copilot_client_create_session
```

```bash
# Cap AI-credit spend and emit citations
COPILOT_ENABLE_CITATIONS="true"
COPILOT_MAX_AI_CREDITS=5
copilot_client_create_session
```

```bash
# Per-message agent mode and display prompt
COPILOT_AGENT_MODE="plan"
COPILOT_DISPLAY_PROMPT="Refactor (planning)"
copilot_session_send_and_wait "Refactor this module"
```

## License

Copyright (c) Microsoft Corporation. All rights reserved.
