# Copilot Supercharged SDK for V (Vlang)

A V client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

The module is not published to VPM, so install it from a clone of this repository. The `copilot` module's sources are in `vlang/src`; link that directory into your V modules folder as `copilot`:

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
mkdir -p ~/.vmodules
ln -s "$PWD/copilot-sdk-supercharged/vlang/src" ~/.vmodules/copilot
```

On Windows, create a directory junction instead:

```bat
mkdir "%USERPROFILE%\.vmodules"
mklink /J "%USERPROFILE%\.vmodules\copilot" "C:\path\to\copilot-sdk-supercharged\vlang\src"
```

Then import it in your V project:

```v
import copilot
```

## Quick Start

```v
import copilot

fn main() {
    mut client := copilot.new_client(copilot.CopilotClientOptions{
        cli_url: 'localhost:3000'
    })
    client.start() or { panic(err) }
    defer { client.stop() or {} }

    mut session := client.create_session(copilot.SessionConfig{
        model: 'gpt-4'
        on_permission_request: copilot.approve_all
    }) or { panic(err) }
    defer { session.disconnect() or {} }

    // Register a streaming event handler
    session.on(fn (ev copilot.SessionEvent) {
        if ev.event_type == 'assistant.message_delta' {
            print(ev.raw_data)
        }
    })

    // Send a message and wait for the full response
    response := session.send_and_wait(copilot.SendOptions{
        prompt: 'What is 2 + 2?'
    }, 30_000) or { panic(err) }

    println('Response: ${response}')
}
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, returned as `RpcError` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret value is never logged. Pass `''` as `config_dir` to use the runtime's configuration directory.

```v
saved := client.mcp_secrets_save('my-server-token', os.getenv('MY_SERVER_TOKEN'), '')!
println(if saved.used_keychain { 'Stored in the OS keychain' } else { 'Stored in the file backend' })

client.mcp_secrets_delete('my-server-token', '')!
client.mcp_secrets_delete_server('my-server', '')! // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions. `extension_approvals_list` and `extension_approvals_revoke` take a `session_id` (`''` for none) to use an attached session's configuration profile.

```v
// Persist a pending extension permission request before answering it normally
grant := client.extension_approvals_grant(session.id(), request_id)!
if approval := grant.approval {
    println('Approved ${approval.extension_name}')
}

listed := client.extension_approvals_list('')!
for approval in listed.approvals {
    // extension_name, plugin_name, capabilities, granted_at, installation_key, installation_label
    client.extension_approvals_revoke(approval.extension_name, approval.installation_key, '')!
}
```

### Ask-user image attachments

Set `on_user_input_request` on the session (it sends `requestUserInput`) to answer the agent's ask_user questions with a `UserInputResponse`. When the runtime accepts images in the answer, the `UserInputRequest` carries `image_attachment_constraints` (`allowed_mime_types`, `max_bytes_per_image`, `max_count`); it is `none` otherwise. The request also exposes `request_id` and `meta` (the raw JSON of the opaque `_meta` object) when sent. Answer with `attachments`, an array of `UserInputImageAttachment` (base64 `data`, `mime_type`, optional `description` and `display_name`; the wire `type` is always `"image"`). Image-only answers use an empty `answer` with `was_freeform: true`; the runtime enforces the limits.

```v
import encoding.base64

mut asking := client.create_session(copilot.SessionConfig{
    on_user_input_request: fn (req copilot.UserInputRequest) copilot.UserInputResponse {
        if constraints := req.image_attachment_constraints {
            if 'image/png' in constraints.allowed_mime_types {
                png := os.read_bytes('screenshot.png') or { []u8{} }
                return copilot.UserInputResponse{
                    was_freeform: true
                    attachments: [
                        copilot.UserInputImageAttachment{
                            data: base64.encode(png)
                            mime_type: 'image/png'
                            display_name: 'screenshot.png'
                        },
                    ]
                }
            }
        }
        return copilot.UserInputResponse{
            answer: 'No screenshot available'
            was_freeform: true
        }
    }
})!
```

## What's New in v3.0.0

**SDK protocol 4.** The SDK now speaks SDK protocol 4 (`copilot.protocol_version == 4`), which requires Copilot CLI 1.0.113 or newer. Older CLIs are rejected by `start()` with an `SDK protocol version mismatch` error. Existing options keep working: you fill in the same `SessionConfig` fields as before, and the SDK translates the flat payload into the grouped protocol 4 shape (`group_session_params`) right before `session.create` and `session.resume`. A legacy single `provider` is translated into a named `"default"` provider automatically. The transport now runs one background reader that answers server requests (hooks, tokens, shell authentication, memory store, network callbacks) and routes `session.event` notifications to sessions.

New client options:

```v
mut client := copilot.new_client(copilot.CopilotClientOptions{
    mode: .coding_agent // default; .empty requires available_tools on every session
    client_info: copilot.ClientInfo{
        editor_name: 'my-editor'
        editor_version: '1.2.0'
    }
    network: copilot.NetworkOptions{
        scope: 'connection'
        resolve_proxy: fn (url string) ?string {
            return 'PROXY proxy.example:8080; DIRECT'
        }
    }
})
client.start()!
client.clear_managed_settings_cache()! // managedSettings.clearCache
```

In `.empty` mode every session must set `available_tools`, and conservative defaults (no telemetry, in-memory OAuth and embedding storage, no repository hooks, no host git operations) are applied beneath your own values:

```v
mut empty_client := copilot.new_client(copilot.CopilotClientOptions{ mode: .empty })
empty_client.start()!
mut minimal := empty_client.create_session(copilot.SessionConfig{
    available_tools: ['view', 'grep']
})!
```

New session options:

```v
mut session := client.create_session(copilot.SessionConfig{
    model: 'gateway/chat'
    ask_user_variant: copilot.ask_user_variant_elicitation
    feature_flags: {
        'my_flag': true
    }
    allowed_models: ['gpt-5', 'claude-sonnet-4.5']
    auth_client_id_metadata_url: 'https://example.com/oauth/client.json'
    agent_directories: ['./agents']
    hook_directories: ['./hooks']
    trust_working_directory: copilot.trust_working_directory_session
    sandbox: copilot.SandboxConfig{
        enabled: true
        allow_bypass: true
    }
    model_capabilities_override: copilot.ModelCapabilitiesOverride{
        max_output_tokens: 4096
    }
    image_generation: copilot.ImageGenerationConfig{
        enabled: true
        save_to_disk: false
    }
    autopilot: copilot.AutopilotConfig{
        continuation_enabled: true
        max_continues: 3
    }
})!
```

Named BYOK providers (select a registered model with `'provider/id'`), with `api_key_command` for fresh keys:

```v
mut byok := client.create_session(copilot.SessionConfig{
    model: 'gateway/chat'
    providers: [
        copilot.NamedProviderConfig{
            name: 'gateway'
            provider_type: 'openai'
            base_url: 'https://models.example.com/v1'
            api_key_command: 'get-model-key'
        },
    ]
    models: [
        copilot.ProviderModelConfig{
            provider: 'gateway'
            id: 'chat'
            wire_model: 'gpt-5'
        },
    ]
    default_provider: 'gateway'
    capi: copilot.CapiConfig{
        enabled: false
    }
})!

// The legacy single provider still works and gains api_key_command:
mut legacy := client.create_session(copilot.SessionConfig{
    model: 'my-deployment'
    provider: copilot.ProviderConfig{
        provider_type: 'openai'
        base_url: 'https://models.example.com/v1'
        api_key_command: 'get-model-key'
    }
})!
```

GitHub token provider (mutually exclusive with `github_token`; unregistered when the session is disconnected or detached), hooks (including `hook_subagent_start` / `hook_subagent_stop`), shell authentication, and memory store callbacks:

```v
mut hooks := map[string]copilot.HookHandler{}
hooks[copilot.hook_subagent_start] = fn (input string) string {
    println('subagent started: ${input}')
    return ''
}
hooks[copilot.hook_subagent_stop] = fn (input string) string {
    return ''
}
mut callbacks := client.create_session(copilot.SessionConfig{
    github_token_provider: fn (req copilot.GitHubTokenRequest) copilot.GitHubTokenResult {
        return copilot.GitHubTokenResult{
            access_token: os.getenv('GH_TOKEN')
            expires_in: 7200
        }
    }
    hooks: hooks
    on_shell_authentication_request: fn (req copilot.ShellAuthenticationRequest) copilot.ShellAuthenticationResponse {
        if req.program == 'sudo' {
            return copilot.ShellAuthenticationResponse{
                accept: true
                value: os.getenv('SUDO_PASSWORD')
            }
        }
        return copilot.ShellAuthenticationResponse{}
    }
    on_store_memory: fn (memory copilot.StoredMemory) {
        println('remember: ${memory.subject} = ${memory.fact}')
    }
})!
```

Idempotent tools and message options (source, client turn id, structured output):

```v
lookup := copilot.Tool{
    name: 'lookup'
    description: 'Look up a record'
    idempotent: true
    handler: fn (args map[string]string) !string {
        return '{"ok":true}'
    }
}
mut tooled := client.create_session(copilot.SessionConfig{ tools: [lookup] })!
tooled.send(copilot.SendOptions{
    prompt: 'List three colors'
    source: copilot.message_source_system // or copilot.agent_message_source('reviewer')
    client_turn_id: 'turn-42'
    response_schema: '{"type":"object","properties":{"colors":{"type":"array"}}}'
})!
```

Session methods, resume options, shell output events, and Policy Helper failures:

```v
session.switch_auto_tier(copilot.auto_tier_balance)! // session.model.switchAutoTier
session.recheck_policy_helper()! // session.managedSettings.recheckPolicyHelper
mode_result := session.set_permission_mode('yolo')!
if !mode_result.success && mode_result.reason == copilot.permission_mode_refusal_managed_policy {
    println('Blocked by managed policy')
}
session.detach()! // session.detach

mut resumed := client.resume_session_with(copilot.ResumeSessionOptions{
    session_id: 'existing-session-id'
    config: copilot.SessionConfig{ tools: [lookup] }
    allow_transcript_recovery: true
    disable_resume: true // sent as suppressResumeEvent
})!
resumed.on(fn (ev copilot.SessionEvent) {
    if ev.event_type == copilot.event_tool_shell_output {
        print(copilot.parse_shell_output(ev.raw_data).text)
    }
})

client.create_session(copilot.SessionConfig{}) or {
    if failure := copilot.policy_helper_failure(err) {
        println('Policy Helper: ${failure.message}')
    }
    return
}
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection. The SDK keeps spawning the Copilot CLI over stdio (or connecting over TCP with `cli_url`).
- Server requests are answered synchronously on the reader thread, so `shellAuthentication.cancel` cannot interrupt a handler that is already running; the cancellation is acknowledged. Handlers must not call back into the SDK synchronously.
- Protocol 4 session limits accept only `max_ai_credits`; `SessionLimitsConfig.max_requests` and `max_tokens` are not sent. Only `enabled` of `MemoryConfiguration` is sent.
- The `connect` handshake is only sent when `client_info` is set; otherwise the SDK uses `ping`.

## Architecture

```
Your V App
    |
    v
CopilotClient  (client.v)
    |-- spawns or connects to CLI process
    |-- connect/ping handshake, SDK protocol 4 check
    |-- Dispatcher answers server requests on the reader thread
    |
    v
RpcTransport   (jsonrpc.v)
    |-- Content-Length framed JSON-RPC 2.0
    |-- request/response correlation via channels
    |-- one background reader for responses, requests, notifications
    |
    v
CopilotSession (session.v)
    |-- receives session.event notifications from the client
    |-- tool call dispatch
    |-- streaming deltas
```

## Core Modules

| File | Purpose |
|------|---------|
| `src/client.v` | `CopilotClient` -- process management, session creation, request dispatch |
| `src/session.v` | `CopilotSession` -- events, send/receive, tool dispatch, session RPCs |
| `src/jsonrpc.v` | `RpcTransport` -- Content-Length framing, request correlation |
| `src/session_params.v` | `group_session_params` -- protocol 4 session payload translation |
| `src/wire.v` | JSON rendering of protocol 4 option types, protocol version check |
| `src/types.v` | All struct and enum definitions with `@[json]` attributes |
| `src/tools.v` | `Tool` definition helpers and schema generation |
| `src/version.v` | Protocol and SDK version constants |

## API Reference

### CopilotClient

```v
// Create a client
mut client := copilot.new_client(copilot.CopilotClientOptions{
    cli_path: '/path/to/copilot-cli'  // or use cli_url for TCP
})

// Lifecycle
client.start()!                       // connect or spawn CLI
client.stop()!                        // shut down everything
client.wait_for_ready(5000)!          // block until connected

// Sessions
mut session := client.create_session(config)!
client.with_session(config, fn [mut s] () ! { ... })!

// Server info
status := client.get_status()!
models := client.get_models()!

// MCP secret store and plugin extension approvals (Copilot CLI 1.0.116 or newer)
saved := client.mcp_secrets_save(secret_id, value, config_dir)!
client.mcp_secrets_delete(secret_id, config_dir)!
client.mcp_secrets_delete_server(server_name, config_dir)!
grant := client.extension_approvals_grant(session_id, request_id)!
listed := client.extension_approvals_list(session_id)!
revoked := client.extension_approvals_revoke(extension_name, installation_key, session_id)!
```

### CopilotSession

```v
// Events
unsub := session.on(fn (ev copilot.SessionEvent) { ... })
defer { unsub() }

// Messaging
session.send(copilot.SendOptions{ prompt: 'Hello' })!
result := session.send_and_wait(opts, 30_000)!

// Cleanup
session.disconnect()!
```

### Tools

```v
mut params := map[string]copilot.ToolParam{}
params['city'] = copilot.ToolParam{
    param_type: 'string'
    description: 'City name'
    required: true
}

tool := copilot.define_tool('get_weather', 'Get weather', params,
    fn (args map[string]string) !string {
        city := args['city'] or { return error('missing city') }
        return '{"temp":"72F","city":"${city}"}'
    }
)
```

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk`. These fields live on
`copilot.SessionConfig` (session-scoped), `copilot.CopilotClientOptions`
(client-scoped), or `copilot.SendOptions` (per message).

### v2.5

- **Reasoning effort** — `reasoning_effort` (e.g. `'low'`, `'medium'`, `'high'`).
- **Session rewind** — `rewind_enabled` rolls the conversation back to an earlier turn.
- **Additional directories** — `additional_directories`.
- **Content exclusion** — `content_exclusion`.
- **Tool search** — `tool_search` discovers tools on demand.
- **Disabled MCP servers** — `disabled_mcp_servers`.
- **GitHub MCP tool config** — `github_mcp_tool_config`.
- **Canvas provider** — `canvas_provider`.
- **Custom agents local-only** — `custom_agents_local_only`.
- **Experimental mode / in-process FFI** — `experimental_mode`; `in_process` on `CopilotClientOptions`.
- **User-prompt-transformed hook** — `on_user_prompt_transformed` (`UserPromptTransformedHandler`).
- **Permission decision context** — `decision_context` (wire `decisionContext`).
- **Agent-factory args schema** — `args_schema` (wire `argsSchema`).
- **Built-in plugin directories** — `builtin_plugin_directories` on `CopilotClientOptions`.

### v2.4

- **BYOK bearer token provider** — `bearer_token_provider` (`BearerTokenProvider`).
- **MCP OAuth token handler** — `on_mcp_auth_request` (`McpAuthHandler`).
- **HTTP request handler** — `request_handler` (`CopilotRequestHandler`).
- **Session citations** — `enable_citations`.
- **Excluded built-in agents** — `excluded_builtin_agents`.
- **Spending limits** — `session_limits` with `SessionLimitsConfig{ max_ai_credits }`.
- **Session memory** — `memory` with `MemoryConfiguration`.
- **OTLP protocol / WebSocket transport** — `otlp_protocol`, `enable_web_socket_responses`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `on_post_tool_use`, `on_pre_mcp_tool_call`.
- **Message agent mode / display prompt** — `agent_mode`, `display_prompt` on `SendOptions`.
- **Constants** — `system_message_preamble`, `system_message_preserve`, `github_commit_attachment`, `github_repository_attachment`, `otlp_protocol_grpc`.
- **Experiment assignments** — `exp_assignments`.

```v
// Reasoning effort + session rewind + extra workspace roots
mut session := client.create_session(copilot.SessionConfig{
    reasoning_effort: 'high'
    rewind_enabled: true
    additional_directories: ['/repo/pkg-a', '/repo/pkg-b']
})!
```

```v
// Honor content exclusion and discover tools on demand
mut session := client.create_session(copilot.SessionConfig{
    content_exclusion: true
    tool_search: {'mode': 'auto'}
})!
```

```v
// Cap AI-credit spend and emit citations
mut session := client.create_session(copilot.SessionConfig{
    enable_citations: true
    session_limits: copilot.SessionLimitsConfig{ max_ai_credits: 5.0 }
})!
```

```v
// Bring-your-own-key: supply bearer tokens dynamically
mut session := client.create_session(copilot.SessionConfig{
    bearer_token_provider: fn (args copilot.ProviderTokenArgs) string {
        return 'ghs_exampletoken'
    }
})!
```

```v
// Per-message agent mode and display prompt
session.send(copilot.SendOptions{
    prompt: 'Refactor this module'
    agent_mode: 'plan'
    display_prompt: 'Refactor (planning)'
})!
```

## Running Tests

```bash
cd vlang
v test src/
```

## Running the Example

```bash
# With an existing CLI server
export COPILOT_CLI_URL=localhost:3000
v run examples/basic_example.v

# Or with a CLI binary
export COPILOT_CLI_PATH=/path/to/copilot-cli
v run examples/basic_example.v
```

## Cookbook

Focused recipes for common tasks:

- [Error Handling](cookbook/error-handling.md) -- result types, retries, graceful degradation
- [Tools and Skills](cookbook/tools-and-skills.md) -- define tools, multi-tool sessions
- [Multiple Sessions](cookbook/multiple-sessions.md) -- parallel conversations
- [Streaming](cookbook/streaming.md) -- real-time token-by-token output
- [Persisting Sessions](cookbook/persisting-sessions.md) -- resume conversations
- [Advanced Features](cookbook/advanced-features.md) -- agents, history, custom system prompts

## V Idioms Used

This SDK uses idiomatic V patterns throughout:

- **Structs with default values** -- `SessionConfig` has `streaming: true` by default
- **Result types (`!T`)** -- all fallible operations return results
- **`or` blocks** -- for inline error handling
- **`defer`** -- for cleanup (disconnect, stop)
- **`spawn`** -- for the background message reader
- **`shared` variables** -- for thread-safe handler lists
- **`@[json]` attributes** -- for serialization
- **`match` expressions** -- for event type dispatch
- **`chan`** -- for request/response correlation and timeouts

## Requirements

- V compiler 0.5+ (https://vlang.io)
- Copilot CLI 1.0.113 or newer (binary, npm `npm-loader.js`, or running server)
- No external dependencies beyond V stdlib
