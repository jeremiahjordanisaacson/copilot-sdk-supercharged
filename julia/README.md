# Copilot CLI SDK for Julia

A Julia SDK for programmatic access to the GitHub Copilot CLI.

> **Note:** This SDK is in public preview and may change in breaking ways.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

`CopilotSDK` is not registered in the Julia General registry yet, so add it straight from this repository and pin the release tag with `rev`:

```julia
using Pkg
Pkg.add(url="https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged", subdir="julia", rev="v3.1.0")
```

Or develop it from a local clone (run this from the repository root):

```julia
using Pkg
Pkg.develop(path="julia")
```

### Prerequisites

- Julia 1.8 or later
- A Copilot CLI installation, or an external server reachable via `cli_url`
- Set `COPILOT_CLI_PATH` or pass `cli_path` to `CopilotClient`

## Quick Start

```julia
using CopilotSDK

# Create and start a client
client = CopilotClient(cli_path="/path/to/copilot-cli")
start!(client)

# Create a session (on_permission_request is required)
session = create_session(client;
    model="gpt-4",
    on_permission_request=approve_all,
)

# Listen for events
on(session) do event
    if event.type == "assistant.message_delta"
        print(get(event.data, "content", ""))
    elseif event.type == "session.idle"
        println()
    end
end

# Send a message
send(session, "What is multiple dispatch in Julia?")
sleep(10)

# Clean up
disconnect(session)
stop!(client)
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The MCP secret store and plugin
extension approval functions are server-scoped client calls that require **Copilot CLI 1.0.116
or newer**; older CLIs answer with a method-not-found `JsonRpcError`, raised like any other RPC
error.

- **MCP secret store**: `mcp_secrets_save(client, secret_id, value; config_dir=nothing)` returns an `McpSecretsSaveResult` whose `used_keychain` is `true` when the OS keychain stored the credential (`false` for the file backend). `mcp_secrets_delete(client, secret_id; config_dir=nothing)` and `mcp_secrets_delete_server(client, server_name; config_dir=nothing)` remove credentials. The value is a credential: the SDK never logs it.
- **Plugin extension approvals**: `extension_approvals_grant(client, session_id, request_id)` persists a pending extension permission request before you answer it and returns a `PermissionsExtensionApprovalsGrantResult` (`granted`, `approval`). `extension_approvals_list(client; session_id=nothing)` returns a `PermissionsExtensionApprovalsListResult` whose `approvals` are `PermissionsExtensionApproval` values (`extension_name`, `plugin_name`, `capabilities`, `granted_at`, `installation_key`, `installation_label`). `extension_approvals_revoke(client, extension_name, installation_key; session_id=nothing)` returns a `PermissionsExtensionApprovalsRevokeResult` (`revoked`).
- **Ask-user image attachments**: the new `on_user_input_request` session option answers `userInput.request` (the `ask_user` tool). The handler gets a `UserInputRequest` (`session_id`, `question`, `choices`, `allow_freeform`, `request_id`, `image_attachment_constraints`, and `meta`, the request's opaque `_meta`). `image_attachment_constraints` is a `UserInputImageAttachmentConstraints` (`allowed_mime_types`, `max_bytes_per_image`, `max_count`), or `nothing` when the runtime does not accept images. Return a `UserInputResponse` (`answer`, `was_freeform`, `attachments`) whose images are `UserInputImageAttachment` values (base64 `data`, `mime_type`, optional `description` and `display_name`; the wire `type` is always `"image"`). `attachments` is sent only when set and non-empty, and the runtime enforces the limits.
- The existing `on_elicitation_request` option is not dispatched by this SDK, so the elicitation `requestId` / `_meta` passthrough does not apply here.

```julia
using Base64

session = create_session(client;
    on_user_input_request = req -> begin
        limits = req.image_attachment_constraints
        if limits !== nothing && "image/png" in limits.allowed_mime_types
            image = UserInputImageAttachment(data=base64encode(read("screenshot.png")),
                mime_type="image/png", display_name="screenshot.png")
            UserInputResponse(answer="", was_freeform=true, attachments=[image])  # image-only answer
        else
            UserInputResponse(answer="No screenshot available", was_freeform=true)
        end
    end,
    on_permission_request = approve_all,
)

saved = mcp_secrets_save(client, "my-server-token", ENV["MY_SERVER_TOKEN"])
saved.used_keychain || @info "Stored with the file backend"
mcp_secrets_delete_server(client, "my-server")

# request_id is the requestId of a pending extension permission request (from its
# permission.requested event): persist it, then answer the request as usual.
grant = extension_approvals_grant(client, session.session_id, request_id)
for approval in extension_approvals_list(client).approvals
    extension_approvals_revoke(client, approval.extension_name, approval.installation_key)
end
```

## What's New in v3.0.0

This release speaks **SDK protocol 4** and requires **Copilot CLI 1.0.113 or newer**; the
startup `ping` check rejects any other protocol version. Existing options keep working: you
still pass flat keyword options to `create_session` / `resume_session`, and the SDK
translates them into the grouped protocol 4 payload (`group_session_params`), for example
`streaming` becomes `events.streaming` and `hooks` becomes `hooks.hasHandlers`. The free-form
`instructions` option is sent as organization custom instructions
(`instructions.organizationText`).

Permission handlers now answer protocol 4 `permission.requested` events: the request has a
`kind` (such as `"write"`, `"shell"`, or `"mcp"`), a `tool_call_id`, and the full request in
`details`. Return `true`/`"allow"` (approve once), `false`/`"deny"` (reject), or a decision
such as `(kind="reject", feedback="Not now")`. Without `on_permission_request` the runtime
denies operations that need approval, including custom tools, unless a tool is defined with
`skip_permission=true`.

### Client options

```julia
client = CopilotClient(
    # Identify your integration on the connect handshake
    client_info = ClientInfo(editor_name="MyEditor", editor_version="1.2.0"),
    # "coding-agent" (default) or "empty": empty mode requires available_tools on every
    # create/resume and applies restrictive defaults ("copilot-cli" is an alias of the default)
    mode = CLIENT_MODE_CODING_AGENT,
    # Proxy and authentication callbacks (registered with network.setProvider)
    network = NetworkConfig(
        scope = "connection",
        resolve_proxy = req -> "PROXY proxy.example:8080; DIRECT",
        authenticate_basic = req -> (username="me", password=ENV["PROXY_PASSWORD"]),
    ),
)
start!(client)
clear_managed_settings_cache(client)   # managedSettings.clearCache
```

```julia
client = CopilotClient(mode=CLIENT_MODE_EMPTY)
session = create_session(client; available_tools=["view", "grep"], on_permission_request=approve_all)
```

### Named BYOK providers and models

```julia
session = create_session(client;
    model = "azure/gpt-5",                      # "provider/id"
    providers = [NamedProviderConfig(name="azure", type="azure",
        base_url="https://my-resource.openai.azure.com",
        api_key_command="az account get-access-token --query accessToken -o tsv")],
    models = [ProviderModelConfig(provider="azure", id="gpt-5", wire_model="my-deployment",
        max_output_tokens=16384)],
    default_provider = "azure",
    capi = Dict{String, Any}("enabled" => false),  # hide Copilot API models
    on_permission_request = approve_all,
)

# The legacy single provider still works (sent as the "default" connection)
session = create_session(client;
    model = "my-model",
    provider = ProviderConfig(type="openai", base_url="http://localhost:11434/v1",
        api_key_command="get-key", max_output_tokens=4096),
    on_permission_request = approve_all,
)

# Clear a saved default provider on resume
session = resume_session(client, session_id; default_provider=missing, on_permission_request=approve_all)
```

### Session options

```julia
session = create_session(client;
    auto_tier = "balance",                          # Auto routing tier
    ask_user_variant = "elicitation",               # or "legacy"
    sandbox_config = SandboxConfig(allow_bypass=true),
    feature_flags = Dict{String, Any}("some_flag" => true),
    allowed_models = ["claude-sonnet-4.5"],
    auth_client_id_metadata_url = "https://example.com/oauth/client.json",
    model_capabilities = Dict{String, Any}("limits" => Dict{String, Any}("max_output_tokens" => 4096)),
    agent_directories = ["./agents"],
    hook_directories = ["./hooks"],
    trust_working_directory = "session",            # create only ("session" or "user")
    image_generation = ImageGenerationConfig(enabled=true, save_to_disk=false),
    autopilot = AutopilotOptions(continuation_enabled=true, max_continues=3),
    on_subagent_start = input -> @info("subagent started", input),
    on_subagent_stop = input -> @info("subagent stopped", input),
    on_permission_request = approve_all,
)

session = resume_session(client, session_id;
    allow_transcript_recovery = true,               # resume only
    on_permission_request = approve_all,
)
```

### Callbacks: GitHub tokens, shell authentication, memory

```julia
memories = StoredMemory[]
session = create_session(client;
    # Answers gitHubToken.getToken; mutually exclusive with github_token
    github_token_provider = args -> (kind="token", access_token=fetch_token(args.host), expires_in=28800),
    # Passwords and other answers requested by shell commands (sudo -A, ssh, git)
    on_shell_authentication_request = req ->
        req.input_kind == "secret" ? (action="accept", value=ENV["SUDO_PASSWORD"]) : (action="cancel",),
    # Memories the agent asks to store
    store_memory = m -> push!(memories, m),
    on_permission_request = approve_all,
)
```

### Messages, tools, and events

```julia
lookup = define_tool("lookup", "Look up a record"; idempotent=true) do inv
    ToolResult(text_result_for_llm="42")
end

id = send(session, MessageOptions(
    prompt = "Summarize the release as JSON",
    source = message_source_agent("release-bot"),   # or MESSAGE_SOURCE_USER / MESSAGE_SOURCE_SYSTEM
    client_turn_id = "turn-42",
    response_schema = Dict{String, Any}("type" => "object",
        "properties" => Dict{String, Any}("summary" => Dict{String, Any}("type" => "string"))),
))

on(session) do event
    if event.type == EVENT_TOOL_SHELL_OUTPUT
        data = shell_output_event_data(event)
        print(data.stream == "stderr" ? stderr : stdout, data.text)
    end
end
```

### Session control and managed policy

```julia
switch_auto_tier(session, "fast")                  # session.model.switchAutoTier
result = set_permission_mode(session, "allow-all")
if !result.success && result.reason == PERMISSION_MODE_REFUSAL_MANAGED_POLICY
    @warn "Permission mode is locked by managed policy"
end
settings = get_managed_settings(session)
recheck_policy_helper(session)
detach_session(session)                            # session.detach (disconnect also detaches)

try
    create_session(client; on_permission_request=approve_all)
catch err
    failure = policy_helper_failure(err)           # PolicyHelperFailure or nothing
    failure === nothing || @error "Policy helper failed" failure.category failure.message
    rethrow()
end
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not
  implemented: it is an experimental upstream surface that depends on the runtime's in-process
  hosting library.
- In-process native runtime hosting is not the default connection: this SDK keeps spawning the
  CLI over stdio (or connecting over TCP with `cli_url`); the existing `in_process` option is
  unchanged.
- `send_and_wait` returns on `session.idle`; a `session.error` event is delivered to listeners
  but does not raise.

## API Reference

### CopilotClient

The main entry point. Manages the connection to the Copilot CLI server.

```julia
# Default construction (uses env or PATH to find CLI)
client = CopilotClient()

# With explicit options
client = CopilotClient(
    cli_path="/path/to/copilot-cli",
    log_level=LOG_DEBUG,
)

# Connect to an external server
client = CopilotClient(cli_url="localhost:3000")
```

#### Methods

| Method | Description |
|---|---|
| `start!(client)` | Start/connect the CLI server |
| `stop!(client)` | Stop the server and disconnect all sessions |
| `create_session(client; kwargs...)` | Create a new conversation session |
| `resume_session(client, session_id; kwargs...)` | Resume an existing session |
| `get_status(client)` | Query server version and auth status |
| `get_models(client)` | List available models |
| `list_sessions(client)` | List known sessions on the server |
| `clear_managed_settings_cache(client)` | Re-read managed (enterprise) settings |
| `mcp_secrets_save(client, secret_id, value; config_dir)` | Save an MCP credential (returns `McpSecretsSaveResult`) |
| `mcp_secrets_delete(client, secret_id; config_dir)` | Delete an MCP credential |
| `mcp_secrets_delete_server(client, server_name; config_dir)` | Delete the MCP credentials indexed for a server |
| `extension_approvals_grant(client, session_id, request_id)` | Persist a pending plugin extension approval |
| `extension_approvals_list(client; session_id)` | List plugin extension approvals |
| `extension_approvals_revoke(client, extension_name, installation_key; session_id)` | Revoke a plugin extension approval |

### CopilotSession

Represents a live conversation. Created via `create_session`.

```julia
session = create_session(client;
    model="gpt-4",
    streaming=true,
    on_permission_request=approve_all,
    tools=[my_tool],
)
```

#### Methods

| Method | Description |
|---|---|
| `on(session, handler)` | Register an event handler |
| `send(session, prompt)` | Send a message (non-blocking) |
| `send_and_wait(session, prompt; timeout=120)` | Send and block until idle |
| `disconnect(session)` | Disconnect the session |
| `detach_session(session)` | Detach the session (it can be resumed later) |
| `switch_auto_tier(session, tier)` | Change the Auto routing tier |
| `set_permission_mode(session, mode)` | Change the permission mode (`PermissionModeSetResult`) |
| `get_managed_settings(session)` | Managed settings resolved for the session |
| `recheck_policy_helper(session)` | Re-run the managed-settings policy helper |

#### Do-block syntax

```julia
on(session) do event
    println(event.type)
end
```

### Tools

Define tools that the assistant can invoke:

```julia
# Do-block syntax
my_tool = define_tool("calculator", "Evaluate math expressions") do inv
    expr = get(inv.arguments, "expression", "0")
    result = eval(Meta.parse(expr))
    ToolResult(text_result_for_llm="Result: $result")
end

# With JSON Schema parameters
my_tool = define_tool("greet", "Greet someone";
    parameters=Dict{String, Any}(
        "type" => "object",
        "properties" => Dict{String, Any}(
            "name" => Dict{String, Any}("type" => "string"),
        ),
        "required" => ["name"],
    ),
) do inv
    ToolResult(text_result_for_llm="Hello, $(inv.arguments["name"])!")
end
```

### Permission Handling

Every session requires a permission handler:

```julia
# Approve everything
session = create_session(client; on_permission_request=approve_all)

# Deny everything
session = create_session(client; on_permission_request=deny_all)

# Custom logic
session = create_session(client;
    on_permission_request=req -> begin
        if req.kind == "read" || req.tool_name in ("read_file", "search")
            return "allow"
        end
        return "deny"
    end,
)
```

### Session Events

Events are dispatched to handlers registered with `on()`:

| Event Type | Description |
|---|---|
| `assistant.message_delta` | Streaming text chunk |
| `assistant.message` | Final consolidated message |
| `assistant.reasoning_delta` | Reasoning text chunk |
| `assistant.reasoning` | Final reasoning text |
| `session.idle` | Session finished processing |
| `session.error` | An error occurred |
| `permission.requested` | Permission prompt (handled via callback) |
| `tool.shell_output` | Shell command output chunk (`shell_output_event_data`) |
| `external_tool.requested` | Tool call for a registered tool (answered automatically) |

### Types

| Type | Description |
|---|---|
| `CopilotClientOptions` | Client configuration (mutable, keyword constructable) |
| `SessionConfig` | Session creation options |
| `MessageOptions` | Message payload for `send` |
| `SessionEvent` | Event from the CLI server |
| `ToolResult` | Return value from tool handlers |
| `ToolInvocation` | Context passed to tool handlers |
| `Tool` | Tool definition struct |
| `PermissionRequest` | Permission request from the server |
| `ServerStatus` | Server status info |
| `ModelInfo` | Model metadata |
| `SessionMetadata` | Session listing metadata |
| `ClientInfo`, `NetworkConfig` | Client handshake identity and network callbacks |
| `ProviderConfig`, `NamedProviderConfig`, `ProviderModelConfig` | BYOK provider connections and models |
| `SandboxConfig`, `ImageGenerationConfig`, `AutopilotOptions` | Session option groups |
| `ShellAuthenticationRequest`, `StoredMemory`, `GitHubTokenProviderArgs` | Callback arguments |
| `ShellOutputEventData` | Data of a `tool.shell_output` event |
| `PermissionModeSetResult`, `PolicyHelperFailure` | Permission mode result and Policy Helper failure |

## Recent Features (v2.4–v2.5)

Recent upstream syncs added session and client options for parity with `@github/copilot-sdk`. Pass session options as keyword arguments to `create_session` (they map onto `SessionConfig`); client options map onto `CopilotClientOptions`.

**v2.5 wave (`create_session` / `SessionConfig` unless noted):**

- **Reasoning effort** — `reasoning_effort` (e.g. `"minimal"`, `"low"`, `"medium"`, `"high"`, `"max"`).
- **Tool search** — `tool_search`; discover tools on demand.
- **Session rewind** — `rewind_enabled`; roll the conversation back to an earlier turn.
- **Content exclusion** — `content_exclusion`; honor rules that hide files from the agent.
- **Additional directories** — `additional_directories`.
- **Disabled MCP servers** — `disabled_mcp_servers`.
- **GitHub MCP tool config** — `github_mcp_tool_config`.
- **Canvas provider** — `canvas_provider`.
- **Custom agents local-only** — `custom_agents_local_only`.
- **Experimental mode** — `experimental_mode`.
- **User-prompt-transformed hook** — `on_user_prompt_transformed`.
- **Permission decision context** — `PermissionResponse.decision_context`.
- **Agent factory args** — `AgentFactoryOptions.args_schema`.
- **Built-in plugin directories** — `CopilotClientOptions.builtin_plugin_directories`.
- **In-process FFI transport** — `CopilotClientOptions.in_process`.

**v2.4 wave:**

- **BYOK bearer token provider** — `bearer_token_provider` (a `Function`).
- **MCP OAuth token handler** — `on_mcp_auth_request`.
- **HTTP request handler** — `request_handler` (`CopilotRequestHandler`).
- **Session citations** — `enable_citations`.
- **Excluded built-in agents** — `excluded_builtin_agents`.
- **Session spending limits** — `session_limits` (`SessionLimitsConfig(max_ai_credits=...)`).
- **OTLP telemetry protocol** — `otlp_protocol` (`OTLP_PROTOCOL_GRPC` / `OTLP_PROTOCOL_HTTP`).
- **WebSocket transport** — `enable_web_socket_responses`.
- **Session memory** — `memory` (`MemoryConfiguration`).
- **Experiment assignments** — `exp_assignments`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `on_post_tool_use`, `on_pre_mcp_tool_call`.
- **Message agent mode / display prompt** — `MessageOptions.agent_mode`, `MessageOptions.display_prompt`.
- **Tool defer loading** — `TOOL_DEFER_EAGER` / `TOOL_DEFER_LAZY`.
- **System-message sections** — `SYSTEM_MESSAGE_PREAMBLE` / `SYSTEM_MESSAGE_PRESERVE`.
- **GitHub attachments** — `GITHUB_COMMIT_ATTACHMENT` / `GITHUB_REPOSITORY_ATTACHMENT`.

### Reasoning effort & tool search

```julia
session = create_session(client;
    model="gpt-4o",
    reasoning_effort="high",
    tool_search=Dict("enabled" => true),
    on_permission_request=approve_all,
)
```

### Session rewind & additional directories

```julia
session = create_session(client;
    rewind_enabled=true,
    additional_directories=["/repo/docs", "/repo/vendor"],
    on_permission_request=approve_all,
)
```

### Content exclusion

```julia
session = create_session(client;
    content_exclusion=true,
    on_permission_request=approve_all,
)
```

### BYOK bearer token provider

```julia
session = create_session(client;
    bearer_token_provider=args -> fetch_token(args),
    on_permission_request=approve_all,
)
```

### Spending limits & citations

```julia
session = create_session(client;
    session_limits=SessionLimitsConfig(max_ai_credits=5.0),
    enable_citations=true,
    on_permission_request=approve_all,
)
```

### In-process transport

```julia
client = CopilotClient(in_process=true, builtin_plugin_directories=["./plugins"])
```

## Cookbook

See [`cookbook/`](./cookbook/) for complete recipes:

| Recipe | Description |
|---|---|
| [`streaming_chat.jl`](./cookbook/streaming_chat.jl) | Real-time streaming output |
| [`custom_tools.jl`](./cookbook/custom_tools.jl) | Register and use custom tools |
| [`multi_turn.jl`](./cookbook/multi_turn.jl) | Multi-turn conversations |
| [`permission_handling.jl`](./cookbook/permission_handling.jl) | Permission strategies |
| [`error_handling.jl`](./cookbook/error_handling.jl) | Robust error handling patterns |
| [`external_server.jl`](./cookbook/external_server.jl) | Connect to external CLI server |
| [`concurrent_sessions.jl`](./cookbook/concurrent_sessions.jl) | Parallel sessions with @async/@sync |

## Running Tests

```bash
cd julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'
julia --project=. test/runtests.jl
```

## Architecture

```
App  -->  CopilotClient  -->  JsonRpcClient  -->  Copilot CLI (stdio/TCP)
               |
         CopilotSession
          - event listeners
          - tool dispatch
          - permission handling
```

The SDK spawns the CLI as a child process (stdio mode) or connects to a running server (TCP mode). All communication uses JSON-RPC 2.0 with Content-Length framing.

## Julia Idioms Used

- **Multiple dispatch**: `send(session, ::String)` vs `send(session, ::MessageOptions)`
- **Do-block callbacks**: `on(session) do event ... end`, `define_tool("name", "desc") do inv ... end`
- **`@async` / `@sync`**: Concurrent sessions (see cookbook)
- **`Channel`**: Event coordination and blocking waits
- **Mutable vs immutable structs**: `CopilotClient` (mutable state) vs `MessageOptions` (immutable value)
- **`@kwdef`**: Keyword-constructable structs throughout

## License

See [LICENSE](../LICENSE) in the repository root.
