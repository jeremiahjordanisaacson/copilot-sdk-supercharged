# Copilot Supercharged SDK for Elixir

Elixir SDK for the [GitHub Copilot CLI](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged). Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio.

## Requirements

- Elixir 1.15+
- The `copilot` CLI installed and on your PATH (or specify the path via `:cli_path`)
- Authenticated via `copilot auth login`

## Installation

Add to your `mix.exs` dependencies:

```elixir
def deps do
  [
    {:copilot_sdk_supercharged, path: "../elixir"}  # or from Hex when published
  ]
end
```

Then fetch dependencies:

```bash
mix deps.get
```

## Quick Start

```elixir
alias Copilot.Client
alias Copilot.Session
alias Copilot.DefineTool
alias Copilot.Types.{CopilotClientOptions, SessionConfig, MessageOptions}

# Start the client (spawns the CLI process automatically)
{:ok, client} = Client.start_link(%CopilotClientOptions{log_level: "info"})

# Create a session
{:ok, session} = Client.create_session(client, %SessionConfig{})

# Subscribe to events
Session.on(session, fn event ->
  if event["type"] == "assistant.message" do
    IO.puts("Assistant: " <> event["data"]["content"])
  end
end)

# Send a message and wait for the response
{:ok, response} = Session.send_and_wait(session, %MessageOptions{prompt: "What is 2+2?"})
IO.puts("Response: #{response["data"]["content"]}")

# Clean up
Session.destroy(session)
Client.stop(client)
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The six new `Copilot.Client` functions below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, returned as `{:error, error}` like any other RPC error.

### MCP secret store and plugin extension approvals

```elixir
alias Copilot.Client

# MCP secret store. The value is a credential: it is sent to the runtime and never logged.
{:ok, saved} =
  Client.mcp_secrets_save(client, "my-server/api-key", System.fetch_env!("MY_API_KEY"),
    config_dir: "/path/to/config"
  )

IO.puts(if saved.used_keychain, do: "Stored in the OS keychain", else: "Stored in the file backend")

:ok = Client.mcp_secrets_delete(client, "my-server/api-key")
:ok = Client.mcp_secrets_delete_server(client, "my-server")

# Plugin extension approvals (user scoped). Grant persists a pending extension
# permission request before you answer it.
{:ok, grant} = Client.extension_approvals_grant(client, session_id, request_id)
if grant.granted, do: IO.puts("Approved #{grant.approval.extension_name}")

{:ok, %{approvals: approvals}} = Client.extension_approvals_list(client)

for approval <- approvals do
  {:ok, %{revoked: true}} =
    Client.extension_approvals_revoke(client, approval.extension_name, approval.installation_key)
end
```

| Function | Wire method | Returns |
|---|---|---|
| `Client.mcp_secrets_save/4` (option `config_dir:`) | `mcp.secrets.save` | `{:ok, %McpSecretsSaveResult{used_keychain: bool}}` (`false` means the file backend stored it) |
| `Client.mcp_secrets_delete/3` (option `config_dir:`) | `mcp.secrets.delete` | `:ok` |
| `Client.mcp_secrets_delete_server/3` (option `config_dir:`) | `mcp.secrets.deleteServer` | `:ok` |
| `Client.extension_approvals_grant/3` | `permissions.extensionApprovals.grant` | `{:ok, %PermissionsExtensionApprovalsGrantResult{granted, approval}}` |
| `Client.extension_approvals_list/2` (option `session_id:`) | `permissions.extensionApprovals.list` | `{:ok, %PermissionsExtensionApprovalsListResult{approvals}}` |
| `Client.extension_approvals_revoke/4` (option `session_id:`) | `permissions.extensionApprovals.revoke` | `{:ok, %PermissionsExtensionApprovalsRevokeResult{revoked: bool}}` |

Each `%PermissionsExtensionApproval{}` has `extension_name` (the stable identity, for example `"plugin:my-plugin:my-extension"`), `plugin_name`, `capabilities`, `granted_at` (RFC 3339), `installation_key` (opaque: pass it back unchanged to revoke), and `installation_label`.

### Ask-user image attachments

When the runtime accepts images, the `%UserInputRequest{}` passed to `on_user_input_request` carries `image_attachment_constraints` (`%UserInputImageAttachmentConstraints{allowed_mime_types, max_bytes_per_image, max_count}`); it is `nil` otherwise. The request also exposes `request_id` and `meta` (the opaque `_meta` map) when the runtime sends them. Answer with `attachments`, a list of `%UserInputImageAttachment{}` (base64 `data`, `mime_type`, optional `description` and `display_name`; the wire `type` is always `"image"`). `attachments` is sent only when set and is only valid when the request advertised constraints; the runtime enforces the limits.

```elixir
alias Copilot.Types.{UserInputImageAttachment, UserInputRequest, UserInputResponse}

config = %SessionConfig{
  on_user_input_request: fn %UserInputRequest{image_attachment_constraints: constraints}, _ctx ->
    if constraints && "image/png" in constraints.allowed_mime_types do
      image = %UserInputImageAttachment{
        data: Base.encode64(File.read!("screenshot.png")),
        mime_type: "image/png",
        display_name: "screenshot.png"
      }

      # Image-only answer: empty answer and was_freeform true
      %UserInputResponse{answer: "", was_freeform: true, attachments: [image]}
    else
      %UserInputResponse{answer: "No screenshot available", was_freeform: true}
    end
  end
}
```

## What's New in v3.0.0

This release speaks **SDK protocol 4**, the contract of the Microsoft Copilot SDK 2.0 runtime, and requires **Copilot CLI 1.0.113 or newer**: older CLIs fail `Client.start/1` with a protocol mismatch error. Existing options keep working. You fill `%SessionConfig{}` and `%ResumeSessionConfig{}` exactly as before, and `Copilot.Client` converts the flat payload into the grouped v4 `session.create` / `session.resume` payload right before sending (for example `streaming` becomes `events.streaming`, `hooks` becomes `hooks.hasHandlers`, and `disable_resume` becomes `suppressResumeEvent`). The legacy single `provider` becomes a named `"default"` connection with Copilot API (CAPI) models disabled, which preserves v1 BYOK behavior. A `cli_path` ending in `.js` (such as the CLI's `npm-loader.js`) now runs under `node`.

This release also brings the Elixir SDK up to the v2.6.0 feature set and fixes session event delivery: notifications are now dispatched with the client's current session list, so events reach every session.

New in this release:

* **Client info**: `%CopilotClientOptions{client_info: %ClientInfo{editor_name: ..., editor_version: ..., extension_name: ..., extension_version: ...}}` is sent on a `connect` handshake before the protocol check (older CLIs without `connect` are tolerated).
* **Auto routing tier** (experimental): the `auto_tier` session option (sent in `github.capi`) and `Session.switch_auto_tier/2` (`session.model.switchAutoTier`; pass `nil` to return to default routing).
* **Ask-user variant**: `ask_user_variant: "legacy"` or `"elicitation"`.
* **GitHub token provider**: `github_token_provider` answers the runtime's `gitHubToken.getToken` requests with a `%GitHubToken{}` (or `:cancelled`). It is mutually exclusive with `github_token`, and the registration is dropped when the session is destroyed or detached.
* **Sandbox**: `sandbox_config: %SandboxConfig{enabled: true, allow_bypass: true}`.
* **Feature flags and model allowlist**: `feature_flags` and `allowed_models`.
* **Session detach**: `Session.detach/1` sends `session.detach` (falling back to `session.destroy`); the session can be resumed later.
* **Message options**: `source` (`MessageSource.user/0`, `system/0`, `agent/1`), `response_schema` (structured output as `responseFormat` `json_schema`), and `client_turn_id` (`clientTurnId`).
* **Managed settings**: `Client.clear_managed_settings_cache/1`, `Session.get_managed_settings/1`, and `Session.recheck_policy_helper/1`.
* **OAuth client metadata URL**: `auth_client_id_metadata_url`.
* **Max output tokens**: `ModelLimits.max_output_tokens`. Override it with `model_capabilities: %{"limits" => %{"max_output_tokens" => 8192}}`, which is now sent as `inference.capabilities`.
* **Directories and trust**: `agent_directories`, `hook_directories`, create-only `name`, `trust_working_directory`, and `refresh_custom_instructions`, and resume-only `allow_transcript_recovery`.
* **Image generation and autopilot**: `image_generation: %ImageGenerationConfig{}` (`enabled`, `watermark`, `save_to_disk`) and `autopilot: %AutopilotOptions{}` (`continuation_enabled`, `max_continues`).
* **Sub-agent hooks**: `SessionHooks` `on_subagent_start` and `on_subagent_stop` (hook types `subagentStart` and `subagentStop`, delivered through `hooks.invoke`).
* **BYOK API key command**: `ProviderConfig` `api_key_command` and `model_provider`.
* **Named BYOK providers**: `providers` (`%NamedProviderConfig{}`), `models` (`%ProviderModelConfig{}`), `default_provider`, and `capi` (`%CapiConfig{enabled: false}`). Select a registered model as `"<provider>/<id>"`. On resume, `default_provider: :clear` sends `null` to clear the default connection.
* **Idempotent tools**: `%Tool{idempotent: true}` or `DefineTool.define(name, idempotent: true, ...)`.
* **Client mode**: `%CopilotClientOptions{mode: "coding-agent"}` (the default, unchanged behavior) or `mode: "empty"`; `"copilot-cli"` is an alias of `"coding-agent"`. In empty mode every create and resume must set `available_tools` (otherwise `{:error, reason}`), and safe defaults are applied under your values: experimental mode, telemetry, cross-session store, memory, on-demand instructions, repository hooks, and host Git operations off; in-memory MCP OAuth token and embedding cache storage; embedding retrieval skipped.
* **Shell output events**: `SessionEventType.tool_shell_output/0` (`"tool.shell_output"`) and `ToolShellOutput.from_map/1` (`tool_call_id`, `text`, `sequence`, `stream`).
* **Shell authentication**: `on_shell_authentication_request` receives a `%ShellAuthenticationRequest{}` (`program`, `prompt`, `input_kind`, `command`, `cwd`) and returns `{:accept, value}` or `:cancel`. When the runtime sends `shellAuthentication.cancel`, the pending prompt is answered with `cancel`.
* **Memory store**: `store_memory` receives each approved `%StoredMemory{}` (`subject`, `fact`, `citations`, `reason`) through `memoryStore.store` and returns `:ok` or `{:error, reason}`.
* **Network callbacks**: `%CopilotClientOptions{network: %NetworkConfig{}}` with `resolve_proxy`, `authenticate_negotiate`, and `authenticate_basic`, registered with `network.setProvider` after connecting. Tokens and passwords are never logged.
* **Policy helper failure**: `PolicyHelperFailure.from_error/1` reads `policyHelperFailure` from a failed `create_session` / `resume_session` result or a managed settings snapshot.
* **Permission mode refusal reason**: `Session.set_permission_mode/2` returns `%PermissionsSetModeResult{success, mode, reason}`; `PermissionModeRefusal.managed_policy/0` is `"managed_policy"`.

Run a shared service in empty mode with client info and proxy callbacks:

```elixir
alias Copilot.{Client, Session, DefineTool}

alias Copilot.Types.{
  CapiConfig,
  ClientInfo,
  CopilotClientOptions,
  GitHubToken,
  MessageOptions,
  MessageSource,
  NamedProviderConfig,
  NetworkConfig,
  PermissionModeRefusal,
  PolicyHelperFailure,
  ProviderModelConfig,
  ResumeSessionConfig,
  SandboxConfig,
  SessionConfig,
  SessionEventType,
  ToolShellOutput
}

{:ok, client} =
  Client.start_link(%CopilotClientOptions{
    mode: "empty",
    client_info: %ClientInfo{editor_name: "acme-portal", editor_version: "2.4.0"},
    network: %NetworkConfig{
      resolve_proxy: fn %{url: url} ->
        if String.contains?(url, ".internal.example"),
          do: "DIRECT",
          else: "PROXY proxy.example:8080"
      end,
      authenticate_basic: fn
        %{previous_rejected: true} -> nil
        _request -> %{username: "svc-proxy", password: System.get_env("PROXY_PASSWORD")}
      end
    }
  })
```

Use named providers, an idempotent tool, a GitHub token provider, shell authentication, and an application memory store:

```elixir
lookup_order =
  DefineTool.define("lookup_order",
    description: "Look up an order by ID",
    parameters: %{"type" => "object", "properties" => %{"id" => %{"type" => "string"}}},
    idempotent: true,
    handler: fn %{"id" => id}, _invocation -> "Order #{id} shipped" end
  )

{:ok, session} =
  Client.create_session(client, %SessionConfig{
    model: "gateway/chat",
    available_tools: ["view", "lookup_order"],
    tools: [lookup_order],
    providers: [
      %NamedProviderConfig{
        name: "gateway",
        type: "openai",
        base_url: "https://models.example.com/v1",
        api_key_command: "print-gateway-key"
      }
    ],
    models: [
      %ProviderModelConfig{
        provider: "gateway",
        id: "chat",
        wire_model: "deployment-1",
        max_output_tokens: 8192
      }
    ],
    capi: %CapiConfig{enabled: false},
    sandbox_config: %SandboxConfig{enabled: true, allow_bypass: true},
    github_token_provider: fn %{host: _host, reason: _reason} ->
      %GitHubToken{access_token: System.fetch_env!("GH_TOKEN"), expires_in: 8 * 3600}
    end,
    on_shell_authentication_request: fn
      %{input_kind: "confirm"} -> :cancel
      %{program: "sudo"} -> {:accept, System.get_env("SUDO_PASSWORD", "")}
      _request -> :cancel
    end,
    store_memory: fn memory ->
      IO.puts("remember [#{memory.subject}]: #{memory.fact}")
      :ok
    end
  })
```

Stream shell output, tag a turn, switch the Auto tier, and change the permission mode:

```elixir
Session.on(session, SessionEventType.tool_shell_output(), fn event ->
  IO.write(ToolShellOutput.from_map(event).text)
end)

{:ok, _message_id} =
  Session.send(session, %MessageOptions{
    prompt: "Run the test suite",
    source: MessageSource.agent("planner"),
    client_turn_id: "turn-42"
  })

{:ok, _ack} = Session.switch_auto_tier(session, "intelligence")

managed_policy = PermissionModeRefusal.managed_policy()

case Session.set_permission_mode(session, "allow-all") do
  {:ok, %{success: false, reason: ^managed_policy, mode: mode}} ->
    IO.puts("Managed policy keeps the session in #{mode} mode")

  {:ok, _applied} ->
    :ok
end
```

Inspect a policy helper failure, and clear the default provider on resume:

```elixir
case Client.create_session(client, %SessionConfig{available_tools: ["view"]}) do
  {:ok, _session} ->
    :ok

  {:error, _} = error ->
    case PolicyHelperFailure.from_error(error) do
      %PolicyHelperFailure{category: category, message: message} ->
        IO.puts("Policy helper failed (#{category}): #{message}")

      nil ->
        error
    end
end

{:ok, _resumed} =
  Client.resume_session(client, session_id, %ResumeSessionConfig{
    available_tools: ["view"],
    default_provider: :clear,
    allow_transcript_recovery: true
  })
```

### Known limitations

* The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not available. It is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process native runtime hosting is not the default connection: this SDK keeps spawning the CLI over stdio. The existing `in_process` option is unchanged.
* Session callbacks (`tool.call`, `hooks.invoke`, shell authentication, memory store) look up the session through the client process. A callback that the runtime raises while `create_session/2` or `resume_session/3` is still in flight (for example a `sessionStart` hook) waits for that call to return, and is answered with no output if it cannot be routed within 5 seconds.
* Option keys this SDK sent before v3.0.0 that have no protocol 4 equivalent (`rewindEnabled`, `otlpProtocol`, `contentExclusion`, `mcpAuthHandler`) are kept unchanged at the top level of the payload for forward compatibility.

## Architecture

The SDK consists of the following modules:

| Module | Description |
|---|---|
| `Copilot.Client` | Main client GenServer. Spawns the CLI process, manages the JSON-RPC connection, and provides session lifecycle operations (create, resume, delete, list). |
| `Copilot.Session` | Session GenServer. Sends messages, subscribes to events, handles tool calls, permissions, user input, and hooks. |
| `Copilot.JsonRpcClient` | Low-level JSON-RPC 2.0 client. It opens and owns the CLI port (stdio) and frames messages with Content-Length headers (see [Connection](#connection)). |
| `Copilot.Types` | All type definitions as Elixir structs with typespecs. |
| `Copilot.DefineTool` | Helper for defining tools to expose to the CLI. |
| `Copilot.SdkProtocolVersion` | Protocol version constant (must match the server). |

## Defining Tools

Tools allow the assistant to call custom functions you define. Use `Copilot.DefineTool.define/2`:

```elixir
tool = Copilot.DefineTool.define("get_weather",
  description: "Get the current weather for a city.",
  parameters: %{
    "type" => "object",
    "properties" => %{
      "city" => %{"type" => "string", "description" => "City name"}
    },
    "required" => ["city"]
  },
  handler: fn %{"city" => city}, _invocation ->
    "The weather in #{city} is sunny, 72F."
  end
)

{:ok, session} = Client.create_session(client, %SessionConfig{tools: [tool]})
```

The handler function receives two arguments:

1. The parsed arguments (a map matching your JSON schema)
2. A `Copilot.Types.ToolInvocation` struct with session context

It can return:
- A plain string (wrapped as a success result)
- A `Copilot.Types.ToolResult` struct (for full control)
- Any other term (JSON-encoded as a success result)

## Image Generation

Request image responses using `response_format` and `image_options`:

```elixir
response = Copilot.Session.send_and_wait(session, %Copilot.Types.MessageOptions{
  prompt: "Generate a sunset over mountains",
  response_format: :image,
  image_options: %{size: "1024x1024", quality: "hd", style: "natural"}
})
```

## Event Subscriptions

Subscribe to all events or specific event types:

```elixir
# All events
ref = Session.on(session, fn event -> IO.inspect(event) end)

# Specific event type
ref = Session.on(session, "assistant.message", fn event ->
  IO.puts(event["data"]["content"])
end)

# Unsubscribe
Session.off(session, ref)
```

### Common Event Types

| Event Type | Description |
|---|---|
| `session.start` | Session started |
| `session.idle` | Session finished processing |
| `session.error` | An error occurred |
| `user.message` | User message recorded |
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk (when streaming enabled) |
| `tool.execution_start` | Tool execution started |
| `tool.execution_complete` | Tool execution finished |

## Permissions

Handle permission requests from the server:

```elixir
alias Copilot.Types.{PermissionRequest, PermissionRequestResult}

config = %SessionConfig{
  on_permission_request: fn %PermissionRequest{kind: kind}, _ctx ->
    IO.puts("Permission requested: #{kind}")
    %PermissionRequestResult{kind: :approved}
  end
}
```

## User Input (ask_user)

Handle user input requests from the agent:

```elixir
alias Copilot.Types.{UserInputRequest, UserInputResponse}

config = %SessionConfig{
  on_user_input_request: fn %UserInputRequest{question: q}, _ctx ->
    answer = IO.gets("#{q} > ") |> String.trim()
    %UserInputResponse{answer: answer, was_freeform: true}
  end
}
```

## Hooks

Intercept session lifecycle events with hooks:

```elixir
alias Copilot.Types.SessionHooks

config = %SessionConfig{
  hooks: %SessionHooks{
    on_pre_tool_use: fn input, _ctx ->
      IO.puts("About to use tool: #{input["toolName"]}")
      %{"permissionDecision" => "allow"}
    end,
    on_post_tool_use: fn input, _ctx ->
      IO.puts("Tool completed: #{input["toolName"]}")
      nil
    end
  }
}
```

## Session Management

```elixir
# List all sessions
{:ok, sessions} = Client.list_sessions(client)

# Resume a previous session
{:ok, session} = Client.resume_session(client, session_id)

# Delete a session
:ok = Client.delete_session(client, session_id)

# Get the last session ID
{:ok, last_id} = Client.get_last_session_id(client)

# List available models
{:ok, models} = Client.list_models(client)
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```elixir
{:ok, client} = CopilotClient.start_link(session_idle_timeout_seconds: 300)
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```elixir
{:ok, client} = CopilotClient.start_link(
  session_fs: %{
    initial_cwd: "/repo",
    session_state_path: "/tmp/state",
    conventions: "posix"
  }
)
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```elixir
{:ok, meta} = CopilotClient.get_session_metadata(client, "session-123")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```elixir
{:ok, session} = CopilotClient.create_session(client,
  skill_directories: ["./skills"],
  disabled_skills: ["test-skill"],
  include_sub_agent_streaming_events: true
)
```

- `skill_directories` - directories to scan for skill definitions
- `disabled_skills` - skills to exclude from the session
- `include_sub_agent_streaming_events` - receive streaming events from sub-agents

## Custom Providers (BYOK)

Use your own API endpoint:

```elixir
alias Copilot.Types.ProviderConfig

config = %SessionConfig{
  provider: %ProviderConfig{
    type: "openai",
    base_url: "http://localhost:11434/v1",
    api_key: "ollama"
  },
  model: "llama3"
}
```

## Recent Features (v2.4–v2.5)

Fields added in the v2.4 and v2.5 upstream syncs. These are `%SessionConfig{}`,
`%CopilotClientOptions{}`, or `%MessageOptions{}` fields (all under
`Copilot.Types`).

**v2.5.0**

- Reasoning effort — `reasoning_effort`
- Tool search — `tool_search`
- Session rewind — `rewind_enabled`
- Additional directories — `additional_directories`
- Disabled MCP servers — `disabled_mcp_servers`
- GitHub MCP tool config — `github_mcp_tool_config`
- Canvas provider — `canvas_provider`
- Custom agents local-only — `custom_agents_local_only`
- Experimental mode — `experimental_mode`
- Content exclusion — `content_exclusion`
- User-prompt-transformed hook — `SessionHooks.on_user_prompt_transformed`
- Permission decision context — `PermissionRequestResult.decision_context`
- Agent-factory args schema — `CustomAgentConfig.args_schema`
- Built-in plugin directories — `CopilotClientOptions.builtin_plugin_directories`
- In-process FFI transport — `CopilotClientOptions.in_process`

**v2.4.0**

- BYOK bearer-token provider — `CopilotClientOptions.bearer_token_provider`
- MCP OAuth token handler — `on_mcp_auth_request`
- HTTP request handler — `CopilotClientOptions.request_handler`
- Session citations — `enable_citations`
- Excluded built-in agents — `excluded_builtin_agents`
- Session spending limits — `Copilot.Types.SessionLimitsConfig` (`max_ai_credits`)
- Session memory — `Copilot.Types.MemoryConfiguration`
- OTLP protocol — `otlp_protocol`
- WebSocket responses — `enable_web_socket_responses`
- Experiment assignments — `exp_assignments`
- Tool defer loading — `Copilot.Types.ToolDefer` (`auto/0`, `never/0`)
- System-message sections — `SystemMessageSection.preamble/0`, `SystemMessageSection.preserve/0`
- Post-tool-use / pre-MCP hooks — `on_post_tool_use`, `on_pre_mcp_tool_call`
- Message agent mode / display prompt — `MessageOptions.agent_mode`, `MessageOptions.display_prompt`
- GitHub attachment variants — `GitHubAttachment.github_commit/0`, `GitHubAttachment.github_repository/0`

Reasoning effort, content exclusion, extra directories, and rewind:

```elixir
{:ok, session} = Client.create_session(client, %SessionConfig{
  reasoning_effort: "high",
  content_exclusion: true,
  additional_directories: ["../shared", "../docs"],
  rewind_enabled: true,
  disabled_mcp_servers: ["playwright"]
})
```

Tool search and experimental mode:

```elixir
config = %SessionConfig{
  tool_search: %{"enabled" => true},
  experimental_mode: true,
  custom_agents_local_only: true
}
```

Session spending limits, memory, and citations:

```elixir
alias Copilot.Types.{SessionConfig, SessionLimitsConfig, MemoryConfiguration}

config = %SessionConfig{
  session_limits: %SessionLimitsConfig{max_ai_credits: 5.0},
  memory: %MemoryConfiguration{enabled: true},
  enable_citations: true,
  otlp_protocol: "http/protobuf"
}
```

BYOK bearer-token provider and in-process transport (client options):

```elixir
{:ok, client} = Client.start_link(%CopilotClientOptions{
  bearer_token_provider: fn _args -> "ghs_ephemeral_token" end,
  builtin_plugin_directories: ["./plugins"],
  in_process: false
})
```

## Connection

`Copilot.Client` starts the CLI with `--headless --no-auto-update --log-level <level> --stdio` after any `cli_args`, passing one argument per item, so paths with spaces work. `Copilot.JsonRpcClient` opens the CLI port itself and owns it, so it receives everything the CLI writes to stdout, from the first byte. The port runs in stream mode with the Content-Length framing of vscode-jsonrpc: a frame split across reads, or several frames in one read, are both handled. The CLI's stderr is not read (it goes to the stderr of the Erlang VM), so log output never mixes with JSON-RPC frames.

* If the CLI cannot be started, `Client.start/1` returns `{:error, "Failed to start the Copilot CLI (<path>): <reason>"}` and the client keeps running.
* If the CLI exits, pending requests return `{:error, %{code: -1, message: "CLI process exited"}}` and the client state becomes `:disconnected`. With `auto_start: true` (the default) the next call starts a new CLI; otherwise requests return `{:error, %{code: -1, message: "JSON-RPC client is not running"}}` until `Client.start/1` is called again.
* `Copilot.JsonRpcClient.request/4` returns `{:error, :timeout}` when no response arrives within its timeout.

`test/copilot/cli_e2e_test.exs` runs this connection against the real CLI and the shared replaying proxy in CI.

## Protocol Version

The SDK speaks SDK protocol **4**, which requires Copilot CLI 1.0.113 or newer. A server that reports another version (or none) fails `Client.start/1` with `{:error, "SDK protocol version mismatch: SDK supports version 4, but server reports version N. Please update your SDK or server to ensure compatibility."}`.

## Running the Example

```bash
cd elixir
mix deps.get
mix run examples/basic_example.exs
```

## License

MIT - See [LICENSE](../LICENSE) for details.
