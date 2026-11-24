# Copilot Supercharged SDK for Crystal

A Crystal client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

Add to your `shard.yml`:

```yaml
dependencies:
  copilot_sdk:
    path: ../crystal
```

Then run:

```sh
shards install
```

## Quick Start

```crystal
require "copilot_sdk"

# Create and start a client
client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_path: "/usr/local/bin/copilot-cli")
)
client.start

# Create a session
session = client.create_session(
  CopilotSDK::SessionConfig.new(model: "gpt-4")
)

# Subscribe to events
session.on do |event|
  if content = event.assistant_message_content
    puts "Assistant: #{content}"
  end
end

# Send a message and wait for the response
result = session.send_and_wait(
  CopilotSDK::MessageOptions.new(prompt: "What is 2 + 2?")
)
puts result

# Clean up
session.destroy
client.stop
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods below call
server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a
JSON-RPC method-not-found error, raised as `CopilotSDK::JsonRpcError` (`method_not_found?` is
true) like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The
secret value is never logged. Each method takes an optional `config_dir`.

```crystal
result = client.mcp_secrets_save("my-server-token", ENV["MY_SERVER_TOKEN"]) # mcp.secrets.save
puts result.used_keychain ? "Stored in the OS keychain" : "Stored in the file backend"

client.mcp_secrets_delete("my-server-token")  # mcp.secrets.delete
client.mcp_secrets_delete_server("my-server") # mcp.secrets.deleteServer: every credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`PermissionsExtensionApproval`: `extension_name`, `plugin_name`, `capabilities`, `granted_at`,
`installation_key`, `installation_label`). `extension_approvals_list` and
`extension_approvals_revoke` take an optional `session_id` to use an attached session's
configuration profile.

```crystal
# Persist a pending extension permission request before answering it normally.
grant = client.extension_approvals_grant(session.session_id, request_id)
if grant.granted && (approval = grant.approval)
  puts "Approved #{approval.extension_name}"
end

client.extension_approvals_list.approvals.each do |approval|
  client.extension_approvals_revoke(approval.extension_name, approval.installation_key)
end
```

### Ask-user image attachments

`SessionConfig#on_user_input_request` (and `ResumeSessionConfig#on_user_input_request`) answers
the agent's ask_user questions (`userInput.request`; the client sends
`interaction.requestUserInput`). When the runtime accepts images in the answer, the
`UserInputRequest` carries `image_attachment_constraints` (`allowed_mime_types`,
`max_bytes_per_image`, `max_count`); it is `nil` otherwise. The request also exposes
`request_id` and `meta` (the opaque `_meta` object) when sent. Answer with
`UserInputResponse.new(answer, was_freeform, attachments)`, where `attachments` is an array of
`UserInputImageAttachment` (base64 `data`, `mime_type`, optional `description` and
`display_name`; the wire `type` is always `"image"`) and is sent only when set. Image-only
answers use an empty `answer` with `was_freeform` true; the runtime enforces the limits.

```crystal
require "base64"

session = client.create_session(CopilotSDK::SessionConfig.new(
  on_user_input_request: ->(request : CopilotSDK::UserInputRequest) {
    constraints = request.image_attachment_constraints
    if constraints && constraints.allowed_mime_types.includes?("image/png")
      image = CopilotSDK::UserInputImageAttachment.new(
        data: Base64.strict_encode(File.read("screenshot.png")),
        mime_type: "image/png", display_name: "screenshot.png")
      CopilotSDK::UserInputResponse.new("", true, [image])
    else
      CopilotSDK::UserInputResponse.new("No screenshot available", true)
    end
  },
))
```

## What's New in v3.0.0

SDK protocol 4 support (the upstream SDK 2.0 wire format). **Requires Copilot CLI 1.0.113 or
newer**: `start` rejects older CLIs with
`SDK protocol version mismatch: SDK supports version 4, but server reports version N.`

Existing options keep working. You still pass the flat `SessionConfig` / `ResumeSessionConfig`
options; right before `session.create` / `session.resume` the client groups them into the
protocol 4 payload (`streaming` becomes `events.streaming`, `skill_directories` becomes
`skills.directories`, and so on). A legacy `provider` is sent as a `"default"` named provider
with `inference.defaultProvider = "default"` and Copilot API models disabled
(`github.capi.enabled = false`). A plain-string `system_message` is sent as
`{mode: "append", content: ...}`, `destroy` now sends `session.detach` (falling back to
`session.destroy`), messages go to `session.send`, events arrive through `session.event`, and
server requests are answered in their own fibers.

```crystal
require "copilot_sdk"

client = CopilotSDK::CopilotClient.new(CopilotSDK::CopilotClientOptions.new(
  # Client info: sent as `clientInfo` on the `connect` handshake.
  client_info: CopilotSDK::ClientInfo.new(editor_name: "acme-portal", editor_version: "2.4.0"),
  # Client mode: ClientMode::CODING_AGENT (default) or ClientMode::EMPTY.
  mode: CopilotSDK::ClientMode::CODING_AGENT,
  # Network callbacks: `network.setProvider` is called after connecting.
  network: CopilotSDK::NetworkConfig.new(
    resolve_proxy: ->(url : String) { url.includes?("corp.example") ? "PROXY proxy.corp.example:8080" : nil },
  ),
))
client.start

hooks = CopilotSDK::SessionHooks.new(
  on_subagent_start: ->(input : CopilotSDK::SubagentStartHookInput, session_id : String) {
    CopilotSDK::SubagentStartHookOutput.new(additional_context: "Focus for #{input.agent_name}").as(CopilotSDK::SubagentStartHookOutput?)
  },
)

session = client.create_session(CopilotSDK::SessionConfig.new(
  # Named BYOK providers: select a registered model with "<provider>/<id>".
  model: "local/qwen3",
  providers: [CopilotSDK::NamedProviderConfig.new("local", "http://localhost:11434/v1",
    type: "openai", api_key_command: "op read op://dev/llm/api-key")],
  models: [CopilotSDK::ProviderModelConfig.new("local", "qwen3", max_output_tokens: 8192_i64)],
  default_provider: "local",
  capi: CopilotSDK::CapiSessionOptions.new(enabled: false),
  # Idempotent tools: `idempotent` is sent in tools.definitions[].
  tools: [CopilotSDK::ToolDefinition.new("lookup_ticket", "Look up a ticket", idempotent: true)],
  # Max output tokens override (`limits.max_output_tokens`).
  model_capabilities: CopilotSDK::ModelCapabilitiesOverride.new(
    limits: CopilotSDK::ModelLimitsOverride.new(max_output_tokens: 8192_i64)).to_h,
  # Memory store: answers `memoryStore.store` (sends state.hasMemoryStore).
  memory_config: CopilotSDK::MemoryConfiguration.new(
    store_memory: ->(memory : CopilotSDK::StoredMemory) { puts "remember #{memory.fact}"; nil }),
  working_directory: "/repo",
  name: "ticket-triage",
  ask_user_variant: CopilotSDK::AskUserVariant::ELICITATION,
  sandbox_config: CopilotSDK::SandboxConfig.new(enabled: true, allow_bypass: true),
  feature_flags: {"someRuntimeFlag" => true},
  allowed_models: ["local/qwen3", "gpt-5"],
  auth_client_id_metadata_url: "https://example.com/oauth/client.json",
  agent_directories: ["./.github/agents"],
  hook_directories: ["./.github/hooks"],
  trust_working_directory: CopilotSDK::WorkingDirectoryTrust::SESSION,
  image_generation: CopilotSDK::ImageGenerationConfig.new(enabled: true, save_to_disk: true),
  autopilot: CopilotSDK::AutopilotOptions.new(continuation_enabled: true, max_continues: 5),
  # Subagent lifecycle hooks (`subagentStart` / `subagentStop`).
  hooks: hooks,
  # Shell authentication: answers `shellAuthentication.request` prompts.
  on_shell_authentication_request: ->(request : CopilotSDK::ShellAuthenticationRequest) {
    request.input_kind == "confirm" ? CopilotSDK::ShellAuthenticationResponse.accept("yes") : CopilotSDK::ShellAuthenticationResponse.cancel
  },
))

# Shell output events (`tool.shell_output`).
session.on_shell_output { |chunk| print chunk.text; nil }

# Client turn ID, message source, and structured output.
answer = session.send_and_wait(CopilotSDK::MessageOptions.new(
  prompt: "Summarize open tickets",
  source: CopilotSDK::MessageSource.agent("triage"),
  response_schema: JSON.parse(%({"type":"object","properties":{"count":{"type":"integer"}}})),
  client_turn_id: "turn-42",
))

# Permission mode refusal reason.
result = session.set_permission_mode("allow-all")
if !result.success && result.reason == CopilotSDK::PermissionModeRefusalReason::MANAGED_POLICY
  puts "Managed policy keeps the current permission mode"
end

session.detach # session.detach: the on-disk state is kept

begin
  resumed = client.resume_session(CopilotSDK::ResumeSessionConfig.new(
    session.session_id,
    suppress_resume_event: true,
    allow_transcript_recovery: true,
    clear_default_provider: true,
  ))
  resumed.detach
rescue ex : CopilotSDK::CopilotError
  puts ex.message
end

client.clear_managed_settings_cache # managedSettings.clearCache
client.stop
```

GitHub token provider (answers `gitHubToken.getToken`; mutually exclusive with `github_token`),
Auto routing tier (the session model must be `auto`), and the managed-settings Policy Helper:

```crystal
session = client.create_session(CopilotSDK::SessionConfig.new(
  model: "auto",
  github_token_provider: ->(request : CopilotSDK::GitHubTokenRequest) {
    CopilotSDK::GitHubTokenResult.token(access_token: mint_token(request.host), expires_in: 7200)
  },
))
session.switch_auto_tier(CopilotSDK::AutoTier::EFFICIENCY) # session.model.switchAutoTier
session.recheck_policy_helper # session.managedSettings.recheckPolicyHelper
```

A Policy Helper failure during `create_session` / `resume_session` is available from the raised
`CopilotSDK::JsonRpcError` (`error.data.policyHelperFailure`):

```crystal
begin
  client.create_session(CopilotSDK::SessionConfig.new)
rescue ex : CopilotSDK::JsonRpcError
  if failure = ex.policy_helper_failure
    puts "#{failure.category}: #{failure.message}"
  end
end
```

Empty client mode: every session must set `available_tools` (`create_session` raises
otherwise), and the empty-mode defaults (no experimental mode, telemetry, persistent MCP tokens,
embeddings, cross-session store, memory, on-demand instructions, repository hooks, or host git
operations) are applied under your own values:

```crystal
minimal = CopilotSDK::CopilotClient.new(CopilotSDK::CopilotClientOptions.new(
  mode: CopilotSDK::ClientMode::EMPTY))
session = minimal.create_session(CopilotSDK::SessionConfig.new(available_tools: ["view", "grep"]))
```

New in this release:

- **Protocol 4 grouped payloads** (`CopilotSDK.group_session_params`), with
  `ResumeSessionConfig#suppress_resume_event` (alias `disable_resume`) and
  `allow_transcript_recovery` kept top level.
- **Named BYOK providers**: `providers`, `models`, `default_provider` (or
  `clear_default_provider: true` on resume), `capi` (which also carries the legacy
  `enable_web_socket_responses` option); legacy `provider` (`ProviderConfig`, including
  `api_key_command`).
- **Client mode**, **client info**, **network callbacks** (`NetworkConfig`), **shell
  authentication** (cancelled prompts resolve to cancel), **memory store**
  (`MemoryConfiguration#store_memory`), **shell output events**
  (`SessionEventTypes::TOOL_SHELL_OUTPUT`, `ToolShellOutputData`, `on_shell_output`),
  **idempotent tools** (`ToolDefinition#idempotent`, `ToolBuilder#idempotent`), **client turn
  ID**, **policy helper failure** (`JsonRpcError#policy_helper_failure`, `PolicyHelperFailure`),
  and **permission mode refusal reason** (`PermissionModeSetResult#reason`,
  `PermissionModeRefusalReason::MANAGED_POLICY`).
- **v2.6.0 parity**: `switch_auto_tier`, `ask_user_variant`, `github_token_provider`,
  `SandboxConfig#allow_bypass`, `feature_flags`, `detach`, `MessageSource`,
  `clear_managed_settings_cache`, `recheck_policy_helper`, `allowed_models`,
  `auth_client_id_metadata_url`, `ModelLimitsOverride#max_output_tokens`
  (`max_output_tokens`), `response_schema`, `agent_directories` / `hook_directories`,
  `trust_working_directory` (`"session"` or `"user"`, create only),
  `allow_transcript_recovery`, `ImageGenerationConfig#save_to_disk`,
  `AutopilotOptions#max_continues`, and `SessionHooks` with `on_subagent_start` /
  `on_subagent_stop` (other hooks via `SessionHooks#on(HookType, &block)`).
- `experimental_mode` is now sent as `experiments.enableExperimentalMode`, and
  `available_tools` is a new `SessionConfig` option.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is an
  experimental upstream surface that depends on the runtime's in-process hosting library; it
  is not available in this SDK.
- In-process native runtime hosting is not available: the client spawns the CLI over stdio or
  connects to `cli_url`. The `in_process` option is accepted but has no effect.
- Tool calls, permission prompts and elicitation are not yet wired to the protocol 4 server
  requests (`tool.call`, `permission.request`): tools registered on the session are sent with
  each message rather than at session creation, and `elicitation_handler`,
  `mcp_auth_handler`, `commands`, `instructions` (a plain string, which collides with the
  protocol 4 `instructions` group), `agent_mode`, `response_format` and `request_headers` on
  `SessionConfig` are passed through unchanged as top-level keys.
- `github_token` on `CopilotClientOptions` is not yet passed to the spawned CLI; use the
  session-level `github_token` or `github_token_provider`.

## Connecting to an External Server

```crystal
client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(cli_url: "localhost:3000")
)
client.start
```

## Tools

Register tools that the Copilot assistant can call:

```crystal
session.define_tool("get_weather") do |t|
  t.description = "Get the current weather for a city"
  t.parameter("city", "string", "City name", required: true)
  t.handler do |args, invocation|
    city = args["city"].as_s
    JSON::Any.new("Sunny, 72F in #{city}")
  end
end
```

## Streaming Events

Subscribe to specific event types for streaming responses:

```crystal
session.on("assistant.message_delta") do |event|
  if delta = event.delta_text
    print delta
  end
end

session.on("assistant.message") do |event|
  puts # newline after streaming
end
```

## Permissions

Handle permission requests from the CLI:

```crystal
# Approve all
session.on_permission do |request, session_id|
  CopilotSDK::PermissionRequestResult.new(allowed: true)
end

# Or use the built-in helpers
session.on_permission do |request, session_id|
  CopilotSDK.approve_all_permissions(request, session_id)
end
```

## Architecture

```
Your Application
       |
   CopilotClient (Crystal)
       | (JSON-RPC 2.0 over stdio or TCP)
   Copilot CLI Server
       |
   LLM Provider
```

The SDK uses Crystal's built-in concurrency model:
- **Fibers** for non-blocking I/O on the JSON-RPC read loop
- **Channels** for request/response matching with timeouts
- **Mutex** for thread-safe access to shared state
- **JSON::Serializable** for type-safe serialization

## API Reference

### CopilotClient

| Method | Description |
|--------|-------------|
| `#start` | Connect to the CLI server |
| `#stop` | Disconnect and clean up |
| `#create_session(config)` | Create a new conversation session |
| `#resume_session(config)` | Resume an existing session |
| `#get_status` | Get server status |
| `#get_auth_status` | Get authentication status |
| `#list_models` | List available models |
| `#list_sessions` | List active sessions |
| `#clear_managed_settings_cache` | Clear cached managed settings (`managedSettings.clearCache`) |
| `#mcp_secrets_save(secret_id, value, config_dir = nil)` | Save an MCP credential (`mcp.secrets.save`) |
| `#mcp_secrets_delete(secret_id, config_dir = nil)` | Delete an MCP credential (`mcp.secrets.delete`) |
| `#mcp_secrets_delete_server(server_name, config_dir = nil)` | Delete a server's MCP credentials (`mcp.secrets.deleteServer`) |
| `#extension_approvals_grant(session_id, request_id)` | Persist a pending plugin extension approval (`permissions.extensionApprovals.grant`) |
| `#extension_approvals_list(session_id = nil)` | List plugin extension approvals (`permissions.extensionApprovals.list`) |
| `#extension_approvals_revoke(extension_name, installation_key, session_id = nil)` | Revoke a plugin extension approval (`permissions.extensionApprovals.revoke`) |

### CopilotSession

| Method | Description |
|--------|-------------|
| `#send(options)` | Send a message (`session.send`) |
| `#send_and_wait(options)` | Send and wait for the final response |
| `#on(&block)` | Subscribe to all events |
| `#on(type, &block)` | Subscribe to a specific event type |
| `#on_shell_output(&block)` | Subscribe to `tool.shell_output` events |
| `#off(id)` | Unsubscribe a handler |
| `#add_tool(defn, &handler)` | Register a tool |
| `#define_tool(name, &block)` | Register a tool using the builder DSL |
| `#on_permission(&block)` | Set the permission handler |
| `#switch_auto_tier(tier)` | Change the Auto routing tier (`session.model.switchAutoTier`) |
| `#recheck_policy_helper` | Re-run the Policy Helper (`session.managedSettings.recheckPolicyHelper`) |
| `#set_permission_mode(mode)` | Set the permission mode (`session.permissions.setMode`) |
| `#detach` | Release the session, keeping its state (`session.detach`) |
| `#destroy` | Best-effort `detach` |

### Session Events

| Event Type | Description |
|------------|-------------|
| `assistant.message` | Final assistant message |
| `assistant.message_delta` | Streaming text delta |
| `assistant.reasoning` | Final reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning delta |
| `session.idle` | Turn completed (`send_and_wait` returns) |
| `tool.shell_output` | Live shell output of a running tool |
| `turn.end` | Turn completed (legacy) |
| `session.compaction_start` | Session compaction began |
| `session.compaction_complete` | Session compaction finished |

## Recent Features (v2.4–v2.5)

Recent upstream syncs added session and client options for parity with `@github/copilot-sdk`. Properties live on `SessionConfig`, `MessageOptions`, or `CopilotClientOptions` unless noted.

**v2.5 wave (`SessionConfig` unless noted):**

- **Reasoning effort** — `reasoning_effort` (e.g. `"minimal"`, `"low"`, `"medium"`, `"high"`, `"max"`).
- **Tool search** — `tool_search` (`JSON::Any`); discover tools on demand.
- **Session rewind** — `rewind_enabled`; roll the conversation back to an earlier turn.
- **Content exclusion** — `content_exclusion`; honor rules that hide files from the agent.
- **Additional directories** — `additional_directories`.
- **Disabled MCP servers** — `disabled_mcp_servers`.
- **GitHub MCP tool config** — `github_mcp_tool_config`.
- **Canvas provider** — `canvas_provider`.
- **Custom agents local-only** — `custom_agents_local_only`.
- **Experimental mode** — `experimental_mode`.
- **Permission decision context** — `PermissionRequestResult#decision_context`.
- **Agent factory args** — `AgentFactoryOptions#args_schema`.
- **Built-in plugin directories** — `CopilotClientOptions#builtin_plugin_directories`.
- **In-process FFI transport** — `CopilotClientOptions#in_process`.

**v2.4 wave:**

- **BYOK bearer token provider** — `CopilotClientOptions#bearer_token_provider` (`BearerTokenProvider`).
- **MCP OAuth token handler** — `CopilotClientOptions#on_mcp_auth_request` (`McpAuthHandler`).
- **Session citations** — `enable_citations`.
- **Excluded built-in agents** — `excluded_builtin_agents`.
- **Session spending limits** — `session_limits` (`SessionLimits#max_ai_credits`).
- **OTLP telemetry protocol** — `otlp_protocol`.
- **WebSocket transport** — `enable_web_socket_responses`.
- **Session memory** — `memory_config` (`MemoryConfiguration`).
- **Experiment assignments** — `exp_assignments`.
- **Message agent mode / display prompt** — `MessageOptions#agent_mode`, `MessageOptions#display_prompt`.
- **Tool defer loading** — `ToolDeferMode::Auto` / `ToolDeferMode::Never`.
- **System-message sections** — `SystemMessageSection::Preamble`, `SystemMessageSection::Preserve`.
- **Hook identifiers** — `HookType::PostToolUse`, `HookType::PreMcpToolCall`, `HookType::UserPromptTransformed`.
- **GitHub attachments** — `GitHubAttachmentType::GitHubCommit`, `GitHubAttachmentType::GitHubRepository`.

### Reasoning effort

```crystal
session = client.create_session(
  CopilotSDK::SessionConfig.new(
    model: "gpt-4o",
    reasoning_effort: "high",
  )
)
```

### Session rewind & additional directories

```crystal
session = client.create_session(
  CopilotSDK::SessionConfig.new(
    rewind_enabled: true,
    additional_directories: ["/repo/docs", "/repo/vendor"],
  )
)
```

### Content exclusion

```crystal
session = client.create_session(
  CopilotSDK::SessionConfig.new(content_exclusion: true)
)
```

### BYOK bearer token provider

```crystal
client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(
    bearer_token_provider: ->(args : JSON::Any) { fetch_token(args) },
  )
)
```

### Spending limits & citations

```crystal
session = client.create_session(
  CopilotSDK::SessionConfig.new(
    session_limits: CopilotSDK::SessionLimits.new(max_ai_credits: 5.0),
    enable_citations: true,
  )
)
```

### In-process transport

```crystal
client = CopilotSDK::CopilotClient.new(
  CopilotSDK::CopilotClientOptions.new(
    in_process: true,
    builtin_plugin_directories: ["./plugins"],
  )
)
```

## Development

```sh
# Install dependencies
shards install

# Run tests
crystal spec

# Format code
crystal tool format

# Lint with ameba
bin/ameba
```

## Cookbook

See the [cookbook/](cookbook/) directory for recipes:

- [Advanced Features](cookbook/advanced-features.md) - Agent mode, model selection, streaming
- [Error Handling](cookbook/error-handling.md) - Retry logic, timeouts, graceful shutdown
- [Multiple Sessions](cookbook/multiple-sessions.md) - Running parallel conversations
- [Persisting Sessions](cookbook/persisting-sessions.md) - Session state across restarts
- [Tools and Skills](cookbook/tools-and-skills.md) - Defining and composing tools
- [Fibers and Channels](cookbook/fibers-and-channels.md) - Crystal concurrency patterns

## License

MIT
