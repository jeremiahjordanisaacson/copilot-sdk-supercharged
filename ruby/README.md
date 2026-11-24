# Copilot Supercharged Ruby SDK

Ruby SDK for the [GitHub Copilot CLI](https://github.com/github/copilot). Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio or TCP.

## Requirements

- Ruby >= 3.1
- GitHub Copilot CLI installed and authenticated

## Installation

Add to your Gemfile:

```ruby
gem "copilot-sdk-supercharged"
```

Or install directly:

```bash
gem install copilot-sdk-supercharged
```

## Quick Start

```ruby
require "copilot"

# Create a client (spawns the CLI server process)
client = Copilot::CopilotClient.new(
  cli_path: "/usr/local/bin/copilot",
  log_level: "info"
)
client.start

# Create a session
session = client.create_session(model: "gpt-4")

# Subscribe to events
unsub = session.on do |event|
  if event.type == Copilot::SessionEventType::ASSISTANT_MESSAGE
    puts event.data["content"]
  end
end

# Send a message and wait for the response
response = session.send_and_wait(prompt: "Hello, world!")

# Clean up
unsub.call
session.destroy
client.stop
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client methods below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, raised as `Copilot::JsonRpcError` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret value is never logged.

```ruby
result = client.mcp_secrets_save("my-server-token", ENV["MY_SERVER_TOKEN"]) # optional config_dir:
puts(result.used_keychain ? "Stored in the OS keychain" : "Stored in the file backend")

client.mcp_secrets_delete("my-server-token")
client.mcp_secrets_delete_server("my-server") # every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions. `extension_approvals_list` and `extension_approvals_revoke` take an optional `session_id:` to use an attached session's configuration profile.

```ruby
# Persist a pending extension permission request before answering it normally
grant = client.extension_approvals_grant(session.session_id, request_id)
puts "Approved #{grant.approval.extension_name}" if grant.granted

client.extension_approvals_list.approvals.each do |approval|
  # extension_name, plugin_name, capabilities, granted_at, installation_key, installation_label
  client.extension_approvals_revoke(approval.extension_name, approval.installation_key)
end
```

### Ask-user image attachments

When the runtime accepts images in an answer, the user input request carries `image_attachment_constraints` (`allowed_mime_types`, `max_bytes_per_image`, `max_count`); it is `nil` otherwise. The request also exposes `request_id` and `meta` (the opaque `_meta` object) when sent. Answer with `attachments:`, an array of `Copilot::UserInputImageAttachment` (base64 `data`, `mime_type`, optional `description` and `display_name`; the wire `type` is always `"image"`). Image-only answers use `answer: ""` and `was_freeform: true`; the runtime enforces the limits.

```ruby
session = client.create_session(
  on_user_input_request: ->(request, _ctx) {
    constraints = request.image_attachment_constraints
    if constraints&.allowed_mime_types&.include?("image/png")
      image = Copilot::UserInputImageAttachment.new(
        data: [File.binread("screenshot.png")].pack("m0"), # base64
        mime_type: "image/png",
        display_name: "screenshot.png"
      )
      Copilot::UserInputResponse.new(answer: "", was_freeform: true, attachments: [image])
    else
      Copilot::UserInputResponse.new(answer: "No screenshot available", was_freeform: true)
    end
  }
)
```

## What's New in v3.0.0

This release speaks **SDK protocol v4**, the protocol of the Microsoft Copilot SDK 2.0 runtime. It requires **Copilot CLI 1.0.113 or newer**; older CLIs are rejected at `start` with an `SDK protocol version mismatch` error.

Existing options keep working: you keep passing the same flat keywords to `create_session` / `resume_session`, and the SDK translates them into the grouped v4 wire payload (for example `streaming:` becomes `events.streaming`, `on_permission_request:` becomes `permissions.request`). A legacy single `provider:` (BYOK) is translated into a named `"default"` provider with Copilot API models disabled, matching v1 behavior.

### Named BYOK providers

Register several provider connections and model definitions, then select a model with `"provider/id"`:

```ruby
gateway = Copilot::NamedProviderConfig.new(
  name: "gateway", type: "openai",
  base_url: "https://models.example.com/v1",
  api_key_command: "print-model-key"
)

session = client.create_session(
  model: "gateway/chat",
  providers: [gateway],
  models: [
    Copilot::ProviderModelConfig.new(provider: "gateway", id: "chat", model_id: "gpt-4o", max_output_tokens: 4096)
  ],
  capi: Copilot::CapiConfig.new(enabled: false) # BYOK-only catalog
)

# Route unqualified model names through a connection, and clear it again on resume
session = client.create_session(model: "deployment", providers: [gateway], default_provider: "gateway",
                                capi: { enabled: false })
client.resume_session(session.session_id, model: "gpt-5.4", capi: { enabled: true }, default_provider: nil)
```

### Idempotent tools

```ruby
lookup = Copilot.define_tool(name: "lookup_order", description: "Look up an order", idempotent: true) do |args, _inv|
  "Order #{args['id']} shipped"
end
```

### Client turn IDs

```ruby
session.send_and_wait(prompt: "Summarize the diff", client_turn_id: "turn-42")
```

### Client mode

`mode: "coding-agent"` (the default) keeps today's behavior. `mode: "empty"` requires every session to set `available_tools:` and applies conservative defaults (no telemetry, in-memory OAuth and embedding storage, no cross-session store or memory, no on-demand instruction discovery, repository hooks or host git operations). Values you pass win. `"copilot-cli"` is accepted as an alias of `"coding-agent"`.

```ruby
client = Copilot::CopilotClient.new(mode: Copilot::ClientMode::EMPTY)
session = client.create_session(model: "gpt-5.4", available_tools: ["view"])
```

### Shell output events

```ruby
session.on(Copilot::SessionEventType::TOOL_SHELL_OUTPUT) do |event|
  chunk = Copilot::ShellOutputData.from_hash(event.data)
  print chunk.text # chunk.stream is "stdout", "stderr" or nil (stdout)
end
```

### Shell authentication

Answer credential prompts from shell programs (for example `git` asking for a password). Return `ShellAuthenticationResponse.accept(value)` or `.cancel`; failures and malformed answers cancel. The answer is never logged.

```ruby
session = client.create_session(
  on_shell_authentication_request: ->(request, _ctx) {
    # request.program, request.prompt, request.input_kind ("secret", "text" or "confirm"), request.command, request.cwd
    Copilot::ShellAuthenticationResponse.accept(fetch_secret(request.program))
  }
)
```

### Memory store

```ruby
session = client.create_session(
  store_memory: ->(memory, _ctx) {
    # memory is a Copilot::StoredMemory (subject, fact, citations, reason)
    MyMemoryDb.save(memory.subject, memory.fact)
  }
)
```

### Network callbacks

Resolve proxies and answer proxy authentication challenges from your application. Tokens and passwords are never logged.

```ruby
client = Copilot::CopilotClient.new(
  network: Copilot::NetworkConfig.new(
    scope: Copilot::NetworkScope::RUNTIME,
    resolve_proxy: ->(request) { request.url.start_with?("https://internal") ? "DIRECT" : "PROXY proxy.example:8080" },
    authenticate_basic: ->(request) { request.previous_rejected ? nil : { username: "svc", password: ENV["PROXY_PASSWORD"] } },
    authenticate_negotiate: ->(request) { kerberos_token_for(request.spn) }
  )
)
```

### Policy helper failures

```ruby
begin
  client.create_session(model: "gpt-5.4")
rescue Copilot::JsonRpcError => e
  if (failure = e.policy_helper_failure)
    warn "Policy helper failed (#{failure.category}, #{failure.failure_mode}): #{failure.message}"
  end
  raise
end
```

### Permission mode refusal reason

```ruby
result = session.set_permission_mode(Copilot::PermissionMode::ALLOW_ALL)
warn "Blocked by managed policy" if result.reason == Copilot::PermissionModeRefusalReason::MANAGED_POLICY
```

The v2.6.0 features (client info, Auto routing tier, ask-user variant, GitHub token provider, sandbox bypass, feature flags, session detach, message source, managed settings cache clear, policy helper recheck, model allowlist, OAuth client metadata URL, max output tokens override, structured output, agent and hook directories, working directory trust, transcript recovery, image generation config, Autopilot options, subagent lifecycle hooks and BYOK `api_key_command`) are unchanged and are sent in their v4 groups.

### Known limitations

* The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not available: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process runtime hosting is not the default connection: the Ruby SDK keeps spawning the CLI (or connecting over TCP with `cli_url`).
* A shell authentication handler runs on its own thread. When the runtime cancels the prompt, the SDK answers `cancel` immediately, but the handler thread keeps running until it returns (its answer is discarded).

## Connecting to an External Server

Instead of spawning a CLI process, connect to an already-running server:

```ruby
client = Copilot::CopilotClient.new(cli_url: "localhost:8080")
client.start
```

## Custom Tools

Register tools that the assistant can invoke:

```ruby
weather_tool = Copilot.define_tool(
  name: "get_weather",
  description: "Get weather for a city",
  parameters: {
    type: "object",
    properties: {
      city: { type: "string", description: "City name" }
    },
    required: ["city"]
  }
) do |args, invocation|
  city = args["city"]
  "It is 22 degrees and sunny in #{city}."
end

session = client.create_session(tools: [weather_tool])
```

Tool handlers can return:
- A **String** (wrapped as a successful result)
- A **Hash** (passed through if it has `textResultForLlm`, otherwise JSON-serialized)
- A **`Copilot::ToolResult`** struct for full control over result type, errors, and telemetry

## Permission Handling

Handle permission requests when the assistant wants to perform privileged operations:

```ruby
session = client.create_session(
  on_permission_request: ->(request, ctx) {
    puts "Permission requested: #{request.kind}"
    Copilot::PermissionRequestResult.new(kind: Copilot::PermissionKind::APPROVED)
  }
)
```

## User Input

Enable the `ask_user` tool so the agent can ask questions:

```ruby
session = client.create_session(
  on_user_input_request: ->(request, ctx) {
    puts "Agent asks: #{request.question}"
    print "> "
    answer = $stdin.gets.chomp
    Copilot::UserInputResponse.new(answer: answer, was_freeform: true)
  }
)
```

## Hooks

Intercept session lifecycle events with hooks:

```ruby
hooks = Copilot::SessionHooks.new(
  on_pre_tool_use: ->(input, ctx) {
    puts "About to call tool: #{input['toolName']}"
    # Return nil to proceed normally, or a hash with modifiedArgs, etc.
    nil
  },
  on_post_tool_use: ->(input, ctx) {
    puts "Tool #{input['toolName']} completed"
    nil
  },
  on_session_start: ->(input, ctx) {
    puts "Session started from: #{input['source']}"
    nil
  }
)

session = client.create_session(hooks: hooks)
```

## Event Subscriptions

### Session Events

Subscribe to all events or specific event types:

```ruby
# All events
unsub = session.on { |event| puts event.type }

# Specific event type
unsub = session.on(Copilot::SessionEventType::ASSISTANT_MESSAGE) do |event|
  puts event.data["content"]
end

# Unsubscribe
unsub.call
```

### Lifecycle Events (Client-Level)

Monitor session lifecycle changes (useful in TUI+server mode):

```ruby
unsub = client.on(Copilot::SessionLifecycleEventType::SESSION_CREATED) do |event|
  puts "Session created: #{event.session_id}"
end
```

## Session Management

```ruby
# List all sessions
sessions = client.list_sessions
sessions.each { |s| puts "#{s.session_id}: #{s.summary}" }

# Resume a session
session = client.resume_session("session-id-here")

# Delete a session
client.delete_session("session-id-here")

# Get last session ID
last_id = client.get_last_session_id
```

## Custom Providers (BYOK)

Use your own API endpoint:

```ruby
session = client.create_session(
  model: "my-model",
  provider: Copilot::ProviderConfig.new(
    type: "openai",
    base_url: "https://api.example.com/v1",
    api_key: "sk-..."
  )
)
```

## MCP Servers

Configure Model Context Protocol servers:

```ruby
session = client.create_session(
  mcp_servers: {
    "my-server" => {
      tools: ["*"],
      command: "npx",
      args: ["-y", "@example/mcp-server"],
    }
  }
)
```

## Infinite Sessions

Enable automatic context compaction for long-running sessions:

```ruby
session = client.create_session(
  infinite_sessions: Copilot::InfiniteSessionConfig.new(
    enabled: true,
    background_compaction_threshold: 0.80,
    buffer_exhaustion_threshold: 0.95
  )
)

# Access workspace path for persisted state
puts session.workspace_path
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```ruby
client = Copilot::Client.new(session_idle_timeout_seconds: 300)
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```ruby
client = Copilot::Client.new(session_fs: {
  initial_cwd: "/repo",
  session_state_path: "/tmp/state",
  conventions: "posix"
})
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```ruby
meta = client.get_session_metadata("session-123")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```ruby
session = client.create_session(
  skill_directories: ["./skills"],
  disabled_skills: ["test-skill"],
  include_sub_agent_streaming_events: true
)
```

- `skill_directories` - Paths to directories containing skill definitions
- `disabled_skills` - Skills to exclude from the session
- `include_sub_agent_streaming_events` - When `true`, receive streaming events from sub-agents

## Streaming

Enable streaming to receive incremental message chunks:

```ruby
session = client.create_session(streaming: true)

session.on(Copilot::SessionEventType::ASSISTANT_MESSAGE_DELTA) do |event|
  print event.data["deltaContent"]
end
```

## Recent Features (v2.4–v2.5)

Recent upstream syncs added a range of session-configuration options. Except where noted, each is a keyword passed to `client.create_session(...)`.

### New in v2.5

- **Reasoning effort** — `reasoning_effort:` — set the model's reasoning budget (e.g. `"low"`, `"medium"`, `"high"`).
- **Tool search** — `tool_search:` — let the agent discover tools on demand instead of pre-loading them.
- **Content exclusion** — `content_exclusion:` — honor content-exclusion rules that hide files from the agent.
- **Session rewind** — `rewind_enabled:` — allow rolling a session back to an earlier snapshot; observe `Copilot::SessionEventType::SESSION_SNAPSHOT_REWIND` events.
- **Additional directories** — `additional_directories:` — extra workspace directories the agent may access.
- **Disabled MCP servers** — `disabled_mcp_servers:` — turn off specific MCP servers for the session.
- **GitHub MCP tool config** — `github_mcp_tool_config:` — configure the built-in GitHub MCP toolset.
- **Canvas provider** — `canvas_provider:` — supply a canvas rendering provider.
- **Custom agents local-only** — `custom_agents_local_only:` — restrict custom agents to local definitions.
- **Built-in plugin directories** — `builtin_plugin_directories:` — directories scanned for built-in plugins.
- **Agent-factory authoring** — `args_schema:` — author agent factories with a typed args schema.
- **Permission decision context** — `decision_context:` — extra context passed to permission decisions.
- **In-process transport** — `in_process:` — run the CLI in-process instead of as a subprocess.
- **Experimental mode** — `experimental_mode:` — opt into experimental CLI behavior.
- **MCP OAuth handler** — `on_mcp_auth_request:` — handle OAuth token requests from MCP servers.

### New in v2.4

- **Session citations** — `enable_citations:` — emit citation metadata.
- **Excluded built-in agents** — `excluded_builtin_agents:` — hide specific built-in agents.
- **Spending limits** — `session_limits:` — cap AI-credit spend with `Copilot::SessionLimitsConfig.new(max_ai_credits:)`.
- **Session memory** — `memory:` — configure persistent memory with `Copilot::MemoryConfiguration.new(enabled:)`.
- **OTLP protocol** — `otlp_protocol:` — choose the OTLP telemetry wire protocol.
- **WebSocket responses** — `enable_web_socket_responses:` — stream responses over WebSocket.
- **Experiment assignments** — `exp_assignments:` — pass experiment assignments to the runtime.
- **Tool defer loading** — `Copilot::ToolDefer` (`AUTO` / `NEVER`) — defer tool loading until first use.
- **System-message sections** — `Copilot::SystemMessageSection` (`PREAMBLE` / `PRESERVE`).
- **GitHub attachments** — `Copilot::GitHubAttachment` (`GITHUB_COMMIT`, `GITHUB_REPOSITORY`, …).
- **Post-tool-use / prompt-transformed hooks** — `on_post_tool_use`, `on_user_prompt_transformed` on `Copilot::SessionHooks`.
- **Per-message agent mode / display prompt** — `agent_mode:` and `display_prompt:` on `session.send` / `session.send_and_wait`.

### Examples

```ruby
# Reasoning effort + on-demand tool search + content exclusion
session = client.create_session(
  reasoning_effort: "high",
  tool_search: true,
  content_exclusion: true
)
```

```ruby
# Enable session rewind and observe rewind snapshots
session = client.create_session(rewind_enabled: true)
session.on(Copilot::SessionEventType::SESSION_SNAPSHOT_REWIND) do |event|
  puts "Rewound to snapshot: #{event.data}"
end
```

```ruby
# Grant the agent access to extra workspace directories
session = client.create_session(
  additional_directories: ["../shared-lib", "../docs"]
)
```

```ruby
# Cap AI-credit spend and enable persistent memory
session = client.create_session(
  session_limits: Copilot::SessionLimitsConfig.new(max_ai_credits: 500),
  memory: Copilot::MemoryConfiguration.new(enabled: true)
)
```

```ruby
# Per-message agent mode and a display-only prompt
session.send_and_wait(
  prompt: "Summarize the diff",
  agent_mode: "plan",
  display_prompt: "Summarize the staged changes"
)
```

## API Reference

### `Copilot::CopilotClient`

| Method | Description |
|--------|-------------|
| `start` | Start the CLI server and connect |
| `stop` | Graceful shutdown, returns array of errors |
| `force_stop` | Forceful shutdown |
| `create_session(**config)` | Create a new session |
| `resume_session(id, **config)` | Resume an existing session |
| `ping(message = nil)` | Ping the server |
| `get_status` | Get CLI version and protocol info |
| `get_auth_status` | Get authentication status |
| `list_models` | List available models (cached) |
| `list_sessions` | List all sessions |
| `get_last_session_id` | Get most recently updated session ID |
| `delete_session(id)` | Permanently delete a session |
| `get_foreground_session_id` | Get TUI foreground session |
| `set_foreground_session_id(id)` | Set TUI foreground session |
| `clear_managed_settings_cache` | Clear cached managed (enterprise policy) settings |
| `mcp_secrets_save(secret_id, value, config_dir: nil)` | Save an MCP credential (returns `McpSecretsSaveResult`) |
| `mcp_secrets_delete(secret_id, config_dir: nil)` | Delete an MCP credential |
| `mcp_secrets_delete_server(server_name, config_dir: nil)` | Delete the MCP credentials indexed for a server |
| `extension_approvals_grant(session_id, request_id)` | Persist a pending plugin extension approval |
| `extension_approvals_list(session_id: nil)` | List plugin extension approvals |
| `extension_approvals_revoke(extension_name, installation_key, session_id: nil)` | Revoke a plugin extension approval |
| `on(event_type = nil, &block)` | Subscribe to lifecycle events |

### `Copilot::CopilotSession`

| Method | Description |
|--------|-------------|
| `send(prompt:, attachments:, mode:, client_turn_id:)` | Send a message |
| `send_and_wait(prompt:, timeout:)` | Send and wait for idle |
| `on(event_type = nil, &block)` | Subscribe to session events |
| `get_messages` | Get conversation history |
| `set_auto_tier(tier)` | Change the Auto routing preference |
| `set_permission_mode(mode)` | Change the permission mode (returns `PermissionModeSetResult`) |
| `recheck_policy_helper` | Re-run the managed-settings policy helper |
| `destroy` | Detach from the session (state is kept for resume) |
| `abort` | Abort current processing |

### Key Types

- `Copilot::Tool` - Tool definition with handler
- `Copilot::ToolResult` - Structured tool result
- `Copilot::SessionEvent` - Session event with type and data
- `Copilot::ModelInfo` - Model metadata
- `Copilot::ProviderConfig` - Custom API provider config
- `Copilot::SessionHooks` - Hook handlers
- `Copilot::InfiniteSessionConfig` - Infinite session settings
- `Copilot::UserInputImageAttachment` - Image attached to an ask_user answer

## Threading Model

The SDK uses a background reader thread for the JSON-RPC connection. Event handlers and tool handlers are called from this reader thread. If you need to interact with non-thread-safe resources, use appropriate synchronization (Mutex, Queue, etc.).

The `send_and_wait` method uses a `Mutex` + `ConditionVariable` to block the calling thread until the session becomes idle, which is the recommended pattern for synchronous usage.

## Image Generation

Request image responses using `response_format` and `image_options`:

```ruby
response = session.send_and_wait(
  prompt: "Generate a sunset over mountains",
  response_format: "image",
  image_options: Copilot::ImageOptions.new(
    size: "1024x1024", quality: "hd", style: "natural"
  )
)
```

## License

MIT - see [LICENSE](../LICENSE) for details.
