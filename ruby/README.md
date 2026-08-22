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
| `on(event_type = nil, &block)` | Subscribe to lifecycle events |

### `Copilot::CopilotSession`

| Method | Description |
|--------|-------------|
| `send(prompt:, attachments:, mode:)` | Send a message |
| `send_and_wait(prompt:, timeout:)` | Send and wait for idle |
| `on(event_type = nil, &block)` | Subscribe to session events |
| `get_messages` | Get conversation history |
| `destroy` | Destroy the session |
| `abort` | Abort current processing |

### Key Types

- `Copilot::Tool` - Tool definition with handler
- `Copilot::ToolResult` - Structured tool result
- `Copilot::SessionEvent` - Session event with type and data
- `Copilot::ModelInfo` - Model metadata
- `Copilot::ProviderConfig` - Custom API provider config
- `Copilot::SessionHooks` - Hook handlers
- `Copilot::InfiniteSessionConfig` - Infinite session settings

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
