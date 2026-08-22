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

### CopilotSession

| Method | Description |
|--------|-------------|
| `#send(options)` | Send a message |
| `#send_and_wait(options)` | Send and wait for the final response |
| `#on(&block)` | Subscribe to all events |
| `#on(type, &block)` | Subscribe to a specific event type |
| `#off(id)` | Unsubscribe a handler |
| `#add_tool(defn, &handler)` | Register a tool |
| `#define_tool(name, &block)` | Register a tool using the builder DSL |
| `#on_permission(&block)` | Set the permission handler |
| `#destroy` | Destroy the session |

### Session Events

| Event Type | Description |
|------------|-------------|
| `assistant.message` | Final assistant message |
| `assistant.message_delta` | Streaming text delta |
| `assistant.reasoning` | Final reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning delta |
| `turn.end` | Turn completed |
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
