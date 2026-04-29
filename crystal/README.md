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
