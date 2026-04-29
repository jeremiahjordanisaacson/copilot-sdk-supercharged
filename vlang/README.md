# Copilot Supercharged SDK for V (Vlang)

A V client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

Clone the repository and reference the module locally:

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
cd copilot-sdk-supercharged/vlang
```

Then import in your V project:

```v
import copilot  // from vlang/src/
```

Or symlink into your `~/.vmodules`:

```bash
v install --git https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged vlang
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

## Architecture

```
Your V App
    |
    v
CopilotClient  (client.v)
    |-- spawns or connects to CLI process
    |-- performs JSON-RPC handshake
    |
    v
RpcTransport   (jsonrpc.v)
    |-- Content-Length framed JSON-RPC 2.0
    |-- request/response correlation via channels
    |
    v
CopilotSession (session.v)
    |-- event loop in spawned thread
    |-- tool call dispatch
    |-- streaming deltas
```

## Core Modules

| File | Purpose |
|------|---------|
| `src/client.v` | `CopilotClient` -- process management, session creation |
| `src/session.v` | `CopilotSession` -- event loop, send/receive, tool dispatch |
| `src/jsonrpc.v` | `RpcTransport` -- Content-Length framing, request correlation |
| `src/types.v` | All struct and enum definitions with `[json]` attributes |
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
- **`spawn`** -- for concurrent event loops
- **`shared` variables** -- for thread-safe handler lists
- **`[json]` attributes** -- for serialization
- **`match` expressions** -- for event type dispatch
- **`chan`** -- for request/response correlation and timeouts

## Requirements

- V compiler 0.4+ (https://vlang.io)
- Copilot CLI binary or running server
- No external dependencies beyond V stdlib
