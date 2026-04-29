# Copilot CLI SDK for Julia

A Julia SDK for programmatic access to the GitHub Copilot CLI.

> **Note:** This SDK is in public preview and may change in breaking ways.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

Add the package from the repository root:

```julia
using Pkg
Pkg.develop(path="julia")
```

Or add the dependency directly in your `Project.toml`:

```toml
[deps]
CopilotSDK = "c7e3a1d0-5f2b-4e8a-9c1d-3b6f7a8e9d0c"
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
| `get_status(client)` | Query server version and auth status |
| `get_models(client)` | List available models |
| `list_sessions(client)` | List known sessions on the server |

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
        if req.tool_name in ("read_file", "search")
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
