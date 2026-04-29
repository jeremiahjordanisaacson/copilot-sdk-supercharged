# D SDK for GitHub Copilot CLI

A native D language SDK for the [GitHub Copilot CLI](https://github.com/github/copilot-sdk), providing idiomatic D bindings with UFCS, ranges, `Nullable!T`, `scope(exit)` resource management, and `@safe`/`@trusted` annotations.

## Quick Start

### Prerequisites

- **D compiler** (DMD, LDC, or GDC)
- **DUB** package manager (ships with DMD)
- **Copilot CLI** installed and on `PATH`, or specify `cliPath` / `cliUrl`

### Installation

Add to your `dub.json`:

```json
{
    "dependencies": {
        "copilot-sdk": { "path": "../dlang" }
    }
}
```

Or clone and use directly:

```bash
cd dlang
dub build
```

### Minimal Example

```d
import copilot;
import std.stdio : writeln;

void main()
{
    auto client = new CopilotClient();
    scope(exit) client.stop();

    auto session = client.createSession();

    session.addEventListener((SessionEvent ev) {
        if (ev.type == SessionEventType.assistantMessage)
        {
            auto content = ev.stringField("content");
            if (!content.isNull)
                writeln("Assistant: ", content.get);
        }
    });

    session.sendAndWait("Explain D's scope guards in two sentences.");
}
```

## Architecture

```
Your App  -->  CopilotClient  -->  JsonRpcClient  -->  Copilot CLI (stdio)
                    |
               CopilotSession
                    |
              EventListeners + ToolHandlers
```

The SDK spawns the Copilot CLI as a child process and communicates over JSON-RPC 2.0 with Content-Length framing (the Language Server Protocol transport).

## API Reference

### CopilotClient

| Method | Description |
|---|---|
| `this(CopilotClientOptions)` | Construct with options; auto-starts if `autoStart` is true |
| `start()` | Start the CLI process or connect to an external server |
| `stop()` | Graceful shutdown |
| `forceStop()` | Kill the CLI process immediately |
| `createSession(SessionConfig)` | Create a new conversation session |
| `createSession(SessionConfig, ToolBinding[])` | Create a session with pre-registered tools |
| `resumeSession(ResumeSessionConfig)` | Resume an existing session by ID |
| `ping()` | Ping the CLI server; returns `true` if reachable |
| `getStatus()` | Get CLI status and version |
| `listModels()` | List available LLM models |
| `getAuthStatus()` | Check authentication status |

### CopilotClientOptions

| Field | Type | Default | Description |
|---|---|---|---|
| `cliPath` | `string` | `"github-copilot"` | Path to CLI binary |
| `cliArgs` | `string[]` | `[]` | Extra CLI arguments |
| `cwd` | `Nullable!string` | null | Working directory |
| `port` | `Nullable!ushort` | null | TCP port (non-stdio) |
| `useStdio` | `bool` | `true` | Use stdio transport |
| `cliUrl` | `Nullable!string` | null | External server URL |
| `logLevel` | `string` | `"info"` | CLI log level |
| `autoStart` | `bool` | `true` | Auto-start on construction |
| `autoRestart` | `bool` | `false` | Restart on unexpected exit |
| `env` | `string[string]` | empty | Extra environment variables |
| `gitHubToken` | `Nullable!string` | null | GitHub auth token |
| `useLoggedInUser` | `bool` | `true` | Use logged-in GitHub user |
| `sessionFs` | `Nullable!string` | null | Session persistence path |
| `sessionIdleTimeoutSeconds` | `Nullable!uint` | null | Idle timeout |

### CopilotSession

| Method | Description |
|---|---|
| `send(message, opts)` | Send a message (fire-and-forget) |
| `sendAndWait(message, opts)` | Send and block until session is idle; returns collected events |
| `abort()` | Abort the current in-flight request |
| `addEventListener(listener)` | Register an event listener |
| `registerTool(ToolBinding)` | Register a tool handler |
| `registerTools(ToolBinding[])` | Register multiple tool handlers |
| `sessionId` | Property: the session's unique ID |
| `isIdle` | Property: whether the session is idle |

### SessionEventType

| Value | Wire String |
|---|---|
| `assistantMessage` | `assistant.message` |
| `assistantMessageDelta` | `assistant.message_delta` |
| `assistantReasoning` | `assistant.reasoning` |
| `assistantReasoningDelta` | `assistant.reasoning_delta` |
| `toolCall` | `tool.call` |
| `sessionIdle` | `session.idle` |
| `sessionError` | `session.error` |
| `sessionEnd` | `session.end` |
| `compactionStart` | `session.compaction_start` |
| `compactionComplete` | `session.compaction_complete` |

### Tool Definition

Use `defineTool` for a concise tool registration:

```d
import copilot.tools;

auto weatherTool = defineTool(
    "get_weather",
    "Get current weather for a city",
    `{
        "type": "object",
        "properties": {
            "city": { "type": "string", "description": "City name" }
        },
        "required": ["city"]
    }`,
    (string callId, JSONValue args) @safe {
        string city = args["city"].str;
        return successResult(callId, "Sunny, 22C in " ~ city);
    },
);

auto session = client.createSession(SessionConfig.init, [weatherTool]);
```

## Module Structure

| Module | Description |
|---|---|
| `copilot.client` | `CopilotClient` -- lifecycle, process management |
| `copilot.session` | `CopilotSession` -- send/receive, event dispatch |
| `copilot.types` | Structs: options, configs, events, responses |
| `copilot.jsonrpc` | JSON-RPC 2.0 client with Content-Length framing |
| `copilot.tools` | `defineTool`, `ToolBinding`, result helpers |
| `copilot` | Package re-export of all public modules |

## Testing

```bash
cd dlang
dub test
```

The test suite uses D's built-in `unittest` blocks and covers:

- Type serialization round-trips
- Tool definition and handler invocation
- Event type parsing
- JSON extraction helpers
- Error type construction
- Default option validation

## D Idioms Used

- **`scope(exit)`** for deterministic cleanup
- **`Nullable!T`** for optional fields (no null pointer surprises)
- **UFCS** (Uniform Function Call Syntax) throughout
- **`@safe` / `@trusted`** annotations for memory safety
- **Contracts** via `enforce` for precondition checks
- **`std.json`** for JSON manipulation
- **`std.process`** for subprocess management
- **Delegates** for event listeners and tool handlers
- **Module system** with `package.d` for clean imports

## Cookbook

See the [`cookbook/`](cookbook/) directory for focused recipes:

| Recipe | Description |
|---|---|
| [Error Handling](cookbook/error_handling.d) | Robust error handling with scope guards |
| [Multiple Sessions](cookbook/multiple_sessions.d) | Run parallel conversations |
| [Persisting Sessions](cookbook/persisting_sessions.d) | Save and resume sessions |
| [Tool Usage](cookbook/tool_usage.d) | Define and register custom tools |
| [Streaming](cookbook/streaming.d) | Handle streaming response deltas |
| [Advanced Config](cookbook/advanced_config.d) | Provider overrides, system messages, idle timeout |

## License

MIT
