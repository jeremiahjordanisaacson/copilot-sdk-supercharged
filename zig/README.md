# Copilot Supercharged SDK for Zig

A Zig client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Requirements

- **Zig** 0.14.0 or later
- **Copilot CLI** installed and on PATH (or specify `cli_path`)
- **GitHub authentication** (OAuth token, `gh` CLI auth, or `GITHUB_TOKEN` environment variable)

## Building

```bash
cd zig
zig build
```

Run unit tests:

```bash
zig build test
```

Run the example (requires Copilot CLI):

```bash
zig build run-example
```

## Quick Start

```zig
const std = @import("std");
const copilot = @import("copilot-sdk");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create and start a client
    var client = copilot.CopilotClient.init(allocator, .{
        // .cli_path = "/path/to/copilot-cli",
    });
    defer client.deinit();
    try client.start();

    // Create a session
    var session = try client.createSession(.{});

    // Subscribe to events
    _ = session.on(struct {
        fn handler(event: copilot.SessionEvent) void {
            if (event.assistantMessageContent()) |content| {
                std.debug.print("Assistant: {s}\n", .{content});
            }
        }
    }.handler);

    // Send a message and wait for the response
    const response = try session.sendAndWait(.{
        .prompt = "What is 2 + 2?",
    }, 30_000);

    if (response) |ev| {
        if (ev.assistantMessageContent()) |content| {
            std.debug.print("Answer: {s}\n", .{content});
        }
    }

    // Clean up
    try session.destroy();
    try client.stop();
}
```

## Custom Tools

Register tools that the Copilot agent can invoke:

```zig
const weather_tool = copilot.defineTool(
    "get_weather",
    "Get weather for a location",
    \\{"type":"object","properties":{"location":{"type":"string"}},"required":["location"]}
);

fn weatherHandler(invocation: copilot.ToolInvocation) anyerror!copilot.ToolResult {
    _ = invocation;
    return .{
        .text_result_for_llm = "{\"temp\":72,\"condition\":\"sunny\"}",
        .result_type = .success,
    };
}

try session.registerTool(weather_tool, weatherHandler);
```

## Permission Handling

Register a handler to approve or deny permission requests from the agent:

```zig
session.registerPermissionHandler(struct {
    fn handler(req: copilot.PermissionRequest, _: []const u8) anyerror!copilot.PermissionResult {
        return switch (req.kind) {
            .read => .{ .kind = .approved },
            else => .{ .kind = .denied_interactively_by_user },
        };
    }
}.handler);
```

## User Input Handling

Enable the `ask_user` tool by registering a user input handler:

```zig
session.registerUserInputHandler(struct {
    fn handler(req: copilot.UserInputRequest, _: []const u8) anyerror!copilot.UserInputResponse {
        std.debug.print("Agent asks: {s}\n", .{req.question});
        return .{ .answer = "Yes, proceed.", .was_freeform = true };
    }
}.handler);
```

## Event Subscription

Subscribe to all events or specific event types:

```zig
// All events
const sub = session.on(myHandler);

// Specific event type
const sub2 = session.onEvent("assistant.message", myHandler);

// Unsubscribe
sub.unsubscribe();
```

### Event Types

| Event Type | Description |
|---|---|
| `session.start` | Session started |
| `session.resume` | Session resumed |
| `session.idle` | Session finished processing |
| `session.error` | Error occurred |
| `session.info` | Informational message |
| `session.model_change` | Model changed |
| `session.truncation` | Context truncated |
| `session.compaction_start` | Context compaction started |
| `session.compaction_complete` | Context compaction finished |
| `user.message` | User message sent |
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning chunk |
| `assistant.turn_start` | Assistant turn started |
| `assistant.turn_end` | Assistant turn ended |
| `assistant.usage` | Token usage information |
| `tool.execution_start` | Tool execution started |
| `tool.execution_complete` | Tool execution finished |
| `tool.execution_progress` | Tool execution progress |
| `abort` | Session aborted |

### Session Idle Timeout

```zig
var client = copilot.CopilotClient.init(allocator, .{
    .session_idle_timeout_seconds = 300,
});
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```zig
var client = copilot.CopilotClient.init(allocator, .{
    .session_fs = .{
        .initial_cwd = "/repo",
        .session_state_path = "/state",
        .conventions = "posix",
    },
});
```

### Skills and Sub-Agent Orchestration

```zig
var session = try client.createSession(.{
    .skill_directories = &.{"./skills"},
    .disabled_skills = &.{"test-skill"},
    .include_sub_agent_streaming_events = true,
});
```

### Custom Provider (BYOK)

```zig
var session = try client.createSession(.{
    .model = "llama3",
    .provider = .{
        .type_name = "openai",
        .base_url = "http://localhost:11434/v1",
    },
});
```

## API Reference

### CopilotClient

| Method | Description |
|---|---|
| `CopilotClient.init(allocator, options)` | Create a new client |
| `client.start()` | Start the CLI server and connect |
| `client.stop()` | Stop the server and clean up sessions |
| `client.forceStop()` | Force stop without graceful cleanup |
| `client.createSession(config)` | Create a new session |
| `client.resumeSession(id, config)` | Resume an existing session |
| `client.deleteSession(id)` | Delete a session permanently |
| `client.listSessions()` | List all sessions |
| `client.ping(message)` | Ping the server |
| `client.getStatus()` | Get CLI version and protocol info |
| `client.getAuthStatus()` | Get authentication status |
| `client.listModels()` | List available models |
| `client.getSessionMetadata(id)` | Get session metadata |
| `client.getState()` | Get current connection state |
| `client.onLifecycle(handler)` | Subscribe to lifecycle events |

### CopilotSession

| Method | Description |
|---|---|
| `session.send(options)` | Send a message (async, non-blocking) |
| `session.sendAndWait(options, timeout)` | Send and wait for idle |
| `session.on(handler)` | Subscribe to all events |
| `session.onEvent(type, handler)` | Subscribe to specific event type |
| `session.registerTool(def, handler)` | Register a tool handler |
| `session.registerPermissionHandler(h)` | Register permission handler |
| `session.registerUserInputHandler(h)` | Register user input handler |
| `session.getMessages()` | Get session history |
| `session.destroy()` | Destroy the session |
| `session.abort()` | Abort current processing |
| `session.getId()` | Get session ID |
| `session.getWorkspacePath()` | Get workspace path |

### ClientOptions

| Field | Type | Default | Description |
|---|---|---|---|
| `cli_path` | `?[]const u8` | `null` | Path to CLI executable |
| `cli_args` | `?[]const []const u8` | `null` | Extra CLI arguments |
| `cwd` | `?[]const u8` | `null` | Working directory |
| `cli_url` | `?[]const u8` | `null` | External server URL |
| `log_level` | `[]const u8` | `"info"` | CLI log level |
| `use_stdio` | `bool` | `true` | Use stdio transport |
| `port` | `u16` | `0` | TCP port (0 = random) |
| `auto_start` | `bool` | `true` | Auto-start on first use |
| `auto_restart` | `bool` | `true` | Auto-restart on crash |
| `github_token` | `?[]const u8` | `null` | GitHub auth token |
| `use_logged_in_user` | `?bool` | `null` | Use stored OAuth |
| `session_idle_timeout_seconds` | `?u32` | `null` | Auto-cleanup timeout |
| `session_fs` | `?SessionFsConfig` | `null` | Session filesystem config |

## Connection Modes

### stdio (default)

The client spawns the CLI process and communicates via stdin/stdout pipes.

```zig
var client = copilot.CopilotClient.init(allocator, .{
    .cli_path = "/path/to/copilot-cli",
    .use_stdio = true, // default
});
```

### External Server

Connect to an already-running CLI server:

```zig
var client = copilot.CopilotClient.init(allocator, .{
    .cli_url = "localhost:3000",
});
```

## Error Handling

All fallible operations return `SdkError!T`. Error variants:

- `SdkError.JsonRpcError` -- Server returned a JSON-RPC error
- `SdkError.Timeout` -- Request timed out
- `SdkError.ConnectionClosed` -- Connection dropped
- `SdkError.ProtocolMismatch` -- SDK/server version incompatible
- `SdkError.SessionError` -- Session-level error
- `SdkError.NotConnected` -- Client not connected
- `SdkError.ProcessSpawn` -- Failed to start CLI process
- `SdkError.AllocationFailed` -- Memory allocation failed

## Protocol Version

The SDK verifies protocol compatibility with the server on startup. The current protocol version is **2**.

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk` v2.4–v2.5. Unless noted, the fields below live on the `createSession` config and default to `null`/`false`, so existing code is unaffected.

### v2.5.0

- **Reasoning effort** — `.reasoning_effort` (`"low"`/`"medium"`/`"high"`/`"xhigh"`).
- **Session rewind** — `.rewind_enabled`.
- **Additional directories** — `.additional_directories`.
- **Content exclusion** — `.content_exclusion`.
- **Tool search** — `.tool_search_json`.
- **Disabled MCP servers** — `.disabled_mcp_servers`.
- **GitHub MCP tool config** — `.github_mcp_tool_config_json`.
- **Canvas provider** — `.canvas_provider_json`.
- **Custom agents local-only** — `.custom_agents_local_only`.
- **Experimental mode** — `.experimental_mode`.
- **Agent factory args schema** — `.args_schema_json`.
- **Permission decision context** — `.decision_context` on the permission result.
- **Built-in plugin directories** — `ClientOptions.builtin_plugin_directories`.
- **In-process FFI transport** — `ClientOptions.in_process`.

### v2.4.0

- **BYOK bearer token provider** — `ClientOptions.bearer_token_provider` (`BearerTokenProvider`).
- **MCP OAuth handler flag** — `.mcp_auth_handler` (`McpAuthHandler`).
- **HTTP request handler** — `ClientOptions.request_handler` (`CopilotRequestHandler`).
- **Session citations** — `.enable_citations`.
- **Excluded built-in agents** — `.excluded_builtin_agents`.
- **Session spending limits** — `.session_limits` (`SessionLimitsConfig.max_ai_credits`).
- **Session memory** — `.memory` (`MemoryConfiguration`).
- **OTLP telemetry protocol** — `.otlp_protocol`.
- **WebSocket transport** — `.enable_web_socket_responses`.
- **Experiment assignments** — `.exp_assignments_json`.
- **Per-message agent mode** — `.agent_mode` and `.display_prompt` on `send`/`sendAndWait`.
- **Tool defer loading** — `ToolDefer` (`.auto`/`.never`).
- **Hook identifiers** — `HookType.post_tool_use`, `HookType.pre_mcp_tool_call`, `HookType.user_prompt_transformed`.
- **GitHub attachments** — `GitHubAttachment.github_commit` / `github_repository`.

Configure reasoning effort, rewind, extra directories, content exclusion, and tool search:

```zig
var session = try client.createSession(.{
    .reasoning_effort = "high",
    .rewind_enabled = true,
    .additional_directories = &.{ "../shared", "/data/corpus" },
    .content_exclusion = true,
    .tool_search_json = "{\"enabled\":true}",
});
```

Supply bring-your-own-key bearer tokens, minted per session:

```zig
fn provideToken(args: copilot.ProviderTokenArgs) anyerror![]const u8 {
    return mintTokenFor(args.session_id);
}

var client = copilot.CopilotClient.init(allocator, .{
    .bearer_token_provider = provideToken,
});
```

Cap spend and turn on citations and persistent memory:

```zig
var session = try client.createSession(.{
    .session_limits = .{ .max_ai_credits = 5.0 },
    .enable_citations = true,
    .memory = .{ .enabled = true },
});
```

Choose an agent mode and display prompt for a single turn:

```zig
try session.send(.{
    .prompt = "Refactor the auth module",
    .agent_mode = "plan",
    .display_prompt = "Refactor auth (planning)",
});
```

## Cookbook

See the [cookbook/](cookbook/) directory for recipes:

- [Tools and Skills](cookbook/tools-and-skills.md)
- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Advanced Features](cookbook/advanced-features.md)
- [Event Handling](cookbook/event-handling.md)

## License

MIT -- See [LICENSE](../LICENSE) for details.
