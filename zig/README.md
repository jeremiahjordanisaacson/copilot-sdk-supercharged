# Copilot Supercharged SDK for Zig

A Zig client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Requirements

- **Zig** 0.14.0 or later
- **Copilot CLI** 1.0.113 or newer, installed and on PATH (or specify `cli_path`; `.js` entry points such as `@github/copilot/npm-loader.js` are run with `node`)
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

## What's New in v3.0.0

The Zig SDK now speaks **SDK protocol 4** and requires **Copilot CLI 1.0.113 or newer**. Older CLIs are rejected at `start` with `SdkError.ProtocolMismatch` (the reason is logged under the `copilot_sdk` log scope).

**Existing options keep working.** The SDK still builds the same flat session options and translates them into the grouped protocol 4 payload right before `session.create` and `session.resume` (for example `streaming` becomes `events.streaming` and `excluded_tools` becomes `tools.excludedTools`). The legacy single `provider` is sent as a named `"default"` provider with Copilot API models disabled, exactly as before. Failures of `session.abort`, `session.delete`, and `session.setForeground` surface as `SdkError.JsonRpcError`; `client.lastRpcError()` returns the code and message.

This release also routes server requests and `session.event` notifications to sessions, sends options that were previously ignored (`tools`, `provider`, `mcp_servers_json`, `commands_json`, `github_token`, `system_prompt` as an appended `systemMessage`, and more), and answers `external_tool.requested` events with the handlers registered through `session.registerTool`.

### Client options: client info, client mode, network callbacks

```zig
fn resolveProxy(_: std.mem.Allocator, url: []const u8) anyerror!?[]const u8 {
    return if (std.mem.startsWith(u8, url, "https://internal.")) "PROXY proxy.corp:8080" else null;
}

var client = copilot.CopilotClient.init(allocator, .{
    // Sent as `clientInfo` on the `connect` handshake (empty fields are dropped)
    .client_info = .{ .editor_name = "my-editor", .editor_version = "1.2.0" },
    // .coding_agent (default) or .empty: minimal defaults, and every session must set available_tools
    .mode = .coding_agent,
    // Registered with `network.setProvider` after connecting
    .network = .{ .scope = copilot.NetworkScope.connection, .resolve_proxy = resolveProxy },
});
```

`NetworkOptions` also accepts `authenticate_negotiate` (returns a SPNEGO token) and `authenticate_basic` (returns `NetworkBasicCredentials`). `ClientMode.parse("copilot-cli")` accepts the legacy alias of `"coding-agent"`.

### Session options

```zig
const session = try client.createSession(.{
    .ask_user_variant = copilot.AskUserVariant.elicitation,
    .sandbox_config = .{ .enabled = true, .allow_bypass = true },
    .feature_flags_json = "{\"my_flag\":true}",
    .allowed_models = &.{ "gpt-5", "claude-sonnet-4.5" },
    .auth_client_id_metadata_url = "https://example.com/oauth/client.json",
    .agent_directories = &.{"./agents"},
    .hook_directories = &.{"./hooks"},
    .working_directory = "/repo",
    .trust_working_directory = copilot.TrustWorkingDirectory.session, // session.create only
    .image_generation = .{ .enabled = true, .save_to_disk = false },
    .autopilot = .{ .continuation_enabled = true, .max_continues = 3 },
    .max_output_tokens = 4096, // inference.capabilities.limits.max_output_tokens
    .auto_tier = copilot.AutoTier.fast,
    .permission_mode = copilot.PermissionMode.assisted,
});

// Resume-only options
const resumed = try client.resumeSession(session_id, .{
    .allow_transcript_recovery = true,
    .suppress_resume_event = true,
    .clear_default_provider = true, // sends defaultProvider: null
});
```

### GitHub token provider

```zig
fn githubToken(_: std.mem.Allocator, args: copilot.GitHubTokenProviderArgs) anyerror!copilot.GitHubTokenResult {
    std.debug.print("token for {s} ({s})\n", .{ args.host, args.reason }); // reason: "initial" or "refresh"
    return .{ .token = .{ .access_token = "gho_from_your_secret_store", .expires_in = 3600 } };
}

const session = try client.createSession(.{ .github_token_provider = githubToken });
```

The SDK registers the provider under a generated ID, answers `gitHubToken.getToken`, and drops the registration when the session detaches or is freed. It cannot be combined with `github_token` (`SdkError.InvalidConfig`).

### Named BYOK providers

```zig
const session = try client.createSession(.{
    .model = "ollama/llama3", // "<provider>/<id>"
    .providers = &.{.{ .name = "ollama", .base_url = "http://localhost:11434/v1", .api_key_command = "my-key-helper" }},
    .models = &.{.{ .provider = "ollama", .id = "llama3", .max_output_tokens = 2048 }},
    .default_provider = "ollama",
    .capi = .{ .enabled = false },
});
```

`ProviderConfig.api_key_command` also works with the legacy single `provider`.

### Tools, messages, and events

```zig
fn lookup(invocation: copilot.ToolInvocation) anyerror!copilot.ToolResult {
    _ = invocation.arguments_json;
    return .{ .text_result_for_llm = "{\"status\":\"open\"}" };
}

const session = try client.createSession(.{
    .tools = &.{.{ .name = "lookup", .description = "Look up a ticket", .idempotent = true }},
});
try session.registerTool(.{ .name = "lookup" }, lookup);

const source = try copilot.MessageSource.agent(allocator, "reviewer"); // "agent-reviewer"
defer allocator.free(source);
_ = try session.send(.{
    .prompt = "List the open tickets as JSON",
    .source = source,
    .client_turn_id = "turn-42",
    .response_schema_json = "{\"type\":\"object\",\"properties\":{\"tickets\":{\"type\":\"array\"}}}",
});

// Live shell output (`tool.shell_output`)
_ = session.onEvent(copilot.EVENT_TOOL_SHELL_OUTPUT, struct {
    fn handler(event: copilot.SessionEvent) void {
        if (event.shell_output) |out| std.debug.print("[{s}] {s}", .{ out.stream orelse "stdout", out.text });
    }
}.handler);
```

`response_schema_json` is sent as `responseFormat: {type: "json_schema", jsonSchema: {name: "response", strict: true, schema}}`.

### Shell authentication, memory store, and sub-agent hooks

```zig
fn answerShell(_: std.mem.Allocator, req: copilot.ShellAuthenticationRequest) anyerror!copilot.ShellAuthenticationResponse {
    if (std.mem.eql(u8, req.input_kind, copilot.ShellAuthenticationInputKind.confirm)) return .{ .accept = "yes" };
    return .cancel;
}

fn storeMemory(memory: copilot.StoredMemory) anyerror!void {
    std.debug.print("remember {s}: {s}\n", .{ memory.subject, memory.fact });
}

fn onHook(_: std.mem.Allocator, hook_type: []const u8, _: copilot.types.JsonValue, _: []const u8) anyerror!?copilot.types.JsonValue {
    if (std.mem.eql(u8, hook_type, copilot.HookType.subagent_start)) std.debug.print("sub-agent starting\n", .{});
    return null;
}

const session = try client.createSession(.{
    .on_shell_authentication = answerShell, // sends interaction.requestShellAuthentication
    .on_memory_store = storeMemory, // sends state.hasMemoryStore
    .on_hook = onHook, // subagentStart / subagentStop arrive through hooks.invoke
});
```

### Session and client methods

```zig
_ = try session.setAutoTier(copilot.AutoTier.balance); // session.model.switchAutoTier
const mode = try session.setPermissionMode(copilot.PermissionMode.allow_all);
if (!mode.success) std.debug.print("refused: {?s}\n", .{mode.reason}); // "managed_policy"
_ = try session.recheckPolicyHelper(); // session.managedSettings.recheckPolicyHelper
try client.clearManagedSettingsCache(); // managedSettings.clearCache
try session.detach(); // session.detach: releases the session, keeps it resumable

// Policy Helper failures reported by a failed session.create / session.resume
const blocked = client.createSession(.{}) catch |err| {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    if (client.lastPolicyHelperFailure(arena.allocator())) |failure| {
        std.debug.print("{s}: {s}\n", .{ failure.category, failure.message });
    }
    return err;
};
_ = blocked;
```

`types.PolicyHelperFailure.fromJson` also reads the failure from a `session.getManagedSettings()` snapshot.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: the SDK keeps spawning the CLI. The existing `in_process` option is unchanged.
- The transport is synchronous: server requests (GitHub token, shell authentication, memory store, hooks, network callbacks) and session events are handled only while the client is blocked in a request or in `sendAndWait`. JSON values returned by client and session methods stay valid until the next request on the client.
- `cli_url` (connecting to an already running server) is not implemented yet; `start` returns `SdkError.NotConnected`.
- Permission and user-input handlers registered on a session are stored but not yet answered over protocol 4; the runtime applies the session's permission mode instead (`permission_mode`, `setPermissionMode`).

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

Register a handler to approve or deny permission requests from the agent (see Known limitations: protocol 4 does not deliver these requests to the Zig SDK yet; use `permission_mode` / `setPermissionMode`):

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

Enable the `ask_user` tool by registering a user input handler (see Known limitations: not yet answered over protocol 4):

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
| `tool.shell_output` | Live shell output (`event.shell_output`: tool call ID, text, sequence, stream); not persisted |
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
| `client.setForegroundSessionId(id)` | Set the foreground session |
| `client.clearManagedSettingsCache()` | Clear cached managed settings (`managedSettings.clearCache`) |
| `client.lastRpcError()` | JSON-RPC error of the last failed request |
| `client.lastPolicyHelperFailure(arena)` | Policy Helper failure from the last failed request's error data |
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
| `session.detach()` | Release the session (`session.detach`); it stays resumable |
| `session.destroy()` | Alias of `detach()` |
| `session.abort()` | Abort current processing |
| `session.setAutoTier(tier)` | Change the Auto routing tier (`session.model.switchAutoTier`) |
| `session.setPermissionMode(mode)` | Set the permission mode; returns `PermissionModeResult` |
| `session.getManagedSettings()` | Effective managed settings snapshot |
| `session.recheckPolicyHelper()` | Re-run the managed-settings Policy Helper |
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
| `client_info` | `?ClientInfo` | `null` | Integration identity sent on `connect` |
| `mode` | `ClientMode` | `.coding_agent` | `.empty` applies minimal session defaults |
| `network` | `?NetworkOptions` | `null` | Proxy and authentication callbacks |

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
- `SdkError.RequestFailed` -- The server answered `success: false`
- `SdkError.InvalidConfig` -- Invalid options (for example `github_token` together with `github_token_provider`)

After `SdkError.JsonRpcError`, `client.lastRpcError()` returns the error code, message, and data.

## Protocol Version

The SDK verifies protocol compatibility with the server on startup. The current protocol version is **4** (Copilot CLI 1.0.113 or newer).

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
