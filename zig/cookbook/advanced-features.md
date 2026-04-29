# Advanced Features

Streaming, permissions, user input, image generation, and custom providers.

## Streaming

Enable streaming to receive incremental content deltas:

```zig
var session = try client.createSession(.{ .streaming = true });

_ = session.on(struct {
    fn handler(event: copilot.SessionEvent) void {
        if (event.isDelta()) {
            if (event.delta_content) |delta| {
                std.io.getStdOut().writer().print("{s}", .{delta}) catch {};
            }
        }
    }
}.handler);
```

## Permission Handling

Register a handler to approve or deny agent permission requests:

```zig
session.registerPermissionHandler(struct {
    fn handler(req: copilot.PermissionRequest, session_id: []const u8) anyerror!copilot.PermissionResult {
        _ = session_id;
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
        return .{
            .answer = "Yes, proceed.",
            .was_freeform = true,
        };
    }
}.handler);
```

## Image Generation

Request image responses using response_format and image_options:

```zig
_ = try session.sendAndWait(.{
    .prompt = "Generate a sunset over mountains",
    .response_format = .image,
    .image_options = .{
        .size = "1024x1024",
        .quality = "hd",
        .style = "natural",
    },
}, 60_000);
```

## Custom Provider (BYOK)

Point to your own OpenAI-compatible endpoint:

```zig
var session = try client.createSession(.{
    .model = "llama3",
    .provider = .{
        .type_name = "openai",
        .base_url = "http://localhost:11434/v1",
        .api_key = null,
    },
});
```

## Session Idle Timeout

Configure automatic session cleanup after inactivity:

```zig
var client = copilot.CopilotClient.init(allocator, .{
    .session_idle_timeout_seconds = 300,
});
```

## Session Metadata

Retrieve metadata about a session:

```zig
const meta = try client.getSessionMetadata("session-123");
_ = meta;
```
