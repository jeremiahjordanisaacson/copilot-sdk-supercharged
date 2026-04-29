# Multiple Sessions

Run multiple independent conversation sessions on a single client connection.

## Creating Multiple Sessions

```zig
const std = @import("std");
const copilot = @import("copilot-sdk");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var client = copilot.CopilotClient.init(allocator, .{});
    defer client.deinit();
    try client.start();

    // Create two sessions with different system prompts
    var code_session = try client.createSession(.{
        .system_prompt = "You are a code assistant. Respond only with code.",
    });

    var chat_session = try client.createSession(.{
        .system_prompt = "You are a friendly chat companion.",
    });

    // Use them independently
    _ = try code_session.sendAndWait(.{
        .prompt = "Write a Zig hello world",
    }, 30_000);

    _ = try chat_session.sendAndWait(.{
        .prompt = "Tell me a joke",
    }, 30_000);

    // Clean up
    try code_session.destroy();
    try chat_session.destroy();
    try client.stop();
}
```

## Session Isolation

Each session is fully independent:

- Separate conversation histories
- Separate tool registrations
- Separate event subscriptions
- Separate system prompts and model settings

## Listing and Deleting Sessions

```zig
// List all sessions on the server
const sessions = try client.listSessions();
_ = sessions;

// Delete a session by ID
try client.deleteSession("session-abc-123");
```
