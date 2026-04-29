# Error Handling

The Zig SDK uses error unions (`anyerror!T`) and the `SdkError` set for all
fallible operations, following standard Zig patterns.

## Error Set

```zig
pub const SdkError = error{
    JsonRpcError,       // Server returned a JSON-RPC error
    Timeout,            // Request or wait timed out
    ConnectionClosed,   // Connection dropped unexpectedly
    ProtocolMismatch,   // SDK/server protocol version mismatch
    SessionError,       // Session-level error
    NotConnected,       // Client is not connected
    ProcessSpawn,       // Failed to start the CLI process
    InvalidResponse,    // Server response could not be parsed
    AllocationFailed,   // Memory allocation failed
    WriteFailed,        // Write to transport failed
    ReadFailed,         // Read from transport failed
    ParseError,         // JSON parse error
};
```

## Using `try`

The idiomatic way to propagate errors in Zig:

```zig
pub fn run(allocator: std.mem.Allocator) !void {
    var client = copilot.CopilotClient.init(allocator, .{});
    defer client.deinit();

    try client.start();
    var session = try client.createSession(.{});
    _ = try session.sendAndWait(.{ .prompt = "Hello!" }, 30_000);
    try session.destroy();
    try client.stop();
}
```

## Catching Specific Errors

```zig
client.start() catch |err| {
    switch (err) {
        error.ProcessSpawn => std.debug.print("CLI not found\n", .{}),
        error.ProtocolMismatch => std.debug.print("Update your CLI\n", .{}),
        else => std.debug.print("Unexpected: {any}\n", .{err}),
    }
    return err;
};
```

## errdefer for Cleanup

Use `errdefer` to clean up resources when an error propagates:

```zig
var session = try client.createSession(.{});
errdefer session.destroy() catch {};

// If this fails, the session is destroyed automatically
_ = try session.sendAndWait(.{ .prompt = "Hello" }, 10_000);
```
