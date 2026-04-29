// examples/basic_example.zig -- Minimal usage of the Copilot SDK.
//
// Build and run:
//   zig build run-example
//
// NOTE: Requires the Copilot CLI to be installed and on PATH (or set
// cli_path in the options).

const std = @import("std");
const copilot = @import("copilot-sdk");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();

    // 1. Create a client
    try stdout.print("Creating Copilot client...\n", .{});

    var client = copilot.CopilotClient.init(allocator, .{
        // Uncomment and set the path if the CLI is not on PATH:
        // .cli_path = "/path/to/copilot-cli",
    });
    defer client.deinit();

    // 2. Start the client (spawns the CLI process)
    client.start() catch |err| {
        try stdout.print("Failed to start client: {any}\n", .{err});
        return;
    };

    // 3. Create a session
    const session = client.createSession(.{
        .streaming = true,
    }) catch |err| {
        try stdout.print("Failed to create session: {any}\n", .{err});
        return;
    };

    // 4. Subscribe to events
    _ = session.on(struct {
        fn handler(event: copilot.SessionEvent) void {
            if (event.assistantMessageContent()) |content| {
                std.io.getStdOut().writer().print("Assistant: {s}\n", .{content}) catch {};
            }
            if (event.isDelta()) {
                if (event.delta_content) |delta| {
                    std.io.getStdOut().writer().print("{s}", .{delta}) catch {};
                }
            }
        }
    }.handler);

    // 5. Send a message and wait for the response
    const response = session.sendAndWait(.{
        .prompt = "What is 2 + 2?",
    }, 30_000) catch |err| {
        try stdout.print("Send failed: {any}\n", .{err});
        return;
    };

    if (response) |ev| {
        if (ev.assistantMessageContent()) |content| {
            try stdout.print("\nFinal answer: {s}\n", .{content});
        }
    }

    // 6. Clean up
    session.destroy() catch {};
    client.stop() catch {};
    try stdout.print("Done.\n", .{});
}
