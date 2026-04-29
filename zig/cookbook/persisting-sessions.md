# Persisting Sessions

Resume sessions across process restarts using session IDs.

## Save and Resume

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

    // --- First run: create a session ---
    var session = try client.createSession(.{});
    const sid = session.getId();
    std.debug.print("Session ID: {s}\n", .{sid});

    _ = try session.sendAndWait(.{
        .prompt = "Remember: the project name is Zephyr.",
    }, 30_000);

    // Save sid to a file or database for the next run...
    try session.destroy();
    try client.stop();
}
```

```zig
// --- Second run: resume the session ---
pub fn resumeExample(allocator: std.mem.Allocator) !void {
    var client = copilot.CopilotClient.init(allocator, .{});
    defer client.deinit();
    try client.start();

    const saved_id = "the-session-id-from-first-run";
    var session = try client.resumeSession(saved_id, .{});

    _ = try session.sendAndWait(.{
        .prompt = "What is the project name?",
    }, 30_000);
    // The assistant should remember "Zephyr"

    try session.destroy();
    try client.stop();
}
```

## SessionFs for Persistent State

Configure a session filesystem to persist workspace state across compaction
boundaries and session resumes:

```zig
var client = copilot.CopilotClient.init(allocator, .{
    .session_fs = .{
        .initial_cwd = "/repo",
        .session_state_path = "/home/user/.copilot/session-state",
        .conventions = "posix",
    },
});
```

The session filesystem is automatically managed by the CLI server and
survives context compaction events.
