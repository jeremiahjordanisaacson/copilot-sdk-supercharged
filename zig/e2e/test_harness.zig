// E2E test harness for the Zig Copilot SDK.
//
// Spawns the shared replay proxy (npx tsx test/harness/server.ts),
// parses the `Listening: http://...` URL from stdout, and exposes it
// as COPILOT_API_URL for the test suite.

const std = @import("std");

pub const ProxyHandle = struct {
    process: std.process.Child,
    url: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ProxyHandle) void {
        // Kill the proxy process
        _ = self.process.kill() catch {};
        _ = self.process.wait() catch {};
        self.allocator.free(self.url);
    }
};

/// Start the replay proxy and return the listening URL.
pub fn startProxy(allocator: std.mem.Allocator) !ProxyHandle {
    // Build path: from zig/e2e -> ../../test/harness/server.ts
    const server_path = "../../test/harness/server.ts";
    const cwd_path = "../../test/harness";

    var child = std.process.Child.init(
        &.{ "npx", "tsx", server_path },
        allocator,
    );
    child.cwd = cwd_path;
    child.stdout_behavior = .pipe;
    child.stderr_behavior = .inherit;

    try child.spawn();

    // Read the first line from stdout to get the proxy URL
    const stdout = child.stdout orelse return error.NoStdout;
    var buf: [1024]u8 = undefined;
    var line_len: usize = 0;

    // Read until newline
    while (line_len < buf.len) {
        const byte_read = stdout.read(buf[line_len .. line_len + 1]) catch |err| {
            _ = child.kill() catch {};
            return err;
        };
        if (byte_read == 0) break;
        if (buf[line_len] == '\n') break;
        line_len += 1;
    }

    const line = buf[0..line_len];

    // Parse "Listening: http://..."
    const prefix = "Listening: ";
    if (std.mem.indexOf(u8, line, prefix)) |idx| {
        const url_start = idx + prefix.len;
        var url_end = url_start;
        while (url_end < line.len and line[url_end] != ' ' and line[url_end] != '\r') {
            url_end += 1;
        }
        const url = try allocator.dupe(u8, line[url_start..url_end]);

        // Set environment variable
        try std.posix.setenv("COPILOT_API_URL", url, true);

        return ProxyHandle{
            .process = child,
            .url = url,
            .allocator = allocator,
        };
    }

    _ = child.kill() catch {};
    return error.CouldNotParseUrl;
}
