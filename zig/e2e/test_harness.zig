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

/// Locates the Copilot CLI for E2E runs, in order:
///   1. COPILOT_CLI_PATH, when it names an existing file (CI points it at
///      nodejs/node_modules/.bin/copilot, which runs directly);
///   2. nodejs/node_modules/@github/copilot/npm-loader.js (CLI 1.0.113 and newer);
///   3. the legacy nodejs/node_modules/@github/copilot-*/index.js and
///      nodejs/node_modules/@github/copilot/index.js entry points.
/// The repository's nodejs directory is searched from the working directory upward.
/// The SDK runs `.js` entry points with node. Returns an owned path, or null.
pub fn findCliPath(allocator: std.mem.Allocator) !?[]u8 {
    if (std.process.getEnvVarOwned(allocator, "COPILOT_CLI_PATH")) |path| {
        if (fileExists(path)) return path;
        allocator.free(path);
    } else |err| switch (err) {
        error.EnvironmentVariableNotFound => {},
        else => return err,
    }

    const start = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(start);
    var current: ?[]const u8 = start;
    while (current) |dir| : (current = std.fs.path.dirname(dir)) {
        if (try findCliUnder(allocator, dir)) |path| return path;
    }
    return null;
}

fn findCliUnder(allocator: std.mem.Allocator, root: []const u8) !?[]u8 {
    const scope_dir = try std.fs.path.join(allocator, &.{ root, "nodejs", "node_modules", "@github" });
    defer allocator.free(scope_dir);

    const loader = try std.fs.path.join(allocator, &.{ scope_dir, "copilot", "npm-loader.js" });
    if (fileExists(loader)) return loader;
    allocator.free(loader);

    var scope = std.fs.cwd().openDir(scope_dir, .{ .iterate = true }) catch return null;
    defer scope.close();
    var it = scope.iterate();
    while (try it.next()) |entry| {
        if (!std.mem.startsWith(u8, entry.name, "copilot-")) continue;
        const legacy = try std.fs.path.join(allocator, &.{ scope_dir, entry.name, "index.js" });
        if (fileExists(legacy)) return legacy;
        allocator.free(legacy);
    }

    const index = try std.fs.path.join(allocator, &.{ scope_dir, "copilot", "index.js" });
    if (fileExists(index)) return index;
    allocator.free(index);
    return null;
}

fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

test "findCliPath returns an existing CLI entry point" {
    const allocator = std.testing.allocator;
    const path = (try findCliPath(allocator)) orelse return error.SkipZigTest;
    defer allocator.free(path);
    try std.testing.expect(fileExists(path));
}
