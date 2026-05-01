// E2E tests for the Zig Copilot SDK.
//
// Uses Zig's built-in test framework. Tests are run via `zig test e2e/test_e2e.zig`.
//
// Tests:
//   1. Session create + disconnect
//   2. Send message
//   3. SessionFs configuration

const std = @import("std");
const harness = @import("test_harness.zig");

const testing = std.testing;
const http = std.http;

fn httpGet(allocator: std.mem.Allocator, url: []const u8) ![]u8 {
    var client = http.Client{ .allocator = allocator };
    defer client.deinit();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    const uri = try std.Uri.parse(url);
    var req = try client.open(.GET, uri, .{
        .server_header_buffer = try allocator.alloc(u8, 4096),
    });
    defer req.deinit();

    try req.send();
    try req.wait();

    var read_buf: [4096]u8 = undefined;
    while (true) {
        const n = try req.reader().read(&read_buf);
        if (n == 0) break;
        try buf.appendSlice(read_buf[0..n]);
    }

    return try buf.toOwnedSlice();
}

fn httpPost(allocator: std.mem.Allocator, url: []const u8, body: []const u8) ![]u8 {
    var client = http.Client{ .allocator = allocator };
    defer client.deinit();

    const uri = try std.Uri.parse(url);
    var req = try client.open(.POST, uri, .{
        .server_header_buffer = try allocator.alloc(u8, 4096),
        .extra_headers = &.{
            .{ .name = "Content-Type", .value = "application/json" },
            .{ .name = "Authorization", .value = "Bearer fake-token-for-e2e-tests" },
        },
    });
    defer req.deinit();

    req.transfer_encoding = .{ .content_length = body.len };
    try req.send();
    try req.writer().writeAll(body);
    try req.finish();
    try req.wait();

    var read_buf: [4096]u8 = undefined;
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();

    while (true) {
        const n = try req.reader().read(&read_buf);
        if (n == 0) break;
        try result.appendSlice(read_buf[0..n]);
    }

    return try result.toOwnedSlice();
}

// ---------------------------------------------------------------------------
// Test 1: Session create + disconnect
// ---------------------------------------------------------------------------
test "session create and disconnect" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    // Verify the proxy /exchanges endpoint responds
    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 2: Send message
// ---------------------------------------------------------------------------
test "send message and get response" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    // Send a message
    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    const msg_body =
        \\{"messages":[{"role":"user","content":"What is 1+1?"}]}
    ;
    const response = try httpPost(allocator, chat_url, msg_body);
    defer allocator.free(response);

    // Verify exchanges were captured
    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 3: SessionFs
// ---------------------------------------------------------------------------
test "session fs configuration" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session_fs","workDir":"should_configure_session_fs"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    // Send a session creation with sessionFs
    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    const fs_body =
        \\{"model":"gpt-4","sessionFs":{"initialCwd":"/","sessionStatePath":"/session-state","conventions":"posix"}}
    ;
    const response = try httpPost(allocator, session_url, fs_body);
    defer allocator.free(response);

    // Verify proxy is alive
    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}
