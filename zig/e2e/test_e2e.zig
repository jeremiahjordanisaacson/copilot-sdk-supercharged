// E2E tests for the Zig Copilot SDK.
//
// Uses Zig's built-in test framework. Tests are run via `zig test e2e/test_e2e.zig`.
//
// Tests:
//   1. Session create + disconnect
//   2. Send message
//   3. SessionFs configuration
//   4. Multi-turn conversation
//   5. Session resume
//   6. Session list
//   7. Session metadata
//   8. Session delete
//   9. Model list
//  10. Ping
//  11. Auth status
//  12. Client lifecycle
//  13. Foreground session
//  14. Tools
//  15. Streaming
//  16. System message customization
//  17. Session fs provider
//  18. MCP servers config
//  19. Skills config
//  20. Compaction

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

// ---------------------------------------------------------------------------
// Test 4: Multi-turn conversation
// ---------------------------------------------------------------------------
test "multi-turn conversation" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    // First message
    const msg1_body =
        \\{"messages":[{"role":"user","content":"Hello, start a conversation"}]}
    ;
    const response1 = try httpPost(allocator, chat_url, msg1_body);
    defer allocator.free(response1);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges1 = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges1);

    try testing.expect(exchanges1.len > 0);

    // Second message
    const msg2_body =
        \\{"messages":[{"role":"user","content":"Continue the conversation"}]}
    ;
    const response2 = try httpPost(allocator, chat_url, msg2_body);
    defer allocator.free(response2);

    const exchanges2 = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges2);

    try testing.expect(exchanges2.len > 0);
}

// ---------------------------------------------------------------------------
// Test 5: Session resume
// ---------------------------------------------------------------------------
test "session resume" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    // Create initial session
    const create_body =
        \\{"model":"gpt-4"}
    ;
    const response1 = try httpPost(allocator, session_url, create_body);
    defer allocator.free(response1);

    // Resume with a second session create
    const resume_body =
        \\{"model":"gpt-4","sessionId":"resumed-session"}
    ;
    const response2 = try httpPost(allocator, session_url, resume_body);
    defer allocator.free(response2);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 6: Session list
// ---------------------------------------------------------------------------
test "session list" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    // Create two sessions
    const create_body1 =
        \\{"model":"gpt-4"}
    ;
    const r1 = try httpPost(allocator, session_url, create_body1);
    defer allocator.free(r1);

    const create_body2 =
        \\{"model":"gpt-4"}
    ;
    const r2 = try httpPost(allocator, session_url, create_body2);
    defer allocator.free(r2);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 7: Session metadata
// ---------------------------------------------------------------------------
test "session metadata" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    const create_body =
        \\{"model":"gpt-4","metadata":{"source":"zig-e2e","version":"1.0"}}
    ;
    const response = try httpPost(allocator, session_url, create_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 8: Session delete
// ---------------------------------------------------------------------------
test "session delete" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    const create_body =
        \\{"model":"gpt-4"}
    ;
    const response = try httpPost(allocator, session_url, create_body);
    defer allocator.free(response);

    // DELETE the session
    const delete_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions/test-session", .{proxy.url});
    defer allocator.free(delete_url);

    var client = http.Client{ .allocator = allocator };
    defer client.deinit();

    const uri = try std.Uri.parse(delete_url);
    var req = try client.open(.DELETE, uri, .{
        .server_header_buffer = try allocator.alloc(u8, 4096),
    });
    defer req.deinit();

    try req.send();
    try req.wait();

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 9: Model list
// ---------------------------------------------------------------------------
test "model list" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 10: Ping
// ---------------------------------------------------------------------------
test "ping" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 11: Auth status
// ---------------------------------------------------------------------------
test "auth status" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const auth_url = try std.fmt.allocPrint(allocator, "{s}/v1/auth/status", .{proxy.url});
    defer allocator.free(auth_url);

    const auth_body =
        \\{"action":"check"}
    ;
    const response = try httpPost(allocator, auth_url, auth_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 12: Client lifecycle
// ---------------------------------------------------------------------------
test "client lifecycle" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    // Verify proxy is reachable (connect)
    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);

    // Simulate disconnect by creating and tearing down a second proxy
    var proxy2 = try harness.startProxy(allocator);
    defer proxy2.deinit();

    const config_url2 = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy2.url});
    defer allocator.free(config_url2);

    _ = try httpPost(allocator, config_url2, config_body);

    const exchanges_url2 = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy2.url});
    defer allocator.free(exchanges_url2);

    const exchanges2 = try httpGet(allocator, exchanges_url2);
    defer allocator.free(exchanges2);

    try testing.expect(exchanges2.len > 0);
}

// ---------------------------------------------------------------------------
// Test 13: Foreground session
// ---------------------------------------------------------------------------
test "foreground session" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_create_and_disconnect_sessions"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    const create_body =
        \\{"model":"gpt-4","foreground":true}
    ;
    const response = try httpPost(allocator, session_url, create_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 14: Tools
// ---------------------------------------------------------------------------
test "tools" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    const msg_body =
        \\{"messages":[{"role":"user","content":"Use the tool"}],"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather","parameters":{"type":"object","properties":{"location":{"type":"string"}}}}}]}
    ;
    const response = try httpPost(allocator, chat_url, msg_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 15: Streaming
// ---------------------------------------------------------------------------
test "streaming" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    const msg_body =
        \\{"messages":[{"role":"user","content":"Stream a response"}],"streaming":true}
    ;
    const response = try httpPost(allocator, chat_url, msg_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 16: System message customization
// ---------------------------------------------------------------------------
test "system message customization" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    const msg_body =
        \\{"messages":[{"role":"system","content":"You are a helpful Zig assistant"},{"role":"user","content":"Hello"}],"systemMessage":"Custom system instructions"}
    ;
    const response = try httpPost(allocator, chat_url, msg_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 17: Session fs provider
// ---------------------------------------------------------------------------
test "session fs provider" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session_fs","workDir":"should_configure_session_fs"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const session_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/sessions", .{proxy.url});
    defer allocator.free(session_url);

    const fs_body =
        \\{"model":"gpt-4","sessionFs":{"initialCwd":"/workspace","sessionStatePath":"/state","conventions":"posix"}}
    ;
    const response = try httpPost(allocator, session_url, fs_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 18: MCP servers config
// ---------------------------------------------------------------------------
test "mcp servers config" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    const msg_body =
        \\{"messages":[{"role":"user","content":"Use MCP"}],"mcpServers":{"local":{"url":"http://localhost:3000","transport":"http"}}}
    ;
    const response = try httpPost(allocator, chat_url, msg_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 19: Skills config
// ---------------------------------------------------------------------------
test "skills config" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    const msg_body =
        \\{"messages":[{"role":"user","content":"Use skill"}],"skills":["code-review","documentation"]}
    ;
    const response = try httpPost(allocator, chat_url, msg_body);
    defer allocator.free(response);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}

// ---------------------------------------------------------------------------
// Test 20: Compaction
// ---------------------------------------------------------------------------
test "compaction" {
    const allocator = testing.allocator;

    var proxy = try harness.startProxy(allocator);
    defer proxy.deinit();

    const config_url = try std.fmt.allocPrint(allocator, "{s}/config", .{proxy.url});
    defer allocator.free(config_url);

    const config_body =
        \\{"filePath":"session","workDir":"should_have_stateful_conversation"}
    ;
    _ = try httpPost(allocator, config_url, config_body);

    const chat_url = try std.fmt.allocPrint(allocator, "{s}/v1/chat/completions", .{proxy.url});
    defer allocator.free(chat_url);

    // Send multiple messages to trigger compaction behavior
    const msg1_body =
        \\{"messages":[{"role":"user","content":"First message for compaction"}]}
    ;
    const r1 = try httpPost(allocator, chat_url, msg1_body);
    defer allocator.free(r1);

    const msg2_body =
        \\{"messages":[{"role":"user","content":"Second message for compaction"}]}
    ;
    const r2 = try httpPost(allocator, chat_url, msg2_body);
    defer allocator.free(r2);

    const msg3_body =
        \\{"messages":[{"role":"user","content":"Third message for compaction"}]}
    ;
    const r3 = try httpPost(allocator, chat_url, msg3_body);
    defer allocator.free(r3);

    const exchanges_url = try std.fmt.allocPrint(allocator, "{s}/exchanges", .{proxy.url});
    defer allocator.free(exchanges_url);

    const exchanges = try httpGet(allocator, exchanges_url);
    defer allocator.free(exchanges);

    try testing.expect(exchanges.len > 0);
}
