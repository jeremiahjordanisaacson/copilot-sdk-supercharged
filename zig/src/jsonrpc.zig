// jsonrpc.zig -- JSON-RPC 2.0 transport with Content-Length framing (LSP style).
//
// Reads and writes messages on a pair of std.io streams using the
// "Content-Length: N\r\n\r\n<payload>" wire format that the Copilot CLI expects.

const std = @import("std");
const types = @import("types.zig");
const Allocator = std.mem.Allocator;

pub const JsonRpcError = types.JsonRpcError;
pub const JsonValue = types.JsonValue;
pub const SdkError = types.SdkError;

// ---------------------------------------------------------------------------
// Request ID generator
// ---------------------------------------------------------------------------

pub const IdGenerator = struct {
    counter: std.atomic.Value(i64) = std.atomic.Value(i64).init(1),

    pub fn next(self: *IdGenerator) i64 {
        return self.counter.fetchAdd(1, .monotonic);
    }
};

// ---------------------------------------------------------------------------
// Pending request bookkeeping
// ---------------------------------------------------------------------------

const PendingEntry = struct {
    result: ?JsonValue = null,
    rpc_error: ?JsonRpcError = null,
    completed: bool = false,
};

// ---------------------------------------------------------------------------
// JsonRpcTransport
// ---------------------------------------------------------------------------

pub const JsonRpcTransport = struct {
    allocator: Allocator,
    writer: std.io.AnyWriter,
    reader: std.io.AnyReader,
    id_gen: IdGenerator = .{},
    pending: std.AutoHashMap(i64, *PendingEntry),
    notification_handler: ?*const fn (method: []const u8, params: ?JsonValue) void = null,
    request_handler: ?*const fn (method: []const u8, params: ?JsonValue, id: i64) ?JsonValue = null,
    write_mutex: std.Thread.Mutex = .{},

    pub fn init(allocator: Allocator, writer: std.io.AnyWriter, reader: std.io.AnyReader) JsonRpcTransport {
        return .{
            .allocator = allocator,
            .writer = writer,
            .reader = reader,
            .pending = std.AutoHashMap(i64, *PendingEntry).init(allocator),
        };
    }

    pub fn deinit(self: *JsonRpcTransport) void {
        var it = self.pending.iterator();
        while (it.next()) |entry| {
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.pending.deinit();
    }

    // -----------------------------------------------------------------------
    // Write a framed message
    // -----------------------------------------------------------------------

    fn writeFramed(self: *JsonRpcTransport, payload: []const u8) SdkError!void {
        self.write_mutex.lock();
        defer self.write_mutex.unlock();

        var header_buf: [64]u8 = undefined;
        const header = std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{payload.len}) catch
            return SdkError.WriteFailed;
        self.writer.writeAll(header) catch return SdkError.WriteFailed;
        self.writer.writeAll(payload) catch return SdkError.WriteFailed;
    }

    // -----------------------------------------------------------------------
    // Read a single framed message
    // -----------------------------------------------------------------------

    pub fn readMessage(self: *JsonRpcTransport) SdkError![]const u8 {
        // Read headers until blank line
        var content_length: ?usize = null;
        var line_buf: [512]u8 = undefined;

        while (true) {
            const line = self.readLine(&line_buf) catch return SdkError.ReadFailed;
            if (line.len == 0) break; // blank line separates headers from body

            if (std.ascii.startsWithIgnoreCase(line, "Content-Length:")) {
                const value_str = std.mem.trimLeft(u8, line["Content-Length:".len..], " \t");
                content_length = std.fmt.parseInt(usize, value_str, 10) catch return SdkError.ParseError;
            }
        }

        const len = content_length orelse return SdkError.ParseError;
        const body = self.allocator.alloc(u8, len) catch return SdkError.AllocationFailed;
        errdefer self.allocator.free(body);

        var total: usize = 0;
        while (total < len) {
            const n = self.reader.read(body[total..]) catch return SdkError.ReadFailed;
            if (n == 0) return SdkError.ConnectionClosed;
            total += n;
        }
        return body;
    }

    fn readLine(self: *JsonRpcTransport, buf: []u8) ![]const u8 {
        var i: usize = 0;
        while (i < buf.len) {
            const byte_slice = buf[i .. i + 1];
            const n = try self.reader.read(byte_slice);
            if (n == 0) return error.EndOfStream;
            if (buf[i] == '\n') {
                // Strip trailing \r\n
                var end = i;
                if (end > 0 and buf[end - 1] == '\r') end -= 1;
                return buf[0..end];
            }
            i += 1;
        }
        return buf[0..i];
    }

    // -----------------------------------------------------------------------
    // Send a JSON-RPC request and return the raw JSON response value
    // -----------------------------------------------------------------------

    pub fn sendRequest(self: *JsonRpcTransport, method: []const u8, params: ?JsonValue) SdkError!JsonValue {
        const id = self.id_gen.next();

        const entry = self.allocator.create(PendingEntry) catch return SdkError.AllocationFailed;
        entry.* = .{};
        self.pending.put(id, entry) catch return SdkError.AllocationFailed;

        // Build request JSON
        const payload = self.buildRequestPayload(id, method, params) catch return SdkError.AllocationFailed;
        defer self.allocator.free(payload);

        try self.writeFramed(payload);

        // Poll for the response (simple synchronous model)
        while (!entry.completed) {
            try self.processOneMessage();
        }

        _ = self.pending.remove(id);
        defer self.allocator.destroy(entry);

        if (entry.rpc_error != null) return SdkError.JsonRpcError;

        return entry.result orelse {
            // Return JSON null for void responses
            return JsonValue.null;
        };
    }

    // -----------------------------------------------------------------------
    // Send a notification (no id, no response expected)
    // -----------------------------------------------------------------------

    pub fn sendNotification(self: *JsonRpcTransport, method: []const u8, params: ?JsonValue) SdkError!void {
        const payload = self.buildNotificationPayload(method, params) catch return SdkError.AllocationFailed;
        defer self.allocator.free(payload);
        try self.writeFramed(payload);
    }

    // -----------------------------------------------------------------------
    // Process one incoming message
    // -----------------------------------------------------------------------

    pub fn processOneMessage(self: *JsonRpcTransport) SdkError!void {
        const raw = try self.readMessage();
        defer self.allocator.free(raw);

        const parsed = std.json.parseFromSlice(std.json.Value, self.allocator, raw, .{}) catch
            return SdkError.ParseError;
        defer parsed.deinit();
        const root = parsed.value;

        if (root != .object) return SdkError.ParseError;

        // Is it a response? (has "id" and ("result" or "error"))
        if (root.object.get("id")) |id_val| {
            const msg_id: i64 = switch (id_val) {
                .integer => |i| i,
                .float => |f| @intFromFloat(f),
                else => return SdkError.ParseError,
            };

            if (self.pending.get(msg_id)) |entry| {
                if (root.object.get("error")) |_| {
                    entry.rpc_error = .{
                        .code = -1,
                        .message = "server error",
                    };
                } else {
                    entry.result = root.object.get("result");
                }
                entry.completed = true;
                return;
            }

            // It is a server-to-client request (has "method" + "id")
            if (root.object.get("method")) |method_val| {
                if (method_val == .string) {
                    const method_str = method_val.string;
                    const params_val = root.object.get("params");
                    if (self.request_handler) |handler| {
                        const result_val = handler(method_str, params_val, msg_id);
                        self.sendResponse(msg_id, result_val) catch {};
                    } else {
                        self.sendResponse(msg_id, null) catch {};
                    }
                }
            }
            return;
        }

        // Notification (no id)
        if (root.object.get("method")) |method_val| {
            if (method_val == .string) {
                const method_str = method_val.string;
                const params_val = root.object.get("params");
                if (self.notification_handler) |handler| {
                    handler(method_str, params_val);
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Send a JSON-RPC response
    // -----------------------------------------------------------------------

    fn sendResponse(self: *JsonRpcTransport, id: i64, result: ?JsonValue) SdkError!void {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        const writer = buf.writer();
        writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":") catch return SdkError.WriteFailed;
        std.fmt.format(writer, "{d}", .{id}) catch return SdkError.WriteFailed;
        if (result) |r| {
            writer.writeAll(",\"result\":") catch return SdkError.WriteFailed;
            std.json.stringify(r, .{}, writer) catch return SdkError.WriteFailed;
        } else {
            writer.writeAll(",\"result\":null") catch return SdkError.WriteFailed;
        }
        writer.writeAll("}") catch return SdkError.WriteFailed;

        self.writeFramed(buf.items) catch return SdkError.WriteFailed;
    }

    // -----------------------------------------------------------------------
    // Payload builders
    // -----------------------------------------------------------------------

    fn buildRequestPayload(self: *JsonRpcTransport, id: i64, method: []const u8, params: ?JsonValue) ![]const u8 {
        var buf = std.ArrayList(u8).init(self.allocator);
        errdefer buf.deinit();
        const w = buf.writer();

        try w.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try std.fmt.format(w, "{d}", .{id});
        try w.writeAll(",\"method\":\"");
        try w.writeAll(method);
        try w.writeAll("\"");
        if (params) |p| {
            try w.writeAll(",\"params\":");
            try std.json.stringify(p, .{}, w);
        }
        try w.writeAll("}");
        return buf.toOwnedSlice();
    }

    fn buildNotificationPayload(self: *JsonRpcTransport, method: []const u8, params: ?JsonValue) ![]const u8 {
        var buf = std.ArrayList(u8).init(self.allocator);
        errdefer buf.deinit();
        const w = buf.writer();

        try w.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"");
        try w.writeAll(method);
        try w.writeAll("\"");
        if (params) |p| {
            try w.writeAll(",\"params\":");
            try std.json.stringify(p, .{}, w);
        }
        try w.writeAll("}");
        return buf.toOwnedSlice();
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "IdGenerator produces sequential ids" {
    var gen = IdGenerator{};
    try std.testing.expectEqual(@as(i64, 1), gen.next());
    try std.testing.expectEqual(@as(i64, 2), gen.next());
    try std.testing.expectEqual(@as(i64, 3), gen.next());
}

test "buildRequestPayload produces valid JSON" {
    const allocator = std.testing.allocator;

    var dummy_buf: [1]u8 = undefined;
    var reader_stream = std.io.fixedBufferStream(&dummy_buf);
    var writer_buf: [1]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buf);

    var transport = JsonRpcTransport.init(
        allocator,
        writer_stream.writer().any(),
        reader_stream.reader().any(),
    );
    defer transport.deinit();

    const payload = try transport.buildRequestPayload(1, "ping", null);
    defer allocator.free(payload);
    try std.testing.expectEqualStrings("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}", payload);
}

test "buildNotificationPayload produces valid JSON" {
    const allocator = std.testing.allocator;

    var dummy_buf: [1]u8 = undefined;
    var reader_stream = std.io.fixedBufferStream(&dummy_buf);
    var writer_buf: [1]u8 = undefined;
    var writer_stream = std.io.fixedBufferStream(&writer_buf);

    var transport = JsonRpcTransport.init(
        allocator,
        writer_stream.writer().any(),
        reader_stream.reader().any(),
    );
    defer transport.deinit();

    const payload = try transport.buildNotificationPayload("session.event", null);
    defer allocator.free(payload);
    try std.testing.expectEqualStrings("{\"jsonrpc\":\"2.0\",\"method\":\"session.event\"}", payload);
}
