// client.zig -- CopilotClient: the primary entry point for the Zig SDK.
//
// Spawns (or connects to) the Copilot CLI server, manages the JSON-RPC
// transport, and creates / resumes sessions.

const std = @import("std");
const builtin = @import("builtin");
const types = @import("types.zig");
const jsonrpc = @import("jsonrpc.zig");
const session_mod = @import("session.zig");
const tools_mod = @import("tools.zig");

const Allocator = std.mem.Allocator;
const JsonValue = types.JsonValue;
const SdkError = types.SdkError;

pub const ClientOptions = types.ClientOptions;
pub const SessionConfig = types.SessionConfig;
pub const ConnectionState = types.ConnectionState;
pub const ServerStatus = types.ServerStatus;
pub const AuthStatus = types.AuthStatus;
pub const SessionMetadata = types.SessionMetadata;
pub const SessionEvent = types.SessionEvent;
pub const CopilotSession = session_mod.CopilotSession;

// ---------------------------------------------------------------------------
// Lifecycle callback
// ---------------------------------------------------------------------------

pub const LifecycleCallback = *const fn (state: ConnectionState) void;

// ---------------------------------------------------------------------------
// CopilotClient
// ---------------------------------------------------------------------------

pub const CopilotClient = struct {
    allocator: Allocator,
    options: ClientOptions,
    state: ConnectionState = .disconnected,
    transport: ?jsonrpc.JsonRpcTransport = null,
    process: ?std.process.Child = null,
    sessions: std.StringHashMap(*CopilotSession),
    lifecycle_callbacks: std.ArrayList(LifecycleCallback),

    // Buffers owned by the client for process I/O
    stdout_buf: ?[]u8 = null,
    stderr_buf: ?[]u8 = null,

    /// Create a new client with the given options.
    pub fn init(allocator: Allocator, options: ClientOptions) CopilotClient {
        return .{
            .allocator = allocator,
            .options = options,
            .sessions = std.StringHashMap(*CopilotSession).init(allocator),
            .lifecycle_callbacks = std.ArrayList(LifecycleCallback).init(allocator),
        };
    }

    pub fn deinit(self: *CopilotClient) void {
        self.stopInternal(false);
        self.sessions.deinit();
        self.lifecycle_callbacks.deinit();
    }

    // -------------------------------------------------------------------
    // Connection lifecycle
    // -------------------------------------------------------------------

    /// Start the client: spawn the CLI process (or connect to an external
    /// server) and verify the protocol version.
    pub fn start(self: *CopilotClient) SdkError!void {
        self.setState(.connecting);
        errdefer self.setState(.error_state);

        if (self.options.cli_url) |_| {
            // External server -- not yet implemented in this minimal SDK.
            // A full implementation would open a TCP connection here.
            return SdkError.NotConnected;
        }

        try self.spawnCliProcess();
        try self.verifyProtocol();

        // Set up session filesystem provider if configured
        if (self.options.session_fs) |fs| {
            var params = std.json.Value{ .object = std.json.ObjectMap.init(self.allocator) };
            try params.object.put("initialCwd", .{ .string = fs.initial_cwd });
            try params.object.put("sessionStatePath", .{ .string = fs.session_state_path });
            try params.object.put("conventions", .{ .string = fs.conventions });
            _ = try self.sendRequest("sessionFs.setProvider", params);
        }

        self.setState(.connected);
    }

    /// Graceful stop: destroy all sessions, then kill the process.
    pub fn stop(self: *CopilotClient) SdkError!void {
        self.stopInternal(true);
    }

    /// Force-stop without graceful session teardown.
    pub fn forceStop(self: *CopilotClient) void {
        self.stopInternal(false);
    }

    fn stopInternal(self: *CopilotClient, graceful: bool) void {
        if (graceful) {
            // Destroy each session
            var it = self.sessions.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.destroy() catch {};
                entry.value_ptr.*.deinit();
                self.allocator.destroy(entry.value_ptr.*);
            }
            self.sessions.clearAndFree();
        } else {
            var it = self.sessions.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit();
                self.allocator.destroy(entry.value_ptr.*);
            }
            self.sessions.clearAndFree();
        }

        if (self.transport) |*t| {
            t.deinit();
            self.transport = null;
        }

        if (self.process) |*proc| {
            _ = proc.kill() catch {};
            _ = proc.wait() catch {};
            self.process = null;
        }

        self.setState(.disconnected);
    }

    // -------------------------------------------------------------------
    // Session management
    // -------------------------------------------------------------------

    /// Create a new conversation session.
    pub fn createSession(self: *CopilotClient, config: SessionConfig) SdkError!*CopilotSession {
        if (self.state != .connected) return SdkError.NotConnected;

        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        try self.populateSessionParams(&params, config);

        const result = try self.transport.?.sendRequest("session.create", .{ .object = params });
        return self.sessionFromResult(result, config);
    }

    /// Resume a previously created session by ID.
    pub fn resumeSession(self: *CopilotClient, session_id: []const u8, config: SessionConfig) SdkError!*CopilotSession {
        if (self.state != .connected) return SdkError.NotConnected;

        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        try self.populateSessionParams(&params, config);

        const result = try self.transport.?.sendRequest("session.resume", .{ .object = params });
        return self.sessionFromResult(result, config);
    }

    /// Delete a session on the server side.
    pub fn deleteSession(self: *CopilotClient, session_id: []const u8) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;

        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        _ = try self.transport.?.sendRequest("session.delete", .{ .object = params });

        if (self.sessions.get(session_id)) |sess| {
            sess.deinit();
            self.allocator.destroy(sess);
        }
        _ = self.sessions.remove(session_id);
    }

    /// List all session IDs known to the server.
    pub fn listSessions(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("session.list", null);
    }

    // -------------------------------------------------------------------
    // Utility RPCs
    // -------------------------------------------------------------------

    /// Ping the server; returns the echoed message.
    pub fn ping(self: *CopilotClient, message: []const u8) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("message", .{ .string = message }) catch return SdkError.AllocationFailed;
        return try self.transport.?.sendRequest("ping", .{ .object = params });
    }

    /// Get server version and protocol information.
    pub fn getStatus(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("status.get", null);
    }

    /// Get authentication status.
    pub fn getAuthStatus(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("auth.getStatus", null);
    }

    /// List available models.
    pub fn listModels(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("models.list", null);
    }

    /// Get session metadata by session ID.
    pub fn getSessionMetadata(self: *CopilotClient, session_id: []const u8) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        return try self.transport.?.sendRequest("session.getMetadata", .{ .object = params });
    }

    /// Get the foreground session ID.
    pub fn getForegroundSessionId(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("session.getForeground", .{ .object = std.json.ObjectMap.init(self.allocator) });
    }

    /// Set the foreground session ID.
    pub fn setForegroundSessionId(self: *CopilotClient, session_id: []const u8) SdkError!void {
        if (self.state != .connected) return SdkError.NotConnected;
        var params = std.json.ObjectMap.init(self.allocator);
        defer params.deinit();
        params.put("sessionId", .{ .string = session_id }) catch return SdkError.AllocationFailed;
        const result = try self.transport.?.sendRequest("session.setForeground", .{ .object = params });
        if (result == .object) {
            if (result.object.get("success")) |sv| {
                if (sv == .bool and sv.bool) return;
            }
        }
        return SdkError.RequestFailed;
    }

    /// Get the last session ID.
    pub fn getLastSessionId(self: *CopilotClient) SdkError!JsonValue {
        if (self.state != .connected) return SdkError.NotConnected;
        return try self.transport.?.sendRequest("session.getLastId", .{ .object = std.json.ObjectMap.init(self.allocator) });
    }

    /// Get the current connection state.
    pub fn getState(self: *const CopilotClient) ConnectionState {
        return self.state;
    }

    /// Subscribe to lifecycle state changes.
    pub fn onLifecycle(self: *CopilotClient, callback: LifecycleCallback) void {
        self.lifecycle_callbacks.append(callback) catch {};
    }

    // -------------------------------------------------------------------
    // Internal helpers
    // -------------------------------------------------------------------

    fn setState(self: *CopilotClient, new_state: ConnectionState) void {
        self.state = new_state;
        for (self.lifecycle_callbacks.items) |cb| {
            cb(new_state);
        }
    }

    fn spawnCliProcess(self: *CopilotClient) SdkError!void {
        const cli_path = self.options.cli_path orelse "copilot-cli";

        var argv = std.ArrayList([]const u8).init(self.allocator);
        defer argv.deinit();

        argv.append(cli_path) catch return SdkError.AllocationFailed;
        argv.append("--headless") catch return SdkError.AllocationFailed;
        argv.append("--no-auto-update") catch return SdkError.AllocationFailed;
        argv.append("--log-level") catch return SdkError.AllocationFailed;
        argv.append(self.options.log_level) catch return SdkError.AllocationFailed;

        if (self.options.use_stdio) {
            argv.append("--stdio") catch return SdkError.AllocationFailed;
        }

        if (self.options.cli_args) |extra| {
            for (extra) |arg| {
                argv.append(arg) catch return SdkError.AllocationFailed;
            }
        }

        var child = std.process.Child.init(argv.items, self.allocator);
        child.stdin_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        child.spawn() catch return SdkError.ProcessSpawn;

        self.process = child;

        // Set up transport over stdin/stdout of the child process
        const stdin_writer = child.stdin.?.writer().any();
        const stdout_reader = child.stdout.?.reader().any();

        self.transport = jsonrpc.JsonRpcTransport.init(self.allocator, stdin_writer, stdout_reader);
    }

    fn verifyProtocol(self: *CopilotClient) SdkError!void {
        const result = try self.ping("hello");
        // Check for protocol version in response
        if (result == .object) {
            if (result.object.get("protocolVersion")) |pv| {
                const version: i64 = switch (pv) {
                    .integer => |i| i,
                    .float => |f| @intFromFloat(f),
                    else => 0,
                };
                if (version != types.PROTOCOL_VERSION) {
                    return SdkError.ProtocolMismatch;
                }
            }
        }
    }

    fn populateSessionParams(self: *CopilotClient, params: *std.json.ObjectMap, config: SessionConfig) SdkError!void {
        _ = self;
        if (config.model) |m| {
            params.put("model", .{ .string = m }) catch return SdkError.AllocationFailed;
        }
        if (config.system_prompt) |sp| {
            params.put("systemPrompt", .{ .string = sp }) catch return SdkError.AllocationFailed;
        }
        if (config.streaming) {
            params.put("streaming", .{ .bool = true }) catch return SdkError.AllocationFailed;
        }
    }

    fn sessionFromResult(self: *CopilotClient, result: JsonValue, config: SessionConfig) SdkError!*CopilotSession {
        _ = config;
        var sid: []const u8 = "";
        var wpath: ?[]const u8 = null;

        if (result == .object) {
            if (result.object.get("sessionId")) |sv| {
                if (sv == .string) sid = sv.string;
            }
            if (result.object.get("workspacePath")) |wp| {
                if (wp == .string) wpath = wp.string;
            }
        }

        const sess = self.allocator.create(CopilotSession) catch return SdkError.AllocationFailed;
        sess.* = CopilotSession.init(self.allocator, sid, wpath, &self.transport.?);

        self.sessions.put(sid, sess) catch return SdkError.AllocationFailed;
        return sess;
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "CopilotClient init and deinit" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();
    try std.testing.expectEqual(ConnectionState.disconnected, client.getState());
}

test "CopilotClient default options" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{
        .cli_path = "/usr/bin/copilot-cli",
    });
    defer client.deinit();
    try std.testing.expect(client.options.use_stdio);
    try std.testing.expect(client.options.auto_start);
}

test "CopilotClient lifecycle callback fires on state change" {
    const allocator = std.testing.allocator;

    const S = struct {
        var last_state: ConnectionState = .disconnected;
        fn handler(state: ConnectionState) void {
            last_state = state;
        }
    };

    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();

    client.onLifecycle(S.handler);
    client.setState(.connecting);
    try std.testing.expectEqual(ConnectionState.connecting, S.last_state);
}

test "CopilotClient getState returns current state" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();
    try std.testing.expectEqual(ConnectionState.disconnected, client.getState());
}

test "CopilotClient operations fail when not connected" {
    const allocator = std.testing.allocator;
    var client = CopilotClient.init(allocator, .{});
    defer client.deinit();

    const result = client.createSession(.{});
    try std.testing.expectError(SdkError.NotConnected, result);
}
