// session.zig -- CopilotSession wraps a single conversation session.
//
// A session is created by the client via `session.create` / `session.resume`
// and provides send, event subscription, tool registration, and lifecycle
// management.

const std = @import("std");
const types = @import("types.zig");
const tools_mod = @import("tools.zig");
const jsonrpc = @import("jsonrpc.zig");

const Allocator = std.mem.Allocator;
const JsonValue = types.JsonValue;
const SdkError = types.SdkError;
const SessionEvent = types.SessionEvent;
const MessageOptions = types.MessageOptions;
const ToolInvocation = types.ToolInvocation;
const ToolResult = types.ToolResult;
const PermissionRequest = types.PermissionRequest;
const PermissionResult = types.PermissionResult;
const UserInputRequest = types.UserInputRequest;
const UserInputResponse = types.UserInputResponse;
const ToolRegistry = tools_mod.ToolRegistry;
const ToolDefinition = types.ToolDefinition;
const ToolHandlerFn = tools_mod.ToolHandlerFn;

// ---------------------------------------------------------------------------
// Callback types
// ---------------------------------------------------------------------------

pub const EventCallback = *const fn (event: SessionEvent) void;
pub const PermissionHandlerFn = *const fn (req: PermissionRequest, session_id: []const u8) anyerror!PermissionResult;
pub const UserInputHandlerFn = *const fn (req: UserInputRequest, session_id: []const u8) anyerror!UserInputResponse;
pub const ExitPlanModeHandlerFn = *const fn (req: types.ExitPlanModeRequest, session_id: []const u8) anyerror!types.ExitPlanModeResponse;

// ---------------------------------------------------------------------------
// EventSubscription
// ---------------------------------------------------------------------------

pub const EventSubscription = struct {
    id: u64,
    session: *CopilotSession,

    /// Remove this subscription.
    pub fn unsubscribe(self: EventSubscription) void {
        self.session.removeSubscription(self.id);
    }
};

// ---------------------------------------------------------------------------
// CopilotSession
// ---------------------------------------------------------------------------

pub const CopilotSession = struct {
    allocator: Allocator,
    session_id: []const u8,
    workspace_path: ?[]const u8 = null,
    transport: *jsonrpc.JsonRpcTransport,
    tool_registry: ToolRegistry,
    listeners: std.ArrayList(Listener),
    next_listener_id: u64 = 1,
    permission_handler: ?PermissionHandlerFn = null,
    user_input_handler: ?UserInputHandlerFn = null,
    exit_plan_mode_handler: ?ExitPlanModeHandlerFn = null,
    trace_context_provider: ?types.TraceContextProvider = null,
    last_event: ?SessionEvent = null,
    idle: bool = true,

    const Listener = struct {
        id: u64,
        event_type: ?[]const u8, // null means all events
        callback: EventCallback,
    };

    pub fn init(
        allocator: Allocator,
        session_id: []const u8,
        workspace_path: ?[]const u8,
        transport: *jsonrpc.JsonRpcTransport,
        trace_context_provider: ?types.TraceContextProvider,
    ) CopilotSession {
        return .{
            .allocator = allocator,
            .session_id = session_id,
            .workspace_path = workspace_path,
            .transport = transport,
            .trace_context_provider = trace_context_provider,
            .tool_registry = ToolRegistry.init(allocator),
            .listeners = std.ArrayList(Listener).init(allocator),
        };
    }

    pub fn deinit(self: *CopilotSession) void {
        self.tool_registry.deinit();
        self.listeners.deinit();
    }

    // -------------------------------------------------------------------
    // Sending messages
    // -------------------------------------------------------------------

    /// Send a message to the session (non-blocking).
    pub fn send(self: *CopilotSession, opts: MessageOptions) SdkError![]const u8 {
        self.idle = false;
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();

        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        obj.put("prompt", .{ .string = opts.prompt }) catch return SdkError.AllocationFailed;

        const params = JsonValue{ .object = obj };
        const result = try self.transport.sendRequest("session.send", params);

        // Extract messageId from result
        if (result == .object) {
            if (result.object.get("messageId")) |mid| {
                if (mid == .string) return mid.string;
            }
        }
        return "";
    }

    /// Send a message and block until the session becomes idle or times out.
    pub fn sendAndWait(self: *CopilotSession, opts: MessageOptions, timeout_ms: ?u64) SdkError!?SessionEvent {
        _ = try self.send(opts);

        const deadline = timeout_ms orelse 60_000;
        const start = std.time.milliTimestamp();

        while (!self.idle) {
            const elapsed: u64 = @intCast(std.time.milliTimestamp() - start);
            if (elapsed >= deadline) return SdkError.Timeout;
            self.transport.processOneMessage() catch |err| {
                if (err == SdkError.ConnectionClosed) return SdkError.ConnectionClosed;
                // For other transient errors, keep trying
                continue;
            };
        }

        return self.last_event;
    }

    // -------------------------------------------------------------------
    // Event subscriptions
    // -------------------------------------------------------------------

    /// Subscribe to all session events.
    pub fn on(self: *CopilotSession, callback: EventCallback) EventSubscription {
        const id = self.next_listener_id;
        self.next_listener_id += 1;
        self.listeners.append(.{
            .id = id,
            .event_type = null,
            .callback = callback,
        }) catch {};
        return .{ .id = id, .session = self };
    }

    /// Subscribe to events of a specific type (e.g. "assistant.message").
    pub fn onEvent(self: *CopilotSession, event_type: []const u8, callback: EventCallback) EventSubscription {
        const id = self.next_listener_id;
        self.next_listener_id += 1;
        self.listeners.append(.{
            .id = id,
            .event_type = event_type,
            .callback = callback,
        }) catch {};
        return .{ .id = id, .session = self };
    }

    /// Remove a subscription by id.
    pub fn removeSubscription(self: *CopilotSession, sub_id: u64) void {
        var i: usize = 0;
        while (i < self.listeners.items.len) {
            if (self.listeners.items[i].id == sub_id) {
                _ = self.listeners.orderedRemove(i);
                return;
            }
            i += 1;
        }
    }

    /// Deliver an event to all matching listeners.
    pub fn emitEvent(self: *CopilotSession, event: SessionEvent) void {
        self.last_event = event;
        if (event.isIdle()) self.idle = true;

        for (self.listeners.items) |listener| {
            if (listener.event_type) |wanted| {
                if (!std.mem.eql(u8, wanted, event.event_type)) continue;
            }
            listener.callback(event);
        }
    }

    // -------------------------------------------------------------------
    // Tool registration
    // -------------------------------------------------------------------

    /// Register a tool handler for this session.
    pub fn registerTool(self: *CopilotSession, definition: types.ToolDefinition, handler: ToolHandlerFn) !void {
        try self.tool_registry.register(definition, handler);
    }

    /// Handle an incoming tool call from the server.
    pub fn handleToolCall(self: *CopilotSession, invocation: ToolInvocation) anyerror!ToolResult {
        return self.tool_registry.dispatch(invocation);
    }

    // -------------------------------------------------------------------
    // Permission & user input handlers
    // -------------------------------------------------------------------

    pub fn registerPermissionHandler(self: *CopilotSession, handler: PermissionHandlerFn) void {
        self.permission_handler = handler;
    }

    pub fn registerUserInputHandler(self: *CopilotSession, handler: UserInputHandlerFn) void {
        self.user_input_handler = handler;
    }

    pub fn registerExitPlanModeHandler(self: *CopilotSession, handler: ExitPlanModeHandlerFn) void {
        self.exit_plan_mode_handler = handler;
    }

    // -------------------------------------------------------------------
    // Lifecycle
    // -------------------------------------------------------------------

    /// Destroy the session on the server side.
    pub fn destroy(self: *CopilotSession) SdkError!void {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        _ = try self.transport.sendRequest("session.destroy", .{ .object = obj });
    }

    /// Abort the current processing turn.
    pub fn abort(self: *CopilotSession) SdkError!void {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        _ = try self.transport.sendRequest("session.abort", .{ .object = obj });
    }

    /// Get the list of messages in this session.
    pub fn getMessages(self: *CopilotSession) SdkError!JsonValue {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        return try self.transport.sendRequest("session.getMessages", .{ .object = obj });
    }

    /// Get the session ID.
    pub fn getId(self: *const CopilotSession) []const u8 {
        return self.session_id;
    }

    /// Get the workspace path, if set.
    pub fn getWorkspacePath(self: *const CopilotSession) ?[]const u8 {
        return self.workspace_path;
    }
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn noopCallback(_: SessionEvent) void {}

test "CopilotSession event subscribe and unsubscribe" {
    const allocator = std.testing.allocator;

    var dummy_r: [1]u8 = undefined;
    var rstream = std.io.fixedBufferStream(&dummy_r);
    var dummy_w: [1]u8 = undefined;
    var wstream = std.io.fixedBufferStream(&dummy_w);

    var transport = jsonrpc.JsonRpcTransport.init(
        allocator,
        wstream.writer().any(),
        rstream.reader().any(),
    );
    defer transport.deinit();

    var session = CopilotSession.init(allocator, "test-session", null, &transport, null);
    defer session.deinit();

    const sub = session.on(noopCallback);
    try std.testing.expectEqual(@as(usize, 1), session.listeners.items.len);
    sub.unsubscribe();
    try std.testing.expectEqual(@as(usize, 0), session.listeners.items.len);
}

test "CopilotSession emitEvent sets idle" {
    const allocator = std.testing.allocator;

    var dummy_r: [1]u8 = undefined;
    var rstream = std.io.fixedBufferStream(&dummy_r);
    var dummy_w: [1]u8 = undefined;
    var wstream = std.io.fixedBufferStream(&dummy_w);

    var transport = jsonrpc.JsonRpcTransport.init(
        allocator,
        wstream.writer().any(),
        rstream.reader().any(),
    );
    defer transport.deinit();

    var session = CopilotSession.init(allocator, "test-session", null, &transport, null);
    defer session.deinit();
    session.idle = false;

    session.emitEvent(.{ .event_type = "session.idle" });
    try std.testing.expect(session.idle);
}

test "CopilotSession getId returns session_id" {
    const allocator = std.testing.allocator;

    var dummy_r: [1]u8 = undefined;
    var rstream = std.io.fixedBufferStream(&dummy_r);
    var dummy_w: [1]u8 = undefined;
    var wstream = std.io.fixedBufferStream(&dummy_w);

    var transport = jsonrpc.JsonRpcTransport.init(
        allocator,
        wstream.writer().any(),
        rstream.reader().any(),
    );
    defer transport.deinit();

    var session = CopilotSession.init(allocator, "abc-123", null, &transport, null);
    defer session.deinit();
    try std.testing.expectEqualStrings("abc-123", session.getId());
}
