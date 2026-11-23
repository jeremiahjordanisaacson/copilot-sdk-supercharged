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

/// Invoked once when a session is detached or freed (used by the client to drop
/// the session's GitHub token provider registration).
pub const CloseHook = struct {
    ctx: *anyopaque,
    func: *const fn (ctx: *anyopaque, session: *CopilotSession) void,
};

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
    /// Last `assistant.message` event (strings owned by `event_arena`).
    last_event: ?SessionEvent = null,
    idle: bool = true,
    /// Handlers answered through server requests routed by the client.
    hook_handler: ?types.HookHandlerFn = null,
    shell_authentication_handler: ?types.ShellAuthenticationHandlerFn = null,
    memory_store_handler: ?types.MemoryStoreHandlerFn = null,
    /// GitHub token provider registration (owned by the client) and its close hook.
    github_token_registration_id: ?[]const u8 = null,
    close_hook: ?CloseHook = null,
    /// True when `session_id` / `workspace_path` were allocated by the client.
    owns_strings: bool = false,
    event_arena: std.heap.ArenaAllocator,

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
            .event_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *CopilotSession) void {
        self.runCloseHook();
        self.tool_registry.deinit();
        self.listeners.deinit();
        self.event_arena.deinit();
        if (self.owns_strings) {
            self.allocator.free(self.session_id);
            if (self.workspace_path) |wp| self.allocator.free(wp);
        }
    }

    fn runCloseHook(self: *CopilotSession) void {
        if (self.close_hook) |hook| {
            self.close_hook = null;
            hook.func(hook.ctx, self);
        }
    }

    // -------------------------------------------------------------------
    // Sending messages
    // -------------------------------------------------------------------

    /// Send a message to the session (non-blocking). The returned message ID stays
    /// valid until the next request on the client.
    pub fn send(self: *CopilotSession, opts: MessageOptions) SdkError![]const u8 {
        self.idle = false;
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const a = arena.allocator();
        var obj = std.json.ObjectMap.init(a);

        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        obj.put("prompt", .{ .string = opts.prompt }) catch return SdkError.AllocationFailed;
        if (opts.mode) |m| {
            obj.put("mode", .{ .string = m }) catch return SdkError.AllocationFailed;
        }
        if (opts.agent_mode) |am| {
            obj.put("agentMode", .{ .string = am }) catch return SdkError.AllocationFailed;
        }
        if (opts.display_prompt) |dp| {
            obj.put("displayPrompt", .{ .string = dp }) catch return SdkError.AllocationFailed;
        }
        if (opts.source) |source| {
            obj.put("source", .{ .string = source }) catch return SdkError.AllocationFailed;
        }
        if (opts.client_turn_id) |turn_id| {
            obj.put("clientTurnId", .{ .string = turn_id }) catch return SdkError.AllocationFailed;
        }
        if (opts.response_schema_json) |schema_text| {
            // Structured output: responseFormat {type: "json_schema", jsonSchema: {...}}
            const schema = std.json.parseFromSliceLeaky(JsonValue, a, schema_text, .{}) catch
                return SdkError.ParseError;
            var json_schema = std.json.ObjectMap.init(a);
            json_schema.put("name", .{ .string = "response" }) catch return SdkError.AllocationFailed;
            json_schema.put("strict", .{ .bool = true }) catch return SdkError.AllocationFailed;
            json_schema.put("schema", schema) catch return SdkError.AllocationFailed;
            var format = std.json.ObjectMap.init(a);
            format.put("type", .{ .string = "json_schema" }) catch return SdkError.AllocationFailed;
            format.put("jsonSchema", .{ .object = json_schema }) catch return SdkError.AllocationFailed;
            obj.put("responseFormat", .{ .object = format }) catch return SdkError.AllocationFailed;
        }

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
    /// Returns the last `assistant.message` event of the turn, if any.
    pub fn sendAndWait(self: *CopilotSession, opts: MessageOptions, timeout_ms: ?u64) SdkError!?SessionEvent {
        self.last_event = null;
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

    /// Deliver an event to all matching listeners. Event strings are only valid
    /// during the callbacks; the last assistant message is copied for `sendAndWait`.
    pub fn emitEvent(self: *CopilotSession, event: SessionEvent) void {
        if (event.assistantMessageContent() != null) self.rememberEvent(event);
        if (event.isIdle()) self.idle = true;

        for (self.listeners.items) |listener| {
            if (listener.event_type) |wanted| {
                if (!std.mem.eql(u8, wanted, event.event_type)) continue;
            }
            listener.callback(event);
        }
    }

    fn rememberEvent(self: *CopilotSession, event: SessionEvent) void {
        _ = self.event_arena.reset(.retain_capacity);
        const a = self.event_arena.allocator();
        var copy = event;
        copy.id = a.dupe(u8, event.id) catch "";
        copy.timestamp = a.dupe(u8, event.timestamp) catch "";
        copy.session_id = a.dupe(u8, event.session_id) catch "";
        copy.event_type = a.dupe(u8, event.event_type) catch "";
        copy.parent_id = dupeOptional(a, event.parent_id);
        copy.agent_id = dupeOptional(a, event.agent_id);
        copy.content = dupeOptional(a, event.content);
        copy.delta_content = dupeOptional(a, event.delta_content);
        copy.raw_json = dupeOptional(a, event.raw_json);
        copy.shell_output = null;
        self.last_event = copy;
    }

    fn dupeOptional(a: Allocator, value: ?[]const u8) ?[]const u8 {
        const v = value orelse return null;
        return a.dupe(u8, v) catch null;
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

    /// Runs the registered handler for an `external_tool.requested` event and answers
    /// it with `session.tools.handlePendingToolCall`. Requests for tools without a
    /// handler in this session are left to other clients. Called by the client.
    pub fn handleExternalToolRequest(self: *CopilotSession, arena: Allocator, data: JsonValue) void {
        if (data != .object) return;
        const obj = data.object;
        if (obj.get("hasHandler")) |has_handler| {
            if (has_handler == .bool and !has_handler.bool) return;
        }
        const request_id = stringField(obj, "requestId") orelse return;
        const tool_name = stringField(obj, "toolName") orelse return;
        const handler = self.tool_registry.findHandler(tool_name) orelse return;

        const arguments_json: []const u8 = if (obj.get("arguments")) |args|
            (std.json.stringifyAlloc(arena, args, .{}) catch return)
        else
            "{}";

        var params = std.json.ObjectMap.init(arena);
        params.put("sessionId", .{ .string = self.session_id }) catch return;
        params.put("requestId", .{ .string = request_id }) catch return;
        if (handler(.{
            .tool_call_id = stringField(obj, "toolCallId") orelse "",
            .name = tool_name,
            .arguments_json = arguments_json,
            .session_id = self.session_id,
        })) |result| {
            var result_obj = std.json.ObjectMap.init(arena);
            result_obj.put("textResultForLlm", .{ .string = result.text_result_for_llm }) catch return;
            result_obj.put("resultType", .{ .string = @tagName(result.result_type) }) catch return;
            params.put("result", .{ .object = result_obj }) catch return;
        } else |err| {
            params.put("error", .{ .string = @errorName(err) }) catch return;
        }
        _ = self.transport.sendRequest("session.tools.handlePendingToolCall", .{ .object = params }) catch {};
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

    /// Release the session on the server (`session.detach`, falling back to
    /// `session.destroy` on CLIs that predate it). On-disk session state is kept,
    /// so the session can be resumed later.
    pub fn detach(self: *CopilotSession) SdkError!void {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        if (self.transport.sendRequest("session.detach", .{ .object = obj })) |_| {} else |err| {
            if (err != SdkError.JsonRpcError or !self.transport.lastErrorIsMethodNotFound()) return err;
            _ = try self.transport.sendRequest("session.destroy", .{ .object = obj });
        }
        self.runCloseHook();
    }

    /// Destroy the session on the server side (alias of `detach`).
    pub fn destroy(self: *CopilotSession) SdkError!void {
        try self.detach();
    }

    /// Abort the current processing turn. Failures surface as `SdkError.JsonRpcError`.
    pub fn abort(self: *CopilotSession) SdkError!void {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        _ = try self.transport.sendRequest("session.abort", .{ .object = obj });
    }

    /// Change the Auto routing preference without changing the selected model
    /// (`session.model.switchAutoTier`). Pass null to return to provider-default routing.
    pub fn setAutoTier(self: *CopilotSession, auto_tier: ?[]const u8) SdkError!JsonValue {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        const tier: JsonValue = if (auto_tier) |t| .{ .string = t } else .null;
        obj.put("autoTier", tier) catch return SdkError.AllocationFailed;
        return self.transport.sendRequest("session.model.switchAutoTier", .{ .object = obj });
    }

    /// Effective managed settings for this session (`session.managedSettings.get`).
    /// Read a Policy Helper failure from it with `types.PolicyHelperFailure.fromJson`.
    pub fn getManagedSettings(self: *CopilotSession) SdkError!JsonValue {
        return self.sessionRequest("session.managedSettings.get");
    }

    /// Re-run the managed-settings Policy Helper (`session.managedSettings.recheckPolicyHelper`).
    pub fn recheckPolicyHelper(self: *CopilotSession) SdkError!JsonValue {
        return self.sessionRequest("session.managedSettings.recheckPolicyHelper");
    }

    /// Request a permission mode (see `types.PermissionMode`) via `session.permissions.setMode`.
    /// When managed policy refuses the change, `success` is false and `reason` is
    /// `types.PermissionModeRefusalReason.managed_policy`.
    pub fn setPermissionMode(self: *CopilotSession, mode: []const u8) SdkError!types.PermissionModeResult {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        obj.put("mode", .{ .string = mode }) catch return SdkError.AllocationFailed;
        const result = try self.transport.sendRequest("session.permissions.setMode", .{ .object = obj });
        var out = types.PermissionModeResult{};
        if (result == .object) {
            if (result.object.get("success")) |v| {
                if (v == .bool) out.success = v.bool;
            }
            if (result.object.get("mode")) |v| {
                if (v == .string) out.mode = v.string;
            }
            if (result.object.get("reason")) |v| {
                if (v == .string) out.reason = v.string;
            }
        }
        return out;
    }

    fn sessionRequest(self: *CopilotSession, method: []const u8) SdkError!JsonValue {
        var obj = std.json.ObjectMap.init(self.allocator);
        defer obj.deinit();
        obj.put("sessionId", .{ .string = self.session_id }) catch return SdkError.AllocationFailed;
        return self.transport.sendRequest(method, .{ .object = obj });
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
// Helpers
// ---------------------------------------------------------------------------

fn stringField(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const value = obj.get(key) orelse return null;
    return if (value == .string) value.string else null;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn noopCallback(_: SessionEvent) void {}

/// Frames a JSON-RPC message the way the CLI sends it.
fn framedMessage(comptime body: []const u8) []const u8 {
    return std.fmt.comptimePrint("Content-Length: {d}\r\n\r\n{s}", .{ body.len, body });
}

test "CopilotSession.send forwards source, clientTurnId, and structured output" {
    const allocator = std.testing.allocator;
    const responses = comptime framedMessage("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"messageId\":\"m1\"}}") ++
        framedMessage("{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"success\":false,\"mode\":\"manual\",\"reason\":\"managed_policy\"}}");
    var input = std.io.fixedBufferStream(responses);
    const input_reader = input.reader();
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();
    const output_writer = output.writer();

    var transport = jsonrpc.JsonRpcTransport.init(allocator, output_writer.any(), input_reader.any());
    defer transport.deinit();
    var session = CopilotSession.init(allocator, "s1", null, &transport, null);
    defer session.deinit();

    const source = try types.MessageSource.agent(allocator, "reviewer");
    defer allocator.free(source);
    const message_id = try session.send(.{
        .prompt = "Summarize",
        .source = source,
        .client_turn_id = "turn-1",
        .response_schema_json = "{\"type\":\"object\"}",
    });
    try std.testing.expectEqualStrings("m1", message_id);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"source\":\"agent-reviewer\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"clientTurnId\":\"turn-1\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items,
        \\"responseFormat":{"type":"json_schema","jsonSchema":{"name":"response","strict":true,"schema":{"type":"object"}}}
    ) != null);

    const mode = try session.setPermissionMode(types.PermissionMode.allow_all);
    try std.testing.expect(!mode.success);
    try std.testing.expectEqualStrings(types.PermissionModeRefusalReason.managed_policy, mode.reason.?);
}

test "CopilotSession answers external tool requests with its registered handler" {
    const allocator = std.testing.allocator;
    const S = struct {
        fn lookup(invocation: ToolInvocation) anyerror!ToolResult {
            const found = std.mem.indexOf(u8, invocation.arguments_json, "\"q\":\"zig\"") != null;
            return .{ .text_result_for_llm = if (found) "found zig" else "missing" };
        }
    };
    const responses = comptime framedMessage("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"success\":true}}");
    var input = std.io.fixedBufferStream(responses);
    const input_reader = input.reader();
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();
    const output_writer = output.writer();

    var transport = jsonrpc.JsonRpcTransport.init(allocator, output_writer.any(), input_reader.any());
    defer transport.deinit();
    var session = CopilotSession.init(allocator, "s1", null, &transport, null);
    defer session.deinit();
    try session.registerTool(.{ .name = "lookup" }, S.lookup);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const data = try std.json.parseFromSliceLeaky(JsonValue, arena.allocator(),
        \\{"requestId":"r1","toolName":"lookup","toolCallId":"c1","arguments":{"q":"zig"}}
    , .{});
    session.handleExternalToolRequest(arena.allocator(), data);

    try std.testing.expect(std.mem.indexOf(u8, output.items, "\"method\":\"session.tools.handlePendingToolCall\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items,
        \\"requestId":"r1","result":{"textResultForLlm":"found zig","resultType":"success"}
    ) != null);
}

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
