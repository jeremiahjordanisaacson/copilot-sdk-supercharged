// tools.zig -- Tool definition helpers for the Copilot SDK.
//
// Provides a convenient API for declaring tools at comptime and for
// dispatching incoming tool-call requests to the correct handler.

const std = @import("std");
const types = @import("types.zig");

pub const ToolDefinition = types.ToolDefinition;
pub const ToolInvocation = types.ToolInvocation;
pub const ToolResult = types.ToolResult;
pub const ToolResultType = types.ToolResultType;

// ---------------------------------------------------------------------------
// Tool handler function signature
// ---------------------------------------------------------------------------

pub const ToolHandlerFn = *const fn (invocation: ToolInvocation) anyerror!ToolResult;

// ---------------------------------------------------------------------------
// Registered tool (definition + handler)
// ---------------------------------------------------------------------------

pub const RegisteredTool = struct {
    definition: ToolDefinition,
    handler: ToolHandlerFn,
};

// ---------------------------------------------------------------------------
// ToolRegistry -- a simple array-backed registry
// ---------------------------------------------------------------------------

pub const ToolRegistry = struct {
    tools: std.ArrayList(RegisteredTool),

    pub fn init(allocator: std.mem.Allocator) ToolRegistry {
        return .{
            .tools = std.ArrayList(RegisteredTool).init(allocator),
        };
    }

    pub fn deinit(self: *ToolRegistry) void {
        self.tools.deinit();
    }

    /// Register a tool with its handler.
    pub fn register(self: *ToolRegistry, definition: ToolDefinition, handler: ToolHandlerFn) !void {
        try self.tools.append(.{
            .definition = definition,
            .handler = handler,
        });
    }

    /// Look up a tool handler by name. Returns null if not found.
    pub fn findHandler(self: *const ToolRegistry, name: []const u8) ?ToolHandlerFn {
        for (self.tools.items) |entry| {
            if (std.mem.eql(u8, entry.definition.name, name)) {
                return entry.handler;
            }
        }
        return null;
    }

    /// Dispatch a tool invocation to the appropriate handler.
    pub fn dispatch(self: *const ToolRegistry, invocation: ToolInvocation) anyerror!ToolResult {
        if (self.findHandler(invocation.name)) |handler| {
            return handler(invocation);
        }
        return ToolResult{
            .text_result_for_llm = "Unknown tool",
            .result_type = .failure,
        };
    }

    /// Return a slice of all registered tool definitions (for session config).
    pub fn definitions(self: *const ToolRegistry) []const RegisteredTool {
        return self.tools.items;
    }
};

// ---------------------------------------------------------------------------
// Comptime helper: build a ToolDefinition literal
// ---------------------------------------------------------------------------

pub fn defineTool(
    comptime name: []const u8,
    comptime description: ?[]const u8,
    comptime parameters_json: ?[]const u8,
) ToolDefinition {
    return .{
        .name = name,
        .description = description,
        .parameters_json = parameters_json,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

fn dummyHandler(_: ToolInvocation) anyerror!ToolResult {
    return ToolResult{
        .text_result_for_llm = "ok",
        .result_type = .success,
    };
}

test "ToolRegistry register and find" {
    const allocator = std.testing.allocator;
    var registry = ToolRegistry.init(allocator);
    defer registry.deinit();

    const def = defineTool("greet", "Say hello", null);
    try registry.register(def, dummyHandler);

    try std.testing.expect(registry.findHandler("greet") != null);
    try std.testing.expect(registry.findHandler("nonexistent") == null);
}

test "ToolRegistry dispatch calls handler" {
    const allocator = std.testing.allocator;
    var registry = ToolRegistry.init(allocator);
    defer registry.deinit();

    try registry.register(defineTool("ping", null, null), dummyHandler);

    const invocation = ToolInvocation{
        .tool_call_id = "1",
        .name = "ping",
        .arguments_json = "{}",
        .session_id = "s1",
    };
    const result = try registry.dispatch(invocation);
    try std.testing.expectEqualStrings("ok", result.text_result_for_llm);
}

test "ToolRegistry dispatch unknown tool returns failure" {
    const allocator = std.testing.allocator;
    var registry = ToolRegistry.init(allocator);
    defer registry.deinit();

    const invocation = ToolInvocation{
        .tool_call_id = "1",
        .name = "nope",
        .arguments_json = "{}",
        .session_id = "s1",
    };
    const result = try registry.dispatch(invocation);
    try std.testing.expectEqual(ToolResultType.failure, result.result_type);
}
