// main.zig -- Library root that re-exports all public APIs.
//
// Usage:
//   const copilot = @import("copilot-sdk");
//   var client = copilot.CopilotClient.init(allocator, .{});

pub const types = @import("types.zig");
pub const jsonrpc = @import("jsonrpc.zig");
pub const session = @import("session.zig");
pub const client = @import("client.zig");
pub const tools = @import("tools.zig");

// Re-export the most commonly used symbols at the top level for convenience.
pub const CopilotClient = client.CopilotClient;
pub const CopilotSession = session.CopilotSession;
pub const ClientOptions = types.ClientOptions;
pub const SessionConfig = types.SessionConfig;
pub const ConnectionState = types.ConnectionState;
pub const SessionEvent = types.SessionEvent;
pub const MessageOptions = types.MessageOptions;
pub const ToolDefinition = types.ToolDefinition;
pub const ToolInvocation = types.ToolInvocation;
pub const ToolResult = types.ToolResult;
pub const ToolResultType = types.ToolResultType;
pub const ToolRegistry = tools.ToolRegistry;
pub const PermissionRequest = types.PermissionRequest;
pub const PermissionResult = types.PermissionResult;
pub const PermissionResultKind = types.PermissionResultKind;
pub const PermissionKind = types.PermissionKind;
pub const UserInputRequest = types.UserInputRequest;
pub const UserInputResponse = types.UserInputResponse;
pub const ServerStatus = types.ServerStatus;
pub const AuthStatus = types.AuthStatus;
pub const SessionMetadata = types.SessionMetadata;
pub const SdkError = types.SdkError;
pub const JsonRpcTransport = jsonrpc.JsonRpcTransport;
pub const IdGenerator = jsonrpc.IdGenerator;
pub const ResponseFormat = types.ResponseFormat;
pub const ImageOptions = types.ImageOptions;
pub const ProviderConfig = types.ProviderConfig;
pub const SessionFsConfig = types.SessionFsConfig;
pub const PROTOCOL_VERSION = types.PROTOCOL_VERSION;

pub const EventCallback = session.EventCallback;
pub const EventSubscription = session.EventSubscription;
pub const PermissionHandlerFn = session.PermissionHandlerFn;
pub const UserInputHandlerFn = session.UserInputHandlerFn;
pub const LifecycleCallback = client.LifecycleCallback;
pub const ToolHandlerFn = tools.ToolHandlerFn;

pub const defineTool = tools.defineTool;

// Pull in all tests from submodules when running `zig build test`.
comptime {
    _ = types;
    _ = jsonrpc;
    _ = session;
    _ = client;
    _ = tools;
}

test "library root imports compile" {
    // Smoke test: creating a client with default options must compile.
    const std = @import("std");
    const allocator = std.testing.allocator;
    var c = CopilotClient.init(allocator, .{});
    defer c.deinit();
    try std.testing.expectEqual(ConnectionState.disconnected, c.getState());
}
