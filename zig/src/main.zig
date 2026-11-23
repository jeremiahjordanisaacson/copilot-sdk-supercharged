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
pub const session_params = @import("session_params.zig");

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
pub const CloudSessionRepository = types.CloudSessionRepository;
pub const CloudSessionOptions = types.CloudSessionOptions;
pub const CanvasAction = types.CanvasAction;
pub const CanvasDeclaration = types.CanvasDeclaration;
pub const CanvasOpenResponse = types.CanvasOpenResponse;
pub const CanvasHostCapabilities = types.CanvasHostCapabilities;
pub const CanvasHostContext = types.CanvasHostContext;
pub const CanvasOpenContext = types.CanvasOpenContext;
pub const CanvasActionContext = types.CanvasActionContext;
pub const CanvasLifecycleContext = types.CanvasLifecycleContext;
pub const SectionOverride = types.SectionOverride;
pub const SystemMessageAppendConfig = types.SystemMessageAppendConfig;
pub const SystemMessageReplaceConfig = types.SystemMessageReplaceConfig;
pub const SystemMessageCustomizeConfig = types.SystemMessageCustomizeConfig;
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

// v2.6.0 / v3.0.0 (SDK protocol 4) types
pub const ClientInfo = types.ClientInfo;
pub const ClientMode = types.ClientMode;
pub const AutoTier = types.AutoTier;
pub const AskUserVariant = types.AskUserVariant;
pub const TrustWorkingDirectory = types.TrustWorkingDirectory;
pub const MessageSource = types.MessageSource;
pub const GitHubTokenProvider = types.GitHubTokenProvider;
pub const GitHubTokenProviderArgs = types.GitHubTokenProviderArgs;
pub const GitHubToken = types.GitHubToken;
pub const GitHubTokenResult = types.GitHubTokenResult;
pub const SandboxConfig = types.SandboxConfig;
pub const ImageGenerationConfig = types.ImageGenerationConfig;
pub const AutopilotOptions = types.AutopilotOptions;
pub const HookType = types.HookType;
pub const HookHandlerFn = types.HookHandlerFn;
pub const NamedProviderConfig = types.NamedProviderConfig;
pub const ProviderModelConfig = types.ProviderModelConfig;
pub const CapiConfig = types.CapiConfig;
pub const PermissionMode = types.PermissionMode;
pub const PermissionModeRefusalReason = types.PermissionModeRefusalReason;
pub const PermissionModeResult = types.PermissionModeResult;
pub const EVENT_TOOL_SHELL_OUTPUT = types.EVENT_TOOL_SHELL_OUTPUT;
pub const ToolShellOutputData = types.ToolShellOutputData;
pub const ShellAuthenticationInputKind = types.ShellAuthenticationInputKind;
pub const ShellAuthenticationRequest = types.ShellAuthenticationRequest;
pub const ShellAuthenticationResponse = types.ShellAuthenticationResponse;
pub const ShellAuthenticationHandlerFn = types.ShellAuthenticationHandlerFn;
pub const StoredMemory = types.StoredMemory;
pub const MemoryStoreHandlerFn = types.MemoryStoreHandlerFn;
pub const NetworkScope = types.NetworkScope;
pub const NetworkOptions = types.NetworkOptions;
pub const NetworkBasicAuthRequest = types.NetworkBasicAuthRequest;
pub const NetworkBasicCredentials = types.NetworkBasicCredentials;
pub const PolicyHelperFailure = types.PolicyHelperFailure;
pub const RpcErrorInfo = jsonrpc.RpcErrorInfo;

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
    _ = session_params;
}

test "library root imports compile" {
    // Smoke test: creating a client with default options must compile.
    const std = @import("std");
    const allocator = std.testing.allocator;
    var c = CopilotClient.init(allocator, .{});
    defer c.deinit();
    try std.testing.expectEqual(ConnectionState.disconnected, c.getState());
}

test "public client and session APIs compile" {
    // Functions are analyzed lazily; reference every public method so unit test
    // runs also type-check the RPC wrappers that need a live CLI.
    const std = @import("std");
    std.testing.refAllDecls(CopilotClient);
    std.testing.refAllDecls(CopilotSession);
    std.testing.refAllDecls(ClientMode);
    std.testing.refAllDecls(MessageSource);
    std.testing.refAllDecls(PolicyHelperFailure);
    std.testing.refAllDecls(ToolShellOutputData);
    std.testing.refAllDecls(SessionEvent);
}
