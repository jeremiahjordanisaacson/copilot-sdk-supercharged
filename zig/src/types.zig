// types.zig -- Shared type definitions for the Copilot SDK.
//
// All public structs use optional fields (?T) for nullable JSON values
// and slices ([]const u8) for strings, following Zig conventions.

const std = @import("std");

// ---------------------------------------------------------------------------
// JSON-RPC
// ---------------------------------------------------------------------------

pub const JsonValue = std.json.Value;

pub const JsonRpcRequest = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?i64 = null,
    method: []const u8,
    params: ?JsonValue = null,
};

pub const JsonRpcResponse = struct {
    jsonrpc: []const u8 = "2.0",
    id: ?i64 = null,
    result: ?JsonValue = null,
    @"error": ?JsonRpcError = null,
};

pub const JsonRpcError = struct {
    code: i64,
    message: []const u8,
    data: ?JsonValue = null,
};

// ---------------------------------------------------------------------------
// TraceContext
// ---------------------------------------------------------------------------

pub const TraceContext = struct {
    traceparent: ?[]const u8 = null,
    tracestate: ?[]const u8 = null,
};

pub const TraceContextProvider = *const fn () anyerror!TraceContext;

// ---------------------------------------------------------------------------
// Client options
// ---------------------------------------------------------------------------

pub const ClientOptions = struct {
    cli_path: ?[]const u8 = null,
    cli_args: ?[]const []const u8 = null,
    cwd: ?[]const u8 = null,
    cli_url: ?[]const u8 = null,
    log_level: []const u8 = "info",
    use_stdio: bool = true,
    port: u16 = 0,
    auto_start: bool = true,
    auto_restart: bool = true,
    github_token: ?[]const u8 = null,
    use_logged_in_user: ?bool = null,
    session_idle_timeout_seconds: ?u32 = null,
    session_fs: ?SessionFsConfig = null,
    copilot_home: ?[]const u8 = null,
    tcp_connection_token: ?[]const u8 = null,
    remote: bool = false,
    on_get_trace_context: ?TraceContextProvider = null,
};

pub const SessionFsConfig = struct {
    initial_cwd: []const u8 = "/",
    session_state_path: []const u8 = "",
    conventions: []const u8 = "posix",
};

// ---------------------------------------------------------------------------
// Connection state
// ---------------------------------------------------------------------------

pub const ConnectionState = enum {
    disconnected,
    connecting,
    connected,
    error_state,
};

// ---------------------------------------------------------------------------
// Session configuration
// ---------------------------------------------------------------------------

pub const SessionConfig = struct {
    model: ?[]const u8 = null,
    system_prompt: ?[]const u8 = null,
    streaming: bool = true,
    tools: ?[]const ToolDefinition = null,
    skill_directories: ?[]const []const u8 = null,
    disabled_skills: ?[]const []const u8 = null,
    excluded_tools: ?[]const []const u8 = null,
    include_sub_agent_streaming_events: ?bool = null,
    provider: ?ProviderConfig = null,
    enable_config_discovery: ?bool = null,
    model_capabilities_json: ?[]const u8 = null,
    request_headers_json: ?[]const u8 = null,
    mcp_servers_json: ?[]const u8 = null,
    commands_json: ?[]const u8 = null,
    github_token: ?[]const u8 = null,
    instruction_directories: ?[]const []const u8 = null,
    enable_session_telemetry: ?bool = null,
};

pub const ProviderConfig = struct {
    type_name: []const u8 = "openai",
    base_url: ?[]const u8 = null,
    api_key: ?[]const u8 = null,
};

// ---------------------------------------------------------------------------
// Tools
// ---------------------------------------------------------------------------

pub const ToolDefinition = struct {
    name: []const u8,
    description: ?[]const u8 = null,
    parameters_json: ?[]const u8 = null,
};

pub const ToolInvocation = struct {
    tool_call_id: []const u8,
    name: []const u8,
    arguments_json: []const u8,
    session_id: []const u8,
};

pub const ToolResultType = enum {
    success,
    failure,
};

pub const ToolResult = struct {
    text_result_for_llm: []const u8 = "",
    result_type: ToolResultType = .success,
};

// ---------------------------------------------------------------------------
// Permissions
// ---------------------------------------------------------------------------

pub const PermissionKind = enum {
    read,
    write,
    shell,
    unknown,
};

pub const PermissionRequest = struct {
    kind: PermissionKind = .unknown,
    description: ?[]const u8 = null,
    raw_json: ?[]const u8 = null,
};

pub const PermissionResultKind = enum {
    approved,
    denied_interactively_by_user,
    denied_by_policy,
};

pub const PermissionResult = struct {
    kind: PermissionResultKind = .approved,
};

// ---------------------------------------------------------------------------
// User input
// ---------------------------------------------------------------------------

pub const UserInputRequest = struct {
    question: []const u8,
    options: ?[]const []const u8 = null,
};

pub const UserInputResponse = struct {
    answer: []const u8,
    was_freeform: bool = true,
};

// ---------------------------------------------------------------------------
// Elicitation
// ---------------------------------------------------------------------------

pub const ElicitationRequest = struct {
    session_id: []const u8 = "",
    message: []const u8 = "",
    mode: ?[]const u8 = null,
    schema_json: ?[]const u8 = null,
};

pub const ElicitationResult = struct {
    action: []const u8 = "accept",
    content_json: ?[]const u8 = null,
};

// ---------------------------------------------------------------------------
// ExitPlanMode
// ---------------------------------------------------------------------------

pub const ExitPlanModeRequest = struct {
    session_id: []const u8 = "",
};

pub const ExitPlanModeResponse = struct {
    approved: bool = true,
};

// ---------------------------------------------------------------------------
// Events
// ---------------------------------------------------------------------------

pub const SessionEvent = struct {
    id: []const u8 = "",
    timestamp: []const u8 = "",
    parent_id: ?[]const u8 = null,
    agent_id: ?[]const u8 = null,
    ephemeral: bool = false,
    session_id: []const u8 = "",
    event_type: []const u8 = "",
    content: ?[]const u8 = null,
    delta_content: ?[]const u8 = null,
    raw_json: ?[]const u8 = null,

    /// Returns assistant message content if this is an assistant.message event.
    pub fn assistantMessageContent(self: *const SessionEvent) ?[]const u8 {
        if (std.mem.eql(u8, self.event_type, "assistant.message")) {
            return self.content;
        }
        return null;
    }

    /// Returns true if this is a session.idle event (turn finished).
    pub fn isIdle(self: *const SessionEvent) bool {
        return std.mem.eql(u8, self.event_type, "session.idle");
    }

    /// Returns true if this is a streaming delta event.
    pub fn isDelta(self: *const SessionEvent) bool {
        return std.mem.eql(u8, self.event_type, "assistant.message_delta");
    }
};

// ---------------------------------------------------------------------------
// Message options
// ---------------------------------------------------------------------------

pub const MessageOptions = struct {
    prompt: []const u8,
    attachments: ?[]const u8 = null,
    mode: ?[]const u8 = null,
    response_format: ?ResponseFormat = null,
    image_options: ?ImageOptions = null,
};

pub const ResponseFormat = enum {
    text,
    image,
};

pub const ImageOptions = struct {
    size: ?[]const u8 = null,
    quality: ?[]const u8 = null,
    style: ?[]const u8 = null,
};

// ---------------------------------------------------------------------------
// Slash Commands
// ---------------------------------------------------------------------------

/// Completion type for slash command inputs.
pub const SlashCommandInputCompletion = enum {
    directory,
};

/// Kind of slash command.
pub const SlashCommandKind = enum {
    builtin,
    client,
    skill,
};

/// Price category for model picker.
pub const ModelPickerPriceCategory = enum {
    high,
    low,
    medium,
    very_high,
};

/// Input definition for a slash command.
pub const SlashCommandInput = struct {
    hint: []const u8,
    completion: ?SlashCommandInputCompletion = null,
};

/// Information about a slash command.
pub const SlashCommandInfo = struct {
    allow_during_agent_execution: bool = false,
    description: []const u8,
    kind: SlashCommandKind = .builtin,
    name: []const u8,
    aliases: ?[]const []const u8 = null,
    experimental: ?bool = null,
    input: ?SlashCommandInput = null,
};

/// Request to invoke a command.
pub const CommandsInvokeRequest = struct {
    name: []const u8,
    input: ?[]const u8 = null,
};

/// Request to list available commands.
pub const CommandsListRequest = struct {
    include_builtins: ?bool = null,
    include_client_commands: ?bool = null,
    include_skills: ?bool = null,
};

/// Token pricing information for model billing.
pub const ModelBillingTokenPrices = struct {
    batch_size: ?i64 = null,
    cache_price: ?i64 = null,
    input_price: ?i64 = null,
    output_price: ?i64 = null,
};

// Experimental
/// Diagnostics from loading skills.
pub const SkillsLoadDiagnostics = struct {
    errors: []const []const u8,
    warnings: []const []const u8,
};

// ---------------------------------------------------------------------------
// Remote Session
// ---------------------------------------------------------------------------

/// Mode for remote session control.
pub const RemoteSessionMode = enum {
    @"export",
    off,
    on,
};

// Experimental
/// Request to enable or configure a remote session.
pub const RemoteEnableRequest = struct {
    mode: ?RemoteSessionMode = null,
};

// Experimental
/// Result of enabling a remote session.
pub const RemoteEnableResult = struct {
    remote_steerable: bool,
    url: ?[]const u8 = null,
};

// ---------------------------------------------------------------------------
// Server status
// ---------------------------------------------------------------------------

pub const ServerStatus = struct {
    version: ?[]const u8 = null,
    protocol_version: ?i64 = null,
    message: ?[]const u8 = null,
};

pub const AuthStatus = struct {
    authenticated: bool = false,
    user: ?[]const u8 = null,
};

pub const SessionMetadata = struct {
    session_id: []const u8 = "",
    model: ?[]const u8 = null,
    created_at: ?[]const u8 = null,
    status: ?[]const u8 = null,
};

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

pub const SdkError = error{
    JsonRpcError,
    Timeout,
    ConnectionClosed,
    ProtocolMismatch,
    SessionError,
    NotConnected,
    ProcessSpawn,
    InvalidResponse,
    AllocationFailed,
    WriteFailed,
    ReadFailed,
    ParseError,
};

// ---------------------------------------------------------------------------
// Protocol constants
// ---------------------------------------------------------------------------

pub const PROTOCOL_VERSION: i64 = 3;

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "SessionEvent.assistantMessageContent" {
    const ev = SessionEvent{
        .event_type = "assistant.message",
        .content = "Hello!",
    };
    try std.testing.expectEqualStrings("Hello!", ev.assistantMessageContent().?);
}

test "SessionEvent.isIdle" {
    const ev = SessionEvent{ .event_type = "session.idle" };
    try std.testing.expect(ev.isIdle());
}

test "SessionEvent.isDelta returns false for non-delta" {
    const ev = SessionEvent{ .event_type = "assistant.message" };
    try std.testing.expect(!ev.isDelta());
}

test "default ClientOptions" {
    const opts = ClientOptions{};
    try std.testing.expect(opts.use_stdio);
    try std.testing.expect(opts.auto_start);
    try std.testing.expectEqual(@as(u16, 0), opts.port);
}

test "default SessionConfig" {
    const cfg = SessionConfig{};
    try std.testing.expect(cfg.streaming);
    try std.testing.expect(cfg.model == null);
}

test "ToolResult defaults" {
    const r = ToolResult{};
    try std.testing.expectEqual(ToolResultType.success, r.result_type);
}
