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
    /// Interceptor for outbound LLM inference HTTP/WebSocket requests.
    request_handler: ?CopilotRequestHandler = null,
    /// BYOK bearer-token provider used to mint fresh tokens for outbound model requests.
    bearer_token_provider: ?BearerTokenProvider = null,
    /// Built-in plugin directories to load (`builtinPluginDirectories` wire key).
    builtin_plugin_directories: ?[]const []const u8 = null,
    /// Use the in-process FFI transport instead of spawning a CLI (`inProcess` wire key).
    in_process: bool = false,
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

    // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
    /// Enable inline source citations in assistant responses.
    enable_citations: ?bool = null,
    /// Built-in agents to exclude from this session.
    excluded_builtin_agents: ?[]const []const u8 = null,
    /// Per-session spending limits (e.g. AI-credit budget).
    session_limits: ?SessionLimitsConfig = null,
    /// Opt-in persistent session memory configuration.
    memory: ?MemoryConfiguration = null,
    /// OTLP telemetry export protocol (e.g. "grpc" or "http/protobuf").
    otlp_protocol: ?[]const u8 = null,
    /// Enable the WebSocket transport for streamed responses.
    enable_web_socket_responses: ?bool = null,
    /// Experiment (feature-flag) assignment overrides, as a JSON object string.
    exp_assignments_json: ?[]const u8 = null,
    /// Set true to signal a registered MCP OAuth host-token handler (mcpAuthHandler wire flag).
    mcp_auth_handler: bool = false,

    // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
    /// Enable session rewind so the conversation can be rolled back (`rewindEnabled` wire key).
    rewind_enabled: ?bool = null,
    /// Extra workspace directories the session may access (`additionalDirectories` wire key).
    additional_directories: ?[]const []const u8 = null,
    /// Names of MCP servers to disable for this session (`disabledMcpServers` wire key).
    disabled_mcp_servers: ?[]const []const u8 = null,
    /// GitHub MCP tool configuration as a JSON object string (`githubMcpToolConfig` wire key).
    github_mcp_tool_config_json: ?[]const u8 = null,
    /// Canvas provider configuration as a JSON object string (`canvasProvider` wire key).
    canvas_provider_json: ?[]const u8 = null,
    /// Restrict custom agents to locally-defined ones only (`customAgentsLocalOnly` wire key).
    custom_agents_local_only: ?bool = null,
    /// Reasoning-effort control for the model (`reasoningEffort` wire key).
    reasoning_effort: ?[]const u8 = null,
    /// Tool-search configuration as a JSON object string (`toolSearch` wire key).
    tool_search_json: ?[]const u8 = null,
    /// Enable experimental mode for this session (`experimentalMode` wire key).
    experimental_mode: ?bool = null,
    /// Enable content-exclusion enforcement (`contentExclusion` wire key).
    content_exclusion: ?bool = null,
    /// Agent-factory authoring args schema as a JSON object string (`argsSchema` wire key).
    args_schema_json: ?[]const u8 = null,
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
    /// Opaque decision context returned to the CLI under the `decisionContext` wire key.
    decision_context: ?[]const u8 = null,
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
// Exit plan mode
// ---------------------------------------------------------------------------

pub const ExitPlanModeRequest = struct {
    session_id: []const u8 = "",
    summary: []const u8 = "",
    plan_content: ?[]const u8 = null,
    actions: ?[]const []const u8 = null,
    recommended_action: []const u8 = "",
};

pub const ExitPlanModeResponse = struct {
    approved: bool = false,
    selected_action: ?[]const u8 = null,
    feedback: ?[]const u8 = null,
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
    /// Agent mode to run this turn under (e.g. a custom agent name).
    agent_mode: ?[]const u8 = null,
    /// Alternate prompt text to show in the transcript in place of `prompt`.
    display_prompt: ?[]const u8 = null,
    /// Custom HTTP headers for outbound model requests this turn, as a JSON object string.
    request_headers_json: ?[]const u8 = null,
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
// Upstream-sync feature types (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Per-session AI-credit budget; set max_ai_credits to cap spend.
pub const SessionLimitsConfig = struct {
    max_ai_credits: ?f64 = null,
};

/// Opt-in persistent session memory configuration.
pub const MemoryConfiguration = struct {
    enabled: ?bool = null,
};

/// Arguments passed to a BYOK bearer-token provider / MCP auth handler (per-session scoping).
pub const ProviderTokenArgs = struct {
    session_id: []const u8,
};

/// BYOK bearer-token provider: returns a fresh token for outbound model requests.
pub const BearerTokenProvider = *const fn (args: ProviderTokenArgs) anyerror![]const u8;

/// Handler invoked when an MCP server requests an OAuth host token.
pub const McpAuthHandler = *const fn (args: ProviderTokenArgs) anyerror![]const u8;

/// Interceptor for outbound LLM inference HTTP/WebSocket requests.
pub const CopilotRequestHandler = *const fn (request_json: []const u8) anyerror![]const u8;

/// W3C Trace Context headers used for distributed trace propagation.
pub const TraceContext = struct {
    traceparent: ?[]const u8 = null,
    tracestate: ?[]const u8 = null,
};

/// Callback that returns the current W3C Trace Context. Wire this up to your
/// OpenTelemetry (or other tracing) SDK to propagate traces to the Copilot CLI.
pub const TraceContextProvider = *const fn () TraceContext;

/// Tool "defer" loading policy: eager pre-load (.never) or lazy via search (.auto).
pub const ToolDefer = enum {
    auto,
    never,
};

/// System-message section identifiers used with section overrides.
/// `preamble` targets only the identity preamble; `preserve` protects an
/// individually-addressable section from a group-level remove.
pub const SystemMessageSection = enum {
    preamble,
    identity,
    tool_instructions,
    preserve,
};

/// Hook type identifiers (the `hookType` field of a hooks.invoke request).
pub const HookType = struct {
    pub const pre_tool_use = "preToolUse";
    pub const post_tool_use = "postToolUse";
    pub const user_prompt_submitted = "userPromptSubmitted";
    pub const user_prompt_transformed = "userPromptTransformed";
    pub const session_start = "sessionStart";
    pub const session_end = "sessionEnd";
    pub const error_occurred = "errorOccurred";
    pub const pre_mcp_tool_call = "preMcpToolCall";
};

/// GitHub-anchored attachment type identifiers.
pub const GitHubAttachment = struct {
    pub const github_commit = "GitHubCommit";
    pub const github_release = "GitHubRelease";
    pub const github_actions_job = "GitHubActionsJob";
    pub const github_repository = "GitHubRepository";
    pub const github_file_diff = "GitHubFileDiff";
    pub const github_tree_comparison = "GitHubTreeComparison";
    pub const github_url = "GitHubUrl";
    pub const github_file = "GitHubFile";
    pub const github_snippet = "GitHubSnippet";
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
