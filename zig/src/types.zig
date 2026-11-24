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
    /// Provider that supplies W3C trace context for outbound requests (`onGetTraceContext`).
    on_get_trace_context: ?TraceContextProvider = null,
    /// Integrating application identity sent on the `connect` handshake (`clientInfo`).
    client_info: ?ClientInfo = null,
    /// Client mode: `.coding_agent` (default) or `.empty` (minimal defaults; every
    /// create/resume must set `available_tools`).
    mode: ClientMode = .coding_agent,
    /// Network callbacks registered with `network.setProvider` after connecting.
    network: ?NetworkOptions = null,
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

    // --- v2.6.0 session options (parity with @github/copilot-sdk) ---
    /// Session name (`name`; session.create only).
    name: ?[]const u8 = null,
    /// Working directory for tool operations (`workingDirectory`).
    working_directory: ?[]const u8 = null,
    /// Allowed tools (`availableTools`). Required in `ClientMode.empty`.
    available_tools: ?[]const []const u8 = null,
    /// Auto routing preference for the `auto` model (see `AutoTier`); sent under `capi`.
    auto_tier: ?[]const u8 = null,
    /// Ask-user tool variant (see `AskUserVariant`).
    ask_user_variant: ?[]const u8 = null,
    /// GitHub token provider answering `gitHubToken.getToken`. Mutually exclusive with `github_token`.
    github_token_provider: ?GitHubTokenProvider = null,
    /// Sandbox configuration (`sandboxConfig`).
    sandbox_config: ?SandboxConfig = null,
    /// Runtime feature-flag overrides as a JSON object string (`featureFlags`).
    feature_flags_json: ?[]const u8 = null,
    /// Model allowlist (`allowedModels`).
    allowed_models: ?[]const []const u8 = null,
    /// OAuth client ID metadata document URL used for MCP OAuth (`authClientIdMetadataUrl`).
    auth_client_id_metadata_url: ?[]const u8 = null,
    /// Directories containing custom agent definitions (`agentDirectories`).
    agent_directories: ?[]const []const u8 = null,
    /// Directories containing hook definitions (`hookDirectories`).
    hook_directories: ?[]const []const u8 = null,
    /// Whether an interactive user is present (`interactiveUser`).
    interactive_user: ?bool = null,
    /// Managed settings to exclude from enforcement (`excludedManagedSettings`).
    excluded_managed_settings: ?[]const []const u8 = null,
    /// Enforce managed model defaults (`enforceManagedModelDefaults`).
    enforce_managed_model_defaults: ?bool = null,
    /// Working-directory trust: "session" or "user" (see `TrustWorkingDirectory`; session.create only).
    trust_working_directory: ?[]const u8 = null,
    /// Re-read custom instructions (`refreshCustomInstructions`; session.create only).
    refresh_custom_instructions: ?bool = null,
    /// Allow transcript recovery (`allowTranscriptRecovery`; session.resume only).
    allow_transcript_recovery: ?bool = null,
    /// Session-level image generation configuration (`imageGeneration`).
    image_generation: ?ImageGenerationConfig = null,
    /// Autopilot continuation options (`autopilot`).
    autopilot: ?AutopilotOptions = null,
    /// Model capabilities override `limits.max_output_tokens`.
    max_output_tokens: ?u32 = null,
    /// Hook handler invoked through `hooks.invoke` (for example `HookType.subagent_start`).
    on_hook: ?HookHandlerFn = null,

    // --- v3.0.0 session options (SDK protocol 4 parity with @github/copilot-sdk 2.0) ---
    /// Skip the session.resume event and its side effects (`suppressResumeEvent`; session.resume only).
    suppress_resume_event: ?bool = null,
    /// Named BYOK provider connections (`inference.providers`).
    providers: ?[]const NamedProviderConfig = null,
    /// BYOK model definitions (`inference.models`); select one as "<provider>/<id>".
    models: ?[]const ProviderModelConfig = null,
    /// Provider used for unqualified model IDs (`inference.defaultProvider`).
    default_provider: ?[]const u8 = null,
    /// Send `defaultProvider: null` to clear the default provider (session.resume only).
    clear_default_provider: bool = false,
    /// Copilot API options (`github.capi`).
    capi: ?CapiConfig = null,
    /// Initial permission mode (see `PermissionMode`).
    permission_mode: ?[]const u8 = null,
    /// Answers password and confirmation prompts from shell commands (`shellAuthentication.request`).
    on_shell_authentication: ?ShellAuthenticationHandlerFn = null,
    /// Persists memories the agent stores (`memoryStore.store`).
    on_memory_store: ?MemoryStoreHandlerFn = null,

    // --- v3.1.0 session options ---
    /// Answers `userInput.request` (the ask_user tool) and sends `interaction.requestUserInput`.
    /// `CopilotSession.registerUserInputHandler` replaces the handler later.
    on_user_input: ?*const fn (req: UserInputRequest, session_id: []const u8) anyerror!UserInputResponse = null,
};

pub const ProviderConfig = struct {
    type_name: []const u8 = "openai",
    base_url: ?[]const u8 = null,
    api_key: ?[]const u8 = null,
    /// Command whose stdout is used as the API key, run before each request (`apiKeyCommand`).
    api_key_command: ?[]const u8 = null,
    /// Bearer token sent as the Authorization header (`bearerToken`).
    bearer_token: ?[]const u8 = null,
    /// Wire API format: "completions" or "responses" (`wireApi`).
    wire_api: ?[]const u8 = null,
};

// ---------------------------------------------------------------------------
// Tools
// ---------------------------------------------------------------------------

pub const ToolDefinition = struct {
    name: []const u8,
    description: ?[]const u8 = null,
    parameters_json: ?[]const u8 = null,
    /// Safe to retry: repeated calls with the same arguments have no additional effect.
    idempotent: ?bool = null,
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

/// Image support advertised by a `userInput.request` (`imageAttachmentConstraints`).
/// Absent (null on the request) when the runtime does not accept images in the answer.
pub const UserInputImageAttachmentConstraints = struct {
    /// MIME types the runtime accepts, such as "image/png".
    allowed_mime_types: []const []const u8 = &.{},
    /// Maximum size of one image, in bytes.
    max_bytes_per_image: i64 = 0,
    /// Maximum number of images in one answer.
    max_count: i64 = 0,

    /// Reads `imageAttachmentConstraints`; null unless `value` is an object. Strings
    /// reference `value`; the MIME type list is allocated from `allocator`.
    pub fn fromJson(allocator: std.mem.Allocator, value: JsonValue) std.mem.Allocator.Error!?UserInputImageAttachmentConstraints {
        if (value != .object) return null;
        var out = UserInputImageAttachmentConstraints{};
        if (value.object.get("allowedMimeTypes")) |mime_types| {
            if (mime_types == .array) {
                var list = std.ArrayList([]const u8).init(allocator);
                for (mime_types.array.items) |item| {
                    if (item == .string) try list.append(item.string);
                }
                out.allowed_mime_types = list.items;
            }
        }
        out.max_bytes_per_image = integerMember(value.object, "maxBytesPerImage") orelse 0;
        out.max_count = integerMember(value.object, "maxCount") orelse 0;
        return out;
    }
};

/// An inline image attached to an ask_user answer (`UserInputResponse.attachments`).
/// The wire `type` is always "image". The runtime enforces the advertised MIME types,
/// size and count limits.
pub const UserInputImageAttachment = struct {
    /// Base64-encoded image bytes.
    data: []const u8,
    /// MIME type of the image, such as "image/png".
    mime_type: []const u8,
    description: ?[]const u8 = null,
    display_name: ?[]const u8 = null,
};

/// A `userInput.request` (the ask_user tool). Slices are valid only during the handler call.
pub const UserInputRequest = struct {
    question: []const u8,
    options: ?[]const []const u8 = null,
    /// Whether a freeform answer is allowed (`allowFreeform`), when the runtime says so.
    allow_freeform: ?bool = null,
    /// Pending request id (`requestId`), when the runtime sends one.
    request_id: ?[]const u8 = null,
    /// Image support for the answer; null when the runtime does not accept images.
    image_attachment_constraints: ?UserInputImageAttachmentConstraints = null,
    /// The request's opaque `_meta` object, when the runtime sends one.
    meta: ?JsonValue = null,
};

pub const UserInputResponse = struct {
    answer: []const u8,
    was_freeform: bool = true,
    /// Images attached to the answer, only valid when the request advertised
    /// `image_attachment_constraints`. Sent only when set. Image-only answers use an
    /// empty `answer`.
    attachments: ?[]const UserInputImageAttachment = null,

    /// The `userInput.request` reply: {answer, wasFreeform, attachments?}.
    pub fn toJson(self: UserInputResponse, allocator: std.mem.Allocator) std.mem.Allocator.Error!JsonValue {
        var obj = std.json.ObjectMap.init(allocator);
        try obj.put("answer", .{ .string = self.answer });
        try obj.put("wasFreeform", .{ .bool = self.was_freeform });
        if (self.attachments) |attachments| {
            var list = std.json.Array.init(allocator);
            for (attachments) |attachment| {
                var item = std.json.ObjectMap.init(allocator);
                try item.put("type", .{ .string = "image" });
                try item.put("data", .{ .string = attachment.data });
                try item.put("mimeType", .{ .string = attachment.mime_type });
                if (attachment.description) |v| try item.put("description", .{ .string = v });
                if (attachment.display_name) |v| try item.put("displayName", .{ .string = v });
                try list.append(.{ .object = item });
            }
            try obj.put("attachments", .{ .array = list });
        }
        return .{ .object = obj };
    }
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
    /// Parsed data of a `tool.shell_output` event (see `EVENT_TOOL_SHELL_OUTPUT`), else null.
    shell_output: ?ToolShellOutputData = null,

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

    /// Returns true if this is a live shell output event (`tool.shell_output`).
    pub fn isShellOutput(self: *const SessionEvent) bool {
        return std.mem.eql(u8, self.event_type, EVENT_TOOL_SHELL_OUTPUT);
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
    /// Message provenance (see `MessageSource`): "user", "system", or "agent-<id>".
    source: ?[]const u8 = null,
    /// JSON Schema (object string) for structured output; sent as
    /// `responseFormat: {type: "json_schema", jsonSchema: {name, strict, schema}}`.
    response_schema_json: ?[]const u8 = null,
    /// Opaque client-generated turn ID echoed on this turn's events (`clientTurnId`).
    client_turn_id: ?[]const u8 = null,
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
    RequestFailed,
    InvalidConfig,
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
    /// Fired before a sub-agent's first turn.
    pub const subagent_start = "subagentStart";
    /// Fired after a sub-agent completes a turn.
    pub const subagent_stop = "subagentStop";
};

/// Hook handler invoked through `hooks.invoke`. Build the optional output with
/// `allocator` (an arena freed after the reply is sent).
pub const HookHandlerFn = *const fn (
    allocator: std.mem.Allocator,
    hook_type: []const u8,
    input: JsonValue,
    session_id: []const u8,
) anyerror!?JsonValue;

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
// v2.6.0 / v3.0.0 feature types (SDK protocol 4 parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Identity of the integrating application, sent on the `connect` handshake as
/// `clientInfo {editorName, editorVersion, extensionName, extensionVersion}`.
/// Empty fields are dropped; an all-empty identity skips `connect`.
pub const ClientInfo = struct {
    editor_name: ?[]const u8 = null,
    editor_version: ?[]const u8 = null,
    extension_name: ?[]const u8 = null,
    extension_version: ?[]const u8 = null,
};

/// Client mode (`ClientOptions.mode`).
pub const ClientMode = enum {
    /// Full coding-agent defaults (identical to earlier releases).
    coding_agent,
    /// Minimal defaults; every create/resume must specify `available_tools`.
    empty,

    /// Parses "coding-agent" or "empty"; "copilot-cli" is a legacy alias of "coding-agent".
    pub fn parse(value: []const u8) ?ClientMode {
        if (std.mem.eql(u8, value, "coding-agent") or std.mem.eql(u8, value, "copilot-cli")) return .coding_agent;
        if (std.mem.eql(u8, value, "empty")) return .empty;
        return null;
    }

    pub fn wireName(self: ClientMode) []const u8 {
        return switch (self) {
            .coding_agent => "coding-agent",
            .empty => "empty",
        };
    }
};

/// Auto routing preferences for the `auto` model (`SessionConfig.auto_tier`,
/// `CopilotSession.setAutoTier`).
pub const AutoTier = struct {
    pub const fast = "fast";
    pub const efficiency = "efficiency";
    pub const balance = "balance";
    pub const intelligence = "intelligence";
};

/// Ask-user tool variants (`SessionConfig.ask_user_variant`).
pub const AskUserVariant = struct {
    pub const legacy = "legacy";
    pub const elicitation = "elicitation";
};

/// Working-directory trust scopes (`SessionConfig.trust_working_directory`).
pub const TrustWorkingDirectory = struct {
    pub const session = "session";
    pub const user = "user";
};

/// Message provenance values (`MessageOptions.source`).
pub const MessageSource = struct {
    pub const user = "user";
    pub const system = "system";

    /// Builds "agent-<id>" for a message originating from an identified agent (caller frees).
    pub fn agent(allocator: std.mem.Allocator, agent_id: []const u8) ![]u8 {
        return std.fmt.allocPrint(allocator, "agent-{s}", .{agent_id});
    }
};

/// Arguments passed to a `GitHubTokenProvider` (`gitHubToken.getToken`).
pub const GitHubTokenProviderArgs = struct {
    /// Effective GitHub host the token is requested for.
    host: []const u8 = "",
    /// Session receiving the token, when known.
    session_id: ?[]const u8 = null,
    /// "initial" or "refresh".
    reason: []const u8 = "initial",
};

pub const GitHubToken = struct {
    access_token: []const u8,
    /// OAuth token type; bearer when null.
    token_type: ?[]const u8 = null,
    /// Remaining token lifetime in seconds.
    expires_in: i64 = 0,
};

pub const GitHubTokenResult = union(enum) {
    token: GitHubToken,
    cancelled,
};

/// Acquires a GitHub token for one session. `allocator` is an arena freed after
/// the reply is sent.
pub const GitHubTokenProvider = *const fn (allocator: std.mem.Allocator, args: GitHubTokenProviderArgs) anyerror!GitHubTokenResult;

/// Sandbox configuration (`sandboxConfig`, sent as `permissions.sandbox`).
pub const SandboxConfig = struct {
    enabled: bool = false,
    /// User policy fragment as a JSON object string (`userPolicy`).
    user_policy_json: ?[]const u8 = null,
    add_current_working_directory: ?bool = null,
    sandbox_mcp_servers: ?bool = null,
    sandbox_lsp_servers: ?bool = null,
    sandbox_extensions: ?bool = null,
    /// Let the agent request an individual command outside the sandbox through
    /// the permission flow (`allowBypass`).
    allow_bypass: ?bool = null,
};

/// Session-level image generation configuration (`imageGeneration`).
pub const ImageGenerationConfig = struct {
    enabled: ?bool = null,
    watermark: ?bool = null,
    save_to_disk: ?bool = null,
};

/// Autopilot continuation options (`autopilot`).
pub const AutopilotOptions = struct {
    continuation_enabled: ?bool = null,
    max_continues: ?u32 = null,
};

/// Named BYOK provider connection (`inference.providers[]`).
pub const NamedProviderConfig = struct {
    /// Provider name referenced by `ProviderModelConfig.provider` (must not contain '/').
    name: []const u8,
    type_name: ?[]const u8 = null,
    base_url: []const u8,
    api_key: ?[]const u8 = null,
    api_key_command: ?[]const u8 = null,
    bearer_token: ?[]const u8 = null,
    wire_api: ?[]const u8 = null,
    /// "http" or "websockets".
    transport: ?[]const u8 = null,
    azure_api_version: ?[]const u8 = null,
    /// Extra HTTP headers as a JSON object string.
    headers_json: ?[]const u8 = null,
    model_provider: ?[]const u8 = null,
};

/// BYOK model definition (`inference.models[]`). Select it with model "<provider>/<id>".
pub const ProviderModelConfig = struct {
    provider: []const u8,
    id: []const u8,
    model_id: ?[]const u8 = null,
    wire_model: ?[]const u8 = null,
    name: ?[]const u8 = null,
    max_prompt_tokens: ?u32 = null,
    max_context_window_tokens: ?u32 = null,
    max_output_tokens: ?u32 = null,
    /// Capability overrides as a JSON object string.
    capabilities_json: ?[]const u8 = null,
};

/// Copilot API options (`github.capi`).
pub const CapiConfig = struct {
    /// Enables Copilot API models alongside or instead of BYOK providers.
    enabled: ?bool = null,
};

/// Permission modes (`SessionConfig.permission_mode`, `CopilotSession.setPermissionMode`).
pub const PermissionMode = struct {
    pub const manual = "manual";
    pub const assisted = "assisted";
    pub const allow_all = "allow-all";
};

/// Known reasons a permission mode change was refused.
pub const PermissionModeRefusalReason = struct {
    pub const managed_policy = "managed_policy";
};

/// Result of `CopilotSession.setPermissionMode`. Slices reference the response and stay
/// valid until the next request on the client.
pub const PermissionModeResult = struct {
    success: bool = false,
    mode: []const u8 = "",
    /// Why the mode was refused (see `PermissionModeRefusalReason`), when known.
    reason: ?[]const u8 = null,
};

/// Live, append-only shell output event type. Not persisted or replayed.
pub const EVENT_TOOL_SHELL_OUTPUT = "tool.shell_output";

/// Data of a `tool.shell_output` event. Slices reference the event JSON.
pub const ToolShellOutputData = struct {
    tool_call_id: []const u8 = "",
    text: []const u8 = "",
    /// Zero-based publication sequence across all streams for this tool call.
    sequence: i64 = 0,
    /// "stdout", "stderr", or null (omitted means stdout).
    stream: ?[]const u8 = null,

    /// Reads the event `data` object of a `tool.shell_output` event.
    pub fn fromJson(data: JsonValue) ?ToolShellOutputData {
        if (data != .object) return null;
        var out = ToolShellOutputData{};
        if (data.object.get("toolCallId")) |v| {
            if (v == .string) out.tool_call_id = v.string;
        }
        if (data.object.get("text")) |v| {
            if (v == .string) out.text = v.string;
        }
        if (data.object.get("sequence")) |v| {
            switch (v) {
                .integer => |i| out.sequence = i,
                .float => |f| out.sequence = @intFromFloat(f),
                else => {},
            }
        }
        if (data.object.get("stream")) |v| {
            if (v == .string) out.stream = v.string;
        }
        return out;
    }
};

/// Input kinds of a shell authentication request.
pub const ShellAuthenticationInputKind = struct {
    pub const secret = "secret";
    pub const text = "text";
    pub const confirm = "confirm";
};

/// A password or other answer a shell command run by the agent asks for
/// (`shellAuthentication.request`). The answer goes only to the program that asked.
pub const ShellAuthenticationRequest = struct {
    session_id: []const u8,
    request_id: []const u8,
    /// Program that asked, such as "sudo", "ssh", or "git".
    program: []const u8 = "",
    prompt: []const u8 = "",
    /// See `ShellAuthenticationInputKind`; "secret" input must be masked.
    input_kind: []const u8 = ShellAuthenticationInputKind.secret,
    command: []const u8 = "",
    cwd: []const u8 = "",
};

/// Answer to a shell authentication request. Answer "confirm" requests with "yes".
pub const ShellAuthenticationResponse = union(enum) {
    accept: []const u8,
    cancel,
};

/// Shell authentication handler. `allocator` is an arena freed after the reply is sent.
pub const ShellAuthenticationHandlerFn = *const fn (
    allocator: std.mem.Allocator,
    request: ShellAuthenticationRequest,
) anyerror!ShellAuthenticationResponse;

/// A memory the agent asked to store, after validation, secret filtering and
/// approval (`memoryStore.store`).
pub const StoredMemory = struct {
    session_id: []const u8,
    subject: []const u8 = "",
    fact: []const u8 = "",
    citations: []const u8 = "",
    reason: []const u8 = "",
};

/// Persists a stored memory; return an error to fail the store request.
pub const MemoryStoreHandlerFn = *const fn (memory: StoredMemory) anyerror!void;

/// Traffic network callbacks serve (`NetworkOptions.scope`).
pub const NetworkScope = struct {
    pub const connection = "connection";
    pub const runtime = "runtime";
};

/// A proxy's Basic authentication challenge (`network.authenticateBasic`).
pub const NetworkBasicAuthRequest = struct {
    proxy: []const u8,
    /// Realm named by the proxy (untrusted text), when present.
    realm: ?[]const u8 = null,
    /// True when the proxy rejected the credentials sent last.
    previous_rejected: bool = false,
};

/// Basic credentials; leave `username` null to supply none.
pub const NetworkBasicCredentials = struct {
    username: ?[]const u8 = null,
    password: ?[]const u8 = null,
};

/// Network callbacks registered with `network.setProvider` after connecting.
/// Each callback receives an arena allocator freed after the reply is sent.
/// The SDK never logs tokens or passwords.
pub const NetworkOptions = struct {
    /// See `NetworkScope`; the runtime defaults to "connection".
    scope: ?[]const u8 = null,
    /// Returns a PAC-style proxy list such as "PROXY host:8080; DIRECT", or null to leave
    /// the request to the runtime's own proxy resolution.
    resolve_proxy: ?*const fn (allocator: std.mem.Allocator, url: []const u8) anyerror!?[]const u8 = null,
    /// Returns a base64 SPNEGO token for a Kerberos service principal, or null.
    authenticate_negotiate: ?*const fn (allocator: std.mem.Allocator, spn: []const u8) anyerror!?[]const u8 = null,
    /// Answers a proxy Basic challenge.
    authenticate_basic: ?*const fn (allocator: std.mem.Allocator, request: NetworkBasicAuthRequest) anyerror!NetworkBasicCredentials = null,
};

/// Structured Policy Helper failure (`PolicyHelperFailure`). Reported in the JSON-RPC
/// error data of a failed session.create/resume and on managed-settings snapshots.
/// Helper-authored text is untrusted: render it as plain text. Slices reference `value`.
pub const PolicyHelperFailure = struct {
    category: []const u8,
    failure_mode: []const u8 = "",
    message: []const u8,
    previous_policy_retained: bool = false,
    reason: []const u8 = "",
    severity: ?[]const u8 = null,
    authority: ?[]const u8 = null,
    code: ?[]const u8 = null,
    diagnostic: ?[]const u8 = null,
    remediation: ?[]const u8 = null,
    help_link_url: ?[]const u8 = null,
    /// Field-specific findings (`details`), an array of {message, path?}.
    details: ?JsonValue = null,

    /// Reads a failure from the failure object itself or from any object carrying it
    /// under `policyHelperFailure` (JSON-RPC error data, managed-settings snapshots).
    pub fn fromJson(value: JsonValue) ?PolicyHelperFailure {
        if (value != .object) return null;
        const failure = value.object.get("policyHelperFailure") orelse value;
        if (failure != .object) return null;
        const obj = failure.object;
        const category = stringMember(obj, "category") orelse return null;
        const message = stringMember(obj, "message") orelse return null;
        var out = PolicyHelperFailure{ .category = category, .message = message };
        out.failure_mode = stringMember(obj, "failureMode") orelse "";
        out.reason = stringMember(obj, "reason") orelse "";
        if (obj.get("previousPolicyRetained")) |v| {
            if (v == .bool) out.previous_policy_retained = v.bool;
        }
        out.severity = stringMember(obj, "severity");
        out.authority = stringMember(obj, "authority");
        out.code = stringMember(obj, "code");
        out.diagnostic = stringMember(obj, "diagnostic");
        out.remediation = stringMember(obj, "remediation");
        if (obj.get("helpLink")) |link| {
            if (link == .object) out.help_link_url = stringMember(link.object, "url");
        }
        if (obj.get("details")) |details| {
            if (details == .array) out.details = details;
        }
        return out;
    }
};

fn stringMember(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const v = obj.get(key) orelse return null;
    return if (v == .string) v.string else null;
}

fn integerMember(obj: std.json.ObjectMap, key: []const u8) ?i64 {
    const v = obj.get(key) orelse return null;
    return switch (v) {
        .integer => |i| i,
        .float => |f| @as(i64, @intFromFloat(f)),
        else => null,
    };
}

/// True when `obj[key]` is the JSON literal true.
fn boolMember(obj: std.json.ObjectMap, key: []const u8) bool {
    const v = obj.get(key) orelse return false;
    return v == .bool and v.bool;
}

// ---------------------------------------------------------------------------
// v3.1.0 feature types (parity with the Microsoft Copilot SDK 2.0.3-preview.0)
// ---------------------------------------------------------------------------

/// Result of `CopilotClient.mcpSecretsSave`.
pub const McpSecretsSaveResult = struct {
    /// True when the OS keychain stored the credential, false for the file backend.
    used_keychain: bool = false,

    pub fn fromJson(value: JsonValue) McpSecretsSaveResult {
        if (value != .object) return .{};
        return .{ .used_keychain = boolMember(value.object, "usedKeychain") };
    }
};

/// A user-scoped approval for one installed plugin extension.
pub const PermissionsExtensionApproval = struct {
    /// Stable identity, for example "plugin:plugin-name:extension-name".
    extension_name: []const u8 = "",
    plugin_name: []const u8 = "",
    /// Exact privileged capabilities the user approved.
    capabilities: []const []const u8 = &.{},
    /// When the approval was granted (RFC 3339 timestamp).
    granted_at: []const u8 = "",
    /// Opaque key; pass it back unchanged to `CopilotClient.extensionApprovalsRevoke`.
    installation_key: []const u8 = "",
    installation_label: ?[]const u8 = null,

    /// Reads an approval object; every slice is copied into `allocator`.
    pub fn fromJson(allocator: std.mem.Allocator, value: JsonValue) std.mem.Allocator.Error!PermissionsExtensionApproval {
        var out = PermissionsExtensionApproval{};
        if (value != .object) return out;
        const obj = value.object;
        out.extension_name = try allocator.dupe(u8, stringMember(obj, "extensionName") orelse "");
        out.plugin_name = try allocator.dupe(u8, stringMember(obj, "pluginName") orelse "");
        out.granted_at = try allocator.dupe(u8, stringMember(obj, "grantedAt") orelse "");
        out.installation_key = try allocator.dupe(u8, stringMember(obj, "installationKey") orelse "");
        if (stringMember(obj, "installationLabel")) |label| out.installation_label = try allocator.dupe(u8, label);
        if (obj.get("capabilities")) |capabilities| {
            if (capabilities == .array) {
                var list = std.ArrayList([]const u8).init(allocator);
                for (capabilities.array.items) |item| {
                    if (item == .string) try list.append(try allocator.dupe(u8, item.string));
                }
                out.capabilities = list.items;
            }
        }
        return out;
    }
};

/// Result of `CopilotClient.extensionApprovalsGrant`.
pub const PermissionsExtensionApprovalsGrantResult = struct {
    /// Whether the pending request was eligible and persisted.
    granted: bool = false,
    /// The stored approval, when granted.
    approval: ?PermissionsExtensionApproval = null,

    /// Reads a grant result; the approval is copied into `allocator`.
    pub fn fromJson(allocator: std.mem.Allocator, value: JsonValue) std.mem.Allocator.Error!PermissionsExtensionApprovalsGrantResult {
        var out = PermissionsExtensionApprovalsGrantResult{};
        if (value != .object) return out;
        out.granted = boolMember(value.object, "granted");
        if (value.object.get("approval")) |approval| {
            if (approval == .object) out.approval = try PermissionsExtensionApproval.fromJson(allocator, approval);
        }
        return out;
    }
};

/// Result of `CopilotClient.extensionApprovalsList`.
pub const PermissionsExtensionApprovalsListResult = struct {
    approvals: []const PermissionsExtensionApproval = &.{},

    /// Reads a list result; every approval is copied into `allocator`.
    pub fn fromJson(allocator: std.mem.Allocator, value: JsonValue) std.mem.Allocator.Error!PermissionsExtensionApprovalsListResult {
        if (value != .object) return .{};
        const approvals = value.object.get("approvals") orelse return .{};
        if (approvals != .array) return .{};
        var list = std.ArrayList(PermissionsExtensionApproval).init(allocator);
        for (approvals.array.items) |item| {
            if (item == .object) try list.append(try PermissionsExtensionApproval.fromJson(allocator, item));
        }
        return .{ .approvals = list.items };
    }
};

/// Result of `CopilotClient.extensionApprovalsRevoke`.
pub const PermissionsExtensionApprovalsRevokeResult = struct {
    /// Whether a matching approval was revoked.
    revoked: bool = false,

    pub fn fromJson(value: JsonValue) PermissionsExtensionApprovalsRevokeResult {
        if (value != .object) return .{};
        return .{ .revoked = boolMember(value.object, "revoked") };
    }
};

// ---------------------------------------------------------------------------
// Protocol constants
// ---------------------------------------------------------------------------

/// SDK protocol version; protocol 4 (grouped session payloads) requires Copilot CLI 1.0.113+.
pub const PROTOCOL_VERSION: i64 = 4;

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

test "ClientMode.parse accepts the legacy copilot-cli alias" {
    try std.testing.expectEqual(ClientMode.coding_agent, ClientMode.parse("copilot-cli").?);
    try std.testing.expectEqual(ClientMode.empty, ClientMode.parse("empty").?);
    try std.testing.expect(ClientMode.parse("bogus") == null);
}

test "PolicyHelperFailure.fromJson reads JSON-RPC error data" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const data = try std.json.parseFromSliceLeaky(JsonValue, arena.allocator(),
        \\{"policyHelperFailure":{"category":"helper_failed","failureMode":"fail_closed","message":"Policy helper exited","previousPolicyRetained":true,"reason":"exit_code","code":"E42","helpLink":{"url":"https://example.com/help"}}}
    , .{});
    const failure = PolicyHelperFailure.fromJson(data).?;
    try std.testing.expectEqualStrings("helper_failed", failure.category);
    try std.testing.expectEqualStrings("fail_closed", failure.failure_mode);
    try std.testing.expect(failure.previous_policy_retained);
    try std.testing.expectEqualStrings("E42", failure.code.?);
    try std.testing.expectEqualStrings("https://example.com/help", failure.help_link_url.?);
    try std.testing.expect(PolicyHelperFailure.fromJson(.{ .string = "x" }) == null);
}
