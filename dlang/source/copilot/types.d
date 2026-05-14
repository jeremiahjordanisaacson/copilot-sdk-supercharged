/// Core type definitions for the Copilot SDK.
///
/// Uses D structs with Nullable fields, UFCS-friendly design,
/// and std.json interop.
module copilot.types;

import std.json;
import std.typecons : Nullable;
import std.conv : to;

// ---------------------------------------------------------------------------
// Client options
// ---------------------------------------------------------------------------

/// Configuration for connecting to the Copilot CLI.
struct CopilotClientOptions
{
    /// Path to the Copilot CLI binary.
    string cliPath = "github-copilot";

    /// Additional CLI arguments.
    string[] cliArgs;

    /// Working directory for the CLI process.
    Nullable!string cwd;

    /// TCP port when not using stdio.
    Nullable!ushort port;

    /// Use stdio transport (default true).
    bool useStdio = true;

    /// Connect to an already-running CLI server at this URL.
    Nullable!string cliUrl;

    /// Log level forwarded to the CLI.
    string logLevel = "info";

    /// Start the CLI automatically on client creation.
    bool autoStart = true;

    /// Restart the CLI if it exits unexpectedly.
    bool autoRestart = false;

    /// Extra environment variables for the CLI process.
    string[string] env;

    /// GitHub token for authentication.
    Nullable!string gitHubToken;

    /// Use the currently logged-in GitHub user.
    bool useLoggedInUser = true;

    /// Session filesystem configuration for persistence.
    Nullable!SessionFsConfig sessionFs;

    /// Idle timeout in seconds before a session is reaped.
    Nullable!uint sessionIdleTimeoutSeconds;

    /// Override the Copilot home directory.
    Nullable!string copilotHome;

    /// Token for TCP connection authentication.
    Nullable!string tcpConnectionToken;

    /// Connect to a remote CLI server.
    bool remote = false;

    /// Provider for W3C Trace Context headers.
    TraceContextProvider onGetTraceContext;
}

// ---------------------------------------------------------------------------
// SessionFs configuration
// ---------------------------------------------------------------------------

/// Configuration for the session filesystem provider.
struct SessionFsConfig
{
    string initialCwd;
    string sessionStatePath;
    string conventions; // "windows" or "posix"

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["initialCwd"] = initialCwd;
        obj["sessionStatePath"] = sessionStatePath;
        obj["conventions"] = conventions;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// MCP server configuration
// ---------------------------------------------------------------------------

/// Transport type for an MCP server.
enum McpServerType : string
{
    stdio = "stdio",
    http = "http",
}

/// Configuration for an MCP server.
struct McpServerConfig
{
    McpServerType type;
    Nullable!string command;
    string[] args;
    Nullable!string url;
    string[string] env;
    string[string] headers;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["type"] = cast(string) type;
        if (!command.isNull) obj["command"] = command.get;
        if (args.length > 0)
        {
            JSONValue[] arr;
            foreach (a; args) arr ~= JSONValue(a);
            obj["args"] = JSONValue(arr);
        }
        if (url.isNull == false) obj["url"] = url.get;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// Command definition
// ---------------------------------------------------------------------------

/// A named command exposed to the model.
struct CommandDefinition
{
    string name;
    string description;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["name"] = name;
        obj["description"] = description;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// Image response format
// ---------------------------------------------------------------------------

/// Desired response format for image generation.
enum ImageResponseFormat : string
{
    text = "text",
    image = "image",
    jsonObject = "json_object",
}

// ---------------------------------------------------------------------------
// Session configuration
// ---------------------------------------------------------------------------

/// Provider configuration for the LLM backend.
struct ProviderConfig
{
    Nullable!string model;
    Nullable!string baseUrl;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!model.isNull) obj["model"] = model.get;
        if (!baseUrl.isNull) obj["baseUrl"] = baseUrl.get;
        return obj;
    }
}

/// Configuration used when creating a new session.
struct SessionConfig
{
    /// Whether to stream assistant responses as deltas.
    bool streaming = false;

    /// System-level instructions prepended to the conversation.
    Nullable!string systemMessage;

    /// LLM provider overrides.
    Nullable!ProviderConfig provider;

    /// Tools available in this session.
    Tool[] tools;

    /// Enable infinite (persistent) sessions.
    bool infiniteSessions = true;

    /// Model to use.
    Nullable!string model;

    /// Reasoning effort level.
    Nullable!string reasoningEffort;

    /// Per-session auth token override.
    Nullable!string gitHubToken;

    /// Excluded tools for this session.
    string[] excludedTools;

    /// MCP server configurations.
    McpServerConfig[string] mcpServers;

    /// Model capabilities override.
    JSONValue modelCapabilities;

    /// Enable automatic config discovery.
    bool enableConfigDiscovery = false;

    /// Include sub-agent streaming events.
    bool includeSubAgentStreamingEvents = false;

    /// Command definitions.
    CommandDefinition[] commands;

    /// Skill directories.
    string[] skillDirectories;

    /// Disabled skills.
    string[] disabledSkills;

    /// Working directory.
    Nullable!string workingDirectory;

    /// Response format for image generation.
    Nullable!ImageResponseFormat responseFormat;

    /// Additional HTTP headers sent with each model request.
    string[string] requestHeaders;

    /// Directories containing instruction files for the session.
    string[] instructionDirectories;

    /// Enable session telemetry reporting.
    Nullable!bool enableSessionTelemetry;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["streaming"] = streaming;
        obj["infiniteSessions"] = infiniteSessions;

        if (!systemMessage.isNull)
            obj["systemMessage"] = systemMessage.get;

        if (!provider.isNull)
            obj["provider"] = provider.get.toJson();

        if (tools.length > 0)
        {
            JSONValue[] arr;
            foreach (ref t; tools)
                arr ~= t.toJson();
            obj["tools"] = JSONValue(arr);
        }

        if (!model.isNull)
            obj["model"] = model.get;

        if (!reasoningEffort.isNull)
            obj["reasoningEffort"] = reasoningEffort.get;

        if (!gitHubToken.isNull)
            obj["gitHubToken"] = gitHubToken.get;

        if (excludedTools.length > 0)
        {
            JSONValue[] arr;
            foreach (t; excludedTools) arr ~= JSONValue(t);
            obj["excludedTools"] = JSONValue(arr);
        }

        if (mcpServers.length > 0)
        {
            auto mcpObj = JSONValue(string[string].init);
            foreach (key, cfg; mcpServers)
                mcpObj[key] = cfg.toJson();
            obj["mcpServers"] = mcpObj;
        }

        if (modelCapabilities.type == JSONType.object)
            obj["modelCapabilities"] = modelCapabilities;

        if (enableConfigDiscovery)
            obj["enableConfigDiscovery"] = true;

        if (includeSubAgentStreamingEvents)
            obj["includeSubAgentStreamingEvents"] = true;

        if (commands.length > 0)
        {
            JSONValue[] arr;
            foreach (ref c; commands) arr ~= c.toJson();
            obj["commands"] = JSONValue(arr);
        }

        if (skillDirectories.length > 0)
        {
            JSONValue[] arr;
            foreach (s; skillDirectories) arr ~= JSONValue(s);
            obj["skillDirectories"] = JSONValue(arr);
        }

        if (disabledSkills.length > 0)
        {
            JSONValue[] arr;
            foreach (s; disabledSkills) arr ~= JSONValue(s);
            obj["disabledSkills"] = JSONValue(arr);
        }

        if (!workingDirectory.isNull)
            obj["workingDirectory"] = workingDirectory.get;

        if (!responseFormat.isNull)
            obj["responseFormat"] = cast(string) responseFormat.get;

        if (requestHeaders.length > 0)
        {
            auto hdrObj = JSONValue(string[string].init);
            foreach (key, val; requestHeaders)
                hdrObj[key] = val;
            obj["requestHeaders"] = hdrObj;
        }

        if (instructionDirectories.length > 0)
        {
            JSONValue[] arr;
            foreach (s; instructionDirectories) arr ~= JSONValue(s);
            obj["instructionDirectories"] = JSONValue(arr);
        }

        if (!enableSessionTelemetry.isNull)
            obj["enableSessionTelemetry"] = enableSessionTelemetry.get;

        return obj;
    }
}

/// Configuration used when resuming an existing session.
struct ResumeSessionConfig
{
    /// The session ID to resume.
    string sessionId;

    /// Whether to stream assistant responses as deltas.
    bool streaming = false;

    /// Tools available in this session.
    Tool[] tools;

    /// Directories containing instruction files for the session.
    string[] instructionDirectories;

    /// Enable session telemetry reporting.
    Nullable!bool enableSessionTelemetry;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["sessionId"] = sessionId;
        obj["streaming"] = streaming;

        if (tools.length > 0)
        {
            JSONValue[] arr;
            foreach (ref t; tools)
                arr ~= t.toJson();
            obj["tools"] = JSONValue(arr);
        }

        if (instructionDirectories.length > 0)
        {
            JSONValue[] arr;
            foreach (s; instructionDirectories) arr ~= JSONValue(s);
            obj["instructionDirectories"] = JSONValue(arr);
        }

        if (!enableSessionTelemetry.isNull)
            obj["enableSessionTelemetry"] = enableSessionTelemetry.get;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// Tool definition
// ---------------------------------------------------------------------------

/// A single tool that the assistant can invoke.
struct Tool
{
    /// Machine-readable name.
    string name;

    /// Human-readable description shown to the model.
    string description;

    /// JSON Schema describing accepted parameters.
    JSONValue parametersSchema;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["name"] = name;
        obj["description"] = description;
        obj["parameters"] = parametersSchema;
        return obj;
    }
}

/// Represents the result of a tool invocation.
struct ToolResult
{
    /// The tool call ID this result corresponds to.
    string id;

    /// The result payload (typically a string or JSON blob).
    string result;

    /// Whether the tool invocation failed.
    bool isError = false;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["id"] = id;
        obj["result"] = result;
        if (isError)
            obj["isError"] = true;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// Message options
// ---------------------------------------------------------------------------

/// Options for sending a message to a session.
struct MessageOptions
{
    /// File attachments or context references.
    JSONValue[] attachments;

    /// Desired response format.
    Nullable!string responseFormat;
}

// ---------------------------------------------------------------------------
// Elicitation
// ---------------------------------------------------------------------------

/// Context for an elicitation request from the server.
struct ElicitationRequest
{
    string sessionId;
    string message;
    Nullable!string mode;
    JSONValue requestedSchema;
}

/// Result from an elicitation handler.
struct ElicitationResult
{
    string action = "accept";
    JSONValue content;
}

// ---------------------------------------------------------------------------
// ExitPlanMode
// ---------------------------------------------------------------------------

/// Request to exit plan mode for a session.
struct ExitPlanModeRequest
{
    string sessionId;
}

/// Response to an exit-plan-mode request.
struct ExitPlanModeResponse
{
    bool approved = true;
}

// ---------------------------------------------------------------------------
// TraceContext
// ---------------------------------------------------------------------------

/// W3C Trace Context for distributed tracing.
struct TraceContext
{
    Nullable!string traceparent;
    Nullable!string tracestate;
}

/// Callback that provides trace context for outgoing RPCs.
alias TraceContextProvider = TraceContext delegate() @safe;

// ---------------------------------------------------------------------------
// ModelCapabilitiesOverride
// ---------------------------------------------------------------------------

/// Fine-grained model feature support flags.
struct ModelCapabilitiesSupports
{
    bool agentMode = false;
    bool toolUse = false;
    bool vision = false;
}

/// Token limits for a model.
struct ModelCapabilitiesLimits
{
    int maxContextTokens = 0;
    int maxOutputTokens = 0;
}

/// Override for model capabilities sent during session creation.
struct ModelCapabilitiesOverride
{
    ModelCapabilitiesSupports supports;
    ModelCapabilitiesLimits limits;
    Nullable!string defaultModel;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        auto sup = JSONValue(string[string].init);
        sup["agentMode"] = JSONValue(supports.agentMode);
        sup["toolUse"] = JSONValue(supports.toolUse);
        sup["vision"] = JSONValue(supports.vision);
        obj["supports"] = sup;

        auto lim = JSONValue(string[string].init);
        lim["maxContextTokens"] = JSONValue(limits.maxContextTokens);
        lim["maxOutputTokens"] = JSONValue(limits.maxOutputTokens);
        obj["limits"] = lim;

        if (!defaultModel.isNull)
            obj["defaultModel"] = defaultModel.get;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// Slash command types
// ---------------------------------------------------------------------------

/// Completion type for slash command input.
enum SlashCommandInputCompletion : string
{
    directory = "directory",
}

/// Kind of a slash command.
enum SlashCommandKind : string
{
    builtin = "builtin",
    client = "client",
    skill = "skill",
}

/// Price category for the model picker.
enum ModelPickerPriceCategory : string
{
    high = "high",
    low = "low",
    medium = "medium",
    veryHigh = "very_high",
}

/// Input specification for a slash command.
struct SlashCommandInput
{
    string hint;
    Nullable!SlashCommandInputCompletion completion;
}

/// Information about a slash command.
struct SlashCommandInfo
{
    bool allowDuringAgentExecution;
    string description;
    SlashCommandKind kind;
    string name;
    Nullable!(string[]) aliases;
    Nullable!bool experimental;
    Nullable!SlashCommandInput input;
}

// ---------------------------------------------------------------------------
// Command request types
// ---------------------------------------------------------------------------

/// Request to invoke a command.
struct CommandsInvokeRequest
{
    string name;
    Nullable!string input;
}

/// Request to list available commands.
struct CommandsListRequest
{
    Nullable!bool includeBuiltins;
    Nullable!bool includeClientCommands;
    Nullable!bool includeSkills;
}

// ---------------------------------------------------------------------------
// Model billing types
// ---------------------------------------------------------------------------

/// Token prices for model billing.
struct ModelBillingTokenPrices
{
    Nullable!int batchSize;
    Nullable!int cachePrice;
    Nullable!int inputPrice;
    Nullable!int outputPrice;
}

/// Model billing information.
struct ModelBilling
{
    double multiplier = 0.0;
    Nullable!ModelBillingTokenPrices tokenPrices;
    Nullable!ModelPickerPriceCategory pickerPriceCategory;
}

/// Experimental
/// Diagnostics from loading skills.
struct SkillsLoadDiagnostics
{
    string[] errors;
    string[] warnings;
}

/// Experimental
/// Mode for remote session control.
enum RemoteSessionMode : string
{
    export_ = "export",
    off     = "off",
    on      = "on",
}

/// Experimental
/// Request to enable or configure remote session mode.
struct RemoteEnableRequest
{
    Nullable!RemoteSessionMode mode;
}

/// Experimental
/// Result of enabling remote session mode.
struct RemoteEnableResult
{
    bool remoteSteerable;
    Nullable!string url;
}

// ---------------------------------------------------------------------------
// Session events
// ---------------------------------------------------------------------------

/// Discriminated tag for session events.
enum SessionEventType : string
{
    assistantMessage      = "assistant.message",
    assistantMessageDelta = "assistant.message_delta",
    assistantReasoning      = "assistant.reasoning",
    assistantReasoningDelta = "assistant.reasoning_delta",
    toolCall              = "tool.call",
    sessionIdle           = "session.idle",
    sessionError          = "session.error",
    sessionEnd            = "session.end",
    compactionStart       = "session.compaction_start",
    compactionComplete    = "session.compaction_complete",
    unknown               = "",
}

/// A single event emitted by a session.
struct SessionEvent
{
    /// The event type tag.
    SessionEventType type;

    /// Full JSON payload of the event.
    JSONValue payload;

    /// Unique event identifier (UUID).
    string id;

    /// ISO 8601 timestamp when the event was created.
    string timestamp;

    /// ID of the preceding event. Null for the first event.
    Nullable!string parentId;

    /// Sub-agent instance identifier. Absent for root agent events.
    Nullable!string agentId;

    /// When true, the event is transient and not persisted.
    Nullable!bool ephemeral;

    /// Convenience: extract a string field from payload.
    Nullable!string stringField(string key) const @safe
    {
        if (payload.type != JSONType.object) return Nullable!string.init;
        auto p = key in payload;
        if (p is null) return Nullable!string.init;
        if ((*p).type == JSONType.string)
            return Nullable!string((*p).str);
        return Nullable!string.init;
    }
}

// ---------------------------------------------------------------------------
// Response wrappers
// ---------------------------------------------------------------------------

/// Metadata returned when a session is created.
struct SessionInfo
{
    string sessionId;

    static SessionInfo fromJson(JSONValue v) @safe
    {
        SessionInfo info;
        if (v.type == JSONType.object)
        {
            auto p = "sessionId" in v;
            if (p !is null && (*p).type == JSONType.string)
                info.sessionId = (*p).str;
        }
        return info;
    }
}

/// Status returned by the CLI status RPC.
struct CopilotStatus
{
    string status;
    Nullable!string version_;

    static CopilotStatus fromJson(JSONValue v) @safe
    {
        CopilotStatus s;
        if (v.type != JSONType.object) return s;
        auto ps = "status" in v;
        if (ps !is null && (*ps).type == JSONType.string)
            s.status = (*ps).str;
        auto pv = "version" in v;
        if (pv !is null && (*pv).type == JSONType.string)
            s.version_ = (*pv).str;
        return s;
    }
}

// ---------------------------------------------------------------------------
// JSON helpers
// ---------------------------------------------------------------------------

/// Parse a SessionEventType from its wire string.
SessionEventType parseEventType(string s) @safe pure nothrow
{
    import std.traits : EnumMembers;
    static foreach (m; EnumMembers!SessionEventType)
    {
        if (s == cast(string) m)
            return m;
    }
    return SessionEventType.unknown;
}
