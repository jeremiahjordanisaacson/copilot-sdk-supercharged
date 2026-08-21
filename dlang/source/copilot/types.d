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

    /// Custom handler used to intercept outbound CAPI HTTP requests.
    CopilotRequestHandler requestHandler;

    /// BYOK bearer-token provider invoked for authenticated model requests.
    BearerTokenProvider bearerTokenProvider;

    /// Directories scanned for built-in plugins.
    string[] builtinPluginDirectories;

    /// Use the in-process FFI transport instead of a child CLI process.
    bool inProcess = false;
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
// Upstream-sync feature types (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Context passed to a bearer-token / MCP-auth provider callback.
struct ProviderTokenArgs
{
    string sessionId;
    string serverUrl;
    string[] scopes;
}

/// Per-session spending / usage guardrails.
struct SessionLimitsConfig
{
    /// Maximum number of AI credits this session may consume.
    Nullable!double maxAiCredits;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!maxAiCredits.isNull)
            obj["maxAiCredits"] = maxAiCredits.get;
        return obj;
    }
}

/// Persistent session-memory settings.
struct MemoryConfiguration
{
    /// Whether persistent session memory is enabled.
    bool enabled = false;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["enabled"] = enabled;
        return obj;
    }
}

/// Custom handler used to intercept outbound CAPI HTTP requests.
alias CopilotRequestHandler = JSONValue delegate(JSONValue);

/// BYOK bearer-token provider invoked for authenticated model requests.
alias BearerTokenProvider = string delegate(ProviderTokenArgs);

/// Handler invoked to satisfy an MCP OAuth authorization request.
alias McpAuthHandler = string delegate(ProviderTokenArgs);

/// Controls when a tool definition is materialised for the model.
enum ToolDefer : string
{
    auto_ = "auto",
    never = "never",
}

/// Named sections of the composed system message.
enum SystemMessageSection : string
{
    preamble = "preamble",
    identity = "identity",
    toolInstructions = "tool_instructions",
    preserve = "preserve",
}

/// Hook-type identifiers (the "hookType" field of a hooks.invoke request).
enum HookType : string
{
    preToolUse          = "preToolUse",
    postToolUse         = "postToolUse",
    userPromptSubmitted = "userPromptSubmitted",
    userPromptTransformed = "userPromptTransformed",
    sessionStart        = "sessionStart",
    sessionEnd          = "sessionEnd",
    errorOccurred       = "errorOccurred",
    preMcpToolCall      = "preMcpToolCall",
}

/// GitHub-anchored attachment type identifiers.
enum GitHubAttachmentType : string
{
    commit         = "GitHubCommit",
    release        = "GitHubRelease",
    actionsJob     = "GitHubActionsJob",
    repository     = "GitHubRepository",
    fileDiff       = "GitHubFileDiff",
    treeComparison = "GitHubTreeComparison",
    pullRequest    = "GitHubPullRequest",
    issue          = "GitHubIssue",
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

    /// Enable inline source citations in assistant responses.
    bool enableCitations = false;

    /// Built-in agents to exclude from this session.
    string[] excludedBuiltinAgents;

    /// Per-session spending / usage limits.
    Nullable!SessionLimitsConfig sessionLimits;

    /// Persistent session-memory configuration.
    Nullable!MemoryConfiguration memory;

    /// OTLP telemetry protocol ("grpc" or "http/protobuf").
    Nullable!string otlpProtocol;

    /// Stream responses over a WebSocket transport.
    bool enableWebSocketResponses = false;

    /// Experiment (feature-flag) assignments forwarded to the CLI.
    JSONValue expAssignments;

    /// Handler invoked to satisfy MCP OAuth authorization requests.
    McpAuthHandler onMcpAuthRequest;

    /// When true, the client handles MCP OAuth authorization requests.
    bool mcpAuthHandler = false;

    /// Enable session rewind (revert the session to an earlier turn).
    bool rewindEnabled = false;

    /// Additional working directories exposed to the session.
    string[] additionalDirectories;

    /// MCP servers to disable for this session.
    string[] disabledMcpServers;

    /// GitHub MCP tool configuration (opaque object).
    JSONValue githubMcpToolConfig;

    /// Canvas provider configuration (opaque object).
    JSONValue canvasProvider;

    /// Restrict custom agents to locally-defined ones only.
    bool customAgentsLocalOnly = false;

    /// Tool-search configuration (opaque object).
    JSONValue toolSearch;

    /// Enable experimental mode.
    bool experimentalMode = false;

    /// Enable content exclusion.
    bool contentExclusion = false;

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

        if (enableCitations)
            obj["enableCitations"] = true;

        if (excludedBuiltinAgents.length > 0)
        {
            JSONValue[] arr;
            foreach (a; excludedBuiltinAgents) arr ~= JSONValue(a);
            obj["excludedBuiltinAgents"] = JSONValue(arr);
        }

        if (!sessionLimits.isNull)
            obj["sessionLimits"] = sessionLimits.get.toJson();

        if (!memory.isNull)
            obj["memory"] = memory.get.toJson();

        if (!otlpProtocol.isNull)
            obj["otlpProtocol"] = otlpProtocol.get;

        if (enableWebSocketResponses)
            obj["enableWebSocketResponses"] = true;

        if (expAssignments.type == JSONType.object)
            obj["expAssignments"] = expAssignments;

        if (mcpAuthHandler || onMcpAuthRequest !is null)
            obj["mcpAuthHandler"] = true;

        if (rewindEnabled)
            obj["rewindEnabled"] = true;

        if (additionalDirectories.length > 0)
        {
            JSONValue[] arr;
            foreach (s; additionalDirectories) arr ~= JSONValue(s);
            obj["additionalDirectories"] = JSONValue(arr);
        }

        if (disabledMcpServers.length > 0)
        {
            JSONValue[] arr;
            foreach (s; disabledMcpServers) arr ~= JSONValue(s);
            obj["disabledMcpServers"] = JSONValue(arr);
        }

        if (githubMcpToolConfig.type == JSONType.object)
            obj["githubMcpToolConfig"] = githubMcpToolConfig;

        if (canvasProvider.type == JSONType.object)
            obj["canvasProvider"] = canvasProvider;

        if (customAgentsLocalOnly)
            obj["customAgentsLocalOnly"] = true;

        if (toolSearch.type == JSONType.object)
            obj["toolSearch"] = toolSearch;

        if (experimentalMode)
            obj["experimentalMode"] = true;

        if (contentExclusion)
            obj["contentExclusion"] = true;

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

    /// Agent mode override for this message (e.g. "agent" or "ask").
    Nullable!string agentMode;

    /// Prompt text shown to the user in place of the raw message.
    Nullable!string displayPrompt;

    /// Additional HTTP headers sent with this message request.
    string[string] requestHeaders;
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

// ---------------------------------------------------------------------------
// Agent factories & permissions (2026-08 upstream sync)
// ---------------------------------------------------------------------------

/// Authoring options for a programmatic agent factory.
struct AgentFactoryOptions
{
    /// JSON schema describing the factory's arguments (opaque object).
    JSONValue argsSchema;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (argsSchema.type == JSONType.object)
            obj["argsSchema"] = argsSchema;
        return obj;
    }
}

/// Reply to a permission request, including optional decision context.
struct PermissionResponse
{
    /// Whether the requested action is allowed.
    bool allowed = false;

    /// Optional human-readable reason for the decision.
    Nullable!string reason;

    /// Opaque decision context forwarded with the reply.
    JSONValue decisionContext;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["allowed"] = allowed;
        if (!reason.isNull)
            obj["reason"] = reason.get;
        if (decisionContext.type == JSONType.object)
            obj["decisionContext"] = decisionContext;
        return obj;
    }
}

// ---------------------------------------------------------------------------
// Exit plan mode
// ---------------------------------------------------------------------------

/// Request to exit plan mode and continue with a selected action.
struct ExitPlanModeRequest
{
    /// The session that produced this request.
    string sessionId;

    /// Summary of the plan or proposed next step.
    string summary;

    /// Full plan content, when available.
    Nullable!string planContent;

    /// Available actions the user can select.
    string[] actions;

    /// The action recommended by the runtime.
    string recommendedAction;

    static ExitPlanModeRequest fromJson(JSONValue v) @safe
    {
        ExitPlanModeRequest req;
        if (v.type != JSONType.object) return req;
        auto pSession = "sessionId" in v;
        if (pSession !is null && (*pSession).type == JSONType.string)
            req.sessionId = (*pSession).str;
        auto pSummary = "summary" in v;
        if (pSummary !is null && (*pSummary).type == JSONType.string)
            req.summary = (*pSummary).str;
        auto pPlan = "planContent" in v;
        if (pPlan !is null && (*pPlan).type == JSONType.string)
            req.planContent = (*pPlan).str;
        auto pActions = "actions" in v;
        if (pActions !is null && (*pActions).type == JSONType.array)
            foreach (a; (*pActions).array)
                if (a.type == JSONType.string)
                    req.actions ~= a.str;
        auto pRec = "recommendedAction" in v;
        if (pRec !is null && (*pRec).type == JSONType.string)
            req.recommendedAction = (*pRec).str;
        return req;
    }
}

/// Response to an exit-plan-mode request.
struct ExitPlanModeResponse
{
    /// Whether the user approved exiting plan mode.
    bool approved = false;

    /// Selected action, if the user chose one.
    Nullable!string selectedAction;

    /// Optional feedback provided by the user.
    Nullable!string feedback;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["approved"] = approved;
        if (!selectedAction.isNull)
            obj["selectedAction"] = selectedAction.get;
        if (!feedback.isNull)
            obj["feedback"] = feedback.get;
        return obj;
    }
}
