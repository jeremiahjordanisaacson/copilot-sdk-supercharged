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

    /// Session filesystem path for persistence.
    Nullable!string sessionFs;

    /// Idle timeout in seconds before a session is reaped.
    Nullable!uint sessionIdleTimeoutSeconds;
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
