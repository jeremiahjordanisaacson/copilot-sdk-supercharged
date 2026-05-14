/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#pragma once

#include <functional>
#include <map>
#include <optional>
#include <string>
#include <vector>

#include <nlohmann/json.hpp>

namespace copilot {

// ============================================================================
// Connection State
// ============================================================================

enum class ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Error
};

inline std::string connectionStateToString(ConnectionState state) {
    switch (state) {
        case ConnectionState::Disconnected: return "disconnected";
        case ConnectionState::Connecting:   return "connecting";
        case ConnectionState::Connected:    return "connected";
        case ConnectionState::Error:        return "error";
    }
    return "unknown";
}

// ============================================================================
// Tool Types
// ============================================================================

/// Result type for a tool invocation.
using ToolResultType = std::string; // "success", "failure", "rejected", "denied"

/// Binary result payload from a tool.
struct ToolBinaryResult {
    std::string data;
    std::string mimeType;
    std::string type;
    std::optional<std::string> description;
};

inline void to_json(nlohmann::json& j, const ToolBinaryResult& r) {
    j = {{"data", r.data}, {"mimeType", r.mimeType}, {"type", r.type}};
    if (r.description) j["description"] = *r.description;
}

inline void from_json(const nlohmann::json& j, ToolBinaryResult& r) {
    j.at("data").get_to(r.data);
    j.at("mimeType").get_to(r.mimeType);
    j.at("type").get_to(r.type);
    if (j.contains("description")) r.description = j["description"].get<std::string>();
}

/// Structured result object returned by tools.
struct ToolResultObject {
    std::string textResultForLlm;
    std::vector<ToolBinaryResult> binaryResultsForLlm;
    ToolResultType resultType = "success";
    std::optional<std::string> error;
    std::optional<std::string> sessionLog;
    std::optional<nlohmann::json> toolTelemetry;
};

inline void to_json(nlohmann::json& j, const ToolResultObject& r) {
    j = {{"textResultForLlm", r.textResultForLlm}, {"resultType", r.resultType}};
    if (!r.binaryResultsForLlm.empty()) j["binaryResultsForLlm"] = r.binaryResultsForLlm;
    if (r.error) j["error"] = *r.error;
    if (r.sessionLog) j["sessionLog"] = *r.sessionLog;
    if (r.toolTelemetry) j["toolTelemetry"] = *r.toolTelemetry;
    else j["toolTelemetry"] = nlohmann::json::object();
}

inline void from_json(const nlohmann::json& j, ToolResultObject& r) {
    j.at("textResultForLlm").get_to(r.textResultForLlm);
    j.at("resultType").get_to(r.resultType);
    if (j.contains("binaryResultsForLlm")) j["binaryResultsForLlm"].get_to(r.binaryResultsForLlm);
    if (j.contains("error")) r.error = j["error"].get<std::string>();
    if (j.contains("sessionLog")) r.sessionLog = j["sessionLog"].get<std::string>();
    if (j.contains("toolTelemetry")) r.toolTelemetry = j["toolTelemetry"];
}

/// Context for a tool invocation, passed to the handler.
struct ToolInvocation {
    std::string sessionId;
    std::string toolCallId;
    std::string toolName;
    nlohmann::json arguments;
};

/// Handler function type for tool execution.
/// Returns a ToolResultObject.
using ToolHandler = std::function<ToolResultObject(const nlohmann::json& args, const ToolInvocation& invocation)>;

/// Definition of a tool that can be registered with a session.
struct Tool {
    std::string name;
    std::optional<std::string> description;
    std::optional<nlohmann::json> parameters; // JSON Schema
    ToolHandler handler;
};

// ============================================================================
// System Message Configuration
// ============================================================================

/// Known system prompt section identifiers for the "customize" mode.
namespace SystemPromptSection {
    inline constexpr const char* Identity = "identity";
    inline constexpr const char* Tone = "tone";
    inline constexpr const char* ToolEfficiency = "tool_efficiency";
    inline constexpr const char* EnvironmentContext = "environment_context";
    inline constexpr const char* CodeChangeRules = "code_change_rules";
    inline constexpr const char* Guidelines = "guidelines";
    inline constexpr const char* Safety = "safety";
    inline constexpr const char* ToolInstructions = "tool_instructions";
    inline constexpr const char* CustomInstructions = "custom_instructions";
    inline constexpr const char* LastInstructions = "last_instructions";
}

/// Override action for a system prompt section.
namespace SectionOverrideAction {
    inline constexpr const char* Replace = "replace";
    inline constexpr const char* Remove = "remove";
    inline constexpr const char* Append = "append";
    inline constexpr const char* Prepend = "prepend";
}

/// Override operation for a single system prompt section.
struct SectionOverride {
    std::string action;
    std::optional<std::string> content;
};

inline void to_json(nlohmann::json& j, const SectionOverride& o) {
    j = {{"action", o.action}};
    if (o.content) j["content"] = *o.content;
}

inline void from_json(const nlohmann::json& j, SectionOverride& o) {
    j.at("action").get_to(o.action);
    if (j.contains("content")) o.content = j["content"].get<std::string>();
}

/// System message configuration for session creation.
///
/// Supports "append" (default), "replace", and "customize" modes.
struct SystemMessageConfig {
    std::string mode;    // "append", "replace", or "customize"
    std::string content;
    std::optional<std::map<std::string, SectionOverride>> sections; // "customize" mode only
};

inline void to_json(nlohmann::json& j, const SystemMessageConfig& c) {
    j = nlohmann::json::object();
    if (!c.mode.empty()) j["mode"] = c.mode;
    if (!c.content.empty()) j["content"] = c.content;
    if (c.sections) j["sections"] = *c.sections;
}

// ============================================================================
// Provider Configuration
// ============================================================================

struct AzureProviderOptions {
    std::optional<std::string> apiVersion;
};

inline void to_json(nlohmann::json& j, const AzureProviderOptions& o) {
    j = nlohmann::json::object();
    if (o.apiVersion) j["apiVersion"] = *o.apiVersion;
}

struct ProviderConfig {
    std::optional<std::string> type;       // "openai", "azure", "anthropic"
    std::optional<std::string> wireApi;    // "completions", "responses"
    std::string baseUrl;
    std::optional<std::string> apiKey;
    std::optional<std::string> bearerToken;
    std::optional<AzureProviderOptions> azure;
};

inline void to_json(nlohmann::json& j, const ProviderConfig& c) {
    j = {{"baseUrl", c.baseUrl}};
    if (c.type) j["type"] = *c.type;
    if (c.wireApi) j["wireApi"] = *c.wireApi;
    if (c.apiKey) j["apiKey"] = *c.apiKey;
    if (c.bearerToken) j["bearerToken"] = *c.bearerToken;
    if (c.azure) j["azure"] = *c.azure;
}

// ============================================================================
// MCP Server Configuration
// ============================================================================

/// Configuration for a local/stdio MCP server.
struct MCPStdioServerConfig {
    std::vector<std::string> tools;
    std::optional<std::string> type;     // "local" or "stdio"
    std::optional<int> timeout;          // Tool call timeout in ms
    std::string command;
    std::vector<std::string> args;
    std::optional<std::map<std::string, std::string>> env;
    std::optional<std::string> cwd;
};

inline void to_json(nlohmann::json& j, const MCPStdioServerConfig& c) {
    j = {{"tools", c.tools}, {"command", c.command}, {"args", c.args}};
    if (c.type) j["type"] = *c.type;
    if (c.timeout) j["timeout"] = *c.timeout;
    if (c.env) j["env"] = *c.env;
    if (c.cwd) j["cwd"] = *c.cwd;
}

inline void from_json(const nlohmann::json& j, MCPStdioServerConfig& c) {
    j.at("tools").get_to(c.tools);
    j.at("command").get_to(c.command);
    if (j.contains("args")) j["args"].get_to(c.args);
    if (j.contains("type")) c.type = j["type"].get<std::string>();
    if (j.contains("timeout")) c.timeout = j["timeout"].get<int>();
    if (j.contains("env")) c.env = j["env"].get<std::map<std::string, std::string>>();
    if (j.contains("cwd")) c.cwd = j["cwd"].get<std::string>();
}

/// Configuration for a remote MCP server (HTTP or SSE).
struct MCPHTTPServerConfig {
    std::vector<std::string> tools;
    std::string type;                    // "http" or "sse"
    std::optional<int> timeout;          // Tool call timeout in ms
    std::string url;
    std::optional<std::map<std::string, std::string>> headers;
};

inline void to_json(nlohmann::json& j, const MCPHTTPServerConfig& c) {
    j = {{"tools", c.tools}, {"type", c.type}, {"url", c.url}};
    if (c.timeout) j["timeout"] = *c.timeout;
    if (c.headers) j["headers"] = *c.headers;
}

inline void from_json(const nlohmann::json& j, MCPHTTPServerConfig& c) {
    j.at("tools").get_to(c.tools);
    j.at("type").get_to(c.type);
    j.at("url").get_to(c.url);
    if (j.contains("timeout")) c.timeout = j["timeout"].get<int>();
    if (j.contains("headers")) c.headers = j["headers"].get<std::map<std::string, std::string>>();
}

/// MCP server config stored as JSON to support both stdio and HTTP variants.
using MCPServerConfig = nlohmann::json;

// ============================================================================
// Custom Agent Configuration
// ============================================================================

struct CustomAgentConfig {
    std::string name;
    std::optional<std::string> displayName;
    std::optional<std::string> description;
    std::optional<std::vector<std::string>> tools;
    std::string prompt;
    std::optional<std::map<std::string, MCPServerConfig>> mcpServers;
    std::optional<bool> infer;

    /// List of skill names to preload into this agent's context.
    std::optional<std::vector<std::string>> skills;
};

inline void to_json(nlohmann::json& j, const CustomAgentConfig& c) {
    j = {{"name", c.name}, {"prompt", c.prompt}};
    if (c.displayName) j["displayName"] = *c.displayName;
    if (c.description) j["description"] = *c.description;
    if (c.tools) j["tools"] = *c.tools;
    if (c.mcpServers) j["mcpServers"] = *c.mcpServers;
    if (c.infer) j["infer"] = *c.infer;
    if (c.skills) j["skills"] = *c.skills;
}

// ============================================================================
// Infinite Session Configuration
// ============================================================================

struct InfiniteSessionConfig {
    std::optional<bool> enabled;
    std::optional<double> backgroundCompactionThreshold;
    std::optional<double> bufferExhaustionThreshold;
};

inline void to_json(nlohmann::json& j, const InfiniteSessionConfig& c) {
    j = nlohmann::json::object();
    if (c.enabled) j["enabled"] = *c.enabled;
    if (c.backgroundCompactionThreshold) j["backgroundCompactionThreshold"] = *c.backgroundCompactionThreshold;
    if (c.bufferExhaustionThreshold) j["bufferExhaustionThreshold"] = *c.bufferExhaustionThreshold;
}

// ============================================================================
// Permission Types
// ============================================================================

struct PermissionRequest {
    std::string kind; // "shell", "write", "mcp", "read", "url"
    std::optional<std::string> toolCallId;
    nlohmann::json extra; // Additional fields vary by kind
};

inline void from_json(const nlohmann::json& j, PermissionRequest& r) {
    j.at("kind").get_to(r.kind);
    if (j.contains("toolCallId")) r.toolCallId = j["toolCallId"].get<std::string>();
    r.extra = j;
}

struct PermissionRequestResult {
    std::string kind; // "approved", "denied-by-rules", "denied-no-approval-rule-and-could-not-request-from-user", etc.
    std::optional<nlohmann::json> rules;
};

inline void to_json(nlohmann::json& j, const PermissionRequestResult& r) {
    j = {{"kind", r.kind}};
    if (r.rules) j["rules"] = *r.rules;
}

/// Handler for permission requests.
using PermissionHandler = std::function<PermissionRequestResult(
    const PermissionRequest& request, const std::string& sessionId)>;

// ============================================================================
// User Input Types
// ============================================================================

struct UserInputRequest {
    std::string question;
    std::optional<std::vector<std::string>> choices;
    std::optional<bool> allowFreeform;
};

inline void from_json(const nlohmann::json& j, UserInputRequest& r) {
    j.at("question").get_to(r.question);
    if (j.contains("choices")) r.choices = j["choices"].get<std::vector<std::string>>();
    if (j.contains("allowFreeform")) r.allowFreeform = j["allowFreeform"].get<bool>();
}

struct UserInputResponse {
    std::string answer;
    bool wasFreeform = false;
};

inline void to_json(nlohmann::json& j, const UserInputResponse& r) {
    j = {{"answer", r.answer}, {"wasFreeform", r.wasFreeform}};
}

/// Handler for user input requests.
using UserInputHandler = std::function<UserInputResponse(
    const UserInputRequest& request, const std::string& sessionId)>;

// ============================================================================
// Hook Types
// ============================================================================

struct PreToolUseHookInput {
    int64_t timestamp = 0;
    std::string cwd;
    std::string toolName;
    nlohmann::json toolArgs;
};

inline void from_json(const nlohmann::json& j, PreToolUseHookInput& i) {
    if (j.contains("timestamp")) j["timestamp"].get_to(i.timestamp);
    if (j.contains("cwd")) j["cwd"].get_to(i.cwd);
    if (j.contains("toolName")) j["toolName"].get_to(i.toolName);
    if (j.contains("toolArgs")) i.toolArgs = j["toolArgs"];
}

struct PreToolUseHookOutput {
    std::optional<std::string> permissionDecision; // "allow", "deny", "ask"
    std::optional<std::string> permissionDecisionReason;
    std::optional<nlohmann::json> modifiedArgs;
    std::optional<std::string> additionalContext;
    std::optional<bool> suppressOutput;
};

inline void to_json(nlohmann::json& j, const PreToolUseHookOutput& o) {
    j = nlohmann::json::object();
    if (o.permissionDecision) j["permissionDecision"] = *o.permissionDecision;
    if (o.permissionDecisionReason) j["permissionDecisionReason"] = *o.permissionDecisionReason;
    if (o.modifiedArgs) j["modifiedArgs"] = *o.modifiedArgs;
    if (o.additionalContext) j["additionalContext"] = *o.additionalContext;
    if (o.suppressOutput) j["suppressOutput"] = *o.suppressOutput;
}

using PreToolUseHandler = std::function<std::optional<PreToolUseHookOutput>(
    const PreToolUseHookInput& input, const std::string& sessionId)>;

struct PostToolUseHookInput {
    int64_t timestamp = 0;
    std::string cwd;
    std::string toolName;
    nlohmann::json toolArgs;
    nlohmann::json toolResult;
};

inline void from_json(const nlohmann::json& j, PostToolUseHookInput& i) {
    if (j.contains("timestamp")) j["timestamp"].get_to(i.timestamp);
    if (j.contains("cwd")) j["cwd"].get_to(i.cwd);
    if (j.contains("toolName")) j["toolName"].get_to(i.toolName);
    if (j.contains("toolArgs")) i.toolArgs = j["toolArgs"];
    if (j.contains("toolResult")) i.toolResult = j["toolResult"];
}

struct PostToolUseHookOutput {
    std::optional<nlohmann::json> modifiedResult;
    std::optional<std::string> additionalContext;
    std::optional<bool> suppressOutput;
};

inline void to_json(nlohmann::json& j, const PostToolUseHookOutput& o) {
    j = nlohmann::json::object();
    if (o.modifiedResult) j["modifiedResult"] = *o.modifiedResult;
    if (o.additionalContext) j["additionalContext"] = *o.additionalContext;
    if (o.suppressOutput) j["suppressOutput"] = *o.suppressOutput;
}

using PostToolUseHandler = std::function<std::optional<PostToolUseHookOutput>(
    const PostToolUseHookInput& input, const std::string& sessionId)>;

struct UserPromptSubmittedHookInput {
    int64_t timestamp = 0;
    std::string cwd;
    std::string prompt;
};

inline void from_json(const nlohmann::json& j, UserPromptSubmittedHookInput& i) {
    if (j.contains("timestamp")) j["timestamp"].get_to(i.timestamp);
    if (j.contains("cwd")) j["cwd"].get_to(i.cwd);
    if (j.contains("prompt")) j["prompt"].get_to(i.prompt);
}

struct UserPromptSubmittedHookOutput {
    std::optional<std::string> modifiedPrompt;
    std::optional<std::string> additionalContext;
    std::optional<bool> suppressOutput;
};

inline void to_json(nlohmann::json& j, const UserPromptSubmittedHookOutput& o) {
    j = nlohmann::json::object();
    if (o.modifiedPrompt) j["modifiedPrompt"] = *o.modifiedPrompt;
    if (o.additionalContext) j["additionalContext"] = *o.additionalContext;
    if (o.suppressOutput) j["suppressOutput"] = *o.suppressOutput;
}

using UserPromptSubmittedHandler = std::function<std::optional<UserPromptSubmittedHookOutput>(
    const UserPromptSubmittedHookInput& input, const std::string& sessionId)>;

struct SessionStartHookInput {
    int64_t timestamp = 0;
    std::string cwd;
    std::string source; // "startup", "resume", "new"
    std::optional<std::string> initialPrompt;
};

inline void from_json(const nlohmann::json& j, SessionStartHookInput& i) {
    if (j.contains("timestamp")) j["timestamp"].get_to(i.timestamp);
    if (j.contains("cwd")) j["cwd"].get_to(i.cwd);
    if (j.contains("source")) j["source"].get_to(i.source);
    if (j.contains("initialPrompt")) i.initialPrompt = j["initialPrompt"].get<std::string>();
}

struct SessionStartHookOutput {
    std::optional<std::string> additionalContext;
    std::optional<nlohmann::json> modifiedConfig;
};

inline void to_json(nlohmann::json& j, const SessionStartHookOutput& o) {
    j = nlohmann::json::object();
    if (o.additionalContext) j["additionalContext"] = *o.additionalContext;
    if (o.modifiedConfig) j["modifiedConfig"] = *o.modifiedConfig;
}

using SessionStartHandler = std::function<std::optional<SessionStartHookOutput>(
    const SessionStartHookInput& input, const std::string& sessionId)>;

struct SessionEndHookInput {
    int64_t timestamp = 0;
    std::string cwd;
    std::string reason; // "complete", "error", "abort", "timeout", "user_exit"
    std::optional<std::string> finalMessage;
    std::optional<std::string> error;
};

inline void from_json(const nlohmann::json& j, SessionEndHookInput& i) {
    if (j.contains("timestamp")) j["timestamp"].get_to(i.timestamp);
    if (j.contains("cwd")) j["cwd"].get_to(i.cwd);
    if (j.contains("reason")) j["reason"].get_to(i.reason);
    if (j.contains("finalMessage")) i.finalMessage = j["finalMessage"].get<std::string>();
    if (j.contains("error")) i.error = j["error"].get<std::string>();
}

struct SessionEndHookOutput {
    std::optional<bool> suppressOutput;
    std::optional<std::vector<std::string>> cleanupActions;
    std::optional<std::string> sessionSummary;
};

inline void to_json(nlohmann::json& j, const SessionEndHookOutput& o) {
    j = nlohmann::json::object();
    if (o.suppressOutput) j["suppressOutput"] = *o.suppressOutput;
    if (o.cleanupActions) j["cleanupActions"] = *o.cleanupActions;
    if (o.sessionSummary) j["sessionSummary"] = *o.sessionSummary;
}

using SessionEndHandler = std::function<std::optional<SessionEndHookOutput>(
    const SessionEndHookInput& input, const std::string& sessionId)>;

struct ErrorOccurredHookInput {
    int64_t timestamp = 0;
    std::string cwd;
    std::string error;
    std::string errorContext; // "model_call", "tool_execution", "system", "user_input"
    bool recoverable = false;
};

inline void from_json(const nlohmann::json& j, ErrorOccurredHookInput& i) {
    if (j.contains("timestamp")) j["timestamp"].get_to(i.timestamp);
    if (j.contains("cwd")) j["cwd"].get_to(i.cwd);
    if (j.contains("error")) j["error"].get_to(i.error);
    if (j.contains("errorContext")) j["errorContext"].get_to(i.errorContext);
    if (j.contains("recoverable")) j["recoverable"].get_to(i.recoverable);
}

struct ErrorOccurredHookOutput {
    std::optional<bool> suppressOutput;
    std::optional<std::string> errorHandling; // "retry", "skip", "abort"
    std::optional<int> retryCount;
    std::optional<std::string> userNotification;
};

inline void to_json(nlohmann::json& j, const ErrorOccurredHookOutput& o) {
    j = nlohmann::json::object();
    if (o.suppressOutput) j["suppressOutput"] = *o.suppressOutput;
    if (o.errorHandling) j["errorHandling"] = *o.errorHandling;
    if (o.retryCount) j["retryCount"] = *o.retryCount;
    if (o.userNotification) j["userNotification"] = *o.userNotification;
}

using ErrorOccurredHandler = std::function<std::optional<ErrorOccurredHookOutput>(
    const ErrorOccurredHookInput& input, const std::string& sessionId)>;

/// Hook handlers for intercepting session lifecycle events.
struct SessionHooks {
    PreToolUseHandler onPreToolUse;
    PostToolUseHandler onPostToolUse;
    UserPromptSubmittedHandler onUserPromptSubmitted;
    SessionStartHandler onSessionStart;
    SessionEndHandler onSessionEnd;
    ErrorOccurredHandler onErrorOccurred;

    /// Returns true if any hook handler is registered.
    bool hasAny() const {
        return onPreToolUse || onPostToolUse || onUserPromptSubmitted ||
               onSessionStart || onSessionEnd || onErrorOccurred;
    }
};

// ============================================================================
// Attachment Types
// ============================================================================

struct Attachment {
    std::string type; // "file", "directory", "selection"
    std::optional<std::string> path;
    std::optional<std::string> displayName;
    std::optional<std::string> filePath;
    std::optional<std::string> text;
    std::optional<nlohmann::json> selection;
};

inline void to_json(nlohmann::json& j, const Attachment& a) {
    j = {{"type", a.type}};
    if (a.path) j["path"] = *a.path;
    if (a.displayName) j["displayName"] = *a.displayName;
    if (a.filePath) j["filePath"] = *a.filePath;
    if (a.text) j["text"] = *a.text;
    if (a.selection) j["selection"] = *a.selection;
}

// ============================================================================
// Session Event
// ============================================================================

/// A session event received from the CLI server.
/// The 'type' field indicates the event kind (e.g., "assistant.message", "session.idle").
/// The 'data' field contains event-specific payload as JSON.
struct SessionEvent {
    std::string id;
    std::string timestamp;
    std::optional<std::string> parentId;
    std::optional<bool> ephemeral;
    std::string type;
    nlohmann::json data;
};

inline void from_json(const nlohmann::json& j, SessionEvent& e) {
    j.at("id").get_to(e.id);
    j.at("timestamp").get_to(e.timestamp);
    if (j.contains("parentId") && !j["parentId"].is_null())
        e.parentId = j["parentId"].get<std::string>();
    if (j.contains("ephemeral"))
        e.ephemeral = j["ephemeral"].get<bool>();
    j.at("type").get_to(e.type);
    if (j.contains("data"))
        e.data = j["data"];
}

inline void to_json(nlohmann::json& j, const SessionEvent& e) {
    j = {{"id", e.id}, {"timestamp", e.timestamp}, {"type", e.type}, {"data", e.data}};
    if (e.parentId) j["parentId"] = *e.parentId; else j["parentId"] = nullptr;
    if (e.ephemeral) j["ephemeral"] = *e.ephemeral;
}

/// Handler for session events.
using SessionEventHandler = std::function<void(const SessionEvent&)>;

// ============================================================================
// Commands
// ============================================================================

/// Context for a slash-command invocation.
struct CommandContext {
    std::string sessionId;
    std::string command;
    std::string commandName;
    std::string args;
};

/// Handler invoked when a registered slash-command is executed.
using CommandHandler = std::function<void(const CommandContext&)>;

/// Definition of a slash command registered with the session.
struct CommandDefinition {
    /// Command name (without leading /).
    std::string name;
    /// Human-readable description shown in command completion UI.
    std::optional<std::string> description;
    /// Handler invoked when the command is executed.
    CommandHandler handler;
};

// ============================================================================
// UI Elicitation
// ============================================================================

/// Context for an elicitation request from the server.
struct ElicitationContext {
    std::string sessionId;
    std::string message;
    std::optional<nlohmann::json> requestedSchema;
    std::optional<std::string> mode;
    std::optional<std::string> elicitationSource;
    std::optional<std::string> url;
};

/// Result returned from an elicitation handler.
struct ElicitationResult {
    /// User action: "accept", "decline", or "cancel".
    std::string action;
    /// Form values submitted by the user (present when action is "accept").
    std::optional<nlohmann::json> content;
};

/// Handler for elicitation requests from the server.
using ElicitationHandler = std::function<ElicitationResult(const ElicitationContext&)>;

// ============================================================================
// Exit Plan Mode
// ============================================================================

/// Request sent by the server when asking the user to approve exiting plan mode.
struct ExitPlanModeRequest {
    std::string sessionId;
};

inline void from_json(const nlohmann::json& j, ExitPlanModeRequest& r) {
    j.at("sessionId").get_to(r.sessionId);
}

/// Response to an exit plan mode request.
struct ExitPlanModeResponse {
    bool approved = true;
};

inline void to_json(nlohmann::json& j, const ExitPlanModeResponse& r) {
    j = {{"approved", r.approved}};
}

/// Handler for exit plan mode requests.
using ExitPlanModeHandler = std::function<ExitPlanModeResponse(const ExitPlanModeRequest& request)>;

// ============================================================================
// Trace Context
// ============================================================================

/// Trace context for distributed tracing (W3C Trace Context format).
struct TraceContext {
    std::optional<std::string> traceparent;
    std::optional<std::string> tracestate;
};

/// Provider callback that returns the current trace context.
using TraceContextProvider = std::function<TraceContext()>;

// ============================================================================
// Session Configuration
// ============================================================================

struct SessionConfig {
    std::optional<std::string> sessionId;
    std::optional<std::string> model;
    std::optional<std::string> reasoningEffort; // "low", "medium", "high", "xhigh"
    std::optional<std::string> configDir;
    std::vector<Tool> tools;
    std::optional<SystemMessageConfig> systemMessage;
    std::optional<std::vector<std::string>> availableTools;
    std::optional<std::vector<std::string>> excludedTools;
    std::optional<ProviderConfig> provider;
    PermissionHandler onPermissionRequest;
    UserInputHandler onUserInputRequest;
    std::optional<SessionHooks> hooks;
    std::optional<std::string> workingDirectory;
    bool streaming = false;

    /// Include sub-agent streaming events in the event stream. Default: true.
    std::optional<bool> includeSubAgentStreamingEvents;

    std::optional<std::map<std::string, MCPServerConfig>> mcpServers;
    std::optional<std::vector<CustomAgentConfig>> customAgents;
    std::optional<std::vector<std::string>> skillDirectories;
    std::optional<std::vector<std::string>> disabledSkills;
    std::optional<InfiniteSessionConfig> infiniteSessions;

    /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
    std::optional<nlohmann::json> modelCapabilities;

    /// When true, auto-discovers MCP server configs from working directory. Default: false.
    std::optional<bool> enableConfigDiscovery;

    /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
    std::optional<std::string> gitHubToken;

    /// Directories containing instruction files for the session.
    std::optional<std::vector<std::string>> instructionDirectories;

    /// Slash commands registered for this session.
    std::vector<CommandDefinition> commands;
    /// Handler for elicitation requests from the server.
    ElicitationHandler onElicitationRequest;
    /// Handler for exit plan mode requests from the server.
    ExitPlanModeHandler onExitPlanMode;
};

struct ResumeSessionConfig {
    std::optional<std::string> model;
    std::optional<std::string> reasoningEffort;
    std::vector<Tool> tools;
    std::optional<SystemMessageConfig> systemMessage;
    std::optional<std::vector<std::string>> availableTools;
    std::optional<std::vector<std::string>> excludedTools;
    std::optional<ProviderConfig> provider;
    bool streaming = false;
    PermissionHandler onPermissionRequest;
    UserInputHandler onUserInputRequest;
    std::optional<SessionHooks> hooks;
    std::optional<std::string> workingDirectory;
    std::optional<std::string> configDir;
    std::optional<std::map<std::string, MCPServerConfig>> mcpServers;
    std::optional<std::vector<CustomAgentConfig>> customAgents;
    std::optional<std::vector<std::string>> skillDirectories;
    std::optional<std::vector<std::string>> disabledSkills;
    std::optional<InfiniteSessionConfig> infiniteSessions;
    bool disableResume = false;

    /// Include sub-agent streaming events in the event stream. Default: true.
    std::optional<bool> includeSubAgentStreamingEvents;

    /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
    std::optional<nlohmann::json> modelCapabilities;

    /// When true, auto-discovers MCP server configs from working directory. Default: false.
    std::optional<bool> enableConfigDiscovery;

    /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
    std::optional<std::string> gitHubToken;

    /// Directories containing instruction files for the session.
    std::optional<std::vector<std::string>> instructionDirectories;

    /// Slash commands registered for this session.
    std::vector<CommandDefinition> commands;
    /// Handler for elicitation requests from the server.
    ElicitationHandler onElicitationRequest;
    /// Handler for exit plan mode requests from the server.
    ExitPlanModeHandler onExitPlanMode;
};

// ============================================================================
// Response Format & Image Types
// ============================================================================

/** Response format for message responses. */
enum class ResponseFormat {
    Text,
    Image,
    JsonObject
};

/** Convert ResponseFormat to string for JSON serialization. */
inline std::string responseFormatToString(ResponseFormat format) {
    switch (format) {
        case ResponseFormat::Text: return "text";
        case ResponseFormat::Image: return "image";
        case ResponseFormat::JsonObject: return "json_object";
        default: return "text";
    }
}

/** Options for image generation. */
struct ImageOptions {
    std::string size;       // e.g. "1024x1024"
    std::string quality;    // "hd" or "standard"
    std::string style;      // "natural" or "vivid"
};

inline void to_json(nlohmann::json& j, const ImageOptions& o) {
    j = nlohmann::json::object();
    if (!o.size.empty()) j["size"] = o.size;
    if (!o.quality.empty()) j["quality"] = o.quality;
    if (!o.style.empty()) j["style"] = o.style;
}

inline void from_json(const nlohmann::json& j, ImageOptions& o) {
    if (j.contains("size")) j["size"].get_to(o.size);
    if (j.contains("quality")) j["quality"].get_to(o.quality);
    if (j.contains("style")) j["style"].get_to(o.style);
}

/** Image data from an assistant image response. */
struct AssistantImageData {
    std::string format;          // "png", "jpeg", "webp"
    std::string base64;          // Base64-encoded image bytes
    std::string url;             // Optional temporary URL
    std::string revisedPrompt;   // The prompt the model actually used
    int width = 0;
    int height = 0;
};

inline void to_json(nlohmann::json& j, const AssistantImageData& d) {
    j = nlohmann::json::object();
    if (!d.format.empty()) j["format"] = d.format;
    if (!d.base64.empty()) j["base64"] = d.base64;
    if (!d.url.empty()) j["url"] = d.url;
    if (!d.revisedPrompt.empty()) j["revisedPrompt"] = d.revisedPrompt;
    if (d.width > 0) j["width"] = d.width;
    if (d.height > 0) j["height"] = d.height;
}

inline void from_json(const nlohmann::json& j, AssistantImageData& d) {
    if (j.contains("format")) j["format"].get_to(d.format);
    if (j.contains("base64")) j["base64"].get_to(d.base64);
    if (j.contains("url")) j["url"].get_to(d.url);
    if (j.contains("revisedPrompt")) j["revisedPrompt"].get_to(d.revisedPrompt);
    if (j.contains("width")) j["width"].get_to(d.width);
    if (j.contains("height")) j["height"].get_to(d.height);
}

/** A content block in a mixed text+image response. */
struct ContentBlock {
    std::string type;            // "text" or "image"
    std::string text;            // Text content (when type is "text")
    AssistantImageData image;    // Image data (when type is "image")
};

inline void to_json(nlohmann::json& j, const ContentBlock& b) {
    j = {{"type", b.type}};
    if (b.type == "text" && !b.text.empty()) j["text"] = b.text;
    if (b.type == "image") j["image"] = b.image;
}

inline void from_json(const nlohmann::json& j, ContentBlock& b) {
    j.at("type").get_to(b.type);
    if (j.contains("text")) j["text"].get_to(b.text);
    if (j.contains("image")) b.image = j["image"].get<AssistantImageData>();
}

// ============================================================================
// Message Options
// ============================================================================

struct MessageOptions {
    std::string prompt;
    std::vector<Attachment> attachments;
    std::optional<std::string> mode;              // "enqueue" or "immediate"
    std::optional<ResponseFormat> responseFormat;  // Desired response format
    std::optional<ImageOptions> imageOptions;      // Options for image generation

    /// Custom HTTP headers to include in outbound model requests for this turn.
    std::optional<std::map<std::string, std::string>> requestHeaders;
};

// ============================================================================
// Ping Response
// ============================================================================

struct PingResponse {
    std::string message;
    int64_t timestamp = 0;
    std::optional<int> protocolVersion;
};

inline void from_json(const nlohmann::json& j, PingResponse& r) {
    if (j.contains("message")) j["message"].get_to(r.message);
    if (j.contains("timestamp")) j["timestamp"].get_to(r.timestamp);
    if (j.contains("protocolVersion") && !j["protocolVersion"].is_null())
        r.protocolVersion = j["protocolVersion"].get<int>();
}

// ============================================================================
// Status Responses
// ============================================================================

struct GetStatusResponse {
    std::string version;
    int protocolVersion = 0;
};

inline void from_json(const nlohmann::json& j, GetStatusResponse& r) {
    if (j.contains("version")) j["version"].get_to(r.version);
    if (j.contains("protocolVersion")) j["protocolVersion"].get_to(r.protocolVersion);
}

struct GetAuthStatusResponse {
    bool isAuthenticated = false;
    std::optional<std::string> authType;
    std::optional<std::string> host;
    std::optional<std::string> login;
    std::optional<std::string> statusMessage;
};

inline void from_json(const nlohmann::json& j, GetAuthStatusResponse& r) {
    if (j.contains("isAuthenticated")) j["isAuthenticated"].get_to(r.isAuthenticated);
    if (j.contains("authType")) r.authType = j["authType"].get<std::string>();
    if (j.contains("host")) r.host = j["host"].get<std::string>();
    if (j.contains("login")) r.login = j["login"].get<std::string>();
    if (j.contains("statusMessage")) r.statusMessage = j["statusMessage"].get<std::string>();
}

// ============================================================================
// Model Types
// ============================================================================

struct ModelSupports {
    bool vision = false;
    bool reasoningEffort = false;
};

inline void from_json(const nlohmann::json& j, ModelSupports& s) {
    if (j.contains("vision")) j["vision"].get_to(s.vision);
    if (j.contains("reasoningEffort")) j["reasoningEffort"].get_to(s.reasoningEffort);
}

struct ModelVisionLimits {
    std::vector<std::string> supportedMediaTypes;
    int maxPromptImages = 0;
    int maxPromptImageSize = 0;
};

inline void from_json(const nlohmann::json& j, ModelVisionLimits& l) {
    if (j.contains("supported_media_types")) j["supported_media_types"].get_to(l.supportedMediaTypes);
    if (j.contains("max_prompt_images")) j["max_prompt_images"].get_to(l.maxPromptImages);
    if (j.contains("max_prompt_image_size")) j["max_prompt_image_size"].get_to(l.maxPromptImageSize);
}

struct ModelLimits {
    std::optional<int> maxPromptTokens;
    int maxContextWindowTokens = 0;
    std::optional<ModelVisionLimits> vision;
};

inline void from_json(const nlohmann::json& j, ModelLimits& l) {
    if (j.contains("max_prompt_tokens") && !j["max_prompt_tokens"].is_null())
        l.maxPromptTokens = j["max_prompt_tokens"].get<int>();
    if (j.contains("max_context_window_tokens"))
        j["max_context_window_tokens"].get_to(l.maxContextWindowTokens);
    if (j.contains("vision") && !j["vision"].is_null())
        l.vision = j["vision"].get<ModelVisionLimits>();
}

struct ModelCapabilities {
    ModelSupports supports;
    ModelLimits limits;
};

inline void from_json(const nlohmann::json& j, ModelCapabilities& c) {
    if (j.contains("supports")) j["supports"].get_to(c.supports);
    if (j.contains("limits")) j["limits"].get_to(c.limits);
}

struct ModelPolicy {
    std::string state; // "enabled", "disabled", "unconfigured"
    std::string terms;
};

inline void from_json(const nlohmann::json& j, ModelPolicy& p) {
    if (j.contains("state")) j["state"].get_to(p.state);
    if (j.contains("terms")) j["terms"].get_to(p.terms);
}

/// Completion type for slash command inputs.
enum class SlashCommandInputCompletion {
    Directory
};

inline void from_json(const nlohmann::json& j, SlashCommandInputCompletion& c) {
    auto s = j.get<std::string>();
    if (s == "directory") c = SlashCommandInputCompletion::Directory;
}

/// Kind of slash command.
enum class SlashCommandKind {
    Builtin,
    Client,
    Skill
};

inline void from_json(const nlohmann::json& j, SlashCommandKind& k) {
    auto s = j.get<std::string>();
    if (s == "builtin") k = SlashCommandKind::Builtin;
    else if (s == "client") k = SlashCommandKind::Client;
    else if (s == "skill") k = SlashCommandKind::Skill;
}

/// Price category for model picker.
enum class ModelPickerPriceCategory {
    High,
    Low,
    Medium,
    VeryHigh
};

inline void from_json(const nlohmann::json& j, ModelPickerPriceCategory& c) {
    auto s = j.get<std::string>();
    if (s == "high") c = ModelPickerPriceCategory::High;
    else if (s == "low") c = ModelPickerPriceCategory::Low;
    else if (s == "medium") c = ModelPickerPriceCategory::Medium;
    else if (s == "very_high") c = ModelPickerPriceCategory::VeryHigh;
}

/// Input definition for a slash command.
struct SlashCommandInput {
    std::string hint;
    std::optional<SlashCommandInputCompletion> completion;
};

inline void from_json(const nlohmann::json& j, SlashCommandInput& i) {
    j.at("hint").get_to(i.hint);
    if (j.contains("completion") && !j["completion"].is_null())
        i.completion = j["completion"].get<SlashCommandInputCompletion>();
}

/// Information about a slash command.
struct SlashCommandInfo {
    bool allowDuringAgentExecution = false;
    std::string description;
    SlashCommandKind kind = SlashCommandKind::Builtin;
    std::string name;
    std::optional<std::vector<std::string>> aliases;
    std::optional<bool> experimental;
    std::optional<SlashCommandInput> input;
};

inline void from_json(const nlohmann::json& j, SlashCommandInfo& s) {
    j.at("allowDuringAgentExecution").get_to(s.allowDuringAgentExecution);
    j.at("description").get_to(s.description);
    if (j.contains("kind")) j["kind"].get_to(s.kind);
    j.at("name").get_to(s.name);
    if (j.contains("aliases") && !j["aliases"].is_null())
        s.aliases = j["aliases"].get<std::vector<std::string>>();
    if (j.contains("experimental") && !j["experimental"].is_null())
        s.experimental = j["experimental"].get<bool>();
    if (j.contains("input") && !j["input"].is_null())
        s.input = j["input"].get<SlashCommandInput>();
}

/// Request to invoke a command.
struct CommandsInvokeRequest {
    std::string name;
    std::optional<std::string> input;
};

inline void from_json(const nlohmann::json& j, CommandsInvokeRequest& r) {
    j.at("name").get_to(r.name);
    if (j.contains("input") && !j["input"].is_null())
        r.input = j["input"].get<std::string>();
}

/// Request to list available commands.
struct CommandsListRequest {
    std::optional<bool> includeBuiltins;
    std::optional<bool> includeClientCommands;
    std::optional<bool> includeSkills;
};

inline void from_json(const nlohmann::json& j, CommandsListRequest& r) {
    if (j.contains("includeBuiltins") && !j["includeBuiltins"].is_null())
        r.includeBuiltins = j["includeBuiltins"].get<bool>();
    if (j.contains("includeClientCommands") && !j["includeClientCommands"].is_null())
        r.includeClientCommands = j["includeClientCommands"].get<bool>();
    if (j.contains("includeSkills") && !j["includeSkills"].is_null())
        r.includeSkills = j["includeSkills"].get<bool>();
}

/// Token pricing information for model billing.
struct ModelBillingTokenPrices {
    std::optional<int> batchSize;
    std::optional<int> cachePrice;
    std::optional<int> inputPrice;
    std::optional<int> outputPrice;
};

inline void from_json(const nlohmann::json& j, ModelBillingTokenPrices& p) {
    if (j.contains("batchSize") && !j["batchSize"].is_null())
        p.batchSize = j["batchSize"].get<int>();
    if (j.contains("cachePrice") && !j["cachePrice"].is_null())
        p.cachePrice = j["cachePrice"].get<int>();
    if (j.contains("inputPrice") && !j["inputPrice"].is_null())
        p.inputPrice = j["inputPrice"].get<int>();
    if (j.contains("outputPrice") && !j["outputPrice"].is_null())
        p.outputPrice = j["outputPrice"].get<int>();
}

// Experimental
/// Diagnostics from loading skills.
struct SkillsLoadDiagnostics {
    std::vector<std::string> errors;
    std::vector<std::string> warnings;
};

inline void from_json(const nlohmann::json& j, SkillsLoadDiagnostics& d) {
    if (j.contains("errors")) j["errors"].get_to(d.errors);
    if (j.contains("warnings")) j["warnings"].get_to(d.warnings);
}

struct ModelBilling {
    double multiplier = 0.0;
    std::optional<ModelBillingTokenPrices> tokenPrices;
    std::optional<ModelPickerPriceCategory> pickerPriceCategory;
};

inline void from_json(const nlohmann::json& j, ModelBilling& b) {
    if (j.contains("multiplier")) j["multiplier"].get_to(b.multiplier);
    if (j.contains("tokenPrices") && !j["tokenPrices"].is_null())
        b.tokenPrices = j["tokenPrices"].get<ModelBillingTokenPrices>();
    if (j.contains("pickerPriceCategory") && !j["pickerPriceCategory"].is_null())
        b.pickerPriceCategory = j["pickerPriceCategory"].get<ModelPickerPriceCategory>();
}

struct ModelInfo {
    std::string id;
    std::string name;
    ModelCapabilities capabilities;
    std::optional<ModelPolicy> policy;
    std::optional<ModelBilling> billing;
    std::optional<std::vector<std::string>> supportedReasoningEfforts;
    std::optional<std::string> defaultReasoningEffort;
};

inline void from_json(const nlohmann::json& j, ModelInfo& m) {
    j.at("id").get_to(m.id);
    j.at("name").get_to(m.name);
    if (j.contains("capabilities")) j["capabilities"].get_to(m.capabilities);
    if (j.contains("policy") && !j["policy"].is_null()) m.policy = j["policy"].get<ModelPolicy>();
    if (j.contains("billing") && !j["billing"].is_null()) m.billing = j["billing"].get<ModelBilling>();
    if (j.contains("supportedReasoningEfforts"))
        m.supportedReasoningEfforts = j["supportedReasoningEfforts"].get<std::vector<std::string>>();
    if (j.contains("defaultReasoningEffort"))
        m.defaultReasoningEffort = j["defaultReasoningEffort"].get<std::string>();
}

// ============================================================================
// Session Metadata
// ============================================================================

struct SessionMetadata {
    std::string sessionId;
    std::string startTime;
    std::string modifiedTime;
    std::optional<std::string> summary;
    bool isRemote = false;
};

inline void from_json(const nlohmann::json& j, SessionMetadata& m) {
    j.at("sessionId").get_to(m.sessionId);
    j.at("startTime").get_to(m.startTime);
    j.at("modifiedTime").get_to(m.modifiedTime);
    if (j.contains("summary") && !j["summary"].is_null())
        m.summary = j["summary"].get<std::string>();
    if (j.contains("isRemote")) j["isRemote"].get_to(m.isRemote);
}

// ============================================================================
// Session Lifecycle Events
// ============================================================================

struct SessionLifecycleEventMetadata {
    std::string startTime;
    std::string modifiedTime;
    std::optional<std::string> summary;
};

inline void from_json(const nlohmann::json& j, SessionLifecycleEventMetadata& m) {
    j.at("startTime").get_to(m.startTime);
    j.at("modifiedTime").get_to(m.modifiedTime);
    if (j.contains("summary") && !j["summary"].is_null())
        m.summary = j["summary"].get<std::string>();
}

struct SessionLifecycleEvent {
    std::string type; // "session.created", "session.deleted", "session.updated", etc.
    std::string sessionId;
    std::optional<SessionLifecycleEventMetadata> metadata;
};

inline void from_json(const nlohmann::json& j, SessionLifecycleEvent& e) {
    j.at("type").get_to(e.type);
    j.at("sessionId").get_to(e.sessionId);
    if (j.contains("metadata") && !j["metadata"].is_null())
        e.metadata = j["metadata"].get<SessionLifecycleEventMetadata>();
}

/// Handler for session lifecycle events.
using SessionLifecycleHandler = std::function<void(const SessionLifecycleEvent&)>;

// ============================================================================
// Session Filesystem Types
// ============================================================================

/// Configuration for a custom session filesystem provider.
struct SessionFsConfig {
    std::string initialCwd;
    std::string sessionStatePath;
    std::string conventions;
};

inline void to_json(nlohmann::json& j, const SessionFsConfig& c) {
    j = {{"initialCwd", c.initialCwd}, {"sessionStatePath", c.sessionStatePath}, {"conventions", c.conventions}};
}

inline void from_json(const nlohmann::json& j, SessionFsConfig& c) {
    j.at("initialCwd").get_to(c.initialCwd);
    j.at("sessionStatePath").get_to(c.sessionStatePath);
    j.at("conventions").get_to(c.conventions);
}

/// File metadata returned by session filesystem operations.
struct SessionFsFileInfo {
    std::string name;
    int64_t size = 0;
    bool isDirectory = false;
    bool isFile = false;
    std::optional<std::string> createdAt;
    std::optional<std::string> modifiedAt;
};

/// Abstract base class for session filesystem providers.
/// Implementors provide file operations scoped to a session.
class SessionFsProvider {
public:
    virtual ~SessionFsProvider() = default;
    virtual std::string readFile(const std::string& sessionId, const std::string& path) = 0;
    virtual void writeFile(const std::string& sessionId, const std::string& path, const std::string& content) = 0;
    virtual void appendFile(const std::string& sessionId, const std::string& path, const std::string& content) = 0;
    virtual bool exists(const std::string& sessionId, const std::string& path) = 0;
    virtual SessionFsFileInfo stat(const std::string& sessionId, const std::string& path) = 0;
    virtual void mkdir(const std::string& sessionId, const std::string& path, bool recursive) = 0;
    virtual std::vector<std::string> readdir(const std::string& sessionId, const std::string& path) = 0;
    virtual std::vector<SessionFsFileInfo> readdirWithTypes(const std::string& sessionId, const std::string& path) = 0;
    virtual void rm(const std::string& sessionId, const std::string& path, bool recursive) = 0;
    virtual void rename(const std::string& sessionId, const std::string& oldPath, const std::string& newPath) = 0;
};

// ============================================================================
// Client Options
// ============================================================================

struct CopilotClientOptions {
    /// Path to the CLI executable (default: "copilot").
    std::string cliPath = "copilot";

    /// Extra arguments to pass to the CLI executable (inserted before SDK-managed args).
    std::vector<std::string> cliArgs;

    /// Working directory for the CLI process.
    std::string cwd;

    /// Port for TCP transport (default: 0 = random port).
    int port = 0;

    /// Use stdio transport instead of TCP (default: true).
    bool useStdio = true;

    /// URL of an existing Copilot CLI server. Mutually exclusive with cliPath/useStdio.
    std::optional<std::string> cliUrl;

    /// Log level for the CLI server.
    std::string logLevel = "info";

    /// Auto-start the CLI server on first use (default: true).
    bool autoStart = true;

    /// Auto-restart the CLI server if it crashes (default: true).
    bool autoRestart = true;

    /// GitHub token for authentication.
    std::optional<std::string> githubToken;

    /// Whether to use the logged-in user for authentication (default: true, false when githubToken is set).
    std::optional<bool> useLoggedInUser;

    /// Server-wide idle timeout for sessions in seconds.
    std::optional<int> sessionIdleTimeoutSeconds;

    /// Configuration for a custom session filesystem provider.
    std::optional<SessionFsConfig> sessionFs;

    /// Override the Copilot home directory.
    std::optional<std::string> copilotHome;

    /// Token for TCP connection authentication.
    std::optional<std::string> tcpConnectionToken;

    /// Provider for distributed tracing context (W3C Trace Context format).
    TraceContextProvider onGetTraceContext;
};

} // namespace copilot
