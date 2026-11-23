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

    /// Enable remote session support (Mission Control); adds the --remote CLI flag.
    bool remote = false;

    /// Identity of the embedding application, declared via `connect` on start.
    ClientInfo clientInfo;

    /// Client behavior profile: `ClientMode.codingAgent` (default) or
    /// `ClientMode.empty`, which requires `availableTools` on every session and
    /// applies conservative defaults beneath the caller's options.
    ClientMode mode = ClientMode.codingAgent;

    /// Proxy resolution and proxy-authentication callbacks, registered with
    /// `network.setProvider` after connecting.
    NetworkOptions network;
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
    subagentStart       = "subagentStart",
    subagentStop        = "subagentStop",
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
// v2.6.0 upstream-sync types (parity with @github/copilot-sdk)
// ---------------------------------------------------------------------------

/// Identity of the application embedding the SDK, declared on the `connect`
/// handshake so the runtime attributes its telemetry to a consistent surface.
/// Empty fields are omitted; when every field is empty `connect` is skipped.
struct ClientInfo
{
    /// Application using the SDK, e.g. "acme-portal" (wire: editorName).
    string applicationName;

    /// Version of the application (wire: editorVersion).
    string applicationVersion;

    /// Specific integration within the application (wire: extensionName).
    string integrationName;

    /// Version of the integration (wire: extensionVersion).
    string integrationVersion;

    /// Wire shape for `connect`; JSON null when every field is empty.
    JSONValue toWire() const @safe
    {
        auto obj = JSONValue(string[string].init);
        bool any = false;
        if (applicationName.length > 0) { obj["editorName"] = applicationName; any = true; }
        if (applicationVersion.length > 0) { obj["editorVersion"] = applicationVersion; any = true; }
        if (integrationName.length > 0) { obj["extensionName"] = integrationName; any = true; }
        if (integrationVersion.length > 0) { obj["extensionVersion"] = integrationVersion; any = true; }
        return any ? obj : JSONValue(null);
    }
}

/// Auto routing tiers (experimental); see `SessionConfig.autoTier` and
/// `CopilotSession.setAutoTier`.
enum AutoTier : string
{
    efficiency   = "efficiency",
    balance      = "balance",
    intelligence = "intelligence",
    fast         = "fast",
}

/// Variants of the ask-user tool; see `SessionConfig.askUserVariant`.
enum AskUserVariant : string
{
    legacy      = "legacy",
    elicitation = "elicitation",
}

/// Scopes for trusting the working directory; see `SessionConfig.trustWorkingDirectory`.
enum TrustWorkingDirectory : string
{
    session = "session",
    user    = "user",
}

/// Products that can serve a BYOK provider's model; see `ProviderConfig.modelProvider`.
enum ModelProvider : string
{
    openai       = "openai",
    anthropic    = "anthropic",
    azureOpenai  = "azure_openai",
    ollama       = "ollama",
    lmStudio     = "lm_studio",
    foundryLocal = "foundry_local",
    llamaCpp     = "llama_cpp",
}

/// Origin of a sent message (`MessageOptions.source`): "user", "system",
/// or "agent-<id>".
struct MessageSource
{
    enum string user = "user";
    enum string system = "system";

    /// Source for a message sent on behalf of the agent with the given ID.
    static string agent(string agentId) @safe pure nothrow
    {
        return "agent-" ~ agentId;
    }
}

/// Sandbox configuration for a session (`sandboxConfig`).
struct SandboxConfig
{
    /// Whether sandboxing is enabled for the session.
    bool enabled = false;

    /// User-managed policy fragment merged into the auto-discovered base policy.
    JSONValue userPolicy;

    /// Auto-add the working directory to the read/write paths (runtime default: true).
    Nullable!bool addCurrentWorkingDirectory;

    /// Confine launched MCP servers (runtime default: true; false opts out).
    Nullable!bool sandboxMcpServers;

    /// Confine launched language servers (runtime default: true; false opts out).
    Nullable!bool sandboxLspServers;

    /// Confine launched extension processes (runtime default: true; false opts out).
    Nullable!bool sandboxExtensions;

    /// Opt in to letting the agent request that an individual command run
    /// outside the sandbox, approved or denied through the normal permission flow.
    Nullable!bool allowBypass;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["enabled"] = enabled;
        if (userPolicy.type == JSONType.object)
            obj["userPolicy"] = userPolicy;
        if (!addCurrentWorkingDirectory.isNull)
            obj["addCurrentWorkingDirectory"] = addCurrentWorkingDirectory.get;
        if (!sandboxMcpServers.isNull)
            obj["sandboxMcpServers"] = sandboxMcpServers.get;
        if (!sandboxLspServers.isNull)
            obj["sandboxLspServers"] = sandboxLspServers.get;
        if (!sandboxExtensions.isNull)
            obj["sandboxExtensions"] = sandboxExtensions.get;
        if (!allowBypass.isNull)
            obj["allowBypass"] = allowBypass.get;
        return obj;
    }
}

/// Image generation configuration (`imageGeneration`, experimental).
struct ImageGenerationConfig
{
    /// Opt in to image generation through an authorized Copilot image model.
    Nullable!bool enabled;

    /// Request a visible "Made with AI" label on generated images.
    Nullable!bool watermark;

    /// Save generated images under the session-state directory (runtime default: true).
    Nullable!bool saveToDisk;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!enabled.isNull)
            obj["enabled"] = enabled.get;
        if (!watermark.isNull)
            obj["watermark"] = watermark.get;
        if (!saveToDisk.isNull)
            obj["saveToDisk"] = saveToDisk.get;
        return obj;
    }
}

/// Autopilot continuation options (`autopilot`).
struct AutopilotConfig
{
    /// Whether autopilot may continue without user input.
    Nullable!bool continuationEnabled;

    /// Maximum number of automatic continuations.
    Nullable!int maxContinues;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!continuationEnabled.isNull)
            obj["continuationEnabled"] = continuationEnabled.get;
        if (!maxContinues.isNull)
            obj["maxContinues"] = maxContinues.get;
        return obj;
    }
}

/// Token limits for a model capabilities override (`modelCapabilities.limits`).
/// Wire keys are snake_case.
struct ModelLimitsOverride
{
    /// Maximum prompt tokens (wire: max_prompt_tokens).
    Nullable!long maxPromptTokens;

    /// Maximum context window tokens (wire: max_context_window_tokens).
    Nullable!long maxContextWindowTokens;

    /// Maximum output tokens (wire: max_output_tokens).
    Nullable!long maxOutputTokens;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!maxPromptTokens.isNull)
            obj["max_prompt_tokens"] = maxPromptTokens.get;
        if (!maxContextWindowTokens.isNull)
            obj["max_context_window_tokens"] = maxContextWindowTokens.get;
        if (!maxOutputTokens.isNull)
            obj["max_output_tokens"] = maxOutputTokens.get;
        return obj;
    }

    /// A `modelCapabilities` override object carrying these limits, suitable
    /// for `SessionConfig.modelCapabilities`.
    JSONValue toModelCapabilities() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["limits"] = toJson();
        return obj;
    }
}

/// Arguments passed to a `GitHubTokenProvider`.
struct GitHubTokenProviderArgs
{
    /// Effective GitHub host the token is for.
    string host;

    /// Session receiving the token (null before a cloud session has an ID).
    Nullable!string sessionId;

    /// Why the runtime needs a credential: "initial" or "refresh".
    string reason;
}

/// Result of a `GitHubTokenProvider`: a token or a cancellation.
struct GitHubTokenProviderResult
{
    /// "token" or "cancelled".
    string kind = "cancelled";

    /// GitHub access token (kind "token").
    string accessToken;

    /// OAuth token type; bearer when null.
    Nullable!string tokenType;

    /// Remaining token lifetime in seconds (kind "token").
    long expiresIn;

    /// Build a token result.
    static GitHubTokenProviderResult token(string accessToken, long expiresIn,
        string tokenType = null) @safe
    {
        GitHubTokenProviderResult r;
        r.kind = "token";
        r.accessToken = accessToken;
        r.expiresIn = expiresIn;
        if (tokenType !is null)
            r.tokenType = tokenType;
        return r;
    }

    /// Build a cancellation result.
    static GitHubTokenProviderResult cancelled() @safe
    {
        return GitHubTokenProviderResult.init;
    }

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["kind"] = kind;
        if (kind == "token")
        {
            obj["accessToken"] = accessToken;
            obj["expiresIn"] = expiresIn;
            if (!tokenType.isNull)
                obj["tokenType"] = tokenType.get;
        }
        return obj;
    }
}

/// Supplies GitHub tokens on demand for a session (`gitHubToken.getToken`).
/// Mutually exclusive with a per-session `gitHubToken`.
alias GitHubTokenProvider = GitHubTokenProviderResult delegate(GitHubTokenProviderArgs args);

/// Input for the hook fired before a sub-agent's first turn (`subagentStart`).
/// Session metadata belongs to the parent session.
struct SubagentStartHookInput
{
    /// Runtime session ID associated with the hook (the parent session).
    string sessionId;

    /// Time the hook fired (Unix epoch milliseconds).
    long timestamp;

    /// Working directory of the session.
    string workingDirectory;

    /// Path to the sub-agent's transcript.
    string transcriptPath;

    /// Name of the sub-agent.
    string agentName;

    /// Display name of the sub-agent, when known.
    Nullable!string agentDisplayName;

    /// Description of the sub-agent, when known.
    Nullable!string agentDescription;

    static SubagentStartHookInput fromJson(JSONValue v) @safe
    {
        SubagentStartHookInput input;
        input.sessionId = jsonStr(v, "sessionId");
        input.timestamp = jsonLong(v, "timestamp");
        input.workingDirectory = jsonStr(v, "workingDirectory");
        input.transcriptPath = jsonStr(v, "transcriptPath");
        input.agentName = jsonStr(v, "agentName");
        input.agentDisplayName = jsonOptStr(v, "agentDisplayName");
        input.agentDescription = jsonOptStr(v, "agentDescription");
        return input;
    }
}

/// Output for the `subagentStart` hook; `additionalContext` is prepended to
/// the sub-agent's initial prompt.
struct SubagentStartHookOutput
{
    Nullable!string additionalContext;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!additionalContext.isNull)
            obj["additionalContext"] = additionalContext.get;
        return obj;
    }
}

/// Input for the hook fired after a sub-agent completes a turn (`subagentStop`).
struct SubagentStopHookInput
{
    /// Runtime session ID associated with the hook (the parent session).
    string sessionId;

    /// Time the hook fired (Unix epoch milliseconds).
    long timestamp;

    /// Working directory of the session.
    string workingDirectory;

    /// Path to the sub-agent's transcript.
    string transcriptPath;

    /// Name of the sub-agent.
    string agentName;

    /// Display name of the sub-agent, when known.
    Nullable!string agentDisplayName;

    /// Description of the sub-agent, when known.
    Nullable!string agentDescription;

    /// Sub-agent instance ID, when available.
    Nullable!string agentId;

    /// Sub-agent type.
    string agentType;

    /// Why the sub-agent stopped ("end_turn").
    string stopReason;

    /// The sub-agent's last assistant message, before any hook rewrite.
    string response;

    static SubagentStopHookInput fromJson(JSONValue v) @safe
    {
        SubagentStopHookInput input;
        input.sessionId = jsonStr(v, "sessionId");
        input.timestamp = jsonLong(v, "timestamp");
        input.workingDirectory = jsonStr(v, "workingDirectory");
        input.transcriptPath = jsonStr(v, "transcriptPath");
        input.agentName = jsonStr(v, "agentName");
        input.agentDisplayName = jsonOptStr(v, "agentDisplayName");
        input.agentDescription = jsonOptStr(v, "agentDescription");
        input.agentId = jsonOptStr(v, "agentId");
        input.agentType = jsonStr(v, "agentType");
        input.stopReason = jsonStr(v, "stopReason");
        input.response = jsonStr(v, "response");
        return input;
    }
}

/// Output for the `subagentStop` hook: `decision` "block" with a non-empty
/// `reason` continues the sub-agent; otherwise `modifiedResponse` replaces
/// the response reported to the parent.
struct SubagentStopHookOutput
{
    /// "block" or "allow".
    Nullable!string decision;

    /// Follow-up instruction when blocking.
    Nullable!string reason;

    /// Replacement for the response reported to the parent.
    Nullable!string modifiedResponse;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        if (!decision.isNull)
            obj["decision"] = decision.get;
        if (!reason.isNull)
            obj["reason"] = reason.get;
        if (!modifiedResponse.isNull)
            obj["modifiedResponse"] = modifiedResponse.get;
        return obj;
    }
}

/// Handler for the `subagentStart` hook.
alias SubagentStartHandler = SubagentStartHookOutput delegate(SubagentStartHookInput input);

/// Handler for the `subagentStop` hook.
alias SubagentStopHandler = SubagentStopHookOutput delegate(SubagentStopHookInput input);

/// Read a string member of a JSON object (null when absent or not a string).
private string jsonStr(const ref JSONValue v, string key) @safe
{
    if (v.type != JSONType.object) return null;
    auto p = key in v;
    return (p !is null && (*p).type == JSONType.string) ? (*p).str : null;
}

/// Read an optional string member of a JSON object.
private Nullable!string jsonOptStr(const ref JSONValue v, string key) @safe
{
    if (v.type != JSONType.object) return Nullable!string.init;
    auto p = key in v;
    if (p !is null && (*p).type == JSONType.string)
        return Nullable!string((*p).str);
    return Nullable!string.init;
}

/// Read an integral member of a JSON object (0 when absent).
private long jsonLong(const ref JSONValue v, string key) @safe
{
    if (v.type != JSONType.object) return 0;
    auto p = key in v;
    if (p is null) return 0;
    if ((*p).type == JSONType.integer) return (*p).integer;
    if ((*p).type == JSONType.uinteger) return cast(long) (*p).uinteger;
    if ((*p).type == JSONType.float_) return cast(long) (*p).floating;
    return 0;
}

/// Serialize a string list as a JSON array.
private JSONValue stringListJson(const string[] items) @safe
{
    JSONValue[] arr;
    foreach (s; items) arr ~= JSONValue(s);
    return JSONValue(arr);
}

/// Build the `capi` object (auto routing tier, WebSocket responses, and
/// Copilot model availability); JSON null when nothing is set.
private JSONValue capiJson(const Nullable!string autoTier, bool webSocketResponses,
    const Nullable!bool enabled = Nullable!bool.init) @safe
{
    if (autoTier.isNull && !webSocketResponses && enabled.isNull)
        return JSONValue(null);
    auto obj = JSONValue(string[string].init);
    if (!enabled.isNull)
        obj["enabled"] = enabled.get;
    if (!autoTier.isNull)
        obj["autoTier"] = autoTier.get;
    if (webSocketResponses)
        obj["enableWebSocketResponses"] = true;
    return obj;
}

/// Serialize the options shared by `session.create` and `session.resume`.
private void putSharedSessionOptions(T)(ref JSONValue obj, ref const T cfg) @safe
{
    if (!cfg.askUserVariant.isNull)
        obj["askUserVariant"] = cfg.askUserVariant.get;
    if (!cfg.sandboxConfig.isNull)
        obj["sandboxConfig"] = cfg.sandboxConfig.get.toJson();
    if (!cfg.featureFlags.isNull)
    {
        auto flags = JSONValue(string[string].init);
        foreach (key, enabled; cfg.featureFlags.get)
            flags[key] = enabled;
        obj["featureFlags"] = flags;
    }
    if (cfg.allowedModels.length > 0)
        obj["allowedModels"] = stringListJson(cfg.allowedModels);
    if (!cfg.authClientIdMetadataUrl.isNull)
        obj["authClientIdMetadataUrl"] = cfg.authClientIdMetadataUrl.get;
    if (cfg.agentDirectories.length > 0)
        obj["agentDirectories"] = stringListJson(cfg.agentDirectories);
    if (cfg.hookDirectories.length > 0)
        obj["hookDirectories"] = stringListJson(cfg.hookDirectories);
    if (cfg.interactiveUser)
        obj["interactiveUser"] = true;
    if (cfg.excludedManagedSettings.length > 0)
        obj["excludedManagedSettings"] = stringListJson(cfg.excludedManagedSettings);
    if (cfg.enforceManagedModelDefaults)
        obj["enforceManagedModelDefaults"] = true;
    if (!cfg.imageGeneration.isNull)
        obj["imageGeneration"] = cfg.imageGeneration.get.toJson();
    if (!cfg.autopilot.isNull)
        obj["autopilot"] = cfg.autopilot.get.toJson();
    if (cfg.onSubagentStart !is null || cfg.onSubagentStop !is null)
        obj["hooks"] = true;

    // v3.0.0 / SDK protocol 4 options.
    if (!cfg.availableTools.isNull)
        obj["availableTools"] = stringListJson(cfg.availableTools.get);
    if (cfg.providers.length > 0)
    {
        JSONValue[] arr;
        foreach (ref p; cfg.providers) arr ~= p.toJson();
        obj["providers"] = JSONValue(arr);
    }
    if (cfg.models.length > 0)
    {
        JSONValue[] arr;
        foreach (ref m; cfg.models) arr ~= m.toJson();
        obj["models"] = JSONValue(arr);
    }
    if (!cfg.defaultProvider.isNull)
        obj["defaultProvider"] = cfg.defaultProvider.get;
    if (cfg.onShellAuthenticationRequest !is null)
        obj["requestShellAuthentication"] = true;
    if (cfg.onStoreMemory !is null)
        obj["hasMemoryStore"] = true;
}

// ---------------------------------------------------------------------------
// v3.0.0 / SDK protocol 4 types (parity with the Microsoft Copilot SDK 2.0)
// ---------------------------------------------------------------------------

/// Client behavior profile (`CopilotClientOptions.mode`).
enum ClientMode : string
{
    /// Default: the coding-agent behavior.
    codingAgent = "coding-agent",
    /// Requires `availableTools` on every session and applies conservative
    /// defaults (no telemetry, in-memory storage, no repository hooks).
    empty = "empty",
}

/// Parse a client mode name; "copilot-cli" is accepted as an alias of
/// "coding-agent".
ClientMode parseClientMode(string name) @safe
{
    switch (name)
    {
        case "", "coding-agent", "copilot-cli":
            return ClientMode.codingAgent;
        case "empty":
            return ClientMode.empty;
        default:
            throw new Exception("unknown client mode: " ~ name);
    }
}

/// A named BYOK provider connection (`inference.providers`).
struct NamedProviderConfig
{
    /// Stable identifier referenced by `ProviderModelConfig.provider`.
    string name;
    /// Provider type, e.g. "openai", "azure", or "anthropic".
    Nullable!string type;
    string baseUrl;
    Nullable!string apiKey;
    /// Command run before each request to obtain a fresh API key.
    Nullable!string apiKeyCommand;
    Nullable!string bearerToken;
    /// "completions" or "responses".
    Nullable!string wireApi;
    /// "http" or "websockets".
    Nullable!string transport;
    /// Product serving the models (see `ModelProvider`).
    Nullable!string modelProvider;
    /// Azure API version (wire: azure.apiVersion).
    Nullable!string azureApiVersion;
    string[string] headers;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["name"] = name;
        putConnectionFields(obj, type, baseUrl.length > 0 ? Nullable!string(baseUrl) : Nullable!string.init,
            apiKey, apiKeyCommand, bearerToken, wireApi, transport, modelProvider,
            azureApiVersion, headers);
        return obj;
    }
}

/// A BYOK model served by a named provider (`inference.models`). Select it
/// with the model id "provider/id".
struct ProviderModelConfig
{
    /// Name of the `NamedProviderConfig` serving this model.
    string provider;
    /// Provider-local model id.
    string id;
    Nullable!string modelId;
    Nullable!string wireModel;
    Nullable!string name;
    Nullable!long maxPromptTokens;
    Nullable!long maxContextWindowTokens;
    Nullable!long maxOutputTokens;
    /// Capability overrides (see `ModelLimitsOverride.toModelCapabilities`).
    JSONValue capabilities;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["provider"] = provider;
        obj["id"] = id;
        if (!modelId.isNull) obj["modelId"] = modelId.get;
        if (!wireModel.isNull) obj["wireModel"] = wireModel.get;
        if (!name.isNull) obj["name"] = name.get;
        if (!maxPromptTokens.isNull) obj["maxPromptTokens"] = maxPromptTokens.get;
        if (!maxContextWindowTokens.isNull) obj["maxContextWindowTokens"] = maxContextWindowTokens.get;
        if (!maxOutputTokens.isNull) obj["maxOutputTokens"] = maxOutputTokens.get;
        if (capabilities.type == JSONType.object) obj["capabilities"] = capabilities;
        return obj;
    }
}

/// Write BYOK connection fields shared by `ProviderConfig` and `NamedProviderConfig`.
private void putConnectionFields(ref JSONValue obj, const Nullable!string type,
    const Nullable!string baseUrl, const Nullable!string apiKey,
    const Nullable!string apiKeyCommand, const Nullable!string bearerToken,
    const Nullable!string wireApi, const Nullable!string transport,
    const Nullable!string modelProvider, const Nullable!string azureApiVersion,
    const string[string] headers) @safe
{
    if (!type.isNull) obj["type"] = type.get;
    if (!baseUrl.isNull) obj["baseUrl"] = baseUrl.get;
    if (!apiKey.isNull) obj["apiKey"] = apiKey.get;
    if (!apiKeyCommand.isNull) obj["apiKeyCommand"] = apiKeyCommand.get;
    if (!bearerToken.isNull) obj["bearerToken"] = bearerToken.get;
    if (!wireApi.isNull) obj["wireApi"] = wireApi.get;
    if (!transport.isNull) obj["transport"] = transport.get;
    if (!modelProvider.isNull) obj["modelProvider"] = modelProvider.get;
    if (!azureApiVersion.isNull)
    {
        auto azure = JSONValue(string[string].init);
        azure["apiVersion"] = azureApiVersion.get;
        obj["azure"] = azure;
    }
    if (headers.length > 0)
    {
        auto hdr = JSONValue(string[string].init);
        foreach (key, val; headers)
            hdr[key] = val;
        obj["headers"] = hdr;
    }
}

/// A password or other answer a shell command asks for
/// (`shellAuthentication.request`). The answer is never shown to the model.
struct ShellAuthenticationRequest
{
    string sessionId;
    /// Identifies the request; a cancellation carries the same id.
    string requestId;
    /// The program that asked: "sudo", "ssh", or "git".
    string program;
    string prompt;
    /// "secret", "text", or "confirm".
    string inputKind;
    string command;
    string cwd;

    static ShellAuthenticationRequest fromJson(JSONValue v) @safe
    {
        ShellAuthenticationRequest req;
        req.sessionId = jsonStr(v, "sessionId");
        req.requestId = jsonStr(v, "requestId");
        req.program = jsonStr(v, "program");
        req.prompt = jsonStr(v, "prompt");
        req.inputKind = jsonStr(v, "inputKind");
        req.command = jsonStr(v, "command");
        req.cwd = jsonStr(v, "cwd");
        return req;
    }
}

/// Answer to a `ShellAuthenticationRequest`.
struct ShellAuthenticationResponse
{
    /// "accept" or "cancel".
    string action = "cancel";
    /// The answer when accepting.
    string value;

    /// Accept the request with an answer.
    static ShellAuthenticationResponse accept(string value) @safe pure nothrow
    {
        return ShellAuthenticationResponse("accept", value);
    }

    /// Decline the request.
    static ShellAuthenticationResponse cancel() @safe pure nothrow
    {
        return ShellAuthenticationResponse.init;
    }

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["action"] = action;
        if (action == "accept")
            obj["value"] = value;
        return obj;
    }
}

/// Answers shell authentication requests for a session.
alias ShellAuthenticationHandler = ShellAuthenticationResponse delegate(ShellAuthenticationRequest request);

/// A memory the agent asked to store (`memoryStore.store`).
struct StoredMemory
{
    string sessionId;
    string subject;
    string fact;
    string citations;
    string reason;

    static StoredMemory fromJson(JSONValue v) @safe
    {
        StoredMemory m;
        m.sessionId = jsonStr(v, "sessionId");
        m.subject = jsonStr(v, "subject");
        m.fact = jsonStr(v, "fact");
        m.citations = jsonStr(v, "citations");
        m.reason = jsonStr(v, "reason");
        return m;
    }
}

/// Receives memories to store for a session.
alias StoreMemoryHandler = void delegate(StoredMemory memory);

/// A proxy Basic authentication challenge (`network.authenticateBasic`).
struct NetworkBasicAuthRequest
{
    string proxy;
    Nullable!string realm;
    bool previousRejected;
}

/// Credentials for a proxy Basic challenge. Never log them.
struct NetworkBasicCredentials
{
    string username;
    string password;
}

/// Network callbacks registered with `network.setProvider` after connecting.
struct NetworkOptions
{
    /// "connection" or "runtime"; null uses the runtime default.
    Nullable!string scope_;
    /// Returns a PAC-style proxy list for a URL, or null to use default routing.
    Nullable!string delegate(string url) resolveProxy;
    /// Returns a base64 SPNEGO token for a service principal, or null.
    Nullable!string delegate(string spn) authenticateNegotiate;
    /// Returns proxy credentials, or null when none are available.
    Nullable!NetworkBasicCredentials delegate(NetworkBasicAuthRequest request) authenticateBasic;

    /// Whether any callback is configured.
    bool hasCallbacks() const @safe pure nothrow
    {
        return resolveProxy !is null || authenticateNegotiate !is null || authenticateBasic !is null;
    }
}

/// Structured Policy Helper failure reported in RPC error data
/// (`error.data.policyHelperFailure`). The text is untrusted: render it as
/// plain text.
struct PolicyHelperFailure
{
    string category;
    string failureMode;
    string message;
    bool previousPolicyRetained;
    string reason;
    Nullable!string authority;
    Nullable!string code;
    Nullable!string diagnostic;
    JSONValue details;
    JSONValue helpLink;

    static Nullable!PolicyHelperFailure fromJson(JSONValue v) @safe
    {
        if (v.type != JSONType.object) return Nullable!PolicyHelperFailure.init;
        PolicyHelperFailure f;
        f.category = jsonStr(v, "category");
        f.failureMode = jsonStr(v, "failureMode");
        f.message = jsonStr(v, "message");
        auto pRetained = "previousPolicyRetained" in v;
        f.previousPolicyRetained = pRetained !is null && (*pRetained).type == JSONType.true_;
        f.reason = jsonStr(v, "reason");
        f.authority = jsonOptStr(v, "authority");
        f.code = jsonOptStr(v, "code");
        f.diagnostic = jsonOptStr(v, "diagnostic");
        auto pDetails = "details" in v;
        if (pDetails !is null) f.details = *pDetails;
        auto pHelp = "helpLink" in v;
        if (pHelp !is null) f.helpLink = *pHelp;
        return Nullable!PolicyHelperFailure(f);
    }
}

/// Known refusal reason for `PermissionModeResult.reason`.
enum string permissionModeRefusalManagedPolicy = "managed_policy";

/// Result of `CopilotSession.setPermissionMode` (`session.permissions.setMode`).
struct PermissionModeResult
{
    bool success;
    /// Authoritative permission mode after the call.
    string mode;
    /// Why the mode was refused, e.g. `permissionModeRefusalManagedPolicy`.
    Nullable!string reason;

    static PermissionModeResult fromJson(JSONValue v) @safe
    {
        PermissionModeResult r;
        if (v.type != JSONType.object) return r;
        auto pSuccess = "success" in v;
        r.success = pSuccess !is null && (*pSuccess).type == JSONType.true_;
        r.mode = jsonStr(v, "mode");
        r.reason = jsonOptStr(v, "reason");
        return r;
    }
}

/// Data of a `tool.shell_output` event: one chunk of live shell output.
struct ShellOutputData
{
    string toolCallId;
    string text;
    long sequence;
    /// "stdout", "stderr", or null when omitted.
    Nullable!string stream;

    static ShellOutputData fromJson(JSONValue v) @safe
    {
        ShellOutputData d;
        d.toolCallId = jsonStr(v, "toolCallId");
        d.text = jsonStr(v, "text");
        d.sequence = jsonLong(v, "sequence");
        d.stream = jsonOptStr(v, "stream");
        return d;
    }

    /// Decode a `tool.shell_output` event (its `data` member).
    static ShellOutputData fromEvent(SessionEvent ev) @safe
    {
        if (ev.payload.type == JSONType.object)
        {
            auto pData = "data" in ev.payload;
            if (pData !is null && (*pData).type == JSONType.object)
                return fromJson(*pData);
        }
        return fromJson(ev.payload);
    }
}

// ---------------------------------------------------------------------------
// Session configuration
// ---------------------------------------------------------------------------

/// Legacy single BYOK provider for the LLM backend. On the protocol 4 wire it
/// becomes the named "default" provider (`inference.providers`) with Copilot
/// models disabled; model-specific fields become an `inference.models` entry
/// for the session model.
struct ProviderConfig
{
    /// Model name sent to the provider API (wire: wireModel).
    Nullable!string model;
    Nullable!string baseUrl;

    /// Product serving the model (see `ModelProvider`), e.g. "ollama".
    Nullable!string modelProvider;

    /// Command whose stdout is used as the API key; run before each request.
    Nullable!string apiKeyCommand;

    /// Provider type, e.g. "openai", "azure", or "anthropic".
    Nullable!string type;

    /// Static API key.
    Nullable!string apiKey;

    /// Bearer token (takes precedence over `apiKey`).
    Nullable!string bearerToken;

    /// "completions" or "responses".
    Nullable!string wireApi;

    /// "http" or "websockets".
    Nullable!string transport;

    /// Azure API version (wire: azure.apiVersion).
    Nullable!string azureApiVersion;

    /// Custom HTTP headers for every provider request.
    string[string] headers;

    /// Well-known base model id used for capability lookup.
    Nullable!string modelId;

    /// Token limits for the model.
    Nullable!long maxPromptTokens;
    Nullable!long maxContextWindowTokens;
    Nullable!long maxOutputTokens;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        putConnectionFields(obj, type, baseUrl, apiKey, apiKeyCommand, bearerToken,
            wireApi, transport, modelProvider, azureApiVersion, headers);
        if (!modelId.isNull) obj["modelId"] = modelId.get;
        if (!model.isNull) obj["wireModel"] = model.get;
        if (!maxPromptTokens.isNull) obj["maxPromptTokens"] = maxPromptTokens.get;
        if (!maxContextWindowTokens.isNull) obj["maxContextWindowTokens"] = maxContextWindowTokens.get;
        if (!maxOutputTokens.isNull) obj["maxOutputTokens"] = maxOutputTokens.get;
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

    // --- v2.6.0 upstream-sync options (parity with @github/copilot-sdk) ---

    /// Auto routing tier (experimental; see `AutoTier`). Sent under `capi`
    /// together with `enableWebSocketResponses`.
    Nullable!string autoTier;

    /// Ask-user tool variant ("legacy" or "elicitation"; see `AskUserVariant`).
    Nullable!string askUserVariant;

    /// Supplies GitHub tokens on demand; mutually exclusive with `gitHubToken`.
    GitHubTokenProvider gitHubTokenProvider;

    /// Sandbox configuration.
    Nullable!SandboxConfig sandboxConfig;

    /// Runtime feature-flag overrides; sent whenever set, even if empty.
    Nullable!(bool[string]) featureFlags;

    /// Restrict the models this session may use.
    string[] allowedModels;

    /// OAuth client ID metadata document URL used for MCP authorization.
    Nullable!string authClientIdMetadataUrl;

    /// Human-readable session name.
    Nullable!string name;

    /// Additional directories to load custom agents from.
    string[] agentDirectories;

    /// Additional directories to load hooks from.
    string[] hookDirectories;

    /// Whether an interactive user is present.
    bool interactiveUser = false;

    /// Managed settings to exclude for this session.
    string[] excludedManagedSettings;

    /// Enforce managed model defaults.
    bool enforceManagedModelDefaults = false;

    /// Trust the working directory for this "session" or for the "user"
    /// (see `TrustWorkingDirectory`).
    Nullable!string trustWorkingDirectory;

    /// Re-read custom instructions when the session is created.
    bool refreshCustomInstructions = false;

    /// Image generation configuration (experimental).
    Nullable!ImageGenerationConfig imageGeneration;

    /// Autopilot continuation options.
    Nullable!AutopilotConfig autopilot;

    /// Hook fired before a sub-agent's first turn.
    SubagentStartHandler onSubagentStart;

    /// Hook fired after a sub-agent completes a turn.
    SubagentStopHandler onSubagentStop;

    // --- v3.0.0 / SDK protocol 4 options ---

    /// Tool allowlist (wire: tools.availableTools). Required on every session
    /// when the client mode is `ClientMode.empty`.
    Nullable!(string[]) availableTools;

    /// Named BYOK provider connections (wire: inference.providers).
    NamedProviderConfig[] providers;

    /// BYOK model definitions (wire: inference.models).
    ProviderModelConfig[] models;

    /// Default named provider (wire: inference.defaultProvider).
    Nullable!string defaultProvider;

    /// Enable or disable Copilot (CAPI) models (wire: github.capi.enabled).
    Nullable!bool capiEnabled;

    /// Answers shell authentication requests (sudo, ssh, and git prompts).
    ShellAuthenticationHandler onShellAuthenticationRequest;

    /// Stores memories the agent asks to keep.
    StoreMemoryHandler onStoreMemory;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["streaming"] = streaming;
        auto infinite = JSONValue(string[string].init);
        infinite["enabled"] = infiniteSessions;
        obj["infiniteSessions"] = infinite;

        if (!systemMessage.isNull)
        {
            auto sys = JSONValue(string[string].init);
            sys["mode"] = "append";
            sys["content"] = systemMessage.get;
            obj["systemMessage"] = sys;
        }

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

        // The runtime accepts the auto routing tier, WebSocket responses, and
        // Copilot model availability only under `capi`.
        auto capi = capiJson(autoTier, enableWebSocketResponses, capiEnabled);
        if (capi.type == JSONType.object)
            obj["capi"] = capi;

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
            obj["enableExperimentalMode"] = true;

        if (contentExclusion)
            obj["contentExclusion"] = true;

        putSharedSessionOptions(obj, this);

        if (!name.isNull)
            obj["name"] = name.get;

        if (!trustWorkingDirectory.isNull)
            obj["trustWorkingDirectory"] = trustWorkingDirectory.get;

        if (refreshCustomInstructions)
            obj["refreshCustomInstructions"] = true;

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

    // --- v2.6.0 upstream-sync options (parity with @github/copilot-sdk) ---

    /// Auto routing tier (experimental; see `AutoTier`). Sent under `capi`.
    Nullable!string autoTier;

    /// Ask-user tool variant ("legacy" or "elicitation"; see `AskUserVariant`).
    Nullable!string askUserVariant;

    /// Supplies GitHub tokens on demand for the resumed session.
    GitHubTokenProvider gitHubTokenProvider;

    /// Sandbox configuration.
    Nullable!SandboxConfig sandboxConfig;

    /// Runtime feature-flag overrides; sent whenever set, even if empty.
    Nullable!(bool[string]) featureFlags;

    /// Restrict the models this session may use.
    string[] allowedModels;

    /// OAuth client ID metadata document URL used for MCP authorization.
    Nullable!string authClientIdMetadataUrl;

    /// Additional directories to load custom agents from.
    string[] agentDirectories;

    /// Additional directories to load hooks from.
    string[] hookDirectories;

    /// Whether an interactive user is present.
    bool interactiveUser = false;

    /// Managed settings to exclude for this session.
    string[] excludedManagedSettings;

    /// Enforce managed model defaults.
    bool enforceManagedModelDefaults = false;

    /// Allow recovering the session from its transcript when state is missing.
    bool allowTranscriptRecovery = false;

    /// Image generation configuration (experimental).
    Nullable!ImageGenerationConfig imageGeneration;

    /// Autopilot continuation options.
    Nullable!AutopilotConfig autopilot;

    /// Hook fired before a sub-agent's first turn.
    SubagentStartHandler onSubagentStart;

    /// Hook fired after a sub-agent completes a turn.
    SubagentStopHandler onSubagentStop;

    // --- v3.0.0 / SDK protocol 4 options ---

    /// Model to use after resuming.
    Nullable!string model;

    /// Suppress the resume event (wire: suppressResumeEvent).
    bool disableResume = false;

    /// Tool allowlist (wire: tools.availableTools). Required on every session
    /// when the client mode is `ClientMode.empty`.
    Nullable!(string[]) availableTools;

    /// Named BYOK provider connections to re-supply (wire: inference.providers).
    NamedProviderConfig[] providers;

    /// BYOK model definitions to re-supply (wire: inference.models).
    ProviderModelConfig[] models;

    /// Default named provider (wire: inference.defaultProvider).
    Nullable!string defaultProvider;

    /// Send an explicit null default provider to clear the resident one.
    bool clearDefaultProvider = false;

    /// Enable or disable Copilot (CAPI) models (wire: github.capi.enabled).
    Nullable!bool capiEnabled;

    /// Answers shell authentication requests (sudo, ssh, and git prompts).
    ShellAuthenticationHandler onShellAuthenticationRequest;

    /// Stores memories the agent asks to keep.
    StoreMemoryHandler onStoreMemory;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["sessionId"] = sessionId;
        obj["streaming"] = streaming;

        if (!model.isNull)
            obj["model"] = model.get;

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

        auto capi = capiJson(autoTier, false, capiEnabled);
        if (capi.type == JSONType.object)
            obj["capi"] = capi;

        putSharedSessionOptions(obj, this);

        if (clearDefaultProvider && defaultProvider.isNull)
            obj["defaultProvider"] = JSONValue(null);

        if (allowTranscriptRecovery)
            obj["allowTranscriptRecovery"] = true;

        if (disableResume)
            obj["disableResume"] = true;
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

    /// Safe to retry with the same arguments (wire: idempotent).
    bool idempotent = false;

    JSONValue toJson() const @safe
    {
        auto obj = JSONValue(string[string].init);
        obj["name"] = name;
        obj["description"] = description;
        obj["parameters"] = parametersSchema;
        if (idempotent)
            obj["idempotent"] = true;
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

    /// Legacy response format string. No longer sent to the runtime; use
    /// `responseSchema` for structured output.
    Nullable!string responseFormat;

    /// Agent mode override for this message (e.g. "agent" or "ask").
    Nullable!string agentMode;

    /// Prompt text shown to the user in place of the raw message.
    Nullable!string displayPrompt;

    /// Additional HTTP headers sent with this message request.
    string[string] requestHeaders;

    /// Origin of the message: `MessageSource.user`, `MessageSource.system`,
    /// or `MessageSource.agent(id)`.
    Nullable!string source;

    /// JSON Schema the final assistant response must satisfy (structured
    /// output). When set, it replaces any legacy `responseFormat` string.
    JSONValue responseSchema;

    /// Opaque caller-chosen turn identifier (wire: clientTurnId).
    Nullable!string clientTurnId;
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
    toolShellOutput       = "tool.shell_output",
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

    static ExitPlanModeRequest fromJson(JSONValue v) @trusted
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
