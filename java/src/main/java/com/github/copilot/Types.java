/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Type definitions for the Copilot SDK.
 */
public final class Types {
    private Types() {}

    /** Connection state of the client. */
    public enum ConnectionState {
        DISCONNECTED, CONNECTING, CONNECTED, ERROR
    }

    /** Result type from a tool execution. */
    public enum ToolResultType {
        @JsonProperty("success") SUCCESS,
        @JsonProperty("failure") FAILURE,
        @JsonProperty("rejected") REJECTED,
        @JsonProperty("denied") DENIED
    }

    /** Reasoning effort levels for models that support it. */
    public enum ReasoningEffort {
        @JsonProperty("low") LOW,
        @JsonProperty("medium") MEDIUM,
        @JsonProperty("high") HIGH,
        @JsonProperty("xhigh") XHIGH
    }

    /** Structured result from a tool execution. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ToolResultObject {
        @JsonProperty("textResultForLlm") public String textResultForLlm;
        @JsonProperty("resultType") public String resultType;
        @JsonProperty("error") public String error;
        @JsonProperty("sessionLog") public String sessionLog;
        @JsonProperty("toolTelemetry") public Map<String, Object> toolTelemetry;

        public ToolResultObject() {}

        public ToolResultObject(String textResultForLlm, String resultType, String error) {
            this.textResultForLlm = textResultForLlm;
            this.resultType = resultType;
            this.error = error;
            this.toolTelemetry = Map.of();
        }
    }

    /** Information about a tool invocation. */
    public static class ToolInvocation {
        public String sessionId;
        public String toolCallId;
        public String toolName;
        public Object arguments;

        public ToolInvocation(String sessionId, String toolCallId, String toolName, Object arguments) {
            this.sessionId = sessionId;
            this.toolCallId = toolCallId;
            this.toolName = toolName;
            this.arguments = arguments;
        }
    }

    /** Tool definition for registering custom tools. */
    public static class Tool {
        public String name;
        public String description;
        public Map<String, Object> parameters;
        public ToolHandler handler;
        /** Tool "defer" loading policy: {@code "auto"} (lazy via search) or {@code "never"} (eager pre-load). */
        public String defer;

        public Tool(String name, String description, Map<String, Object> parameters, ToolHandler handler) {
            this.name = name;
            this.description = description;
            this.parameters = parameters;
            this.handler = handler;
        }
    }

    /** Functional interface for tool handlers. */
    @FunctionalInterface
    public interface ToolHandler {
        Object handle(Object args, ToolInvocation invocation) throws Exception;
    }

    /** System message configuration - append mode. */
    public static class SystemMessageAppendConfig {
        public String mode = "append";
        public String content;
    }

    /** System message configuration - replace mode. */
    public static class SystemMessageReplaceConfig {
        public String mode = "replace";
        public String content;
    }

    /** Permission request from the server. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class PermissionRequest {
        public String kind;
        public String toolCallId;
    }

    /** Permission request result. */
    public static class PermissionRequestResult {
        public String kind;
        public List<Object> rules;
        /** Opaque context forwarded on permission replies. */
        public Map<String, Object> decisionContext;

        public PermissionRequestResult() {}
        public PermissionRequestResult(String kind) { this.kind = kind; }
    }

    /** Functional interface for permission request handlers. */
    @FunctionalInterface
    public interface PermissionHandler {
        PermissionRequestResult handle(PermissionRequest request, String sessionId) throws Exception;
    }

    /** User input request from the agent. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class UserInputRequest {
        public String question;
        public List<String> choices;
        public Boolean allowFreeform;
    }

    /** User input response. */
    public static class UserInputResponse {
        public String answer;
        public boolean wasFreeform;

        public UserInputResponse(String answer, boolean wasFreeform) {
            this.answer = answer;
            this.wasFreeform = wasFreeform;
        }
    }

    /** Functional interface for user input handlers. */
    @FunctionalInterface
    public interface UserInputHandler {
        UserInputResponse handle(UserInputRequest request, String sessionId) throws Exception;
    }

    /** Request to exit plan mode and continue with a selected action. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ExitPlanModeRequest {
        /** The session that produced this request. */
        public String sessionId;
        /** Summary of the plan or proposed next step. */
        public String summary;
        /** Full plan content, when available. */
        public String planContent;
        /** Available actions the user can select. */
        public List<String> actions;
        /** The action recommended by the runtime. */
        public String recommendedAction;
    }

    /** Response to an exit-plan-mode request. */
    public static class ExitPlanModeResponse {
        /** Whether the user approved exiting plan mode. */
        public boolean approved;
        /** Selected action, if the user chose one. */
        public String selectedAction;
        /** Optional feedback provided by the user. */
        public String feedback;

        public ExitPlanModeResponse() {}
        public ExitPlanModeResponse(boolean approved) { this.approved = approved; }
    }

    /** Functional interface for exit-plan-mode request handlers. */
    @FunctionalInterface
    public interface ExitPlanModeHandler {
        ExitPlanModeResponse handle(ExitPlanModeRequest request) throws Exception;
    }

    /** Context for a slash-command invocation. */
    public static class CommandContext {
        public String sessionId;
        public String command;
        public String commandName;
        public String args;

        public CommandContext() {}

        public CommandContext(String sessionId, String command, String commandName, String args) {
            this.sessionId = sessionId;
            this.command = command;
            this.commandName = commandName;
            this.args = args;
        }
    }

    /** Handler invoked when a registered slash-command is executed. */
    @FunctionalInterface
    public interface CommandHandler {
        void handle(CommandContext context) throws Exception;
    }

    /** Definition of a slash command registered with the session. */
    public static class CommandDefinition {
        public String name;
        public String description;
        public CommandHandler handler;

        public CommandDefinition() {}

        public CommandDefinition(String name, String description, CommandHandler handler) {
            this.name = name;
            this.description = description;
            this.handler = handler;
        }
    }

    /** Context for an elicitation request from the server. */
    public static class ElicitationContext {
        public String sessionId;
        public String message;
        public Map<String, Object> requestedSchema;
        public String mode;
        public String elicitationSource;
        public String url;

        public ElicitationContext() {}
    }

    /** Result returned from an elicitation handler. */
    public static class ElicitationResult {
        public String action;
        public Map<String, Object> content;

        public ElicitationResult() {}

        public ElicitationResult(String action, Map<String, Object> content) {
            this.action = action;
            this.content = content;
        }
    }

    /** Handler for elicitation requests. */
    @FunctionalInterface
    public interface ElicitationHandler {
        ElicitationResult handle(ElicitationContext context) throws Exception;
    }

    /** Provider configuration for BYOK. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ProviderConfig {
        public String type;
        @JsonProperty("baseUrl") public String baseUrl;
        @JsonProperty("apiKey") public String apiKey;
        @JsonProperty("bearerToken") public String bearerToken;
        @JsonProperty("wireApi") public String wireApi;
        /** Product serving the model, reported as {@code model_provider} (see {@link ModelProvider}). Omitted when null. */
        @JsonProperty("modelProvider") @JsonInclude(JsonInclude.Include.NON_NULL) public String modelProvider;
        /** Command run before each provider request; its stdout is used as the API key. Omitted when null. */
        @JsonProperty("apiKeyCommand") @JsonInclude(JsonInclude.Include.NON_NULL) public String apiKeyCommand;
    }

    /** MCP server configuration. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class MCPServerConfig {
        public List<String> tools;
        public String type;
        public Integer timeout;
        public String command;
        public List<String> args;
        public Map<String, String> env;
        public String cwd;
        public String url;
        public Map<String, String> headers;
    }

    /** MCP server configuration for stdio-based servers. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class MCPStdioServerConfig extends MCPServerConfig {
        public MCPStdioServerConfig() { this.type = "stdio"; }

        public MCPStdioServerConfig(String command, List<String> args) {
            this.type = "stdio";
            this.command = command;
            this.args = args;
        }
    }

    /** MCP server configuration for HTTP (SSE) servers. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class MCPHTTPServerConfig extends MCPServerConfig {
        public MCPHTTPServerConfig() { this.type = "sse"; }

        public MCPHTTPServerConfig(String url) {
            this.type = "sse";
            this.url = url;
        }
    }

    /** Custom agent configuration. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CustomAgentConfig {
        public String name;
        @JsonProperty("displayName") public String displayName;
        public String description;
        public List<String> tools;
        public String prompt;
        @JsonProperty("mcpServers") public Map<String, MCPServerConfig> mcpServers;
        public Boolean infer;
        /** List of skill names to preload into this agent's context. */
        public List<String> skills;
        /** JSON schema declaring a factory's arguments. */
        @JsonProperty("argsSchema") public Map<String, Object> argsSchema;
    }

    /** Infinite session configuration. */
    public static class InfiniteSessionConfig {
        public Boolean enabled;
        @JsonProperty("backgroundCompactionThreshold") public Double backgroundCompactionThreshold;
        @JsonProperty("bufferExhaustionThreshold") public Double bufferExhaustionThreshold;
    }

    /** Ping response. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class PingResponse {
        public String message;
        public long timestamp;
        @JsonProperty("protocolVersion") public Integer protocolVersion;
    }

    /** Status response. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class GetStatusResponse {
        public String version;
        @JsonProperty("protocolVersion") public int protocolVersion;
    }

    /** Auth status response. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class GetAuthStatusResponse {
        @JsonProperty("isAuthenticated") public boolean isAuthenticated;
        @JsonProperty("authType") public String authType;
        public String host;
        public String login;
        @JsonProperty("statusMessage") public String statusMessage;
    }

    /** Model capabilities. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ModelCapabilities {
        public ModelSupports supports;
        public ModelLimits limits;

        @JsonIgnoreProperties(ignoreUnknown = true)
        public static class ModelSupports {
            public boolean vision;
            @JsonProperty("reasoningEffort") public boolean reasoningEffort;
        }

        @JsonIgnoreProperties(ignoreUnknown = true)
        public static class ModelLimits {
            @JsonProperty("max_prompt_tokens") public Integer maxPromptTokens;
            @JsonProperty("max_output_tokens") public Integer maxOutputTokens;
            @JsonProperty("max_context_window_tokens") public int maxContextWindowTokens;
        }
    }

    /** Model information. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ModelInfo {
        public String id;
        public String name;
        public ModelCapabilities capabilities;
    }

    /** Session metadata. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SessionMetadata {
        @JsonProperty("sessionId") public String sessionId;
        @JsonProperty("startTime") public String startTime;
        @JsonProperty("modifiedTime") public String modifiedTime;
        public String summary;
        @JsonProperty("isRemote") public boolean isRemote;
    }

    /** Session lifecycle event types. */
    public enum SessionLifecycleEventType {
        @JsonProperty("session.created") SESSION_CREATED,
        @JsonProperty("session.deleted") SESSION_DELETED,
        @JsonProperty("session.updated") SESSION_UPDATED,
        @JsonProperty("session.foreground") SESSION_FOREGROUND,
        @JsonProperty("session.background") SESSION_BACKGROUND
    }

    /** Session lifecycle event. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SessionLifecycleEvent {
        public String type;
        @JsonProperty("sessionId") public String sessionId;
        public Map<String, Object> metadata;
    }

    /** Session event (generic). */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SessionEvent {
        /** Sub-agent instance identifier. Absent for root/main agent events. */
        @JsonProperty("agentId") public String agentId;
        /** When true, the event is transient and not persisted. */
        public Boolean ephemeral;
        /** Unique event identifier (UUID). */
        public String id;
        /** ID of the chronologically preceding event. Null for the first event. */
        @JsonProperty("parentId") public String parentId;
        /** ISO 8601 timestamp when the event was created. */
        public String timestamp;
        /** The event type discriminator. */
        public String type;
        /** The event data payload. */
        public Map<String, Object> data;
    }

    /** Functional interface for session lifecycle handlers. */
    @FunctionalInterface
    public interface SessionLifecycleHandler {
        void handle(SessionLifecycleEvent event);
    }

    /** Functional interface for session event handlers. */
    @FunctionalInterface
    public interface SessionEventHandler {
        void handle(SessionEvent event);
    }

    /** Response format for message responses. */
    public enum ResponseFormat {
        @JsonProperty("text") TEXT,
        @JsonProperty("image") IMAGE,
        @JsonProperty("json_object") JSON_OBJECT
    }

    /** Options for image generation. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ImageOptions {
        @JsonProperty("size") public String size;
        @JsonProperty("quality") public String quality;
        @JsonProperty("style") public String style;

        public ImageOptions() {}

        public ImageOptions(String size, String quality, String style) {
            this.size = size;
            this.quality = quality;
            this.style = style;
        }
    }

    /** Image data from an assistant image response. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class AssistantImageData {
        @JsonProperty("format") public String format;
        @JsonProperty("base64") public String base64;
        @JsonProperty("url") public String url;
        @JsonProperty("revisedPrompt") public String revisedPrompt;
        @JsonProperty("width") public int width;
        @JsonProperty("height") public int height;
    }

    /** A content block in a mixed text+image response. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ContentBlock {
        @JsonProperty("type") public String type;
        @JsonProperty("text") public String text;
        @JsonProperty("image") public AssistantImageData image;
    }

    /** Error encountered during stop(). */
    public static class StopError {
        public String message;
        public StopError(String message) { this.message = message; }
    }

    /** Configuration for a custom session filesystem provider. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SessionFsConfig {
        @JsonProperty("initialCwd") public String initialCwd;
        @JsonProperty("sessionStatePath") public String sessionStatePath;
        @JsonProperty("conventions") public String conventions;
    }

    /** File metadata returned by SessionFsProvider methods. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SessionFsFileInfo {
        public String name;
        public long size;
        @JsonProperty("isDirectory") public boolean isDirectory;
        @JsonProperty("isFile") public boolean isFile;
        @JsonProperty("createdAt") public String createdAt;
        @JsonProperty("modifiedAt") public String modifiedAt;
    }

    /**
     * Interface for session filesystem providers.
     * Implementors provide file operations scoped to a session.
     */
    public interface SessionFsProvider {
        String readFile(String sessionId, String path) throws Exception;
        void writeFile(String sessionId, String path, String content) throws Exception;
        void appendFile(String sessionId, String path, String content) throws Exception;
        boolean exists(String sessionId, String path) throws Exception;
        SessionFsFileInfo stat(String sessionId, String path) throws Exception;
        void mkdir(String sessionId, String path, boolean recursive) throws Exception;
        List<String> readdir(String sessionId, String path) throws Exception;
        List<SessionFsFileInfo> readdirWithTypes(String sessionId, String path) throws Exception;
        void rm(String sessionId, String path, boolean recursive) throws Exception;
        void rename(String sessionId, String oldPath, String newPath) throws Exception;
    }

    // --- Upstream-sync feature types (parity with @github/copilot-sdk) ---

    /** Per-session AI-credit budget; set {@code maxAiCredits} to cap spend. */
    public static class SessionLimitsConfig {
        @JsonProperty("maxAiCredits") public Integer maxAiCredits;

        public SessionLimitsConfig() {}
        public SessionLimitsConfig(Integer maxAiCredits) { this.maxAiCredits = maxAiCredits; }
    }

    /** Opt-in persistent session memory configuration. */
    public static class MemoryConfiguration {
        @JsonProperty("enabled") public Boolean enabled;

        public MemoryConfiguration() {}
        public MemoryConfiguration(Boolean enabled) { this.enabled = enabled; }
    }

    /** Arguments passed to a BYOK bearer-token provider (per-session scoping). */
    public static class ProviderTokenArgs {
        @JsonProperty("sessionId") public String sessionId;

        public ProviderTokenArgs() {}
        public ProviderTokenArgs(String sessionId) { this.sessionId = sessionId; }
    }

    /** Supplies a per-session bearer token for BYOK model providers. */
    @FunctionalInterface
    public interface BearerTokenProvider {
        String getBearerToken(ProviderTokenArgs args) throws Exception;
    }

    /**
     * Intercepts outbound LLM inference HTTP/WebSocket requests. Assign an instance to
     * {@link CopilotClientOptions#requestHandler(CopilotRequestHandler)} to mutate, replace,
     * or forward the request before it is sent.
     */
    @FunctionalInterface
    public interface CopilotRequestHandler {
        Map<String, Object> sendRequest(Map<String, Object> request, Map<String, Object> context) throws Exception;
    }

    /** Handler invoked when an MCP server requests an OAuth host token. */
    @FunctionalInterface
    public interface McpAuthHandler {
        String handle(Map<String, Object> request, String sessionId) throws Exception;
    }

    // --- v2.6.0 upstream-sync feature types ---

    /**
     * Identity of the integrating application, declared once on the {@code connect} handshake so
     * runtime telemetry is attributed to a consistent surface. All fields are optional; empty
     * fields are dropped and an all-empty identity skips the handshake entirely.
     */
    public static class ClientInfo {
        /** Name of the application using the SDK, e.g. {@code "acme-developer-portal"}. */
        public String applicationName;
        /** Version of the application using the SDK, e.g. {@code "2.4.0"}. */
        public String applicationVersion;
        /** Name of a specific integration within the application, such as an extension or plugin. */
        public String integrationName;
        /** Version of the integration identified by {@code integrationName}. */
        public String integrationVersion;

        public ClientInfo() {}

        public ClientInfo(String applicationName, String applicationVersion) {
            this.applicationName = applicationName;
            this.applicationVersion = applicationVersion;
        }

        public ClientInfo applicationName(String name) { this.applicationName = name; return this; }
        public ClientInfo applicationVersion(String version) { this.applicationVersion = version; return this; }
        public ClientInfo integrationName(String name) { this.integrationName = name; return this; }
        public ClientInfo integrationVersion(String version) { this.integrationVersion = version; return this; }

        /**
         * Maps this identity onto the {@code connect} wire shape, dropping empty fields.
         *
         * @return the wire map, or null when no field carries a non-empty value
         */
        public Map<String, Object> toWire() {
            Map<String, Object> wire = new HashMap<>();
            if (applicationName != null && !applicationName.isEmpty()) wire.put("editorName", applicationName);
            if (applicationVersion != null && !applicationVersion.isEmpty()) wire.put("editorVersion", applicationVersion);
            if (integrationName != null && !integrationName.isEmpty()) wire.put("extensionName", integrationName);
            if (integrationVersion != null && !integrationVersion.isEmpty()) wire.put("extensionVersion", integrationVersion);
            return wire.isEmpty() ? null : wire;
        }
    }

    /** Auto-model routing preferences for {@code SessionConfig.autoTier} and {@code CopilotSession.setAutoTier} (experimental). */
    public static final class AutoTier {
        public static final String EFFICIENCY = "efficiency";
        public static final String BALANCE = "balance";
        public static final String INTELLIGENCE = "intelligence";
        public static final String FAST = "fast";
        private AutoTier() {}
    }

    /** Model-facing shapes of the built-in {@code ask_user} tool. */
    public static final class AskUserVariant {
        public static final String LEGACY = "legacy";
        public static final String ELICITATION = "elicitation";
        private AskUserVariant() {}
    }

    /** Working-directory trust scopes for {@code SessionConfig.trustWorkingDirectory}. */
    public static final class WorkingDirectoryTrust {
        /** Trust applies only to this session. */
        public static final String SESSION = "session";
        /** Trust is persisted for later sessions. */
        public static final String USER = "user";
        private WorkingDirectoryTrust() {}
    }

    /** Message provenance values for {@code MessageOptions.source}. */
    public static final class MessageSource {
        public static final String USER = "user";
        public static final String SYSTEM = "system";
        private MessageSource() {}

        /**
         * Builds the source value for a message originating from an identified agent.
         *
         * @param agentId the agent identifier
         * @return {@code "agent-" + agentId}
         */
        public static String agent(String agentId) {
            return "agent-" + agentId;
        }
    }

    /** Products serving a BYOK model, for {@code ProviderConfig.modelProvider}. */
    public static final class ModelProvider {
        public static final String OPENAI = "openai";
        public static final String ANTHROPIC = "anthropic";
        public static final String AZURE_OPENAI = "azure_openai";
        public static final String OLLAMA = "ollama";
        public static final String LM_STUDIO = "lm_studio";
        public static final String FOUNDRY_LOCAL = "foundry_local";
        public static final String LLAMA_CPP = "llama_cpp";
        private ModelProvider() {}
    }

    /** Sandbox configuration applied before MCP servers or session tools start (experimental). */
    public static class SandboxConfig {
        /** Whether sandboxing is enabled for the session. */
        public boolean enabled;
        /** User-managed sandbox policy fragment merged into the auto-discovered base policy. */
        public Map<String, Object> userPolicy;
        /** Whether to auto-add the current working directory to the read-write paths (runtime default: true). */
        public Boolean addCurrentWorkingDirectory;
        /** Whether MCP servers the session launches are confined by the sandbox (runtime default: true). */
        public Boolean sandboxMcpServers;
        /** Whether language servers the session launches are confined by the sandbox (runtime default: true). */
        public Boolean sandboxLspServers;
        /** Whether extension processes the session launches are confined by the sandbox (runtime default: true). */
        public Boolean sandboxExtensions;
        /**
         * Opt-in: lets the agent request that an individual command run outside the sandbox,
         * which the host approves or denies through the normal permission flow (default: false).
         */
        public Boolean allowBypass;

        public SandboxConfig() {}
        public SandboxConfig(boolean enabled) { this.enabled = enabled; }

        public SandboxConfig userPolicy(Map<String, Object> policy) { this.userPolicy = policy; return this; }
        public SandboxConfig addCurrentWorkingDirectory(boolean add) { this.addCurrentWorkingDirectory = add; return this; }
        public SandboxConfig sandboxMcpServers(boolean sandbox) { this.sandboxMcpServers = sandbox; return this; }
        public SandboxConfig sandboxLspServers(boolean sandbox) { this.sandboxLspServers = sandbox; return this; }
        public SandboxConfig sandboxExtensions(boolean sandbox) { this.sandboxExtensions = sandbox; return this; }
        public SandboxConfig allowBypass(boolean allow) { this.allowBypass = allow; return this; }

        /**
         * Converts this configuration to its wire shape, omitting unset optional fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            wire.put("enabled", enabled);
            if (userPolicy != null) wire.put("userPolicy", userPolicy);
            if (addCurrentWorkingDirectory != null) wire.put("addCurrentWorkingDirectory", addCurrentWorkingDirectory);
            if (sandboxMcpServers != null) wire.put("sandboxMcpServers", sandboxMcpServers);
            if (sandboxLspServers != null) wire.put("sandboxLspServers", sandboxLspServers);
            if (sandboxExtensions != null) wire.put("sandboxExtensions", sandboxExtensions);
            if (allowBypass != null) wire.put("allowBypass", allowBypass);
            return wire;
        }
    }

    /** Non-persisted session-level opt-in to image generation (experimental). */
    public static class ImageGenerationConfig {
        /** Opt in to image generation through an authorized Copilot image model. */
        public Boolean enabled;
        /** Request a visible "Made with AI" label on generated and edited images. */
        public Boolean watermark;
        /** Save generated images under the session-state {@code images} folder (runtime default: true). */
        public Boolean saveToDisk;

        public ImageGenerationConfig() {}
        public ImageGenerationConfig(Boolean enabled) { this.enabled = enabled; }

        public ImageGenerationConfig enabled(boolean enabled) { this.enabled = enabled; return this; }
        public ImageGenerationConfig watermark(boolean watermark) { this.watermark = watermark; return this; }
        public ImageGenerationConfig saveToDisk(boolean saveToDisk) { this.saveToDisk = saveToDisk; return this; }

        /**
         * Converts this configuration to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            if (enabled != null) wire.put("enabled", enabled);
            if (watermark != null) wire.put("watermark", watermark);
            if (saveToDisk != null) wire.put("saveToDisk", saveToDisk);
            return wire;
        }
    }

    /** Autopilot continuation policy (experimental). */
    public static class AutopilotOptions {
        /** Enable runtime-owned automatic continuation while in Autopilot mode (runtime default: true). */
        public Boolean continuationEnabled;
        /** Maximum automatic continuations outside an active objective (runtime default: 5; 0 allows none). */
        public Integer maxContinues;

        public AutopilotOptions() {}
        public AutopilotOptions(Boolean continuationEnabled, Integer maxContinues) {
            this.continuationEnabled = continuationEnabled;
            this.maxContinues = maxContinues;
        }

        public AutopilotOptions continuationEnabled(boolean enabled) { this.continuationEnabled = enabled; return this; }
        public AutopilotOptions maxContinues(int max) { this.maxContinues = max; return this; }

        /**
         * Converts these options to their wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            if (continuationEnabled != null) wire.put("continuationEnabled", continuationEnabled);
            if (maxContinues != null) wire.put("maxContinues", maxContinues);
            return wire;
        }
    }

    /** Arguments passed to a {@link GitHubTokenProvider}. */
    public static class GitHubTokenProviderArgs {
        /** Effective GitHub host for which a token is required. */
        public String host;
        /** Session receiving the token, or null before a session id is assigned. */
        public String sessionId;
        /** Why a token is requested: {@code "initial"} or {@code "refresh"}. */
        public String reason;

        public GitHubTokenProviderArgs() {}

        public GitHubTokenProviderArgs(String host, String sessionId, String reason) {
            this.host = host;
            this.sessionId = sessionId;
            this.reason = reason;
        }
    }

    /** Tagged token or cancellation returned by a {@link GitHubTokenProvider}. */
    public static class GitHubTokenProviderResult {
        /** {@code "token"} or {@code "cancelled"}. */
        public String kind;
        /** GitHub access token (token results only). */
        public String accessToken;
        /** OAuth token type; defaults to bearer when omitted. */
        public String tokenType;
        /** Remaining token lifetime in seconds (token results only). */
        public Long expiresIn;

        public GitHubTokenProviderResult() {}

        /**
         * Creates a token result.
         *
         * @param accessToken the GitHub access token
         * @param expiresIn remaining lifetime in seconds
         * @return the token result
         */
        public static GitHubTokenProviderResult token(String accessToken, long expiresIn) {
            return token(accessToken, null, expiresIn);
        }

        /**
         * Creates a token result with an explicit token type.
         *
         * @param accessToken the GitHub access token
         * @param tokenType the OAuth token type, or null for bearer
         * @param expiresIn remaining lifetime in seconds
         * @return the token result
         */
        public static GitHubTokenProviderResult token(String accessToken, String tokenType, long expiresIn) {
            GitHubTokenProviderResult result = new GitHubTokenProviderResult();
            result.kind = "token";
            result.accessToken = accessToken;
            result.tokenType = tokenType;
            result.expiresIn = expiresIn;
            return result;
        }

        /**
         * Creates a cancellation result.
         *
         * @return the cancellation result
         */
        public static GitHubTokenProviderResult cancelled() {
            GitHubTokenProviderResult result = new GitHubTokenProviderResult();
            result.kind = "cancelled";
            return result;
        }

        /**
         * Converts this result to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            wire.put("kind", kind);
            if (accessToken != null) wire.put("accessToken", accessToken);
            if (tokenType != null) wire.put("tokenType", tokenType);
            if (expiresIn != null) wire.put("expiresIn", expiresIn);
            return wire;
        }
    }

    /**
     * Acquires short-lived GitHub credentials for one session on demand.
     * Mutually exclusive with {@code SessionConfig.gitHubToken}.
     */
    @FunctionalInterface
    public interface GitHubTokenProvider {
        GitHubTokenProviderResult getToken(GitHubTokenProviderArgs args) throws Exception;
    }
}
