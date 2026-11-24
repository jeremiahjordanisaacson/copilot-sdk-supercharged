/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.github.copilot.internal.JsonRpcClient;
import java.util.ArrayList;
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
        /**
         * Declares that repeating a call with the same arguments has no additional effect, so the
         * runtime may safely retry it. Omitted when null.
         */
        public Boolean idempotent;

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
        /** ID of the pending user input request, when the runtime sends one; null otherwise. */
        public String requestId;
        /**
         * Image support advertised by the runtime; null when the runtime does not accept images in
         * the answer. When set, the answer may carry {@link UserInputResponse#attachments}.
         */
        public UserInputImageAttachmentConstraints imageAttachmentConstraints;
        /** The request's opaque {@code _meta} object, when the runtime sends one; null otherwise. */
        @JsonProperty("_meta") public Map<String, Object> meta;
    }

    /**
     * User input response. {@link #attachments} is only valid when the request advertised
     * {@link UserInputRequest#imageAttachmentConstraints}; for an image-only answer use an empty
     * {@code answer} and {@code wasFreeform = true}. The runtime enforces the advertised limits.
     */
    public static class UserInputResponse {
        public String answer;
        public boolean wasFreeform;
        /** Images attached to the answer; sent only when set. */
        public List<UserInputImageAttachment> attachments;

        public UserInputResponse(String answer, boolean wasFreeform) {
            this.answer = answer;
            this.wasFreeform = wasFreeform;
        }

        public UserInputResponse(String answer, boolean wasFreeform, List<UserInputImageAttachment> attachments) {
            this(answer, wasFreeform);
            this.attachments = attachments;
        }

        /**
         * Converts this response to its wire shape, including {@code attachments} only when set.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            wire.put("answer", answer);
            wire.put("wasFreeform", wasFreeform);
            if (attachments != null) {
                List<Map<String, Object>> images = new ArrayList<>();
                for (UserInputImageAttachment attachment : attachments) {
                    if (attachment != null) images.add(attachment.toWire());
                }
                wire.put("attachments", images);
            }
            return wire;
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
        /**
         * Stores memories the agent saves (sent to the runtime as {@code state.hasMemoryStore}).
         * Requires {@code enabled} to be true. Never serialized.
         */
        @JsonIgnore public MemoryStoreHandler storeMemory;

        public MemoryConfiguration() {}
        public MemoryConfiguration(Boolean enabled) { this.enabled = enabled; }

        /**
         * Sets the callback that stores memories the agent saves.
         *
         * @param handler the memory store callback
         * @return this configuration
         */
        public MemoryConfiguration storeMemory(MemoryStoreHandler handler) { this.storeMemory = handler; return this; }
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

    // --- v3.0.0 feature types (SDK protocol 4) ---

    /** Client modes for {@code CopilotClientOptions.mode}. */
    public static final class ClientMode {
        /** The default: the full Copilot coding agent, identical to earlier SDK behavior. */
        public static final String CODING_AGENT = "coding-agent";
        /**
         * Every session must choose its tools ({@code availableTools}) and runtime features such as
         * telemetry, repository hooks, and cross-session memory default to off.
         */
        public static final String EMPTY = "empty";
        /** Legacy alias of {@link #CODING_AGENT}. */
        public static final String COPILOT_CLI = "copilot-cli";
        private ClientMode() {}
    }

    /** Copilot API (CAPI) provider options, sent as {@code github.capi}. */
    public static class CapiConfig {
        /** Enables Copilot API models; set false to keep a BYOK-only model catalog. */
        public Boolean enabled;
        /** Routing preference used when the session model is {@code auto} (see {@link AutoTier}). */
        public String autoTier;
        /** Whether to use the WebSocket transport for the CAPI Responses API. */
        public Boolean enableWebSocketResponses;

        public CapiConfig() {}
        public CapiConfig(Boolean enabled) { this.enabled = enabled; }

        public CapiConfig enabled(boolean enabled) { this.enabled = enabled; return this; }
        public CapiConfig autoTier(String autoTier) { this.autoTier = autoTier; return this; }
        public CapiConfig enableWebSocketResponses(boolean enable) { this.enableWebSocketResponses = enable; return this; }

        /**
         * Converts these options to their wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            if (enabled != null) wire.put("enabled", enabled);
            if (autoTier != null) wire.put("autoTier", autoTier);
            if (enableWebSocketResponses != null) wire.put("enableWebSocketResponses", enableWebSocketResponses);
            return wire;
        }
    }

    /**
     * A named BYOK provider connection, sent in {@code inference.providers}. Model definitions
     * ({@link ProviderModelConfig}) reference it by {@link #name}.
     */
    public static class NamedProviderConfig {
        /** Stable connection identifier; must not contain {@code /}. */
        public String name;
        /** Provider type: {@code "openai"} (default), {@code "azure"}, or {@code "anthropic"}. */
        public String type;
        /** API endpoint URL. */
        public String baseUrl;
        /** API key; optional for local providers. */
        public String apiKey;
        /** Command run before each request whose stdout is used as the API key. */
        public String apiKeyCommand;
        /** Bearer token sent as the Authorization header; takes precedence over the API key. */
        public String bearerToken;
        /** Wire API format: {@code "completions"} (default) or {@code "responses"}. */
        public String wireApi;
        /** Transport: {@code "http"} (default) or {@code "websockets"} (Responses API only). */
        public String transport;
        /** Azure-specific options, for example {@code apiVersion}. */
        public Map<String, Object> azure;
        /** Custom HTTP headers sent with every request to the provider. */
        public Map<String, String> headers;
        /** Product serving the models, reported in telemetry (see {@link ModelProvider}). */
        public String modelProvider;

        public NamedProviderConfig() {}

        public NamedProviderConfig(String name, String baseUrl) {
            this.name = name;
            this.baseUrl = baseUrl;
        }

        public NamedProviderConfig name(String name) { this.name = name; return this; }
        public NamedProviderConfig type(String type) { this.type = type; return this; }
        public NamedProviderConfig baseUrl(String baseUrl) { this.baseUrl = baseUrl; return this; }
        public NamedProviderConfig apiKey(String apiKey) { this.apiKey = apiKey; return this; }
        public NamedProviderConfig apiKeyCommand(String command) { this.apiKeyCommand = command; return this; }
        public NamedProviderConfig bearerToken(String token) { this.bearerToken = token; return this; }
        public NamedProviderConfig wireApi(String wireApi) { this.wireApi = wireApi; return this; }
        public NamedProviderConfig transport(String transport) { this.transport = transport; return this; }
        public NamedProviderConfig azure(Map<String, Object> azure) { this.azure = azure; return this; }
        public NamedProviderConfig headers(Map<String, String> headers) { this.headers = headers; return this; }
        public NamedProviderConfig modelProvider(String modelProvider) { this.modelProvider = modelProvider; return this; }

        /**
         * Converts this connection to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            if (name != null) wire.put("name", name);
            if (type != null) wire.put("type", type);
            if (baseUrl != null) wire.put("baseUrl", baseUrl);
            if (apiKey != null) wire.put("apiKey", apiKey);
            if (apiKeyCommand != null) wire.put("apiKeyCommand", apiKeyCommand);
            if (bearerToken != null) wire.put("bearerToken", bearerToken);
            if (wireApi != null) wire.put("wireApi", wireApi);
            if (transport != null) wire.put("transport", transport);
            if (azure != null) wire.put("azure", azure);
            if (headers != null) wire.put("headers", headers);
            if (modelProvider != null) wire.put("modelProvider", modelProvider);
            return wire;
        }
    }

    /**
     * A BYOK model definition served by a {@link NamedProviderConfig}, sent in
     * {@code inference.models}. Select it with the canonical {@code "provider/id"} model ID.
     */
    public static class ProviderModelConfig {
        /** Name of the {@link NamedProviderConfig} that serves this model. */
        public String provider;
        /** Provider-local model ID, unique within its provider. */
        public String id;
        /** Well-known base model ID used for capability lookup; defaults to {@link #id}. */
        public String modelId;
        /** Model or deployment name sent to the provider API; defaults to {@link #id}. */
        public String wireModel;
        /** Display name; defaults to {@code "provider/id"}. */
        public String name;
        public Integer maxPromptTokens;
        public Integer maxContextWindowTokens;
        public Integer maxOutputTokens;
        /** Capability overrides for the model (same shape as {@code SessionConfig.modelCapabilities}). */
        public Map<String, Object> capabilities;

        public ProviderModelConfig() {}

        public ProviderModelConfig(String provider, String id) {
            this.provider = provider;
            this.id = id;
        }

        public ProviderModelConfig modelId(String modelId) { this.modelId = modelId; return this; }
        public ProviderModelConfig wireModel(String wireModel) { this.wireModel = wireModel; return this; }
        public ProviderModelConfig name(String name) { this.name = name; return this; }
        public ProviderModelConfig maxPromptTokens(int tokens) { this.maxPromptTokens = tokens; return this; }
        public ProviderModelConfig maxContextWindowTokens(int tokens) { this.maxContextWindowTokens = tokens; return this; }
        public ProviderModelConfig maxOutputTokens(int tokens) { this.maxOutputTokens = tokens; return this; }
        public ProviderModelConfig capabilities(Map<String, Object> capabilities) { this.capabilities = capabilities; return this; }

        /**
         * Converts this model definition to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toMap() {
            Map<String, Object> wire = new HashMap<>();
            if (provider != null) wire.put("provider", provider);
            if (id != null) wire.put("id", id);
            if (modelId != null) wire.put("modelId", modelId);
            if (wireModel != null) wire.put("wireModel", wireModel);
            if (name != null) wire.put("name", name);
            if (maxPromptTokens != null) wire.put("maxPromptTokens", maxPromptTokens);
            if (maxContextWindowTokens != null) wire.put("maxContextWindowTokens", maxContextWindowTokens);
            if (maxOutputTokens != null) wire.put("maxOutputTokens", maxOutputTokens);
            if (capabilities != null) wire.put("capabilities", capabilities);
            return wire;
        }
    }

    /** A program's request for a secret or confirmation while a shell command runs. */
    public static class ShellAuthenticationRequest {
        /** Opaque request ID, unique within the session. */
        public String requestId;
        /** Program that asked, for example {@code "sudo"} or {@code "ssh"}. */
        public String program;
        /** Prompt text the program printed. Treat it as untrusted. */
        public String prompt;
        /** {@code "secret"}, {@code "text"}, or {@code "confirm"}. */
        public String inputKind;
        /** Command line that triggered the prompt (may be empty). */
        public String command;
        /** Working directory of the command (may be empty). */
        public String cwd;

        public ShellAuthenticationRequest() {}
    }

    /** Answer to a {@link ShellAuthenticationRequest}. */
    public static class ShellAuthenticationResult {
        /** {@code "accept"} or {@code "cancel"}. */
        public String action;
        /** The value typed for the program (accept only). Never logged. */
        public String value;

        public ShellAuthenticationResult() {}

        /**
         * Creates an accept answer carrying the value to send to the program.
         *
         * @param value the secret, text, or confirmation value
         * @return the answer
         */
        public static ShellAuthenticationResult accept(String value) {
            ShellAuthenticationResult result = new ShellAuthenticationResult();
            result.action = "accept";
            result.value = value;
            return result;
        }

        /**
         * Creates a cancel answer.
         *
         * @return the answer
         */
        public static ShellAuthenticationResult cancel() {
            ShellAuthenticationResult result = new ShellAuthenticationResult();
            result.action = "cancel";
            return result;
        }
    }

    /**
     * Answers shell authentication prompts (served via {@code shellAuthentication.request}).
     * Returning null cancels the prompt.
     */
    @FunctionalInterface
    public interface ShellAuthenticationHandler {
        ShellAuthenticationResult handle(ShellAuthenticationRequest request, String sessionId) throws Exception;
    }

    /** A memory the agent asked to store, after validation, secret filtering, and approval. */
    public static class StoredMemory {
        /** ID of the session that stored the memory. */
        public String sessionId;
        /** Short topic the memory is about. */
        public String subject;
        /** The fact to remember. */
        public String fact;
        /** Sources supporting the fact, such as file paths with line numbers. */
        public String citations;
        /** Why the agent considers the fact worth remembering. */
        public String reason;

        public StoredMemory() {}
    }

    /** Stores memories the agent saves (served via {@code memoryStore.store}). */
    @FunctionalInterface
    public interface MemoryStoreHandler {
        void store(StoredMemory memory) throws Exception;
    }

    /** Network traffic scopes for {@link NetworkConfig#scope}. */
    public static final class NetworkScope {
        /** Requests made for sessions this client creates or resumes (the default). */
        public static final String CONNECTION = "connection";
        /** Also the runtime's shared background traffic; only one connection can hold it. */
        public static final String RUNTIME = "runtime";
        private NetworkScope() {}
    }

    /** Arguments of a {@code network.resolveProxy} request. */
    public static class NetworkResolveProxyRequest {
        /** Request target as a PAC script sees it. */
        public String url;

        public NetworkResolveProxyRequest() {}
        public NetworkResolveProxyRequest(String url) { this.url = url; }
    }

    /** Arguments of a {@code network.authenticateNegotiate} request. */
    public static class NetworkAuthenticateNegotiateRequest {
        /** Service principal in {@code service/host[@REALM]} form. */
        public String spn;

        public NetworkAuthenticateNegotiateRequest() {}
        public NetworkAuthenticateNegotiateRequest(String spn) { this.spn = spn; }
    }

    /** Arguments of a {@code network.authenticateBasic} request. */
    public static class NetworkAuthenticateBasicRequest {
        /** Proxy that challenged, as {@code http://host:port} or {@code https://host:port}. */
        public String proxy;
        /** Realm named by the proxy, or null. Untrusted text. */
        public String realm;
        /** True when the proxy rejected the credentials sent last. */
        public boolean previousRejected;

        public NetworkAuthenticateBasicRequest() {}
    }

    /** Proxy credentials returned by a {@link BasicAuthenticator}. Never logged. */
    public static class NetworkBasicCredentials {
        public String username;
        public String password;

        public NetworkBasicCredentials() {}

        public NetworkBasicCredentials(String username, String password) {
            this.username = username;
            this.password = password;
        }
    }

    /** Returns a PAC-style proxy list (for example {@code "PROXY proxy:8080; DIRECT"}), or null for default routing. */
    @FunctionalInterface
    public interface ProxyResolver {
        String resolveProxy(NetworkResolveProxyRequest request) throws Exception;
    }

    /** Returns a base64 SPNEGO token without the {@code Negotiate} prefix, or null when unavailable. */
    @FunctionalInterface
    public interface NegotiateAuthenticator {
        String authenticateNegotiate(NetworkAuthenticateNegotiateRequest request) throws Exception;
    }

    /** Returns credentials for a proxy's Basic challenge, or null when unavailable. */
    @FunctionalInterface
    public interface BasicAuthenticator {
        NetworkBasicCredentials authenticateBasic(NetworkAuthenticateBasicRequest request) throws Exception;
    }

    /**
     * Network callbacks through which the host takes over parts of the runtime's networking
     * (proxy resolution and proxy authentication). Registered with {@code network.setProvider}
     * after the client connects when at least one callback is set.
     */
    public static class NetworkConfig {
        /** Traffic the callbacks serve (see {@link NetworkScope}); the runtime default is {@code "connection"}. */
        public String scope;
        public ProxyResolver resolveProxy;
        public NegotiateAuthenticator authenticateNegotiate;
        public BasicAuthenticator authenticateBasic;

        public NetworkConfig() {}

        public NetworkConfig scope(String scope) { this.scope = scope; return this; }
        public NetworkConfig resolveProxy(ProxyResolver resolver) { this.resolveProxy = resolver; return this; }
        public NetworkConfig authenticateNegotiate(NegotiateAuthenticator authenticator) { this.authenticateNegotiate = authenticator; return this; }
        public NetworkConfig authenticateBasic(BasicAuthenticator authenticator) { this.authenticateBasic = authenticator; return this; }

        /**
         * Returns whether any callback is configured.
         *
         * @return true if at least one callback is set
         */
        public boolean hasCallbacks() {
            return resolveProxy != null || authenticateNegotiate != null || authenticateBasic != null;
        }
    }

    /** Data of a {@code tool.shell_output} session event: live, append-only output of a running shell tool call. */
    public static class ToolShellOutputData {
        /** The session event type carrying this data. */
        public static final String EVENT_TYPE = "tool.shell_output";

        /** Tool call that owns this output. */
        public String toolCallId;
        /** New output to append; chunks need not contain complete lines. */
        public String text;
        /** Zero-based publication sequence across all streams of the tool call. */
        public long sequence;
        /** {@code "stdout"}, {@code "stderr"}, or null (stdout). */
        public String stream;

        public ToolShellOutputData() {}

        /**
         * Reads the shell output data of a {@code tool.shell_output} event.
         *
         * @param event the session event
         * @return the data, or null when the event is not a shell output event
         */
        public static ToolShellOutputData fromEvent(SessionEvent event) {
            if (event == null || !EVENT_TYPE.equals(event.type) || event.data == null) return null;
            ToolShellOutputData data = new ToolShellOutputData();
            Object toolCallId = event.data.get("toolCallId");
            Object text = event.data.get("text");
            Object sequence = event.data.get("sequence");
            Object stream = event.data.get("stream");
            data.toolCallId = toolCallId instanceof String ? (String) toolCallId : null;
            data.text = text instanceof String ? (String) text : "";
            data.sequence = sequence instanceof Number ? ((Number) sequence).longValue() : 0L;
            data.stream = stream instanceof String ? (String) stream : null;
            return data;
        }
    }

    /**
     * The latest unresolved Policy Helper failure. Carried by {@code session.create} and
     * {@code session.resume} errors as {@code error.data.policyHelperFailure} and by the managed
     * settings snapshot as {@code policyHelperFailure}. Helper-authored text is untrusted plain text.
     */
    public static class PolicyHelperFailure {
        /** Stable runtime classification, for example {@code "timeout"} or {@code "helper-reported"}. */
        public String category;
        /** Effective runtime behavior: {@code "ignore"} or {@code "failClosed"}. */
        public String failureMode;
        /** Primary plain-text explanation. */
        public String message;
        /** True when an earlier successful helper-derived layer still contributes. */
        public boolean previousPolicyRetained;
        /** Invocation reason, for example {@code "sessionStart"}. */
        public String reason;
        /** Managed authority that registered the helper ({@code "device"} or {@code "server"}), or null. */
        public String authority;
        /** Helper-defined error identifier, or null. */
        public String code;
        /** Field-specific findings ({@code path}, {@code message}), or null. */
        public List<Map<String, Object>> details;
        /** Secondary troubleshooting text, or null. */
        public String diagnostic;
        /** Validated help link ({@code url}, {@code label}), or null. */
        public Map<String, Object> helpLink;
        /** Helper-authored recovery guidance, or null. */
        public String remediation;
        /** {@code "warning"} or {@code "error"}, or null. */
        public String severity;

        public PolicyHelperFailure() {}

        /**
         * Reads the Policy Helper failure carried by a failed {@code session.create} or
         * {@code session.resume} request.
         *
         * @param error the exception thrown by create/resume
         * @return the failure, or null when the error does not carry one
         */
        public static PolicyHelperFailure fromError(Throwable error) {
            Throwable current = error;
            while (current != null) {
                if (current instanceof JsonRpcClient.JsonRpcException) {
                    Object data = ((JsonRpcClient.JsonRpcException) current).data;
                    return data instanceof Map ? fromMap(((Map<?, ?>) data).get("policyHelperFailure")) : null;
                }
                current = current.getCause();
            }
            return null;
        }

        /**
         * Parses a Policy Helper failure from its wire shape, for example the
         * {@code policyHelperFailure} member of a managed settings snapshot.
         *
         * @param value the wire value
         * @return the failure, or null when {@code value} is not an object
         */
        @SuppressWarnings("unchecked")
        public static PolicyHelperFailure fromMap(Object value) {
            if (!(value instanceof Map)) return null;
            Map<String, Object> map = (Map<String, Object>) value;
            PolicyHelperFailure failure = new PolicyHelperFailure();
            failure.category = stringOrNull(map.get("category"));
            failure.failureMode = stringOrNull(map.get("failureMode"));
            failure.message = stringOrNull(map.get("message"));
            failure.previousPolicyRetained = Boolean.TRUE.equals(map.get("previousPolicyRetained"));
            failure.reason = stringOrNull(map.get("reason"));
            failure.authority = stringOrNull(map.get("authority"));
            failure.code = stringOrNull(map.get("code"));
            if (map.get("details") instanceof List) {
                failure.details = new ArrayList<>();
                for (Object detail : (List<Object>) map.get("details")) {
                    if (detail instanceof Map) failure.details.add((Map<String, Object>) detail);
                }
            }
            failure.diagnostic = stringOrNull(map.get("diagnostic"));
            if (map.get("helpLink") instanceof Map) failure.helpLink = (Map<String, Object>) map.get("helpLink");
            failure.remediation = stringOrNull(map.get("remediation"));
            failure.severity = stringOrNull(map.get("severity"));
            return failure;
        }

        private static String stringOrNull(Object value) {
            return value instanceof String ? (String) value : null;
        }
    }

    /** Permission modes for {@code SessionConfig.permissionMode} and {@code CopilotSession.setPermissionMode}. */
    public static final class PermissionMode {
        /** Permission requests follow the normal approval flow. */
        public static final String MANUAL = "manual";
        /** Permission requests include an LLM safety recommendation. */
        public static final String ASSISTED = "assisted";
        /** Tool, path, and URL permission requests are approved automatically. */
        public static final String ALLOW_ALL = "allow-all";
        private PermissionMode() {}
    }

    /** Known values of {@link PermissionModeResult#reason}. */
    public static final class PermissionModeRefusalReason {
        /** Managed (enterprise) policy refused the requested mode. */
        public static final String MANAGED_POLICY = "managed_policy";
        private PermissionModeRefusalReason() {}
    }

    /** Result of a permission-mode change: whether it applied and the authoritative mode afterwards. */
    public static class PermissionModeResult {
        public boolean success;
        /** Authoritative permission mode after the request (see {@link PermissionMode}). */
        public String mode;
        /** Why the mode was refused, when known (see {@link PermissionModeRefusalReason}); null otherwise. */
        public String reason;

        public PermissionModeResult() {}

        /**
         * Parses a permission-mode result from its wire shape.
         *
         * @param map the wire map
         * @return the result
         */
        public static PermissionModeResult fromMap(Map<String, Object> map) {
            PermissionModeResult result = new PermissionModeResult();
            if (map == null) return result;
            result.success = Boolean.TRUE.equals(map.get("success"));
            Object mode = map.get("mode");
            Object reason = map.get("reason");
            result.mode = mode instanceof String ? (String) mode : null;
            result.reason = reason instanceof String ? (String) reason : null;
            return result;
        }
    }

    // --- v3.1.0 feature types (parity with the Microsoft Copilot SDK 2.0.3-preview.0) ---

    /**
     * Image support advertised by a {@code userInput.request}
     * ({@link UserInputRequest#imageAttachmentConstraints}). Absent (null) when the runtime does not
     * accept images in the answer.
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class UserInputImageAttachmentConstraints {
        /** MIME types accepted by the request. */
        public List<String> allowedMimeTypes = new ArrayList<>();
        /** Maximum decoded byte length of each image. */
        public long maxBytesPerImage;
        /** Maximum number of images accepted in one answer. */
        public int maxCount;

        public UserInputImageAttachmentConstraints() {}

        /**
         * Parses image attachment constraints from their wire shape.
         *
         * @param value the wire value
         * @return the constraints, or null when {@code value} is not an object
         */
        @SuppressWarnings("unchecked")
        public static UserInputImageAttachmentConstraints fromMap(Object value) {
            if (!(value instanceof Map)) return null;
            Map<String, Object> map = (Map<String, Object>) value;
            UserInputImageAttachmentConstraints constraints = new UserInputImageAttachmentConstraints();
            if (map.get("allowedMimeTypes") instanceof List) {
                for (Object mimeType : (List<Object>) map.get("allowedMimeTypes")) {
                    if (mimeType instanceof String) constraints.allowedMimeTypes.add((String) mimeType);
                }
            }
            Object maxBytesPerImage = map.get("maxBytesPerImage");
            Object maxCount = map.get("maxCount");
            constraints.maxBytesPerImage = maxBytesPerImage instanceof Number ? ((Number) maxBytesPerImage).longValue() : 0L;
            constraints.maxCount = maxCount instanceof Number ? ((Number) maxCount).intValue() : 0;
            return constraints;
        }
    }

    /**
     * An inline image attached to an ask_user answer ({@link UserInputResponse#attachments}). The
     * wire {@code type} is always {@code "image"}; the runtime enforces the advertised MIME types,
     * size and count limits.
     */
    public static class UserInputImageAttachment {
        /** The attachment's wire {@code type}, always {@code "image"}. */
        public static final String TYPE = "image";

        /** Image bytes encoded as standard padded base64, without a data URL prefix. */
        public String data;
        /** MIME type of the image, matching an advertised allowed MIME type. */
        public String mimeType;
        /** Optional description of the image; omitted when null. */
        public String description;
        /** Optional client-facing name of the image; omitted when null. */
        public String displayName;

        public UserInputImageAttachment() {}

        public UserInputImageAttachment(String data, String mimeType) {
            this.data = data;
            this.mimeType = mimeType;
        }

        public UserInputImageAttachment description(String description) { this.description = description; return this; }
        public UserInputImageAttachment displayName(String displayName) { this.displayName = displayName; return this; }

        /**
         * Maps this attachment onto its wire shape, omitting unset optional fields.
         *
         * @return the wire map
         */
        public Map<String, Object> toWire() {
            Map<String, Object> wire = new HashMap<>();
            wire.put("type", TYPE);
            wire.put("data", data);
            wire.put("mimeType", mimeType);
            if (description != null) wire.put("description", description);
            if (displayName != null) wire.put("displayName", displayName);
            return wire;
        }
    }

    /**
     * Result of {@code CopilotClient.mcpSecretsSave}: {@link #usedKeychain} is true when the OS
     * keychain stored the credential and false when the runtime used its file backend.
     */
    public static class McpSecretsSaveResult {
        public boolean usedKeychain;

        public McpSecretsSaveResult() {}

        /**
         * Parses an {@code mcp.secrets.save} result from its wire shape.
         *
         * @param map the wire map
         * @return the result
         */
        public static McpSecretsSaveResult fromMap(Map<String, Object> map) {
            McpSecretsSaveResult result = new McpSecretsSaveResult();
            if (map == null) return result;
            result.usedKeychain = Boolean.TRUE.equals(map.get("usedKeychain"));
            return result;
        }
    }

    /**
     * A user-scoped approval for one installed plugin extension. Pass {@link #installationKey}
     * back unchanged to {@code CopilotClient.extensionApprovalsRevoke}.
     */
    public static class PermissionsExtensionApproval {
        /** Stable extension identity, for example {@code "plugin:plugin-name:extension-name"}. */
        public String extensionName;
        /** Name of the plugin that provides the extension. */
        public String pluginName;
        /** Exact privileged capabilities the user approved. */
        public List<String> capabilities = new ArrayList<>();
        /** When the approval was granted (RFC 3339 timestamp). */
        public String grantedAt;
        /** Opaque installation identity; pass it back unchanged to revoke. */
        public String installationKey;
        /** Human-readable installation label, or null. */
        public String installationLabel;

        public PermissionsExtensionApproval() {}

        /**
         * Parses an extension approval from its wire shape.
         *
         * @param value the wire value
         * @return the approval, or null when {@code value} is not an object
         */
        @SuppressWarnings("unchecked")
        public static PermissionsExtensionApproval fromMap(Object value) {
            if (!(value instanceof Map)) return null;
            Map<String, Object> map = (Map<String, Object>) value;
            PermissionsExtensionApproval approval = new PermissionsExtensionApproval();
            approval.extensionName = stringOrNull(map.get("extensionName"));
            approval.pluginName = stringOrNull(map.get("pluginName"));
            if (map.get("capabilities") instanceof List) {
                for (Object capability : (List<Object>) map.get("capabilities")) {
                    if (capability instanceof String) approval.capabilities.add((String) capability);
                }
            }
            approval.grantedAt = stringOrNull(map.get("grantedAt"));
            approval.installationKey = stringOrNull(map.get("installationKey"));
            approval.installationLabel = stringOrNull(map.get("installationLabel"));
            return approval;
        }

        private static String stringOrNull(Object value) {
            return value instanceof String ? (String) value : null;
        }
    }

    /** Result of {@code CopilotClient.extensionApprovalsGrant}. */
    public static class PermissionsExtensionApprovalsGrantResult {
        /** Whether the pending request was eligible and persisted. */
        public boolean granted;
        /** The stored approval when it was persisted, or null. */
        public PermissionsExtensionApproval approval;

        public PermissionsExtensionApprovalsGrantResult() {}

        /**
         * Parses a {@code permissions.extensionApprovals.grant} result from its wire shape.
         *
         * @param map the wire map
         * @return the result
         */
        public static PermissionsExtensionApprovalsGrantResult fromMap(Map<String, Object> map) {
            PermissionsExtensionApprovalsGrantResult result = new PermissionsExtensionApprovalsGrantResult();
            if (map == null) return result;
            result.granted = Boolean.TRUE.equals(map.get("granted"));
            result.approval = PermissionsExtensionApproval.fromMap(map.get("approval"));
            return result;
        }
    }

    /** Result of {@code CopilotClient.extensionApprovalsList}: every user-scoped plugin extension approval. */
    public static class PermissionsExtensionApprovalsListResult {
        public List<PermissionsExtensionApproval> approvals = new ArrayList<>();

        public PermissionsExtensionApprovalsListResult() {}

        /**
         * Parses a {@code permissions.extensionApprovals.list} result from its wire shape.
         *
         * @param map the wire map
         * @return the result
         */
        @SuppressWarnings("unchecked")
        public static PermissionsExtensionApprovalsListResult fromMap(Map<String, Object> map) {
            PermissionsExtensionApprovalsListResult result = new PermissionsExtensionApprovalsListResult();
            if (map == null || !(map.get("approvals") instanceof List)) return result;
            for (Object value : (List<Object>) map.get("approvals")) {
                PermissionsExtensionApproval approval = PermissionsExtensionApproval.fromMap(value);
                if (approval != null) result.approvals.add(approval);
            }
            return result;
        }
    }

    /** Result of {@code CopilotClient.extensionApprovalsRevoke}. */
    public static class PermissionsExtensionApprovalsRevokeResult {
        /** Whether a matching approval was revoked. */
        public boolean revoked;

        public PermissionsExtensionApprovalsRevokeResult() {}

        /**
         * Parses a {@code permissions.extensionApprovals.revoke} result from its wire shape.
         *
         * @param map the wire map
         * @return the result
         */
        public static PermissionsExtensionApprovalsRevokeResult fromMap(Map<String, Object> map) {
            PermissionsExtensionApprovalsRevokeResult result = new PermissionsExtensionApprovalsRevokeResult();
            if (map == null) return result;
            result.revoked = Boolean.TRUE.equals(map.get("revoked"));
            return result;
        }
    }
}
