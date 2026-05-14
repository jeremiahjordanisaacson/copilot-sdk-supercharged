/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
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

    /** Per-session remote mode. "off" disables remote, "export" exports session events to Mission Control without enabling remote steering, "on" enables both export and remote steering. */
    public enum RemoteSessionMode {
        @JsonProperty("export") EXPORT,
        @JsonProperty("off") OFF,
        @JsonProperty("on") ON
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

    /** Request payload for an exit plan mode request from the server. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ExitPlanModeRequest {
        @JsonProperty("sessionId") public String sessionId;
    }

    /** Response payload for an exit plan mode request. */
    public static class ExitPlanModeResponse {
        public boolean approved;

        public ExitPlanModeResponse() {}
        public ExitPlanModeResponse(boolean approved) { this.approved = approved; }
    }

    /** Functional interface for exit plan mode handlers. */
    @FunctionalInterface
    public interface ExitPlanModeHandler {
        ExitPlanModeResponse handle(ExitPlanModeRequest request) throws Exception;
    }

    /** Trace context for distributed tracing. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class TraceContext {
        @JsonProperty("traceparent") public String traceparent;
        @JsonProperty("tracestate") public String tracestate;
    }

    /** Provider that returns a trace context for outbound requests. */
    @FunctionalInterface
    public interface TraceContextProvider {
        TraceContext getTraceContext() throws Exception;
    }

    /** Completion type for slash command input. */
    public enum SlashCommandInputCompletion {
        @JsonProperty("directory") DIRECTORY
    }

    /** Kind of slash command. */
    public enum SlashCommandKind {
        @JsonProperty("builtin") BUILTIN,
        @JsonProperty("client") CLIENT,
        @JsonProperty("skill") SKILL
    }

    /** Price category for the model picker. */
    public enum ModelPickerPriceCategory {
        @JsonProperty("high") HIGH,
        @JsonProperty("low") LOW,
        @JsonProperty("medium") MEDIUM,
        @JsonProperty("very_high") VERY_HIGH
    }

    /** Input configuration for a slash command. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SlashCommandInput {
        @JsonProperty("hint") public String hint;
        @JsonProperty("completion") public SlashCommandInputCompletion completion;
    }

    /** Information about a slash command. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SlashCommandInfo {
        @JsonProperty("allowDuringAgentExecution") public boolean allowDuringAgentExecution;
        @JsonProperty("description") public String description;
        @JsonProperty("kind") public SlashCommandKind kind;
        @JsonProperty("name") public String name;
        @JsonProperty("aliases") public List<String> aliases;
        @JsonProperty("experimental") public Boolean experimental;
        @JsonProperty("input") public SlashCommandInput input;
    }

    /** Request to invoke a command. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CommandsInvokeRequest {
        @JsonProperty("name") public String name;
        @JsonProperty("input") public String input;
    }

    /** Request to list commands. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CommandsListRequest {
        @JsonProperty("includeBuiltins") public Boolean includeBuiltins;
        @JsonProperty("includeClientCommands") public Boolean includeClientCommands;
        @JsonProperty("includeSkills") public Boolean includeSkills;
    }

    /** Token prices for model billing. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ModelBillingTokenPrices {
        @JsonProperty("batchSize") public Integer batchSize;
        @JsonProperty("cachePrice") public Integer cachePrice;
        @JsonProperty("inputPrice") public Integer inputPrice;
        @JsonProperty("outputPrice") public Integer outputPrice;
    }

    /** Model billing information. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ModelBilling {
        @JsonProperty("multiplier") public double multiplier;
        @JsonProperty("tokenPrices") public ModelBillingTokenPrices tokenPrices;
        @JsonProperty("pickerPriceCategory") public ModelPickerPriceCategory pickerPriceCategory;
    }

    /** Experimental: Diagnostics from loading skills. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class SkillsLoadDiagnostics {
        @JsonProperty("errors") public List<String> errors;
        @JsonProperty("warnings") public List<String> warnings;
    }

    /** Experimental: Request to enable remote mode for a session. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class RemoteEnableRequest {
        /** Per-session remote mode. */
        @JsonProperty("mode") public String mode;
    }

    /** Experimental: Result of enabling remote mode for a session. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class RemoteEnableResult {
        /** Whether remote steering is enabled. */
        @JsonProperty("remoteSteerable") public boolean remoteSteerable;
        /** Mission Control frontend URL for this session. */
        @JsonProperty("url") public String url;
    }
}
