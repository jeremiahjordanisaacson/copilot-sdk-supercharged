/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import groovy.transform.CompileStatic
import groovy.transform.ToString
import groovy.transform.EqualsAndHashCode

/**
 * Type definitions for the Groovy Copilot SDK.
 *
 * Uses Groovy idioms: {@code @ToString}, {@code @EqualsAndHashCode}, closures as handlers,
 * and map-friendly constructors.
 */
@CompileStatic
final class Types {
    private Types() {}

    /** Connection state of the client. */
    enum ConnectionState {
        DISCONNECTED, CONNECTING, CONNECTED, ERROR
    }

    /** Reasoning effort levels for models that support it. */
    enum ReasoningEffort {
        LOW, MEDIUM, HIGH, XHIGH
    }

    /** Result type from a tool execution. */
    enum ToolResultType {
        SUCCESS, FAILURE, REJECTED, DENIED
    }

    /** Structured result from a tool execution. */
    @ToString(includeNames = true)
    static class ToolResultObject {
        String textResultForLlm
        String resultType
        String error
        String sessionLog
        Map<String, Object> toolTelemetry = [:]
    }

    /** Information about a tool invocation. */
    @ToString(includeNames = true)
    static class ToolInvocation {
        String sessionId
        String toolCallId
        String toolName
        Object arguments

        ToolInvocation(String sessionId, String toolCallId, String toolName, Object arguments) {
            this.sessionId = sessionId
            this.toolCallId = toolCallId
            this.toolName = toolName
            this.arguments = arguments
        }
    }

    /** Tool definition for registering custom tools. */
    @ToString(includeNames = true, excludes = ['handler'])
    static class Tool {
        String name
        String description
        Map<String, Object> parameters
        Closure handler
        String defer
        /** Declares that repeating a call with the same arguments has no additional effect, so the runtime may retry it. */
        Boolean idempotent

        Tool(String name, String description, Map<String, Object> parameters, Closure handler) {
            this.name = name
            this.description = description
            this.parameters = parameters
            this.handler = handler
        }
    }

    /** Provider bearer-token request arguments (BYOK). */
    @ToString(includeNames = true)
    static class ProviderTokenArgs {
        String sessionId
    }

    /** Per-session resource limits. */
    @ToString(includeNames = true)
    static class SessionLimitsConfig {
        Double maxAiCredits
    }

    /** Long-term memory configuration for a session. */
    @ToString(includeNames = true, excludes = ['storeMemory'])
    static class MemoryConfiguration {
        boolean enabled = true
        /**
         * Closure receiving a {@link StoredMemory} for each memory the agent saves (sent to the
         * runtime as {@code state.hasMemoryStore}). Requires {@code enabled}. Never serialized.
         */
        Closure storeMemory
    }

    /** Permission request from the server. */
    @ToString(includeNames = true, excludes = ['details'])
    static class PermissionRequest {
        String kind
        String toolCallId
        /** The full permission request as sent by the runtime (kind-specific fields such as {@code fullCommandText}). */
        Map<String, Object> details
    }

    /** Permission request result. */
    @ToString(includeNames = true)
    static class PermissionRequestResult {
        String kind
        List<Object> rules
        /** Opaque context forwarded on permission replies. */
        Map<String, Object> decisionContext

        PermissionRequestResult() {}
        PermissionRequestResult(String kind) { this.kind = kind }
    }

    /** User input request from the agent. */
    @ToString(includeNames = true)
    static class UserInputRequest {
        String question
        List<String> choices
        Boolean allowFreeform
        /** ID of the pending request ({@code requestId}), when the runtime sends it. */
        String requestId
        /** Image limits; null when the runtime does not accept image attachments in the answer. */
        UserInputImageAttachmentConstraints imageAttachmentConstraints
        /** The request's opaque {@code _meta} object, forwarded unchanged; null when absent. */
        Map<String, Object> meta
    }

    /**
     * User input response. For an image-only answer, use an empty {@code answer} with
     * {@code wasFreeform} true.
     */
    @ToString(includeNames = true)
    static class UserInputResponse {
        String answer
        boolean wasFreeform
        /**
         * Images sent with the answer ({@code attachments}), omitted from the response when null.
         * Only valid when the request advertised {@code imageAttachmentConstraints}; the runtime
         * enforces the limits.
         */
        List<UserInputImageAttachment> attachments

        UserInputResponse(String answer, boolean wasFreeform) {
            this.answer = answer
            this.wasFreeform = wasFreeform
        }

        UserInputResponse(String answer, boolean wasFreeform, List<UserInputImageAttachment> attachments) {
            this(answer, wasFreeform)
            this.attachments = attachments
        }

        /** Wire shape: {@code answer}, {@code wasFreeform}, and {@code attachments} when set. */
        Map<String, Object> toMap() {
            Map<String, Object> map = [answer: answer, wasFreeform: wasFreeform] as Map<String, Object>
            if (attachments != null) {
                map.attachments = attachments.collect { UserInputImageAttachment attachment -> attachment.toMap() }
            }
            map
        }
    }

    /** Ping response. */
    @ToString(includeNames = true)
    static class PingResponse {
        String message
        String timestamp
        Integer protocolVersion
    }

    /** Status response. */
    @ToString(includeNames = true)
    static class GetStatusResponse {
        String version
        int protocolVersion
    }

    /** Auth status response. */
    @ToString(includeNames = true)
    static class GetAuthStatusResponse {
        boolean isAuthenticated
        String authType
        String host
        String login
        String statusMessage
    }

    /** Model capabilities. */
    @ToString(includeNames = true)
    static class ModelCapabilities {
        ModelSupports supports
        ModelLimits limits

        /**
         * Converts these capabilities to the {@code modelCapabilities} override wire shape. Limits
         * use the runtime's snake_case keys ({@code max_prompt_tokens},
         * {@code max_context_window_tokens}, {@code max_output_tokens}).
         *
         * @return the wire map
         */
        Map<String, Object> toWire() {
            Map<String, Object> wire = [:]
            if (supports != null) {
                wire.supports = [vision: supports.vision, reasoningEffort: supports.reasoningEffort] as Map<String, Object>
            }
            if (limits != null) {
                Map<String, Object> wireLimits = [:]
                if (limits.maxPromptTokens != null) wireLimits.max_prompt_tokens = limits.maxPromptTokens
                if (limits.maxContextWindowTokens > 0) wireLimits.max_context_window_tokens = limits.maxContextWindowTokens
                if (limits.maxOutputTokens != null) wireLimits.max_output_tokens = limits.maxOutputTokens
                if (!wireLimits.isEmpty()) wire.limits = wireLimits
            }
            wire
        }

        @ToString(includeNames = true)
        static class ModelSupports {
            boolean vision
            boolean reasoningEffort
        }

        @ToString(includeNames = true)
        static class ModelLimits {
            Integer maxPromptTokens
            int maxContextWindowTokens
            /** Maximum output tokens (wire key {@code max_output_tokens}). */
            Integer maxOutputTokens
        }
    }

    /** Model information. */
    @ToString(includeNames = true)
    static class ModelInfo {
        String id
        String name
        ModelCapabilities capabilities
    }

    /** Session metadata. */
    @ToString(includeNames = true)
    static class SessionMetadata {
        String sessionId
        String startTime
        String modifiedTime
        String summary
        boolean isRemote
    }

    /** Session lifecycle event. */
    @ToString(includeNames = true)
    static class SessionLifecycleEvent {
        String type
        String sessionId
        Map<String, Object> metadata
    }

    /** Session event (generic). */
    @ToString(includeNames = true)
    static class SessionEvent {
        String type
        Map<String, Object> data = [:]
        String id
        String timestamp
        String parentId
        String agentId
        Boolean ephemeral
    }

    /** Provider configuration for BYOK. */
    @ToString(includeNames = true)
    static class ProviderConfig {
        String type
        String baseUrl
        String apiKey
        String bearerToken
        String wireApi
        /** Product serving the model, reported in telemetry (see {@link ModelProvider}). */
        String modelProvider
        /** Command run before each provider request; its stdout is used as the API key. */
        String apiKeyCommand
        /** Transport: {@code 'http'} (default) or {@code 'websockets'} (Responses API only). */
        String transport
        /** Azure-specific options, for example {@code apiVersion}. */
        Map<String, Object> azure
        /** Custom HTTP headers sent with every request to the provider. */
        Map<String, String> headers
        /** Well-known base model ID used for capability lookup (model-specific). */
        String modelId
        /** Model or deployment name sent to the provider API (model-specific). */
        String wireModel
        /** Prompt token limit of the session model (model-specific). */
        Integer maxPromptTokens
        /** Context window of the session model (model-specific). */
        Integer maxContextWindowTokens
        /** Output token limit of the session model (model-specific). */
        Integer maxOutputTokens

        /**
         * Converts this provider to its wire shape, omitting unset fields. The model-specific fields
         * become an {@code inference.models} entry for the session model when the SDK translates the
         * provider to protocol 4.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [:]
            if (type != null) wire.type = type
            if (baseUrl != null) wire.baseUrl = baseUrl
            if (apiKey != null) wire.apiKey = apiKey
            if (bearerToken != null) wire.bearerToken = bearerToken
            if (wireApi != null) wire.wireApi = wireApi
            if (modelProvider != null) wire.modelProvider = modelProvider
            if (apiKeyCommand != null) wire.apiKeyCommand = apiKeyCommand
            if (transport != null) wire.transport = transport
            if (azure != null) wire.azure = azure
            if (headers != null) wire.headers = headers
            if (modelId != null) wire.modelId = modelId
            if (wireModel != null) wire.wireModel = wireModel
            if (maxPromptTokens != null) wire.maxPromptTokens = maxPromptTokens
            if (maxContextWindowTokens != null) wire.maxContextWindowTokens = maxContextWindowTokens
            if (maxOutputTokens != null) wire.maxOutputTokens = maxOutputTokens
            wire
        }
    }

    /** MCP server configuration. */
    @ToString(includeNames = true)
    static class MCPServerConfig {
        List<String> tools
        String type
        Integer timeout
        String command
        List<String> args
        Map<String, String> env
        String cwd
        String url
        Map<String, String> headers
    }

    /** Custom agent configuration. */
    @ToString(includeNames = true)
    static class CustomAgentConfig {
        String name
        String displayName
        String description
        List<String> tools
        String prompt
        Map<String, MCPServerConfig> mcpServers
        Boolean infer
        List<String> skills
        /** JSON schema declaring a factory's arguments. */
        Map<String, Object> argsSchema
    }

    /** Infinite session configuration. */
    @ToString(includeNames = true)
    static class InfiniteSessionConfig {
        Boolean enabled
        Double backgroundCompactionThreshold
        Double bufferExhaustionThreshold
    }

    /** Image options for generation requests. */
    @ToString(includeNames = true)
    static class ImageOptions {
        String size
        String quality
        String style

        ImageOptions() {}
        ImageOptions(String size, String quality, String style) {
            this.size = size
            this.quality = quality
            this.style = style
        }
    }

    /** Context passed to a command handler when a command is executed. */
    @ToString(includeNames = true)
    static class CommandContext {
        String sessionId
        String command
        String commandName
        String args
    }

    /** Definition of a slash command registered with the session. */
    @ToString(includeNames = true, excludes = ['handler'])
    static class CommandDefinition {
        String name
        String description
        Closure handler

        CommandDefinition() {}
        CommandDefinition(String name, String description, Closure handler) {
            this.name = name
            this.description = description
            this.handler = handler
        }
    }

    /** Context for an elicitation handler invocation. */
    @ToString(includeNames = true)
    static class ElicitationContext {
        String sessionId
        String message
        Object requestedSchema
        String mode
        String elicitationSource
        String url
        /** ID of the pending elicitation request ({@code requestId}). */
        String requestId
        /** The request's opaque {@code _meta} object (negotiated extensions), forwarded unchanged; null when absent. */
        Map<String, Object> meta
    }

    /** Result returned from an elicitation request. */
    @ToString(includeNames = true)
    static class ElicitationResult {
        String action
        Map<String, Object> content
        /**
         * Private negotiated extensions, sent back unchanged as {@code _meta} when set. Never put
         * image bytes in primitive {@code content}.
         */
        Map<String, Object> meta

        ElicitationResult() {}
        ElicitationResult(String action, Map<String, Object> content = null) {
            this.action = action
            this.content = content
        }
    }

    /** Configuration for a custom session filesystem provider. */
    @ToString(includeNames = true)
    static class SessionFsConfig {
        String initialCwd
        String sessionStatePath
        String conventions

        SessionFsConfig() {}
        SessionFsConfig(String initialCwd, String sessionStatePath, String conventions) {
            this.initialCwd = initialCwd
            this.sessionStatePath = sessionStatePath
            this.conventions = conventions
        }
    }

    /** Error encountered during stop(). */
    @ToString(includeNames = true)
    static class StopError {
        String message
        StopError(String message) { this.message = message }
    }

    /** Request payload for an exit plan mode request from the server. */
    @ToString(includeNames = true)
    static class ExitPlanModeRequest {
        String sessionId
    }

    /** Response payload for an exit plan mode request. */
    @ToString(includeNames = true)
    static class ExitPlanModeResponse {
        boolean approved

        ExitPlanModeResponse() {}
        ExitPlanModeResponse(boolean approved) { this.approved = approved }
    }

    /** Trace context for distributed tracing. */
    @ToString(includeNames = true)
    static class TraceContext {
        String traceparent
        String tracestate
    }

    /** Completion type for slash command input. */
    enum SlashCommandInputCompletion {
        DIRECTORY
    }

    /** Kind of slash command. */
    enum SlashCommandKind {
        BUILTIN, CLIENT, SKILL
    }

    /** Price category for the model picker. */
    enum ModelPickerPriceCategory {
        HIGH, LOW, MEDIUM, VERY_HIGH
    }

    /** Input configuration for a slash command. */
    @ToString(includeNames = true)
    static class SlashCommandInput {
        String hint
        SlashCommandInputCompletion completion
    }

    /** Information about a slash command. */
    @ToString(includeNames = true)
    static class SlashCommandInfo {
        boolean allowDuringAgentExecution
        String description
        SlashCommandKind kind
        String name
        List<String> aliases
        Boolean experimental
        SlashCommandInput input
    }

    /** Request to invoke a command. */
    @ToString(includeNames = true)
    static class CommandsInvokeRequest {
        String name
        String input
    }

    /** Request to list commands. */
    @ToString(includeNames = true)
    static class CommandsListRequest {
        Boolean includeBuiltins
        Boolean includeClientCommands
        Boolean includeSkills
    }

    /** Token prices for model billing. */
    @ToString(includeNames = true)
    static class ModelBillingTokenPrices {
        Integer batchSize
        Integer cachePrice
        Integer inputPrice
        Integer outputPrice
    }

    /** Model billing information. */
    @ToString(includeNames = true)
    static class ModelBilling {
        double multiplier
        ModelBillingTokenPrices tokenPrices
        ModelPickerPriceCategory pickerPriceCategory
    }

    /** Experimental: Diagnostics from loading skills. */
    @ToString(includeNames = true)
    static class SkillsLoadDiagnostics {
        List<String> errors
        List<String> warnings
    }

    /** Per-session remote mode. "off" disables remote, "export" exports session events to Mission Control without enabling remote steering, "on" enables both export and remote steering. */
    enum RemoteSessionMode {
        EXPORT, OFF, ON
    }

    /** Experimental: Request to enable remote mode for a session. */
    @ToString(includeNames = true)
    static class RemoteEnableRequest {
        String mode
    }

    /** Experimental: Result of enabling remote mode for a session. */
    @ToString(includeNames = true)
    static class RemoteEnableResult {
        boolean remoteSteerable
        String url
    }

    /** Response format for image generation responses. */
    enum ResponseFormat {
        TEXT, IMAGE, JSON_OBJECT
    }

    /** Override operation for a single system prompt section. */
    @ToString(includeNames = true)
    static class SectionOverride {
        String action
        String content
    }

    /** System message configuration in append mode. */
    @ToString(includeNames = true)
    static class SystemMessageAppendConfig {
        String mode = 'append'
        String content
    }

    /** System message configuration in replace mode. */
    @ToString(includeNames = true)
    static class SystemMessageReplaceConfig {
        String mode = 'replace'
        String content
    }

    /** System message configuration in customize mode. */
    @ToString(includeNames = true)
    static class SystemMessageCustomizeConfig {
        String mode = 'customize'
        Map<String, SectionOverride> sections
        String content
    }

    /** Declaration for a canvas exposed by an extension. */
    @ToString(includeNames = true)
    static class CanvasDeclaration {
        String id
        String displayName
        String description
        Map<String, Object> inputSchema
        List<CanvasAction> actions
    }

    /** Action exposed by a canvas. */
    @ToString(includeNames = true)
    static class CanvasAction {
        String name
        String description
        Map<String, Object> inputSchema
    }

    /** Response returned when opening a canvas. */
    @ToString(includeNames = true)
    static class CanvasOpenResponse {
        String url
        String title
        String status
    }

    /** Host capabilities available to canvases. */
    @ToString(includeNames = true)
    static class CanvasHostCapabilities {
        Boolean canvases = false
    }

    /** Host context available to canvases. */
    @ToString(includeNames = true)
    static class CanvasHostContext {
        CanvasHostCapabilities capabilities
    }

    /** Context passed when opening a canvas. */
    @ToString(includeNames = true)
    static class CanvasOpenContext {
        String sessionId
        String extensionId
        String canvasId
        String instanceId
        Object input
        CanvasHostContext host
    }

    /** Context passed when invoking a canvas action. */
    @ToString(includeNames = true)
    static class CanvasActionContext {
        String sessionId
        String extensionId
        String canvasId
        String instanceId
        String actionName
        Object input
        CanvasHostContext host
    }

    /** Lifecycle context for a canvas instance. */
    @ToString(includeNames = true)
    static class CanvasLifecycleContext {
        String sessionId
        String extensionId
        String canvasId
        String instanceId
        CanvasHostContext host
    }

    /** Repository context for a cloud session. */
    @ToString(includeNames = true)
    static class CloudSessionRepository {
        String owner
        String name
        String branch
    }

    /** Options for creating a cloud session. */
    @ToString(includeNames = true)
    static class CloudSessionOptions {
        CloudSessionRepository repository
    }

    // --- v2.6.0 feature types ---

    /**
     * Identity of the integrating application, declared once on the {@code connect} handshake so
     * runtime telemetry is attributed to a consistent surface. All fields are optional: empty
     * fields are dropped and an all-empty identity skips the handshake.
     */
    @ToString(includeNames = true)
    static class ClientInfo {
        /** Name of the application using the SDK (wire {@code editorName}). */
        String applicationName
        /** Version of the application using the SDK (wire {@code editorVersion}). */
        String applicationVersion
        /** Name of an integration within the application, such as a plugin (wire {@code extensionName}). */
        String integrationName
        /** Version of the integration (wire {@code extensionVersion}). */
        String integrationVersion

        /**
         * Maps this identity onto the {@code connect} wire shape, dropping empty fields.
         *
         * @return the wire map, or null when every field is empty
         */
        Map<String, Object> toWire() {
            Map<String, Object> wire = [:]
            if (applicationName) wire.editorName = applicationName
            if (applicationVersion) wire.editorVersion = applicationVersion
            if (integrationName) wire.extensionName = integrationName
            if (integrationVersion) wire.extensionVersion = integrationVersion
            wire.isEmpty() ? null : wire
        }
    }

    /** Auto-model routing preferences for the {@code autoTier} option and {@code CopilotSession.setAutoTier} (experimental). */
    static final class AutoTier {
        static final String FAST = 'fast'
        static final String EFFICIENCY = 'efficiency'
        static final String BALANCE = 'balance'
        static final String INTELLIGENCE = 'intelligence'

        private AutoTier() {}
    }

    /** Model-facing shapes of the built-in {@code ask_user} tool, for the {@code askUserVariant} option. */
    static final class AskUserVariant {
        static final String LEGACY = 'legacy'
        static final String ELICITATION = 'elicitation'

        private AskUserVariant() {}
    }

    /** Working-directory trust scopes for the create-only {@code trustWorkingDirectory} option. */
    static final class WorkingDirectoryTrust {
        /** Trust applies only to this session. */
        static final String SESSION = 'session'
        /** Trust is persisted for later sessions. */
        static final String USER = 'user'

        private WorkingDirectoryTrust() {}
    }

    /** Message provenance values for the {@code source} send option. */
    static final class MessageSource {
        static final String USER = 'user'
        static final String SYSTEM = 'system'

        private MessageSource() {}

        /**
         * Builds the source value for a message originating from an identified agent.
         *
         * @param agentId the agent identifier
         * @return {@code 'agent-' + agentId}
         */
        static String agent(String agentId) {
            'agent-' + agentId
        }
    }

    /** Products serving a BYOK model, for the {@code modelProvider} provider field. */
    static final class ModelProvider {
        static final String OPENAI = 'openai'
        static final String ANTHROPIC = 'anthropic'
        static final String AZURE_OPENAI = 'azure_openai'
        static final String OLLAMA = 'ollama'
        static final String LM_STUDIO = 'lm_studio'
        static final String FOUNDRY_LOCAL = 'foundry_local'
        static final String LLAMA_CPP = 'llama_cpp'

        private ModelProvider() {}
    }

    /** Sandbox configuration applied before MCP servers or session tools start (experimental). */
    @ToString(includeNames = true)
    static class SandboxConfig {
        /** Whether sandboxing is enabled for the session. */
        boolean enabled
        /** User-managed sandbox policy fragment merged into the auto-discovered base policy. */
        Map<String, Object> userPolicy
        /** Whether to auto-add the current working directory to the read-write paths (runtime default: true). */
        Boolean addCurrentWorkingDirectory
        /** Whether MCP servers the session launches are confined by the sandbox (runtime default: true). */
        Boolean sandboxMcpServers
        /** Whether language servers the session launches are confined by the sandbox (runtime default: true). */
        Boolean sandboxLspServers
        /** Whether extension processes the session launches are confined by the sandbox (runtime default: true). */
        Boolean sandboxExtensions
        /**
         * Opt-in: lets the agent request that an individual command run outside the sandbox, which
         * the host approves or denies through the normal permission flow (default: false).
         */
        Boolean allowBypass

        /**
         * Converts this configuration to its wire shape, omitting unset optional fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [enabled: enabled] as Map<String, Object>
            if (userPolicy != null) wire.userPolicy = userPolicy
            if (addCurrentWorkingDirectory != null) wire.addCurrentWorkingDirectory = addCurrentWorkingDirectory
            if (sandboxMcpServers != null) wire.sandboxMcpServers = sandboxMcpServers
            if (sandboxLspServers != null) wire.sandboxLspServers = sandboxLspServers
            if (sandboxExtensions != null) wire.sandboxExtensions = sandboxExtensions
            if (allowBypass != null) wire.allowBypass = allowBypass
            wire
        }
    }

    /** Non-persisted session-level opt-in to image generation (experimental). */
    @ToString(includeNames = true)
    static class ImageGenerationConfig {
        /** Opt in to image generation through an authorized Copilot image model. */
        Boolean enabled
        /** Request a visible "Made with AI" label on generated and edited images. */
        Boolean watermark
        /** Save generated images under the session-state {@code images} folder (runtime default: true). */
        Boolean saveToDisk

        /**
         * Converts this configuration to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [:]
            if (enabled != null) wire.enabled = enabled
            if (watermark != null) wire.watermark = watermark
            if (saveToDisk != null) wire.saveToDisk = saveToDisk
            wire
        }
    }

    /** Autopilot continuation policy (experimental). */
    @ToString(includeNames = true)
    static class AutopilotOptions {
        /** Enable runtime-owned automatic continuation while in Autopilot mode (runtime default: true). */
        Boolean continuationEnabled
        /** Maximum automatic continuations outside an active objective (runtime default: 5; 0 allows none). */
        Integer maxContinues

        /**
         * Converts these options to their wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [:]
            if (continuationEnabled != null) wire.continuationEnabled = continuationEnabled
            if (maxContinues != null) wire.maxContinues = maxContinues
            wire
        }
    }

    /** Arguments passed to a {@code gitHubTokenProvider} closure. */
    @ToString(includeNames = true)
    static class GitHubTokenProviderArgs {
        /** Effective GitHub host for which a token is required. */
        String host
        /** Session receiving the token, or null before a session ID is assigned. */
        String sessionId
        /** Why a token is requested: {@code 'initial'} or {@code 'refresh'}. */
        String reason
    }

    /** Tagged token or cancellation returned by a {@code gitHubTokenProvider} closure. */
    @ToString(includeNames = true, excludes = ['accessToken'])
    static class GitHubTokenProviderResult {
        /** {@code 'token'} or {@code 'cancelled'}. */
        String kind
        /** GitHub access token (token results only). */
        String accessToken
        /** OAuth token type; defaults to bearer when omitted. */
        String tokenType
        /** Remaining token lifetime in seconds (required for token results). */
        Long expiresIn

        /**
         * Creates a token result.
         *
         * @param accessToken the GitHub access token
         * @param expiresIn remaining lifetime in seconds
         * @param tokenType the OAuth token type, or null for bearer
         * @return the token result
         */
        static GitHubTokenProviderResult token(String accessToken, long expiresIn, String tokenType = null) {
            GitHubTokenProviderResult result = new GitHubTokenProviderResult()
            result.kind = 'token'
            result.accessToken = accessToken
            result.expiresIn = expiresIn
            result.tokenType = tokenType
            result
        }

        /**
         * Creates a cancellation result.
         *
         * @return the cancellation result
         */
        static GitHubTokenProviderResult cancelled() {
            GitHubTokenProviderResult result = new GitHubTokenProviderResult()
            result.kind = 'cancelled'
            result
        }

        /**
         * Converts this result to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [kind: kind] as Map<String, Object>
            if (accessToken != null) wire.accessToken = accessToken
            if (tokenType != null) wire.tokenType = tokenType
            if (expiresIn != null) wire.expiresIn = expiresIn
            wire
        }
    }

    // --- v3.0.0 feature types (SDK protocol 4) ---

    /** Client modes for the {@code mode} client option. */
    static final class ClientMode {
        /** The default: the full Copilot coding agent, identical to earlier SDK behavior. */
        static final String CODING_AGENT = 'coding-agent'
        /**
         * Every session must choose its tools ({@code availableTools}), and runtime features such as
         * telemetry, repository hooks, and cross-session memory default to off.
         */
        static final String EMPTY = 'empty'
        /** Legacy alias of {@link #CODING_AGENT}. */
        static final String COPILOT_CLI = 'copilot-cli'

        private ClientMode() {}
    }

    /** Copilot API (CAPI) provider options, sent as {@code github.capi}. */
    @ToString(includeNames = true)
    static class CapiConfig {
        /** Enables Copilot API models; set false to keep a BYOK-only model catalog. */
        Boolean enabled
        /** Routing preference used when the session model is {@code auto} (see {@link AutoTier}). */
        String autoTier
        /** Whether to use the WebSocket transport for the CAPI Responses API. */
        Boolean enableWebSocketResponses

        /**
         * Converts these options to their wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [:]
            if (enabled != null) wire.enabled = enabled
            if (autoTier != null) wire.autoTier = autoTier
            if (enableWebSocketResponses != null) wire.enableWebSocketResponses = enableWebSocketResponses
            wire
        }
    }

    /**
     * A named BYOK provider connection, sent in {@code inference.providers}. Model definitions
     * ({@link ProviderModelConfig}) reference it by {@code name}.
     */
    @ToString(includeNames = true, excludes = ['apiKey', 'bearerToken'])
    static class NamedProviderConfig {
        /** Stable connection identifier; must not contain {@code /}. */
        String name
        /** Provider type: {@code 'openai'} (default), {@code 'azure'}, or {@code 'anthropic'}. */
        String type
        /** API endpoint URL. */
        String baseUrl
        /** API key; optional for local providers. */
        String apiKey
        /** Command run before each request whose stdout is used as the API key. */
        String apiKeyCommand
        /** Bearer token sent as the Authorization header; takes precedence over the API key. */
        String bearerToken
        /** Wire API format: {@code 'completions'} (default) or {@code 'responses'}. */
        String wireApi
        /** Transport: {@code 'http'} (default) or {@code 'websockets'} (Responses API only). */
        String transport
        /** Azure-specific options, for example {@code apiVersion}. */
        Map<String, Object> azure
        /** Custom HTTP headers sent with every request to the provider. */
        Map<String, String> headers
        /** Product serving the models, reported in telemetry (see {@link ModelProvider}). */
        String modelProvider

        /**
         * Converts this connection to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [:]
            if (name != null) wire.name = name
            if (type != null) wire.type = type
            if (baseUrl != null) wire.baseUrl = baseUrl
            if (apiKey != null) wire.apiKey = apiKey
            if (apiKeyCommand != null) wire.apiKeyCommand = apiKeyCommand
            if (bearerToken != null) wire.bearerToken = bearerToken
            if (wireApi != null) wire.wireApi = wireApi
            if (transport != null) wire.transport = transport
            if (azure != null) wire.azure = azure
            if (headers != null) wire.headers = headers
            if (modelProvider != null) wire.modelProvider = modelProvider
            wire
        }
    }

    /**
     * A BYOK model definition served by a {@link NamedProviderConfig}, sent in
     * {@code inference.models}. Select it with the canonical {@code 'provider/id'} model ID.
     */
    @ToString(includeNames = true)
    static class ProviderModelConfig {
        /** Name of the {@link NamedProviderConfig} that serves this model. */
        String provider
        /** Provider-local model ID, unique within its provider. */
        String id
        /** Well-known base model ID used for capability lookup; defaults to {@code id}. */
        String modelId
        /** Model or deployment name sent to the provider API; defaults to {@code id}. */
        String wireModel
        /** Display name; defaults to {@code 'provider/id'}. */
        String name
        Integer maxPromptTokens
        Integer maxContextWindowTokens
        Integer maxOutputTokens
        /** Capability overrides for the model (same shape as the {@code modelCapabilities} option). */
        Map<String, Object> capabilities

        /**
         * Converts this model definition to its wire shape, omitting unset fields.
         *
         * @return the wire map
         */
        Map<String, Object> toMap() {
            Map<String, Object> wire = [:]
            if (provider != null) wire.provider = provider
            if (id != null) wire.id = id
            if (modelId != null) wire.modelId = modelId
            if (wireModel != null) wire.wireModel = wireModel
            if (name != null) wire.name = name
            if (maxPromptTokens != null) wire.maxPromptTokens = maxPromptTokens
            if (maxContextWindowTokens != null) wire.maxContextWindowTokens = maxContextWindowTokens
            if (maxOutputTokens != null) wire.maxOutputTokens = maxOutputTokens
            if (capabilities != null) wire.capabilities = capabilities
            wire
        }
    }

    /** A program's request for a secret or confirmation while a shell command runs. */
    @ToString(includeNames = true)
    static class ShellAuthenticationRequest {
        /** Opaque request ID, unique within the session. */
        String requestId
        /** Program that asked, for example {@code 'sudo'} or {@code 'ssh'}. */
        String program
        /** Prompt text the program printed. Treat it as untrusted. */
        String prompt
        /** {@code 'secret'}, {@code 'text'}, or {@code 'confirm'}. */
        String inputKind
        /** Command line that triggered the prompt (may be empty). */
        String command
        /** Working directory of the command (may be empty). */
        String cwd
    }

    /** Answer to a {@link ShellAuthenticationRequest}. */
    @ToString(includeNames = true, excludes = ['value'])
    static class ShellAuthenticationResult {
        /** {@code 'accept'} or {@code 'cancel'}. */
        String action
        /** The value typed for the program (accept only). Never logged. */
        String value

        /**
         * Creates an accept answer carrying the value to send to the program.
         *
         * @param value the secret, text, or confirmation value
         * @return the answer
         */
        static ShellAuthenticationResult accept(String value) {
            ShellAuthenticationResult result = new ShellAuthenticationResult()
            result.action = 'accept'
            result.value = value
            result
        }

        /**
         * Creates a cancel answer.
         *
         * @return the answer
         */
        static ShellAuthenticationResult cancel() {
            ShellAuthenticationResult result = new ShellAuthenticationResult()
            result.action = 'cancel'
            result
        }
    }

    /** A memory the agent asked to store, after validation, secret filtering, and approval. */
    @ToString(includeNames = true)
    static class StoredMemory {
        /** ID of the session that stored the memory. */
        String sessionId
        /** Short topic the memory is about. */
        String subject
        /** The fact to remember. */
        String fact
        /** Sources supporting the fact, such as file paths with line numbers. */
        String citations
        /** Why the agent considers the fact worth remembering. */
        String reason
    }

    /** Network traffic scopes for {@link NetworkConfig#scope}. */
    static final class NetworkScope {
        /** Requests made for sessions this client creates or resumes (the default). */
        static final String CONNECTION = 'connection'
        /** Also the runtime's shared background traffic; only one connection can hold it. */
        static final String RUNTIME = 'runtime'

        private NetworkScope() {}
    }

    /** Arguments of a {@code network.resolveProxy} request. */
    @ToString(includeNames = true)
    static class NetworkResolveProxyRequest {
        /** Request target as a PAC script sees it. */
        String url
    }

    /** Arguments of a {@code network.authenticateNegotiate} request. */
    @ToString(includeNames = true)
    static class NetworkAuthenticateNegotiateRequest {
        /** Service principal in {@code service/host[@REALM]} form. */
        String spn
    }

    /** Arguments of a {@code network.authenticateBasic} request. */
    @ToString(includeNames = true)
    static class NetworkAuthenticateBasicRequest {
        /** Proxy that challenged, as {@code http://host:port} or {@code https://host:port}. */
        String proxy
        /** Realm named by the proxy, or null. Untrusted text. */
        String realm
        /** True when the proxy rejected the credentials sent last. */
        boolean previousRejected
    }

    /** Proxy credentials returned by an {@code authenticateBasic} closure. Never logged. */
    @ToString(includeNames = true, excludes = ['password'])
    static class NetworkBasicCredentials {
        String username
        String password

        NetworkBasicCredentials() {}

        NetworkBasicCredentials(String username, String password) {
            this.username = username
            this.password = password
        }
    }

    /**
     * Network callbacks through which the host takes over parts of the runtime's networking (proxy
     * resolution and proxy authentication). Registered with {@code network.setProvider} after the
     * client connects when at least one callback is set. Each callback is a closure:
     * {@code resolveProxy} receives a {@link NetworkResolveProxyRequest} and returns a PAC-style
     * proxy list (for example {@code 'PROXY proxy:8080; DIRECT'}) or null;
     * {@code authenticateNegotiate} receives a {@link NetworkAuthenticateNegotiateRequest} and
     * returns a base64 SPNEGO token or null; {@code authenticateBasic} receives a
     * {@link NetworkAuthenticateBasicRequest} and returns {@link NetworkBasicCredentials} (or a map
     * with {@code username}/{@code password}) or null.
     */
    @ToString(includeNames = true, includes = ['scope'])
    static class NetworkConfig {
        /** Traffic the callbacks serve (see {@link NetworkScope}); the runtime default is {@code 'connection'}. */
        String scope
        Closure resolveProxy
        Closure authenticateNegotiate
        Closure authenticateBasic

        /**
         * Returns whether any callback is configured.
         *
         * @return true if at least one callback is set
         */
        boolean hasCallbacks() {
            resolveProxy != null || authenticateNegotiate != null || authenticateBasic != null
        }
    }

    /** Data of a {@code tool.shell_output} session event: live, append-only output of a running shell tool call. */
    @ToString(includeNames = true)
    static class ToolShellOutputData {
        /** The session event type carrying this data. */
        static final String EVENT_TYPE = 'tool.shell_output'

        /** Tool call that owns this output. */
        String toolCallId
        /** New output to append; chunks need not contain complete lines. */
        String text
        /** Zero-based publication sequence across all streams of the tool call. */
        long sequence
        /** {@code 'stdout'}, {@code 'stderr'}, or null (stdout). */
        String stream

        /**
         * Reads the shell output data of a {@code tool.shell_output} event.
         *
         * @param event the session event
         * @return the data, or null when the event is not a shell output event
         */
        static ToolShellOutputData fromEvent(SessionEvent event) {
            if (event == null || event.type != EVENT_TYPE || event.data == null) return null
            Map<String, Object> data = event.data
            ToolShellOutputData output = new ToolShellOutputData()
            Object toolCallId = data.get('toolCallId')
            Object text = data.get('text')
            Object sequence = data.get('sequence')
            Object stream = data.get('stream')
            output.toolCallId = toolCallId instanceof String ? (String) toolCallId : null
            output.text = text instanceof String ? (String) text : ''
            output.sequence = sequence instanceof Number ? ((Number) sequence).longValue() : 0L
            output.stream = stream instanceof String ? (String) stream : null
            output
        }
    }

    /**
     * The latest unresolved Policy Helper failure. Carried by {@code session.create} and
     * {@code session.resume} errors as {@code error.data.policyHelperFailure} and by the managed
     * settings snapshot as {@code policyHelperFailure}. Helper-authored text is untrusted plain text.
     */
    @ToString(includeNames = true)
    static class PolicyHelperFailure {
        /** Stable runtime classification, for example {@code 'timeout'} or {@code 'helper-reported'}. */
        String category
        /** Effective runtime behavior: {@code 'ignore'} or {@code 'failClosed'}. */
        String failureMode
        /** Primary plain-text explanation. */
        String message
        /** True when an earlier successful helper-derived layer still contributes. */
        boolean previousPolicyRetained
        /** Invocation reason, for example {@code 'sessionStart'}. */
        String reason
        /** Managed authority that registered the helper ({@code 'device'} or {@code 'server'}), or null. */
        String authority
        /** Helper-defined error identifier, or null. */
        String code
        /** Field-specific findings ({@code path}, {@code message}), or null. */
        List<Map<String, Object>> details
        /** Secondary troubleshooting text, or null. */
        String diagnostic
        /** Validated help link ({@code url}, {@code label}), or null. */
        Map<String, Object> helpLink
        /** Helper-authored recovery guidance, or null. */
        String remediation
        /** {@code 'warning'} or {@code 'error'}, or null. */
        String severity

        /**
         * Reads the Policy Helper failure carried by a failed {@code session.create} or
         * {@code session.resume} request.
         *
         * @param error the exception thrown by create/resume
         * @return the failure, or null when the error does not carry one
         */
        static PolicyHelperFailure fromError(Throwable error) {
            Throwable current = error
            while (current != null) {
                if (current instanceof JsonRpcClient.JsonRpcException) {
                    Object data = ((JsonRpcClient.JsonRpcException) current).data
                    return data instanceof Map ? fromMap(((Map) data).get('policyHelperFailure')) : null
                }
                current = current.cause
            }
            null
        }

        /**
         * Parses a Policy Helper failure from its wire shape, for example the
         * {@code policyHelperFailure} member of a managed settings snapshot.
         *
         * @param value the wire value
         * @return the failure, or null when {@code value} is not an object
         */
        @SuppressWarnings('unchecked')
        static PolicyHelperFailure fromMap(Object value) {
            if (!(value instanceof Map)) return null
            Map<String, Object> map = (Map<String, Object>) value
            PolicyHelperFailure failure = new PolicyHelperFailure()
            failure.category = stringOrNull(map.get('category'))
            failure.failureMode = stringOrNull(map.get('failureMode'))
            failure.message = stringOrNull(map.get('message'))
            failure.previousPolicyRetained = map.get('previousPolicyRetained') == Boolean.TRUE
            failure.reason = stringOrNull(map.get('reason'))
            failure.authority = stringOrNull(map.get('authority'))
            failure.code = stringOrNull(map.get('code'))
            Object details = map.get('details')
            if (details instanceof List) {
                List<Map<String, Object>> findings = []
                for (Object detail : (List<Object>) details) {
                    if (detail instanceof Map) findings.add((Map<String, Object>) detail)
                }
                failure.details = findings
            }
            failure.diagnostic = stringOrNull(map.get('diagnostic'))
            Object helpLink = map.get('helpLink')
            if (helpLink instanceof Map) failure.helpLink = (Map<String, Object>) helpLink
            failure.remediation = stringOrNull(map.get('remediation'))
            failure.severity = stringOrNull(map.get('severity'))
            failure
        }

        private static String stringOrNull(Object value) {
            value instanceof String ? (String) value : null
        }
    }

    /** Permission modes for the {@code permissionMode} option and {@code CopilotSession.setPermissionMode}. */
    static final class PermissionMode {
        /** Permission requests follow the normal approval flow. */
        static final String MANUAL = 'manual'
        /** Permission requests include an LLM safety recommendation. */
        static final String ASSISTED = 'assisted'
        /** Tool, path, and URL permission requests are approved automatically. */
        static final String ALLOW_ALL = 'allow-all'

        private PermissionMode() {}
    }

    /** Known values of {@link PermissionModeResult#reason}. */
    static final class PermissionModeRefusalReason {
        /** Managed (enterprise) policy refused the requested mode. */
        static final String MANAGED_POLICY = 'managed_policy'

        private PermissionModeRefusalReason() {}
    }

    /** Result of a permission-mode change: whether it applied and the authoritative mode afterwards. */
    @ToString(includeNames = true)
    static class PermissionModeResult {
        boolean success
        /** Authoritative permission mode after the request (see {@link PermissionMode}). */
        String mode
        /** Why the mode was refused, when known (see {@link PermissionModeRefusalReason}); null otherwise. */
        String reason

        /**
         * Parses a permission-mode result from its wire shape.
         *
         * @param map the wire map
         * @return the result
         */
        static PermissionModeResult fromMap(Map<String, Object> map) {
            PermissionModeResult result = new PermissionModeResult()
            if (map == null) return result
            result.success = map.get('success') == Boolean.TRUE
            Object mode = map.get('mode')
            Object reason = map.get('reason')
            result.mode = mode instanceof String ? (String) mode : null
            result.reason = reason instanceof String ? (String) reason : null
            result
        }
    }

    // --- v3.1.0 feature types (ask-user images, MCP secret store, plugin extension approvals) ---

    /**
     * Image limits a user input request advertises when the runtime accepts image attachments in
     * the answer ({@code imageAttachmentConstraints}).
     */
    @ToString(includeNames = true)
    static class UserInputImageAttachmentConstraints {
        /** Accepted MIME types, for example {@code 'image/png'}. */
        List<String> allowedMimeTypes = []
        /** Largest accepted image, in bytes. */
        long maxBytesPerImage
        /** Most images accepted in one answer. */
        int maxCount

        /**
         * Parses the constraints from their wire shape.
         *
         * @param value the wire value
         * @return the constraints, or null when {@code value} is not an object
         */
        @SuppressWarnings('unchecked')
        static UserInputImageAttachmentConstraints fromMap(Object value) {
            if (!(value instanceof Map)) return null
            Map<String, Object> map = (Map<String, Object>) value
            UserInputImageAttachmentConstraints constraints = new UserInputImageAttachmentConstraints()
            Object mimeTypes = map.get('allowedMimeTypes')
            if (mimeTypes instanceof List) {
                List<String> allowed = []
                for (Object mimeType : (List<Object>) mimeTypes) {
                    if (mimeType instanceof String) allowed.add((String) mimeType)
                }
                constraints.allowedMimeTypes = allowed
            }
            Object maxBytes = map.get('maxBytesPerImage')
            Object maxCount = map.get('maxCount')
            constraints.maxBytesPerImage = maxBytes instanceof Number ? ((Number) maxBytes).longValue() : 0L
            constraints.maxCount = maxCount instanceof Number ? ((Number) maxCount).intValue() : 0
            constraints
        }
    }

    /**
     * An image attached to an ask-user answer ({@link UserInputResponse#attachments}). The wire
     * {@code type} is always {@code 'image'}.
     */
    @ToString(includeNames = true, excludes = ['data'])
    static class UserInputImageAttachment {
        /** Base64-encoded image bytes. */
        String data
        /** MIME type of the image, for example {@code 'image/png'}. */
        String mimeType
        String description
        String displayName

        UserInputImageAttachment() {}
        UserInputImageAttachment(String data, String mimeType) {
            this.data = data
            this.mimeType = mimeType
        }

        /** Always {@code 'image'}. */
        String getType() { 'image' }

        /** Wire shape: {@code type}, {@code data}, {@code mimeType}, and the optional fields when set. */
        Map<String, Object> toMap() {
            Map<String, Object> map = [type: 'image', data: data, mimeType: mimeType] as Map<String, Object>
            if (description != null) map.description = description
            if (displayName != null) map.displayName = displayName
            map
        }
    }

    /** Result of {@code CopilotClient.mcpSecretsSave} ({@code mcp.secrets.save}). */
    @ToString(includeNames = true)
    static class McpSecretsSaveResult {
        /** True when the OS keychain stored the credential, false for the file backend. */
        boolean usedKeychain

        static McpSecretsSaveResult fromMap(Map<String, Object> map) {
            McpSecretsSaveResult result = new McpSecretsSaveResult()
            if (map != null) result.usedKeychain = map.get('usedKeychain') == Boolean.TRUE
            result
        }
    }

    /** A user-scoped approval for one installed plugin extension ({@code permissions.extensionApprovals.*}). */
    @ToString(includeNames = true)
    static class PermissionsExtensionApproval {
        /** Stable identity, for example {@code 'plugin:plugin-name:extension-name'}. */
        String extensionName
        String pluginName
        /** Exact privileged capabilities the user approved. */
        List<String> capabilities = []
        /** When the approval was granted (RFC 3339 timestamp). */
        String grantedAt
        /** Opaque installation identity; pass it back unchanged to revoke the approval. */
        String installationKey
        /** Display label of the installation, or null. */
        String installationLabel

        /**
         * Parses an approval from its wire shape.
         *
         * @param value the wire value
         * @return the approval, or null when {@code value} is not an object
         */
        @SuppressWarnings('unchecked')
        static PermissionsExtensionApproval fromMap(Object value) {
            if (!(value instanceof Map)) return null
            Map<String, Object> map = (Map<String, Object>) value
            PermissionsExtensionApproval approval = new PermissionsExtensionApproval()
            approval.extensionName = stringOrNull(map.get('extensionName'))
            approval.pluginName = stringOrNull(map.get('pluginName'))
            Object capabilities = map.get('capabilities')
            if (capabilities instanceof List) {
                List<String> approved = []
                for (Object capability : (List<Object>) capabilities) {
                    if (capability instanceof String) approved.add((String) capability)
                }
                approval.capabilities = approved
            }
            approval.grantedAt = stringOrNull(map.get('grantedAt'))
            approval.installationKey = stringOrNull(map.get('installationKey'))
            approval.installationLabel = stringOrNull(map.get('installationLabel'))
            approval
        }

        private static String stringOrNull(Object value) {
            value instanceof String ? (String) value : null
        }
    }

    /** Result of {@code CopilotClient.extensionApprovalsGrant} ({@code permissions.extensionApprovals.grant}). */
    @ToString(includeNames = true)
    static class PermissionsExtensionApprovalsGrantResult {
        /** Whether the pending request was eligible and persisted. */
        boolean granted
        /** The persisted approval when granted, or null. */
        PermissionsExtensionApproval approval

        static PermissionsExtensionApprovalsGrantResult fromMap(Map<String, Object> map) {
            PermissionsExtensionApprovalsGrantResult result = new PermissionsExtensionApprovalsGrantResult()
            if (map == null) return result
            result.granted = map.get('granted') == Boolean.TRUE
            result.approval = PermissionsExtensionApproval.fromMap(map.get('approval'))
            result
        }
    }

    /** Result of {@code CopilotClient.extensionApprovalsList} ({@code permissions.extensionApprovals.list}). */
    @ToString(includeNames = true)
    static class PermissionsExtensionApprovalsListResult {
        List<PermissionsExtensionApproval> approvals = []

        @SuppressWarnings('unchecked')
        static PermissionsExtensionApprovalsListResult fromMap(Map<String, Object> map) {
            PermissionsExtensionApprovalsListResult result = new PermissionsExtensionApprovalsListResult()
            Object approvals = map?.get('approvals')
            if (approvals instanceof List) {
                for (Object approval : (List<Object>) approvals) {
                    PermissionsExtensionApproval parsed = PermissionsExtensionApproval.fromMap(approval)
                    if (parsed != null) result.approvals.add(parsed)
                }
            }
            result
        }
    }

    /** Result of {@code CopilotClient.extensionApprovalsRevoke} ({@code permissions.extensionApprovals.revoke}). */
    @ToString(includeNames = true)
    static class PermissionsExtensionApprovalsRevokeResult {
        /** Whether a matching approval was revoked. */
        boolean revoked

        static PermissionsExtensionApprovalsRevokeResult fromMap(Map<String, Object> map) {
            PermissionsExtensionApprovalsRevokeResult result = new PermissionsExtensionApprovalsRevokeResult()
            if (map != null) result.revoked = map.get('revoked') == Boolean.TRUE
            result
        }
    }
}
