/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.booleanOrNull
import kotlinx.serialization.json.longOrNull

// ============================================================================
// Connection State
// ============================================================================

/**
 * Connection state of the client.
 */
enum class ConnectionState {
    DISCONNECTED,
    CONNECTING,
    CONNECTED,
    ERROR
}

// ============================================================================
// Tool Types
// ============================================================================

/**
 * Result type from a tool execution.
 */
@Serializable
enum class ToolResultType {
    @SerialName("success") SUCCESS,
    @SerialName("failure") FAILURE,
    @SerialName("rejected") REJECTED,
    @SerialName("denied") DENIED
}

/**
 * Binary result from a tool execution.
 */
@Serializable
data class ToolBinaryResult(
    val data: String,
    val mimeType: String,
    val type: String,
    val description: String? = null
)

/**
 * Structured result from a tool execution.
 */
@Serializable
data class ToolResultObject(
    val textResultForLlm: String,
    val resultType: String,
    val binaryResultsForLlm: List<ToolBinaryResult>? = null,
    val error: String? = null,
    val sessionLog: String? = null,
    val toolTelemetry: Map<String, JsonElement>? = null
)

/**
 * Information about a tool invocation.
 */
data class ToolInvocation(
    val sessionId: String,
    val toolCallId: String,
    val toolName: String,
    val arguments: Any?
)

/**
 * Handler function for tool calls.
 */
typealias ToolHandler = suspend (args: Any?, invocation: ToolInvocation) -> Any?

/**
 * Tool definition for registering custom tools.
 */
data class Tool(
    val name: String,
    val description: String? = null,
    val parameters: Map<String, Any?>? = null,
    val handler: ToolHandler,
    /** Tool "defer" loading policy: "auto" (lazy via search) or "never" (eager pre-load). */
    val defer: String? = null,
    /** Declares that repeating a call with the same arguments has no additional effect, so the runtime may retry it. */
    val idempotent: Boolean? = null
)

// ============================================================================
// System Message Configuration
// ============================================================================

/**
 * Known system prompt section identifiers for the "customize" mode.
 */
object SystemPromptSection {
    const val IDENTITY = "identity"
    const val TONE = "tone"
    const val TOOL_EFFICIENCY = "tool_efficiency"
    const val ENVIRONMENT_CONTEXT = "environment_context"
    const val CODE_CHANGE_RULES = "code_change_rules"
    const val GUIDELINES = "guidelines"
    const val SAFETY = "safety"
    const val TOOL_INSTRUCTIONS = "tool_instructions"
    const val CUSTOM_INSTRUCTIONS = "custom_instructions"
    const val LAST_INSTRUCTIONS = "last_instructions"
}

/**
 * Override action for a system prompt section.
 */
object SectionOverrideAction {
    const val REPLACE = "replace"
    const val REMOVE = "remove"
    const val APPEND = "append"
    const val PREPEND = "prepend"
}

/**
 * Override operation for a single system prompt section.
 */
@Serializable
data class SectionOverride(
    val action: String,
    val content: String? = null
)

/**
 * System message configuration.
 *
 * Use [mode] = "append" (default) to append content after SDK-managed sections.
 * Use [mode] = "replace" to provide the entire system message.
 * Use [mode] = "customize" to override individual sections of the system prompt.
 */
@Serializable
data class SystemMessageConfig(
    val mode: String? = null,
    val content: String? = null,
    val sections: Map<String, SectionOverride>? = null
)

// ============================================================================
// Session Configuration
// ============================================================================

/**
 * Reasoning effort levels for models that support it.
 */
@Serializable
enum class ReasoningEffort {
    @SerialName("low") LOW,
    @SerialName("medium") MEDIUM,
    @SerialName("high") HIGH,
    @SerialName("xhigh") XHIGH
}

/**
 * Configuration for a custom API provider (BYOK).
 */
@Serializable
data class ProviderConfig(
    val type: String? = null,
    val wireApi: String? = null,
    val baseUrl: String,
    val apiKey: String? = null,
    val bearerToken: String? = null,
    val azure: AzureConfig? = null,
    /** Product serving the model, reported as `model_provider` (see [ModelProvider]). */
    val modelProvider: String? = null,
    /** Command run before each provider request; its stdout is used as the API key. */
    val apiKeyCommand: String? = null
) {
    @Serializable
    data class AzureConfig(
        val apiVersion: String? = null
    )
}

/**
 * MCP server configuration.
 */
@Serializable
data class MCPServerConfig(
    val tools: List<String>,
    val type: String? = null,
    val timeout: Int? = null,
    val command: String? = null,
    val args: List<String>? = null,
    val env: Map<String, String>? = null,
    val cwd: String? = null,
    val url: String? = null,
    val headers: Map<String, String>? = null
)

/**
 * Custom agent configuration.
 */
@Serializable
data class CustomAgentConfig(
    val name: String,
    val displayName: String? = null,
    val description: String? = null,
    val tools: List<String>? = null,
    val prompt: String,
    val mcpServers: Map<String, MCPServerConfig>? = null,
    val infer: Boolean? = null,

    /**
     * List of skill names to preload into this agent's context.
     */
    val skills: List<String>? = null,

    /**
     * JSON schema declaring a factory's arguments.
     */
    val argsSchema: JsonElement? = null
)

/**
 * Infinite session configuration.
 */
@Serializable
data class InfiniteSessionConfig(
    val enabled: Boolean? = null,
    val backgroundCompactionThreshold: Double? = null,
    val bufferExhaustionThreshold: Double? = null
)

// ============================================================================
// Command & Elicitation Types
// ============================================================================

/**
 * Context for a slash-command invocation.
 */
data class CommandContext(
    val sessionId: String,
    val command: String,
    val commandName: String,
    val args: String
)

/**
 * Handler invoked when a registered slash-command is executed.
 */
typealias CommandHandler = suspend (context: CommandContext) -> Unit

/**
 * Definition of a slash command registered with the session.
 */
data class CommandDefinition(
    val name: String,
    val description: String? = null,
    val handler: CommandHandler
)

/**
 * Context for an elicitation request from the server.
 */
data class ElicitationContext(
    val sessionId: String,
    val message: String,
    val requestedSchema: Map<String, Any?>? = null,
    val mode: String? = null,
    val elicitationSource: String? = null,
    val url: String? = null,
    /** ID of the pending elicitation request, when the runtime sends one. */
    val requestId: String? = null,
    /** The request's opaque `_meta` object (negotiated extensions), when the runtime sends one. */
    val meta: JsonObject? = null
)

/**
 * Result returned from an elicitation handler.
 */
data class ElicitationResult(
    val action: String,
    val content: Map<String, Any?>? = null,
    /** Opaque `_meta` object sent back to the runtime unchanged, when set. */
    val meta: JsonObject? = null
)

/**
 * Handler for elicitation requests from the server.
 */
typealias ElicitationHandler = suspend (context: ElicitationContext) -> ElicitationResult

/**
 * Configuration for creating a session.
 */
data class SessionConfig(
    val sessionId: String? = null,
    val model: String? = null,
    val reasoningEffort: ReasoningEffort? = null,
    val configDir: String? = null,
    val tools: List<Tool>? = null,
    val systemMessage: SystemMessageConfig? = null,
    val availableTools: List<String>? = null,
    val excludedTools: List<String>? = null,
    val provider: ProviderConfig? = null,
    val onPermissionRequest: PermissionHandler? = null,
    val onUserInputRequest: UserInputHandler? = null,
    val hooks: SessionHooks? = null,
    val workingDirectory: String? = null,
    val streaming: Boolean? = null,

    /**
     * Include sub-agent streaming events in the event stream. Default: true.
     */
    val includeSubAgentStreamingEvents: Boolean? = null,

    val mcpServers: Map<String, MCPServerConfig>? = null,
    val customAgents: List<CustomAgentConfig>? = null,
    val skillDirectories: List<String>? = null,
    val disabledSkills: List<String>? = null,
    val infiniteSessions: InfiniteSessionConfig? = null,

    /**
     * Per-property overrides for model capabilities, deep-merged over runtime defaults.
     */
    val modelCapabilities: Map<String, Any?>? = null,

    /**
     * When true, auto-discovers MCP server configs from working directory. Default: false.
     */
    val enableConfigDiscovery: Boolean? = null,

    /**
     * GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
     */
    val gitHubToken: String? = null,

    /** Slash commands registered for this session. */
    val commands: List<CommandDefinition>? = null,

    /** Handler for elicitation requests from the server. */
    val onElicitationRequest: ElicitationHandler? = null,

    /** Additional HTTP headers sent with each model request. */
    val requestHeaders: Map<String, String>? = null,
    /** Response format for image generation ("text", "image", "json_object"). */
    val responseFormat: String? = null,
    /** Idle timeout in seconds before the session is automatically closed. */
    val idleTimeout: Int? = null,
    /** Custom instruction directory paths. */
    val instructionDirectories: List<String>? = null,
    /** Enable inline source citations in assistant responses. */
    val enableCitations: Boolean? = null,
    /** Built-in agent names to exclude from this session. */
    val excludedBuiltinAgents: List<String>? = null,
    /** Per-session AI-credit spending limits. */
    val sessionLimits: SessionLimitsConfig? = null,
    /** Opt-in persistent session memory configuration. */
    val memory: MemoryConfiguration? = null,
    /** OTLP telemetry export protocol ("grpc" or "http/protobuf"). */
    val otlpProtocol: String? = null,
    /** Stream model responses over a WebSocket transport when true. */
    val enableWebSocketResponses: Boolean? = null,
    /** Experiment assignment overrides forwarded to the runtime. */
    val expAssignments: Map<String, Any?>? = null,
    /** Enable session rewind to restore earlier session points. */
    val rewindEnabled: Boolean? = null,
    /** Extra working directories available to the session. */
    val additionalDirectories: List<String>? = null,
    /** Names of configured MCP servers to disable for this session. */
    val disabledMcpServers: List<String>? = null,
    /** Configuration for the built-in GitHub MCP tools. */
    val githubMcpToolConfig: Map<String, Any?>? = null,
    /** Identity of the canvas provider to register. */
    val canvasProvider: Map<String, Any?>? = null,
    /** Only load workspace-local custom agents. */
    val customAgentsLocalOnly: Boolean? = null,
    /** Tool search configuration for large tool sets. */
    val toolSearch: Map<String, Any?>? = null,
    /** Enable experimental runtime features. */
    val experimentalMode: Boolean? = null,
    /** Respect repository content-exclusion rules. */
    val contentExclusion: Boolean? = null,
    /** Handler invoked when an MCP server requests an OAuth host token. */
    val onMcpAuthRequest: McpAuthHandler? = null,
    /** Auto-model routing preference (see [AutoTier]); sent under `capi`. Experimental. */
    val autoTier: String? = null,
    /** Model-facing shape of the built-in ask_user tool (see [AskUserVariant]). */
    val askUserVariant: String? = null,
    /** Acquires short-lived GitHub tokens on demand. Mutually exclusive with [gitHubToken]. */
    val gitHubTokenProvider: GitHubTokenProvider? = null,
    /** Sandbox configuration applied before MCP servers or session tools start. */
    val sandboxConfig: SandboxConfig? = null,
    /** Host-resolved feature-flag values; sent whenever provided, even if empty. */
    val featureFlags: Map<String, Boolean>? = null,
    /** Exact model IDs allowed by the host. */
    val allowedModels: List<String>? = null,
    /** OAuth Client ID Metadata Document URL identifying the host for MCP authorization. */
    val authClientIdMetadataUrl: String? = null,
    /** Session name applied at creation. */
    val name: String? = null,
    /** Explicit discovery-only custom-agent roots. */
    val agentDirectories: List<String>? = null,
    /** Explicit discovery-only file-hook roots. */
    val hookDirectories: List<String>? = null,
    /** Declares that a human drives this session's conversation. */
    val interactiveUser: Boolean? = null,
    /** Managed-setting paths this host handles externally or does not support. */
    val excludedManagedSettings: List<String>? = null,
    /** Enforce locked managed model controls for this session. */
    val enforceManagedModelDefaults: Boolean? = null,
    /** Grants trust to the working directory at creation (see [WorkingDirectoryTrust]). */
    val trustWorkingDirectory: String? = null,
    /** Invalidate the custom-instruction discovery cache before creating the session. */
    val refreshCustomInstructions: Boolean? = null,
    /** Session-level image generation opt-in. */
    val imageGeneration: ImageGenerationConfig? = null,
    /** Autopilot continuation policy. */
    val autopilot: AutopilotOptions? = null,
    /** Named BYOK provider connections (`inference.providers`). */
    val providers: List<NamedProviderConfig>? = null,
    /** BYOK model definitions served by [providers] (`inference.models`); select one as "provider/id". */
    val models: List<ProviderModelConfig>? = null,
    /** Connection that serves unqualified model names (`inference.defaultProvider`). */
    val defaultProvider: String? = null,
    /** Copilot API options (`github.capi`); its members win over [autoTier] and [enableWebSocketResponses]. */
    val capi: CapiConfig? = null,
    /** Answers secret, text, or confirmation prompts from programs run by shell tools. */
    val onShellAuthenticationRequest: ShellAuthenticationHandler? = null,
    /** Initial permission mode (see [PermissionMode]). */
    val permissionMode: String? = null,
)

/**
 * Configuration for resuming a session.
 */
data class ResumeSessionConfig(
    val model: String? = null,
    val reasoningEffort: ReasoningEffort? = null,
    val configDir: String? = null,
    val tools: List<Tool>? = null,
    val systemMessage: SystemMessageConfig? = null,
    val availableTools: List<String>? = null,
    val excludedTools: List<String>? = null,
    val provider: ProviderConfig? = null,
    val onPermissionRequest: PermissionHandler? = null,
    val onUserInputRequest: UserInputHandler? = null,
    val hooks: SessionHooks? = null,
    val workingDirectory: String? = null,
    val streaming: Boolean? = null,

    /**
     * Include sub-agent streaming events in the event stream. Default: true.
     */
    val includeSubAgentStreamingEvents: Boolean? = null,

    val mcpServers: Map<String, MCPServerConfig>? = null,
    val customAgents: List<CustomAgentConfig>? = null,
    val skillDirectories: List<String>? = null,
    val disabledSkills: List<String>? = null,
    val infiniteSessions: InfiniteSessionConfig? = null,

    /**
     * Per-property overrides for model capabilities, deep-merged over runtime defaults.
     */
    val modelCapabilities: Map<String, Any?>? = null,

    /**
     * When true, auto-discovers MCP server configs from working directory. Default: false.
     */
    val enableConfigDiscovery: Boolean? = null,

    /**
     * GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
     */
    val gitHubToken: String? = null,

    /** Slash commands registered for this session. */
    val commands: List<CommandDefinition>? = null,

    /** Handler for elicitation requests from the server. */
    val onElicitationRequest: ElicitationHandler? = null,

    val disableResume: Boolean? = null,
    /** Custom instruction directory paths. */
    val instructionDirectories: List<String>? = null,
    /** Stream model responses over a WebSocket transport when true; sent under `capi`. */
    val enableWebSocketResponses: Boolean? = null,
    /** Auto-model routing preference (see [AutoTier]); sent under `capi`. Experimental. */
    val autoTier: String? = null,
    /** Model-facing shape of the built-in ask_user tool (see [AskUserVariant]). */
    val askUserVariant: String? = null,
    /** Acquires short-lived GitHub tokens on demand. Mutually exclusive with [gitHubToken]. */
    val gitHubTokenProvider: GitHubTokenProvider? = null,
    /** Sandbox configuration applied before MCP servers or session tools start. */
    val sandboxConfig: SandboxConfig? = null,
    /** Host-resolved feature-flag values; sent whenever provided, even if empty. */
    val featureFlags: Map<String, Boolean>? = null,
    /** Exact model IDs allowed by the host. */
    val allowedModels: List<String>? = null,
    /** OAuth Client ID Metadata Document URL identifying the host for MCP authorization. */
    val authClientIdMetadataUrl: String? = null,
    /** Explicit discovery-only custom-agent roots. */
    val agentDirectories: List<String>? = null,
    /** Explicit discovery-only file-hook roots. */
    val hookDirectories: List<String>? = null,
    /** Declares that a human drives this session's conversation. */
    val interactiveUser: Boolean? = null,
    /** Managed-setting paths this host handles externally or does not support. */
    val excludedManagedSettings: List<String>? = null,
    /** Enforce locked managed model controls for this session. */
    val enforceManagedModelDefaults: Boolean? = null,
    /** Allow the runtime to recover a damaged transcript during resume. */
    val allowTranscriptRecovery: Boolean? = null,
    /** Session-level image generation opt-in. */
    val imageGeneration: ImageGenerationConfig? = null,
    /** Autopilot continuation policy. */
    val autopilot: AutopilotOptions? = null,
    /** Skip emitting the `session.resume` event (same as [disableResume]). */
    val suppressResumeEvent: Boolean? = null,
    /** Continue tool calls and permission prompts that were pending when the session was suspended. */
    val continuePendingWork: Boolean? = null,
    /** Named BYOK provider connections (`inference.providers`); re-supply them on cold resume. */
    val providers: List<NamedProviderConfig>? = null,
    /** BYOK model definitions served by [providers] (`inference.models`). */
    val models: List<ProviderModelConfig>? = null,
    /** Connection that serves unqualified model names (`inference.defaultProvider`). */
    val defaultProvider: String? = null,
    /** When true, sends `inference.defaultProvider: null` to clear the session's default provider. */
    val clearDefaultProvider: Boolean = false,
    /** Copilot API options (`github.capi`); its members win over [autoTier] and [enableWebSocketResponses]. */
    val capi: CapiConfig? = null,
    /** Opt-in persistent session memory configuration, including the [MemoryConfiguration.storeMemory] callback. */
    val memory: MemoryConfiguration? = null,
    /** Answers secret, text, or confirmation prompts from programs run by shell tools. */
    val onShellAuthenticationRequest: ShellAuthenticationHandler? = null,
    /** Permission mode to apply (see [PermissionMode]). */
    val permissionMode: String? = null,
)

// ============================================================================
// Session Event Types
// ============================================================================

/**
 * Session event from the server.
 */
@Serializable
data class SessionEvent(
    val type: String,
    val id: String = "",
    val timestamp: String = "",
    val parentId: String? = null,
    val agentId: String? = null,
    val ephemeral: Boolean? = null,
    val data: JsonObject? = null
)

/**
 * Event handler for session events.
 */
typealias SessionEventHandler = (event: SessionEvent) -> Unit

// ============================================================================
// Permission Types
// ============================================================================

/**
 * Permission request from the server.
 */
@Serializable
data class PermissionRequest(
    val kind: String,
    val toolCallId: String? = null
)

/**
 * Permission request result.
 */
@Serializable
data class PermissionRequestResult(
    val kind: String,
    val rules: List<JsonElement>? = null,
    /** Opaque context forwarded on permission replies. */
    val decisionContext: Map<String, JsonElement>? = null
)

/**
 * Handler for permission requests.
 */
typealias PermissionHandler = suspend (request: PermissionRequest, sessionId: String) -> PermissionRequestResult

// ============================================================================
// User Input Types
// ============================================================================

/**
 * Request for user input from the agent (enables ask_user tool).
 */
@Serializable
data class UserInputRequest(
    val question: String,
    val choices: List<String>? = null,
    val allowFreeform: Boolean? = null,
    /** ID of the pending user input request, when the runtime sends one. */
    val requestId: String? = null,
    /**
     * Image support advertised by the runtime; null when the runtime does not accept images in the
     * answer. When set, the answer may carry [UserInputResponse.attachments].
     */
    val imageAttachmentConstraints: UserInputImageAttachmentConstraints? = null,
    /** The request's opaque `_meta` object, when the runtime sends one. */
    @SerialName("_meta") val meta: JsonObject? = null
)

/**
 * Response to a user input request. [attachments] is only valid when the request advertised
 * [UserInputRequest.imageAttachmentConstraints] and is sent only when set; for an image-only answer
 * use an empty [answer] and `wasFreeform = true`. The runtime enforces the advertised limits.
 */
@Serializable
data class UserInputResponse(
    val answer: String,
    val wasFreeform: Boolean,
    /** Images attached to the answer. */
    val attachments: List<UserInputImageAttachment>? = null
)

/**
 * Handler for user input requests.
 */
typealias UserInputHandler = suspend (request: UserInputRequest, sessionId: String) -> UserInputResponse

/**
 * Request from the agent to exit plan mode and begin executing a plan.
 */
@Serializable
data class ExitPlanModeRequest(
    val sessionId: String,
    val summary: String,
    val planContent: String? = null,
    val actions: List<String>,
    val recommendedAction: String
)

/**
 * Response to an exit plan mode request.
 */
@Serializable
data class ExitPlanModeResponse(
    val approved: Boolean,
    val selectedAction: String? = null,
    val feedback: String? = null
)

/**
 * Handler for exit plan mode requests.
 */
typealias ExitPlanModeHandler = suspend (request: ExitPlanModeRequest) -> ExitPlanModeResponse

// ============================================================================
// Hook Types
// ============================================================================

/**
 * Generic hook handler type.
 * Takes hook input (as a map) and a session ID, returns hook output (as a map) or null.
 */
typealias HookHandler = suspend (input: Map<String, Any?>, sessionId: String) -> Map<String, Any?>?

/**
 * Configuration for session hooks - lifecycle event interceptors.
 */
data class SessionHooks(
    val onPreToolUse: HookHandler? = null,
    val onPostToolUse: HookHandler? = null,
    val onPreMcpToolCall: HookHandler? = null,
    val onUserPromptSubmitted: HookHandler? = null,
    val onUserPromptTransformed: HookHandler? = null,
    val onSessionStart: HookHandler? = null,
    val onSessionEnd: HookHandler? = null,
    val onErrorOccurred: HookHandler? = null,
    /**
     * Called before a sub-agent's first turn (`subagentStart`). Input carries `transcriptPath`,
     * `agentName`, `agentDisplayName`, `agentDescription` plus the base hook fields; return
     * `additionalContext` to prepend to the child's prompt.
     */
    val onSubagentStart: HookHandler? = null,
    /**
     * Called after a sub-agent completes a turn (`subagentStop`). Input adds `agentId`, `agentType`,
     * `stopReason` and `response`; return `decision` ("block"/"allow"), `reason`, or `modifiedResponse`.
     */
    val onSubagentStop: HookHandler? = null
) {
    /**
     * Returns true if any hook handler is registered.
     */
    fun hasAnyHook(): Boolean =
        onPreToolUse != null || onPostToolUse != null || onPreMcpToolCall != null || onUserPromptSubmitted != null ||
            onUserPromptTransformed != null ||
            onSessionStart != null || onSessionEnd != null || onErrorOccurred != null ||
            onSubagentStart != null || onSubagentStop != null

    /**
     * Gets a handler by hook type name.
     */
    fun getHandler(hookType: String): HookHandler? = when (hookType) {
        "preToolUse" -> onPreToolUse
        "postToolUse" -> onPostToolUse
        "preMcpToolCall" -> onPreMcpToolCall
        "userPromptSubmitted" -> onUserPromptSubmitted
        "userPromptTransformed" -> onUserPromptTransformed
        "sessionStart" -> onSessionStart
        "sessionEnd" -> onSessionEnd
        "errorOccurred" -> onErrorOccurred
        "subagentStart" -> onSubagentStart
        "subagentStop" -> onSubagentStop
        else -> null
    }
}

// ============================================================================
// Image Generation Types
// ============================================================================

/**
 * Response format for message responses.
 */
@Serializable
enum class ResponseFormat {
    @SerialName("text") TEXT,
    @SerialName("image") IMAGE,
    @SerialName("json_object") JSON_OBJECT
}

/**
 * Options for image generation.
 */
@Serializable
data class ImageOptions(
    val size: String? = null,
    val quality: String? = null,
    val style: String? = null
)

/**
 * Image data from an assistant image response.
 */
@Serializable
data class AssistantImageData(
    val format: String,
    val base64: String,
    val url: String? = null,
    val revisedPrompt: String? = null,
    val width: Int,
    val height: Int
)

/**
 * A content block in a mixed text+image response.
 */
@Serializable
data class ContentBlock(
    val type: String,
    val text: String? = null,
    val image: AssistantImageData? = null
)

// ============================================================================
// Message Options
// ============================================================================

/**
 * Attachment for a message.
 */
@Serializable
data class Attachment(
    val type: String,
    val path: String? = null,
    val filePath: String? = null,
    val displayName: String? = null,
    val selection: SelectionRange? = null,
    val text: String? = null
) {
    @Serializable
    data class SelectionRange(
        val start: Position,
        val end: Position
    )

    @Serializable
    data class Position(
        val line: Int,
        val character: Int
    )
}

/**
 * Options for sending a message to a session.
 */
data class MessageOptions(
    val prompt: String,
    val attachments: List<Attachment>? = null,
    val mode: String? = null,
    /**
     * Legacy per-message response format. No longer sent: protocol 4 only accepts the structured
     * [responseSchema] format. Use [SessionConfig.imageGeneration] for images.
     */
    val responseFormat: ResponseFormat? = null,
    val imageOptions: ImageOptions? = null,

    /**
     * Custom HTTP headers to include in outbound model requests for this turn.
     */
    val requestHeaders: Map<String, String>? = null,

    /** Agent execution mode for this turn (e.g. "agent", "chat"). */
    val agentMode: String? = null,

    /** Prompt text to display in the UI, when it differs from the model prompt. */
    val displayPrompt: String? = null,

    /** Message provenance: "user", "system", or "agent-<id>" (see [MessageSource]). */
    val source: String? = null,

    /** JSON Schema for structured output; sent as a `json_schema` response format when set. */
    val responseSchema: Map<String, Any?>? = null,

    /** Opaque caller-chosen identifier for this turn, echoed on the turn's events. */
    val clientTurnId: String? = null
)

// ============================================================================
// Ping / Status / Auth Types
// ============================================================================

/**
 * Response from a ping request.
 */
@Serializable
data class PingResponse(
    val message: String? = null,
    val timestamp: String = "",
    val protocolVersion: Int? = null
)

/**
 * Response from status.get.
 */
@Serializable
data class GetStatusResponse(
    val version: String,
    val protocolVersion: Int
)

/**
 * Response from auth.getStatus.
 */
@Serializable
data class GetAuthStatusResponse(
    val isAuthenticated: Boolean,
    val authType: String? = null,
    val host: String? = null,
    val login: String? = null,
    val statusMessage: String? = null
)

// ============================================================================
// Model Types
// ============================================================================

/**
 * Model supports flags.
 */
@Serializable
data class ModelSupports(
    val vision: Boolean = false,
    val reasoningEffort: Boolean = false
)

/**
 * Model limits.
 */
@Serializable
data class ModelLimits(
    @SerialName("max_prompt_tokens")
    val maxPromptTokens: Int? = null,
    @SerialName("max_context_window_tokens")
    val maxContextWindowTokens: Int = 0,
    @SerialName("max_output_tokens")
    val maxOutputTokens: Int? = null
)

/**
 * Model capabilities and limits.
 */
@Serializable
data class ModelCapabilities(
    val supports: ModelSupports = ModelSupports(),
    val limits: ModelLimits = ModelLimits()
)

/**
 * Model policy state.
 */
@Serializable
data class ModelPolicy(
    val state: String,
    val terms: String
)

/**
 * Model billing information.
 */
@Serializable
data class ModelBilling(
    val multiplier: Double
)

/**
 * Information about an available model.
 */
@Serializable
data class ModelInfo(
    val id: String,
    val name: String,
    val capabilities: ModelCapabilities = ModelCapabilities(),
    val policy: ModelPolicy? = null,
    val billing: ModelBilling? = null,
    val supportedReasoningEfforts: List<String>? = null,
    val defaultReasoningEffort: String? = null
)

// ============================================================================
// Session Metadata
// ============================================================================

/**
 * Metadata about a session.
 */
@Serializable
data class SessionMetadata(
    val sessionId: String,
    val startTime: String,
    val modifiedTime: String,
    val summary: String? = null,
    val isRemote: Boolean = false
)

// ============================================================================
// Session Lifecycle Types
// ============================================================================

/**
 * Session lifecycle event notification.
 */
@Serializable
data class SessionLifecycleEvent(
    val type: String,
    val sessionId: String,
    val metadata: LifecycleMetadata? = null
) {
    @Serializable
    data class LifecycleMetadata(
        val startTime: String? = null,
        val modifiedTime: String? = null,
        val summary: String? = null
    )
}

/**
 * Handler for session lifecycle events.
 */
typealias SessionLifecycleHandler = (event: SessionLifecycleEvent) -> Unit

// ============================================================================
// Session Filesystem Types
// ============================================================================

/**
 * Configuration for a custom session filesystem provider.
 */
@Serializable
data class SessionFsConfig(
    val initialCwd: String,
    val sessionStatePath: String,
    val conventions: String
)

/**
 * File metadata returned by session filesystem operations.
 */
data class SessionFsFileInfo(
    val name: String,
    val size: Long,
    val isDirectory: Boolean,
    val isFile: Boolean,
    val createdAt: String? = null,
    val modifiedAt: String? = null
)

/**
 * Interface for session filesystem providers.
 * Implementors provide file operations scoped to a session.
 */
interface SessionFsProvider {
    suspend fun readFile(sessionId: String, path: String): String
    suspend fun writeFile(sessionId: String, path: String, content: String)
    suspend fun appendFile(sessionId: String, path: String, content: String)
    suspend fun exists(sessionId: String, path: String): Boolean
    suspend fun stat(sessionId: String, path: String): SessionFsFileInfo
    suspend fun mkdir(sessionId: String, path: String, recursive: Boolean)
    suspend fun readdir(sessionId: String, path: String): List<String>
    suspend fun readdirWithTypes(sessionId: String, path: String): List<SessionFsFileInfo>
    suspend fun rm(sessionId: String, path: String, recursive: Boolean)
    suspend fun rename(sessionId: String, oldPath: String, newPath: String)
}

// ============================================================================
// Client Options
// ============================================================================

/**
 * Options for creating a [CopilotClient].
 */
data class CopilotClientOptions(
    /**
     * Path to the CLI executable.
     * Defaults to "copilot" (found via PATH) or COPILOT_CLI_PATH env var.
     */
    val cliPath: String? = null,

    /**
     * Extra arguments to pass to the CLI executable (inserted before SDK-managed args).
     */
    val cliArgs: List<String>? = null,

    /**
     * Working directory for the CLI process.
     */
    val cwd: String? = null,

    /**
     * Port for the CLI server (TCP mode only). 0 = random available port.
     */
    val port: Int = 0,

    /**
     * Use stdio transport instead of TCP.
     * When true, communicates with CLI via stdin/stdout pipes.
     * Defaults to true.
     */
    val useStdio: Boolean? = null,

    /**
     * URL of an existing Copilot CLI server to connect to over TCP.
     * When provided, the client will not spawn a CLI process.
     * Mutually exclusive with cliPath, useStdio.
     */
    val cliUrl: String? = null,

    /**
     * Log level for the CLI server.
     */
    val logLevel: String = "info",

    /**
     * Auto-start the CLI server on first use. Defaults to true.
     */
    val autoStart: Boolean = true,

    /**
     * Auto-restart the CLI server if it crashes. Defaults to true.
     */
    val autoRestart: Boolean = true,

    /**
     * Environment variables to pass to the CLI process.
     */
    val env: Map<String, String>? = null,

    /**
     * GitHub token for authentication.
     */
    val githubToken: String? = null,

    /**
     * Whether to use the logged-in user for authentication. Defaults to true (false when githubToken provided).
     */
    val useLoggedInUser: Boolean? = null,

    /**
     * Server-wide idle timeout for sessions in seconds.
     */
    val sessionIdleTimeoutSeconds: Int? = null,

    /**
     * Configuration for a custom session filesystem provider.
     */
    val sessionFs: SessionFsConfig? = null,

    /**
     * Configurable data directory, passed as COPILOT_HOME env var.
     */
    val copilotHome: String? = null,

    /**
     * Auth token for TCP server connections.
     */
    val tcpConnectionToken: String? = null,

    /**
     * Intercepts outbound LLM inference HTTP/WebSocket requests before they are sent.
     */
    val requestHandler: CopilotRequestHandler? = null,

    /**
     * BYOK provider that supplies per-session bearer tokens.
     */
    val bearerTokenProvider: BearerTokenProvider? = null,

    /**
     * Directories to load built-in plugins from.
     */
    val builtinPluginDirectories: List<String>? = null,

    /**
     * Use the in-process (FFI) transport instead of a subprocess.
     */
    val inProcess: Boolean? = null,

    /**
     * Identity of the integrating application, sent on the `connect` handshake.
     */
    val clientInfo: ClientInfo? = null,

    /**
     * Client mode: [ClientMode.CODING_AGENT] (default, unchanged behavior) or [ClientMode.EMPTY]
     * (every session must set `availableTools`; runtime features default off).
     * The legacy value "copilot-cli" is accepted as an alias of "coding-agent".
     */
    val mode: String = ClientMode.CODING_AGENT,

    /**
     * Network callbacks (proxy resolution and proxy authentication), registered with
     * `network.setProvider` after connecting.
     */
    val network: NetworkConfig? = null
)

// ============================================================================
// Upstream-sync feature types (parity with @github/copilot-sdk)
// ============================================================================

/** Per-session AI-credit budget; set [maxAiCredits] to cap spend. */
@Serializable
data class SessionLimitsConfig(
    val maxAiCredits: Int? = null
)

/** Opt-in persistent session memory configuration. */
@Serializable
data class MemoryConfiguration(
    val enabled: Boolean? = null,
    /**
     * Stores the memories the agent saves (sent to the runtime as `state.hasMemoryStore`).
     * Requires [enabled] to be true. Never serialized.
     */
    @kotlinx.serialization.Transient
    val storeMemory: MemoryStoreHandler? = null
)

/** Arguments passed to a BYOK bearer-token provider (per-session scoping). */
@Serializable
data class ProviderTokenArgs(
    val sessionId: String
)

/** Supplies a per-session bearer token for BYOK model providers. */
typealias BearerTokenProvider = suspend (args: ProviderTokenArgs) -> String

/** Intercepts outbound LLM inference HTTP/WebSocket requests before they are sent. */
typealias CopilotRequestHandler = suspend (request: Map<String, Any?>, context: Map<String, Any?>) -> Map<String, Any?>

/** Handler invoked when an MCP server requests an OAuth host token. */
typealias McpAuthHandler = suspend (request: Map<String, Any?>, sessionId: String) -> String

// ============================================================================
// v2.6.0 upstream-sync feature types
// ============================================================================

/**
 * Identity of the integrating application, declared once on the `connect` handshake so runtime
 * telemetry is attributed to a consistent surface. Empty fields are dropped and an all-empty
 * identity skips the handshake entirely.
 */
data class ClientInfo(
    /** Name of the application using the SDK, e.g. "acme-developer-portal". */
    val applicationName: String? = null,
    /** Version of the application using the SDK, e.g. "2.4.0". */
    val applicationVersion: String? = null,
    /** Name of a specific integration within the application, such as an extension or plugin. */
    val integrationName: String? = null,
    /** Version of the integration identified by [integrationName]. */
    val integrationVersion: String? = null
)

/** Auto-model routing preferences for [SessionConfig.autoTier] and [CopilotSession.setAutoTier] (experimental). */
object AutoTier {
    const val EFFICIENCY = "efficiency"
    const val BALANCE = "balance"
    const val INTELLIGENCE = "intelligence"
    const val FAST = "fast"
}

/** Model-facing shapes of the built-in `ask_user` tool. */
object AskUserVariant {
    const val LEGACY = "legacy"
    const val ELICITATION = "elicitation"
}

/** Working-directory trust scopes for [SessionConfig.trustWorkingDirectory]. */
object WorkingDirectoryTrust {
    /** Trust applies only to this session. */
    const val SESSION = "session"
    /** Trust is persisted for later sessions. */
    const val USER = "user"
}

/** Message provenance values for [MessageOptions.source]. */
object MessageSource {
    const val USER = "user"
    const val SYSTEM = "system"

    /** Builds the source value for a message originating from an identified agent. */
    fun agent(agentId: String): String = "agent-$agentId"
}

/** Products serving a BYOK model, for [ProviderConfig.modelProvider]. */
object ModelProvider {
    const val OPENAI = "openai"
    const val ANTHROPIC = "anthropic"
    const val AZURE_OPENAI = "azure_openai"
    const val OLLAMA = "ollama"
    const val LM_STUDIO = "lm_studio"
    const val FOUNDRY_LOCAL = "foundry_local"
    const val LLAMA_CPP = "llama_cpp"
}

/** Sandbox configuration applied before MCP servers or session tools start (experimental). */
data class SandboxConfig(
    /** Whether sandboxing is enabled for the session. */
    val enabled: Boolean,
    /** User-managed sandbox policy fragment merged into the auto-discovered base policy. */
    val userPolicy: Map<String, Any?>? = null,
    /** Whether to auto-add the current working directory to the read-write paths (runtime default: true). */
    val addCurrentWorkingDirectory: Boolean? = null,
    /** Whether MCP servers the session launches are confined by the sandbox (runtime default: true). */
    val sandboxMcpServers: Boolean? = null,
    /** Whether language servers the session launches are confined by the sandbox (runtime default: true). */
    val sandboxLspServers: Boolean? = null,
    /** Whether extension processes the session launches are confined by the sandbox (runtime default: true). */
    val sandboxExtensions: Boolean? = null,
    /**
     * Opt-in: lets the agent request that an individual command run outside the sandbox, which the
     * host approves or denies through the normal permission flow (default: false).
     */
    val allowBypass: Boolean? = null
)

/** Non-persisted session-level opt-in to image generation (experimental). */
@Serializable
data class ImageGenerationConfig(
    /** Opt in to image generation through an authorized Copilot image model. */
    val enabled: Boolean? = null,
    /** Request a visible "Made with AI" label on generated and edited images. */
    val watermark: Boolean? = null,
    /** Save generated images under the session-state `images` folder (runtime default: true). */
    val saveToDisk: Boolean? = null
)

/** Autopilot continuation policy (experimental). */
@Serializable
data class AutopilotOptions(
    /** Enable runtime-owned automatic continuation while in Autopilot mode (runtime default: true). */
    val continuationEnabled: Boolean? = null,
    /** Maximum automatic continuations outside an active objective (runtime default: 5; 0 allows none). */
    val maxContinues: Int? = null
)

/** Arguments passed to a [GitHubTokenProvider]. */
data class GitHubTokenProviderArgs(
    /** Effective GitHub host for which a token is required. */
    val host: String,
    /** Session receiving the token, or null before a session id is assigned. */
    val sessionId: String?,
    /** Why a token is requested: "initial" or "refresh". */
    val reason: String
)

/** Tagged token or cancellation returned by a [GitHubTokenProvider]. */
sealed class GitHubTokenProviderResult {
    /** A GitHub access token with its remaining lifetime in seconds. */
    data class Token(
        val accessToken: String,
        val expiresIn: Long,
        /** OAuth token type; defaults to bearer when omitted. */
        val tokenType: String? = null
    ) : GitHubTokenProviderResult()

    /** The host declined to supply a token. */
    object Cancelled : GitHubTokenProviderResult()
}

/** Acquires short-lived GitHub credentials for one session on demand. */
typealias GitHubTokenProvider = suspend (args: GitHubTokenProviderArgs) -> GitHubTokenProviderResult

// ============================================================================
// v3.0.0 feature types (SDK protocol 4)
// ============================================================================

/** Client modes for [CopilotClientOptions.mode]. */
object ClientMode {
    /** The default: the full Copilot coding agent, identical to earlier SDK behavior. */
    const val CODING_AGENT = "coding-agent"
    /** Every session must set `availableTools`; telemetry, repository hooks, and similar features default off. */
    const val EMPTY = "empty"
    /** Legacy alias of [CODING_AGENT]. */
    const val COPILOT_CLI = "copilot-cli"
}

/** Copilot API (CAPI) provider options, sent as `github.capi`. */
data class CapiConfig(
    /** Enables Copilot API models; set false to keep a BYOK-only model catalog. */
    val enabled: Boolean? = null,
    /** Routing preference used when the session model is `auto` (see [AutoTier]). */
    val autoTier: String? = null,
    /** Whether to use the WebSocket transport for the CAPI Responses API. */
    val enableWebSocketResponses: Boolean? = null
)

/**
 * A named BYOK provider connection, sent in `inference.providers`. Model definitions
 * ([ProviderModelConfig]) reference it by [name].
 */
data class NamedProviderConfig(
    /** Stable connection identifier; must not contain `/`. */
    val name: String,
    /** API endpoint URL. */
    val baseUrl: String,
    /** Provider type: "openai" (default), "azure", or "anthropic". */
    val type: String? = null,
    /** API key; optional for local providers. */
    val apiKey: String? = null,
    /** Command run before each request whose stdout is used as the API key. */
    val apiKeyCommand: String? = null,
    /** Bearer token sent as the Authorization header; takes precedence over [apiKey]. */
    val bearerToken: String? = null,
    /** Wire API format: "completions" (default) or "responses". */
    val wireApi: String? = null,
    /** Transport: "http" (default) or "websockets" (Responses API only). */
    val transport: String? = null,
    /** Azure-specific options. */
    val azure: ProviderConfig.AzureConfig? = null,
    /** Custom HTTP headers sent with every request to the provider. */
    val headers: Map<String, String>? = null,
    /** Product serving the models, reported in telemetry (see [ModelProvider]). */
    val modelProvider: String? = null
)

/**
 * A BYOK model definition served by a [NamedProviderConfig], sent in `inference.models`.
 * Select it with the canonical "provider/id" model ID.
 */
data class ProviderModelConfig(
    /** Name of the [NamedProviderConfig] that serves this model. */
    val provider: String,
    /** Provider-local model ID, unique within its provider. */
    val id: String,
    /** Well-known base model ID used for capability lookup; defaults to [id]. */
    val modelId: String? = null,
    /** Model or deployment name sent to the provider API; defaults to [id]. */
    val wireModel: String? = null,
    /** Display name; defaults to "provider/id". */
    val name: String? = null,
    val maxPromptTokens: Int? = null,
    val maxContextWindowTokens: Int? = null,
    val maxOutputTokens: Int? = null,
    /** Capability overrides (same shape as [SessionConfig.modelCapabilities]). */
    val capabilities: Map<String, Any?>? = null
)

/** A program's request for a secret or confirmation while a shell command runs. */
data class ShellAuthenticationRequest(
    /** Opaque request ID, unique within the session. */
    val requestId: String,
    /** Program that asked, for example "sudo" or "ssh". */
    val program: String,
    /** Prompt text the program printed. Treat it as untrusted. */
    val prompt: String,
    /** "secret", "text", or "confirm". */
    val inputKind: String,
    /** Command line that triggered the prompt (may be empty). */
    val command: String = "",
    /** Working directory of the command (may be empty). */
    val cwd: String = ""
)

/** Answer to a [ShellAuthenticationRequest]. */
sealed class ShellAuthenticationResult {
    /** Sends [value] to the program. */
    data class Accept(val value: String) : ShellAuthenticationResult() {
        override fun toString(): String = "Accept(value=***)"
    }

    /** Declines the prompt. */
    object Cancel : ShellAuthenticationResult()
}

/** Answers shell authentication prompts (`shellAuthentication.request`); returning null cancels. */
typealias ShellAuthenticationHandler = suspend (request: ShellAuthenticationRequest, sessionId: String) -> ShellAuthenticationResult?

/** A memory the agent asked to store, after validation, secret filtering, and approval. */
data class StoredMemory(
    /** ID of the session that stored the memory. */
    val sessionId: String,
    /** Short topic the memory is about. */
    val subject: String,
    /** The fact to remember. */
    val fact: String,
    /** Sources supporting the fact, such as file paths with line numbers. */
    val citations: String,
    /** Why the agent considers the fact worth remembering. */
    val reason: String
)

/** Stores the memories the agent saves (`memoryStore.store`). */
typealias MemoryStoreHandler = suspend (memory: StoredMemory) -> Unit

/** Network traffic scopes for [NetworkConfig.scope]. */
object NetworkScope {
    /** Requests made for sessions this client creates or resumes (the default). */
    const val CONNECTION = "connection"
    /** Also the runtime's shared background traffic; only one connection can hold it. */
    const val RUNTIME = "runtime"
}

/** Arguments of a `network.resolveProxy` request. */
data class NetworkResolveProxyRequest(
    /** Request target as a PAC script sees it. */
    val url: String
)

/** Arguments of a `network.authenticateNegotiate` request. */
data class NetworkAuthenticateNegotiateRequest(
    /** Service principal in `service/host[@REALM]` form. */
    val spn: String
)

/** Arguments of a `network.authenticateBasic` request. */
data class NetworkAuthenticateBasicRequest(
    /** Proxy that challenged, as `http://host:port` or `https://host:port`. */
    val proxy: String,
    /** Realm named by the proxy, or null. Untrusted text. */
    val realm: String? = null,
    /** True when the proxy rejected the credentials sent last. */
    val previousRejected: Boolean = false
)

/** Proxy credentials returned by a [BasicAuthenticator]. */
data class NetworkBasicCredentials(
    val username: String? = null,
    val password: String? = null
) {
    override fun toString(): String = "NetworkBasicCredentials(username=***, password=***)"
}

/** Returns a PAC-style proxy list (for example "PROXY proxy:8080; DIRECT"), or null for default routing. */
typealias ProxyResolver = suspend (request: NetworkResolveProxyRequest) -> String?

/** Returns a base64 SPNEGO token without the `Negotiate` prefix, or null when unavailable. */
typealias NegotiateAuthenticator = suspend (request: NetworkAuthenticateNegotiateRequest) -> String?

/** Returns credentials for a proxy's Basic challenge, or null when unavailable. */
typealias BasicAuthenticator = suspend (request: NetworkAuthenticateBasicRequest) -> NetworkBasicCredentials?

/**
 * Network callbacks through which the host takes over parts of the runtime's networking (proxy
 * resolution and proxy authentication). Registered with `network.setProvider` after the client
 * connects when at least one callback is set. Never log the tokens or passwords they return.
 */
data class NetworkConfig(
    /** Traffic the callbacks serve (see [NetworkScope]); the runtime default is "connection". */
    val scope: String? = null,
    val resolveProxy: ProxyResolver? = null,
    val authenticateNegotiate: NegotiateAuthenticator? = null,
    val authenticateBasic: BasicAuthenticator? = null
) {
    /** Returns true if at least one callback is set. */
    fun hasCallbacks(): Boolean = resolveProxy != null || authenticateNegotiate != null || authenticateBasic != null
}

/** Data of a `tool.shell_output` session event: live, append-only output of a running shell tool call. */
data class ToolShellOutputData(
    /** Tool call that owns this output. */
    val toolCallId: String,
    /** New output to append; chunks need not contain complete lines. */
    val text: String,
    /** Zero-based publication sequence across all streams of the tool call. */
    val sequence: Long,
    /** "stdout", "stderr", or null (stdout). */
    val stream: String? = null
) {
    companion object {
        /** The session event type carrying this data. */
        const val EVENT_TYPE = "tool.shell_output"

        /** Reads the data of a `tool.shell_output` event, or returns null for other events. */
        fun fromEvent(event: SessionEvent): ToolShellOutputData? {
            if (event.type != EVENT_TYPE) return null
            val data = event.data ?: return null
            return ToolShellOutputData(
                toolCallId = data.stringOrNull("toolCallId") ?: "",
                text = data.stringOrNull("text") ?: "",
                sequence = (data["sequence"] as? JsonPrimitive)?.longOrNull ?: 0L,
                stream = data.stringOrNull("stream")
            )
        }
    }
}

/**
 * The latest unresolved Policy Helper failure. Carried by `session.create` and `session.resume`
 * errors as `error.data.policyHelperFailure` and by the managed settings snapshot as
 * `policyHelperFailure`. Helper-authored text is untrusted plain text.
 */
data class PolicyHelperFailure(
    /** Stable runtime classification, for example "timeout" or "helper-reported". */
    val category: String,
    /** Effective runtime behavior: "ignore" or "failClosed". */
    val failureMode: String,
    /** Primary plain-text explanation. */
    val message: String,
    /** True when an earlier successful helper-derived layer still contributes. */
    val previousPolicyRetained: Boolean,
    /** Invocation reason, for example "sessionStart". */
    val reason: String,
    /** Managed authority that registered the helper ("device" or "server"). */
    val authority: String? = null,
    /** Helper-defined error identifier. */
    val code: String? = null,
    /** Field-specific findings (`path`, `message`). */
    val details: List<JsonObject>? = null,
    /** Secondary troubleshooting text. */
    val diagnostic: String? = null,
    /** Validated help link (`url`, `label`). */
    val helpLink: JsonObject? = null,
    /** Helper-authored recovery guidance. */
    val remediation: String? = null,
    /** "warning" or "error". */
    val severity: String? = null
) {
    companion object {
        /** Reads the failure carried by a failed `session.create` or `session.resume` request. */
        fun fromError(error: Throwable?): PolicyHelperFailure? {
            var current = error
            while (current != null) {
                if (current is JsonRpcException) {
                    return fromJson((current.data as? JsonObject)?.get("policyHelperFailure"))
                }
                current = current.cause
            }
            return null
        }

        /** Parses a failure from its wire shape, for example a managed settings snapshot's `policyHelperFailure`. */
        fun fromJson(element: JsonElement?): PolicyHelperFailure? {
            val obj = element as? JsonObject ?: return null
            return PolicyHelperFailure(
                category = obj.stringOrNull("category") ?: "",
                failureMode = obj.stringOrNull("failureMode") ?: "",
                message = obj.stringOrNull("message") ?: "",
                previousPolicyRetained = (obj["previousPolicyRetained"] as? JsonPrimitive)?.booleanOrNull ?: false,
                reason = obj.stringOrNull("reason") ?: "",
                authority = obj.stringOrNull("authority"),
                code = obj.stringOrNull("code"),
                details = (obj["details"] as? JsonArray)?.mapNotNull { it as? JsonObject },
                diagnostic = obj.stringOrNull("diagnostic"),
                helpLink = obj["helpLink"] as? JsonObject,
                remediation = obj.stringOrNull("remediation"),
                severity = obj.stringOrNull("severity")
            )
        }
    }
}

/** Permission modes for [SessionConfig.permissionMode] and [CopilotSession.setPermissionMode]. */
object PermissionMode {
    /** Permission requests follow the normal approval flow. */
    const val MANUAL = "manual"
    /** Permission requests include an LLM safety recommendation. */
    const val ASSISTED = "assisted"
    /** Tool, path, and URL permission requests are approved automatically. */
    const val ALLOW_ALL = "allow-all"
}

/** Known values of [PermissionModeResult.reason]. */
object PermissionModeRefusalReason {
    /** Managed (enterprise) policy refused the requested mode. */
    const val MANAGED_POLICY = "managed_policy"
}

/** Result of a permission-mode change: whether it applied and the authoritative mode afterwards. */
data class PermissionModeResult(
    val success: Boolean,
    /** Authoritative permission mode after the request (see [PermissionMode]). */
    val mode: String?,
    /** Why the mode was refused, when known (see [PermissionModeRefusalReason]). */
    val reason: String? = null
)

// ============================================================================
// v3.1.0 feature types (parity with the Microsoft Copilot SDK 2.0.3-preview.0)
// ============================================================================

/**
 * Image support advertised by a `userInput.request` ([UserInputRequest.imageAttachmentConstraints]).
 * Absent (null) when the runtime does not accept images in the answer.
 */
@Serializable
data class UserInputImageAttachmentConstraints(
    /** MIME types accepted by the request. */
    val allowedMimeTypes: List<String> = emptyList(),
    /** Maximum decoded byte length of each image. */
    val maxBytesPerImage: Long = 0L,
    /** Maximum number of images accepted in one answer. */
    val maxCount: Int = 0
) {
    companion object {
        /** Parses the constraints from their wire shape, or returns null when [element] is not an object. */
        fun fromJson(element: JsonElement?): UserInputImageAttachmentConstraints? {
            val obj = element as? JsonObject ?: return null
            return UserInputImageAttachmentConstraints(
                allowedMimeTypes = obj.stringListOrEmpty("allowedMimeTypes"),
                maxBytesPerImage = (obj["maxBytesPerImage"] as? JsonPrimitive)?.longOrNull ?: 0L,
                maxCount = (obj["maxCount"] as? JsonPrimitive)?.longOrNull?.toInt() ?: 0
            )
        }
    }
}

/**
 * An inline image attached to an ask_user answer ([UserInputResponse.attachments]). The wire
 * `type` is always "image"; the runtime enforces the advertised MIME types, size and count limits.
 */
@Serializable
data class UserInputImageAttachment(
    /** Image bytes encoded as standard padded base64, without a data URL prefix. */
    val data: String,
    /** MIME type of the image, matching an advertised allowed MIME type. */
    val mimeType: String,
    /** Optional description of the image. */
    val description: String? = null,
    /** Optional client-facing name of the image. */
    val displayName: String? = null
) {
    /** The attachment's wire `type`, always "image". */
    val type: String get() = TYPE

    /** Returns the wire shape, omitting unset optional fields. */
    fun toJson(): JsonObject = JsonObject(buildMap<String, JsonElement> {
        put("type", JsonPrimitive(TYPE))
        put("data", JsonPrimitive(data))
        put("mimeType", JsonPrimitive(mimeType))
        description?.let { put("description", JsonPrimitive(it)) }
        displayName?.let { put("displayName", JsonPrimitive(it)) }
    })

    companion object {
        /** The attachment's wire `type`. */
        const val TYPE = "image"
    }
}

/** Result of [CopilotClient.mcpSecretsSave]. */
data class McpSecretsSaveResult(
    /** True when the OS keychain stored the credential, false when the runtime used its file backend. */
    val usedKeychain: Boolean
) {
    companion object {
        /** Parses an `mcp.secrets.save` result from its wire shape. */
        fun fromJson(element: JsonElement?): McpSecretsSaveResult {
            val obj = element as? JsonObject ?: JsonObject(emptyMap())
            return McpSecretsSaveResult(usedKeychain = (obj["usedKeychain"] as? JsonPrimitive)?.booleanOrNull ?: false)
        }
    }
}

/**
 * A user-scoped approval for one installed plugin extension. Pass [installationKey] back unchanged
 * to [CopilotClient.extensionApprovalsRevoke].
 */
data class PermissionsExtensionApproval(
    /** Stable extension identity, for example "plugin:plugin-name:extension-name". */
    val extensionName: String,
    /** Name of the plugin that provides the extension. */
    val pluginName: String,
    /** Exact privileged capabilities the user approved. */
    val capabilities: List<String>,
    /** When the approval was granted (RFC 3339 timestamp). */
    val grantedAt: String,
    /** Opaque installation identity; pass it back unchanged to revoke. */
    val installationKey: String,
    /** Human-readable installation label. */
    val installationLabel: String? = null
) {
    companion object {
        /** Parses an approval from its wire shape, or returns null when [element] is not an object. */
        fun fromJson(element: JsonElement?): PermissionsExtensionApproval? {
            val obj = element as? JsonObject ?: return null
            return PermissionsExtensionApproval(
                extensionName = obj.stringOrNull("extensionName") ?: "",
                pluginName = obj.stringOrNull("pluginName") ?: "",
                capabilities = obj.stringListOrEmpty("capabilities"),
                grantedAt = obj.stringOrNull("grantedAt") ?: "",
                installationKey = obj.stringOrNull("installationKey") ?: "",
                installationLabel = obj.stringOrNull("installationLabel")
            )
        }
    }
}

/** Result of [CopilotClient.extensionApprovalsGrant]. */
data class PermissionsExtensionApprovalsGrantResult(
    /** Whether the pending request was eligible and persisted. */
    val granted: Boolean,
    /** The stored approval when it was persisted. */
    val approval: PermissionsExtensionApproval? = null
) {
    companion object {
        /** Parses a `permissions.extensionApprovals.grant` result from its wire shape. */
        fun fromJson(element: JsonElement?): PermissionsExtensionApprovalsGrantResult {
            val obj = element as? JsonObject ?: JsonObject(emptyMap())
            return PermissionsExtensionApprovalsGrantResult(
                granted = (obj["granted"] as? JsonPrimitive)?.booleanOrNull ?: false,
                approval = PermissionsExtensionApproval.fromJson(obj["approval"])
            )
        }
    }
}

/** Result of [CopilotClient.extensionApprovalsList]: every user-scoped plugin extension approval. */
data class PermissionsExtensionApprovalsListResult(
    val approvals: List<PermissionsExtensionApproval>
) {
    companion object {
        /** Parses a `permissions.extensionApprovals.list` result from its wire shape. */
        fun fromJson(element: JsonElement?): PermissionsExtensionApprovalsListResult {
            val approvals = ((element as? JsonObject)?.get("approvals") as? JsonArray)
                ?.mapNotNull { PermissionsExtensionApproval.fromJson(it) } ?: emptyList()
            return PermissionsExtensionApprovalsListResult(approvals)
        }
    }
}

/** Result of [CopilotClient.extensionApprovalsRevoke]. */
data class PermissionsExtensionApprovalsRevokeResult(
    /** Whether a matching approval was revoked. */
    val revoked: Boolean
) {
    companion object {
        /** Parses a `permissions.extensionApprovals.revoke` result from its wire shape. */
        fun fromJson(element: JsonElement?): PermissionsExtensionApprovalsRevokeResult {
            val obj = element as? JsonObject ?: JsonObject(emptyMap())
            return PermissionsExtensionApprovalsRevokeResult(revoked = (obj["revoked"] as? JsonPrimitive)?.booleanOrNull ?: false)
        }
    }
}

/** Returns the string elements of the array member [key], or an empty list when it is absent. */
private fun JsonObject.stringListOrEmpty(key: String): List<String> =
    (this[key] as? JsonArray)?.mapNotNull { (it as? JsonPrimitive)?.takeIf { p -> p.isString }?.content } ?: emptyList()

/** Returns the string member [key], or null when it is absent or not a string. */
private fun JsonObject.stringOrNull(key: String): String? =
    (this[key] as? JsonPrimitive)?.takeIf { it.isString }?.content
