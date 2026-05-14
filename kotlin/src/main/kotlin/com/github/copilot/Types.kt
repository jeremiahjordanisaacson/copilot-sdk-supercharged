/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject

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
    val handler: ToolHandler
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
    val azure: AzureConfig? = null
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
    val skills: List<String>? = null
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
    val url: String? = null
)

/**
 * Result returned from an elicitation handler.
 */
data class ElicitationResult(
    val action: String,
    val content: Map<String, Any?>? = null
)

/**
 * Handler for elicitation requests from the server.
 */
typealias ElicitationHandler = suspend (context: ElicitationContext) -> ElicitationResult

// ============================================================================
// Exit Plan Mode Types
// ============================================================================

/**
 * Request payload for an exit plan mode request from the server.
 */
@Serializable
data class ExitPlanModeRequest(
    val sessionId: String
)

/**
 * Response payload for an exit plan mode request.
 */
@Serializable
data class ExitPlanModeResponse(
    val approved: Boolean
)

/**
 * Handler for exit plan mode requests.
 */
typealias ExitPlanModeHandler = suspend (request: ExitPlanModeRequest) -> ExitPlanModeResponse

// ============================================================================
// Trace Context Types
// ============================================================================

/**
 * Trace context for distributed tracing.
 */
@Serializable
data class TraceContext(
    val traceparent: String? = null,
    val tracestate: String? = null
)

/**
 * Provider that returns a trace context for outbound requests.
 */
typealias TraceContextProvider = suspend () -> TraceContext

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

    /** Handler for exit plan mode requests from the server. */
    val onExitPlanMode: ExitPlanModeHandler? = null,

    /** Additional HTTP headers sent with each model request. */
    val requestHeaders: Map<String, String>? = null,
    /** Response format for image generation ("text", "image", "json_object"). */
    val responseFormat: String? = null,
    /** Idle timeout in seconds before the session is automatically closed. */
    val idleTimeout: Int? = null,
    /** Custom instruction directory paths. */
    val instructionDirectories: List<String>? = null,
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

    /** Handler for exit plan mode requests from the server. */
    val onExitPlanMode: ExitPlanModeHandler? = null,

    val disableResume: Boolean? = null,
    /** Custom instruction directory paths. */
    val instructionDirectories: List<String>? = null,
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
    val rules: List<JsonElement>? = null
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
    val allowFreeform: Boolean? = null
)

/**
 * Response to a user input request.
 */
@Serializable
data class UserInputResponse(
    val answer: String,
    val wasFreeform: Boolean
)

/**
 * Handler for user input requests.
 */
typealias UserInputHandler = suspend (request: UserInputRequest, sessionId: String) -> UserInputResponse

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
    val onUserPromptSubmitted: HookHandler? = null,
    val onSessionStart: HookHandler? = null,
    val onSessionEnd: HookHandler? = null,
    val onErrorOccurred: HookHandler? = null
) {
    /**
     * Returns true if any hook handler is registered.
     */
    fun hasAnyHook(): Boolean =
        onPreToolUse != null || onPostToolUse != null || onUserPromptSubmitted != null ||
            onSessionStart != null || onSessionEnd != null || onErrorOccurred != null

    /**
     * Gets a handler by hook type name.
     */
    fun getHandler(hookType: String): HookHandler? = when (hookType) {
        "preToolUse" -> onPreToolUse
        "postToolUse" -> onPostToolUse
        "userPromptSubmitted" -> onUserPromptSubmitted
        "sessionStart" -> onSessionStart
        "sessionEnd" -> onSessionEnd
        "errorOccurred" -> onErrorOccurred
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
    val responseFormat: ResponseFormat? = null,
    val imageOptions: ImageOptions? = null,

    /**
     * Custom HTTP headers to include in outbound model requests for this turn.
     */
    val requestHeaders: Map<String, String>? = null
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
    val timestamp: Long = 0,
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
    val maxContextWindowTokens: Int = 0
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
 * Completion type for slash command input.
 */
@Serializable
enum class SlashCommandInputCompletion {
    @SerialName("directory") DIRECTORY
}

/**
 * Kind of slash command.
 */
@Serializable
enum class SlashCommandKind {
    @SerialName("builtin") BUILTIN,
    @SerialName("client") CLIENT,
    @SerialName("skill") SKILL
}

/**
 * Price category for the model picker.
 */
@Serializable
enum class ModelPickerPriceCategory {
    @SerialName("high") HIGH,
    @SerialName("low") LOW,
    @SerialName("medium") MEDIUM,
    @SerialName("very_high") VERY_HIGH
}

/**
 * Token prices for model billing.
 */
@Serializable
data class ModelBillingTokenPrices(
    val batchSize: Int? = null,
    val cachePrice: Int? = null,
    val inputPrice: Int? = null,
    val outputPrice: Int? = null
)

/**
 * Model billing information.
 */
@Serializable
data class ModelBilling(
    val multiplier: Double,
    val tokenPrices: ModelBillingTokenPrices? = null,
    val pickerPriceCategory: ModelPickerPriceCategory? = null
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
// Slash Command Types
// ============================================================================

/**
 * Input configuration for a slash command.
 */
@Serializable
data class SlashCommandInput(
    val hint: String,
    val completion: SlashCommandInputCompletion? = null
)

/**
 * Information about a slash command.
 */
@Serializable
data class SlashCommandInfo(
    val allowDuringAgentExecution: Boolean,
    val description: String,
    val kind: SlashCommandKind,
    val name: String,
    val aliases: List<String>? = null,
    val experimental: Boolean? = null,
    val input: SlashCommandInput? = null
)

// ============================================================================
// Command Request Types
// ============================================================================

/**
 * Request to invoke a command.
 */
@Serializable
data class CommandsInvokeRequest(
    val name: String,
    val input: String? = null
)

/**
 * Request to list commands.
 */
@Serializable
data class CommandsListRequest(
    val includeBuiltins: Boolean? = null,
    val includeClientCommands: Boolean? = null,
    val includeSkills: Boolean? = null
)

/**
 * Experimental: Diagnostics from loading skills.
 */
@Serializable
data class SkillsLoadDiagnostics(
    val errors: List<String>,
    val warnings: List<String>
)

// ============================================================================
// Remote Session Types
// ============================================================================

/**
 * Per-session remote mode. "off" disables remote, "export" exports session
 * events to Mission Control without enabling remote steering, "on" enables
 * both export and remote steering.
 */
@Serializable
enum class RemoteSessionMode {
    @SerialName("export") EXPORT,
    @SerialName("off") OFF,
    @SerialName("on") ON
}

/**
 * Experimental: Request to enable remote mode for a session.
 */
@Serializable
data class RemoteEnableRequest(
    /** Per-session remote mode. */
    val mode: RemoteSessionMode? = null
)

/**
 * Experimental: Result of enabling remote mode for a session.
 */
@Serializable
data class RemoteEnableResult(
    /** Whether remote steering is enabled. */
    val remoteSteerable: Boolean,
    /** Mission Control frontend URL for this session. */
    val url: String? = null
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
     * Provider for trace context to include in outbound requests.
     */
    val onGetTraceContext: TraceContextProvider? = null
)
