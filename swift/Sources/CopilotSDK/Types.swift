// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import Foundation

// MARK: - Connection State

/// Represents the client connection state.
public enum ConnectionState: String, Sendable {
    case disconnected
    case connecting
    case connected
    case error
}

// MARK: - Tool Result Types

/// The outcome type of a tool invocation.
public enum ToolResultType: String, Codable, Sendable {
    case success
    case failure
    case rejected
    case denied
}

/// Binary data returned by a tool.
public struct ToolBinaryResult: Codable, Sendable {
    public var data: String
    public var mimeType: String
    public var type: String
    public var description: String?

    public init(data: String, mimeType: String, type: String, description: String? = nil) {
        self.data = data
        self.mimeType = mimeType
        self.type = type
        self.description = description
    }
}

/// Structured result from a tool invocation.
public struct ToolResultObject: Codable, Sendable {
    public var textResultForLlm: String
    public var binaryResultsForLlm: [ToolBinaryResult]?
    public var resultType: ToolResultType
    public var error: String?
    public var sessionLog: String?
    public var toolTelemetry: [String: AnyCodable]?

    public init(
        textResultForLlm: String,
        resultType: ToolResultType,
        binaryResultsForLlm: [ToolBinaryResult]? = nil,
        error: String? = nil,
        sessionLog: String? = nil,
        toolTelemetry: [String: AnyCodable]? = nil
    ) {
        self.textResultForLlm = textResultForLlm
        self.resultType = resultType
        self.binaryResultsForLlm = binaryResultsForLlm
        self.error = error
        self.sessionLog = sessionLog
        self.toolTelemetry = toolTelemetry
    }
}

/// A tool result can be either a simple string or a structured object.
public enum ToolResult: Sendable {
    case string(String)
    case object(ToolResultObject)

    /// Converts to a ToolResultObject for wire format.
    public func toResultObject() -> ToolResultObject {
        switch self {
        case .string(let s):
            return ToolResultObject(textResultForLlm: s, resultType: .success)
        case .object(let obj):
            return obj
        }
    }
}

// MARK: - Tool Invocation

/// Context passed to a tool handler when the tool is called.
public struct ToolInvocation: Sendable {
    public let sessionId: String
    public let toolCallId: String
    public let toolName: String
    public let arguments: Any? // The decoded JSON arguments

    public init(sessionId: String, toolCallId: String, toolName: String, arguments: Any?) {
        self.sessionId = sessionId
        self.toolCallId = toolCallId
        self.toolName = toolName
        self.arguments = arguments
    }
}

/// A handler function invoked when a tool is called.
public typealias ToolHandler = @Sendable (Any?, ToolInvocation) async throws -> Any?

// MARK: - Tool Definition

/// Describes a tool that can be invoked by the Copilot agent.
public struct Tool: Sendable {
    public let name: String
    public let description: String?
    public let parameters: [String: Any]?
    public let handler: ToolHandler

    public init(
        name: String,
        description: String? = nil,
        parameters: [String: Any]? = nil,
        handler: @escaping ToolHandler
    ) {
        self.name = name
        self.description = description
        self.parameters = parameters
        self.handler = handler
    }

    /// Converts the tool definition to a dictionary suitable for sending over JSON-RPC.
    func toWireFormat() -> [String: Any] {
        var dict: [String: Any] = ["name": name]
        if let description = description {
            dict["description"] = description
        }
        if let parameters = parameters {
            dict["parameters"] = parameters
        }
        return dict
    }
}

// MARK: - System Message Configuration

/// Known system prompt section identifiers for the "customize" mode.
public struct SystemPromptSection {
    public static let identity = "identity"
    public static let tone = "tone"
    public static let toolEfficiency = "tool_efficiency"
    public static let environmentContext = "environment_context"
    public static let codeChangeRules = "code_change_rules"
    public static let guidelines = "guidelines"
    public static let safety = "safety"
    public static let toolInstructions = "tool_instructions"
    public static let customInstructions = "custom_instructions"
    public static let lastInstructions = "last_instructions"
}

/// Override action for a system prompt section.
public struct SectionOverrideAction {
    public static let replace = "replace"
    public static let remove = "remove"
    public static let append = "append"
    public static let prepend = "prepend"
}

/// Override operation for a single system prompt section.
public struct SectionOverride: Codable, Sendable {
    public var action: String
    public var content: String?

    public init(action: String, content: String? = nil) {
        self.action = action
        self.content = content
    }
}

/// System message configuration for session creation.
///
/// - Append mode (default): SDK foundation + optional custom content
/// - Replace mode: Full control, caller provides entire system message
/// - Customize mode: Section-level overrides with graceful fallback
public struct SystemMessageConfig: Codable, Sendable {
    /// The configuration mode: "append", "replace", or "customize".
    public var mode: String?
    /// Content string. For append: appended after SDK sections. For replace: the entire message.
    /// For customize: additional content appended after all sections.
    public var content: String?
    /// Section overrides, keyed by section identifier. Only used in "customize" mode.
    public var sections: [String: SectionOverride]?

    public init(mode: String? = nil, content: String? = nil, sections: [String: SectionOverride]? = nil) {
        self.mode = mode
        self.content = content
        self.sections = sections
    }

    /// Append mode: SDK foundation + optional custom content.
    public static func append(content: String? = nil) -> SystemMessageConfig {
        SystemMessageConfig(mode: "append", content: content)
    }

    /// Replace mode: caller provides entire system message.
    public static func replace(content: String) -> SystemMessageConfig {
        SystemMessageConfig(mode: "replace", content: content)
    }

    /// Customize mode: override individual sections of the system prompt.
    public static func customize(sections: [String: SectionOverride]? = nil, content: String? = nil) -> SystemMessageConfig {
        SystemMessageConfig(mode: "customize", content: content, sections: sections)
    }
}

// MARK: - Permission Types

/// A permission request from the server.
public struct PermissionRequest: Sendable {
    public let kind: String
    public let toolCallId: String?
    public let rawData: [String: Any]

    public init(kind: String, toolCallId: String? = nil, rawData: [String: Any] = [:]) {
        self.kind = kind
        self.toolCallId = toolCallId
        self.rawData = rawData
    }
}

/// The result of a permission decision.
public struct PermissionRequestResult: Codable, Sendable {
    public let kind: String
    public let rules: [AnyCodable]?

    public init(kind: String, rules: [AnyCodable]? = nil) {
        self.kind = kind
        self.rules = rules
    }

    /// Convenience: approved.
    public static let approved = PermissionRequestResult(kind: "approved")

    /// Convenience: denied by rules.
    public static let deniedByRules = PermissionRequestResult(kind: "denied-by-rules")

    /// Convenience: denied because no approval rule and could not request from user.
    public static let denied = PermissionRequestResult(
        kind: "denied-no-approval-rule-and-could-not-request-from-user"
    )

    /// Convenience: denied interactively by user.
    public static let deniedByUser = PermissionRequestResult(kind: "denied-interactively-by-user")
}

/// Handler for permission requests.
public typealias PermissionHandler = @Sendable (PermissionRequest, String) async throws
    -> PermissionRequestResult

// MARK: - User Input Types

/// A request for user input from the agent (ask_user tool).
public struct UserInputRequest: Codable, Sendable {
    public let question: String
    public let choices: [String]?
    public let allowFreeform: Bool?

    public init(question: String, choices: [String]? = nil, allowFreeform: Bool? = nil) {
        self.question = question
        self.choices = choices
        self.allowFreeform = allowFreeform
    }
}

/// The user's response to an input request.
public struct UserInputResponse: Codable, Sendable {
    public let answer: String
    public let wasFreeform: Bool

    public init(answer: String, wasFreeform: Bool) {
        self.answer = answer
        self.wasFreeform = wasFreeform
    }
}

/// Handler for user input requests.
public typealias UserInputHandler = @Sendable (UserInputRequest, String) async throws
    -> UserInputResponse

// MARK: - Hook Types

/// Base hook input with timestamp and cwd.
public struct BaseHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
}

/// Input for pre-tool-use hook.
public struct PreToolUseHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let toolName: String
    public let toolArgs: AnyCodable?
}

/// Output for pre-tool-use hook.
public struct PreToolUseHookOutput: Codable, Sendable {
    public var permissionDecision: String?
    public var permissionDecisionReason: String?
    public var modifiedArgs: AnyCodable?
    public var additionalContext: String?
    public var suppressOutput: Bool?

    public init(
        permissionDecision: String? = nil,
        permissionDecisionReason: String? = nil,
        modifiedArgs: AnyCodable? = nil,
        additionalContext: String? = nil,
        suppressOutput: Bool? = nil
    ) {
        self.permissionDecision = permissionDecision
        self.permissionDecisionReason = permissionDecisionReason
        self.modifiedArgs = modifiedArgs
        self.additionalContext = additionalContext
        self.suppressOutput = suppressOutput
    }
}

/// Handler for pre-tool-use hook.
public typealias PreToolUseHandler = @Sendable (PreToolUseHookInput, String) async throws
    -> PreToolUseHookOutput?

/// Input for post-tool-use hook.
public struct PostToolUseHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let toolName: String
    public let toolArgs: AnyCodable?
    public let toolResult: AnyCodable?
}

/// Output for post-tool-use hook.
public struct PostToolUseHookOutput: Codable, Sendable {
    public var modifiedResult: AnyCodable?
    public var additionalContext: String?
    public var suppressOutput: Bool?

    public init(
        modifiedResult: AnyCodable? = nil,
        additionalContext: String? = nil,
        suppressOutput: Bool? = nil
    ) {
        self.modifiedResult = modifiedResult
        self.additionalContext = additionalContext
        self.suppressOutput = suppressOutput
    }
}

/// Handler for post-tool-use hook.
public typealias PostToolUseHandler = @Sendable (PostToolUseHookInput, String) async throws
    -> PostToolUseHookOutput?

/// Input for user-prompt-submitted hook.
public struct UserPromptSubmittedHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let prompt: String
}

/// Output for user-prompt-submitted hook.
public struct UserPromptSubmittedHookOutput: Codable, Sendable {
    public var modifiedPrompt: String?
    public var additionalContext: String?
    public var suppressOutput: Bool?

    public init(
        modifiedPrompt: String? = nil,
        additionalContext: String? = nil,
        suppressOutput: Bool? = nil
    ) {
        self.modifiedPrompt = modifiedPrompt
        self.additionalContext = additionalContext
        self.suppressOutput = suppressOutput
    }
}

/// Handler for user-prompt-submitted hook.
public typealias UserPromptSubmittedHandler = @Sendable (
    UserPromptSubmittedHookInput, String
) async throws -> UserPromptSubmittedHookOutput?

/// Input for session-start hook.
public struct SessionStartHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let source: String
    public let initialPrompt: String?
}

/// Output for session-start hook.
public struct SessionStartHookOutput: Codable, Sendable {
    public var additionalContext: String?
    public var modifiedConfig: [String: AnyCodable]?

    public init(
        additionalContext: String? = nil,
        modifiedConfig: [String: AnyCodable]? = nil
    ) {
        self.additionalContext = additionalContext
        self.modifiedConfig = modifiedConfig
    }
}

/// Handler for session-start hook.
public typealias SessionStartHandler = @Sendable (SessionStartHookInput, String) async throws
    -> SessionStartHookOutput?

/// Input for session-end hook.
public struct SessionEndHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let reason: String
    public let finalMessage: String?
    public let error: String?
}

/// Output for session-end hook.
public struct SessionEndHookOutput: Codable, Sendable {
    public var suppressOutput: Bool?
    public var cleanupActions: [String]?
    public var sessionSummary: String?

    public init(
        suppressOutput: Bool? = nil,
        cleanupActions: [String]? = nil,
        sessionSummary: String? = nil
    ) {
        self.suppressOutput = suppressOutput
        self.cleanupActions = cleanupActions
        self.sessionSummary = sessionSummary
    }
}

/// Handler for session-end hook.
public typealias SessionEndHandler = @Sendable (SessionEndHookInput, String) async throws
    -> SessionEndHookOutput?

/// Input for error-occurred hook.
public struct ErrorOccurredHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let error: String
    public let errorContext: String
    public let recoverable: Bool
}

/// Output for error-occurred hook.
public struct ErrorOccurredHookOutput: Codable, Sendable {
    public var suppressOutput: Bool?
    public var errorHandling: String?
    public var retryCount: Int?
    public var userNotification: String?

    public init(
        suppressOutput: Bool? = nil,
        errorHandling: String? = nil,
        retryCount: Int? = nil,
        userNotification: String? = nil
    ) {
        self.suppressOutput = suppressOutput
        self.errorHandling = errorHandling
        self.retryCount = retryCount
        self.userNotification = userNotification
    }
}

/// Handler for error-occurred hook.
public typealias ErrorOccurredHandler = @Sendable (ErrorOccurredHookInput, String) async throws
    -> ErrorOccurredHookOutput?

/// Configuration for session hooks.
public struct SessionHooks: Sendable {
    public var onPreToolUse: PreToolUseHandler?
    public var onPostToolUse: PostToolUseHandler?
    public var onUserPromptSubmitted: UserPromptSubmittedHandler?
    public var onSessionStart: SessionStartHandler?
    public var onSessionEnd: SessionEndHandler?
    public var onErrorOccurred: ErrorOccurredHandler?

    public init(
        onPreToolUse: PreToolUseHandler? = nil,
        onPostToolUse: PostToolUseHandler? = nil,
        onUserPromptSubmitted: UserPromptSubmittedHandler? = nil,
        onSessionStart: SessionStartHandler? = nil,
        onSessionEnd: SessionEndHandler? = nil,
        onErrorOccurred: ErrorOccurredHandler? = nil
    ) {
        self.onPreToolUse = onPreToolUse
        self.onPostToolUse = onPostToolUse
        self.onUserPromptSubmitted = onUserPromptSubmitted
        self.onSessionStart = onSessionStart
        self.onSessionEnd = onSessionEnd
        self.onErrorOccurred = onErrorOccurred
    }

    /// Returns true if any hook handler is set.
    public var hasAnyHandler: Bool {
        onPreToolUse != nil || onPostToolUse != nil || onUserPromptSubmitted != nil
            || onSessionStart != nil || onSessionEnd != nil || onErrorOccurred != nil
    }
}

// MARK: - MCP Server Configuration

/// Configuration for MCP server connections.
/// Use the static factory methods to create local or remote configs.
public struct MCPServerConfig: Codable, Sendable {
    public var tools: [String]
    public var type: String?
    public var timeout: Int?
    public var command: String?
    public var args: [String]?
    public var env: [String: String]?
    public var cwd: String?
    public var url: String?
    public var headers: [String: String]?

    /// Creates a local/stdio MCP server configuration.
    public static func local(
        command: String,
        args: [String] = [],
        tools: [String] = ["*"],
        env: [String: String]? = nil,
        cwd: String? = nil,
        timeout: Int? = nil
    ) -> MCPServerConfig {
        MCPServerConfig(
            tools: tools, type: "local", timeout: timeout,
            command: command, args: args, env: env, cwd: cwd
        )
    }

    /// Creates a remote HTTP MCP server configuration.
    public static func http(
        url: String,
        tools: [String] = ["*"],
        headers: [String: String]? = nil,
        timeout: Int? = nil
    ) -> MCPServerConfig {
        MCPServerConfig(
            tools: tools, type: "http", timeout: timeout,
            url: url, headers: headers
        )
    }

    /// Creates a remote SSE MCP server configuration.
    public static func sse(
        url: String,
        tools: [String] = ["*"],
        headers: [String: String]? = nil,
        timeout: Int? = nil
    ) -> MCPServerConfig {
        MCPServerConfig(
            tools: tools, type: "sse", timeout: timeout,
            url: url, headers: headers
        )
    }
}

// MARK: - Custom Agent Configuration

/// Configuration for a custom agent.
public struct CustomAgentConfig: Codable, Sendable {
    public var name: String
    public var displayName: String?
    public var description: String?
    public var tools: [String]?
    public var prompt: String
    public var mcpServers: [String: MCPServerConfig]?
    public var infer: Bool?
    /// List of skill names to preload into this agent's context.
    public var skills: [String]?

    public init(
        name: String,
        prompt: String,
        displayName: String? = nil,
        description: String? = nil,
        tools: [String]? = nil,
        mcpServers: [String: MCPServerConfig]? = nil,
        infer: Bool? = nil,
        skills: [String]? = nil
    ) {
        self.name = name
        self.prompt = prompt
        self.displayName = displayName
        self.description = description
        self.tools = tools
        self.mcpServers = mcpServers
        self.infer = infer
        self.skills = skills
    }
}

// MARK: - Infinite Session Configuration

/// Configuration for infinite sessions with automatic context compaction.
public struct InfiniteSessionConfig: Codable, Sendable {
    public var enabled: Bool?
    public var backgroundCompactionThreshold: Double?
    public var bufferExhaustionThreshold: Double?

    public init(
        enabled: Bool? = nil,
        backgroundCompactionThreshold: Double? = nil,
        bufferExhaustionThreshold: Double? = nil
    ) {
        self.enabled = enabled
        self.backgroundCompactionThreshold = backgroundCompactionThreshold
        self.bufferExhaustionThreshold = bufferExhaustionThreshold
    }
}

// MARK: - Provider Configuration

/// Configuration for a custom model API provider (BYOK).
public struct ProviderConfig: Codable, Sendable {
    public var type: String?
    public var wireApi: String?
    public var baseUrl: String
    public var apiKey: String?
    public var bearerToken: String?
    public var azure: AzureProviderOptions?

    public init(
        baseUrl: String,
        type: String? = nil,
        wireApi: String? = nil,
        apiKey: String? = nil,
        bearerToken: String? = nil,
        azure: AzureProviderOptions? = nil
    ) {
        self.baseUrl = baseUrl
        self.type = type
        self.wireApi = wireApi
        self.apiKey = apiKey
        self.bearerToken = bearerToken
        self.azure = azure
    }
}

/// Azure-specific provider options.
public struct AzureProviderOptions: Codable, Sendable {
    public var apiVersion: String?

    public init(apiVersion: String? = nil) {
        self.apiVersion = apiVersion
    }
}

// MARK: - Reasoning Effort

/// Valid reasoning effort levels.
public enum ReasoningEffort: String, Codable, Sendable {
    case low
    case medium
    case high
    case xhigh
}

// MARK: - Commands

/// Context for a slash-command invocation.
public struct CommandContext: Sendable {
    /// Session ID where the command was invoked.
    public var sessionId: String
    /// The full command text (e.g. "/deploy production").
    public var command: String
    /// Command name without leading /.
    public var commandName: String
    /// Raw argument string after the command name.
    public var args: String

    public init(sessionId: String, command: String, commandName: String, args: String) {
        self.sessionId = sessionId
        self.command = command
        self.commandName = commandName
        self.args = args
    }
}

/// Handler invoked when a registered slash-command is executed.
public typealias CommandHandler = @Sendable (CommandContext) async throws -> Void

/// Definition of a slash command registered with the session.
public struct CommandDefinition: Sendable {
    /// Command name (without leading /).
    public var name: String
    /// Human-readable description shown in command completion UI.
    public var description: String?
    /// Handler invoked when the command is executed.
    public var handler: CommandHandler

    public init(name: String, description: String? = nil, handler: @escaping CommandHandler) {
        self.name = name
        self.description = description
        self.handler = handler
    }
}

// MARK: - UI Elicitation

/// Context for an elicitation request from the server.
public struct ElicitationContext: Sendable {
    /// Session ID that triggered the request.
    public var sessionId: String
    /// Prompt message describing what information is needed.
    public var message: String
    /// JSON Schema describing the form fields (optional).
    public var requestedSchema: [String: AnyCodable]?
    /// Elicitation mode ("form" or "url").
    public var mode: String?
    /// Source that initiated the request.
    public var elicitationSource: String?
    /// URL to open in the user's browser (url mode only).
    public var url: String?

    public init(
        sessionId: String,
        message: String,
        requestedSchema: [String: AnyCodable]? = nil,
        mode: String? = nil,
        elicitationSource: String? = nil,
        url: String? = nil
    ) {
        self.sessionId = sessionId
        self.message = message
        self.requestedSchema = requestedSchema
        self.mode = mode
        self.elicitationSource = elicitationSource
        self.url = url
    }
}

/// Result returned from an elicitation handler.
public struct ElicitationResult: Sendable {
    /// User action: "accept", "decline", or "cancel".
    public var action: String
    /// Form values submitted by the user (present when action is "accept").
    public var content: [String: AnyCodable]?

    public init(action: String, content: [String: AnyCodable]? = nil) {
        self.action = action
        self.content = content
    }
}

/// Handler for elicitation requests from the server.
public typealias ElicitationHandler = @Sendable (ElicitationContext) async throws
    -> ElicitationResult

// MARK: - Session Configuration

/// Configuration for creating a new session.
public struct SessionConfig: Sendable {
    public var sessionId: String?
    public var model: String?
    public var reasoningEffort: ReasoningEffort?
    public var configDir: String?
    public var tools: [Tool]?
    public var systemMessage: SystemMessageConfig?
    public var availableTools: [String]?
    public var excludedTools: [String]?
    public var provider: ProviderConfig?
    public var onPermissionRequest: PermissionHandler?
    public var onUserInputRequest: UserInputHandler?
    public var hooks: SessionHooks?
    public var workingDirectory: String?
    public var streaming: Bool?
    /// Include sub-agent streaming events in the event stream. Defaults to true.
    public var includeSubAgentStreamingEvents: Bool?
    public var mcpServers: [String: MCPServerConfig]?
    public var customAgents: [CustomAgentConfig]?
    public var skillDirectories: [String]?
    public var disabledSkills: [String]?
    public var infiniteSessions: InfiniteSessionConfig?
    /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
    public var modelCapabilities: [String: AnyCodable]?
    /// When true, automatically discovers MCP server configurations from the working directory. Defaults to false.
    public var enableConfigDiscovery: Bool?
    /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
    public var gitHubToken: String?
    /// Slash commands registered for this session.
    public var commands: [CommandDefinition]?
    /// Handler for elicitation requests from the server.
    public var onElicitationRequest: ElicitationHandler?

    /// Directories to search for instruction files.
    public var instructionDirectories: [String]?

    public init(
        sessionId: String? = nil,
        model: String? = nil,
        reasoningEffort: ReasoningEffort? = nil,
        configDir: String? = nil,
        tools: [Tool]? = nil,
        systemMessage: SystemMessageConfig? = nil,
        availableTools: [String]? = nil,
        excludedTools: [String]? = nil,
        provider: ProviderConfig? = nil,
        onPermissionRequest: PermissionHandler? = nil,
        onUserInputRequest: UserInputHandler? = nil,
        hooks: SessionHooks? = nil,
        workingDirectory: String? = nil,
        streaming: Bool? = nil,
        includeSubAgentStreamingEvents: Bool? = nil,
        mcpServers: [String: MCPServerConfig]? = nil,
        customAgents: [CustomAgentConfig]? = nil,
        skillDirectories: [String]? = nil,
        disabledSkills: [String]? = nil,
        infiniteSessions: InfiniteSessionConfig? = nil,
        modelCapabilities: [String: AnyCodable]? = nil,
        enableConfigDiscovery: Bool? = nil,
        gitHubToken: String? = nil,
        commands: [CommandDefinition]? = nil,
        onElicitationRequest: ElicitationHandler? = nil,
        instructionDirectories: [String]? = nil
    ) {
        self.sessionId = sessionId
        self.model = model
        self.reasoningEffort = reasoningEffort
        self.configDir = configDir
        self.tools = tools
        self.systemMessage = systemMessage
        self.availableTools = availableTools
        self.excludedTools = excludedTools
        self.provider = provider
        self.onPermissionRequest = onPermissionRequest
        self.onUserInputRequest = onUserInputRequest
        self.hooks = hooks
        self.workingDirectory = workingDirectory
        self.streaming = streaming
        self.includeSubAgentStreamingEvents = includeSubAgentStreamingEvents
        self.mcpServers = mcpServers
        self.customAgents = customAgents
        self.skillDirectories = skillDirectories
        self.disabledSkills = disabledSkills
        self.infiniteSessions = infiniteSessions
        self.modelCapabilities = modelCapabilities
        self.enableConfigDiscovery = enableConfigDiscovery
        self.gitHubToken = gitHubToken
        self.commands = commands
        self.onElicitationRequest = onElicitationRequest
        self.instructionDirectories = instructionDirectories
    }
}

/// Configuration for resuming a session.
public struct ResumeSessionConfig: Sendable {
    public var model: String?
    public var reasoningEffort: ReasoningEffort?
    public var configDir: String?
    public var tools: [Tool]?
    public var systemMessage: SystemMessageConfig?
    public var availableTools: [String]?
    public var excludedTools: [String]?
    public var provider: ProviderConfig?
    public var onPermissionRequest: PermissionHandler?
    public var onUserInputRequest: UserInputHandler?
    public var hooks: SessionHooks?
    public var workingDirectory: String?
    public var streaming: Bool?
    /// Include sub-agent streaming events in the event stream. Defaults to true.
    public var includeSubAgentStreamingEvents: Bool?
    public var mcpServers: [String: MCPServerConfig]?
    public var customAgents: [CustomAgentConfig]?
    public var skillDirectories: [String]?
    public var disabledSkills: [String]?
    public var infiniteSessions: InfiniteSessionConfig?
    /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
    public var modelCapabilities: [String: AnyCodable]?
    /// When true, automatically discovers MCP server configurations from the working directory. Defaults to false.
    public var enableConfigDiscovery: Bool?
    /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
    public var gitHubToken: String?
    public var disableResume: Bool?
    /// Slash commands registered for this session.
    public var commands: [CommandDefinition]?
    /// Handler for elicitation requests from the server.
    public var onElicitationRequest: ElicitationHandler?

    /// Directories to search for instruction files.
    public var instructionDirectories: [String]?

    public init(
        model: String? = nil,
        reasoningEffort: ReasoningEffort? = nil,
        configDir: String? = nil,
        tools: [Tool]? = nil,
        systemMessage: SystemMessageConfig? = nil,
        availableTools: [String]? = nil,
        excludedTools: [String]? = nil,
        provider: ProviderConfig? = nil,
        onPermissionRequest: PermissionHandler? = nil,
        onUserInputRequest: UserInputHandler? = nil,
        hooks: SessionHooks? = nil,
        workingDirectory: String? = nil,
        streaming: Bool? = nil,
        includeSubAgentStreamingEvents: Bool? = nil,
        mcpServers: [String: MCPServerConfig]? = nil,
        customAgents: [CustomAgentConfig]? = nil,
        skillDirectories: [String]? = nil,
        disabledSkills: [String]? = nil,
        infiniteSessions: InfiniteSessionConfig? = nil,
        modelCapabilities: [String: AnyCodable]? = nil,
        enableConfigDiscovery: Bool? = nil,
        gitHubToken: String? = nil,
        disableResume: Bool? = nil,
        commands: [CommandDefinition]? = nil,
        onElicitationRequest: ElicitationHandler? = nil,
        instructionDirectories: [String]? = nil
    ) {
        self.model = model
        self.reasoningEffort = reasoningEffort
        self.configDir = configDir
        self.tools = tools
        self.systemMessage = systemMessage
        self.availableTools = availableTools
        self.excludedTools = excludedTools
        self.provider = provider
        self.onPermissionRequest = onPermissionRequest
        self.onUserInputRequest = onUserInputRequest
        self.hooks = hooks
        self.workingDirectory = workingDirectory
        self.streaming = streaming
        self.includeSubAgentStreamingEvents = includeSubAgentStreamingEvents
        self.mcpServers = mcpServers
        self.customAgents = customAgents
        self.skillDirectories = skillDirectories
        self.disabledSkills = disabledSkills
        self.infiniteSessions = infiniteSessions
        self.modelCapabilities = modelCapabilities
        self.enableConfigDiscovery = enableConfigDiscovery
        self.gitHubToken = gitHubToken
        self.disableResume = disableResume
        self.commands = commands
        self.onElicitationRequest = onElicitationRequest
        self.instructionDirectories = instructionDirectories
    }
}

// MARK: - Response Format

/// Response format for message responses.
public enum ResponseFormat: String, Codable, Sendable {
    case text
    case image
    case jsonObject = "json_object"
}

// MARK: - Image Generation Types

/// Options for image generation.
public struct ImageOptions: Codable, Sendable {
    /// Image size (e.g. "1024x1024")
    public var size: String?
    /// Image quality ("hd" or "standard")
    public var quality: String?
    /// Image style ("natural" or "vivid")
    public var style: String?

    public init(size: String? = nil, quality: String? = nil, style: String? = nil) {
        self.size = size
        self.quality = quality
        self.style = style
    }
}

/// Image data from an assistant image response.
public struct AssistantImageData: Codable, Sendable {
    /// Image format ("png", "jpeg", "webp")
    public var format: String
    /// Base64-encoded image bytes
    public var base64: String
    /// Optional temporary URL for the image
    public var url: String?
    /// The prompt the model actually used
    public var revisedPrompt: String?
    /// Image width in pixels
    public var width: Int
    /// Image height in pixels
    public var height: Int
}

/// A content block in a mixed text+image response.
public enum ContentBlock: Codable, Sendable {
    case text(String)
    case image(AssistantImageData)

    enum CodingKeys: String, CodingKey {
        case type, text, image
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let type = try container.decode(String.self, forKey: .type)
        switch type {
        case "text":
            let text = try container.decode(String.self, forKey: .text)
            self = .text(text)
        case "image":
            let image = try container.decode(AssistantImageData.self, forKey: .image)
            self = .image(image)
        default:
            throw DecodingError.dataCorruptedError(
                forKey: .type, in: container,
                debugDescription: "Unknown content block type: \(type)")
        }
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .text(let text):
            try container.encode("text", forKey: .type)
            try container.encode(text, forKey: .text)
        case .image(let image):
            try container.encode("image", forKey: .type)
            try container.encode(image, forKey: .image)
        }
    }
}

// MARK: - Message Options

/// Attachment types for messages.
public enum Attachment: Sendable {
    case file(path: String, displayName: String? = nil)
    case directory(path: String, displayName: String? = nil)
    case selection(
        filePath: String, displayName: String, text: String? = nil,
        startLine: Int? = nil, startCharacter: Int? = nil,
        endLine: Int? = nil, endCharacter: Int? = nil
    )

    /// Converts to a dictionary for wire format.
    func toDict() -> [String: Any] {
        switch self {
        case .file(let path, let displayName):
            var dict: [String: Any] = ["type": "file", "path": path]
            if let dn = displayName { dict["displayName"] = dn }
            return dict
        case .directory(let path, let displayName):
            var dict: [String: Any] = ["type": "directory", "path": path]
            if let dn = displayName { dict["displayName"] = dn }
            return dict
        case .selection(let filePath, let displayName, let text, let sl, let sc, let el, let ec):
            var dict: [String: Any] = [
                "type": "selection",
                "filePath": filePath,
                "displayName": displayName,
            ]
            if let t = text { dict["text"] = t }
            if let sl = sl, let sc = sc, let el = el, let ec = ec {
                dict["selection"] = [
                    "start": ["line": sl, "character": sc],
                    "end": ["line": el, "character": ec],
                ]
            }
            return dict
        }
    }
}

/// Options for sending a message to a session.
public struct MessageOptions: Sendable {
    public var prompt: String
    public var attachments: [Attachment]?
    public var mode: String?
    public var responseFormat: ResponseFormat?
    public var imageOptions: ImageOptions?
    /// Custom HTTP headers to include in outbound model requests for this turn.
    public var requestHeaders: [String: String]?

    public init(
        prompt: String,
        attachments: [Attachment]? = nil,
        mode: String? = nil,
        responseFormat: ResponseFormat? = nil,
        imageOptions: ImageOptions? = nil,
        requestHeaders: [String: String]? = nil
    ) {
        self.prompt = prompt
        self.attachments = attachments
        self.mode = mode
        self.responseFormat = responseFormat
        self.imageOptions = imageOptions
        self.requestHeaders = requestHeaders
    }
}

// MARK: - Session Event

/// A session event received from the server.
/// The `data` field is a dictionary because event types vary widely.
public struct SessionEvent: Sendable {
    public let id: String
    public let timestamp: String
    public let parentId: String?
    public let agentId: String?
    public let ephemeral: Bool?
    public let type: String
    public let data: [String: Any]

    public init(
        id: String, timestamp: String, parentId: String?, agentId: String? = nil, ephemeral: Bool?,
        type: String, data: [String: Any]
    ) {
        self.id = id
        self.timestamp = timestamp
        self.parentId = parentId
        self.ephemeral = ephemeral
        self.type = type
        self.data = data
    }

    /// Parses a SessionEvent from a raw dictionary.
    public static func from(dict: [String: Any]) -> SessionEvent? {
        guard let id = dict["id"] as? String,
            let timestamp = dict["timestamp"] as? String,
            let type = dict["type"] as? String,
            let data = dict["data"] as? [String: Any]
        else { return nil }

        return SessionEvent(
            id: id,
            timestamp: timestamp,
            parentId: dict["parentId"] as? String,
            agentId: dict["agentId"] as? String,
            ephemeral: dict["ephemeral"] as? Bool,
            type: type,
            data: data
        )
    }
}

/// Handler for session events.
public typealias SessionEventHandler = @Sendable (SessionEvent) -> Void

// MARK: - Ping Response

/// Response from the server's ping method.
public struct PingResponse: Codable, Sendable {
    public let message: String
    public let timestamp: Int64
    public let protocolVersion: Int?
}

// MARK: - Status Responses

/// Response from status.get.
public struct GetStatusResponse: Codable, Sendable {
    public let version: String
    public let protocolVersion: Int
}

/// Response from auth.getStatus.
public struct GetAuthStatusResponse: Codable, Sendable {
    public let isAuthenticated: Bool
    public let authType: String?
    public let host: String?
    public let login: String?
    public let statusMessage: String?
}

// MARK: - Model Info

/// Model support flags.
public struct ModelSupports: Codable, Sendable {
    public let vision: Bool
    public let reasoningEffort: Bool
}

/// Vision-specific limits.
public struct ModelVisionLimits: Codable, Sendable {
    public let supported_media_types: [String]
    public let max_prompt_images: Int
    public let max_prompt_image_size: Int
}

/// Model limits.
public struct ModelLimits: Codable, Sendable {
    public let max_prompt_tokens: Int?
    public let max_context_window_tokens: Int
    public let vision: ModelVisionLimits?
}

/// Model capabilities.
public struct ModelCapabilities: Codable, Sendable {
    public let supports: ModelSupports
    public let limits: ModelLimits
}

/// Model policy state.
public struct ModelPolicy: Codable, Sendable {
    public let state: String
    public let terms: String
}

/// Model billing information.
public struct ModelBilling: Codable, Sendable {
    public let multiplier: Double
}

/// Information about an available model.
public struct ModelInfo: Codable, Sendable {
    public let id: String
    public let name: String
    public let capabilities: ModelCapabilities
    public let policy: ModelPolicy?
    public let billing: ModelBilling?
    public let supportedReasoningEfforts: [String]?
    public let defaultReasoningEffort: String?
}

// MARK: - Session Metadata

/// Metadata about a session.
public struct SessionMetadata: Sendable {
    public let sessionId: String
    public let startTime: String
    public let modifiedTime: String
    public let summary: String?
    public let isRemote: Bool
}

// MARK: - Session Lifecycle Events

/// Types of session lifecycle events.
public enum SessionLifecycleEventType: String, Codable, Sendable {
    case created = "session.created"
    case deleted = "session.deleted"
    case updated = "session.updated"
    case foreground = "session.foreground"
    case background = "session.background"
}

/// A session lifecycle event.
public struct SessionLifecycleEvent: Codable, Sendable {
    public let type: SessionLifecycleEventType
    public let sessionId: String
    public let metadata: SessionLifecycleEventMetadata?
}

/// Metadata in lifecycle events.
public struct SessionLifecycleEventMetadata: Codable, Sendable {
    public let startTime: String
    public let modifiedTime: String
    public let summary: String?
}

/// Handler for session lifecycle events.
public typealias SessionLifecycleHandler = @Sendable (SessionLifecycleEvent) -> Void

// MARK: - Foreground Session Info

/// Information about the foreground session in TUI+server mode.
public struct ForegroundSessionInfo: Codable, Sendable {
    public let sessionId: String?
    public let workspacePath: String?
}

// MARK: - Session Filesystem

/// Configuration for a custom session filesystem provider.
public struct SessionFsConfig: Codable, Sendable {
    public var initialCwd: String
    public var sessionStatePath: String
    public var conventions: String

    public init(initialCwd: String, sessionStatePath: String, conventions: String) {
        self.initialCwd = initialCwd
        self.sessionStatePath = sessionStatePath
        self.conventions = conventions
    }
}

/// File metadata returned by session filesystem operations.
public struct SessionFsFileInfo {
    public var name: String
    public var size: Int64
    public var isDirectory: Bool
    public var isFile: Bool
    public var createdAt: String?
    public var modifiedAt: String?

    public init(name: String, size: Int64, isDirectory: Bool, isFile: Bool, createdAt: String? = nil, modifiedAt: String? = nil) {
        self.name = name
        self.size = size
        self.isDirectory = isDirectory
        self.isFile = isFile
        self.createdAt = createdAt
        self.modifiedAt = modifiedAt
    }
}

/// Protocol for session filesystem providers.
public protocol SessionFsProvider {
    func readFile(sessionId: String, path: String) async throws -> String
    func writeFile(sessionId: String, path: String, content: String) async throws
    func appendFile(sessionId: String, path: String, content: String) async throws
    func exists(sessionId: String, path: String) async throws -> Bool
    func stat(sessionId: String, path: String) async throws -> SessionFsFileInfo
    func mkdir(sessionId: String, path: String, recursive: Bool) async throws
    func readdir(sessionId: String, path: String) async throws -> [String]
    func readdirWithTypes(sessionId: String, path: String) async throws -> [SessionFsFileInfo]
    func rm(sessionId: String, path: String, recursive: Bool) async throws
    func rename(sessionId: String, oldPath: String, newPath: String) async throws
}

// MARK: - Client Options

/// Options for creating a CopilotClient.
public struct CopilotClientOptions: Sendable {
    /// Path to the Copilot CLI executable. Defaults to "copilot" (searched in PATH).
    public var cliPath: String?

    /// Extra arguments inserted before SDK-managed args.
    public var cliArgs: [String]?

    /// Working directory for the CLI process. Defaults to current directory.
    public var cwd: String?

    /// Port for TCP mode. 0 = random available port.
    public var port: Int?

    /// Use stdio transport instead of TCP. Defaults to true.
    public var useStdio: Bool?

    /// URL of an existing CLI server. Mutually exclusive with cliPath/useStdio.
    public var cliUrl: String?

    /// Log level for the CLI server.
    public var logLevel: String?

    /// Auto-start the CLI server on first use. Defaults to true.
    public var autoStart: Bool?

    /// Auto-restart the CLI server if it crashes. Defaults to true.
    public var autoRestart: Bool?

    /// Environment variables for the CLI process.
    public var env: [String: String]?

    /// GitHub token for authentication.
    public var githubToken: String?

    /// Whether to use the logged-in user for authentication. Defaults to true.
    public var useLoggedInUser: Bool?

    /// Server-wide idle timeout for sessions in seconds. Sessions without activity for this duration are automatically cleaned up.
    public var sessionIdleTimeoutSeconds: Int?

    /// Configuration for a custom session filesystem provider.
    public var sessionFs: SessionFsConfig?

    /// Override path to the Copilot home directory.
    public var copilotHome: String?

    /// Token for TCP connection authentication.
    public var tcpConnectionToken: String?

    public init(
        cliPath: String? = nil,
        cliArgs: [String]? = nil,
        cwd: String? = nil,
        port: Int? = nil,
        useStdio: Bool? = nil,
        cliUrl: String? = nil,
        logLevel: String? = nil,
        autoStart: Bool? = nil,
        autoRestart: Bool? = nil,
        env: [String: String]? = nil,
        githubToken: String? = nil,
        useLoggedInUser: Bool? = nil,
        sessionIdleTimeoutSeconds: Int? = nil,
        sessionFs: SessionFsConfig? = nil,
        copilotHome: String? = nil,
        tcpConnectionToken: String? = nil
    ) {
        self.cliPath = cliPath
        self.cliArgs = cliArgs
        self.cwd = cwd
        self.port = port
        self.useStdio = useStdio
        self.cliUrl = cliUrl
        self.logLevel = logLevel
        self.autoStart = autoStart
        self.autoRestart = autoRestart
        self.env = env
        self.githubToken = githubToken
        self.useLoggedInUser = useLoggedInUser
        self.sessionIdleTimeoutSeconds = sessionIdleTimeoutSeconds
        self.sessionFs = sessionFs
        self.copilotHome = copilotHome
        self.tcpConnectionToken = tcpConnectionToken
    }
}

// MARK: - AnyCodable (Type-erased Codable wrapper)

/// A type-erased Codable value. Used for dynamic JSON fields.
public struct AnyCodable: Codable, @unchecked Sendable {
    public let value: Any

    public init(_ value: Any) {
        self.value = value
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        if container.decodeNil() {
            value = NSNull()
        } else if let bool = try? container.decode(Bool.self) {
            value = bool
        } else if let int = try? container.decode(Int.self) {
            value = int
        } else if let double = try? container.decode(Double.self) {
            value = double
        } else if let string = try? container.decode(String.self) {
            value = string
        } else if let array = try? container.decode([AnyCodable].self) {
            value = array.map { $0.value }
        } else if let dict = try? container.decode([String: AnyCodable].self) {
            value = dict.mapValues { $0.value }
        } else {
            throw DecodingError.dataCorruptedError(
                in: container, debugDescription: "Unsupported type")
        }
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        switch value {
        case is NSNull:
            try container.encodeNil()
        case let bool as Bool:
            try container.encode(bool)
        case let int as Int:
            try container.encode(int)
        case let double as Double:
            try container.encode(double)
        case let string as String:
            try container.encode(string)
        case let array as [Any]:
            try container.encode(array.map { AnyCodable($0) })
        case let dict as [String: Any]:
            try container.encode(dict.mapValues { AnyCodable($0) })
        default:
            throw EncodingError.invalidValue(
                value,
                EncodingError.Context(
                    codingPath: container.codingPath,
                    debugDescription: "Unsupported type: \(type(of: value))"
                ))
        }
    }
}
