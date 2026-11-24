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
    /// Tool deferral policy: `ToolDefer.auto` (default) or `ToolDefer.never`.
    public let deferMode: String?
    /// When true, repeating a call with the same arguments has no additional effect beyond
    /// the first call (true of any tool without side effects). The runtime may then safely
    /// repeat a call whose outcome was lost.
    public let idempotent: Bool?

    public init(
        name: String,
        description: String? = nil,
        parameters: [String: Any]? = nil,
        deferMode: String? = nil,
        idempotent: Bool? = nil,
        handler: @escaping ToolHandler
    ) {
        self.name = name
        self.description = description
        self.parameters = parameters
        self.deferMode = deferMode
        self.idempotent = idempotent
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
        if let deferMode = deferMode {
            dict["defer"] = deferMode
        }
        if let idempotent = idempotent {
            dict["idempotent"] = idempotent
        }
        return dict
    }
}

/// Tool deferral policies for `Tool.deferMode`.
public struct ToolDefer {
    public static let auto = "auto"
    public static let never = "never"
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

/// Known system message section identifiers for section overrides.
public struct SystemMessageSection {
    public static let preamble = "preamble"
    public static let identity = "identity"
    public static let toolInstructions = "tool_instructions"
    public static let preserve = "preserve"
}

/// GitHub attachment type constants.
public struct GitHubAttachment {
    public static let gitHubCommit = "GitHubCommit"
    public static let gitHubRepository = "GitHubRepository"
    public static let gitHubPullRequest = "GitHubPullRequest"
    public static let gitHubIssue = "GitHubIssue"
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
    /// Runtime ID of the pending request, when the runtime sends it.
    public let requestId: String?
    /// Image support advertised by the runtime; nil when the answer cannot carry images
    /// (see `UserInputResponse.attachments`).
    public let imageAttachmentConstraints: UserInputImageAttachmentConstraints?
    /// The request's opaque `_meta` object, forwarded unchanged when the runtime sends it.
    public let meta: [String: AnyCodable]?

    private enum CodingKeys: String, CodingKey {
        case question, choices, allowFreeform, requestId, imageAttachmentConstraints
        case meta = "_meta"
    }

    public init(
        question: String,
        choices: [String]? = nil,
        allowFreeform: Bool? = nil,
        requestId: String? = nil,
        imageAttachmentConstraints: UserInputImageAttachmentConstraints? = nil,
        meta: [String: AnyCodable]? = nil
    ) {
        self.question = question
        self.choices = choices
        self.allowFreeform = allowFreeform
        self.requestId = requestId
        self.imageAttachmentConstraints = imageAttachmentConstraints
        self.meta = meta
    }
}

/// The user's response to an input request.
///
/// `attachments` is only valid when the request advertised `imageAttachmentConstraints` and
/// is sent only when set. Image-only answers use an empty `answer` and `wasFreeform: true`;
/// the runtime enforces the advertised MIME types, size and count limits.
public struct UserInputResponse: Codable, Sendable {
    public let answer: String
    public let wasFreeform: Bool
    /// Images attached to the answer, or nil to send none.
    public let attachments: [UserInputImageAttachment]?

    public init(answer: String, wasFreeform: Bool, attachments: [UserInputImageAttachment]? = nil) {
        self.answer = answer
        self.wasFreeform = wasFreeform
        self.attachments = attachments
    }

    /// The `userInput.request` result; `attachments` is included only when set.
    func toWireFormat() -> [String: Any] {
        var wire: [String: Any] = ["answer": answer, "wasFreeform": wasFreeform]
        if let attachments = attachments {
            wire["attachments"] = attachments.map { $0.toWireFormat() }
        }
        return wire
    }
}

/// Handler for user input requests.
public typealias UserInputHandler = @Sendable (UserInputRequest, String) async throws
    -> UserInputResponse

// MARK: - Exit Plan Mode Types

/// A request from the agent to exit plan mode and begin executing a plan.
public struct ExitPlanModeRequest: Codable, Sendable {
    /// The session that produced the plan.
    public let sessionId: String
    /// A short summary of the plan.
    public let summary: String
    /// The full plan content, if available.
    public let planContent: String?
    /// The actions the user can choose from.
    public let actions: [String]
    /// The action the agent recommends.
    public let recommendedAction: String

    public init(
        sessionId: String,
        summary: String = "",
        planContent: String? = nil,
        actions: [String] = [],
        recommendedAction: String = ""
    ) {
        self.sessionId = sessionId
        self.summary = summary
        self.planContent = planContent
        self.actions = actions
        self.recommendedAction = recommendedAction
    }
}

/// The user's response to an exit-plan-mode request.
public struct ExitPlanModeResponse: Codable, Sendable {
    /// Whether the plan was approved.
    public let approved: Bool
    /// The action the user selected, if any.
    public let selectedAction: String?
    /// Optional feedback from the user.
    public let feedback: String?

    public init(approved: Bool, selectedAction: String? = nil, feedback: String? = nil) {
        self.approved = approved
        self.selectedAction = selectedAction
        self.feedback = feedback
    }
}

/// Handler for exit-plan-mode requests.
public typealias ExitPlanModeHandler = @Sendable (ExitPlanModeRequest) async throws
    -> ExitPlanModeResponse

// MARK: - Trace Context Types

/// W3C trace-context values injected into outbound requests for distributed tracing.
public struct TraceContext: Codable, Sendable {
    /// The `traceparent` header value.
    public let traceparent: String?
    /// The `tracestate` header value.
    public let tracestate: String?

    public init(traceparent: String? = nil, tracestate: String? = nil) {
        self.traceparent = traceparent
        self.tracestate = tracestate
    }
}

/// Supplies the current trace context for outbound requests.
public typealias TraceContextProvider = @Sendable () async -> TraceContext

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
    /// Opaque decision context returned with the permission reply.
    public var decisionContext: [String: AnyCodable]?

    public init(
        permissionDecision: String? = nil,
        permissionDecisionReason: String? = nil,
        modifiedArgs: AnyCodable? = nil,
        additionalContext: String? = nil,
        suppressOutput: Bool? = nil,
        decisionContext: [String: AnyCodable]? = nil
    ) {
        self.permissionDecision = permissionDecision
        self.permissionDecisionReason = permissionDecisionReason
        self.modifiedArgs = modifiedArgs
        self.additionalContext = additionalContext
        self.suppressOutput = suppressOutput
        self.decisionContext = decisionContext
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

/// Input for the `subagentStart` hook, fired before a sub-agent's first turn.
public struct SubagentStartHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let transcriptPath: String
    public let agentName: String
    public let agentDisplayName: String?
    public let agentDescription: String?
}

/// Output for the `subagentStart` hook.
public struct SubagentStartHookOutput: Codable, Sendable {
    /// Context prepended to the sub-agent's initial prompt.
    public var additionalContext: String?

    public init(additionalContext: String? = nil) {
        self.additionalContext = additionalContext
    }
}

/// Handler for the `subagentStart` hook.
public typealias SubagentStartHandler = @Sendable (SubagentStartHookInput, String) async throws
    -> SubagentStartHookOutput?

/// Input for the `subagentStop` hook, fired after a sub-agent completes a turn.
public struct SubagentStopHookInput: Codable, Sendable {
    public let timestamp: Double
    public let cwd: String
    public let transcriptPath: String
    public let agentName: String
    public let agentDisplayName: String?
    public let agentDescription: String?
    public let agentId: String?
    public let agentType: String
    /// Why the sub-agent stopped (currently always `"end_turn"`).
    public let stopReason: String
    /// The sub-agent's last assistant message before any hook rewrite.
    public let response: String
}

/// Output for the `subagentStop` hook. `decision: "block"` with a non-empty `reason`
/// continues the sub-agent; otherwise `modifiedResponse` replaces the response reported
/// to the parent agent.
public struct SubagentStopHookOutput: Codable, Sendable {
    /// `"block"` or `"allow"`.
    public var decision: String?
    public var reason: String?
    public var modifiedResponse: String?

    public init(decision: String? = nil, reason: String? = nil, modifiedResponse: String? = nil) {
        self.decision = decision
        self.reason = reason
        self.modifiedResponse = modifiedResponse
    }
}

/// Handler for the `subagentStop` hook.
public typealias SubagentStopHandler = @Sendable (SubagentStopHookInput, String) async throws
    -> SubagentStopHookOutput?

/// Configuration for session hooks.
public struct SessionHooks: Sendable {
    public var onPreToolUse: PreToolUseHandler?
    public var onPostToolUse: PostToolUseHandler?
    public var onPreMcpToolCall: PostToolUseHandler?
    public var onUserPromptSubmitted: UserPromptSubmittedHandler?
    /// Invoked after the user prompt has been transformed, before submission.
    public var onUserPromptTransformed: UserPromptSubmittedHandler?
    public var onSessionStart: SessionStartHandler?
    public var onSessionEnd: SessionEndHandler?
    public var onErrorOccurred: ErrorOccurredHandler?
    /// Invoked before a sub-agent's first turn (`subagentStart`).
    public var onSubagentStart: SubagentStartHandler?
    /// Invoked after a sub-agent completes a turn (`subagentStop`).
    public var onSubagentStop: SubagentStopHandler?

    public init(
        onPreToolUse: PreToolUseHandler? = nil,
        onPostToolUse: PostToolUseHandler? = nil,
        onPreMcpToolCall: PostToolUseHandler? = nil,
        onUserPromptSubmitted: UserPromptSubmittedHandler? = nil,
        onUserPromptTransformed: UserPromptSubmittedHandler? = nil,
        onSessionStart: SessionStartHandler? = nil,
        onSessionEnd: SessionEndHandler? = nil,
        onErrorOccurred: ErrorOccurredHandler? = nil,
        onSubagentStart: SubagentStartHandler? = nil,
        onSubagentStop: SubagentStopHandler? = nil
    ) {
        self.onPreToolUse = onPreToolUse
        self.onPostToolUse = onPostToolUse
        self.onPreMcpToolCall = onPreMcpToolCall
        self.onUserPromptSubmitted = onUserPromptSubmitted
        self.onUserPromptTransformed = onUserPromptTransformed
        self.onSessionStart = onSessionStart
        self.onSessionEnd = onSessionEnd
        self.onErrorOccurred = onErrorOccurred
        self.onSubagentStart = onSubagentStart
        self.onSubagentStop = onSubagentStop
    }

    /// Returns true if any hook handler is set.
    public var hasAnyHandler: Bool {
        onPreToolUse != nil || onPostToolUse != nil || onPreMcpToolCall != nil
            || onUserPromptSubmitted != nil || onUserPromptTransformed != nil
            || onSessionStart != nil || onSessionEnd != nil || onErrorOccurred != nil
            || onSubagentStart != nil || onSubagentStop != nil
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
    /// JSON schema describing the arguments accepted by an agent factory.
    public var argsSchema: [String: AnyCodable]?

    public init(
        name: String,
        prompt: String,
        displayName: String? = nil,
        description: String? = nil,
        tools: [String]? = nil,
        mcpServers: [String: MCPServerConfig]? = nil,
        infer: Bool? = nil,
        skills: [String]? = nil,
        argsSchema: [String: AnyCodable]? = nil
    ) {
        self.name = name
        self.prompt = prompt
        self.displayName = displayName
        self.description = description
        self.tools = tools
        self.mcpServers = mcpServers
        self.infer = infer
        self.skills = skills
        self.argsSchema = argsSchema
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
///
/// SDK protocol 4 no longer accepts a single `provider`; the SDK translates it into a
/// named `"default"` provider (`inference.providers`) with Copilot API models disabled.
/// Model fields (`modelId`, `wireModel`, token limits) become an `inference.models`
/// entry for the session model.
public struct ProviderConfig: Codable, Sendable {
    public var type: String?
    public var wireApi: String?
    public var baseUrl: String
    public var apiKey: String?
    public var bearerToken: String?
    public var azure: AzureProviderOptions?
    /// Command run before each request to obtain a fresh API key. Failure fails the
    /// request without static-key fallback; bearer authentication takes precedence.
    public var apiKeyCommand: String?
    /// Transport: `"http"` (default) or `"websockets"` (requires the Responses API).
    public var transport: String?
    /// Extra HTTP headers sent with every model request.
    public var headers: [String: String]?
    /// Product serving the models (for example `"ollama"`); only affects telemetry.
    public var modelProvider: String?
    /// Provider-side model identifier for the session model.
    public var modelId: String?
    /// Model name sent on the wire, when it differs from `modelId`.
    public var wireModel: String?
    public var maxPromptTokens: Int?
    public var maxContextWindowTokens: Int?
    public var maxOutputTokens: Int?

    public init(
        baseUrl: String,
        type: String? = nil,
        wireApi: String? = nil,
        apiKey: String? = nil,
        bearerToken: String? = nil,
        azure: AzureProviderOptions? = nil,
        apiKeyCommand: String? = nil,
        transport: String? = nil,
        headers: [String: String]? = nil,
        modelProvider: String? = nil,
        modelId: String? = nil,
        wireModel: String? = nil,
        maxPromptTokens: Int? = nil,
        maxContextWindowTokens: Int? = nil,
        maxOutputTokens: Int? = nil
    ) {
        self.baseUrl = baseUrl
        self.type = type
        self.wireApi = wireApi
        self.apiKey = apiKey
        self.bearerToken = bearerToken
        self.azure = azure
        self.apiKeyCommand = apiKeyCommand
        self.transport = transport
        self.headers = headers
        self.modelProvider = modelProvider
        self.modelId = modelId
        self.wireModel = wireModel
        self.maxPromptTokens = maxPromptTokens
        self.maxContextWindowTokens = maxContextWindowTokens
        self.maxOutputTokens = maxOutputTokens
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
/// Provider bearer-token request arguments (bring-your-own-key).
public struct ProviderTokenArgs: Sendable {
    public let sessionId: String

    public init(sessionId: String) {
        self.sessionId = sessionId
    }
}

/// Supplies a bearer token for outbound model requests (bring-your-own-key).
public typealias BearerTokenProvider = @Sendable (ProviderTokenArgs) async throws -> String

/// Handles an MCP OAuth authorization request, returning an access token.
public typealias McpAuthHandler = @Sendable (ProviderTokenArgs) async throws -> String

/// Custom transport for outbound Copilot HTTP requests.
public protocol CopilotRequestHandler: Sendable {
    func sendRequest(_ request: [String: Any]) async throws -> [String: Any]
}

/// Per-session resource limits.
public struct SessionLimitsConfig: Sendable {
    public var maxAiCredits: Double?

    public init(maxAiCredits: Double? = nil) {
        self.maxAiCredits = maxAiCredits
    }
}

/// Long-term memory configuration for a session.
public struct MemoryConfiguration: Sendable {
    public var enabled: Bool
    /// Stores memories in the app instead of the runtime's built-in store
    /// (`memoryStore.store`). Requires `enabled` to be true. Experimental.
    public var storeMemory: MemoryStoreHandler?

    public init(enabled: Bool = true, storeMemory: MemoryStoreHandler? = nil) {
        self.enabled = enabled
        self.storeMemory = storeMemory
    }
}

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

    /// Handler invoked when the agent requests to exit plan mode.
    public var onExitPlanMode: ExitPlanModeHandler?

    /// Directories to search for instruction files.
    public var instructionDirectories: [String]?

    /// Enable inline source citations in assistant responses.
    public var enableCitations: Bool?
    /// Names of built-in agents to exclude from this session.
    public var excludedBuiltinAgents: [String]?
    /// Per-session resource limits (e.g. max AI credits).
    public var sessionLimits: SessionLimitsConfig?
    /// Long-term memory configuration.
    public var memory: MemoryConfiguration?
    /// OTLP export protocol for telemetry ("grpc" or "http/protobuf").
    public var otlpProtocol: String?
    /// Stream model responses over WebSocket transport.
    public var enableWebSocketResponses: Bool?
    /// Experiment assignment overrides.
    public var expAssignments: [String: AnyCodable]?
    /// Handler invoked when an MCP server requires OAuth authorization.
    public var onMcpAuthRequest: McpAuthHandler?

    // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
    /// Enable session rewind so the conversation can be rolled back.
    public var rewindEnabled: Bool?
    /// Extra workspace directories the session may access.
    public var additionalDirectories: [String]?
    /// Names of MCP servers to disable for this session.
    public var disabledMcpServers: [String]?
    /// GitHub MCP tool configuration.
    public var githubMcpToolConfig: [String: AnyCodable]?
    /// Canvas provider configuration.
    public var canvasProvider: [String: AnyCodable]?
    /// Restrict custom agents to locally-defined ones only.
    public var customAgentsLocalOnly: Bool?
    /// Tool-search configuration.
    public var toolSearch: [String: AnyCodable]?
    /// Enable experimental mode for this session.
    public var experimentalMode: Bool?
    /// Enable content-exclusion enforcement.
    public var contentExclusion: Bool?

    // --- v2.6.0 / v3.0.0 upstream-sync session options (SDK protocol 4) ---
    /// Session name (create only).
    public var name: String?
    /// Ask-user tool variant (`interaction.askUserVariant`).
    public var askUserVariant: AskUserVariant?
    /// Supplies GitHub tokens on demand. The client registers it under a generated
    /// registration ID (`github.tokenProviderRegistrationId`) and answers the runtime's
    /// `gitHubToken.getToken` requests with it. Mutually exclusive with `gitHubToken`.
    public var gitHubTokenProvider: GitHubTokenProvider?
    /// Sandbox configuration for tool execution (`permissions.sandbox`).
    public var sandboxConfig: SandboxConfig?
    /// Runtime feature flag overrides (`experiments.flags`).
    public var featureFlags: [String: Bool]?
    /// Model allowlist (`policy.allowedModels`).
    public var allowedModels: [String]?
    /// OAuth Client ID Metadata Document URL for MCP authorization (`mcp.authClientIdMetadataUrl`).
    public var authClientIdMetadataUrl: String?
    /// Custom agent definition directories (`agents.directories`).
    public var agentDirectories: [String]?
    /// Hook definition directories (`hooks.directories`).
    public var hookDirectories: [String]?
    /// Grants trust to the working directory at creation: `WorkingDirectoryTrust.session` or
    /// `WorkingDirectoryTrust.user` (create only; requires `workingDirectory`).
    public var trustWorkingDirectory: String?
    /// Image generation configuration (`tools.imageGeneration`).
    public var imageGeneration: ImageGenerationConfig?
    /// Autopilot continuation options (`execution.autopilot`).
    public var autopilot: AutopilotOptions?
    /// Named BYOK provider connections (`inference.providers`).
    public var providers: [NamedProviderConfig]?
    /// BYOK model definitions (`inference.models`). Select one with `"<provider>/<id>"`.
    public var models: [ProviderModelConfig]?
    /// Named connection for unqualified BYOK model selections (`inference.defaultProvider`).
    public var defaultProvider: String?
    /// Copilot API options (`github.capi`); `enabled: false` makes a BYOK-only session.
    public var capi: CapiSessionOptions?
    /// Answers shell authentication prompts (sudo, ssh, git) for commands the agent runs.
    /// When set, the client sends `interaction.requestShellAuthentication`.
    public var onShellAuthenticationRequest: ShellAuthenticationHandler?

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
        onExitPlanMode: ExitPlanModeHandler? = nil,
        instructionDirectories: [String]? = nil,
        enableCitations: Bool? = nil,
        excludedBuiltinAgents: [String]? = nil,
        sessionLimits: SessionLimitsConfig? = nil,
        memory: MemoryConfiguration? = nil,
        otlpProtocol: String? = nil,
        enableWebSocketResponses: Bool? = nil,
        expAssignments: [String: AnyCodable]? = nil,
        onMcpAuthRequest: McpAuthHandler? = nil,
        rewindEnabled: Bool? = nil,
        additionalDirectories: [String]? = nil,
        disabledMcpServers: [String]? = nil,
        githubMcpToolConfig: [String: AnyCodable]? = nil,
        canvasProvider: [String: AnyCodable]? = nil,
        customAgentsLocalOnly: Bool? = nil,
        toolSearch: [String: AnyCodable]? = nil,
        experimentalMode: Bool? = nil,
        contentExclusion: Bool? = nil,
        name: String? = nil,
        askUserVariant: AskUserVariant? = nil,
        gitHubTokenProvider: GitHubTokenProvider? = nil,
        sandboxConfig: SandboxConfig? = nil,
        featureFlags: [String: Bool]? = nil,
        allowedModels: [String]? = nil,
        authClientIdMetadataUrl: String? = nil,
        agentDirectories: [String]? = nil,
        hookDirectories: [String]? = nil,
        trustWorkingDirectory: String? = nil,
        imageGeneration: ImageGenerationConfig? = nil,
        autopilot: AutopilotOptions? = nil,
        providers: [NamedProviderConfig]? = nil,
        models: [ProviderModelConfig]? = nil,
        defaultProvider: String? = nil,
        capi: CapiSessionOptions? = nil,
        onShellAuthenticationRequest: ShellAuthenticationHandler? = nil
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
        self.onExitPlanMode = onExitPlanMode
        self.instructionDirectories = instructionDirectories
        self.enableCitations = enableCitations
        self.excludedBuiltinAgents = excludedBuiltinAgents
        self.sessionLimits = sessionLimits
        self.memory = memory
        self.otlpProtocol = otlpProtocol
        self.enableWebSocketResponses = enableWebSocketResponses
        self.expAssignments = expAssignments
        self.onMcpAuthRequest = onMcpAuthRequest
        self.rewindEnabled = rewindEnabled
        self.additionalDirectories = additionalDirectories
        self.disabledMcpServers = disabledMcpServers
        self.githubMcpToolConfig = githubMcpToolConfig
        self.canvasProvider = canvasProvider
        self.customAgentsLocalOnly = customAgentsLocalOnly
        self.toolSearch = toolSearch
        self.experimentalMode = experimentalMode
        self.contentExclusion = contentExclusion
        self.name = name
        self.askUserVariant = askUserVariant
        self.gitHubTokenProvider = gitHubTokenProvider
        self.sandboxConfig = sandboxConfig
        self.featureFlags = featureFlags
        self.allowedModels = allowedModels
        self.authClientIdMetadataUrl = authClientIdMetadataUrl
        self.agentDirectories = agentDirectories
        self.hookDirectories = hookDirectories
        self.trustWorkingDirectory = trustWorkingDirectory
        self.imageGeneration = imageGeneration
        self.autopilot = autopilot
        self.providers = providers
        self.models = models
        self.defaultProvider = defaultProvider
        self.capi = capi
        self.onShellAuthenticationRequest = onShellAuthenticationRequest
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

    /// Handler invoked when the agent requests to exit plan mode.
    public var onExitPlanMode: ExitPlanModeHandler?

    /// Directories to search for instruction files.
    public var instructionDirectories: [String]?

    // --- v2.6.0 / v3.0.0 upstream-sync resume options (SDK protocol 4) ---
    /// Long-term memory configuration (`state.memory`); `storeMemory` keeps memories in the app.
    public var memory: MemoryConfiguration?
    /// Ask-user tool variant (`interaction.askUserVariant`).
    public var askUserVariant: AskUserVariant?
    /// Supplies GitHub tokens on demand (`gitHubToken.getToken`). Mutually exclusive with
    /// `gitHubToken`.
    public var gitHubTokenProvider: GitHubTokenProvider?
    /// Sandbox configuration for tool execution (`permissions.sandbox`).
    public var sandboxConfig: SandboxConfig?
    /// Runtime feature flag overrides (`experiments.flags`).
    public var featureFlags: [String: Bool]?
    /// Model allowlist (`policy.allowedModels`).
    public var allowedModels: [String]?
    /// OAuth Client ID Metadata Document URL for MCP authorization (`mcp.authClientIdMetadataUrl`).
    public var authClientIdMetadataUrl: String?
    /// Custom agent definition directories (`agents.directories`).
    public var agentDirectories: [String]?
    /// Hook definition directories (`hooks.directories`).
    public var hookDirectories: [String]?
    /// Allows recovering the conversation from the transcript when saved state cannot be
    /// loaded (resume only).
    public var allowTranscriptRecovery: Bool?
    /// Image generation configuration (`tools.imageGeneration`).
    public var imageGeneration: ImageGenerationConfig?
    /// Autopilot continuation options (`execution.autopilot`).
    public var autopilot: AutopilotOptions?
    /// Named BYOK provider connections (`inference.providers`).
    public var providers: [NamedProviderConfig]?
    /// BYOK model definitions (`inference.models`). Select one with `"<provider>/<id>"`.
    public var models: [ProviderModelConfig]?
    /// Named connection for unqualified BYOK model selections (`inference.defaultProvider`).
    public var defaultProvider: String?
    /// When true (and `defaultProvider` is nil), clears the session's default provider by
    /// sending `inference.defaultProvider: null`.
    public var clearDefaultProvider: Bool?
    /// Copilot API options (`github.capi`).
    public var capi: CapiSessionOptions?
    /// Suppresses the `session.resume` event. Preferred over `disableResume`.
    public var suppressResumeEvent: Bool?
    /// Answers shell authentication prompts (sudo, ssh, git) for commands the agent runs.
    /// When set, the client sends `interaction.requestShellAuthentication`.
    public var onShellAuthenticationRequest: ShellAuthenticationHandler?

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
        onExitPlanMode: ExitPlanModeHandler? = nil,
        instructionDirectories: [String]? = nil,
        memory: MemoryConfiguration? = nil,
        askUserVariant: AskUserVariant? = nil,
        gitHubTokenProvider: GitHubTokenProvider? = nil,
        sandboxConfig: SandboxConfig? = nil,
        featureFlags: [String: Bool]? = nil,
        allowedModels: [String]? = nil,
        authClientIdMetadataUrl: String? = nil,
        agentDirectories: [String]? = nil,
        hookDirectories: [String]? = nil,
        allowTranscriptRecovery: Bool? = nil,
        imageGeneration: ImageGenerationConfig? = nil,
        autopilot: AutopilotOptions? = nil,
        providers: [NamedProviderConfig]? = nil,
        models: [ProviderModelConfig]? = nil,
        defaultProvider: String? = nil,
        clearDefaultProvider: Bool? = nil,
        capi: CapiSessionOptions? = nil,
        suppressResumeEvent: Bool? = nil,
        onShellAuthenticationRequest: ShellAuthenticationHandler? = nil
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
        self.onExitPlanMode = onExitPlanMode
        self.instructionDirectories = instructionDirectories
        self.memory = memory
        self.askUserVariant = askUserVariant
        self.gitHubTokenProvider = gitHubTokenProvider
        self.sandboxConfig = sandboxConfig
        self.featureFlags = featureFlags
        self.allowedModels = allowedModels
        self.authClientIdMetadataUrl = authClientIdMetadataUrl
        self.agentDirectories = agentDirectories
        self.hookDirectories = hookDirectories
        self.allowTranscriptRecovery = allowTranscriptRecovery
        self.imageGeneration = imageGeneration
        self.autopilot = autopilot
        self.providers = providers
        self.models = models
        self.defaultProvider = defaultProvider
        self.clearDefaultProvider = clearDefaultProvider
        self.capi = capi
        self.suppressResumeEvent = suppressResumeEvent
        self.onShellAuthenticationRequest = onShellAuthenticationRequest
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
    /// Legacy response format. SDK protocol 4 runtimes do not accept it, so it is no longer
    /// sent; use `responseSchema` for structured output.
    public var responseFormat: ResponseFormat?
    public var imageOptions: ImageOptions?
    /// Custom HTTP headers to include in outbound model requests for this turn.
    public var requestHeaders: [String: String]?
    /// Agent execution mode for this turn (e.g. "agent", "chat").
    public var agentMode: String?
    /// Prompt text to display in the UI in place of the actual prompt.
    public var displayPrompt: String?
    /// Who the message is from (`MessageSource.user`, `.system`, or `.agent(id)`).
    public var source: MessageSource?
    /// JSON schema the final assistant response must match (structured output). Sent as
    /// `responseFormat: {type: "json_schema", jsonSchema: {name: "response", strict: true, schema}}`.
    public var responseSchema: [String: Any]?
    /// Opaque caller-chosen identifier echoed on the events of this turn.
    public var clientTurnId: String?

    public init(
        prompt: String,
        attachments: [Attachment]? = nil,
        mode: String? = nil,
        responseFormat: ResponseFormat? = nil,
        imageOptions: ImageOptions? = nil,
        requestHeaders: [String: String]? = nil,
        agentMode: String? = nil,
        displayPrompt: String? = nil,
        source: MessageSource? = nil,
        responseSchema: [String: Any]? = nil,
        clientTurnId: String? = nil
    ) {
        self.prompt = prompt
        self.attachments = attachments
        self.mode = mode
        self.responseFormat = responseFormat
        self.imageOptions = imageOptions
        self.requestHeaders = requestHeaders
        self.agentMode = agentMode
        self.displayPrompt = displayPrompt
        self.source = source
        self.responseSchema = responseSchema
        self.clientTurnId = clientTurnId
    }
}

/// Origin of a message sent with `CopilotSession.send` (`session.send` param `source`).
public struct MessageSource: Sendable, Hashable {
    /// The wire value: `"user"`, `"system"`, or `"agent-<id>"`.
    public let rawValue: String

    public init(_ rawValue: String) {
        self.rawValue = rawValue
    }

    /// A message typed by the user (the default).
    public static let user = MessageSource("user")
    /// A message injected by the host application.
    public static let system = MessageSource("system")

    /// A message from another agent, identified by `id`.
    public static func agent(_ id: String) -> MessageSource {
        MessageSource("agent-\(id)")
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
        self.agentId = agentId
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
    public let timestamp: String
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
    public let vision: Bool?
    public let reasoningEffort: Bool?
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
    public let max_context_window_tokens: Int?
    public let vision: ModelVisionLimits?
}

/// Model capabilities.
public struct ModelCapabilities: Codable, Sendable {
    public let supports: ModelSupports?
    public let limits: ModelLimits?
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

    /// Custom transport for outbound Copilot HTTP requests.
    public var requestHandler: CopilotRequestHandler?

    /// Supplies bearer tokens for outbound model requests (bring-your-own-key).
    public var bearerTokenProvider: BearerTokenProvider?

    /// Built-in plugin directories to load.
    public var builtinPluginDirectories: [String]?

    /// Use the in-process FFI transport instead of spawning a CLI.
    public var inProcess: Bool?

    /// Supplies the current W3C trace context for outbound requests.
    public var onGetTraceContext: TraceContextProvider?

    /// Identity of the integrating application, sent as `clientInfo` on the `connect`
    /// handshake. Empty fields are dropped; an all-empty identity is not sent.
    public var clientInfo: ClientInfo?

    /// Session defaults profile: `.codingAgent` (default, today's behavior) or `.empty`
    /// (ambient runtime features off; every session must set `availableTools`).
    /// `.copilotCli` is a legacy alias of `.codingAgent`.
    public var mode: ClientMode?

    /// Network callbacks through which the app resolves proxies and supplies proxy
    /// credentials (`network.setProvider`).
    public var network: NetworkConfig?

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
        tcpConnectionToken: String? = nil,
        requestHandler: CopilotRequestHandler? = nil,
        bearerTokenProvider: BearerTokenProvider? = nil,
        builtinPluginDirectories: [String]? = nil,
        inProcess: Bool? = nil,
        onGetTraceContext: TraceContextProvider? = nil,
        clientInfo: ClientInfo? = nil,
        mode: ClientMode? = nil,
        network: NetworkConfig? = nil
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
        self.requestHandler = requestHandler
        self.bearerTokenProvider = bearerTokenProvider
        self.builtinPluginDirectories = builtinPluginDirectories
        self.inProcess = inProcess
        self.onGetTraceContext = onGetTraceContext
        self.clientInfo = clientInfo
        self.mode = mode
        self.network = network
    }
}

// MARK: - SDK Protocol 4 (v3.0.0) Types

/// Identity of the integrating application, sent as `clientInfo` on the `connect`
/// handshake. Empty fields are dropped; an all-empty identity is not sent.
public struct ClientInfo: Sendable {
    /// Name of the application using the SDK (for example "acme-portal").
    public var editorName: String?
    /// Version of the application using the SDK.
    public var editorVersion: String?
    /// Name of the integration within the application (extension, plugin).
    public var extensionName: String?
    /// Version of the integration.
    public var extensionVersion: String?

    public init(
        editorName: String? = nil,
        editorVersion: String? = nil,
        extensionName: String? = nil,
        extensionVersion: String? = nil
    ) {
        self.editorName = editorName
        self.editorVersion = editorVersion
        self.extensionName = extensionName
        self.extensionVersion = extensionVersion
    }

    /// The wire form with empty fields dropped, or nil when every field is empty.
    func toWireFormat() -> [String: Any]? {
        var wire: [String: Any] = [:]
        if let value = editorName, !value.isEmpty { wire["editorName"] = value }
        if let value = editorVersion, !value.isEmpty { wire["editorVersion"] = value }
        if let value = extensionName, !value.isEmpty { wire["extensionName"] = value }
        if let value = extensionVersion, !value.isEmpty { wire["extensionVersion"] = value }
        return wire.isEmpty ? nil : wire
    }
}

/// Session defaults profile for `CopilotClientOptions.mode`.
public enum ClientMode: String, Sendable {
    /// Default: identical to the Copilot CLI's ambient behavior ("coding-agent").
    case codingAgent = "coding-agent"
    /// Legacy alias of `codingAgent`.
    case copilotCli = "copilot-cli"
    /// Ambient runtime features off; every session must set `availableTools`. The SDK
    /// applies conservative defaults under the caller's own session options.
    case empty
}

/// Auto routing tiers for `CopilotSession.switchAutoTier(_:)` and `CapiSessionOptions.autoTier`.
public enum AutoTier: String, Codable, Sendable {
    case fast
    case efficiency
    case balance
    case intelligence
}

/// Ask-user tool variants (`interaction.askUserVariant`).
public enum AskUserVariant: String, Codable, Sendable {
    case legacy
    case elicitation
}

/// Working-directory trust scopes for `SessionConfig.trustWorkingDirectory`.
public struct WorkingDirectoryTrust {
    /// Trust applies only to the created session.
    public static let session = "session"
    /// Trust is persisted for later sessions.
    public static let user = "user"
}

/// Request passed to a `GitHubTokenProvider` (`gitHubToken.getToken`).
public struct GitHubTokenRequest: Sendable {
    /// Effective GitHub host the token is for.
    public let host: String
    /// Session receiving the token, when known.
    public let sessionId: String?
    /// Why the runtime is requesting a credential.
    public let reason: String

    public init(host: String, sessionId: String? = nil, reason: String) {
        self.host = host
        self.sessionId = sessionId
        self.reason = reason
    }
}

/// Result of a `GitHubTokenProvider`: a token or a cancellation.
public enum GitHubTokenResult: Sendable {
    /// A token with its remaining lifetime in seconds (more than one hour must remain)
    /// and an optional OAuth token type (bearer when omitted).
    case token(accessToken: String, expiresIn: Int, tokenType: String? = nil)
    /// No token is available; session creation or resume fails.
    case cancelled

    func toWireFormat() -> [String: Any] {
        switch self {
        case .token(let accessToken, let expiresIn, let tokenType):
            var wire: [String: Any] = [
                "kind": "token", "accessToken": accessToken, "expiresIn": expiresIn,
            ]
            if let tokenType = tokenType { wire["tokenType"] = tokenType }
            return wire
        case .cancelled:
            return ["kind": "cancelled"]
        }
    }
}

/// Acquires a GitHub token for one session on demand.
public typealias GitHubTokenProvider = @Sendable (GitHubTokenRequest) async throws
    -> GitHubTokenResult

/// Sandbox configuration for tool execution (`permissions.sandbox`).
public struct SandboxConfig: Codable, Sendable {
    public var enabled: Bool
    /// User sandbox policy (filesystem and network rules), passed through as-is.
    public var userPolicy: [String: AnyCodable]?
    public var addCurrentWorkingDirectory: Bool?
    public var sandboxMcpServers: Bool?
    public var sandboxLspServers: Bool?
    public var sandboxExtensions: Bool?
    /// Lets the agent request an individual command outside the sandbox through the
    /// normal permission flow.
    public var allowBypass: Bool?

    public init(
        enabled: Bool,
        userPolicy: [String: AnyCodable]? = nil,
        addCurrentWorkingDirectory: Bool? = nil,
        sandboxMcpServers: Bool? = nil,
        sandboxLspServers: Bool? = nil,
        sandboxExtensions: Bool? = nil,
        allowBypass: Bool? = nil
    ) {
        self.enabled = enabled
        self.userPolicy = userPolicy
        self.addCurrentWorkingDirectory = addCurrentWorkingDirectory
        self.sandboxMcpServers = sandboxMcpServers
        self.sandboxLspServers = sandboxLspServers
        self.sandboxExtensions = sandboxExtensions
        self.allowBypass = allowBypass
    }
}

/// Session image generation configuration (`tools.imageGeneration`).
public struct ImageGenerationConfig: Codable, Sendable {
    public var enabled: Bool?
    /// Requests a visible "Made with AI" label on generated images.
    public var watermark: Bool?
    /// Saves generated images under the session-state `images` folder.
    public var saveToDisk: Bool?

    public init(enabled: Bool? = nil, watermark: Bool? = nil, saveToDisk: Bool? = nil) {
        self.enabled = enabled
        self.watermark = watermark
        self.saveToDisk = saveToDisk
    }
}

/// Autopilot continuation options (`execution.autopilot`).
public struct AutopilotOptions: Codable, Sendable {
    public var continuationEnabled: Bool?
    public var maxContinues: Int?

    public init(continuationEnabled: Bool? = nil, maxContinues: Int? = nil) {
        self.continuationEnabled = continuationEnabled
        self.maxContinues = maxContinues
    }
}

/// Model capability limit overrides. Wire keys are snake_case; for example
/// `maxOutputTokens` is sent as `max_output_tokens`.
public struct ModelLimitsOverride: Codable, Sendable {
    public var maxPromptTokens: Int?
    public var maxOutputTokens: Int?
    public var maxContextWindowTokens: Int?

    enum CodingKeys: String, CodingKey {
        case maxPromptTokens = "max_prompt_tokens"
        case maxOutputTokens = "max_output_tokens"
        case maxContextWindowTokens = "max_context_window_tokens"
    }

    public init(
        maxPromptTokens: Int? = nil,
        maxOutputTokens: Int? = nil,
        maxContextWindowTokens: Int? = nil
    ) {
        self.maxPromptTokens = maxPromptTokens
        self.maxOutputTokens = maxOutputTokens
        self.maxContextWindowTokens = maxContextWindowTokens
    }
}

/// Typed builder for `SessionConfig.modelCapabilities` (`inference.capabilities`) and
/// `ProviderModelConfig.capabilities`.
public struct ModelCapabilitiesOverride: Codable, Sendable {
    /// Feature support overrides, for example `["vision": true, "toolCalls": true]`.
    public var supports: [String: Bool]?
    public var limits: ModelLimitsOverride?

    public init(supports: [String: Bool]? = nil, limits: ModelLimitsOverride? = nil) {
        self.supports = supports
        self.limits = limits
    }

    /// The override as a `modelCapabilities` dictionary.
    public func toDictionary() -> [String: AnyCodable] {
        (encodeToDictionary(self) ?? [:]).mapValues { AnyCodable($0) }
    }
}

/// A named BYOK provider connection (`inference.providers`).
public struct NamedProviderConfig: Codable, Sendable {
    /// Stable identifier referenced by `ProviderModelConfig.provider`.
    public var name: String
    public var type: String?
    public var baseUrl: String
    public var apiKey: String?
    public var apiKeyCommand: String?
    public var bearerToken: String?
    public var wireApi: String?
    public var transport: String?
    public var azure: AzureProviderOptions?
    public var headers: [String: String]?
    public var modelProvider: String?

    public init(
        name: String,
        baseUrl: String,
        type: String? = nil,
        apiKey: String? = nil,
        apiKeyCommand: String? = nil,
        bearerToken: String? = nil,
        wireApi: String? = nil,
        transport: String? = nil,
        azure: AzureProviderOptions? = nil,
        headers: [String: String]? = nil,
        modelProvider: String? = nil
    ) {
        self.name = name
        self.baseUrl = baseUrl
        self.type = type
        self.apiKey = apiKey
        self.apiKeyCommand = apiKeyCommand
        self.bearerToken = bearerToken
        self.wireApi = wireApi
        self.transport = transport
        self.azure = azure
        self.headers = headers
        self.modelProvider = modelProvider
    }
}

/// A BYOK model definition (`inference.models`) served by a named provider.
public struct ProviderModelConfig: Codable, Sendable {
    /// Name of the `NamedProviderConfig` serving this model.
    public var provider: String
    /// Provider-local model id; select it with `"<provider>/<id>"`.
    public var id: String
    public var modelId: String?
    public var wireModel: String?
    public var name: String?
    public var maxPromptTokens: Int?
    public var maxContextWindowTokens: Int?
    public var maxOutputTokens: Int?
    public var capabilities: ModelCapabilitiesOverride?

    public init(
        provider: String,
        id: String,
        modelId: String? = nil,
        wireModel: String? = nil,
        name: String? = nil,
        maxPromptTokens: Int? = nil,
        maxContextWindowTokens: Int? = nil,
        maxOutputTokens: Int? = nil,
        capabilities: ModelCapabilitiesOverride? = nil
    ) {
        self.provider = provider
        self.id = id
        self.modelId = modelId
        self.wireModel = wireModel
        self.name = name
        self.maxPromptTokens = maxPromptTokens
        self.maxContextWindowTokens = maxContextWindowTokens
        self.maxOutputTokens = maxOutputTokens
        self.capabilities = capabilities
    }
}

/// Copilot API (CAPI) options (`github.capi`).
public struct CapiSessionOptions: Codable, Sendable {
    /// Enables Copilot API models; `false` for BYOK-only sessions.
    public var enabled: Bool?
    /// Auto routing preference used when the session model is `auto`.
    public var autoTier: AutoTier?
    /// Use the WebSocket transport for the Responses API.
    public var enableWebSocketResponses: Bool?

    public init(enabled: Bool? = nil, autoTier: AutoTier? = nil, enableWebSocketResponses: Bool? = nil) {
        self.enabled = enabled
        self.autoTier = autoTier
        self.enableWebSocketResponses = enableWebSocketResponses
    }
}

/// A password or other answer a shell command run by the agent asks for
/// (`shellAuthentication.request`). The answer goes only to that program.
public struct ShellAuthenticationRequest: Sendable {
    public let sessionId: String
    /// Identifies the request; a `shellAuthentication.cancel` carries it too.
    public let requestId: String
    /// The program that asked: `sudo`, `ssh`, or `git`.
    public let program: String
    public let prompt: String
    /// `"secret"` (mask the input), `"text"`, or `"confirm"` (yes/no).
    public let inputKind: String
    public let command: String
    public let cwd: String

    public init(
        sessionId: String, requestId: String, program: String, prompt: String,
        inputKind: String, command: String, cwd: String
    ) {
        self.sessionId = sessionId
        self.requestId = requestId
        self.program = program
        self.prompt = prompt
        self.inputKind = inputKind
        self.command = command
        self.cwd = cwd
    }

    static func from(_ params: [String: Any]) -> ShellAuthenticationRequest {
        ShellAuthenticationRequest(
            sessionId: params["sessionId"] as? String ?? "",
            requestId: params["requestId"] as? String ?? "",
            program: params["program"] as? String ?? "",
            prompt: params["prompt"] as? String ?? "",
            inputKind: params["inputKind"] as? String ?? "secret",
            command: params["command"] as? String ?? "",
            cwd: params["cwd"] as? String ?? ""
        )
    }
}

/// Answer to a `ShellAuthenticationRequest`.
public enum ShellAuthenticationResponse: Sendable {
    /// Answers the prompt with a value (use `"yes"` for a confirm prompt).
    case accept(String)
    /// Declines the prompt.
    case cancel

    func toWireFormat() -> [String: Any] {
        switch self {
        case .accept(let value):
            return ["action": "accept", "value": value]
        case .cancel:
            return ["action": "cancel"]
        }
    }
}

/// Answers shell authentication prompts. The answer is ignored when the runtime cancels
/// the request first (`shellAuthentication.cancel`).
public typealias ShellAuthenticationHandler = @Sendable (ShellAuthenticationRequest) async throws
    -> ShellAuthenticationResponse

/// A memory the agent asked to store (`memoryStore.store`), after validation, secret
/// filtering and approval.
public struct StoredMemory: Sendable {
    public let sessionId: String
    public let subject: String
    public let fact: String
    public let citations: String
    public let reason: String

    public init(sessionId: String, subject: String, fact: String, citations: String, reason: String) {
        self.sessionId = sessionId
        self.subject = subject
        self.fact = fact
        self.citations = citations
        self.reason = reason
    }

    static func from(_ params: [String: Any]) -> StoredMemory {
        StoredMemory(
            sessionId: params["sessionId"] as? String ?? "",
            subject: params["subject"] as? String ?? "",
            fact: params["fact"] as? String ?? "",
            citations: params["citations"] as? String ?? "",
            reason: params["reason"] as? String ?? ""
        )
    }
}

/// Stores a memory in the app. The agent's `store_memory` tool succeeds once this returns.
public typealias MemoryStoreHandler = @Sendable (StoredMemory) async throws -> Void

/// Scope of the network callbacks registered with `network.setProvider`.
public enum NetworkScope: String, Sendable {
    /// Applies to this client connection (default).
    case connection
    /// Applies to the whole runtime.
    case runtime
}

/// Request for `network.authenticateBasic`.
public struct NetworkAuthenticateBasicRequest: Sendable {
    /// Proxy URL that issued the Basic challenge.
    public let proxy: String
    /// Realm from the proxy (untrusted text).
    public let realm: String?
    /// True when the proxy rejected the previously supplied credentials.
    public let previousRejected: Bool

    public init(proxy: String, realm: String? = nil, previousRejected: Bool) {
        self.proxy = proxy
        self.realm = realm
        self.previousRejected = previousRejected
    }
}

/// Credentials returned for `network.authenticateBasic`.
public struct NetworkBasicCredentials: Sendable {
    public var username: String?
    public var password: String?

    public init(username: String? = nil, password: String? = nil) {
        self.username = username
        self.password = password
    }
}

/// Network callbacks through which the app decides the runtime's proxies and supplies
/// proxy credentials. The SDK never logs the returned tokens or passwords.
public struct NetworkConfig: Sendable {
    /// `.connection` (default) or `.runtime`.
    public var scope: NetworkScope?
    /// Returns a PAC-style proxy list for a URL (`DIRECT` for none), or nil to let the
    /// runtime resolve it (`network.resolveProxy`).
    public var resolveProxy: (@Sendable (String) async throws -> String?)?
    /// Returns a base64 SPNEGO token for a service principal name, or nil
    /// (`network.authenticateNegotiate`).
    public var authenticateNegotiate: (@Sendable (String) async throws -> String?)?
    /// Returns Basic credentials for a proxy, or nil (`network.authenticateBasic`).
    public var authenticateBasic:
        (@Sendable (NetworkAuthenticateBasicRequest) async throws -> NetworkBasicCredentials?)?

    public init(
        scope: NetworkScope? = nil,
        resolveProxy: (@Sendable (String) async throws -> String?)? = nil,
        authenticateNegotiate: (@Sendable (String) async throws -> String?)? = nil,
        authenticateBasic: (
            @Sendable (NetworkAuthenticateBasicRequest) async throws -> NetworkBasicCredentials?
        )? = nil
    ) {
        self.scope = scope
        self.resolveProxy = resolveProxy
        self.authenticateNegotiate = authenticateNegotiate
        self.authenticateBasic = authenticateBasic
    }

    /// True when at least one callback is set.
    public var hasCallbacks: Bool {
        resolveProxy != nil || authenticateNegotiate != nil || authenticateBasic != nil
    }
}

/// Session event type names.
public struct SessionEventTypes {
    /// Live, append-only shell output of a running tool (`tool.shell_output`); not persisted.
    public static let toolShellOutput = "tool.shell_output"
    /// Removed upstream; still tolerated when received.
    public static let toolExecutionPartialResult = "tool.execution_partial_result"
}

/// Data of a `tool.shell_output` event.
public struct ToolShellOutputData: Sendable {
    public let toolCallId: String
    public let text: String
    /// Zero-based publication sequence across the call's output streams.
    public let sequence: Int
    /// `"stdout"`, `"stderr"`, or nil (stdout).
    public let stream: String?

    public init(toolCallId: String, text: String, sequence: Int, stream: String? = nil) {
        self.toolCallId = toolCallId
        self.text = text
        self.sequence = sequence
        self.stream = stream
    }

    /// Parses the data of a `tool.shell_output` event; nil for other event types.
    public static func from(_ event: SessionEvent) -> ToolShellOutputData? {
        guard event.type == SessionEventTypes.toolShellOutput else { return nil }
        let rawSequence = event.data["sequence"]
        return ToolShellOutputData(
            toolCallId: event.data["toolCallId"] as? String ?? "",
            text: event.data["text"] as? String ?? "",
            sequence: (rawSequence as? Int) ?? (rawSequence as? Double).map { Int($0) } ?? 0,
            stream: event.data["stream"] as? String
        )
    }
}

/// A field-specific finding reported by a Policy Helper.
public struct PolicyHelperFailureDetail: Codable, Sendable {
    public let message: String
    public let path: String?

    public init(message: String, path: String? = nil) {
        self.message = message
        self.path = path
    }
}

/// The latest unresolved Policy Helper failure. Exposed from the JSON-RPC error data
/// (`error.data.policyHelperFailure`) when `session.create` / `session.resume` fails (see
/// `JsonRpcError.policyHelperFailure`). Helper text is untrusted: render it as plain text.
public struct PolicyHelperFailure: Codable, Sendable {
    public let category: String
    public let failureMode: String
    public let message: String
    public let previousPolicyRetained: Bool
    public let reason: String
    public let authority: String?
    public let code: String?
    public let details: [PolicyHelperFailureDetail]?
    public let diagnostic: String?
    /// Validated additional-information link (`{url, label?}`), if any.
    public let helpLink: [String: AnyCodable]?

    public init(
        category: String,
        failureMode: String,
        message: String,
        previousPolicyRetained: Bool,
        reason: String,
        authority: String? = nil,
        code: String? = nil,
        details: [PolicyHelperFailureDetail]? = nil,
        diagnostic: String? = nil,
        helpLink: [String: AnyCodable]? = nil
    ) {
        self.category = category
        self.failureMode = failureMode
        self.message = message
        self.previousPolicyRetained = previousPolicyRetained
        self.reason = reason
        self.authority = authority
        self.code = code
        self.details = details
        self.diagnostic = diagnostic
        self.helpLink = helpLink
    }

    /// Parses a failure from a JSON dictionary, tolerating missing fields.
    public static func from(_ dict: [String: Any]) -> PolicyHelperFailure {
        let details = (dict["details"] as? [[String: Any]])?.map {
            PolicyHelperFailureDetail(message: $0["message"] as? String ?? "", path: $0["path"] as? String)
        }
        return PolicyHelperFailure(
            category: dict["category"] as? String ?? "",
            failureMode: dict["failureMode"] as? String ?? "",
            message: dict["message"] as? String ?? "",
            previousPolicyRetained: dict["previousPolicyRetained"] as? Bool ?? false,
            reason: dict["reason"] as? String ?? "",
            authority: dict["authority"] as? String,
            code: dict["code"] as? String,
            details: details,
            diagnostic: dict["diagnostic"] as? String,
            helpLink: (dict["helpLink"] as? [String: Any])?.mapValues { AnyCodable($0) }
        )
    }
}

/// Known refusal reasons on a `PermissionModeSetResult`.
public struct PermissionModeRefusalReason {
    /// Managed (enterprise) policy refused the requested permission mode.
    public static let managedPolicy = "managed_policy"
}

/// Result of `CopilotSession.setPermissionMode(_:)` (`session.permissions.setMode`).
public struct PermissionModeSetResult: Sendable {
    /// Whether the requested mode was applied.
    public let success: Bool
    /// Authoritative permission mode after the call.
    public let mode: String
    /// Why the mode was refused, when known (see `PermissionModeRefusalReason.managedPolicy`).
    public let reason: String?

    public init(success: Bool, mode: String, reason: String? = nil) {
        self.success = success
        self.mode = mode
        self.reason = reason
    }
}

// MARK: - v3.1.0 Types (Microsoft Copilot SDK 2.0.3-preview.0 parity)

/// Image support advertised by a `userInput.request` (`UserInputRequest.imageAttachmentConstraints`).
/// Absent (nil) when the runtime does not accept images in the answer.
public struct UserInputImageAttachmentConstraints: Codable, Sendable {
    /// MIME types accepted by the request.
    public let allowedMimeTypes: [String]
    /// Maximum decoded byte length of each image.
    public let maxBytesPerImage: Int
    /// Maximum number of images accepted in one answer.
    public let maxCount: Int

    public init(allowedMimeTypes: [String], maxBytesPerImage: Int, maxCount: Int) {
        self.allowedMimeTypes = allowedMimeTypes
        self.maxBytesPerImage = maxBytesPerImage
        self.maxCount = maxCount
    }

    static func from(_ dict: [String: Any]) -> UserInputImageAttachmentConstraints {
        let maxBytes = dict["maxBytesPerImage"]
        let maxCount = dict["maxCount"]
        return UserInputImageAttachmentConstraints(
            allowedMimeTypes: dict["allowedMimeTypes"] as? [String] ?? [],
            maxBytesPerImage: (maxBytes as? Int) ?? (maxBytes as? Double).map { Int($0) } ?? 0,
            maxCount: (maxCount as? Int) ?? (maxCount as? Double).map { Int($0) } ?? 0
        )
    }
}

/// An inline image attached to an ask_user answer (`UserInputResponse.attachments`).
/// `data` is the base64-encoded image without a data URL prefix; the wire `type` is always
/// `"image"`.
public struct UserInputImageAttachment: Codable, Sendable {
    /// Attachment type; always `"image"`.
    public let type: String
    /// Base64-encoded image bytes.
    public let data: String
    /// MIME type of the image, matching one of the request's `allowedMimeTypes`.
    public let mimeType: String
    /// Optional description of the image.
    public let description: String?
    /// Optional client-facing name of the image.
    public let displayName: String?

    public init(data: String, mimeType: String, description: String? = nil, displayName: String? = nil) {
        self.type = "image"
        self.data = data
        self.mimeType = mimeType
        self.description = description
        self.displayName = displayName
    }

    func toWireFormat() -> [String: Any] {
        var wire: [String: Any] = ["type": "image", "data": data, "mimeType": mimeType]
        if let description = description { wire["description"] = description }
        if let displayName = displayName { wire["displayName"] = displayName }
        return wire
    }
}

/// Result of `CopilotClient.mcpSecretsSave(secretId:value:configDir:)` (`mcp.secrets.save`).
public struct McpSecretsSaveResult: Sendable {
    /// True when the OS keychain stored the credential, false for the file backend.
    public let usedKeychain: Bool

    public init(usedKeychain: Bool) {
        self.usedKeychain = usedKeychain
    }
}

/// A user-scoped approval for one installed plugin extension.
public struct PermissionsExtensionApproval: Codable, Sendable {
    /// Stable identity, for example `"plugin:plugin-name:extension-name"`.
    public let extensionName: String
    /// Name of the plugin that contributes the extension.
    public let pluginName: String
    /// Exact privileged capabilities the user approved.
    public let capabilities: [String]
    /// When the approval was granted (RFC 3339 timestamp).
    public let grantedAt: String
    /// Opaque installation key; pass it back unchanged to
    /// `CopilotClient.extensionApprovalsRevoke(extensionName:installationKey:sessionId:)`.
    public let installationKey: String
    /// Human-readable installation label, when known.
    public let installationLabel: String?

    public init(
        extensionName: String,
        pluginName: String,
        capabilities: [String],
        grantedAt: String,
        installationKey: String,
        installationLabel: String? = nil
    ) {
        self.extensionName = extensionName
        self.pluginName = pluginName
        self.capabilities = capabilities
        self.grantedAt = grantedAt
        self.installationKey = installationKey
        self.installationLabel = installationLabel
    }

    static func from(_ dict: [String: Any]) -> PermissionsExtensionApproval {
        PermissionsExtensionApproval(
            extensionName: dict["extensionName"] as? String ?? "",
            pluginName: dict["pluginName"] as? String ?? "",
            capabilities: dict["capabilities"] as? [String] ?? [],
            grantedAt: dict["grantedAt"] as? String ?? "",
            installationKey: dict["installationKey"] as? String ?? "",
            installationLabel: dict["installationLabel"] as? String
        )
    }
}

/// Result of `CopilotClient.extensionApprovalsGrant(sessionId:requestId:)`
/// (`permissions.extensionApprovals.grant`).
public struct PermissionsExtensionApprovalsGrantResult: Sendable {
    /// Whether the pending request was eligible and its approval was persisted.
    public let granted: Bool
    /// The stored approval, when one was persisted.
    public let approval: PermissionsExtensionApproval?

    public init(granted: Bool, approval: PermissionsExtensionApproval? = nil) {
        self.granted = granted
        self.approval = approval
    }
}

/// Result of `CopilotClient.extensionApprovalsList(sessionId:)`
/// (`permissions.extensionApprovals.list`).
public struct PermissionsExtensionApprovalsListResult: Sendable {
    /// The persisted plugin extension approvals.
    public let approvals: [PermissionsExtensionApproval]

    public init(approvals: [PermissionsExtensionApproval]) {
        self.approvals = approvals
    }
}

/// Result of `CopilotClient.extensionApprovalsRevoke(extensionName:installationKey:sessionId:)`
/// (`permissions.extensionApprovals.revoke`).
public struct PermissionsExtensionApprovalsRevokeResult: Sendable {
    /// Whether a matching approval was revoked.
    public let revoked: Bool

    public init(revoked: Bool) {
        self.revoked = revoked
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
