## Core types for the GitHub Copilot SDK for Nim.
##
## Provides object definitions for client configuration, sessions,
## tools, messages, and JSON-RPC protocol structures.

import std/[json, tables, options]

# ---------------------------------------------------------------------------
# JSON-RPC types
# ---------------------------------------------------------------------------

type
  JsonRpcRequest* = object
    jsonrpc*: string
    id*: int
    `method`*: string
    params*: JsonNode

  JsonRpcResponse* = object
    jsonrpc*: string
    id*: int
    result*: JsonNode
    error*: Option[JsonRpcError]

  JsonRpcNotification* = object
    jsonrpc*: string
    `method`*: string
    params*: JsonNode

  JsonRpcError* = object
    code*: int
    message*: string
    data*: JsonNode

# ---------------------------------------------------------------------------
# Upstream-sync feature types (parity with @github/copilot-sdk)
# ---------------------------------------------------------------------------

type
  # Context passed to a bearer-token / MCP-auth provider callback.
  ProviderTokenArgs* = object
    sessionId*: string
    serverUrl*: string
    scopes*: seq[string]

  # Per-session spending / usage guardrails.
  SessionLimitsConfig* = object
    maxAiCredits*: float

  # Persistent session-memory settings.
  MemoryConfiguration* = object
    enabled*: bool

  # Custom handler used to intercept outbound CAPI HTTP requests.
  CopilotRequestHandler* = proc(requestJson: string): string {.closure.}

  # BYOK bearer-token provider invoked for authenticated model requests.
  BearerTokenProvider* = proc(args: ProviderTokenArgs): string {.closure.}

  # Handler invoked to satisfy an MCP OAuth authorization request.
  McpAuthHandler* = proc(args: ProviderTokenArgs): string {.closure.}

  # Controls when a tool definition is materialised for the model.
  ToolDefer* = enum
    tdAuto = "auto"
    tdNever = "never"

  # Named sections of the composed system message.
  SystemMessageSection* = enum
    smsPreamble = "preamble"
    smsIdentity = "identity"
    smsToolInstructions = "tool_instructions"
    smsPreserve = "preserve"

const
  # Hook-type identifiers (the "hookType" field of a hooks.invoke request).
  HookPreToolUse* = "preToolUse"
  HookPostToolUse* = "postToolUse"
  HookUserPromptSubmitted* = "userPromptSubmitted"
  HookUserPromptTransformed* = "userPromptTransformed"
  HookSessionStart* = "sessionStart"
  HookSessionEnd* = "sessionEnd"
  HookErrorOccurred* = "errorOccurred"
  HookPreMcpToolCall* = "preMcpToolCall"

  # GitHub-anchored attachment type identifiers.
  GitHubCommit* = "GitHubCommit"
  GitHubRelease* = "GitHubRelease"
  GitHubActionsJob* = "GitHubActionsJob"
  GitHubRepository* = "GitHubRepository"
  GitHubFileDiff* = "GitHubFileDiff"
  GitHubTreeComparison* = "GitHubTreeComparison"
  GitHubPullRequest* = "GitHubPullRequest"
  GitHubIssue* = "GitHubIssue"

# ---------------------------------------------------------------------------
# Client configuration
# ---------------------------------------------------------------------------

type
  ClientConfig* = object
    cliPath*: string
    cliUrl*: string
    extraArgs*: seq[string]
    sessionIdleTimeoutSeconds*: int
    sessionFs*: SessionFsConfig
    copilotHome*: string
    tcpConnectionToken*: string
    requestHandler*: CopilotRequestHandler
    bearerTokenProvider*: BearerTokenProvider
    builtinPluginDirectories*: seq[string]
    inProcess*: bool

  SessionFsConfig* = object
    initialCwd*: string
    sessionStatePath*: string
    conventions*: string

# ---------------------------------------------------------------------------
# Session configuration
# ---------------------------------------------------------------------------

type
  SessionConfig* = object
    systemPrompt*: string
    githubToken*: string
    sessionIdleTimeoutSeconds*: int
    skillDirectories*: seq[string]
    disabledSkills*: seq[string]
    excludedTools*: seq[string]
    enableConfigDiscovery*: bool
    modelCapabilitiesJson*: string
    mcpServersJson*: string
    commandsJson*: string
    responseFormat*: string
    imageSize*: string
    imageQuality*: string
    imageStyle*: string
    includeSubAgentStreamingEvents*: bool
    authToken*: string
    instructionDirectories*: seq[string]
    enableCitations*: bool
    excludedBuiltinAgents*: seq[string]
    sessionLimits*: SessionLimitsConfig
    memory*: MemoryConfiguration
    otlpProtocol*: string
    enableWebSocketResponses*: bool
    expAssignmentsJson*: string
    onMcpAuthRequest*: McpAuthHandler
    mcpAuthHandler*: bool
    # --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
    rewindEnabled*: bool
    additionalDirectories*: seq[string]
    disabledMcpServers*: seq[string]
    githubMcpToolConfigJson*: string
    canvasProvider*: string
    customAgentsLocalOnly*: bool
    reasoningEffort*: string
    toolSearchJson*: string
    experimentalMode*: bool
    contentExclusion*: bool
    argsSchemaJson*: string

  ResumeSessionConfig* = object
    sessionId*: string
    systemPrompt*: string
    githubToken*: string
    instructionDirectories*: seq[string]

# ---------------------------------------------------------------------------
# Message types
# ---------------------------------------------------------------------------

type
  MessageRole* = enum
    mrUser = "user"
    mrAssistant = "assistant"
    mrSystem = "system"

  Message* = object
    role*: MessageRole
    content*: string
    turnId*: string

  MessageOptions* = object
    message*: string
    streaming*: bool
    agentMode*: string
    displayPrompt*: string
    requestHeaders*: Table[string, string]

  SendResult* = object
    message*: string
    turnId*: string
    rawEvents*: seq[SessionEvent]

# ---------------------------------------------------------------------------
# Session events
# ---------------------------------------------------------------------------

type
  SessionEventKind* = enum
    sekAssistantMessage = "assistant.message"
    sekAssistantMessageDelta = "assistant.message_delta"
    sekAssistantReasoning = "assistant.reasoning"
    sekAssistantReasoningDelta = "assistant.reasoning_delta"
    sekSessionIdle = "session.idle"
    sekToolCall = "tool.call"
    sekToolResult = "tool.result"
    sekPermissionRequest = "permission.request"
    sekUserInputRequest = "userInput.request"
    sekHooksInvoke = "hooks.invoke"
    sekElicitationRequest = "elicitation.request"
    sekSessionCompactionStart = "session.compaction_start"
    sekSessionCompactionComplete = "session.compaction_complete"
    sekUnknown = "unknown"

  SessionEvent* = object
    kind*: SessionEventKind
    data*: JsonNode
    id*: string
    timestamp*: string
    parentId*: string
    agentId*: string
    ephemeral*: bool

# ---------------------------------------------------------------------------
# Tool types
# ---------------------------------------------------------------------------

type
  ToolParameter* = object
    name*: string
    `type`*: string
    description*: string
    required*: bool

  ToolHandler* = proc(params: JsonNode): string {.closure.}

  Tool* = object
    name*: string
    description*: string
    parameters*: seq[ToolParameter]
    handler*: ToolHandler

  ToolInvocation* = object
    callId*: string
    toolName*: string
    parameters*: JsonNode

  ToolResult* = object
    callId*: string
    result*: string

# ---------------------------------------------------------------------------
# Permission and UI handler types
# ---------------------------------------------------------------------------

type
  PermissionRequest* = object
    id*: string
    toolName*: string
    description*: string
    ## Opaque decision context echoed back on the permission reply (`decisionContext`).
    decisionContextJson*: string

  PermissionDecision* = enum
    pdAllow = "allow"
    pdDeny = "deny"

  PermissionHandler* = proc(req: PermissionRequest): PermissionDecision {.closure.}

  UserInputRequest* = object
    id*: string
    prompt*: string

  UserInputHandler* = proc(req: UserInputRequest): string {.closure.}

  ElicitationRequest* = object
    id*: string
    message*: string
    options*: seq[string]

  ElicitationHandler* = proc(req: ElicitationRequest): string {.closure.}

# ---------------------------------------------------------------------------
# Connection state
# ---------------------------------------------------------------------------

type
  ConnectionState* = enum
    csDisconnected = "disconnected"
    csConnecting = "connecting"
    csConnected = "connected"
    csError = "error"

# ---------------------------------------------------------------------------
# Helper constructors
# ---------------------------------------------------------------------------

proc newClientConfig*(cliPath = ""; cliUrl = "";
                      extraArgs: seq[string] = @[];
                      sessionIdleTimeoutSeconds = 0;
                      sessionFs = SessionFsConfig();
                      copilotHome = "";
                      tcpConnectionToken = ""): ClientConfig =
  ClientConfig(cliPath: cliPath, cliUrl: cliUrl, extraArgs: extraArgs,
               sessionIdleTimeoutSeconds: sessionIdleTimeoutSeconds,
               sessionFs: sessionFs, copilotHome: copilotHome,
               tcpConnectionToken: tcpConnectionToken)

proc newSessionConfig*(systemPrompt = ""; githubToken = "";
                       sessionIdleTimeoutSeconds = 0;
                       skillDirectories: seq[string] = @[];
                       disabledSkills: seq[string] = @[];
                       excludedTools: seq[string] = @[];
                       enableConfigDiscovery = false;
                       modelCapabilitiesJson = "";
                       mcpServersJson = "";
                       commandsJson = "";
                       responseFormat = "";
                       imageSize = "";
                       imageQuality = "";
                       imageStyle = "";
                       includeSubAgentStreamingEvents = false;
                       authToken = "";
                       instructionDirectories: seq[string] = @[];
                       enableCitations = false;
                       excludedBuiltinAgents: seq[string] = @[];
                       sessionLimits = SessionLimitsConfig();
                       memory = MemoryConfiguration();
                       otlpProtocol = "";
                       enableWebSocketResponses = false;
                       expAssignmentsJson = "";
                       onMcpAuthRequest: McpAuthHandler = nil;
                       mcpAuthHandler = false;
                       rewindEnabled = false;
                       additionalDirectories: seq[string] = @[];
                       disabledMcpServers: seq[string] = @[];
                       githubMcpToolConfigJson = "";
                       canvasProvider = "";
                       customAgentsLocalOnly = false;
                       reasoningEffort = "";
                       toolSearchJson = "";
                       experimentalMode = false;
                       contentExclusion = false;
                       argsSchemaJson = ""): SessionConfig =
  SessionConfig(
    systemPrompt: systemPrompt,
    githubToken: githubToken,
    sessionIdleTimeoutSeconds: sessionIdleTimeoutSeconds,
    skillDirectories: skillDirectories,
    disabledSkills: disabledSkills,
    excludedTools: excludedTools,
    enableConfigDiscovery: enableConfigDiscovery,
    modelCapabilitiesJson: modelCapabilitiesJson,
    mcpServersJson: mcpServersJson,
    commandsJson: commandsJson,
    responseFormat: responseFormat,
    imageSize: imageSize,
    imageQuality: imageQuality,
    imageStyle: imageStyle,
    includeSubAgentStreamingEvents: includeSubAgentStreamingEvents,
    authToken: authToken,
    instructionDirectories: instructionDirectories,
    enableCitations: enableCitations,
    excludedBuiltinAgents: excludedBuiltinAgents,
    sessionLimits: sessionLimits,
    memory: memory,
    otlpProtocol: otlpProtocol,
    enableWebSocketResponses: enableWebSocketResponses,
    expAssignmentsJson: expAssignmentsJson,
    onMcpAuthRequest: onMcpAuthRequest,
    mcpAuthHandler: mcpAuthHandler,
    rewindEnabled: rewindEnabled,
    additionalDirectories: additionalDirectories,
    disabledMcpServers: disabledMcpServers,
    githubMcpToolConfigJson: githubMcpToolConfigJson,
    canvasProvider: canvasProvider,
    customAgentsLocalOnly: customAgentsLocalOnly,
    reasoningEffort: reasoningEffort,
    toolSearchJson: toolSearchJson,
    experimentalMode: experimentalMode,
    contentExclusion: contentExclusion,
    argsSchemaJson: argsSchemaJson,
  )

proc newResumeSessionConfig*(sessionId: string; systemPrompt = "";
                             githubToken = "";
                             instructionDirectories: seq[string] = @[]): ResumeSessionConfig =
  ResumeSessionConfig(
    sessionId: sessionId,
    systemPrompt: systemPrompt,
    githubToken: githubToken,
    instructionDirectories: instructionDirectories,
  )

proc newMessageOptions*(message: string; streaming = false;
                        agentMode = ""; displayPrompt = "";
                        requestHeaders = initTable[string, string]()): MessageOptions =
  MessageOptions(
    message: message,
    streaming: streaming,
    agentMode: agentMode,
    displayPrompt: displayPrompt,
    requestHeaders: requestHeaders,
  )
