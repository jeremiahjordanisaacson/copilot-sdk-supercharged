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
                       instructionDirectories: seq[string] = @[]): SessionConfig =
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
                        requestHeaders = initTable[string, string]()): MessageOptions =
  MessageOptions(
    message: message,
    streaming: streaming,
    requestHeaders: requestHeaders,
  )
