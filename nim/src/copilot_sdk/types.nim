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
    idempotent*: bool
      ## Marks the tool as safe to retry; sent as `idempotent` in the tool definition.

  ToolInvocation* = object
    callId*: string
    toolName*: string
    parameters*: JsonNode

  ToolResult* = object
    callId*: string
    result*: string

# ---------------------------------------------------------------------------
# SDK protocol 4 / v3.0.0 feature types
# ---------------------------------------------------------------------------

type
  ClientMode* = enum
    ## Client behavior profile. `cmCodingAgent` (the default) keeps the
    ## coding-agent behavior. `cmEmpty` requires `availableTools` on every
    ## session and applies conservative defaults beneath the user's options.
    cmCodingAgent = "coding-agent"
    cmEmpty = "empty"

  ClientInfo* = object
    ## Identity of the integrating application, sent as `clientInfo` on `connect`.
    ## Empty fields are dropped; nothing is sent when every field is empty.
    editorName*: string
    editorVersion*: string
    extensionName*: string
    extensionVersion*: string

  AutoTier* = enum
    ## Routing tier for `CopilotSession.switchAutoTier`.
    atFast = "fast"
    atEfficiency = "efficiency"
    atBalance = "balance"
    atIntelligence = "intelligence"

  GitHubTokenRequest* = object
    ## A `gitHubToken.getToken` request from the runtime.
    registrationId*: string
    host*: string
    sessionId*: string
    reason*: string

  GitHubTokenResult* = object
    ## Answer to a `GitHubTokenRequest`. Set `cancelled` (or leave
    ## `accessToken` empty) to decline.
    accessToken*: string
    tokenType*: string
    expiresIn*: int
      ## Remaining token lifetime in seconds.
    cancelled*: bool

  GitHubTokenProvider* = proc(req: GitHubTokenRequest): GitHubTokenResult {.closure.}

  SandboxConfig* = object
    ## Sandbox settings sent as `permissions.sandbox`.
    enabled*: bool
    allowBypass*: bool
      ## Let the agent ask (through the permission flow) to run a command outside the sandbox.
    addCurrentWorkingDirectory*: Option[bool]
    sandboxMcpServers*: Option[bool]
    userPolicyJson*: string
      ## Optional user policy fragment as a JSON string.

  ModelCapabilitiesOverride* = object
    ## Model capability overrides sent as `inference.capabilities`.
    supportsVision*: Option[bool]
    supportsToolCalls*: Option[bool]
    supportsReasoningEffort*: Option[bool]
    maxPromptTokens*: int
    maxContextWindowTokens*: int
    maxOutputTokens*: int
      ## Sent as `limits.max_output_tokens`; 0 leaves the model default.

  ImageGenerationConfig* = object
    ## Image generation settings sent as `tools.imageGeneration`.
    enabled*: Option[bool]
    watermark*: Option[bool]
    saveToDisk*: Option[bool]

  AutopilotConfig* = object
    ## Autopilot continuation settings sent as `execution.autopilot`.
    continuationEnabled*: Option[bool]
    maxContinues*: Option[int]

  AzureProviderOptions* = object
    apiVersion*: string

  ProviderConfig* = object
    ## Legacy single BYOK provider. On the protocol 4 wire it becomes a named
    ## "default" provider in `inference.providers` with Copilot models disabled.
    `type`*: string
    baseUrl*: string
    apiKey*: string
    apiKeyCommand*: string
      ## Command run before each request to obtain a fresh API key.
    bearerToken*: string
    wireApi*: string
    transport*: string
    azure*: AzureProviderOptions
    headers*: Table[string, string]
    modelProvider*: string
    modelId*: string
    wireModel*: string
    maxPromptTokens*: int
    maxContextWindowTokens*: int
    maxOutputTokens*: int

  NamedProviderConfig* = object
    ## A named BYOK provider connection (`inference.providers`).
    name*: string
    `type`*: string
    baseUrl*: string
    apiKey*: string
    apiKeyCommand*: string
    bearerToken*: string
    wireApi*: string
    transport*: string
    azure*: AzureProviderOptions
    headers*: Table[string, string]
    modelProvider*: string

  ProviderModelConfig* = object
    ## A BYOK model served by a named provider (`inference.models`).
    ## Select it with the model id "provider/id".
    provider*: string
    id*: string
    modelId*: string
    wireModel*: string
    name*: string
    maxPromptTokens*: int
    maxContextWindowTokens*: int
    maxOutputTokens*: int
    capabilities*: Option[ModelCapabilitiesOverride]

  CapiConfig* = object
    ## Copilot API (CAPI) model availability (`github.capi`).
    enabled*: Option[bool]

  ShellAuthenticationRequest* = object
    ## A password or other answer a shell command asks for (`shellAuthentication.request`).
    sessionId*: string
    requestId*: string
    program*: string
    prompt*: string
    inputKind*: string
      ## "secret", "text", or "confirm".
    command*: string
    cwd*: string

  ShellAuthenticationResponse* = object
    ## Set `accept` with a `value` to answer; leave `accept` false to cancel.
    accept*: bool
    value*: string

  ShellAuthenticationHandler* = proc(req: ShellAuthenticationRequest): ShellAuthenticationResponse {.closure.}

  StoredMemory* = object
    ## A memory the agent asked to store (`memoryStore.store`).
    sessionId*: string
    subject*: string
    fact*: string
    citations*: string
    reason*: string

  StoreMemoryHandler* = proc(memory: StoredMemory) {.closure.}

  HookHandler* = proc(input: JsonNode): JsonNode {.closure.}
    ## Handles one hook type (for example `HookSubagentStart`); the returned
    ## JSON (or nil) is sent back as the hook `output`.

  NetworkBasicAuthRequest* = object
    proxy*: string
    realm*: string
    previousRejected*: bool

  NetworkBasicCredentials* = object
    username*: string
    password*: string

  ProxyResolver* = proc(url: string): Option[string] {.closure.}
    ## Returns a PAC-style proxy list such as "PROXY proxy:8080; DIRECT", or none.
  NegotiateAuthenticator* = proc(spn: string): Option[string] {.closure.}
    ## Returns a base64 SPNEGO token for the service principal, or none.
  BasicAuthenticator* = proc(req: NetworkBasicAuthRequest): Option[NetworkBasicCredentials] {.closure.}

  NetworkOptions* = object
    ## Network callbacks registered with `network.setProvider` after connecting.
    scope*: string
      ## "connection" or "runtime"; empty uses the runtime default.
    resolveProxy*: ProxyResolver
    authenticateNegotiate*: NegotiateAuthenticator
    authenticateBasic*: BasicAuthenticator

  PolicyHelperFailure* = object
    ## Structured Policy Helper failure reported in RPC error data. The text is
    ## untrusted: render it as plain text.
    category*: string
    failureMode*: string
    message*: string
    previousPolicyRetained*: bool
    reason*: string
    authority*: string
    code*: string
    diagnostic*: string
    details*: JsonNode
    helpLink*: JsonNode

  PermissionModeResult* = object
    ## Result of `CopilotSession.setPermissionMode`.
    success*: bool
    mode*: string
    reason*: string
      ## Why the mode was refused, for example `PermissionModeRefusalManagedPolicy`.

  ExitPlanModeHandler* = proc(request: JsonNode): JsonNode {.closure.}
    ## Answers an `exitPlanMode.request` with its JSON reply.

const
  HookSubagentStart* = "subagentStart"
  HookSubagentStop* = "subagentStop"
  AskUserVariantLegacy* = "legacy"
  AskUserVariantElicitation* = "elicitation"
  TrustWorkingDirectorySession* = "session"
  TrustWorkingDirectoryUser* = "user"
  PermissionModeRefusalManagedPolicy* = "managed_policy"

proc parseClientMode*(value: string): ClientMode =
  ## Parses a client mode name. "copilot-cli" is accepted as an alias of "coding-agent".
  case value
  of "", "coding-agent", "copilot-cli": cmCodingAgent
  of "empty": cmEmpty
  else: raise newException(ValueError, "Unknown client mode: " & value)

proc toWireJson*(info: ClientInfo): JsonNode =
  ## `clientInfo` connect payload, or nil when every field is empty.
  result = newJObject()
  if info.editorName.len > 0: result["editorName"] = %info.editorName
  if info.editorVersion.len > 0: result["editorVersion"] = %info.editorVersion
  if info.extensionName.len > 0: result["extensionName"] = %info.extensionName
  if info.extensionVersion.len > 0: result["extensionVersion"] = %info.extensionVersion
  if result.len == 0:
    result = nil

proc toWireJson*(res: GitHubTokenResult): JsonNode =
  ## Wire answer for `gitHubToken.getToken`.
  if res.cancelled or res.accessToken.len == 0:
    return %*{"kind": "cancelled"}
  result = %*{"kind": "token", "accessToken": res.accessToken,
              "expiresIn": res.expiresIn}
  if res.tokenType.len > 0:
    result["tokenType"] = %res.tokenType

proc toWireJson*(sandbox: SandboxConfig): JsonNode =
  result = %*{"enabled": sandbox.enabled}
  if sandbox.allowBypass:
    result["allowBypass"] = %true
  if sandbox.addCurrentWorkingDirectory.isSome:
    result["addCurrentWorkingDirectory"] = %sandbox.addCurrentWorkingDirectory.get()
  if sandbox.sandboxMcpServers.isSome:
    result["sandboxMcpServers"] = %sandbox.sandboxMcpServers.get()
  if sandbox.userPolicyJson.len > 0:
    result["userPolicy"] = parseJson(sandbox.userPolicyJson)

proc toWireJson*(caps: ModelCapabilitiesOverride): JsonNode =
  result = newJObject()
  let supports = newJObject()
  if caps.supportsVision.isSome: supports["vision"] = %caps.supportsVision.get()
  if caps.supportsToolCalls.isSome: supports["toolCalls"] = %caps.supportsToolCalls.get()
  if caps.supportsReasoningEffort.isSome:
    supports["reasoningEffort"] = %caps.supportsReasoningEffort.get()
  if supports.len > 0:
    result["supports"] = supports
  let limits = newJObject()
  if caps.maxPromptTokens > 0: limits["max_prompt_tokens"] = %caps.maxPromptTokens
  if caps.maxOutputTokens > 0: limits["max_output_tokens"] = %caps.maxOutputTokens
  if caps.maxContextWindowTokens > 0:
    limits["max_context_window_tokens"] = %caps.maxContextWindowTokens
  if limits.len > 0:
    result["limits"] = limits

proc toWireJson*(cfg: ImageGenerationConfig): JsonNode =
  result = newJObject()
  if cfg.enabled.isSome: result["enabled"] = %cfg.enabled.get()
  if cfg.watermark.isSome: result["watermark"] = %cfg.watermark.get()
  if cfg.saveToDisk.isSome: result["saveToDisk"] = %cfg.saveToDisk.get()

proc toWireJson*(cfg: AutopilotConfig): JsonNode =
  result = newJObject()
  if cfg.continuationEnabled.isSome:
    result["continuationEnabled"] = %cfg.continuationEnabled.get()
  if cfg.maxContinues.isSome: result["maxContinues"] = %cfg.maxContinues.get()

proc addConnectionFields(node: JsonNode; providerType, baseUrl, apiKey,
                         apiKeyCommand, bearerToken, wireApi, transport,
                         modelProvider: string; azure: AzureProviderOptions;
                         headers: Table[string, string]) =
  if providerType.len > 0: node["type"] = %providerType
  if baseUrl.len > 0: node["baseUrl"] = %baseUrl
  if apiKey.len > 0: node["apiKey"] = %apiKey
  if apiKeyCommand.len > 0: node["apiKeyCommand"] = %apiKeyCommand
  if bearerToken.len > 0: node["bearerToken"] = %bearerToken
  if wireApi.len > 0: node["wireApi"] = %wireApi
  if transport.len > 0: node["transport"] = %transport
  if modelProvider.len > 0: node["modelProvider"] = %modelProvider
  if azure.apiVersion.len > 0: node["azure"] = %*{"apiVersion": azure.apiVersion}
  if headers.len > 0:
    let h = newJObject()
    for k, v in headers:
      h[k] = %v
    node["headers"] = h

proc toWireJson*(p: ProviderConfig): JsonNode =
  ## Legacy flat `provider` object (translated by `groupSessionParams`).
  result = newJObject()
  result.addConnectionFields(p.`type`, p.baseUrl, p.apiKey, p.apiKeyCommand,
                             p.bearerToken, p.wireApi, p.transport,
                             p.modelProvider, p.azure, p.headers)
  if p.modelId.len > 0: result["modelId"] = %p.modelId
  if p.wireModel.len > 0: result["wireModel"] = %p.wireModel
  if p.maxPromptTokens > 0: result["maxPromptTokens"] = %p.maxPromptTokens
  if p.maxContextWindowTokens > 0:
    result["maxContextWindowTokens"] = %p.maxContextWindowTokens
  if p.maxOutputTokens > 0: result["maxOutputTokens"] = %p.maxOutputTokens

proc toWireJson*(p: NamedProviderConfig): JsonNode =
  result = %*{"name": p.name}
  result.addConnectionFields(p.`type`, p.baseUrl, p.apiKey, p.apiKeyCommand,
                             p.bearerToken, p.wireApi, p.transport,
                             p.modelProvider, p.azure, p.headers)

proc toWireJson*(m: ProviderModelConfig): JsonNode =
  result = %*{"provider": m.provider, "id": m.id}
  if m.modelId.len > 0: result["modelId"] = %m.modelId
  if m.wireModel.len > 0: result["wireModel"] = %m.wireModel
  if m.name.len > 0: result["name"] = %m.name
  if m.maxPromptTokens > 0: result["maxPromptTokens"] = %m.maxPromptTokens
  if m.maxContextWindowTokens > 0:
    result["maxContextWindowTokens"] = %m.maxContextWindowTokens
  if m.maxOutputTokens > 0: result["maxOutputTokens"] = %m.maxOutputTokens
  if m.capabilities.isSome:
    result["capabilities"] = m.capabilities.get().toWireJson()

proc toWireJson*(capi: CapiConfig): JsonNode =
  result = newJObject()
  if capi.enabled.isSome: result["enabled"] = %capi.enabled.get()

proc parsePolicyHelperFailure*(node: JsonNode): Option[PolicyHelperFailure] =
  ## Decodes a `policyHelperFailure` object, or none when absent.
  if node.isNil or node.kind != JObject:
    return none(PolicyHelperFailure)
  some(PolicyHelperFailure(
    category: node{"category"}.getStr(),
    failureMode: node{"failureMode"}.getStr(),
    message: node{"message"}.getStr(),
    previousPolicyRetained: node{"previousPolicyRetained"}.getBool(),
    reason: node{"reason"}.getStr(),
    authority: node{"authority"}.getStr(),
    code: node{"code"}.getStr(),
    diagnostic: node{"diagnostic"}.getStr(),
    details: node{"details"},
    helpLink: node{"helpLink"},
  ))

proc parsePermissionModeResult*(node: JsonNode): PermissionModeResult =
  ## Decodes a `session.permissions.setMode` result.
  if node.isNil or node.kind != JObject:
    return PermissionModeResult()
  PermissionModeResult(
    success: node{"success"}.getBool(),
    mode: node{"mode"}.getStr(),
    reason: node{"reason"}.getStr(),
  )

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
    mode*: ClientMode
      ## `cmCodingAgent` (default) or `cmEmpty`. See `ClientMode`.
    clientInfo*: ClientInfo
      ## Integrating application identity sent on the `connect` handshake.
    network*: NetworkOptions
      ## Proxy and proxy-authentication callbacks (`network.setProvider`).

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
    # --- v3.0.0 / SDK protocol 4 session options ---
    model*: string
      ## Model id, or "provider/id" for a model registered through `models`.
    tools*: seq[Tool]
      ## Tools declared for this session (`tools.definitions`).
    availableTools*: Option[seq[string]]
      ## Tool allowlist. Required on every session when the client mode is `cmEmpty`.
    askUserVariant*: string
      ## `AskUserVariantLegacy` or `AskUserVariantElicitation`.
    gitHubTokenProvider*: GitHubTokenProvider
      ## Supplies GitHub tokens on demand (`gitHubToken.getToken`).
      ## Mutually exclusive with `githubToken`.
    sandbox*: Option[SandboxConfig]
    featureFlags*: Table[string, bool]
      ## Runtime feature flag overrides (`experiments.flags`).
    allowedModels*: seq[string]
      ## Model allowlist (`policy.allowedModels`).
    authClientIdMetadataUrl*: string
      ## OAuth client metadata document URL for MCP servers.
    modelCapabilities*: Option[ModelCapabilitiesOverride]
    agentDirectories*: seq[string]
    hookDirectories*: seq[string]
    trustWorkingDirectory*: string
      ## Trust the working directory: `TrustWorkingDirectorySession` or
      ## `TrustWorkingDirectoryUser` (create only; empty leaves trust unchanged).
    imageGeneration*: Option[ImageGenerationConfig]
    autopilot*: Option[AutopilotConfig]
    hooks*: Table[string, HookHandler]
      ## Hook handlers keyed by hook type, for example `HookSubagentStart`.
    provider*: Option[ProviderConfig]
      ## Legacy single BYOK provider.
    providers*: seq[NamedProviderConfig]
    models*: seq[ProviderModelConfig]
    defaultProvider*: string
    capi*: Option[CapiConfig]
    onShellAuthenticationRequest*: ShellAuthenticationHandler
    onStoreMemory*: StoreMemoryHandler

  ResumeSessionConfig* = object
    sessionId*: string
    systemPrompt*: string
    githubToken*: string
    instructionDirectories*: seq[string]
    options*: SessionConfig
      ## Session options re-supplied on resume (tools, handlers, providers, ...).
      ## Create-only options such as `trustWorkingDirectory` are not sent.
    disableResume*: bool
      ## Suppress the resume event (`suppressResumeEvent` on the wire).
    allowTranscriptRecovery*: bool
      ## Allow recovering the session from its transcript when its state is unavailable.
    clearDefaultProvider*: bool
      ## Send an explicit null default provider to clear the resident one.

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
    source*: string
      ## Message provenance sent as `source`: "user", "system", or "agent-<id>".
      ## See `MessageSourceUser`, `MessageSourceSystem`, and `agentMessageSource`.
    clientTurnId*: string
      ## Opaque caller-chosen turn identifier sent as `clientTurnId`.
    responseSchemaJson*: string
      ## JSON Schema (as a JSON string) for structured output. Sent as
      ## `responseFormat: {type: "json_schema", jsonSchema: {...}}`.

const
  MessageSourceUser* = "user"
  MessageSourceSystem* = "system"

proc agentMessageSource*(agentId: string): string =
  ## Message source for a message authored by the agent with the given id.
  "agent-" & agentId

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
    sekToolShellOutput = "tool.shell_output"
    sekUnknown = "unknown"

  SessionEvent* = object
    kind*: SessionEventKind
    data*: JsonNode
    id*: string
    timestamp*: string
    parentId*: string
    agentId*: string
    ephemeral*: bool

  SendResult* = object
    message*: string
    turnId*: string
    rawEvents*: seq[SessionEvent]

  ShellOutputData* = object
    ## Payload of a `tool.shell_output` event: one chunk of live shell output.
    toolCallId*: string
    text*: string
    sequence*: int
    stream*: string
      ## "stdout", "stderr", or empty when the runtime omits it.

proc parseShellOutputData*(data: JsonNode): ShellOutputData =
  ## Decode the data of a `tool.shell_output` event.
  if data.isNil or data.kind != JObject:
    return ShellOutputData()
  ShellOutputData(
    toolCallId: data{"toolCallId"}.getStr(),
    text: data{"text"}.getStr(),
    sequence: data{"sequence"}.getInt(),
    stream: data{"stream"}.getStr(),
  )

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
                      tcpConnectionToken = "";
                      mode = cmCodingAgent;
                      clientInfo = ClientInfo();
                      network = NetworkOptions()): ClientConfig =
  ClientConfig(cliPath: cliPath, cliUrl: cliUrl, extraArgs: extraArgs,
               sessionIdleTimeoutSeconds: sessionIdleTimeoutSeconds,
               sessionFs: sessionFs, copilotHome: copilotHome,
               tcpConnectionToken: tcpConnectionToken, mode: mode,
               clientInfo: clientInfo, network: network)

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
                       argsSchemaJson = "";
                       model = "";
                       tools: seq[Tool] = @[];
                       availableTools = none(seq[string]);
                       askUserVariant = "";
                       gitHubTokenProvider: GitHubTokenProvider = nil;
                       sandbox = none(SandboxConfig);
                       featureFlags = initTable[string, bool]();
                       allowedModels: seq[string] = @[];
                       authClientIdMetadataUrl = "";
                       modelCapabilities = none(ModelCapabilitiesOverride);
                       agentDirectories: seq[string] = @[];
                       hookDirectories: seq[string] = @[];
                       trustWorkingDirectory = "";
                       imageGeneration = none(ImageGenerationConfig);
                       autopilot = none(AutopilotConfig);
                       hooks = initTable[string, HookHandler]();
                       provider = none(ProviderConfig);
                       providers: seq[NamedProviderConfig] = @[];
                       models: seq[ProviderModelConfig] = @[];
                       defaultProvider = "";
                       capi = none(CapiConfig);
                       onShellAuthenticationRequest: ShellAuthenticationHandler = nil;
                       onStoreMemory: StoreMemoryHandler = nil): SessionConfig =
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
    model: model,
    tools: tools,
    availableTools: availableTools,
    askUserVariant: askUserVariant,
    gitHubTokenProvider: gitHubTokenProvider,
    sandbox: sandbox,
    featureFlags: featureFlags,
    allowedModels: allowedModels,
    authClientIdMetadataUrl: authClientIdMetadataUrl,
    modelCapabilities: modelCapabilities,
    agentDirectories: agentDirectories,
    hookDirectories: hookDirectories,
    trustWorkingDirectory: trustWorkingDirectory,
    imageGeneration: imageGeneration,
    autopilot: autopilot,
    hooks: hooks,
    provider: provider,
    providers: providers,
    models: models,
    defaultProvider: defaultProvider,
    capi: capi,
    onShellAuthenticationRequest: onShellAuthenticationRequest,
    onStoreMemory: onStoreMemory,
  )

proc newResumeSessionConfig*(sessionId: string; systemPrompt = "";
                             githubToken = "";
                             instructionDirectories: seq[string] = @[];
                             options = SessionConfig();
                             disableResume = false;
                             allowTranscriptRecovery = false;
                             clearDefaultProvider = false): ResumeSessionConfig =
  ResumeSessionConfig(
    sessionId: sessionId,
    systemPrompt: systemPrompt,
    githubToken: githubToken,
    instructionDirectories: instructionDirectories,
    options: options,
    disableResume: disableResume,
    allowTranscriptRecovery: allowTranscriptRecovery,
    clearDefaultProvider: clearDefaultProvider,
  )

proc newMessageOptions*(message: string; streaming = false;
                        agentMode = ""; displayPrompt = "";
                        requestHeaders = initTable[string, string]();
                        source = ""; clientTurnId = "";
                        responseSchemaJson = ""): MessageOptions =
  MessageOptions(
    message: message,
    streaming: streaming,
    agentMode: agentMode,
    displayPrompt: displayPrompt,
    requestHeaders: requestHeaders,
    source: source,
    clientTurnId: clientTurnId,
    responseSchemaJson: responseSchemaJson,
  )
