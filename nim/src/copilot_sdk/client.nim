## Copilot client for the GitHub Copilot SDK for Nim.
##
## Manages the lifecycle of the Copilot CLI child process, JSON-RPC
## communication, tool registration, permission handling, and session
## creation and resumption.

import std/[json, options, osproc, streams, os, strutils, tables,
            asyncdispatch, locks, threadpool, sysrand]
import ./types
import ./jsonrpc
import ./session
import ./tools as toolsMod
import ./session_params
import ./sdk_protocol_version

type
  CopilotClient* = ref object
    config*: ClientConfig
    state*: ConnectionState
    process: Process
    inputStream: Stream
    outputStream: Stream
    pending: PendingRequests
    tools: seq[Tool]
    sessions: Table[string, CopilotSession]
    permissionHandler*: PermissionHandler
    userInputHandler*: UserInputHandler
    elicitationHandler*: ElicitationHandler
    readerThread: Thread[CopilotClient]
    running: bool
    runLock: Lock
    gitHubTokenProviders: Table[string, GitHubTokenProvider]

# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

proc newCopilotClient*(config: ClientConfig = newClientConfig()): CopilotClient =
  ## Create a new CopilotClient with the given configuration.
  result = CopilotClient(
    config: config,
    state: csDisconnected,
    pending: newPendingRequests(),
    tools: @[],
    sessions: initTable[string, CopilotSession](),
    running: false,
    gitHubTokenProviders: initTable[string, GitHubTokenProvider](),
  )
  initLock(result.runLock)

# ---------------------------------------------------------------------------
# Protocol version check
# ---------------------------------------------------------------------------

proc verifyProtocolVersion*(handshake: JsonNode) =
  ## Raise unless the `connect` / `ping` result reports exactly
  ## `SdkProtocolVersion`.
  let reported = if handshake.isNil or handshake.kind != JObject: nil
                 else: handshake{"protocolVersion"}
  if reported.isNil or reported.kind == JNull:
    raise newException(IOError,
      "SDK protocol version mismatch: SDK supports version " &
      $SdkProtocolVersion & ", but server does not report a protocol version. " &
      "Please update your SDK or server to ensure compatibility.")
  var serverVersion = ""
  var matches = false
  case reported.kind
  of JInt:
    serverVersion = $reported.getInt()
    matches = reported.getInt() == SdkProtocolVersion
  of JFloat:
    serverVersion = $reported.getFloat()
    matches = reported.getFloat() == float(SdkProtocolVersion)
  of JString:
    serverVersion = reported.getStr()
    matches = serverVersion == $SdkProtocolVersion
  else:
    serverVersion = $reported
  if not matches:
    raise newException(IOError,
      "SDK protocol version mismatch: SDK supports version " &
      $SdkProtocolVersion & ", but server reports version " & serverVersion &
      ". Please update your SDK or server to ensure compatibility.")

proc newRegistrationId(): string =
  ## Random RFC 4122 version 4 UUID used for callback registrations.
  var bytes: array[16, byte]
  if not urandom(bytes):
    raise newException(OSError, "Unable to generate a registration id")
  bytes[6] = (bytes[6] and 0x0f'u8) or 0x40'u8
  bytes[8] = (bytes[8] and 0x3f'u8) or 0x80'u8
  const hexDigits = "0123456789abcdef"
  result = newStringOfCap(36)
  for i, b in bytes:
    if i == 4 or i == 6 or i == 8 or i == 10:
      result.add('-')
    result.add(hexDigits[int(b shr 4)])
    result.add(hexDigits[int(b and 0x0f'u8)])

# ---------------------------------------------------------------------------
# CLI process resolution
# ---------------------------------------------------------------------------

proc resolveCliPath(client: CopilotClient): string =
  if client.config.cliPath.len > 0:
    return client.config.cliPath

  let envPath = getEnv("COPILOT_CLI_PATH")
  if envPath.len > 0:
    return envPath

  # Fallback: try to find in PATH
  let found = findExe("github-copilot")
  if found.len > 0:
    return found

  raise newException(IOError,
    "Copilot CLI not found. Set cliPath, COPILOT_CLI_PATH, or install the CLI.")

# ---------------------------------------------------------------------------
# Sending messages over the transport
# ---------------------------------------------------------------------------

proc writeMessage(client: CopilotClient; msg: JsonNode) =
  let encoded = encodeMessage(msg)
  withLock client.runLock:
    client.inputStream.write(encoded)
    client.inputStream.flush()

proc sendRpcRequest(client: CopilotClient; `method`: string;
                    params: JsonNode): Future[JsonNode] =
  let id = nextRequestId()
  let fut = client.pending.register(id)
  let req = buildRequest(`method`, params, id)
  client.writeMessage(req)
  result = fut

proc sendNotification(client: CopilotClient; `method`: string;
                      params: JsonNode) =
  let notif = buildNotification(`method`, params)
  client.writeMessage(notif)

# ---------------------------------------------------------------------------
# Server request dispatcher (tool calls, permissions, etc.)
# ---------------------------------------------------------------------------

proc handleServerRequest(client: CopilotClient; msg: JsonNode) =
  let meth = msg.getMethod()
  let params = msg.getParams()
  let id = msg.getId()
  let sessionId = params{"sessionId"}.getStr()
  let owner = if sessionId.len > 0 and client.sessions.hasKey(sessionId):
                client.sessions[sessionId]
              else: nil

  case meth
  of "tool.call":
    let toolName = if params.hasKey("toolName"): params["toolName"].getStr()
                   else: params{"name"}.getStr()
    let callParams = if params.hasKey("arguments"): params["arguments"]
                     elif params.hasKey("parameters"): params["parameters"]
                     else: newJObject()
    var resultStr = """{"error": """ & $(%("Tool not found: " & toolName)) & "}"

    var candidates: seq[Tool] = @[]
    if not owner.isNil:
      candidates.add(owner.config.tools)
    candidates.add(client.tools)
    for t in candidates:
      if t.name == toolName and not t.handler.isNil:
        try:
          resultStr = t.handler(callParams)
        except CatchableError as e:
          resultStr = """{"error": """ & $(%e.msg) & "}"
        break

    let resp = buildResponse(id, %*{"result": resultStr})
    client.writeMessage(resp)

  of "permission.request":
    var decision = pdAllow
    var decisionContext = ""
    if not client.permissionHandler.isNil:
      let req = PermissionRequest(
        id: params{"id"}.getStr(),
        toolName: params{"toolName"}.getStr(),
        description: params{"description"}.getStr(),
      )
      decision = client.permissionHandler(req)
      decisionContext = req.decisionContextJson

    var replyBody = %*{"decision": $decision}
    if decisionContext.len > 0:
      replyBody["decisionContext"] = parseJson(decisionContext)
    let resp = buildResponse(id, replyBody)
    client.writeMessage(resp)

  of "userInput.request":
    var answer = ""
    if not client.userInputHandler.isNil:
      let req = UserInputRequest(
        id: params{"id"}.getStr(),
        prompt: params{"prompt"}.getStr(),
      )
      answer = client.userInputHandler(req)

    let resp = buildResponse(id, %*{"response": answer})
    client.writeMessage(resp)

  of "elicitation.request":
    var answer = ""
    if not client.elicitationHandler.isNil:
      var opts: seq[string] = @[]
      if params.hasKey("options"):
        for o in params["options"]:
          opts.add(o.getStr())
      let req = ElicitationRequest(
        id: params{"id"}.getStr(),
        message: params{"message"}.getStr(),
        options: opts,
      )
      answer = client.elicitationHandler(req)

    let resp = buildResponse(id, %*{"response": answer})
    client.writeMessage(resp)

  of "hooks.invoke":
    let hookType = params{"hookType"}.getStr()
    let input = if params.hasKey("input"): params["input"] else: newJNull()
    var reply = newJObject()
    if not owner.isNil and owner.config.hooks.hasKey(hookType) and
        not owner.config.hooks[hookType].isNil:
      try:
        let output = owner.config.hooks[hookType](input)
        if not output.isNil and output.kind != JNull:
          reply["output"] = output
      except CatchableError as e:
        reply = nil
        client.writeMessage(buildErrorResponse(id, -32603,
          "Hook " & hookType & " failed: " & e.msg))
    if not reply.isNil:
      client.writeMessage(buildResponse(id, reply))

  of "gitHubToken.getToken":
    let registrationId = params{"registrationId"}.getStr()
    if not client.gitHubTokenProviders.hasKey(registrationId):
      client.writeMessage(buildErrorResponse(id, -32603,
        "No GitHub token provider is registered for this request"))
    else:
      let request = GitHubTokenRequest(
        registrationId: registrationId,
        host: params{"host"}.getStr(),
        sessionId: sessionId,
        reason: params{"reason"}.getStr(),
      )
      try:
        let answer = client.gitHubTokenProviders[registrationId](request)
        client.writeMessage(buildResponse(id, answer.toWireJson()))
      except CatchableError as e:
        client.writeMessage(buildErrorResponse(id, -32603,
          "GitHub token provider failed: " & e.msg))

  of "shellAuthentication.request":
    var reply = %*{"action": "cancel"}
    if not owner.isNil and not owner.config.onShellAuthenticationRequest.isNil:
      let request = ShellAuthenticationRequest(
        sessionId: sessionId,
        requestId: params{"requestId"}.getStr(),
        program: params{"program"}.getStr(),
        prompt: params{"prompt"}.getStr(),
        inputKind: params{"inputKind"}.getStr(),
        command: params{"command"}.getStr(),
        cwd: params{"cwd"}.getStr(),
      )
      try:
        let answer = owner.config.onShellAuthenticationRequest(request)
        if answer.accept:
          reply = %*{"action": "accept", "value": answer.value}
      except CatchableError:
        discard
    client.writeMessage(buildResponse(id, reply))

  of "shellAuthentication.cancel":
    # Requests are answered synchronously on the reader thread, so a
    # cancellation always refers to a request that was already answered.
    client.writeMessage(buildResponse(id, newJObject()))

  of "memoryStore.store":
    if owner.isNil or owner.config.onStoreMemory.isNil:
      client.writeMessage(buildErrorResponse(id, -32603,
        "Memory store owner is unavailable"))
    else:
      let memory = StoredMemory(
        sessionId: sessionId,
        subject: params{"subject"}.getStr(),
        fact: params{"fact"}.getStr(),
        citations: params{"citations"}.getStr(),
        reason: params{"reason"}.getStr(),
      )
      try:
        owner.config.onStoreMemory(memory)
        client.writeMessage(buildResponse(id, newJNull()))
      except CatchableError as e:
        client.writeMessage(buildErrorResponse(id, -32603,
          "Memory store failed: " & e.msg))

  of "network.resolveProxy":
    let resolver = client.config.network.resolveProxy
    if resolver.isNil:
      client.writeMessage(buildErrorResponse(id, -32601,
        "No network.resolveProxy callback registered"))
    else:
      try:
        let reply = newJObject()
        let proxy = resolver(params{"url"}.getStr())
        if proxy.isSome and proxy.get().len > 0:
          reply["proxy"] = %proxy.get()
        client.writeMessage(buildResponse(id, reply))
      except CatchableError:
        client.writeMessage(buildErrorResponse(id, -32603,
          "network.resolveProxy callback failed"))

  of "network.authenticateNegotiate":
    let authenticator = client.config.network.authenticateNegotiate
    if authenticator.isNil:
      client.writeMessage(buildErrorResponse(id, -32601,
        "No network.authenticateNegotiate callback registered"))
    else:
      try:
        let reply = newJObject()
        let token = authenticator(params{"spn"}.getStr())
        if token.isSome and token.get().len > 0:
          reply["token"] = %token.get()
        client.writeMessage(buildResponse(id, reply))
      except CatchableError:
        client.writeMessage(buildErrorResponse(id, -32603,
          "network.authenticateNegotiate callback failed"))

  of "network.authenticateBasic":
    let authenticator = client.config.network.authenticateBasic
    if authenticator.isNil:
      client.writeMessage(buildErrorResponse(id, -32601,
        "No network.authenticateBasic callback registered"))
    else:
      try:
        let reply = newJObject()
        let credentials = authenticator(NetworkBasicAuthRequest(
          proxy: params{"proxy"}.getStr(),
          realm: params{"realm"}.getStr(),
          previousRejected: params{"previousRejected"}.getBool(),
        ))
        if credentials.isSome:
          if credentials.get().username.len > 0:
            reply["username"] = %credentials.get().username
          if credentials.get().password.len > 0:
            reply["password"] = %credentials.get().password
        client.writeMessage(buildResponse(id, reply))
      except CatchableError:
        client.writeMessage(buildErrorResponse(id, -32603,
          "network.authenticateBasic callback failed"))

  else:
    let errResp = buildErrorResponse(id, -32601,
      "Method not found: " & meth)
    client.writeMessage(errResp)

# ---------------------------------------------------------------------------
# Notification dispatcher
# ---------------------------------------------------------------------------

proc handleServerNotification(client: CopilotClient; msg: JsonNode) =
  let meth = msg.getMethod()
  let params = msg.getParams()

  # Route to session if sessionId is present
  if params.hasKey("sessionId"):
    let sid = params["sessionId"].getStr()
    if client.sessions.hasKey(sid):
      client.sessions[sid].handleNotification(meth, params)

# ---------------------------------------------------------------------------
# Reader loop (runs in a separate thread)
# ---------------------------------------------------------------------------

proc readerLoop(client: CopilotClient) {.thread.} =
  # Handlers are user closures shared with the owning thread; the client
  # serializes writes with `runLock`, so the GC-safety check is waived here.
  {.cast(gcsafe).}:
    while client.running:
      try:
        let msg = readMessage(client.outputStream)

        if msg.isResponse():
          let id = msg.getId()
          let err = msg.getError()
          if err.isSome:
            client.pending.rejectWithError(id, err.get())
          else:
            client.pending.resolve(id, msg.getResult())
        elif msg.isRequest():
          client.handleServerRequest(msg)
        elif msg.isNotification():
          client.handleServerNotification(msg)
      except IOError:
        break
      except CatchableError:
        if not client.running:
          break

# ---------------------------------------------------------------------------
# Client lifecycle
# ---------------------------------------------------------------------------

proc start*(client: CopilotClient) {.async.} =
  ## Start the Copilot CLI process and establish the JSON-RPC connection.
  if client.state == csConnected:
    return

  client.state = csConnecting

  if client.config.cliUrl.len > 0:
    raise newException(IOError,
      "External server connections via cliUrl are not yet supported in Nim SDK. " &
      "Use cliPath to spawn a local CLI process.")

  let cliPath = client.resolveCliPath()
  var args = @["--headless", "--no-auto-update", "--stdio"]
  args.add(client.config.extraArgs)

  # A `.js` entry point (for example npm-loader.js) runs under node; any other
  # path (a native binary or the `.bin/copilot` shim) is executed directly.
  var command = cliPath
  if cliPath.toLowerAscii().endsWith(".js"):
    command = "node"
    args.insert(cliPath, 0)

  client.process = startProcess(
    command,
    args = args,
    options = {poUsePath, poStdErrToStdOut},
  )

  client.inputStream = client.process.inputStream
  client.outputStream = client.process.outputStream
  client.running = true

  # Start the reader thread
  createThread(client.readerThread, readerLoop, client)

  # Handshake. `connect` is only needed to declare `clientInfo`; servers
  # without `connect` (method not found) fall back to `ping`.
  var handshake: JsonNode = nil
  let clientInfo = client.config.clientInfo.toWireJson()
  if not clientInfo.isNil:
    try:
      handshake = await client.sendRpcRequest("connect", %*{"clientInfo": clientInfo})
    except CopilotRpcError as e:
      if e.code != -32601 and not e.msg.contains("Unhandled method connect"):
        raise
  if handshake.isNil:
    handshake = await client.sendRpcRequest("ping", newJObject())
  verifyProtocolVersion(handshake)

  # Register network callbacks (an empty object marks each registered callback).
  let network = client.config.network
  if not network.resolveProxy.isNil or not network.authenticateNegotiate.isNil or
      not network.authenticateBasic.isNil:
    let providerParams = newJObject()
    if network.scope.len > 0:
      providerParams["scope"] = %network.scope
    if not network.resolveProxy.isNil:
      providerParams["resolveProxy"] = newJObject()
    if not network.authenticateNegotiate.isNil:
      providerParams["authenticateNegotiate"] = newJObject()
    if not network.authenticateBasic.isNil:
      providerParams["authenticateBasic"] = newJObject()
    discard await client.sendRpcRequest("network.setProvider", providerParams)

  # Set up session filesystem provider if configured
  if client.config.sessionFs.initialCwd.len > 0:
    let fsParams = %*{
      "initialCwd": client.config.sessionFs.initialCwd,
      "sessionStatePath": client.config.sessionFs.sessionStatePath,
      "conventions": client.config.sessionFs.conventions,
    }
    discard await client.sendRpcRequest("sessionFs.setProvider", fsParams)

  client.state = csConnected

proc startSync*(client: CopilotClient) =
  ## Synchronous wrapper for start.
  waitFor client.start()

proc stop*(client: CopilotClient) =
  ## Stop the CLI process and clean up resources.
  client.running = false
  client.state = csDisconnected
  client.pending.rejectAll("Client stopped")

  if not client.process.isNil:
    try:
      client.process.terminate()
      discard client.process.waitForExit(timeout = 5000)
    except CatchableError:
      try:
        client.process.kill()
      except CatchableError:
        discard
    client.process.close()

  joinThread(client.readerThread)

# ---------------------------------------------------------------------------
# Tool management
# ---------------------------------------------------------------------------

proc registerTools*(client: CopilotClient; newTools: seq[Tool]) {.async.} =
  ## Register tools that the model can call during sessions.
  for t in newTools:
    client.tools.add(t)

  let toolsJson = toolsMod.toolsToJson(client.tools)
  discard await client.sendRpcRequest("tools.register", %*{
    "tools": toolsJson,
  })

proc registerTool*(client: CopilotClient; tool: Tool) {.async.} =
  ## Register a single tool.
  await client.registerTools(@[tool])

proc defineTool*(client: CopilotClient; name, description: string;
                 params: seq[ToolParameter];
                 handler: ToolHandler) {.async.} =
  ## Define and register a tool in one step.
  let tool = newTool(name, description, params, handler)
  await client.registerTool(tool)

# ---------------------------------------------------------------------------
# Session management
# ---------------------------------------------------------------------------

proc buildSessionParams(config: SessionConfig): JsonNode =
  ## Flat session payload shared by create and resume. `groupSessionParams`
  ## converts it to the grouped protocol 4 shape right before it is sent.
  var params = %*{}
  if config.model.len > 0:
    params["model"] = %config.model
  if config.systemPrompt.len > 0:
    params["systemMessage"] = %*{"mode": "append", "content": config.systemPrompt}
  if config.githubToken.len > 0:
    params["gitHubToken"] = %config.githubToken
  if config.sessionIdleTimeoutSeconds > 0:
    params["sessionIdleTimeoutSeconds"] = %config.sessionIdleTimeoutSeconds
  if config.skillDirectories.len > 0:
    params["skillDirectories"] = %config.skillDirectories
  if config.disabledSkills.len > 0:
    params["disabledSkills"] = %config.disabledSkills
  if config.excludedTools.len > 0:
    params["excludedTools"] = %config.excludedTools
  if config.enableCitations:
    params["enableCitations"] = %true
  if config.excludedBuiltinAgents.len > 0:
    params["excludedBuiltinAgents"] = %config.excludedBuiltinAgents
  if config.sessionLimits.maxAiCredits > 0:
    params["sessionLimits"] = %*{"maxAiCredits": config.sessionLimits.maxAiCredits}
  if config.memory.enabled:
    params["memory"] = %*{"enabled": true}
  if config.otlpProtocol.len > 0:
    params["otlpProtocol"] = %config.otlpProtocol
  if config.enableWebSocketResponses:
    params["enableWebSocketResponses"] = %true
  if config.expAssignmentsJson.len > 0:
    params["expAssignments"] = parseJson(config.expAssignmentsJson)
  if config.mcpAuthHandler or not config.onMcpAuthRequest.isNil:
    params["mcpAuthHandler"] = %true
  if config.enableConfigDiscovery:
    params["enableConfigDiscovery"] = %true
  if config.modelCapabilities.isSome:
    params["modelCapabilities"] = config.modelCapabilities.get().toWireJson()
  elif config.modelCapabilitiesJson.len > 0:
    params["modelCapabilities"] = parseJson(config.modelCapabilitiesJson)
  if config.mcpServersJson.len > 0:
    params["mcpServers"] = parseJson(config.mcpServersJson)
  if config.commandsJson.len > 0:
    params["commands"] = parseJson(config.commandsJson)
  if config.includeSubAgentStreamingEvents:
    params["includeSubAgentStreamingEvents"] = %true
  if config.instructionDirectories.len > 0:
    params["instructionDirectories"] = %config.instructionDirectories

  # --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
  if config.rewindEnabled:
    params["rewindEnabled"] = %true
  if config.additionalDirectories.len > 0:
    params["additionalDirectories"] = %config.additionalDirectories
  if config.disabledMcpServers.len > 0:
    params["disabledMcpServers"] = %config.disabledMcpServers
  if config.githubMcpToolConfigJson.len > 0:
    params["githubMcpToolConfig"] = parseJson(config.githubMcpToolConfigJson)
  if config.canvasProvider.len > 0:
    params["canvasProvider"] = %*{"id": config.canvasProvider}
  if config.customAgentsLocalOnly:
    params["customAgentsLocalOnly"] = %true
  if config.reasoningEffort.len > 0:
    params["reasoningEffort"] = %config.reasoningEffort
  if config.toolSearchJson.len > 0:
    params["toolSearch"] = parseJson(config.toolSearchJson)
  if config.experimentalMode:
    params["enableExperimentalMode"] = %true
  if config.contentExclusion:
    params["contentExclusion"] = %true
  if config.argsSchemaJson.len > 0:
    params["argsSchema"] = parseJson(config.argsSchemaJson)

  # --- v3.0.0 / SDK protocol 4 session options ---
  if config.tools.len > 0:
    params["tools"] = toolsMod.toolsToJson(config.tools)
  if config.availableTools.isSome:
    params["availableTools"] = %config.availableTools.get()
  if config.askUserVariant.len > 0:
    params["askUserVariant"] = %config.askUserVariant
  if config.sandbox.isSome:
    params["sandboxConfig"] = config.sandbox.get().toWireJson()
  if config.featureFlags.len > 0:
    params["featureFlags"] = %config.featureFlags
  if config.allowedModels.len > 0:
    params["allowedModels"] = %config.allowedModels
  if config.authClientIdMetadataUrl.len > 0:
    params["authClientIdMetadataUrl"] = %config.authClientIdMetadataUrl
  if config.agentDirectories.len > 0:
    params["agentDirectories"] = %config.agentDirectories
  if config.hookDirectories.len > 0:
    params["hookDirectories"] = %config.hookDirectories
  if config.trustWorkingDirectory.len > 0:
    params["trustWorkingDirectory"] = %config.trustWorkingDirectory
  if config.imageGeneration.isSome:
    params["imageGeneration"] = config.imageGeneration.get().toWireJson()
  if config.autopilot.isSome:
    params["autopilot"] = config.autopilot.get().toWireJson()
  if config.hooks.len > 0:
    params["hooks"] = %true
  if config.provider.isSome:
    params["provider"] = config.provider.get().toWireJson()
  if config.providers.len > 0:
    let providers = newJArray()
    for p in config.providers:
      providers.add(p.toWireJson())
    params["providers"] = providers
  if config.models.len > 0:
    let models = newJArray()
    for m in config.models:
      models.add(m.toWireJson())
    params["models"] = models
  if config.defaultProvider.len > 0:
    params["defaultProvider"] = %config.defaultProvider
  if config.capi.isSome:
    params["capi"] = config.capi.get().toWireJson()
  if not config.onShellAuthenticationRequest.isNil:
    params["requestShellAuthentication"] = %true
  if not config.onStoreMemory.isNil:
    params["hasMemoryStore"] = %true
  result = params

proc prepareTokenProvider(client: CopilotClient; config: SessionConfig;
                          params: JsonNode): string =
  ## Registers the session's GitHub token provider under a fresh id and adds
  ## it to the flat payload. Returns "" when no provider is configured.
  if config.gitHubTokenProvider.isNil:
    return ""
  if config.githubToken.len > 0:
    raise newException(ValueError,
      "gitHubTokenProvider and githubToken are mutually exclusive")
  result = newRegistrationId()
  client.gitHubTokenProviders[result] = config.gitHubTokenProvider
  params["gitHubTokenProviderRegistrationId"] = %result

proc registerSession(client: CopilotClient; sessionId: string;
                     config: SessionConfig;
                     registrationId: string): CopilotSession =
  if client.sessions.hasKey(sessionId):
    client.sessions[sessionId].runCloseCallbacks()
  let sess = newCopilotSession(
    sessionId,
    config,
    proc(`method`: string; p: JsonNode): Future[JsonNode] =
      client.sendRpcRequest(`method`, p),
    proc(`method`: string; p: JsonNode) =
      client.sendNotification(`method`, p),
  )
  if registrationId.len > 0:
    sess.addCloseCallback(proc() =
      client.gitHubTokenProviders.del(registrationId))
  client.sessions[sessionId] = sess
  result = sess

proc requireAvailableTools(client: CopilotClient; config: SessionConfig) =
  if client.config.mode == cmEmpty and config.availableTools.isNone:
    raise newException(ValueError,
      "Client mode \"empty\" requires availableTools on every session")

proc createSession*(client: CopilotClient;
                    config: SessionConfig = newSessionConfig()): Future[CopilotSession] {.async.} =
  ## Create a new Copilot session.
  client.requireAvailableTools(config)
  let params = buildSessionParams(config)
  let registrationId = client.prepareTokenProvider(config, params)
  let payload = groupSessionParams(params, isResume = false)
  if client.config.mode == cmEmpty:
    applyEmptyModeDefaults(payload)

  var res: JsonNode
  try:
    res = await client.sendRpcRequest("session.create", payload)
  except CatchableError as e:
    if registrationId.len > 0:
      client.gitHubTokenProviders.del(registrationId)
    raise e
  let sessionId = res["sessionId"].getStr()
  result = client.registerSession(sessionId, config, registrationId)

proc resumeSession*(client: CopilotClient;
                    config: ResumeSessionConfig): Future[CopilotSession] {.async.} =
  ## Resume an existing session by ID. Session options such as tools,
  ## handlers, and providers are re-supplied through `config.options`.
  var options = config.options
  if config.systemPrompt.len > 0:
    options.systemPrompt = config.systemPrompt
  if config.githubToken.len > 0:
    options.githubToken = config.githubToken
  if config.instructionDirectories.len > 0:
    options.instructionDirectories = config.instructionDirectories
  client.requireAvailableTools(options)

  let params = buildSessionParams(options)
  params["sessionId"] = %config.sessionId
  if config.disableResume:
    params["disableResume"] = %true
  if config.allowTranscriptRecovery:
    params["allowTranscriptRecovery"] = %true
  if config.clearDefaultProvider and options.defaultProvider.len == 0:
    params["defaultProvider"] = newJNull()
  let registrationId = client.prepareTokenProvider(options, params)
  let payload = groupSessionParams(params, isResume = true)
  if client.config.mode == cmEmpty:
    applyEmptyModeDefaults(payload)

  var res: JsonNode
  try:
    res = await client.sendRpcRequest("session.resume", payload)
  except CatchableError as e:
    if registrationId.len > 0:
      client.gitHubTokenProviders.del(registrationId)
    raise e
  let sessionId = res{"sessionId"}.getStr(config.sessionId)
  result = client.registerSession(sessionId, options, registrationId)

proc getSession*(client: CopilotClient;
                 sessionId: string): Option[CopilotSession] =
  ## Look up a session by ID.
  if client.sessions.hasKey(sessionId):
    some(client.sessions[sessionId])
  else:
    none(CopilotSession)

proc removeSession*(client: CopilotClient; sessionId: string) =
  ## Remove a session from the client's tracked sessions.
  if client.sessions.hasKey(sessionId):
    client.sessions[sessionId].runCloseCallbacks()
  client.sessions.del(sessionId)

proc deleteSession*(client: CopilotClient; sessionId: string) {.async.} =
  ## Delete a session on the server.
  let params = %*{"sessionId": sessionId}
  discard await client.sendRpcRequest("session.delete", params)
  client.removeSession(sessionId)

proc clearManagedSettingsCache*(client: CopilotClient) {.async.} =
  ## Clear the runtime's cached managed settings (`managedSettings.clearCache`).
  discard await client.sendRpcRequest("managedSettings.clearCache", newJObject())

proc listSessions*(client: CopilotClient): Future[JsonNode] {.async.} =
  ## List all sessions known to the server.
  result = await client.sendRpcRequest("session.list", newJObject())

proc getForegroundSessionId*(client: CopilotClient): Future[string] {.async.} =
  ## Get the foreground session ID from the server.
  let res = await client.sendRpcRequest("session.getForeground", newJObject())
  result = res["sessionId"].getStr()

proc setForegroundSessionId*(client: CopilotClient; sessionId: string) {.async.} =
  ## Set the foreground session to the given session ID.
  let params = %*{"sessionId": sessionId}
  let res = await client.sendRpcRequest("session.setForeground", params)
  if not res{"success"}.getBool(false):
    let errMsg = res{"error"}.getStr("Unknown")
    raise newException(IOError,
      "Failed to set foreground session: " & errMsg)

proc getLastSessionId*(client: CopilotClient): Future[Option[string]] {.async.} =
  ## Get the last session ID from the server. Returns none if not available.
  let res = await client.sendRpcRequest("session.getLastId", newJObject())
  let sid = res{"sessionId"}.getStr("")
  if sid.len > 0:
    result = some(sid)
  else:
    result = none(string)

proc getSessionMetadata*(client: CopilotClient; sessionId: string): Future[JsonNode] {.async.} =
  ## Get metadata for a session by ID.
  let params = %*{"sessionId": sessionId}
  result = await client.sendRpcRequest("session.getMetadata", params)

proc listModels*(client: CopilotClient): Future[JsonNode] {.async.} =
  ## List available models from the server.
  result = await client.sendRpcRequest("models.list", newJObject())

proc ping*(client: CopilotClient; message: string = "ping"): Future[JsonNode] {.async.} =
  ## Send a ping to verify connectivity.
  let params = %*{"message": message}
  result = await client.sendRpcRequest("ping", params)

proc getStatus*(client: CopilotClient): Future[JsonNode] {.async.} =
  ## Get the server status.
  result = await client.sendRpcRequest("status.get", newJObject())

proc getAuthStatus*(client: CopilotClient): Future[JsonNode] {.async.} =
  ## Get the authentication status.
  result = await client.sendRpcRequest("auth.getStatus", newJObject())
