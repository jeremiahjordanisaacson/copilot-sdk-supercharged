## Copilot client for the GitHub Copilot SDK for Nim.
##
## Manages the lifecycle of the Copilot CLI child process, JSON-RPC
## communication, tool registration, permission handling, and session
## creation and resumption.

import std/[json, options, osproc, streams, os, strutils, tables,
            asyncdispatch, locks, threadpool]
import ./types
import ./jsonrpc
import ./session
import ./tools as toolsMod

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
  )
  initLock(result.runLock)

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

  case meth
  of "tool.call":
    let toolName = params["name"].getStr()
    let callParams = if params.hasKey("parameters"): params["parameters"]
                     else: newJObject()
    var resultStr = """{"error": "Tool not found: """ & toolName & "\"}"

    for t in client.tools:
      if t.name == toolName:
        try:
          resultStr = t.handler(callParams)
        except CatchableError as e:
          resultStr = """{"error": """ & $(%e.msg) & "}"
        break

    let resp = buildResponse(id, %*{"result": resultStr})
    client.writeMessage(resp)

  of "permission.request":
    var decision = pdAllow
    if not client.permissionHandler.isNil:
      let req = PermissionRequest(
        id: params{"id"}.getStr(),
        toolName: params{"toolName"}.getStr(),
        description: params{"description"}.getStr(),
      )
      decision = client.permissionHandler(req)

    let resp = buildResponse(id, %*{"decision": $decision})
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
    let resp = buildResponse(id, %*{"status": "ok"})
    client.writeMessage(resp)

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
  while client.running:
    try:
      let msg = readMessage(client.outputStream)

      if msg.isResponse():
        let id = msg.getId()
        let err = msg.getError()
        if err.isSome:
          client.pending.reject(id, err.get().message)
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

  client.process = startProcess(
    cliPath,
    args = args,
    options = {poUsePath, poStdErrToStdOut},
  )

  client.inputStream = client.process.inputStream
  client.outputStream = client.process.outputStream
  client.running = true

  # Start the reader thread
  createThread(client.readerThread, readerLoop, client)

  # Verify connection with ping
  let pingResult = await client.sendRpcRequest("ping", newJObject())
  if pingResult.hasKey("protocolVersion"):
    let ver = pingResult["protocolVersion"].getStr()
    if not ver.startsWith("2"):
      raise newException(IOError,
        "Unsupported protocol version: " & ver & ". Requires v2.x")

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

proc createSession*(client: CopilotClient;
                    config: SessionConfig = newSessionConfig()): Future[CopilotSession] {.async.} =
  ## Create a new Copilot session.
  var params = %*{}
  if config.systemPrompt.len > 0:
    params["systemPrompt"] = %config.systemPrompt
  if config.githubToken.len > 0:
    params["githubToken"] = %config.githubToken
  if config.sessionIdleTimeoutSeconds > 0:
    params["sessionIdleTimeoutSeconds"] = %config.sessionIdleTimeoutSeconds
  if config.skillDirectories.len > 0:
    params["skillDirectories"] = %config.skillDirectories
  if config.disabledSkills.len > 0:
    params["disabledSkills"] = %config.disabledSkills
  if config.excludedTools.len > 0:
    params["excludedTools"] = %config.excludedTools

  let res = await client.sendRpcRequest("session.create", params)
  let sessionId = res["sessionId"].getStr()

  let sess = newCopilotSession(
    sessionId,
    config,
    proc(`method`: string; p: JsonNode): Future[JsonNode] =
      client.sendRpcRequest(`method`, p),
    proc(`method`: string; p: JsonNode) =
      client.sendNotification(`method`, p),
  )

  client.sessions[sessionId] = sess
  result = sess

proc resumeSession*(client: CopilotClient;
                    config: ResumeSessionConfig): Future[CopilotSession] {.async.} =
  ## Resume an existing session by ID.
  var params = %*{
    "sessionId": config.sessionId,
  }
  if config.systemPrompt.len > 0:
    params["systemPrompt"] = %config.systemPrompt
  if config.githubToken.len > 0:
    params["githubToken"] = %config.githubToken

  let res = await client.sendRpcRequest("session.resume", params)
  let sessionId = res["sessionId"].getStr()

  let sessConfig = newSessionConfig(
    systemPrompt = config.systemPrompt,
    githubToken = config.githubToken,
  )

  let sess = newCopilotSession(
    sessionId,
    sessConfig,
    proc(`method`: string; p: JsonNode): Future[JsonNode] =
      client.sendRpcRequest(`method`, p),
    proc(`method`: string; p: JsonNode) =
      client.sendNotification(`method`, p),
  )

  client.sessions[sessionId] = sess
  result = sess

proc getSession*(client: CopilotClient;
                 sessionId: string): Option[CopilotSession] =
  ## Look up a session by ID.
  if client.sessions.hasKey(sessionId):
    some(client.sessions[sessionId])
  else:
    none(CopilotSession)

proc removeSession*(client: CopilotClient; sessionId: string) =
  ## Remove a session from the client's tracked sessions.
  client.sessions.del(sessionId)

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
