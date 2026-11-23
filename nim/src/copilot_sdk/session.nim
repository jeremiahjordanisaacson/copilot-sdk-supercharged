## Session management for the GitHub Copilot SDK for Nim.
##
## A CopilotSession represents one conversation with the Copilot model.
## It supports sending messages, streaming events, subscribing to
## event callbacks, and managing the session lifecycle.

import std/[json, options, tables, asyncdispatch, strutils]
import ./types
import ./jsonrpc

type
  EventCallback* = proc(event: SessionEvent) {.closure.}

  CopilotSession* = ref object
    sessionId*: string
    config*: SessionConfig
    sendRpc: proc(`method`: string; params: JsonNode): Future[JsonNode] {.closure.}
    sendNotification: proc(`method`: string; params: JsonNode) {.closure.}
    listeners: Table[string, seq[EventCallback]]
    messages: seq[Message]
    turnCounter: int
    exitPlanModeHandler*: ExitPlanModeHandler
    closeCallbacks: seq[proc() {.closure.}]

# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

proc newCopilotSession*(sessionId: string; config: SessionConfig;
    sendRpc: proc(`method`: string; params: JsonNode): Future[JsonNode];
    sendNotification: proc(`method`: string; params: JsonNode)): CopilotSession =
  CopilotSession(
    sessionId: sessionId,
    config: config,
    sendRpc: sendRpc,
    sendNotification: sendNotification,
    listeners: initTable[string, seq[EventCallback]](),
    messages: @[],
    turnCounter: 0,
  )

# ---------------------------------------------------------------------------
# Event subscription
# ---------------------------------------------------------------------------

proc on*(session: CopilotSession; eventName: string; cb: EventCallback) =
  ## Subscribe to a session event by name. Use "*" for all events.
  if not session.listeners.hasKey(eventName):
    session.listeners[eventName] = @[]
  session.listeners[eventName].add(cb)

proc emit*(session: CopilotSession; event: SessionEvent) =
  ## Dispatch an event to registered listeners.
  let name = $event.kind
  if session.listeners.hasKey(name):
    for cb in session.listeners[name]:
      cb(event)
  if session.listeners.hasKey("*"):
    for cb in session.listeners["*"]:
      cb(event)

# ---------------------------------------------------------------------------
# Internal event helpers
# ---------------------------------------------------------------------------

proc parseEventKind(raw: string): SessionEventKind =
  case raw
  of "assistant.message": sekAssistantMessage
  of "assistant.message_delta": sekAssistantMessageDelta
  of "assistant.reasoning": sekAssistantReasoning
  of "assistant.reasoning_delta": sekAssistantReasoningDelta
  of "session.idle": sekSessionIdle
  of "tool.call": sekToolCall
  of "tool.result": sekToolResult
  of "permission.request": sekPermissionRequest
  of "userInput.request": sekUserInputRequest
  of "hooks.invoke": sekHooksInvoke
  of "elicitation.request": sekElicitationRequest
  of "session.compaction_start": sekSessionCompactionStart
  of "session.compaction_complete": sekSessionCompactionComplete
  of "tool.shell_output": sekToolShellOutput
  else: sekUnknown

proc handleNotification*(session: CopilotSession; `method`: string;
                         params: JsonNode) =
  ## Process an incoming notification from the server. `session.event`
  ## notifications are unwrapped into their event type and data; unknown
  ## event types are delivered as `sekUnknown`.
  var event = SessionEvent(kind: parseEventKind(`method`), data: params)
  if `method` == "session.event" and params.hasKey("event") and
      params["event"].kind == JObject:
    let raw = params["event"]
    event = SessionEvent(
      kind: parseEventKind(raw{"type"}.getStr()),
      data: if raw.hasKey("data"): raw["data"] else: newJObject(),
      id: raw{"id"}.getStr(),
      timestamp: raw{"timestamp"}.getStr(),
      parentId: raw{"parentId"}.getStr(),
      agentId: raw{"agentId"}.getStr(),
      ephemeral: raw{"ephemeral"}.getBool(),
    )
  let kind = event.kind
  let data = event.data
  session.emit(event)

  # Track assistant messages in local history
  if kind == sekAssistantMessage:
    let content = if data.hasKey("content"): data["content"].getStr()
                  elif data.hasKey("message"): data["message"].getStr()
                  else: ""
    session.messages.add(Message(
      role: mrAssistant,
      content: content,
      turnId: if data.hasKey("turnId"): data["turnId"].getStr() else: "",
    ))

# ---------------------------------------------------------------------------
# Sending messages
# ---------------------------------------------------------------------------

proc buildSendParams(session: CopilotSession; opts: MessageOptions): JsonNode =
  inc session.turnCounter
  let turnId = "turn-" & $session.turnCounter

  session.messages.add(Message(
    role: mrUser,
    content: opts.message,
    turnId: turnId,
  ))

  result = %*{
    "sessionId": session.sessionId,
    "prompt": opts.message,
    "turnId": turnId,
  }
  if opts.streaming:
    result["streaming"] = %true
  if opts.agentMode.len > 0:
    result["agentMode"] = %opts.agentMode
  if opts.displayPrompt.len > 0:
    result["displayPrompt"] = %opts.displayPrompt
  if opts.requestHeaders.len > 0:
    let headers = newJObject()
    for k, v in opts.requestHeaders:
      headers[k] = %v
    result["requestHeaders"] = headers
  if opts.source.len > 0:
    result["source"] = %opts.source
  if opts.clientTurnId.len > 0:
    result["clientTurnId"] = %opts.clientTurnId
  if opts.responseSchemaJson.len > 0:
    result["responseFormat"] = %*{
      "type": "json_schema",
      "jsonSchema": {
        "name": "response",
        "strict": true,
        "schema": parseJson(opts.responseSchemaJson),
      },
    }

proc send*(session: CopilotSession; opts: MessageOptions): Future[JsonNode] {.async.} =
  ## Send a message to the session without waiting for idle.
  ## Returns the immediate RPC response.
  let params = session.buildSendParams(opts)
  result = await session.sendRpc("session.send", params)

proc sendAndWait*(session: CopilotSession;
                  opts: MessageOptions): Future[SendResult] {.async.} =
  ## Send a message and wait for the session to become idle.
  ## Returns the final assistant response.
  let params = session.buildSendParams(opts)
  let rpcResult = await session.sendRpc("session.send", params)

  # Wait for session.idle
  let idleResult = await session.sendRpc("session.idle", %*{
    "sessionId": session.sessionId,
  })

  var msg = ""
  var turnId = ""
  if idleResult.hasKey("message"):
    msg = idleResult["message"].getStr()
  if idleResult.hasKey("turnId"):
    turnId = idleResult["turnId"].getStr()

  result = SendResult(
    message: msg,
    turnId: turnId,
    rawEvents: @[],
  )

# ---------------------------------------------------------------------------
# Session lifecycle
# ---------------------------------------------------------------------------

proc getMessages*(session: CopilotSession): seq[Message] =
  ## Return the local message history for this session.
  session.messages

proc addCloseCallback*(session: CopilotSession; callback: proc() {.closure.}) =
  ## Register a callback that runs once when the session is disconnected,
  ## destroyed, or detached. The client uses it to unregister per-session
  ## callbacks such as the GitHub token provider.
  session.closeCallbacks.add(callback)

proc runCloseCallbacks*(session: CopilotSession) =
  ## Run and clear the registered close callbacks.
  let callbacks = session.closeCallbacks
  session.closeCallbacks = @[]
  for callback in callbacks:
    callback()

proc disconnect*(session: CopilotSession) {.async.} =
  ## Disconnect this session gracefully.
  discard await session.sendRpc("session.disconnect", %*{
    "sessionId": session.sessionId,
  })
  session.runCloseCallbacks()

proc destroy*(session: CopilotSession) {.async.} =
  ## Destroy this session and release server resources.
  discard await session.sendRpc("session.destroy", %*{
    "sessionId": session.sessionId,
  })
  session.runCloseCallbacks()

proc abort*(session: CopilotSession) {.async.} =
  ## Abort the current turn for this session.
  discard await session.sendRpc("session.abort", %*{
    "sessionId": session.sessionId,
  })

proc detach*(session: CopilotSession) {.async.} =
  ## Detach this client from the session without ending it (`session.detach`).
  ## The session keeps running in the runtime and can be resumed later.
  discard await session.sendRpc("session.detach", %*{
    "sessionId": session.sessionId,
  })
  session.runCloseCallbacks()

proc switchAutoTier*(session: CopilotSession; tier: AutoTier): Future[JsonNode] {.async.} =
  ## Switch the Auto model routing tier (`session.model.switchAutoTier`).
  result = await session.sendRpc("session.model.switchAutoTier", %*{
    "sessionId": session.sessionId,
    "autoTier": $tier,
  })

proc recheckPolicyHelper*(session: CopilotSession): Future[JsonNode] {.async.} =
  ## Ask the runtime for a fresh Policy Helper evaluation
  ## (`session.managedSettings.recheckPolicyHelper`).
  result = await session.sendRpc("session.managedSettings.recheckPolicyHelper", %*{
    "sessionId": session.sessionId,
  })

proc setPermissionMode*(session: CopilotSession;
                        mode: string): Future[PermissionModeResult] {.async.} =
  ## Set the session permission mode (`session.permissions.setMode`). When the
  ## mode is refused, `reason` explains why (for example
  ## `PermissionModeRefusalManagedPolicy`).
  let res = await session.sendRpc("session.permissions.setMode", %*{
    "sessionId": session.sessionId,
    "mode": mode,
  })
  result = parsePermissionModeResult(res)

# ---------------------------------------------------------------------------
# Session state for persistence
# ---------------------------------------------------------------------------

proc getState*(session: CopilotSession): JsonNode =
  ## Serialize session state for persistence.
  let msgs = newJArray()
  for m in session.messages:
    msgs.add(%*{
      "role": $m.role,
      "content": m.content,
      "turnId": m.turnId,
    })
  result = %*{
    "sessionId": session.sessionId,
    "turnCounter": session.turnCounter,
    "messages": msgs,
  }
