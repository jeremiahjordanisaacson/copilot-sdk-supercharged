## Unit tests for the GitHub Copilot SDK for Nim.
##
## Tests cover JSON-RPC framing, message building, types, tool schema
## generation, session event dispatch, and client configuration.

import std/[unittest, json, options, tables, asyncdispatch, strutils]
import ../src/copilot_sdk/types
import ../src/copilot_sdk/jsonrpc
import ../src/copilot_sdk/tools
import ../src/copilot_sdk/session

# ===========================================================================
# JSON-RPC tests
# ===========================================================================

suite "JSON-RPC message framing":
  test "encodeMessage produces Content-Length header":
    let payload = %*{"jsonrpc": "2.0", "id": 1, "method": "ping"}
    let encoded = encodeMessage(payload)
    check encoded.startsWith("Content-Length: ")
    check "\r\n\r\n" in encoded

  test "encodeMessage body length matches header":
    let payload = %*{"test": "data"}
    let encoded = encodeMessage(payload)
    let parts = encoded.split("\r\n\r\n", maxsplit = 1)
    let headerVal = parseContentLength(parts[0])
    check headerVal == parts[1].len

  test "parseContentLength extracts value":
    check parseContentLength("Content-Length: 42") == 42
    check parseContentLength("Content-Length: 0") == 0

  test "parseContentLength rejects invalid header":
    expect ValueError:
      discard parseContentLength("Invalid-Header: 10")

suite "JSON-RPC message builders":
  test "buildRequest creates valid request":
    let req = buildRequest("test.method", %*{"key": "value"}, id = 99)
    check req["jsonrpc"].getStr() == "2.0"
    check req["id"].getInt() == 99
    check req["method"].getStr() == "test.method"
    check req["params"]["key"].getStr() == "value"

  test "buildNotification has no id":
    let notif = buildNotification("test.event", %*{"data": 1})
    check notif["jsonrpc"].getStr() == "2.0"
    check notif["method"].getStr() == "test.event"
    check not notif.hasKey("id")

  test "buildResponse creates success response":
    let resp = buildResponse(5, %*{"status": "ok"})
    check resp["id"].getInt() == 5
    check resp["result"]["status"].getStr() == "ok"
    check not resp.hasKey("error")

  test "buildErrorResponse creates error response":
    let resp = buildErrorResponse(7, -32600, "Invalid request")
    check resp["id"].getInt() == 7
    check resp["error"]["code"].getInt() == -32600
    check resp["error"]["message"].getStr() == "Invalid request"

suite "JSON-RPC message classification":
  test "isRequest identifies request messages":
    let req = buildRequest("foo", id = 1)
    check req.isRequest()
    check not req.isResponse()
    check not req.isNotification()

  test "isResponse identifies response messages":
    let resp = buildResponse(1, %*{})
    check resp.isResponse()
    check not resp.isRequest()
    check not resp.isNotification()

  test "isNotification identifies notification messages":
    let notif = buildNotification("bar")
    check notif.isNotification()
    check not notif.isRequest()
    check not notif.isResponse()

  test "getMethod extracts method name":
    let req = buildRequest("session.send", id = 1)
    check req.getMethod() == "session.send"

  test "getParams returns params or empty object":
    let req = buildRequest("test", %*{"a": 1}, id = 1)
    check req.getParams()["a"].getInt() == 1

    let req2 = %*{"jsonrpc": "2.0", "id": 2, "method": "nop"}
    check req2.getParams().kind == JObject

  test "getError returns none for success response":
    let resp = buildResponse(1, %*{})
    check resp.getError().isNone

  test "getError returns error object for error response":
    let resp = buildErrorResponse(1, -32601, "Method not found")
    let err = resp.getError()
    check err.isSome
    check err.get().code == -32601
    check err.get().message == "Method not found"

# ===========================================================================
# Pending requests tests
# ===========================================================================

suite "PendingRequests tracker":
  test "register and resolve":
    let pr = newPendingRequests()
    let fut = pr.register(1)
    pr.resolve(1, %*{"ok": true})
    let val = waitFor fut
    check val["ok"].getBool() == true

  test "reject causes future to fail":
    let pr = newPendingRequests()
    let fut = pr.register(2)
    pr.reject(2, "test error")
    var caught = false
    try:
      discard waitFor fut
    except IOError:
      caught = true
    check caught

  test "rejectAll clears all pending":
    let pr = newPendingRequests()
    let f1 = pr.register(10)
    let f2 = pr.register(11)
    pr.rejectAll("shutdown")
    var errors = 0
    try: discard waitFor f1
    except IOError: inc errors
    try: discard waitFor f2
    except IOError: inc errors
    check errors == 2

# ===========================================================================
# Types tests
# ===========================================================================

suite "Type constructors":
  test "newClientConfig defaults":
    let cfg = newClientConfig()
    check cfg.cliPath == ""
    check cfg.cliUrl == ""
    check cfg.extraArgs.len == 0

  test "newClientConfig with values":
    let cfg = newClientConfig(cliPath = "/usr/bin/copilot",
                              extraArgs = @["--verbose"])
    check cfg.cliPath == "/usr/bin/copilot"
    check cfg.extraArgs == @["--verbose"]

  test "newSessionConfig defaults":
    let cfg = newSessionConfig()
    check cfg.systemPrompt == ""
    check cfg.sessionIdleTimeoutSeconds == 0
    check cfg.skillDirectories.len == 0

  test "newSessionConfig with prompt":
    let cfg = newSessionConfig(systemPrompt = "Be helpful")
    check cfg.systemPrompt == "Be helpful"

  test "newResumeSessionConfig":
    let cfg = newResumeSessionConfig("sess-123", systemPrompt = "Resume")
    check cfg.sessionId == "sess-123"
    check cfg.systemPrompt == "Resume"

  test "newMessageOptions defaults":
    let opts = newMessageOptions("Hello")
    check opts.message == "Hello"
    check opts.streaming == false
    check opts.requestHeaders.len == 0

  test "newMessageOptions with streaming":
    let opts = newMessageOptions("Hi", streaming = true)
    check opts.streaming == true

  test "MessageRole enum string values":
    check $mrUser == "user"
    check $mrAssistant == "assistant"
    check $mrSystem == "system"

  test "ConnectionState enum values":
    check $csDisconnected == "disconnected"
    check $csConnected == "connected"

  test "PermissionDecision enum values":
    check $pdAllow == "allow"
    check $pdDeny == "deny"

# ===========================================================================
# Tool tests
# ===========================================================================

suite "Tool definition and schema":
  test "toJsonSchema generates valid schema":
    let params = @[
      newToolParameter("city", "string", "City name"),
      newToolParameter("units", "string", "Temperature units", required = false),
    ]
    let schema = toJsonSchema(params)
    check schema["type"].getStr() == "object"
    check schema["properties"]["city"]["type"].getStr() == "string"
    check schema["properties"]["city"]["description"].getStr() == "City name"
    check schema["required"].len == 1
    check schema["required"][0].getStr() == "city"

  test "toJsonSchema with no required fields":
    let params = @[
      newToolParameter("opt", "string", "Optional", required = false),
    ]
    let schema = toJsonSchema(params)
    check not schema.hasKey("required")

  test "toJsonSchema with all required":
    let params = @[
      newToolParameter("a", "string", "A"),
      newToolParameter("b", "integer", "B"),
    ]
    let schema = toJsonSchema(params)
    check schema["required"].len == 2

  test "newTool constructs tool with handler":
    var called = false
    let tool = newTool("test", "A test tool", @[], proc(p: JsonNode): string =
      called = true
      "result"
    )
    check tool.name == "test"
    check tool.description == "A test tool"
    discard tool.handler(newJObject())
    check called

  test "defineTool template works":
    let tool = defineTool("greet", "Greet by name", @[
      newToolParameter("name", "string", "Name"),
    ]):
      let name = params{"name"}.getStr("World")
      "Hello, " & name & "!"

    check tool.name == "greet"
    let result = tool.handler(%*{"name": "Nim"})
    check result == "Hello, Nim!"

  test "toRegistrationJson serializes tool":
    let tool = newTool("calc", "Calculator", @[
      newToolParameter("expr", "string", "Expression"),
    ], proc(p: JsonNode): string = "42")
    let j = tool.toRegistrationJson()
    check j["name"].getStr() == "calc"
    check j["description"].getStr() == "Calculator"
    check j["parameters"]["properties"]["expr"]["type"].getStr() == "string"

  test "toolsToJson creates array":
    let t1 = newTool("a", "Tool A", @[], proc(p: JsonNode): string = "a")
    let t2 = newTool("b", "Tool B", @[], proc(p: JsonNode): string = "b")
    let arr = toolsToJson(@[t1, t2])
    check arr.len == 2
    check arr[0]["name"].getStr() == "a"
    check arr[1]["name"].getStr() == "b"

# ===========================================================================
# Session tests
# ===========================================================================

suite "CopilotSession event dispatch":
  test "on and emit deliver events":
    var received = ""
    let sess = newCopilotSession(
      "test-sess",
      newSessionConfig(),
      proc(m: string; p: JsonNode): Future[JsonNode] =
        let f = newFuture[JsonNode]("mock")
        f.complete(newJObject())
        f,
      proc(m: string; p: JsonNode) = discard,
    )

    sess.on("assistant.message", proc(e: SessionEvent) =
      received = e.data{"message"}.getStr()
    )

    sess.emit(SessionEvent(
      kind: sekAssistantMessage,
      data: %*{"message": "Hello from test"},
    ))

    check received == "Hello from test"

  test "wildcard listener receives all events":
    var count = 0
    let sess = newCopilotSession(
      "test-sess",
      newSessionConfig(),
      proc(m: string; p: JsonNode): Future[JsonNode] =
        let f = newFuture[JsonNode]("mock")
        f.complete(newJObject())
        f,
      proc(m: string; p: JsonNode) = discard,
    )

    sess.on("*", proc(e: SessionEvent) = inc count)

    sess.emit(SessionEvent(kind: sekAssistantMessage, data: newJObject()))
    sess.emit(SessionEvent(kind: sekSessionIdle, data: newJObject()))

    check count == 2

  test "handleNotification dispatches to listeners":
    var receivedKind = sekUnknown
    let sess = newCopilotSession(
      "test-sess",
      newSessionConfig(),
      proc(m: string; p: JsonNode): Future[JsonNode] =
        let f = newFuture[JsonNode]("mock")
        f.complete(newJObject())
        f,
      proc(m: string; p: JsonNode) = discard,
    )

    sess.on("session.idle", proc(e: SessionEvent) =
      receivedKind = e.kind
    )

    sess.handleNotification("session.idle", %*{"sessionId": "test-sess"})
    check receivedKind == sekSessionIdle

  test "handleNotification tracks assistant messages":
    let sess = newCopilotSession(
      "test-sess",
      newSessionConfig(),
      proc(m: string; p: JsonNode): Future[JsonNode] =
        let f = newFuture[JsonNode]("mock")
        f.complete(newJObject())
        f,
      proc(m: string; p: JsonNode) = discard,
    )

    sess.handleNotification("assistant.message", %*{
      "sessionId": "test-sess",
      "message": "Response text",
      "turnId": "turn-1",
    })

    let msgs = sess.getMessages()
    check msgs.len == 1
    check msgs[0].role == mrAssistant
    check msgs[0].content == "Response text"

  test "getState returns serializable state":
    let sess = newCopilotSession(
      "persist-sess",
      newSessionConfig(),
      proc(m: string; p: JsonNode): Future[JsonNode] =
        let f = newFuture[JsonNode]("mock")
        f.complete(newJObject())
        f,
      proc(m: string; p: JsonNode) = discard,
    )

    sess.handleNotification("assistant.message", %*{
      "sessionId": "persist-sess",
      "message": "Stored message",
      "turnId": "t-1",
    })

    let state = sess.getState()
    check state["sessionId"].getStr() == "persist-sess"
    check state["messages"].len == 1
    check state["messages"][0]["content"].getStr() == "Stored message"

# ===========================================================================
# Client config tests
# ===========================================================================

suite "CopilotClient construction":
  test "newCopilotClient creates disconnected client":
    let client = newCopilotClient()
    check client.state == csDisconnected

  test "newCopilotClient accepts config":
    let cfg = newClientConfig(cliPath = "/my/copilot")
    let client = newCopilotClient(cfg)
    check client.config.cliPath == "/my/copilot"

echo "\nAll tests passed."
