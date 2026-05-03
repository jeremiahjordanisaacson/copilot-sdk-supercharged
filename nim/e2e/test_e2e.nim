## E2E tests for the Copilot SDK

import std/[unittest, os, tempfiles]
import test_harness
import copilot_sdk
import copilot_sdk/[client, session, types]

proc getRepoRoot(): string =
  normalizedPath(currentSourcePath().parentDir() / ".." / ".." / "..")

suite "Copilot SDK E2E":

  test "session create and disconnect":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    check sess.id.len > 0

    cl.stop()

  test "send message":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    let response = sess.sendAndWait(MessageOptions(content: "Hello"))
    check response != nil

    cl.stop()

  test "session fs config":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl
    config.sessionFs = SessionFsConfig(
      initialCwd: getTempDir(),
      sessionStatePath: getTempDir() / "copilot-state"
    )

    var cl = newCopilotClient(config)
    # Starting should internally call sessionFs.setProvider
    cl.start()
    check true  # If we got here, SessionFs config was accepted

    cl.stop()

  test "multi-turn conversation":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    let response1 = sess.sendAndWait(MessageOptions(content: "What is 1+1?"))
    check response1 != nil

    let response2 = sess.sendAndWait(MessageOptions(content: "And what is 2+2?"))
    check response2 != nil

    cl.stop()

  test "session resume":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    let sessionId = sess.id
    check sessionId.len > 0

    cl.stop()

    var config2 = newClientConfig()
    config2.cliUrl = proxyUrl

    var cl2 = newCopilotClient(config2)
    cl2.start()

    let resumed = cl2.createSession(SessionConfig(sessionId: sessionId))
    check resumed.id == sessionId

    cl2.stop()

  test "session list":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    discard cl.createSession()
    discard cl.createSession()

    let sessions = cl.listSessions()
    check sessions.len >= 2

    cl.stop()

  test "session metadata":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    let metadata = cl.getSessionMetadata(sess.id)
    check metadata != nil

    cl.stop()

  test "session delete":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    let sessionId = sess.id

    cl.deleteSession(sessionId)

    let sessions = cl.listSessions()
    var found = false
    for s in sessions:
      if s.id == sessionId:
        found = true
        break
    check not found

    cl.stop()

  test "model list":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let models = cl.listModels()
    check models.len > 0

    cl.stop()

  test "ping":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let result = cl.ping()
    check result != nil

    cl.stop()

  test "auth status":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let auth = cl.getAuthStatus()
    check auth != nil

    cl.stop()

  test "client lifecycle":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()
    check cl.state == "connected"

    cl.stop()
    check cl.state == "disconnected"

  test "foreground session":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()
    let sessionId = sess.id

    cl.setForegroundSessionId(sessionId)
    let fgId = cl.getForegroundSessionId()
    check fgId == sessionId

    cl.stop()

  test "tools":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession(SessionConfig(
      tools: @[ToolDefinition(
        name: "test_tool",
        description: "A test tool for E2E testing"
      )]
    ))
    check sess.id.len > 0

    let response = sess.sendAndWait(MessageOptions(content: "Use the test_tool"))
    check response != nil

    cl.stop()

  test "streaming":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession(SessionConfig(streaming: true))
    let response = sess.sendAndWait(MessageOptions(content: "Hello"))
    check response != nil

    cl.stop()

  test "system message customization":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession(SessionConfig(
      systemMessage: SystemMessageConfig(
        mode: "append",
        content: "You are a helpful test assistant."
      )
    ))
    check sess.id.len > 0

    cl.stop()

  test "mcp servers config":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession(SessionConfig(
      mcpServers: @[McpServerConfig(
        url: "http://localhost:9999/mcp"
      )]
    ))
    check sess.id.len > 0

    cl.stop()

  test "skills config":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession(SessionConfig(
      skills: SkillsConfig(
        directories: @[getRepoRoot()]
      )
    ))
    check sess.id.len > 0

    cl.stop()

  test "compaction":
    var proxy = newCapiProxy()
    defer: proxy.stop()

    let proxyUrl = proxy.start()
    proxy.configure("test/snapshots/basic.yaml", getRepoRoot())

    var config = newClientConfig()
    config.cliUrl = proxyUrl

    var cl = newCopilotClient(config)
    cl.start()

    let sess = cl.createSession()

    # Send multiple messages to trigger compaction events
    for i in 1..5:
      discard sess.sendAndWait(MessageOptions(content: "Message number " & $i))

    check true  # If we got here, multiple messages completed (compaction may have occurred)

    cl.stop()
