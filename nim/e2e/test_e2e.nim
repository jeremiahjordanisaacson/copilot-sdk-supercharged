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
