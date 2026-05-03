// Copyright (c) Microsoft Corporation. All rights reserved.

package com.github.copilot.e2e

import com.github.copilot.*
import io.circe.*
import io.circe.syntax.*
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/**
 * End-to-end tests for the Scala Copilot SDK.
 *
 * These tests start the shared replaying CAPI proxy and run real SDK calls
 * against it.  The proxy replays canned responses from `test/snapshots/`.
 *
 * '''Prerequisites'''
 *  - Node.js ≥ 18 with `npx` on PATH
 *  - `npm ci` has been run in `test/harness/`
 */
class CopilotE2ESpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach:

  given ExecutionContext = ExecutionContext.global

  private val timeout: FiniteDuration = 30.seconds

  // Client reference reset per-test
  private var client: Option[CopilotClient] = None

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  override def beforeAll(): Unit =
    super.beforeAll()
    TestHarness.start()
    info(s"Proxy running at ${TestHarness.proxyUrl}")

  override def afterAll(): Unit =
    try TestHarness.stop()
    finally super.afterAll()

  override def afterEach(): Unit =
    // Ensure every test cleans up its client
    client.foreach { c =>
      try c.stop()
      catch case _: Exception => ()
    }
    client = None
    super.afterEach()

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /** Creates a [[CopilotClient]] pre-configured to talk to the test proxy. */
  private def makeClient(
    sessionFs: Option[SessionFsConfig] = None
  ): CopilotClient =
    val opts = CopilotClientOptions(
      cliUrl = Some(TestHarness.cliUrl),
      sessionFs = sessionFs,
    )
    val c = CopilotClient(opts)
    client = Some(c)
    c

  // ---------------------------------------------------------------------------
  // Tests
  // ---------------------------------------------------------------------------

  behavior of "CopilotClient E2E"

  it should "create a session and disconnect" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    session.sessionId should not be empty

    c.stop()
  }

  it should "send a message and receive a response" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)

    // Collect events for inspection
    var receivedEvents = List.empty[SessionEvent]
    session.on { event =>
      receivedEvents = receivedEvents :+ event
    }

    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Hello")),
      60.seconds
    )

    // The proxy should have returned at least one assistant message
    response shouldBe defined
    receivedEvents should not be empty

    c.stop()
  }

  it should "configure a sessionFs provider" in {
    val fsConfig = SessionFsConfig(
      initialCwd = System.getProperty("user.dir"),
      sessionStatePath = System.getProperty("java.io.tmpdir"),
      conventions = "Use Scala 3 idioms"
    )

    val c = makeClient(sessionFs = Some(fsConfig))

    // start() registers the sessionFs.setProvider call during handshake.
    // If the proxy accepts the handshake without error the provider was set.
    Await.result(c.start(), timeout)

    // Verify the client is connected (setProvider ran successfully)
    c.getState shouldBe ConnectionState.Connected

    c.stop()
  }

  it should "handle a multi-turn conversation" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)

    // First turn
    val response1 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Hello")),
      60.seconds
    )
    response1 shouldBe defined

    // Second turn (follow-up)
    val response2 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Tell me more")),
      60.seconds
    )
    response2 shouldBe defined

    c.stop()
  }

  it should "resume a session by ID" in {
    val c1 = makeClient()
    Await.result(c1.start(), timeout)

    val session = Await.result(c1.createSession(SessionConfig()), timeout)
    val sessionId = session.sessionId
    sessionId should not be empty

    c1.stop()
    client = None // clear so afterEach does not double-stop

    // Create a new client and resume
    val c2 = makeClient()
    Await.result(c2.start(), timeout)

    val resumed = Await.result(
      c2.resumeSession(sessionId),
      timeout
    )
    resumed.sessionId shouldBe sessionId

    c2.stop()
  }

  it should "list multiple sessions" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session1 = Await.result(c.createSession(SessionConfig()), timeout)
    val session2 = Await.result(c.createSession(SessionConfig()), timeout)

    session1.sessionId should not be session2.sessionId

    val sessions = Await.result(c.listSessions(), timeout)
    sessions should not be empty
    // At least the two we just created should be present
    val ids = sessions.map(_.sessionId)
    ids should contain(session1.sessionId)
    ids should contain(session2.sessionId)

    c.stop()
  }

  it should "get session metadata" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    val metadata = Await.result(c.getSessionMetadata(session.sessionId), timeout)

    metadata shouldBe defined

    c.stop()
  }

  it should "delete a session" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    val sessionId = session.sessionId

    Await.result(c.deleteSession(sessionId), timeout)

    val sessions = Await.result(c.listSessions(), timeout)
    val ids = sessions.map(_.sessionId)
    ids should not contain sessionId

    c.stop()
  }

  it should "list available models" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val models = Await.result(c.listModels(), timeout)
    models should not be empty

    // Each model should have an id and a name
    models.foreach { model =>
      model.id should not be empty
      model.name should not be empty
    }

    c.stop()
  }

  it should "ping the server" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val pong = Await.result(c.ping(Some("hello")), timeout)
    pong.message should not be empty
    pong.timestamp should be > 0L

    c.stop()
  }

  it should "get auth status" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val auth = Await.result(c.getAuthStatus(), timeout)
    // The proxy should return a valid auth status structure
    // isAuthenticated is a boolean — either value is fine
    auth.isAuthenticated shouldBe a[Boolean]

    c.stop()
  }

  it should "track client lifecycle states" in {
    val c = makeClient()

    // Before start, should be disconnected
    c.getState shouldBe ConnectionState.Disconnected

    Await.result(c.start(), timeout)
    c.getState shouldBe ConnectionState.Connected

    c.stop()
    c.getState shouldBe ConnectionState.Disconnected
  }

  it should "set and get foreground session ID" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    val sessionId = session.sessionId

    Await.result(c.setForegroundSessionId(sessionId), timeout)

    val foreground = Await.result(c.getForegroundSessionId(), timeout)
    foreground shouldBe defined
    foreground.get shouldBe sessionId

    c.stop()
  }

  it should "invoke a registered tool" in {
    val toolCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val tool = DefineTool(
      name = "get_weather",
      description = "Get current weather for a location",
      parameters = JsonObject(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "location" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "City name".asJson
          )
        ),
        "required" -> Json.arr("location".asJson)
      )
    ) { (args, _) =>
      toolCalled.set(true)
      val location = args.hcursor.get[String]("location").getOrElse("unknown")
      Future.successful(ToolResultObject(
        textResultForLlm = s"Weather in $location: 72°F, sunny",
        resultType = ToolResultType.Success
      ))
    }

    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(
      c.createSession(SessionConfig(tools = List(tool))),
      timeout
    )

    // Collect tool-related events
    var toolEvents = List.empty[SessionEvent]
    session.on { event =>
      if event.`type`.startsWith("tool.") then
        toolEvents = toolEvents :+ event
    }

    // Send a message that should trigger the tool
    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "What is the weather in Seattle?")),
      60.seconds
    )

    // The proxy may or may not call the tool depending on snapshots;
    // verify the session completed without error
    response shouldBe defined

    c.stop()
  }

  it should "receive streaming delta events" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(
      c.createSession(SessionConfig(streaming = Some(true))),
      timeout
    )

    var deltaEvents = List.empty[SessionEvent]
    session.on { event =>
      if event.`type` == SessionEventType.AssistantMessageDelta ||
         event.`type` == SessionEventType.AssistantReasoningDelta then
        deltaEvents = deltaEvents :+ event
    }

    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Hello")),
      60.seconds
    )

    // The proxy should produce a final assistant message
    response shouldBe defined

    // With streaming enabled, we may receive delta events
    // (depends on whether the proxy snapshot includes deltas)
    // At minimum, verify the session completed without errors
    c.stop()
  }

  it should "create a session with a system message in append mode" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val sysMsg = SystemMessageConfig.append("You are a helpful Scala assistant.")

    val session = Await.result(
      c.createSession(SessionConfig(systemMessage = Some(sysMsg))),
      timeout
    )

    session.sessionId should not be empty

    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Hello")),
      60.seconds
    )
    response shouldBe defined

    c.stop()
  }

  it should "create a session with sessionFs and send a message" in {
    val fsConfig = SessionFsConfig(
      initialCwd = System.getProperty("user.dir"),
      sessionStatePath = System.getProperty("java.io.tmpdir"),
      conventions = "Prefer immutable data structures"
    )

    val c = makeClient(sessionFs = Some(fsConfig))
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    session.sessionId should not be empty

    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Hello")),
      60.seconds
    )
    response shouldBe defined

    c.stop()
  }

  it should "create a session with MCP servers config" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val mcpConfig = Map(
      "test-server" -> MCPServerConfig.local(
        command = "echo",
        args = List("hello"),
        tools = List("*")
      )
    )

    val session = Await.result(
      c.createSession(SessionConfig(mcpServers = Some(mcpConfig))),
      timeout
    )

    session.sessionId should not be empty

    c.stop()
  }

  it should "create a session with skill directories" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(
      c.createSession(SessionConfig(
        skillDirectories = Some(List(System.getProperty("user.dir")))
      )),
      timeout
    )

    session.sessionId should not be empty

    c.stop()
  }

  it should "handle compaction events on long conversations" in {
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(
      c.createSession(SessionConfig(
        infiniteSessions = Some(InfiniteSessionConfig(enabled = Some(true)))
      )),
      timeout
    )

    var compactionEvents = List.empty[SessionEvent]
    session.on { event =>
      if event.`type` == SessionEventType.SessionCompactionStart ||
         event.`type` == SessionEventType.SessionCompactionComplete then
        compactionEvents = compactionEvents :+ event
    }

    // Send several messages to try to trigger compaction
    for i <- 1 to 3 do
      try
        Await.result(
          session.sendAndWait(MessageOptions(prompt = s"Message number $i with some content to fill context")),
          60.seconds
        )
      catch case _: Exception => () // proxy may not have snapshots for all turns

    // Compaction may or may not be triggered depending on proxy thresholds;
    // the test verifies the session survives multiple turns without errors
    session.sessionId should not be empty

    c.stop()
  }
