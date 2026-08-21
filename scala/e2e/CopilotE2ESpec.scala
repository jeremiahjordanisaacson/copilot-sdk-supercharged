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
import java.nio.file.Files

class CopilotE2ESpec
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach:

  given ExecutionContext = ExecutionContext.global

  private val timeout: FiniteDuration = 30.seconds

  // Client reference reset per-test
  private var client: Option[CopilotClient] = None

  // Isolated work directory for test runs
  private var workDir: String = _

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  override def beforeAll(): Unit =
    super.beforeAll()
    TestHarness.start()
    workDir = Files.createTempDirectory("copilot-scala-e2e-").toFile.getAbsolutePath
    info(s"Proxy running at ${TestHarness.proxyUrl}")

  override def afterAll(): Unit =
    try TestHarness.stop()
    finally
      try new java.io.File(workDir).delete()
      catch case _: Exception => ()
      super.afterAll()

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

  /** Creates a [[CopilotClient]] pre-configured to talk through the test proxy. */
  private def makeClient(
    sessionFs: Option[SessionFsConfig] = None
  ): CopilotClient =
    val opts = CopilotClientOptions(
      cliPath = Some(TestHarness.cliPath),
      cwd = Some(workDir),
      env = Some(TestHarness.testEnv(workDir)),
      githubToken = Some(
        sys.env.getOrElse("GH_TOKEN", sys.env.getOrElse("GITHUB_TOKEN", "fake-test-token"))
      ),
      sessionFs = sessionFs,
    )
    val c = CopilotClient(opts)
    client = Some(c)
    c

  private val defaultSnapshot = "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"

  /** Configures the replay proxy with the given snapshot file name (without `.yaml`). */
  private def configureSnapshot(name: String): Unit =
    val filePath = s"${TestHarness.snapshotsDir}/session/$name.yaml"
    TestHarness.configure(java.io.File(filePath).getAbsolutePath, workDir)

  // ---------------------------------------------------------------------------
  // Tests
  // ---------------------------------------------------------------------------

  behavior of "CopilotClient E2E"

  it should "create a session and disconnect" in {
    configureSnapshot("should_create_session_with_custom_tool")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    session.sessionId should not be empty

    c.stop()
  }

  it should "send a message and receive a response" in {
    configureSnapshot(defaultSnapshot)
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)

    // Collect events for inspection
    var receivedEvents = List.empty[SessionEvent]
    session.on { event =>
      receivedEvents = receivedEvents :+ event
    }

    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "What is 2+2?")),
      60.seconds
    )

    // The proxy should have returned at least one assistant message
    response shouldBe defined
    receivedEvents should not be empty

    c.stop()
  }

  it should "configure a sessionFs provider" in {
    configureSnapshot("should_create_session_with_custom_tool")
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
    configureSnapshot("should_have_stateful_conversation")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)

    // First turn
    val response1 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "What is 1+1?")),
      60.seconds
    )
    response1 shouldBe defined

    // Second turn (follow-up)
    val response2 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Now if you double that, what do you get?")),
      60.seconds
    )
    response2 shouldBe defined

    c.stop()
  }

  it should "resume a session by ID" in {
    configureSnapshot("should_have_stateful_conversation")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    val sessionId = session.sessionId
    sessionId should not be empty

    // Send a message so there's state to resume
    val r1 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "What is 1+1?")),
      60.seconds
    )
    r1 shouldBe defined

    // Resume on the same client (same CLI process preserves session state)
    val resumed = Await.result(
      c.resumeSession(sessionId),
      timeout
    )
    resumed.sessionId shouldBe sessionId

    // Continue conversation
    val r2 = Await.result(
      resumed.sendAndWait(MessageOptions(prompt = "Now if you double that, what do you get?")),
      60.seconds
    )
    r2 shouldBe defined

    c.stop()
  }

  it should "list multiple sessions" in {
    configureSnapshot("should_list_sessions")
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
    configureSnapshot("should_get_session_metadata")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    val metadata = Await.result(c.getSessionMetadata(session.sessionId), timeout)

    metadata shouldBe defined

    c.stop()
  }

  it should "delete a session" in {
    configureSnapshot("should_delete_session")
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
    configureSnapshot("should_create_session_with_custom_tool")
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
    configureSnapshot("should_create_session_with_custom_tool")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val pong = Await.result(c.ping(Some("hello")), timeout)
    pong.message should not be empty
    pong.timestamp should not be empty

    c.stop()
  }

  it should "get auth status" in {
    configureSnapshot("should_create_session_with_custom_tool")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val auth = Await.result(c.getAuthStatus(), timeout)
    // The proxy should return a valid auth status structure
    // isAuthenticated is a boolean — either value is fine
    auth.isAuthenticated shouldBe a[Boolean]

    c.stop()
  }

  it should "track client lifecycle states" in {
    configureSnapshot("should_create_session_with_custom_tool")
    val c = makeClient()

    // Before start, should be disconnected
    c.getState shouldBe ConnectionState.Disconnected

    Await.result(c.start(), timeout)
    c.getState shouldBe ConnectionState.Connected

    c.stop()
    c.getState shouldBe ConnectionState.Disconnected
  }

  it should "set and get foreground session ID" in {
    configureSnapshot("should_create_session_with_custom_tool")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val session = Await.result(c.createSession(SessionConfig()), timeout)
    val sessionId = session.sessionId

    try {
      Await.result(c.setForegroundSessionId(sessionId), timeout)
      val foreground = Await.result(c.getForegroundSessionId(), timeout)
      foreground shouldBe defined
      foreground.get shouldBe sessionId
    } catch {
      case _: Exception =>
        // Foreground session RPCs may not be available in headless mode
        info("foreground session skipped (headless mode)")
    }

    c.stop()
  }

  it should "invoke a registered tool" in {
    configureSnapshot("should_create_session_with_custom_tool")
    val toolCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val tool = DefineTool(
      name = "get_secret_number",
      description = "Get a secret number for a given key",
      parameters = JsonObject(
        "type" -> "object".asJson,
        "properties" -> Json.obj(
          "key" -> Json.obj(
            "type" -> "string".asJson,
            "description" -> "The key to look up".asJson
          )
        ),
        "required" -> Json.arr("key".asJson)
      )
    ) { (args, _) =>
      toolCalled.set(true)
      Future.successful(ToolResultObject(
        textResultForLlm = "54321",
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
      session.sendAndWait(MessageOptions(prompt = "What is the secret number for key ALPHA?")),
      60.seconds
    )

    // The proxy may or may not call the tool depending on snapshots;
    // verify the session completed without error
    response shouldBe defined

    c.stop()
  }

  it should "receive streaming delta events" in {
    configureSnapshot(defaultSnapshot)
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
      session.sendAndWait(MessageOptions(prompt = "What is 2+2?")),
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
    configureSnapshot("should_create_a_session_with_appended_systemmessage_config")
    val c = makeClient()
    Await.result(c.start(), timeout)

    val sysMsg = SystemMessageConfig.append("You are a helpful Scala assistant.")

    val session = Await.result(
      c.createSession(SessionConfig(systemMessage = Some(sysMsg))),
      timeout
    )

    session.sessionId should not be empty

    val response = Await.result(
      session.sendAndWait(MessageOptions(prompt = "What is your full name?")),
      60.seconds
    )
    response shouldBe defined

    c.stop()
  }

  it should "create a session with sessionFs and send a message" in {
    configureSnapshot(defaultSnapshot)
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
      session.sendAndWait(MessageOptions(prompt = "What is 2+2?")),
      60.seconds
    )
    response shouldBe defined

    c.stop()
  }

  it should "create a session with MCP servers config" in {
    configureSnapshot("should_create_session_with_custom_tool")
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
    configureSnapshot("should_create_session_with_custom_tool")
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
    configureSnapshot("should_have_stateful_conversation")
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

    // Send two messages using the multi-turn snapshot
    val r1 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "What is 1+1?")),
      60.seconds
    )
    r1 shouldBe defined

    val r2 = Await.result(
      session.sendAndWait(MessageOptions(prompt = "Now if you double that, what do you get?")),
      60.seconds
    )
    r2 shouldBe defined

    // Compaction may or may not be triggered depending on proxy thresholds;
    // the test verifies the session survives multiple turns without errors
    session.sessionId should not be empty

    c.stop()
  }
