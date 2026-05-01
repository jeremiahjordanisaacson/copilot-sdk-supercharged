// Copyright (c) Microsoft Corporation. All rights reserved.

package com.github.copilot.e2e

import com.github.copilot.*
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{Await, ExecutionContext}
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
