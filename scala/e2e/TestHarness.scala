// Copyright (c) Microsoft Corporation. All rights reserved.

package com.github.copilot.e2e

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Path, Paths}
import scala.util.matching.Regex

/**
 * Manages the replaying CAPI proxy for E2E tests.
 *
 * Spawns the shared test harness server (`test/harness/server.ts`) and
 * exposes the proxy URL so that [[CopilotClient]] can connect to it via `cliUrl`.
 */
object TestHarness:

  @volatile private var process: Option[Process] = None
  @volatile private var _proxyUrl: Option[String] = None

  /** The full proxy URL (e.g. `http://localhost:12345`), available after [[start]]. */
  def proxyUrl: String =
    _proxyUrl.getOrElse(throw new IllegalStateException("TestHarness not started"))

  /**
   * Extracts the `host:port` portion from the proxy URL.
   *
   * The [[CopilotClient]] `cliUrl` option expects a value like `localhost:12345`
   * (without the `http://` scheme), so this helper strips the scheme prefix.
   */
  def cliUrl: String =
    proxyUrl.replaceFirst("^https?://", "")

  /** `true` when the proxy process is running and the URL has been parsed. */
  def isRunning: Boolean = _proxyUrl.isDefined && process.exists(_.isAlive)

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  /**
   * Starts the replaying proxy server and blocks until the `Listening:` line is emitted.
   *
   * The server script lives at `{repoRoot}/test/harness/server.ts`.  From the Scala
   * `e2e/` directory, the relative path is `../../test/harness/server.ts`.
   *
   * @throws RuntimeException if the proxy cannot be started or the URL cannot be parsed.
   */
  def start(): Unit = synchronized {
    if _proxyUrl.isDefined then return

    val serverPath = resolveServerPath()
    val serverDir = serverPath.getParent.toFile

    val isWindows = System.getProperty("os.name", "").toLowerCase.contains("win")

    val builder = if isWindows then
      // On Windows use cmd /c so that npx is found via PATH / shell resolution.
      new ProcessBuilder("cmd", "/c", "npx", "tsx", serverPath.toString)
    else
      new ProcessBuilder("npx", "tsx", serverPath.toString)

    builder.directory(serverDir)
    builder.redirectErrorStream(false)
    // Inherit stderr so proxy debug output goes to the test console.
    builder.redirectError(ProcessBuilder.Redirect.INHERIT)

    // Forward COPILOT_API_URL if set in the environment — some CI setups need it.
    val env = builder.environment()
    Option(System.getenv("COPILOT_API_URL")).foreach(v => env.put("COPILOT_API_URL", v))

    val proc = builder.start()
    process = Some(proc)

    // Read stdout until we see the "Listening: http://..." line.
    val reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val line = reader.readLine()
    if line == null then
      proc.destroyForcibly()
      throw new RuntimeException(
        "Proxy process exited before emitting a Listening URL. " +
        "Make sure Node.js ≥ 18, npx, and tsx are on PATH and `npm ci` was run in test/harness."
      )

    val ListeningPattern: Regex = """Listening:\s+(http://\S+)""".r
    line match
      case ListeningPattern(url) =>
        _proxyUrl = Some(url)
      case _ =>
        proc.destroyForcibly()
        throw new RuntimeException(s"Unexpected proxy output (expected 'Listening: http://...'): $line")
  }

  /**
   * Stops the proxy server.
   *
   * Sends a `/stop` request to the proxy for a graceful shutdown, then destroys
   * the process tree as a fallback.
   */
  def stop(): Unit = synchronized {
    // Best-effort graceful shutdown via the /stop endpoint.
    _proxyUrl.foreach { url =>
      try
        val conn = java.net.URI(s"$url/stop?skipWritingCache=true").toURL.openConnection()
          .asInstanceOf[java.net.HttpURLConnection]
        conn.setRequestMethod("POST")
        conn.setConnectTimeout(2000)
        conn.setReadTimeout(2000)
        conn.getResponseCode // trigger the request
        conn.disconnect()
      catch case _: Exception => () // ignore
    }

    process.foreach { p =>
      p.destroyForcibly()
      p.waitFor()
    }

    process = None
    _proxyUrl = None
  }

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /** Resolves the absolute path to `test/harness/server.ts` relative to the Scala e2e dir. */
  private def resolveServerPath(): Path =
    // This file lives at  scala/e2e/TestHarness.scala
    // The server lives at test/harness/server.ts
    // From scala/e2e/ → ../../test/harness/server.ts
    val e2eDir = Paths.get(System.getProperty("user.dir"))
    // When sbt runs tests the cwd is the project root (scala/), so we go up once.
    // Try both relative resolutions and pick whichever exists.
    val candidates = Seq(
      e2eDir.resolve("../test/harness/server.ts").normalize(),  // cwd = scala/
      e2eDir.resolve("../../test/harness/server.ts").normalize(), // cwd = scala/e2e/
      e2eDir.resolve("test/harness/server.ts").normalize(),      // cwd = repo root
    )
    candidates.find(_.toFile.exists()).getOrElse(
      throw new RuntimeException(
        s"Cannot find test/harness/server.ts. Tried: ${candidates.mkString(", ")}. cwd=${e2eDir}"
      )
    )
