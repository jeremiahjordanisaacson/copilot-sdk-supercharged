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
  @volatile private var _connectProxyUrl: Option[String] = None
  @volatile private var _caFilePath: Option[String] = None

  /** The full proxy URL (e.g. `http://localhost:12345`), available after [[start]]. */
  def proxyUrl: String =
   _proxyUrl.getOrElse(throw new IllegalStateException("TestHarness not started"))

  /** `true` when the proxy process is running and the URL has been parsed. */
  def isRunning: Boolean = _proxyUrl.isDefined && process.exists(_.isAlive)

  /** Resolve the CLI executable path. */
  def cliPath: String =
   val envPath = Option(System.getenv("COPILOT_CLI_PATH")).filter(_.nonEmpty)
   envPath.filter(p => java.io.File(p).exists()).getOrElse {
     val repoRoot = resolveRepoRoot()
     val nodeCliPath = repoRoot.resolve("nodejs/node_modules/@github/copilot/index.js")
     if nodeCliPath.toFile.exists() then nodeCliPath.toString
     else "copilot"
   }

  /** Environment variables that route the CLI through the proxy. */
  def testEnv(workDir: String): Map[String, String] =
   // Start with parent environment so PATH and other system vars are inherited
   val base = scala.collection.mutable.Map[String, String]()
   base ++= scala.jdk.CollectionConverters.MapHasAsScala(System.getenv()).asScala
   base ++= Seq(
     "COPILOT_API_URL" -> proxyUrl,
     "COPILOT_HOME" -> workDir,
     "XDG_CONFIG_HOME" -> workDir,
     "XDG_STATE_HOME" -> workDir,
     "GH_TOKEN" -> Option(System.getenv("GH_TOKEN")).getOrElse("fake-test-token"),
     "GITHUB_TOKEN" -> Option(System.getenv("GITHUB_TOKEN")).getOrElse("fake-test-token"),
   )
   _connectProxyUrl.foreach { url =>
     base ++= Seq(
       "HTTP_PROXY" -> url, "HTTPS_PROXY" -> url,
       "http_proxy" -> url, "https_proxy" -> url,
       "NO_PROXY" -> "127.0.0.1,localhost,::1",
       "no_proxy" -> "127.0.0.1,localhost,::1",
     )
   }
   _caFilePath.foreach { path =>
     base ++= Seq(
       "NODE_EXTRA_CA_CERTS" -> path,
       "SSL_CERT_FILE" -> path,
     )
   }
   base.toMap

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

    val ListeningPattern: Regex = """Listening:\s+(http://\S+).*""".r
    line match
      case ListeningPattern(url) =>
        _proxyUrl = Some(url)
      case _ =>
        proc.destroyForcibly()
        throw new RuntimeException(s"Unexpected proxy output (expected 'Listening: http://...'): $line")

    // Parse connect proxy metadata JSON from the same line
    val MetadataPattern: Regex = """(\{.*\})\s*$""".r
    MetadataPattern.findFirstMatchIn(line).foreach { m =>
      try
        val jsonStr = m.group(1)
        import io.circe.parser.parse
        parse(jsonStr).foreach { json =>
          _connectProxyUrl = json.hcursor.get[String]("connectProxyUrl").toOption
          _caFilePath = json.hcursor.get[String]("caFilePath").toOption
        }
      catch case _: Exception => () // non-fatal
    }
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
    _connectProxyUrl = None
    _caFilePath = None
  }

  /** The directory containing test snapshot YAML files. */
  def snapshotsDir: String =
    resolveRepoRoot().resolve("test/snapshots").toString

  /**
   * Configures the proxy to use a specific snapshot file.
   *
   * Must be called before each test to tell the proxy which canned
   * exchanges to replay.
   */
  def configure(filePath: String, workDir: String): Unit =
    val url = proxyUrl
    val conn = java.net.URI(s"$url/config").toURL.openConnection()
      .asInstanceOf[java.net.HttpURLConnection]
    conn.setRequestMethod("POST")
    conn.setDoOutput(true)
    conn.setRequestProperty("Content-Type", "application/json")
    conn.setConnectTimeout(5000)
    conn.setReadTimeout(5000)
    val body = s"""{"filePath":"$filePath","workDir":"$workDir"}"""
    conn.getOutputStream.write(body.getBytes("UTF-8"))
    conn.getOutputStream.flush()
    val code = conn.getResponseCode
    conn.disconnect()
    if code != 200 then
      throw new RuntimeException(s"Proxy /config returned $code")

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /** Resolves the repo root by walking up from cwd. */
  private def resolveRepoRoot(): Path =
    val cwd = Paths.get(System.getProperty("user.dir"))
    val candidates = Seq(
      cwd.resolve("..").normalize(),    // cwd = scala/
      cwd.resolve("../..").normalize(), // cwd = scala/e2e/
      cwd,                              // cwd = repo root
    )
    candidates.find(p => p.resolve("test/harness/server.ts").toFile.exists()).getOrElse(
      throw new RuntimeException(
        s"Cannot find repo root (test/harness/server.ts). Tried: ${candidates.mkString(", ")}. cwd=$cwd"
      )
    )

  /** Resolves the absolute path to `test/harness/server.ts`. */
  private def resolveServerPath(): Path =
    resolveRepoRoot().resolve("test/harness/server.ts")
