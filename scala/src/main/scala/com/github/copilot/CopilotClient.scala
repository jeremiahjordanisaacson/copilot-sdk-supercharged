// Copyright (c) Microsoft Corporation. All rights reserved.

package com.github.copilot

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

import java.io.File
import java.net.Socket
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/**
 * Main client for interacting with the Copilot CLI.
 *
 * The CopilotClient manages the connection to the Copilot CLI server and provides
 * methods to create and manage conversation sessions. It can either spawn a CLI
 * server process or connect to an existing server.
 *
 * == Usage ==
 * {{{
 * import com.github.copilot.*
 * import scala.concurrent.Await
 * import scala.concurrent.duration.*
 * given ExecutionContext = ExecutionContext.global
 *
 * val client = CopilotClient()
 * Await.result(client.start(), 30.seconds)
 *
 * val session = Await.result(client.createSession(SessionConfig()), 30.seconds)
 * val response = Await.result(
 *   session.sendAndWait(MessageOptions(prompt = "Hello!")),
 *   60.seconds
 * )
 * println(response.map(_.data))
 *
 * Await.result(session.destroy(), 10.seconds)
 * client.stop()
 * }}}
 *
 * @param options Configuration options for the client
 */
class CopilotClient(options: CopilotClientOptions = CopilotClientOptions())(using ec: ExecutionContext):

  // -------------------------------------------------------------------------
  // State
  // -------------------------------------------------------------------------

  private val stateRef = new AtomicReference[ConnectionState](ConnectionState.Disconnected)
  private var cliProcess: Option[Process] = None
  private var rpcClient: Option[JsonRpcClient] = None
  private var tcpSocket: Option[Socket] = None
  private val sessions = new ConcurrentHashMap[String, CopilotSession]()
  private val lifecycleHandlers = java.util.Collections.newSetFromMap(
    new ConcurrentHashMap[SessionLifecycleHandler, java.lang.Boolean]()
  )
  private val typedLifecycleHandlers = new ConcurrentHashMap[String, mutable.Set[SessionLifecycleHandler]]()
  @volatile private var modelsCache: Option[List[ModelInfo]] = None

  /** A session's GitHub token provider registration. */
  private final class GitHubTokenRegistration(val provider: GitHubTokenProvider, @volatile var sessionId: Option[String]):
    @volatile var committed: Boolean = false

  /** Session GitHub token providers keyed by opaque registration ID (served via `gitHubToken.getToken`). */
  private val gitHubTokenProviders = new ConcurrentHashMap[String, GitHubTokenRegistration]()

  private val isExternalServer: Boolean = options.cliUrl.isDefined
  private val resolvedCliPath: String = options.cliPath.getOrElse("copilot")
  private val resolvedUseLoggedInUser: Boolean =
    options.useLoggedInUser.getOrElse(!options.githubToken.isDefined)

  /** Custom transport for outbound Copilot HTTP requests, if configured. */
  private val resolvedRequestHandler: Option[CopilotRequestHandler] = options.requestHandler
  /** Bring-your-own-key bearer token provider, if configured. */
  private val resolvedBearerTokenProvider: Option[BearerTokenProvider] = options.bearerTokenProvider
  /** Client mode; the legacy "copilot-cli" value is an alias of "coding-agent". */
  private val resolvedMode: String = options.mode match
    case ClientMode.CodingAgent | ClientMode.CopilotCli => ClientMode.CodingAgent
    case ClientMode.Empty => ClientMode.Empty
    case other => throw new IllegalArgumentException(s"mode must be 'coding-agent' or 'empty', got '$other'")

  // Validate options
  if options.cliUrl.isDefined && options.cliPath.isDefined then
    throw new IllegalArgumentException("cliUrl is mutually exclusive with cliPath")
  if options.cliUrl.isDefined && (options.githubToken.isDefined || options.useLoggedInUser.isDefined) then
    throw new IllegalArgumentException(
      "githubToken and useLoggedInUser cannot be used with cliUrl (external server manages its own auth)"
    )

  // Parse external server host/port if provided
  private val (externalHost, externalPort): (String, Int) = options.cliUrl match
    case Some(url) => parseCliUrl(url)
    case None      => ("localhost", 0)

  // -------------------------------------------------------------------------
  // Public API
  // -------------------------------------------------------------------------

  /** Gets the current connection state. */
  def getState: ConnectionState = stateRef.get()

  /**
   * Starts the CLI server and establishes a connection.
   *
   * If connecting to an external server (via cliUrl), only establishes the connection.
   * Otherwise, spawns the CLI server process and connects.
   *
   * This method is called automatically when creating a session if autoStart is true.
   */
  def start(): Future[Unit] = Future:
    if stateRef.get() == ConnectionState.Connected then return Future.successful(())

    stateRef.set(ConnectionState.Connecting)
    try
      if !isExternalServer then
        startCliServer()

      connectToServer()

      // Set up connection handlers
      attachConnectionHandlers()

      // Declare the integrating application's identity (if configured) before the ping handshake.
      sendConnectHandshake(rpcClient.get)

      // Verify protocol version compatibility. Call rpcClient directly rather than the public
      // ping(), which routes through ensureConnected() and would re-enter start() (state is still
      // Connecting here, not Connected), deadlocking until the Await times out.
      val pingRaw = concurrent.Await.result(
        rpcClient.get.sendRequest("ping", Json.obj("message" -> Option.empty[String].asJson).dropNullValues),
        scala.concurrent.duration.Duration(10, "s")
      )
      val pingResult = pingRaw.as[PingResponse] match
        case Right(r) => r
        case Left(err) => throw new RuntimeException(s"Failed to decode ping response: $err")
      verifyProtocolVersion(pingResult)

      // Register network callbacks (proxy resolution and proxy authentication) if configured
      options.network.filter(_.hasCallbacks).foreach { network =>
        concurrent.Await.result(
          rpcClient.get.sendRequest("network.setProvider", Json.obj(
            "scope" -> network.scope.asJson,
            "resolveProxy" -> Option.when(network.resolveProxy.isDefined)(Json.obj()).asJson,
            "authenticateNegotiate" -> Option.when(network.authenticateNegotiate.isDefined)(Json.obj()).asJson,
            "authenticateBasic" -> Option.when(network.authenticateBasic.isDefined)(Json.obj()).asJson
          ).dropNullValues),
          scala.concurrent.duration.Duration(10, "s")
        )
      }

      // Register session filesystem provider if configured
      options.sessionFs.foreach { fs =>
        concurrent.Await.result(
          rpcClient.get.sendRequest("sessionFs.setProvider", Json.obj(
            "initialCwd" -> fs.initialCwd.asJson,
            "sessionStatePath" -> fs.sessionStatePath.asJson,
            "conventions" -> fs.conventions.asJson
          )),
          scala.concurrent.duration.Duration(10, "s")
        )
      }

      stateRef.set(ConnectionState.Connected)
    catch
      case ex: Exception =>
        stateRef.set(ConnectionState.Error)
        throw ex

  /**
   * Stops the CLI server and closes all active sessions.
   *
   * @return A list of errors encountered during cleanup (empty on full success)
   */
  def stop(): List[Throwable] =
    val errors = mutable.ListBuffer[Throwable]()

    // Destroy all active sessions
    sessions.values().asScala.foreach { session =>
      try
        concurrent.Await.result(session.destroy(), scala.concurrent.duration.Duration(5, "s"))
      catch
        case ex: Exception =>
          errors += new RuntimeException(
            s"Failed to destroy session ${session.sessionId}: ${ex.getMessage}", ex
          )
    }
    sessions.clear()
    gitHubTokenProviders.clear()

    // Stop RPC client
    rpcClient.foreach { client =>
      try client.stop()
      catch case ex: Exception =>
        errors += new RuntimeException(s"Failed to stop RPC client: ${ex.getMessage}", ex)
    }
    rpcClient = None

    // Close TCP socket
    tcpSocket.foreach { socket =>
      try socket.close()
      catch case ex: Exception =>
        errors += new RuntimeException(s"Failed to close socket: ${ex.getMessage}", ex)
    }
    tcpSocket = None

    // Kill CLI process (only if we spawned it)
    if !isExternalServer then
      cliProcess.foreach { process =>
        try process.destroy()
        catch case ex: Exception =>
          errors += new RuntimeException(s"Failed to kill CLI process: ${ex.getMessage}", ex)
      }
    cliProcess = None

    modelsCache = None
    stateRef.set(ConnectionState.Disconnected)
    errors.toList

  /**
   * Forcefully stops the client without graceful session cleanup.
   */
  def forceStop(): Unit =
    sessions.clear()
    gitHubTokenProviders.clear()
    rpcClient.foreach(c => Try(c.stop()))
    rpcClient = None
    tcpSocket.foreach(s => Try(s.close()))
    tcpSocket = None
    if !isExternalServer then
      cliProcess.foreach(p => Try(p.destroyForcibly()))
    cliProcess = None
    modelsCache = None
    stateRef.set(ConnectionState.Disconnected)

  /**
   * Creates a new conversation session.
   *
   * @param config Session configuration
   * @return A Future that resolves with the created session
   */
  def createSession(config: SessionConfig = SessionConfig()): Future[CopilotSession] =
    checkGitHubTokenOptions(config.gitHubToken, config.gitHubTokenProvider)
      .flatMap(_ => checkSessionOptions(config.availableTools, config.memory))
      .flatMap(_ => ensureConnected()).flatMap { client =>
      val registrationId = registerGitHubTokenProvider(config.gitHubTokenProvider, config.sessionId)
      val toolDefs = config.tools.map(t => Json.obj(
        "name" -> t.name.asJson,
        "description" -> t.description.asJson,
        "parameters" -> t.parameters.map(Json.fromJsonObject).asJson,
        "defer" -> t.defer.asJson,
        "idempotent" -> t.idempotent.asJson
      ).dropNullValues)

      val hookTypes: Option[List[String]] = config.hooks.map { h =>
        List(
          Option.when(h.onPreToolUse.isDefined)("preToolUse"),
          Option.when(h.onPostToolUse.isDefined)("postToolUse"),
          Option.when(h.onPreMcpToolCall.isDefined)("preMcpToolCall"),
          Option.when(h.onUserPromptSubmitted.isDefined)("userPromptSubmitted"),
          Option.when(h.onUserPromptTransformed.isDefined)("userPromptTransformed"),
          Option.when(h.onSessionStart.isDefined)("sessionStart"),
          Option.when(h.onSessionEnd.isDefined)("sessionEnd"),
          Option.when(h.onErrorOccurred.isDefined)("errorOccurred"),
          Option.when(h.onSubagentStart.isDefined)("subagentStart"),
          Option.when(h.onSubagentStop.isDefined)("subagentStop")
        ).flatten
      }

      val params = Json.obj(
        "model" -> config.model.asJson,
        "sessionId" -> config.sessionId.asJson,
        "reasoningEffort" -> config.reasoningEffort.asJson,
        "tools" -> (if toolDefs.nonEmpty then toolDefs.asJson else Json.Null),
        "systemMessage" -> config.systemMessage.asJson,
        "availableTools" -> config.availableTools.asJson,
        "excludedTools" -> config.excludedTools.asJson,
        "provider" -> config.provider.asJson,
        "requestPermission" -> config.onPermissionRequest.isDefined.asJson,
        "requestUserInput" -> config.onUserInputRequest.isDefined.asJson,
        "hooks" -> config.hooks.exists(_.hasAny).asJson,
        "hookTypes" -> hookTypes.asJson,
        "workingDirectory" -> config.workingDirectory.asJson,
        "streaming" -> config.streaming.asJson,
        "mcpServers" -> config.mcpServers.map(m => Json.fromJsonObject(
          JsonObject.fromIterable(m.map((k, v) => k -> Json.fromJsonObject(v)))
        )).asJson,
        "customAgents" -> config.customAgents.asJson,
        "configDir" -> config.configDir.asJson,
        "skillDirectories" -> config.skillDirectories.asJson,
        "disabledSkills" -> config.disabledSkills.asJson,
        "infiniteSessions" -> config.infiniteSessions.asJson,
        "enableConfigDiscovery" -> config.enableConfigDiscovery.asJson,
        "gitHubToken" -> config.gitHubToken.asJson,
        // Wire idleTimeout, requestHeaders, modelCapabilities, elicitation, Command, imageGeneration
        "requestHeaders" -> config.requestHeaders.asJson,
        "modelCapabilities" -> config.modelCapabilities.asJson,
        // Additional session-config passthroughs.
        "enableCitations" -> config.enableCitations.asJson,
        "excludedBuiltinAgents" -> config.excludedBuiltinAgents.asJson,
        "sessionLimits" -> config.sessionLimits.map { (sl: SessionLimitsConfig) =>
          Json.obj("maxAiCredits" -> sl.maxAiCredits.asJson).dropNullValues
        }.asJson,
        "memory" -> config.memory.map { (m: MemoryConfiguration) =>
          Json.obj("enabled" -> m.enabled.asJson)
        }.asJson,
        "otlpProtocol" -> config.otlpProtocol.asJson,
        "expAssignments" -> config.expAssignments.asJson,
        "rewindEnabled" -> config.rewindEnabled.asJson,
        "additionalDirectories" -> config.additionalDirectories.asJson,
        "disabledMcpServers" -> config.disabledMcpServers.asJson,
        "githubMcpToolConfig" -> config.githubMcpToolConfig.asJson,
        "canvasProvider" -> config.canvasProvider.asJson,
        "customAgentsLocalOnly" -> config.customAgentsLocalOnly.asJson,
        "toolSearch" -> config.toolSearch.asJson,
        "enableExperimentalMode" -> config.experimentalMode.asJson,
        "contentExclusion" -> config.contentExclusion.asJson,
        "instructionDirectories" -> config.instructionDirectories.asJson,
        "mcpAuthHandler" -> config.onMcpAuthRequest.isDefined.asJson,
        // v2.6.0 upstream-sync session options. CAPI options go to github.capi;
        // explicit CapiConfig members win over the flat fields.
        "capi" -> capiOptions(config.autoTier, config.enableWebSocketResponses, config.capi).asJson,
        "askUserVariant" -> config.askUserVariant.asJson,
        "gitHubTokenProviderRegistrationId" -> registrationId.asJson,
        "sandboxConfig" -> config.sandboxConfig.asJson,
        "featureFlags" -> config.featureFlags.asJson,
        "allowedModels" -> config.allowedModels.asJson,
        "authClientIdMetadataUrl" -> config.authClientIdMetadataUrl.asJson,
        "name" -> config.name.asJson,
        "agentDirectories" -> config.agentDirectories.asJson,
        "hookDirectories" -> config.hookDirectories.asJson,
        "interactiveUser" -> config.interactiveUser.asJson,
        "excludedManagedSettings" -> config.excludedManagedSettings.asJson,
        "enforceManagedModelDefaults" -> config.enforceManagedModelDefaults.asJson,
        "trustWorkingDirectory" -> config.trustWorkingDirectory.asJson,
        "refreshCustomInstructions" -> config.refreshCustomInstructions.asJson,
        "imageGeneration" -> config.imageGeneration.asJson,
        "autopilot" -> config.autopilot.asJson,
        // v3.0.0 session options (SDK protocol 4)
        "providers" -> config.providers.asJson,
        "models" -> config.models.asJson,
        "defaultProvider" -> config.defaultProvider.asJson,
        "hasMemoryStore" -> Option.when(config.memory.exists(_.storeMemory.isDefined))(true).asJson,
        "requestShellAuthentication" -> Option.when(config.onShellAuthenticationRequest.isDefined)(true).asJson,
        "permissionMode" -> config.permissionMode.asJson,
      ).dropNullValues

      val payload = applyModeDefaults(groupSessionParams(injectTraceContext(params), resume = false))
      client.sendRequest("session.create", payload).map { result =>
        val sessionId = result.hcursor.get[String]("sessionId").getOrElse(
          throw new RuntimeException("session.create response missing sessionId")
        )
        val workspacePath = result.hcursor.get[String]("workspacePath").toOption
        val session = new CopilotSession(sessionId, client, workspacePath)
        session.registerTools(config.tools)
        config.onPermissionRequest.foreach(session.registerPermissionHandler)
        config.onUserInputRequest.foreach(session.registerUserInputHandler)
        config.hooks.foreach(session.registerHooks)
        config.onExitPlanMode.foreach(session.registerExitPlanModeHandler)
        config.onShellAuthenticationRequest.foreach(session.registerShellAuthenticationHandler)
        config.memory.flatMap(_.storeMemory).foreach(session.registerMemoryStore)
        commitGitHubTokenProvider(session, registrationId)
        sessions.put(sessionId, session)
        session
      }.recoverWith { case ex =>
        registrationId.foreach(id => gitHubTokenProviders.remove(id))
        Future.failed(ex)
      }
    }

  /**
   * Resumes an existing session by ID.
   *
   * @param sessionId The session ID to resume
   * @param config    Optional configuration overrides
   * @return A Future that resolves with the resumed session
   */
  def resumeSession(sessionId: String, config: ResumeSessionConfig = ResumeSessionConfig()): Future[CopilotSession] =
    checkGitHubTokenOptions(config.gitHubToken, config.gitHubTokenProvider)
      .flatMap(_ => checkSessionOptions(config.availableTools, config.memory))
      .flatMap(_ => ensureConnected()).flatMap { client =>
      val registrationId = registerGitHubTokenProvider(config.gitHubTokenProvider, Some(sessionId))
      val toolDefs = config.tools.map(t => Json.obj(
        "name" -> t.name.asJson,
        "description" -> t.description.asJson,
        "parameters" -> t.parameters.map(Json.fromJsonObject).asJson,
        "defer" -> t.defer.asJson,
        "idempotent" -> t.idempotent.asJson
      ).dropNullValues)

      val params = Json.obj(
        "sessionId" -> sessionId.asJson,
        "model" -> config.model.asJson,
        "reasoningEffort" -> config.reasoningEffort.asJson,
        "tools" -> (if toolDefs.nonEmpty then toolDefs.asJson else Json.Null),
        "systemMessage" -> config.systemMessage.asJson,
        "availableTools" -> config.availableTools.asJson,
        "excludedTools" -> config.excludedTools.asJson,
        "provider" -> config.provider.asJson,
        "requestPermission" -> config.onPermissionRequest.isDefined.asJson,
        "requestUserInput" -> config.onUserInputRequest.isDefined.asJson,
        "hooks" -> config.hooks.exists(_.hasAny).asJson,
        "workingDirectory" -> config.workingDirectory.asJson,
        "configDir" -> config.configDir.asJson,
        "streaming" -> config.streaming.asJson,
        "mcpServers" -> config.mcpServers.map(m => Json.fromJsonObject(
          JsonObject.fromIterable(m.map((k, v) => k -> Json.fromJsonObject(v)))
        )).asJson,
        "customAgents" -> config.customAgents.asJson,
        "skillDirectories" -> config.skillDirectories.asJson,
        "disabledSkills" -> config.disabledSkills.asJson,
        "infiniteSessions" -> config.infiniteSessions.asJson,
        "enableConfigDiscovery" -> config.enableConfigDiscovery.asJson,
        "gitHubToken" -> config.gitHubToken.asJson,
        "disableResume" -> config.disableResume.asJson,
        "instructionDirectories" -> config.instructionDirectories.asJson,
        // v2.6.0 upstream-sync session options.
        "capi" -> capiOptions(config.autoTier, config.enableWebSocketResponses, config.capi).asJson,
        "askUserVariant" -> config.askUserVariant.asJson,
        "gitHubTokenProviderRegistrationId" -> registrationId.asJson,
        "sandboxConfig" -> config.sandboxConfig.asJson,
        "featureFlags" -> config.featureFlags.asJson,
        "allowedModels" -> config.allowedModels.asJson,
        "authClientIdMetadataUrl" -> config.authClientIdMetadataUrl.asJson,
        "agentDirectories" -> config.agentDirectories.asJson,
        "hookDirectories" -> config.hookDirectories.asJson,
        "interactiveUser" -> config.interactiveUser.asJson,
        "excludedManagedSettings" -> config.excludedManagedSettings.asJson,
        "enforceManagedModelDefaults" -> config.enforceManagedModelDefaults.asJson,
        "allowTranscriptRecovery" -> config.allowTranscriptRecovery.asJson,
        "imageGeneration" -> config.imageGeneration.asJson,
        "autopilot" -> config.autopilot.asJson,
        // v3.0.0 session options (SDK protocol 4)
        "suppressResumeEvent" -> config.suppressResumeEvent.asJson,
        "continuePendingWork" -> config.continuePendingWork.asJson,
        "providers" -> config.providers.asJson,
        "models" -> config.models.asJson,
        "defaultProvider" -> config.defaultProvider.asJson,
        "memory" -> config.memory.map { (m: MemoryConfiguration) =>
          Json.obj("enabled" -> m.enabled.asJson)
        }.asJson,
        "hasMemoryStore" -> Option.when(config.memory.exists(_.storeMemory.isDefined))(true).asJson,
        "requestShellAuthentication" -> Option.when(config.onShellAuthenticationRequest.isDefined)(true).asJson,
        "permissionMode" -> config.permissionMode.asJson,
      ).dropNullValues
      // An explicit null clears the default provider; dropNullValues above would otherwise remove it.
      val flat =
        if config.clearDefaultProvider && config.defaultProvider.isEmpty then
          params.deepMerge(Json.obj("defaultProvider" -> Json.Null))
        else params

      val payload = applyModeDefaults(groupSessionParams(injectTraceContext(flat), resume = true))
      client.sendRequest("session.resume", payload).map { result =>
        val resumedId = result.hcursor.get[String]("sessionId").getOrElse(sessionId)
        val workspacePath = result.hcursor.get[String]("workspacePath").toOption
        val session = new CopilotSession(resumedId, client, workspacePath)
        session.registerTools(config.tools)
        config.onPermissionRequest.foreach(session.registerPermissionHandler)
        config.onUserInputRequest.foreach(session.registerUserInputHandler)
        config.hooks.foreach(session.registerHooks)
        config.onExitPlanMode.foreach(session.registerExitPlanModeHandler)
        config.onShellAuthenticationRequest.foreach(session.registerShellAuthenticationHandler)
        config.memory.flatMap(_.storeMemory).foreach(session.registerMemoryStore)
        commitGitHubTokenProvider(session, registrationId)
        sessions.put(resumedId, session)
        session
      }.recoverWith { case ex =>
        registrationId.foreach(id => gitHubTokenProviders.remove(id))
        Future.failed(ex)
      }
    }

  /**
   * Sends a ping to the server.
   *
   * @param message Optional message to include
   * @return A Future with the ping response
   */
  def ping(message: Option[String] = None): Future[PingResponse] =
    ensureConnected().flatMap { client =>
      client.sendRequest("ping", Json.obj("message" -> message.asJson).dropNullValues).flatMap { result =>
        result.as[PingResponse] match
          case Right(r) => Future.successful(r)
          case Left(err) => Future.failed(new RuntimeException(s"Failed to decode ping response: $err"))
      }
    }

  /** Get CLI status including version and protocol information. */
  def getStatus(): Future[GetStatusResponse] =
    ensureConnected().flatMap { client =>
      client.sendRequest("status.get", Json.obj()).flatMap { result =>
        result.as[GetStatusResponse] match
          case Right(r) => Future.successful(r)
          case Left(err) => Future.failed(new RuntimeException(s"Failed to decode status response: $err"))
      }
    }

  /** Get current authentication status. */
  def getAuthStatus(): Future[GetAuthStatusResponse] =
    ensureConnected().flatMap { client =>
      client.sendRequest("auth.getStatus", Json.obj()).flatMap { result =>
        result.as[GetAuthStatusResponse] match
          case Right(r) => Future.successful(r)
          case Left(err) => Future.failed(new RuntimeException(s"Failed to decode auth status: $err"))
      }
    }

  /**
   * List available models. Results are cached after the first call.
   */
  def listModels(): Future[List[ModelInfo]] =
    modelsCache match
      case Some(models) => Future.successful(models)
      case None =>
        ensureConnected().flatMap { client =>
          client.sendRequest("models.list", Json.obj()).flatMap { result =>
            result.hcursor.get[List[ModelInfo]]("models") match
              case Right(models) =>
                modelsCache = Some(models)
                Future.successful(models)
              case Left(err) =>
                Future.failed(new RuntimeException(s"Failed to decode models: $err"))
          }
        }

  /** Get metadata for a session by ID. */
  def getSessionMetadata(sessionId: String): Future[Option[Json]] =
    ensureConnected().flatMap { client =>
      client.sendRequest("session.getMetadata", Json.obj("sessionId" -> sessionId.asJson)).map { result =>
        Some(result)
      }.recover { case _ => None }
    }

  /** Get the last session ID. */
  def getLastSessionId(): Future[Option[String]] =
    ensureConnected().flatMap { client =>
      client.sendRequest("session.getLastId", Json.obj()).map { result =>
        result.hcursor.get[String]("sessionId").toOption
      }
    }

  /** Delete a session by ID. */
  def deleteSession(sessionId: String): Future[Unit] =
    ensureConnected().flatMap { client =>
      client.sendRequest("session.delete", Json.obj("sessionId" -> sessionId.asJson)).flatMap { result =>
        val success = result.hcursor.get[Boolean]("success").getOrElse(false)
        if success then
          sessions.remove(sessionId)
          Future.successful(())
        else
          val error = result.hcursor.get[String]("error").getOrElse("Unknown error")
          Future.failed(new RuntimeException(s"Failed to delete session $sessionId: $error"))
      }
    }

  /** List all available sessions. */
  def listSessions(): Future[List[SessionMetadata]] =
    ensureConnected().flatMap { client =>
      client.sendRequest("session.list", Json.obj()).flatMap { result =>
        result.hcursor.get[List[SessionMetadata]]("sessions") match
          case Right(sessions) => Future.successful(sessions)
          case Left(err) => Future.failed(new RuntimeException(s"Failed to decode sessions: $err"))
      }
    }

  /** Get the foreground session ID (TUI+server mode). */
  def getForegroundSessionId(): Future[Option[String]] =
    ensureConnected().flatMap { client =>
      client.sendRequest("session.getForeground", Json.obj()).map { result =>
        result.hcursor.get[String]("sessionId").toOption
      }
    }

  /** Set the foreground session (TUI+server mode). */
  def setForegroundSessionId(sessionId: String): Future[Unit] =
    ensureConnected().flatMap { client =>
      client.sendRequest("session.setForeground", Json.obj("sessionId" -> sessionId.asJson)).flatMap { result =>
        val success = result.hcursor.get[Boolean]("success").getOrElse(false)
        if success then Future.successful(())
        else
          val error = result.hcursor.get[String]("error").getOrElse("Failed to set foreground session")
          Future.failed(new RuntimeException(error))
      }
    }

  /**
   * Clears the runtime's cached enterprise managed settings for every account so the next
   * session resolution re-fetches server policy.
   */
  def clearManagedSettingsCache(): Future[Unit] =
    ensureConnected().flatMap { client =>
      client.sendRequest("managedSettings.clearCache", Json.obj()).map(_ => ())
    }

  /**
   * Saves a legacy MCP configuration credential in the runtime-owned secret store
   * (`mcp.secrets.save`). Requires Copilot CLI 1.0.116 or newer.
   *
   * @param secretId  Identifier of the credential
   * @param value     The credential; it is never logged
   * @param configDir Configuration directory, or None for the runtime's default
   * @return The result; `usedKeychain` is true when the OS keychain stored the credential and
   *         false when the runtime used its file backend
   */
  def mcpSecretsSave(secretId: String, value: String, configDir: Option[String] = None): Future[McpSecretsSaveResult] =
    ensureConnected().flatMap { client =>
      client.sendRequest("mcp.secrets.save", Json.obj(
        "secretId" -> secretId.asJson,
        "value" -> value.asJson,
        "configDir" -> configDir.asJson
      ).dropNullValues).map(McpSecretsSaveResult.fromJson)
    }

  /**
   * Deletes a legacy MCP configuration credential from the runtime-owned secret store
   * (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
   *
   * @param secretId  Identifier of the credential
   * @param configDir Configuration directory, or None for the runtime's default
   */
  def mcpSecretsDelete(secretId: String, configDir: Option[String] = None): Future[Unit] =
    ensureConnected().flatMap { client =>
      client.sendRequest("mcp.secrets.delete", Json.obj(
        "secretId" -> secretId.asJson,
        "configDir" -> configDir.asJson
      ).dropNullValues).map(_ => ())
    }

  /**
   * Deletes the legacy credentials indexed for an MCP server (`mcp.secrets.deleteServer`).
   * Requires Copilot CLI 1.0.116 or newer.
   *
   * @param serverName The MCP server name
   * @param configDir  Configuration directory, or None for the runtime's default
   */
  def mcpSecretsDeleteServer(serverName: String, configDir: Option[String] = None): Future[Unit] =
    ensureConnected().flatMap { client =>
      client.sendRequest("mcp.secrets.deleteServer", Json.obj(
        "serverName" -> serverName.asJson,
        "configDir" -> configDir.asJson
      ).dropNullValues).map(_ => ())
    }

  /**
   * Persists a pending plugin extension permission request as a user-scoped approval, before the
   * host answers that request normally (`permissions.extensionApprovals.grant`). Requires Copilot
   * CLI 1.0.116 or newer.
   *
   * @param sessionId Session that owns the pending request
   * @param requestId ID of the pending extension permission request
   * @return Whether the request was eligible and persisted, and the stored approval
   */
  def extensionApprovalsGrant(sessionId: String, requestId: String): Future[PermissionsExtensionApprovalsGrantResult] =
    ensureConnected().flatMap { client =>
      client.sendRequest("permissions.extensionApprovals.grant", Json.obj(
        "sessionId" -> sessionId.asJson,
        "requestId" -> requestId.asJson
      )).map(PermissionsExtensionApprovalsGrantResult.fromJson)
    }

  /**
   * Lists the user-scoped plugin extension approvals (`permissions.extensionApprovals.list`).
   * Requires Copilot CLI 1.0.116 or newer.
   *
   * @param sessionId Attached session whose configuration profile to list, or None for the
   *                  server's profile
   */
  def extensionApprovalsList(sessionId: Option[String] = None): Future[PermissionsExtensionApprovalsListResult] =
    ensureConnected().flatMap { client =>
      client.sendRequest("permissions.extensionApprovals.list", Json.obj(
        "sessionId" -> sessionId.asJson
      ).dropNullValues).map(PermissionsExtensionApprovalsListResult.fromJson)
    }

  /**
   * Revokes one user-scoped plugin extension approval (`permissions.extensionApprovals.revoke`).
   * Requires Copilot CLI 1.0.116 or newer.
   *
   * @param extensionName   The approval's `extensionName`
   * @param installationKey The approval's `installationKey`, unchanged
   * @param sessionId       Attached session whose configuration profile to use, or None for the
   *                        server's profile
   * @return Whether a matching approval was revoked
   */
  def extensionApprovalsRevoke(
    extensionName: String,
    installationKey: String,
    sessionId: Option[String] = None
  ): Future[PermissionsExtensionApprovalsRevokeResult] =
    ensureConnected().flatMap { client =>
      client.sendRequest("permissions.extensionApprovals.revoke", Json.obj(
        "extensionName" -> extensionName.asJson,
        "installationKey" -> installationKey.asJson,
        "sessionId" -> sessionId.asJson
      ).dropNullValues).map(PermissionsExtensionApprovalsRevokeResult.fromJson)
    }

  /**
   * Subscribe to all session lifecycle events.
   *
   * @param handler Callback for lifecycle events
   * @return An unsubscribe function
   */
  def onLifecycle(handler: SessionLifecycleHandler): () => Unit =
    lifecycleHandlers.add(handler)
    () => lifecycleHandlers.remove(handler)

  /**
   * Subscribe to a specific lifecycle event type.
   *
   * @param eventType The event type to listen for
   * @param handler   Callback for matching events
   * @return An unsubscribe function
   */
  def onLifecycle(eventType: String, handler: SessionLifecycleHandler): () => Unit =
    typedLifecycleHandlers
      .computeIfAbsent(eventType, _ => mutable.Set.empty)
      .addOne(handler)
    () =>
      val handlers = typedLifecycleHandlers.get(eventType)
      if handlers != null then handlers.remove(handler)

  // -------------------------------------------------------------------------
  // Private: CLI process management
  // -------------------------------------------------------------------------

  private def parseCliUrl(url: String): (String, Int) =
    val clean = url.replaceFirst("^https?://", "")
    if clean.matches("^\\d+$") then
      ("localhost", clean.toInt)
    else
      val parts = clean.split(":")
      if parts.length != 2 then
        throw new IllegalArgumentException(
          s"Invalid cliUrl format: $url. Expected \"host:port\", \"http://host:port\", or \"port\""
        )
      val host = if parts(0).isEmpty then "localhost" else parts(0)
      val port = parts(1).toInt
      if port <= 0 || port > 65535 then
        throw new IllegalArgumentException(s"Invalid port in cliUrl: $url")
      (host, port)

  private def startCliServer(): Unit =
    val args = mutable.ListBuffer[String]()
    args ++= options.cliArgs
    args += "--headless"
    args += "--no-auto-update"
    args += "--log-level"
    args += options.logLevel

    if options.useStdio then
      args += "--stdio"
    else if options.port > 0 then
      args += "--port"
      args += options.port.toString

    // Auth flags
    options.githubToken.foreach { _ =>
      args += "--auth-token-env"
      args += "COPILOT_SDK_AUTH_TOKEN"
    }
    if !resolvedUseLoggedInUser then
      args += "--no-auto-login"

    // A .js entry point (for example the CLI's npm-loader.js) runs with node; other paths run directly.
    val command = if resolvedCliPath.endsWith(".js") then Seq("node", resolvedCliPath) else Seq(resolvedCliPath)
    val processBuilder = new ProcessBuilder((command ++ args.toSeq)*)
    processBuilder.redirectErrorStream(false)

    // Set working directory
    options.cwd.foreach(dir => processBuilder.directory(new File(dir)))

    // Set environment variables
    val env = processBuilder.environment()
    options.env.foreach { envMap =>
      env.clear()
      envMap.foreach((k, v) => env.put(k, v))
    }
    env.remove("NODE_DEBUG") // Suppress debug output that might pollute stdout
    options.githubToken.foreach(token => env.put("COPILOT_SDK_AUTH_TOKEN", token))

    val process = processBuilder.start()
    cliProcess = Some(process)

    // Forward stderr
    val stderrThread = new Thread(() => {
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getErrorStream))
      try
        var line = reader.readLine()
        while line != null do
          System.err.println(s"[CLI subprocess] $line")
          line = reader.readLine()
      catch
        case _: Exception => // ignore
    }, "cli-stderr-reader")
    stderrThread.setDaemon(true)
    stderrThread.start()

    if !options.useStdio then
      // TCP mode: read stdout to find port announcement
      val stdoutReader = new java.io.BufferedReader(
        new java.io.InputStreamReader(process.getInputStream)
      )
      val portPattern = "listening on port (\\d+)".r
      var found = false
      val startTime = System.currentTimeMillis()
      while !found && (System.currentTimeMillis() - startTime < 10000) do
        val line = stdoutReader.readLine()
        if line == null then
          throw new RuntimeException("CLI process exited before announcing port")
        portPattern.findFirstMatchIn(line).foreach { m =>
          // externalPort is immutable, so for TCP we'd store elsewhere -- but for stdio this is unused
          found = true
        }
      if !found then
        throw new RuntimeException("Timeout waiting for CLI server to start")

  private def connectToServer(): Unit =
    if options.useStdio && !isExternalServer then
      connectViaStdio()
    else
      connectViaTcp()

  private def connectViaStdio(): Unit =
    val process = cliProcess.getOrElse(
      throw new RuntimeException("CLI process not started")
    )
    val client = new JsonRpcClient(process.getInputStream, process.getOutputStream)
    client.start()
    rpcClient = Some(client)

  private def connectViaTcp(): Unit =
    val host = if isExternalServer then externalHost else "localhost"
    val port = if isExternalServer then externalPort else options.port
    if port <= 0 then
      throw new RuntimeException("Server port not available")

    val socket = new Socket(host, port)
    tcpSocket = Some(socket)
    val client = new JsonRpcClient(socket.getInputStream, socket.getOutputStream)
    client.start()
    rpcClient = Some(client)

  private def verifyProtocolVersion(pingResult: PingResponse): Unit =
    val expectedVersion = SdkProtocolVersion.get
    pingResult.protocolVersion match
      case None =>
        throw new RuntimeException(
          s"SDK protocol version mismatch: SDK supports version $expectedVersion, " +
            "but server does not report a protocol version. Please update your server to ensure compatibility."
        )
      case Some(serverVersion) if serverVersion != expectedVersion =>
        throw new RuntimeException(
          s"SDK protocol version mismatch: SDK supports version $expectedVersion, " +
            s"but server reports version $serverVersion. Please update your SDK or server to ensure compatibility."
        )
      case _ => // versions match

  // -------------------------------------------------------------------------
  // Connection handlers
  // -------------------------------------------------------------------------

  private def attachConnectionHandlers(): Unit =
    val client = rpcClient.getOrElse(return)

    // session.event notification
    client.onNotification("session.event", handleSessionEvent)

    // session.lifecycle notification
    client.onNotification("session.lifecycle", handleSessionLifecycle)

    // tool.call request (server asks client to execute a tool)
    client.onRequest("tool.call", handleToolCall)

    // permission.request (server asks client for permission)
    client.onRequest("permission.request", handlePermissionRequest)

    // userInput.request (server asks client for user input)
    client.onRequest("userInput.request", handleUserInputRequest)

    // hooks.invoke (server asks client to run a hook)
    client.onRequest("hooks.invoke", handleHooksInvoke)

    // exitPlanMode.request (server asks client to approve exiting plan mode)
    client.onRequest("exitPlanMode.request", handleExitPlanModeRequest)

    // gitHubToken.getToken (server asks a session's GitHub token provider for a credential)
    client.onRequest("gitHubToken.getToken", handleGitHubTokenRequest)

    // shellAuthentication.* (server asks a session to answer or cancel a shell prompt)
    client.onRequest("shellAuthentication.request", handleShellAuthenticationRequest)
    client.onRequest("shellAuthentication.cancel", handleShellAuthenticationCancel)

    // memoryStore.store (server hands a session's memory store a memory to save)
    client.onRequest("memoryStore.store", handleMemoryStoreRequest)

    // network.* (server asks the configured network callbacks)
    attachNetworkHandlers(client)

    // sessionFs.* (server drives filesystem access through the client)
    client.onRequest("sessionFs.readFile", handleSessionFsReadFile)
    client.onRequest("sessionFs.writeFile", handleSessionFsWriteFile)
    client.onRequest("sessionFs.appendFile", handleSessionFsAppendFile)
    client.onRequest("sessionFs.exists", handleSessionFsExists)
    client.onRequest("sessionFs.stat", handleSessionFsStat)
    client.onRequest("sessionFs.mkdir", handleSessionFsMkdir)
    client.onRequest("sessionFs.readdir", handleSessionFsReaddir)
    client.onRequest("sessionFs.readdirWithTypes", handleSessionFsReaddirWithTypes)
    client.onRequest("sessionFs.rm", handleSessionFsRm)
    client.onRequest("sessionFs.rename", handleSessionFsRename)

  private def handleSessionEvent(params: Json): Unit =
    val cursor = params.hcursor
    for
      sessionId <- cursor.get[String]("sessionId").toOption
      eventJson <- cursor.get[Json]("event").toOption
      event     <- eventJson.as[SessionEvent].toOption
      session   <- Option(sessions.get(sessionId))
    do
      session.dispatchEvent(event)

  private def handleSessionLifecycle(params: Json): Unit =
    params.as[SessionLifecycleEvent].toOption.foreach { event =>
      // Typed handlers
      val typedSet = typedLifecycleHandlers.get(event.`type`)
      if typedSet != null then
        typedSet.foreach { handler =>
          Try(handler(event)).failed.foreach(_ => ()) // ignore handler errors
        }
      // Wildcard handlers
      lifecycleHandlers.forEach { handler =>
        Try(handler(event)).failed.foreach(_ => ()) // ignore handler errors
      }
    }

  private def handleToolCall(params: Json): Future[Json] =
    val cursor = params.hcursor
    val sessionId = cursor.get[String]("sessionId").getOrElse(
      throw new RuntimeException("Invalid tool call: missing sessionId")
    )
    val toolCallId = cursor.get[String]("toolCallId").getOrElse(
      throw new RuntimeException("Invalid tool call: missing toolCallId")
    )
    val toolName = cursor.get[String]("toolName").getOrElse(
      throw new RuntimeException("Invalid tool call: missing toolName")
    )
    val arguments = cursor.get[Json]("arguments").getOrElse(Json.obj())

    val session = Option(sessions.get(sessionId)).getOrElse(
      throw new RuntimeException(s"Unknown session $sessionId")
    )

    val handler = session.getToolHandler(toolName)
    handler match
      case None =>
        val errorResult = ToolResultObject(
          textResultForLlm = s"Tool '$toolName' is not supported by this client instance.",
          resultType = ToolResultType.Failure,
          error = Some(s"tool '$toolName' not supported"),
          toolTelemetry = Some(Map.empty)
        )
        Future.successful(Json.obj("result" -> errorResult.asJson))

      case Some(h) =>
        val invocation = ToolInvocation(sessionId, toolCallId, toolName, arguments)
        h(arguments, invocation)
          .map { result =>
            Json.obj("result" -> result.asJson)
          }
          .recover { case ex: Exception =>
            val errorResult = ToolResultObject(
              textResultForLlm = "Invoking this tool produced an error. Detailed information is not available.",
              resultType = ToolResultType.Failure,
              error = Some(ex.getMessage),
              toolTelemetry = Some(Map.empty)
            )
            Json.obj("result" -> errorResult.asJson)
          }

  private def handlePermissionRequest(params: Json): Future[Json] =
    val cursor = params.hcursor
    val sessionId = cursor.get[String]("sessionId").getOrElse(
      throw new RuntimeException("Invalid permission request: missing sessionId")
    )
    val requestJson = cursor.get[Json]("permissionRequest").getOrElse(Json.obj())

    val session = Option(sessions.get(sessionId)).getOrElse(
      throw new RuntimeException(s"Session not found: $sessionId")
    )

    session.handlePermissionRequest(requestJson).map { result =>
      Json.obj("result" -> result.asJson)
    }.recover { case _ =>
      Json.obj("result" -> Json.obj(
        "kind" -> "denied-no-approval-rule-and-could-not-request-from-user".asJson
      ))
    }

  private def handleUserInputRequest(params: Json): Future[Json] =
    val cursor = params.hcursor
    val sessionId = cursor.get[String]("sessionId").getOrElse(
      throw new RuntimeException("Invalid user input request: missing sessionId")
    )
    val question = cursor.get[String]("question").getOrElse("")
    val choices = cursor.get[List[String]]("choices").toOption
    val allowFreeform = cursor.get[Boolean]("allowFreeform").toOption
    val requestId = cursor.get[String]("requestId").toOption
    val imageAttachmentConstraints = cursor.get[UserInputImageAttachmentConstraints]("imageAttachmentConstraints").toOption
    val meta = cursor.downField("_meta").focus.filter(_.isObject)

    val session = Option(sessions.get(sessionId)).getOrElse(
      throw new RuntimeException(s"Session not found: $sessionId")
    )

    val request = UserInputRequest(Some(question), choices, allowFreeform, requestId, imageAttachmentConstraints, meta)
    session.handleUserInputRequest(request).map(_.asJson)

  private def handleHooksInvoke(params: Json): Future[Json] =
    val cursor = params.hcursor
    val sessionId = cursor.get[String]("sessionId").getOrElse(
      throw new RuntimeException("Invalid hooks invoke: missing sessionId")
    )
    val hookType = cursor.get[String]("hookType").getOrElse(
      throw new RuntimeException("Invalid hooks invoke: missing hookType")
    )
    val input = cursor.get[Json]("input").getOrElse(Json.obj())

    val session = Option(sessions.get(sessionId)).getOrElse(
      throw new RuntimeException(s"Session not found: $sessionId")
    )

    session.handleHooksInvoke(hookType, input).map { output =>
      Json.obj("output" -> output.getOrElse(Json.Null))
    }

  private def handleExitPlanModeRequest(params: Json): Future[Json] =
    val cursor = params.hcursor
    val sessionId = cursor.get[String]("sessionId").getOrElse(
      throw new RuntimeException("Invalid exit plan mode request: missing sessionId")
    )

    val session = Option(sessions.get(sessionId)).getOrElse(
      throw new RuntimeException(s"Session not found: $sessionId")
    )

    val request = ExitPlanModeRequest(sessionId)
    session.handleExitPlanModeRequest(request).map { result =>
      Json.obj("approved" -> result.approved.asJson)
    }

  /** Serves `gitHubToken.getToken` by invoking the provider registered under `registrationId`. */
  private def handleGitHubTokenRequest(params: Json): Future[Json] =
    val cursor = params.hcursor
    val registrationId = cursor.get[String]("registrationId").toOption
    registrationId.flatMap(id => Option(gitHubTokenProviders.get(id))) match
      case None =>
        Future.failed(new RuntimeException(
          s"No GitHub token provider registered for registration ID '${registrationId.getOrElse("")}'"
        ))
      case Some(registration) =>
        val args = GitHubTokenProviderArgs(
          host = cursor.get[String]("host").getOrElse(""),
          sessionId = cursor.get[String]("sessionId").toOption.orElse(registration.sessionId),
          reason = cursor.get[String]("reason").getOrElse("initial")
        )
        registration.provider(args).map(_.asJson)

  /** Answers `shellAuthentication.request` with the owning session's handler (`cancel` when unavailable). */
  private def handleShellAuthenticationRequest(params: Json): Future[Json] =
    params.hcursor.get[String]("sessionId").toOption.flatMap(id => Option(sessions.get(id))) match
      case Some(session) => session.handleShellAuthenticationRequest(params)
      case None => Future.successful(Json.obj("action" -> "cancel".asJson))

  /** Cancels a pending shell authentication prompt; always answers `{}`. */
  private def handleShellAuthenticationCancel(params: Json): Future[Json] =
    val cursor = params.hcursor
    for
      sessionId <- cursor.get[String]("sessionId").toOption
      requestId <- cursor.get[String]("requestId").toOption
      session   <- Option(sessions.get(sessionId))
    do session.cancelShellAuthentication(requestId)
    Future.successful(Json.obj())

  /** Stores a memory through the owning session's callback; answers with a JSON null result. */
  private def handleMemoryStoreRequest(params: Json): Future[Json] =
    params.hcursor.get[String]("sessionId").toOption.flatMap(id => Option(sessions.get(id))) match
      case Some(session) => session.handleMemoryStore(params)
      case None => Future.failed(new IllegalStateException("Memory store owner is unavailable"))

  /** Serves the configured network callbacks. Tokens and passwords are never logged. */
  private def attachNetworkHandlers(client: JsonRpcClient): Unit =
    options.network.foreach { network =>
      network.resolveProxy.foreach { resolver =>
        client.onRequest("network.resolveProxy", { params =>
          val url = params.hcursor.get[String]("url").getOrElse("")
          Future.delegate(resolver(NetworkResolveProxyRequest(url))).map { proxy =>
            Json.obj("proxy" -> proxy.asJson).dropNullValues
          }
        })
      }
      network.authenticateNegotiate.foreach { authenticator =>
        client.onRequest("network.authenticateNegotiate", { params =>
          val spn = params.hcursor.get[String]("spn").getOrElse("")
          Future.delegate(authenticator(NetworkAuthenticateNegotiateRequest(spn))).map { token =>
            Json.obj("token" -> token.asJson).dropNullValues
          }
        })
      }
      network.authenticateBasic.foreach { authenticator =>
        client.onRequest("network.authenticateBasic", { params =>
          val cursor = params.hcursor
          val request = NetworkAuthenticateBasicRequest(
            proxy = cursor.get[String]("proxy").getOrElse(""),
            realm = cursor.get[String]("realm").toOption,
            previousRejected = cursor.get[Boolean]("previousRejected").getOrElse(false)
          )
          // Send only the credential fields.
          Future.delegate(authenticator(request)).map { credentials =>
            Json.obj(
              "username" -> credentials.flatMap(_.username).asJson,
              "password" -> credentials.flatMap(_.password).asJson
            ).dropNullValues
          }
        })
      }
    }

  // -------------------------------------------------------------------------
  // v2.6.0 helpers: client info handshake, CAPI options, GitHub token providers
  // -------------------------------------------------------------------------

  /**
   * Declares the integrating application's identity via `connect` before the ping handshake.
   * Skipped when no [[ClientInfo]] field is set; ignored by older CLIs without `connect`.
   */
  private def sendConnectHandshake(client: JsonRpcClient): Unit =
    options.clientInfo.flatMap(clientInfoToWire).foreach { wire =>
      try
        concurrent.Await.result(
          client.sendRequest("connect", Json.obj("clientInfo" -> wire)),
          scala.concurrent.duration.Duration(10, "s")
        )
      catch
        case ex: JsonRpcException if isMethodNotFound(ex, "connect") => () // older CLI
    }

  /** Maps [[ClientInfo]] onto the `connect` wire shape, dropping empty fields. */
  private def clientInfoToWire(info: ClientInfo): Option[Json] =
    val fields = List(
      info.applicationName.filter(_.nonEmpty).map(v => "editorName" -> v.asJson),
      info.applicationVersion.filter(_.nonEmpty).map(v => "editorVersion" -> v.asJson),
      info.integrationName.filter(_.nonEmpty).map(v => "extensionName" -> v.asJson),
      info.integrationVersion.filter(_.nonEmpty).map(v => "extensionVersion" -> v.asJson)
    ).flatten
    if fields.isEmpty then None else Some(Json.obj(fields*))

  /**
   * Builds the `capi` provider options object (`github.capi`), or None when nothing is set.
   * Members of the explicit `capi` options win over the flat `autoTier`/`enableWebSocketResponses` fields.
   */
  private def capiOptions(
    autoTier: Option[String],
    enableWebSocketResponses: Option[Boolean],
    capi: Option[CapiConfig] = None
  ): Option[Json] =
    val wire = Json.obj(
      "enabled" -> capi.flatMap(_.enabled).asJson,
      "autoTier" -> capi.flatMap(_.autoTier).orElse(autoTier).asJson,
      "enableWebSocketResponses" -> capi.flatMap(_.enableWebSocketResponses).orElse(enableWebSocketResponses).asJson
    ).dropNullValues
    if wire.asObject.exists(_.isEmpty) then None else Some(wire)

  /** Rejects session options that cannot work: empty mode without tools, or a memory store without memory. */
  private def checkSessionOptions(
    availableTools: Option[List[String]],
    memory: Option[MemoryConfiguration]
  ): Future[Unit] =
    if resolvedMode == ClientMode.Empty && availableTools.isEmpty then
      Future.failed(new IllegalArgumentException(
        "CopilotClient is in mode 'empty' but the session config did not specify availableTools. " +
          "Empty mode requires every session to explicitly opt into the tools it wants."
      ))
    else if memory.exists(m => m.storeMemory.isDefined && !m.enabled) then
      Future.failed(new IllegalArgumentException("memory.storeMemory requires memory.enabled to be true."))
    else Future.unit

  /** Empty mode: applies safe runtime defaults under the caller's values (caller values win). */
  private def applyModeDefaults(wire: Json): Json =
    if resolvedMode != ClientMode.Empty then wire
    else
      val defaults = List(
        ("experiments", "enableExperimentalMode", false.asJson),
        ("github", "telemetryEnabled", false.asJson),
        ("mcp", "oauthTokenStorage", "in-memory".asJson),
        ("state", "skipEmbeddingRetrieval", true.asJson),
        ("state", "embeddingCacheStorage", "in-memory".asJson),
        ("state", "enableCrossSessionStore", false.asJson),
        ("state", "memory", Json.obj("enabled" -> false.asJson)),
        ("instructions", "onDemandDiscovery", false.asJson),
        ("hooks", "enableRepositoryHooks", false.asJson),
        ("git", "enableHostGitOperations", false.asJson)
      )
      defaults.foldLeft(wire) { case (acc, (group, member, value)) =>
        val existing = acc.hcursor.downField(group).focus.flatMap(_.asObject).getOrElse(JsonObject.empty)
        if existing.contains(member) then acc
        else acc.mapObject(_.add(group, Json.fromJsonObject(existing.add(member, value))))
      }

  private def checkGitHubTokenOptions(
    gitHubToken: Option[String],
    provider: Option[GitHubTokenProvider]
  ): Future[Unit] =
    if gitHubToken.isDefined && provider.isDefined then
      Future.failed(new IllegalArgumentException("gitHubToken and gitHubTokenProvider are mutually exclusive"))
    else Future.unit

  private def registerGitHubTokenProvider(
    provider: Option[GitHubTokenProvider],
    sessionId: Option[String]
  ): Option[String] =
    provider.map { p =>
      val registrationId = java.util.UUID.randomUUID().toString
      gitHubTokenProviders.put(registrationId, new GitHubTokenRegistration(p, sessionId))
      registrationId
    }

  /**
   * Binds a successful create/resume to its provider registration: drops registrations committed by
   * an earlier open of the same session and removes the new one when the session is destroyed.
   */
  private def commitGitHubTokenProvider(session: CopilotSession, registrationId: Option[String]): Unit =
    val sessionId = session.sessionId
    gitHubTokenProviders.entrySet().removeIf((entry: java.util.Map.Entry[String, GitHubTokenRegistration]) =>
      entry.getValue.committed && entry.getValue.sessionId.contains(sessionId) && !registrationId.contains(entry.getKey)
    )
    registrationId.foreach { id =>
      Option(gitHubTokenProviders.get(id)).foreach { registration =>
        registration.sessionId = Some(sessionId)
        registration.committed = true
        session.registerOnDestroyed(() => gitHubTokenProviders.remove(id))
      }
    }

  // -------------------------------------------------------------------------
  // SessionFs provider handlers
  //
  // Service the CLI's sessionFs.* callbacks against the real filesystem. Success
  // returns mirror the generated result shapes; failures are returned in-band as
  // a SessionFsError ({ code, message }) so the CLI can distinguish a missing
  // file (ENOENT) from other errors without failing the JSON-RPC request.
  // -------------------------------------------------------------------------

  private def handleSessionFsReadFile(params: Json): Future[Json] =
    val path = params.hcursor.get[String]("path").getOrElse("")
    val result =
      try
        val content = java.nio.file.Files.readString(java.nio.file.Paths.get(path))
        Json.obj("content" -> content.asJson)
      catch
        case e: Exception =>
          Json.obj("content" -> "".asJson, "error" -> sessionFsError(e))
    Future.successful(result)

  private def handleSessionFsWriteFile(params: Json): Future[Json] =
    val cursor = params.hcursor
    val path = cursor.get[String]("path").getOrElse("")
    val content = cursor.get[String]("content").getOrElse("")
    val result =
      try
        val p = java.nio.file.Paths.get(path)
        Option(p.getParent()).foreach(parent => java.nio.file.Files.createDirectories(parent))
        java.nio.file.Files.writeString(p, content)
        Json.Null
      catch
        case e: Exception => sessionFsError(e)
    Future.successful(result)

  private def handleSessionFsAppendFile(params: Json): Future[Json] =
    val cursor = params.hcursor
    val path = cursor.get[String]("path").getOrElse("")
    val content = cursor.get[String]("content").getOrElse("")
    val result =
      try
        val p = java.nio.file.Paths.get(path)
        Option(p.getParent()).foreach(parent => java.nio.file.Files.createDirectories(parent))
        java.nio.file.Files.writeString(p, content,
          java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
        Json.Null
      catch
        case e: Exception => sessionFsError(e)
    Future.successful(result)

  private def handleSessionFsExists(params: Json): Future[Json] =
    val path = params.hcursor.get[String]("path").getOrElse("")
    val exists =
      try java.nio.file.Files.exists(java.nio.file.Paths.get(path))
      catch case _: Exception => false
    Future.successful(Json.obj("exists" -> exists.asJson))

  private def handleSessionFsStat(params: Json): Future[Json] =
    val path = params.hcursor.get[String]("path").getOrElse("")
    val result =
      try
        val attrs = java.nio.file.Files.readAttributes(
          java.nio.file.Paths.get(path),
          classOf[java.nio.file.attribute.BasicFileAttributes]
        )
        Json.obj(
          "isFile" -> attrs.isRegularFile().asJson,
          "isDirectory" -> attrs.isDirectory().asJson,
          "size" -> attrs.size().asJson,
          "mtime" -> java.time.Instant.ofEpochMilli(attrs.lastModifiedTime().toMillis()).toString.asJson,
          "birthtime" -> java.time.Instant.ofEpochMilli(attrs.creationTime().toMillis()).toString.asJson,
        )
      catch
        case e: Exception =>
          val now = java.time.Instant.now().toString
          Json.obj(
            "isFile" -> false.asJson,
            "isDirectory" -> false.asJson,
            "size" -> 0.asJson,
            "mtime" -> now.asJson,
            "birthtime" -> now.asJson,
            "error" -> sessionFsError(e),
          )
    Future.successful(result)

  private def handleSessionFsMkdir(params: Json): Future[Json] =
    val cursor = params.hcursor
    val path = cursor.get[String]("path").getOrElse("")
    val recursive = cursor.get[Boolean]("recursive").getOrElse(false)
    val result =
      try
        val p = java.nio.file.Paths.get(path)
        if recursive then java.nio.file.Files.createDirectories(p)
        else java.nio.file.Files.createDirectory(p)
        Json.Null
      catch
        case e: Exception => sessionFsError(e)
    Future.successful(result)

  private def handleSessionFsReaddir(params: Json): Future[Json] =
    val path = params.hcursor.get[String]("path").getOrElse("")
    val result =
      try
        val dir = new File(path)
        if !dir.isDirectory() then throw new java.io.FileNotFoundException(path)
        val names = Option(dir.listFiles()).getOrElse(Array.empty[File]).map(_.getName()).toList
        Json.obj("entries" -> names.asJson)
      catch
        case e: Exception =>
          Json.obj("entries" -> List.empty[String].asJson, "error" -> sessionFsError(e))
    Future.successful(result)

  private def handleSessionFsReaddirWithTypes(params: Json): Future[Json] =
    val path = params.hcursor.get[String]("path").getOrElse("")
    val result =
      try
        val dir = new File(path)
        if !dir.isDirectory() then throw new java.io.FileNotFoundException(path)
        val entries = Option(dir.listFiles()).getOrElse(Array.empty[File]).map { f =>
          Json.obj(
            "name" -> f.getName().asJson,
            "type" -> (if f.isDirectory() then "directory" else "file").asJson,
          )
        }.toList
        Json.obj("entries" -> entries.asJson)
      catch
        case e: Exception =>
          Json.obj("entries" -> List.empty[Json].asJson, "error" -> sessionFsError(e))
    Future.successful(result)

  private def handleSessionFsRm(params: Json): Future[Json] =
    val cursor = params.hcursor
    val path = cursor.get[String]("path").getOrElse("")
    val recursive = cursor.get[Boolean]("recursive").getOrElse(false)
    val force = cursor.get[Boolean]("force").getOrElse(false)
    val result =
      try
        val p = java.nio.file.Paths.get(path)
        if java.nio.file.Files.exists(p) then
          if recursive then deleteRecursively(p) else java.nio.file.Files.delete(p)
          Json.Null
        else if force then
          Json.Null
        else
          sessionFsError(new java.io.FileNotFoundException(path))
      catch
        case e: Exception => if force then Json.Null else sessionFsError(e)
    Future.successful(result)

  private def handleSessionFsRename(params: Json): Future[Json] =
    val cursor = params.hcursor
    val src = cursor.get[String]("src").getOrElse("")
    val dest = cursor.get[String]("dest").getOrElse("")
    val result =
      try
        val destPath = java.nio.file.Paths.get(dest)
        Option(destPath.getParent()).foreach(parent => java.nio.file.Files.createDirectories(parent))
        java.nio.file.Files.move(
          java.nio.file.Paths.get(src), destPath,
          java.nio.file.StandardCopyOption.REPLACE_EXISTING
        )
        Json.Null
      catch
        case e: Exception => sessionFsError(e)
    Future.successful(result)

  private def deleteRecursively(path: java.nio.file.Path): Unit =
    val file = path.toFile()
    if file.isDirectory() then
      Option(file.listFiles())
        .getOrElse(Array.empty[File])
        .foreach(child => deleteRecursively(child.toPath()))
    java.nio.file.Files.deleteIfExists(path)

  private def sessionFsError(err: Throwable): Json =
    val code = err match
      case _: java.nio.file.NoSuchFileException => "ENOENT"
      case _: java.io.FileNotFoundException     => "ENOENT"
      case _                                    => "UNKNOWN"
    Json.obj(
      "code" -> code.asJson,
      "message" -> Option(err.getMessage()).getOrElse("").asJson,
    )

  private def injectTraceContext(params: Json): Json =
    options.onGetTraceContext match
      case None => params
      case Some(provider) =>
        try
          val ctx = concurrent.Await.result(provider(), scala.concurrent.duration.Duration(5, "s"))
          var result = params
          ctx.traceparent.foreach { tp =>
            result = result.deepMerge(Json.obj("traceparent" -> tp.asJson))
          }
          ctx.tracestate.foreach { ts =>
            result = result.deepMerge(Json.obj("tracestate" -> ts.asJson))
          }
          result
        catch
          case _: Exception => params

  // -------------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------------

  private def ensureConnected(): Future[JsonRpcClient] =
    rpcClient match
      case Some(client) if stateRef.get() == ConnectionState.Connected =>
        Future.successful(client)
      case _ if options.autoStart =>
        start().map(_ =>
          rpcClient.getOrElse(throw new RuntimeException("Failed to connect"))
        )
      case _ =>
        Future.failed(new RuntimeException("Client not connected. Call start() first."))

// ============================================================================
// Message payload builder + wire constants
// ============================================================================

/**
 * Builds the JSON-RPC `session.send` payload from [[MessageOptions]]. The legacy string
 * `responseFormat` is not sent: protocol 4 only accepts the structured format.
 */
private[copilot] def buildSendPayload(sessionId: String, options: MessageOptions): Json =
  Json.obj(
    "sessionId" -> sessionId.asJson,
    "prompt" -> options.prompt.asJson,
    "source" -> options.source.asJson,
    "clientTurnId" -> options.clientTurnId.asJson,
    "attachments" -> options.attachments.asJson,
    "mode" -> options.mode.asJson,
    "responseFormat" -> options.responseSchema.map(structuredResponseFormat).asJson,
    "imageOptions" -> options.imageOptions.asJson,
    "requestHeaders" -> options.requestHeaders.asJson,
    "agentMode" -> options.agentMode.asJson,
    "displayPrompt" -> options.displayPrompt.asJson,
  ).dropNullValues

/** Wraps a JSON Schema as the structured-output `responseFormat` object. */
private[copilot] def structuredResponseFormat(schema: JsonObject): Json =
  Json.obj(
    "type" -> "json_schema".asJson,
    "jsonSchema" -> Json.obj(
      "name" -> "response".asJson,
      "strict" -> true.asJson,
      "schema" -> Json.fromJsonObject(schema)
    )
  )

// ============================================================================
// SDK protocol 4: grouped session payloads
// ============================================================================

/** Flat keys that stay at the root of protocol 4 create/resume payloads. */
private[copilot] val RootSessionKeys: Set[String] = Set(
  "sessionId", "model", "name", "clientName", "workingDirectory", "configDirectory",
  "enableConfigDiscovery", "systemMessage", "diagnostics", "lifecycleDiagnostics", "cloud",
  "traceparent", "tracestate", "suppressResumeEvent", "continuePendingWork",
  "allowTranscriptRecovery", "requestedEnvironmentVariables"
)

/** Flat keys accepted only by `session.create`; dropped on resume. */
private[copilot] val CreateOnlySessionKeys: Set[String] =
  Set("name", "cloud", "trustWorkingDirectory", "refreshCustomInstructions")

/** Flat keys accepted only by `session.resume`; dropped on create. */
private[copilot] val ResumeOnlySessionKeys: Set[String] = Set(
  "suppressResumeEvent", "continuePendingWork", "allowTranscriptRecovery", "requestedEnvironmentVariables", "openCanvases"
)

/** Option groups of protocol 4 create/resume payloads. */
private[copilot] val SessionOptionGroups: Set[String] = Set(
  "inference", "tools", "permissions", "interaction", "events", "agents", "skills", "mcp",
  "github", "filesystem", "execution", "instructions", "hooks", "git", "state", "plugins",
  "canvases", "extensions", "experiments", "policy"
)

/** Flat keys whose JSON null is an explicit clear that must be sent rather than skipped. */
private[copilot] val ExplicitNullSessionKeys: Set[String] = Set("defaultProvider")

/** Legacy provider fields that describe the model rather than the connection. */
private[copilot] val ProviderModelFields: Set[String] = Set(
  "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens", "capabilities"
)

/** Flat keys mapped to their protocol 4 (group, member) location. */
private[copilot] val GroupedSessionKeys: Map[String, (String, String)] = Map(
  "reasoningEffort" -> ("inference", "reasoningEffort"),
  "reasoningSummary" -> ("inference", "reasoningSummary"),
  "contextTier" -> ("inference", "contextTier"),
  "allowMultipleToolCalls" -> ("inference", "allowMultipleToolCalls"),
  "enableCitations" -> ("inference", "enableCitations"),
  "modelCapabilities" -> ("inference", "capabilities"),
  "verbosity" -> ("inference", "verbosity"),
  "providers" -> ("inference", "providers"),
  "models" -> ("inference", "models"),
  "defaultProvider" -> ("inference", "defaultProvider"),
  "tools" -> ("tools", "definitions"),
  "availableTools" -> ("tools", "availableTools"),
  "excludedTools" -> ("tools", "excludedTools"),
  "toolSearch" -> ("tools", "search"),
  "largeOutput" -> ("tools", "largeOutput"),
  "imageGeneration" -> ("tools", "imageGeneration"),
  "manageScheduleEnabled" -> ("tools", "manageScheduleEnabled"),
  "webSearch" -> ("tools", "webSearch"),
  "shell" -> ("tools", "shell"),
  "maxInlineBinaryBytes" -> ("tools", "maxInlineBinaryBytes"),
  "requestPermission" -> ("permissions", "request"),
  "sandboxConfig" -> ("permissions", "sandbox"),
  "sandbox" -> ("permissions", "sandbox"),
  "permissionMode" -> ("permissions", "mode"),
  "sandboxConfigSource" -> ("permissions", "sandboxSource"),
  "commands" -> ("interaction", "commands"),
  "askUserVariant" -> ("interaction", "askUserVariant"),
  "requestUserInput" -> ("interaction", "requestUserInput"),
  "requestElicitation" -> ("interaction", "requestElicitation"),
  "requestShellAuthentication" -> ("interaction", "requestShellAuthentication"),
  "requestExitPlanMode" -> ("interaction", "requestExitPlanMode"),
  "requestAutoModeSwitch" -> ("interaction", "requestAutoModeSwitch"),
  "requestAutoModeRecovery" -> ("interaction", "requestAutoModeRecovery"),
  "streaming" -> ("events", "streaming"),
  "includeSubAgentStreamingEvents" -> ("events", "includeSubAgentStreamingEvents"),
  "customAgents" -> ("agents", "definitions"),
  "defaultAgent" -> ("agents", "default"),
  "agent" -> ("agents", "selected"),
  "includedBuiltinAgents" -> ("agents", "includedBuiltinAgents"),
  "excludedBuiltinAgents" -> ("agents", "excludedBuiltinAgents"),
  "customAgentsLocalOnly" -> ("agents", "localDiscoveryOnly"),
  "agentDirectories" -> ("agents", "directories"),
  "skillDirectories" -> ("skills", "directories"),
  "enableSkills" -> ("skills", "enabled"),
  "includedBuiltinSkills" -> ("skills", "includedBuiltinSkills"),
  "disabledSkills" -> ("skills", "disabledSkills"),
  "hasSkillProvider" -> ("skills", "hasProvider"),
  "ignoredSkillsLocations" -> ("skills", "ignoredLocations"),
  "mcpServers" -> ("mcp", "servers"),
  "disabledMcpServers" -> ("mcp", "disabledServers"),
  "requestMcpApps" -> ("mcp", "enableApps"),
  "enableMcpApps" -> ("mcp", "enableApps"),
  "mcpOAuthTokenStorage" -> ("mcp", "oauthTokenStorage"),
  "authClientIdMetadataUrl" -> ("mcp", "authClientIdMetadataUrl"),
  "envValueMode" -> ("mcp", "envValueMode"),
  "managedMcpServers" -> ("mcp", "managedServers"),
  "allowAllMcpServerInstructions" -> ("mcp", "allowAllServerInstructions"),
  "gitHubToken" -> ("github", "token"),
  "gitHubTokenProviderRegistrationId" -> ("github", "tokenProviderRegistrationId"),
  "githubMcpToolConfig" -> ("github", "mcp"),
  "capi" -> ("github", "capi"),
  "enableSessionTelemetry" -> ("github", "telemetryEnabled"),
  "enableManagedSettings" -> ("github", "fetchManagedSettings"),
  "interactiveUser" -> ("github", "interactiveUser"),
  "enableGitHubTelemetryForwarding" -> ("github", "telemetryForwarding"),
  "authInfo" -> ("github", "authInfo"),
  "copilotUrl" -> ("github", "copilotUrl"),
  "additionalDirectories" -> ("filesystem", "additionalDirectories"),
  "enableFileChangeTracking" -> ("filesystem", "trackFileChanges"),
  "trustWorkingDirectory" -> ("filesystem", "trustWorkingDirectory"),
  "sessionLimits" -> ("execution", "limits"),
  "autopilot" -> ("execution", "autopilot"),
  "autopilotContinuation" -> ("execution", "autopilotContinuation"),
  "instructionDirectories" -> ("instructions", "directories"),
  "organizationCustomInstructions" -> ("instructions", "organizationText"),
  "enableOnDemandInstructionDiscovery" -> ("instructions", "onDemandDiscovery"),
  "skipCustomInstructions" -> ("instructions", "skipCustomInstructions"),
  "refreshCustomInstructions" -> ("instructions", "refreshCustomInstructions"),
  "hooks" -> ("hooks", "hasHandlers"),
  "hookDirectories" -> ("hooks", "directories"),
  "enableFileHooks" -> ("hooks", "enableRepositoryHooks"),
  "coauthorEnabled" -> ("git", "coauthorEnabled"),
  "enableHostGitOperations" -> ("git", "enableHostGitOperations"),
  "infiniteSessions" -> ("state", "infiniteSessions"),
  "memory" -> ("state", "memory"),
  "hasMemoryStore" -> ("state", "hasMemoryStore"),
  "enableSessionStore" -> ("state", "enableCrossSessionStore"),
  "skipEmbeddingRetrieval" -> ("state", "skipEmbeddingRetrieval"),
  "embeddingCacheStorage" -> ("state", "embeddingCacheStorage"),
  "pluginDirectories" -> ("plugins", "directories"),
  "installedPlugins" -> ("plugins", "installed"),
  "canvases" -> ("canvases", "definitions"),
  "canvasProvider" -> ("canvases", "provider"),
  "requestCanvasRenderer" -> ("canvases", "requestRenderer"),
  "openCanvases" -> ("canvases", "open"),
  "requestExtensions" -> ("extensions", "enabled"),
  "extensionSdkPath" -> ("extensions", "sdkPath"),
  "extensionInfo" -> ("extensions", "identity"),
  "enableExperimentalMode" -> ("experiments", "enableExperimentalMode"),
  "isExperimentalMode" -> ("experiments", "enableExperimentalMode"),
  "featureFlags" -> ("experiments", "flags"),
  "expAssignments" -> ("experiments", "assignments"),
  "featureFlagConfiguration" -> ("experiments", "configuration"),
  "allowedModels" -> ("policy", "allowedModels"),
  "managedSettings" -> ("policy", "managedSettings"),
  "excludedManagedSettings" -> ("policy", "excludedManagedSettings"),
  "enforceManagedModelDefaults" -> ("policy", "enforceManagedModelDefaults"),
)

/**
 * Converts the flat `session.create`/`session.resume` payload this SDK builds into the SDK protocol 4
 * grouped payload. Root keys stay at the top level, known flat keys move to their option group,
 * unknown keys pass through unchanged, null values are skipped (except explicit clears such as
 * `defaultProvider`), empty groups are omitted, and members of an already-grouped object win over
 * flat keys. A legacy single `provider` becomes the named "default" connection.
 */
private[copilot] def groupSessionParams(flat: Json, resume: Boolean): Json =
  val fields = flat.asObject.map(_.toList).getOrElse(Nil)
  val root = mutable.LinkedHashMap[String, Json]()
  val groups = mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, Json]]()
  // Already-grouped objects first, so their members win over flat keys.
  for (key, value) <- fields if SessionOptionGroups.contains(key) && value.isObject do
    val group = groups.getOrElseUpdate(key, mutable.LinkedHashMap.empty[String, Json])
    for (member, memberValue) <- value.asObject.map(_.toList).getOrElse(Nil) if !memberValue.isNull do
      group(member) = memberValue
  var legacyProvider: Option[JsonObject] = None
  for (flatKey, value) <- fields do
    val isGroupObject = SessionOptionGroups.contains(flatKey) && value.isObject
    val skippedNull = value.isNull && !ExplicitNullSessionKeys.contains(flatKey)
    if !isGroupObject && !skippedNull then
      val key = flatKey match
        case "configDir"     => "configDirectory"
        case "disableResume" => "suppressResumeEvent"
        case other           => other
      val dropped = if resume then CreateOnlySessionKeys.contains(key) else ResumeOnlySessionKeys.contains(key)
      if !dropped then
        if key == "provider" then legacyProvider = value.asObject
        else
          val path = if RootSessionKeys.contains(key) then None else GroupedSessionKeys.get(key)
          path match
            case None =>
              if !root.contains(key) then root(key) = value
            case Some((groupName, member)) =>
              val group = groups.getOrElseUpdate(groupName, mutable.LinkedHashMap.empty[String, Json])
              if !group.contains(member) then group(member) = value
  groups.get("mcp").foreach { mcp =>
    if mcp.contains("servers") && !mcp.contains("envValueMode") then mcp("envValueMode") = "direct".asJson
  }
  legacyProvider.foreach(provider => translateLegacyProvider(provider, root.get("model"), groups))
  for (groupName, members) <- groups if members.nonEmpty do
    root(groupName) = Json.fromFields(members)
  Json.fromFields(root)

/**
 * Translates a legacy single BYOK `provider` into the named "default" connection (plus a model
 * definition when model-specific fields are present), selects it as the default provider, and
 * disables Copilot API models as v1 BYOK did. Explicit `providers`, `defaultProvider`, or
 * `capi.enabled` win: the legacy provider is then dropped.
 */
private def translateLegacyProvider(
  provider: JsonObject,
  model: Option[Json],
  groups: mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, Json]]
): Unit =
  val inference = groups.getOrElseUpdate("inference", mutable.LinkedHashMap.empty[String, Json])
  val github = groups.getOrElseUpdate("github", mutable.LinkedHashMap.empty[String, Json])
  val capi = github.get("capi").flatMap(_.asObject)
  val explicit = inference.contains("providers") || inference.contains("defaultProvider") ||
    capi.exists(_.contains("enabled"))
  if !explicit then
    val connection = mutable.LinkedHashMap[String, Json]("name" -> "default".asJson)
    val modelFields = mutable.LinkedHashMap[String, Json]()
    for (field, value) <- provider.toList if !value.isNull && field != "name" do
      if ProviderModelFields.contains(field) then modelFields(field) = value
      else connection(field) = value
    inference("providers") = Json.arr(Json.fromFields(connection))
    inference("defaultProvider") = "default".asJson
    model.filterNot(_.isNull).foreach { modelId =>
      if modelFields.nonEmpty && !inference.contains("models") then
        val definition = mutable.LinkedHashMap[String, Json]("provider" -> "default".asJson, "id" -> modelId)
        definition ++= modelFields
        inference("models") = Json.arr(Json.fromFields(definition))
    }
    val capiWire = mutable.LinkedHashMap[String, Json]()
    capi.foreach(existing => capiWire ++= existing.toList)
    capiWire("enabled") = false.asJson
    github("capi") = Json.fromFields(capiWire)

/** Tool deferral policies for [[Tool.defer]]. */
object ToolDefer:
  val Auto  = "auto"
  val Never = "never"

/** Known system message section identifiers for section overrides. */
object SystemMessageSection:
  val Preamble         = "preamble"
  val Identity         = "identity"
  val ToolInstructions = "tool_instructions"
  val Preserve         = "preserve"

/** GitHub attachment type constants. */
object GitHubAttachment:
  val GitHubCommit      = "GitHubCommit"
  val GitHubRepository  = "GitHubRepository"
  val GitHubPullRequest = "GitHubPullRequest"
  val GitHubIssue       = "GitHubIssue"
