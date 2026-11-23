// Copyright (c) Microsoft Corporation. All rights reserved.

package com.github.copilot

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/**
 * Represents a single conversation session with the Copilot CLI.
 *
 * A session maintains conversation state, handles events, and manages tool execution.
 * Sessions are created via [[CopilotClient.createSession]] or resumed via
 * [[CopilotClient.resumeSession]].
 *
 * == Usage ==
 * {{{
 * val session = Await.result(client.createSession(), 30.seconds)
 *
 * // Subscribe to events
 * val unsub = session.on { event =>
 *   if event.`type` == SessionEventType.AssistantMessage then
 *     println(s"Assistant: ${event.data}")
 * }
 *
 * // Send and wait
 * val response = Await.result(
 *   session.sendAndWait(MessageOptions(prompt = "Hello!")),
 *   60.seconds
 * )
 *
 * unsub() // unsubscribe
 * Await.result(session.destroy(), 10.seconds)
 * }}}
 *
 * @param sessionId     The unique session identifier
 * @param connection    The JSON-RPC client for server communication
 * @param workspacePath Path to the workspace directory (when infinite sessions enabled)
 */
class CopilotSession(
  val sessionId: String,
  private val connection: JsonRpcClient,
  val workspacePath: Option[String] = None
)(using ec: ExecutionContext):

  // Event handlers
  private val eventHandlers = java.util.Collections.newSetFromMap(
    new ConcurrentHashMap[SessionEvent => Unit, java.lang.Boolean]()
  )
  private val typedEventHandlers = new ConcurrentHashMap[String, mutable.Set[SessionEvent => Unit]]()

  // Tool handlers
  private val toolHandlers = new ConcurrentHashMap[String, ToolHandler]()

  // Permission, user input, and hooks handlers
  @volatile private var permissionHandler: Option[PermissionHandler] = None
  @volatile private var userInputHandler: Option[UserInputHandler] = None
  @volatile private var hooks: Option[SessionHooks] = None
  @volatile private var exitPlanModeHandler: Option[ExitPlanModeHandler] = None
  @volatile private var shellAuthenticationHandler: Option[ShellAuthenticationHandler] = None
  @volatile private var memoryStoreHandler: Option[MemoryStoreHandler] = None
  @volatile private var onDestroyed: Option[() => Unit] = None

  /** In-flight shell authentication answers keyed by request ID, completed early by `shellAuthentication.cancel`. */
  private val pendingShellAuthentications = new ConcurrentHashMap[String, Promise[Json]]()
  private val shellAuthInputKinds = Set("secret", "text", "confirm")
  private def shellAuthenticationCancel: Json = Json.obj("action" -> "cancel".asJson)

  /** In-flight external tool calls keyed by request ID; `external_tool.completed` completes the token so no answer is sent. */
  private val pendingExternalTools = new ConcurrentHashMap[String, Promise[Unit]]()

  // -------------------------------------------------------------------------
  // Sending Messages
  // -------------------------------------------------------------------------

  /**
   * Sends a message to the session.
   *
   * The message is enqueued for processing. Events are dispatched to registered handlers
   * as the assistant processes the message.
   *
   * @param options The message options
   * @return A Future with the message ID
   */
  def send(options: MessageOptions): Future[String] =
    val params = buildSendPayload(sessionId, options)

    connection.sendRequest("session.send", params).map { result =>
      result.hcursor.get[String]("messageId").getOrElse(
        throw new RuntimeException("session.send response missing messageId")
      )
    }

  /**
   * Sends a message and waits until the session becomes idle.
   *
   * This is a convenience method that combines [[send]] with waiting for the
   * `session.idle` event. Events are still delivered to registered handlers while waiting.
   *
   * @param options The message options
   * @param timeout Maximum time to wait (default: 60 seconds)
   * @return A Future with the last assistant.message event, or None if no assistant message was received
   */
  def sendAndWait(
    options: MessageOptions,
    timeout: FiniteDuration = 60.seconds
  ): Future[Option[SessionEvent]] =
    val idlePromise = Promise[Option[SessionEvent]]()
    @volatile var lastAssistantMessage: Option[SessionEvent] = None

    // Register event handler BEFORE calling send to avoid race condition
    val unsubscribe = on { event =>
      event.`type` match
        case SessionEventType.AssistantMessage =>
          lastAssistantMessage = Some(event)
        case SessionEventType.SessionIdle =>
          idlePromise.trySuccess(lastAssistantMessage)
        case SessionEventType.SessionError =>
          val errorMsg = event.data.hcursor.get[String]("message").getOrElse("Unknown error")
          val errorStack = event.data.hcursor.get[String]("stack").toOption
          val ex = new RuntimeException(errorMsg)
          errorStack.foreach(s => ex.setStackTrace(Array.empty)) // stack from server
          idlePromise.tryFailure(ex)
        case _ => // ignore other events
    }

    // Create timeout Future
    val timeoutFuture = Future:
      Thread.sleep(timeout.toMillis)
      throw new java.util.concurrent.TimeoutException(
        s"Timeout after ${timeout.toMillis}ms waiting for session.idle"
      )

    val resultFuture = send(options).flatMap { _ =>
      Future.firstCompletedOf(Seq(idlePromise.future, timeoutFuture))
    }

    // Clean up on completion
    resultFuture.andThen { case _ => unsubscribe() }

  /**
   * Sends a message with `responseSchema` set to `schema`, waits via [[sendAndWait]], and returns
   * the final assistant content parsed as JSON. The caller's options are not modified.
   *
   * @param options The message options
   * @param schema  JSON Schema the response must satisfy
   * @param timeout Maximum time to wait (default: 60 seconds)
   * @return A Future with the structured response parsed as JSON; fails when no content is returned
   */
  def sendAndWaitStructured(
    options: MessageOptions,
    schema: JsonObject,
    timeout: FiniteDuration = 60.seconds
  ): Future[Json] =
    sendAndWait(options.copy(responseSchema = Some(schema)), timeout).flatMap { response =>
      response.flatMap(_.data.hcursor.get[String]("content").toOption).filter(_.trim.nonEmpty) match
        case None =>
          Future.failed(new IllegalStateException(
            "The requested run completed without a structured assistant response."
          ))
        case Some(content) =>
          parse(content) match
            case Right(json) => Future.successful(json)
            case Left(err) =>
              Future.failed(new RuntimeException(s"Structured response is not valid JSON: ${err.message}", err))
    }

  // -------------------------------------------------------------------------
  // Event Subscriptions
  // -------------------------------------------------------------------------

  /**
   * Subscribes to all events from this session.
   *
   * @param handler Callback for all session events
   * @return An unsubscribe function
   */
  def on(handler: SessionEvent => Unit): () => Unit =
    eventHandlers.add(handler)
    () => eventHandlers.remove(handler)

  /**
   * Subscribes to a specific event type.
   *
   * @param eventType The event type string (e.g., SessionEventType.AssistantMessage)
   * @param handler   Callback for matching events
   * @return An unsubscribe function
   */
  def on(eventType: String, handler: SessionEvent => Unit): () => Unit =
    typedEventHandlers
      .computeIfAbsent(eventType, _ => mutable.Set.empty)
      .addOne(handler)
    () =>
      val handlers = typedEventHandlers.get(eventType)
      if handlers != null then handlers.remove(handler)

  // -------------------------------------------------------------------------
  // Message History
  // -------------------------------------------------------------------------

  /**
   * Retrieves all events and messages from this session's history.
   *
   * @return A Future with all session events
   */
  def getMessages(): Future[List[SessionEvent]] =
    connection.sendRequest("session.getMessages", Json.obj("sessionId" -> sessionId.asJson)).flatMap { result =>
      result.hcursor.get[List[SessionEvent]]("events") match
        case Right(events) => Future.successful(events)
        case Left(err) => Future.failed(new RuntimeException(s"Failed to decode messages: $err"))
    }

  /**
   * Retrieves metadata for this session.
   *
   * @return A Future with the session metadata
   */
  def getMetadata(): Future[Json] =
    connection.sendRequest("session.getMetadata", Json.obj("sessionId" -> sessionId.asJson))

  // -------------------------------------------------------------------------
  // Session Lifecycle
  // -------------------------------------------------------------------------

  /**
   * Detaches this session and releases all associated resources.
   *
   * Sends `session.detach` (falling back to `session.destroy` on older runtimes); on-disk session
   * state is preserved. After calling this, the session can no longer be used. To continue the
   * conversation, use [[CopilotClient.resumeSession]] with the session ID.
   */
  def destroy(): Future[Unit] =
    val params = Json.obj("sessionId" -> sessionId.asJson)
    connection.sendRequest("session.detach", params).recoverWith {
      case ex: JsonRpcException if isMethodNotFound(ex, "session.detach") =>
        connection.sendRequest("session.destroy", params)
    }.map { result =>
      if result.hcursor.get[Boolean]("success").toOption.contains(false) then
        val error = result.hcursor.get[String]("error").getOrElse("Unknown error")
        throw new RuntimeException(s"Failed to disconnect session $sessionId: $error")
      cancelPendingShellAuthentications()
      pendingExternalTools.forEach((_, token) => { token.trySuccess(()); () })
      pendingExternalTools.clear()
      eventHandlers.clear()
      typedEventHandlers.clear()
      toolHandlers.clear()
      permissionHandler = None
      userInputHandler = None
      hooks = None
      exitPlanModeHandler = None
      shellAuthenticationHandler = None
      memoryStoreHandler = None
      val callback = onDestroyed
      onDestroyed = None
      callback.foreach(f => f())
    }

  /**
   * Aborts the currently processing message.
   *
   * The session remains valid and can be used for new messages.
   */
  def abort(): Future[Unit] =
    connection.sendRequest("session.abort", Json.obj("sessionId" -> sessionId.asJson)).map(_ => ())

  /**
   * Changes the Auto routing preference without changing the selected model (experimental).
   * The runtime commits it on a later turn that uses the `auto` model.
   *
   * @param autoTier Routing preference (see [[AutoTier]]), or None to return to default routing
   * @return A Future with the acknowledgement (`status`, `effectiveAutoTier`, `pendingAutoTier`,
   *         `activatingAutoTier`, `supersededAutoTier`)
   */
  def setAutoTier(autoTier: Option[String]): Future[Json] =
    connection.sendRequest("session.model.switchAutoTier", Json.obj(
      "sessionId" -> sessionId.asJson,
      "autoTier" -> autoTier.asJson
    ))

  /** Returns the session's effective enterprise managed settings snapshot. */
  def getManagedSettings(): Future[Json] =
    connection.sendRequest("session.managedSettings.get", Json.obj("sessionId" -> sessionId.asJson))

  /** Runs a fresh Policy Helper evaluation and returns the resulting managed settings snapshot. */
  def recheckPolicyHelper(): Future[Json] =
    connection.sendRequest("session.managedSettings.recheckPolicyHelper", Json.obj("sessionId" -> sessionId.asJson))

  /**
   * Changes the session's permission mode. A refused change reports `success == false` with the
   * authoritative mode and, when known, a `reason` such as [[PermissionModeRefusalReason.ManagedPolicy]].
   *
   * @param mode The permission mode (see [[PermissionMode]])
   */
  def setPermissionMode(mode: String): Future[PermissionModeResult] =
    connection.sendRequest("session.permissions.setMode", Json.obj(
      "sessionId" -> sessionId.asJson,
      "mode" -> mode.asJson
    )).map { result =>
      val c = result.hcursor
      PermissionModeResult(
        success = c.get[Boolean]("success").getOrElse(false),
        mode = c.get[String]("mode").toOption,
        reason = c.get[String]("reason").toOption
      )
    }

  // -------------------------------------------------------------------------
  // Tool Registration (internal)
  // -------------------------------------------------------------------------

  /** Registers tool handlers. Called internally by CopilotClient. */
  private[copilot] def registerTools(tools: List[Tool]): Unit =
    toolHandlers.clear()
    tools.foreach(t => toolHandlers.put(t.name, t.handler))

  /** Gets a registered tool handler by name. */
  private[copilot] def getToolHandler(name: String): Option[ToolHandler] =
    Option(toolHandlers.get(name))

  /** Registers a permission handler. */
  private[copilot] def registerPermissionHandler(handler: PermissionHandler): Unit =
    permissionHandler = Some(handler)

  /** Registers a user input handler. */
  private[copilot] def registerUserInputHandler(handler: UserInputHandler): Unit =
    userInputHandler = Some(handler)

  /** Registers session hooks. */
  private[copilot] def registerHooks(h: SessionHooks): Unit =
    hooks = Some(h)

  /** Registers an exit plan mode handler. */
  private[copilot] def registerExitPlanModeHandler(handler: ExitPlanModeHandler): Unit =
    exitPlanModeHandler = Some(handler)

  /** Registers a callback run once after this session is successfully destroyed. */
  private[copilot] def registerOnDestroyed(callback: () => Unit): Unit =
    onDestroyed = Some(callback)

  /** Registers a shell authentication handler. */
  private[copilot] def registerShellAuthenticationHandler(handler: ShellAuthenticationHandler): Unit =
    shellAuthenticationHandler = Some(handler)

  /** Registers the memory store callback. */
  private[copilot] def registerMemoryStore(handler: MemoryStoreHandler): Unit =
    memoryStoreHandler = Some(handler)

  /**
   * Answers a `shellAuthentication.request`. A `shellAuthentication.cancel` completes the answer
   * with `cancel` without waiting for the handler; invalid requests, missing handlers, and failures
   * also answer `cancel`. Never logs the answer.
   */
  private[copilot] def handleShellAuthenticationRequest(params: Json): Future[Json] =
    val c = params.hcursor
    val prepared = for
      handler   <- shellAuthenticationHandler
      requestId <- c.get[String]("requestId").toOption
      program   <- c.get[String]("program").toOption
      prompt    <- c.get[String]("prompt").toOption
      inputKind <- c.get[String]("inputKind").toOption
      if shellAuthInputKinds.contains(inputKind)
    yield (handler, ShellAuthenticationRequest(
      requestId = requestId,
      program = program,
      prompt = prompt,
      inputKind = inputKind,
      command = c.get[String]("command").getOrElse(""),
      cwd = c.get[String]("cwd").getOrElse("")
    ))
    prepared match
      case None => Future.successful(shellAuthenticationCancel)
      case Some((handler, request)) =>
        val answer = Promise[Json]()
        if pendingShellAuthentications.putIfAbsent(request.requestId, answer) != null then
          Future.successful(shellAuthenticationCancel)
        else
          val handled = Future.delegate(handler(request, sessionId)).map {
            case ShellAuthenticationResult.Accept(value) =>
              Json.obj("action" -> "accept".asJson, "value" -> value.asJson)
            case _ => shellAuthenticationCancel
          }.recover { case _ => shellAuthenticationCancel } // the error could contain the answer
          answer.completeWith(handled)
          answer.future.andThen { case _ => pendingShellAuthentications.remove(request.requestId, answer) }

  /** Cancels a pending shell authentication prompt (`shellAuthentication.cancel`). */
  private[copilot] def cancelShellAuthentication(requestId: String): Unit =
    val pending = pendingShellAuthentications.remove(requestId)
    if pending != null then pending.trySuccess(shellAuthenticationCancel)

  private def cancelPendingShellAuthentications(): Unit =
    pendingShellAuthentications.keySet().asScala.toList.foreach(cancelShellAuthentication)

  /** Invokes the memory store callback for a `memoryStore.store` request; answers with a JSON null. */
  private[copilot] def handleMemoryStore(params: Json): Future[Json] =
    memoryStoreHandler match
      case None => Future.failed(new IllegalStateException("No memory store is registered"))
      case Some(handler) =>
        val c = params.hcursor
        val memory = StoredMemory(
          sessionId = c.get[String]("sessionId").getOrElse(sessionId),
          subject = c.get[String]("subject").getOrElse(""),
          fact = c.get[String]("fact").getOrElse(""),
          citations = c.get[String]("citations").getOrElse(""),
          reason = c.get[String]("reason").getOrElse("")
        )
        Future.delegate(handler(memory)).map(_ => Json.Null).recoverWith { case _ =>
          Future.failed(new RuntimeException("Memory store callback failed"))
        }

  // -------------------------------------------------------------------------
  // Event Dispatch (internal)
  // -------------------------------------------------------------------------

  /** Dispatches an event to all registered handlers. Called by CopilotClient. */
  private[copilot] def dispatchEvent(event: SessionEvent): Unit =
    handleBroadcastEvent(event)
    // Typed handlers
    val typedSet = typedEventHandlers.get(event.`type`)
    if typedSet != null then
      typedSet.foreach { handler =>
        Try(handler(event)).failed.foreach(_ => ()) // ignore handler errors
      }
    // Wildcard handlers
    eventHandlers.forEach { handler =>
      Try(handler(event)).failed.foreach(_ => ()) // ignore handler errors
    }

  /**
   * Services the protocol 4 broadcast requests: `external_tool.requested` runs the registered tool
   * handler and answers via `session.tools.handlePendingToolCall` (`external_tool.completed`
   * cancels the answer), and `permission.requested` runs the permission handler and answers via
   * `session.permissions.handlePendingPermissionRequest`.
   */
  private def handleBroadcastEvent(event: SessionEvent): Unit =
    val c = event.data.hcursor
    event.`type` match
      case "external_tool.requested" =>
        for
          requestId <- c.get[String]("requestId").toOption
          toolName <- c.get[String]("toolName").toOption
          if !c.get[Boolean]("hasHandler").toOption.contains(false)
          handler <- Option(toolHandlers.get(toolName))
        do
          val token = Promise[Unit]()
          if pendingExternalTools.putIfAbsent(requestId, token) == null then
            val toolCallId = c.get[String]("toolCallId").getOrElse("")
            val arguments = c.get[Json]("arguments").getOrElse(Json.obj())
            runExternalTool(requestId, toolCallId, toolName, arguments, handler, token)
      case "external_tool.completed" =>
        c.get[String]("requestId").toOption.foreach { requestId =>
          Option(pendingExternalTools.remove(requestId)).foreach(_.trySuccess(()))
        }
      case "permission.requested" =>
        for
          requestId <- c.get[String]("requestId").toOption
          if !c.get[Boolean]("resolvedByHook").toOption.contains(true)
          if permissionHandler.isDefined
        do respondToPermissionRequest(requestId, c.get[Json]("permissionRequest").getOrElse(Json.obj()))
      case _ => ()

  private def runExternalTool(
    requestId: String,
    toolCallId: String,
    toolName: String,
    arguments: Json,
    handler: ToolHandler,
    token: Promise[Unit]
  ): Unit =
    val invocation = ToolInvocation(sessionId, toolCallId, toolName, arguments)
    Future.delegate(handler(arguments, invocation)).transformWith { outcome =>
      // Claim the request; it fails once the call was cancelled or completed elsewhere.
      if token.isCompleted || !pendingExternalTools.remove(requestId, token) then Future.unit
      else
        val answer = outcome match
          case Success(result) => Json.obj("result" -> result.asJson.dropNullValues)
          case Failure(ex)     => Json.obj("error" -> Option(ex.getMessage).getOrElse(ex.toString).asJson)
        val body = Json.obj("sessionId" -> sessionId.asJson, "requestId" -> requestId.asJson).deepMerge(answer)
        connection.sendRequest("session.tools.handlePendingToolCall", body).map(_ => ())
    }.recover { case _ => () } // Connection lost or RPC error: nothing more to report.
    ()

  private def respondToPermissionRequest(requestId: String, permissionRequest: Json): Unit =
    handlePermissionRequest(permissionRequest)
      .recover { case _ => PermissionRequestResult(kind = "user-not-available") }
      .flatMap { result =>
        if result.kind == "no-result" then Future.unit
        else
          val decision = Json.obj("kind" -> result.kind.asJson)
            .deepMerge(result.rules.fold(Json.obj())(rules => Json.obj("rules" -> rules.asJson)))
          val body = Json.obj("sessionId" -> sessionId.asJson, "requestId" -> requestId.asJson, "result" -> decision)
            .deepMerge(result.decisionContext.fold(Json.obj())(ctx => Json.obj("decisionContext" -> ctx.asJson)))
          connection.sendRequest("session.permissions.handlePendingPermissionRequest", body).map(_ => ())
      }
      .recover { case _ => () } // Connection lost or RPC error: nothing more to report.
    ()

  // -------------------------------------------------------------------------
  // Request Handlers (internal, called by CopilotClient)
  // -------------------------------------------------------------------------

  /** Handles a permission request from the server. */
  private[copilot] def handlePermissionRequest(requestJson: Json): Future[PermissionRequestResult] =
    permissionHandler match
      case None =>
        Future.successful(PermissionRequestResult(
          kind = PermissionRequestResult.DeniedNoApprovalRule
        ))
      case Some(handler) =>
        requestJson.as[PermissionRequest] match
          case Left(_) =>
            Future.successful(PermissionRequestResult(
              kind = PermissionRequestResult.DeniedNoApprovalRule
            ))
          case Right(request) =>
            handler(request, PermissionInvocation(sessionId)).recover {
              case _ => PermissionRequestResult(
                kind = PermissionRequestResult.DeniedNoApprovalRule
              )
            }

  /** Handles a user input request from the server. */
  private[copilot] def handleUserInputRequest(request: UserInputRequest): Future[Json] =
    userInputHandler match
      case None =>
        Future.failed(new RuntimeException("User input requested but no handler registered"))
      case Some(handler) =>
        handler(request, UserInputInvocation(sessionId)).map(_.asJson)

  /** Handles a hooks invocation from the server. */
  private[copilot] def handleHooksInvoke(hookType: String, input: Json): Future[Option[Json]] =
    hooks match
      case None => Future.successful(None)
      case Some(h) =>
        val invocation = HookInvocation(sessionId)
        hookType match
          case "preToolUse" =>
            h.onPreToolUse match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[PreToolUseHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "postToolUse" =>
            h.onPostToolUse match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[PostToolUseHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "userPromptSubmitted" =>
            h.onUserPromptSubmitted match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[UserPromptSubmittedHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "userPromptTransformed" =>
            h.onUserPromptTransformed match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[UserPromptSubmittedHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "sessionStart" =>
            h.onSessionStart match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[SessionStartHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "sessionEnd" =>
            h.onSessionEnd match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[SessionEndHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "errorOccurred" =>
            h.onErrorOccurred match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[ErrorOccurredHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "subagentStart" =>
            h.onSubagentStart match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[SubagentStartHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case "subagentStop" =>
            h.onSubagentStop match
              case None => Future.successful(None)
              case Some(handler) =>
                input.as[SubagentStopHookInput] match
                  case Left(_) => Future.successful(None)
                  case Right(parsed) =>
                    handler(parsed, invocation).map(_.map(_.asJson)).recover { case _ => None }

          case _ => Future.successful(None)

  /** Handles an exit plan mode request from the server. */
  private[copilot] def handleExitPlanModeRequest(request: ExitPlanModeRequest): Future[ExitPlanModeResponse] =
    exitPlanModeHandler match
      case None => Future.successful(ExitPlanModeResponse(approved = true))
      case Some(handler) =>
        handler(request).recover { case _ => ExitPlanModeResponse(approved = true) }
