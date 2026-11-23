/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot

import kotlinx.coroutines.*
import kotlinx.serialization.json.*
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList

/**
 * Represents a single conversation session with the Copilot CLI.
 *
 * A session maintains conversation state, handles events, and manages tool execution.
 * Sessions are created via [CopilotClient.createSession] or resumed via
 * [CopilotClient.resumeSession].
 *
 * Example:
 * ```kotlin
 * val session = client.createSession(SessionConfig(model = "gpt-4"))
 *
 * session.on { event ->
 *     if (event.type == "assistant.message") {
 *         println(event.data)
 *     }
 * }
 *
 * val response = session.sendAndWait(MessageOptions(prompt = "Hello, world!"))
 * println(response?.data)
 *
 * session.destroy()
 * ```
 */
class CopilotSession internal constructor(
    /** The unique identifier for this session. */
    val sessionId: String,
    private val client: JsonRpcClient,
    private val scope: CoroutineScope,
    /**
     * Path to the session workspace directory when infinite sessions are enabled.
     * Contains checkpoints/, plan.md, and files/ subdirectories.
     * Null if infinite sessions are disabled.
     */
    val workspacePath: String? = null
) {
    private val json = Json {
        ignoreUnknownKeys = true
        encodeDefaults = false
    }

    private val eventHandlers = CopyOnWriteArrayList<SessionEventHandler>()
    private val typedEventHandlers = ConcurrentHashMap<String, CopyOnWriteArrayList<SessionEventHandler>>()
    private val toolHandlers = ConcurrentHashMap<String, ToolHandler>()

    @Volatile
    private var permissionHandler: PermissionHandler? = null

    @Volatile
    private var userInputHandler: UserInputHandler? = null

    @Volatile
    private var hooks: SessionHooks? = null

    @Volatile
    private var elicitationHandler: ElicitationHandler? = null

    @Volatile
    private var exitPlanModeHandler: ExitPlanModeHandler? = null

    @Volatile
    private var shellAuthenticationHandler: ShellAuthenticationHandler? = null

    @Volatile
    private var memoryStoreHandler: MemoryStoreHandler? = null

    /** In-flight shell authentication prompts keyed by request ID, cancelled on `shellAuthentication.cancel`. */
    private val pendingShellAuthentications = ConcurrentHashMap<String, Deferred<ShellAuthenticationResult?>>()

    /** In-flight external tool calls keyed by request ID, cancelled on `external_tool.completed`. */
    private val pendingExternalTools = ConcurrentHashMap<String, Job>()

    @Volatile
    private var onDestroyed: (() -> Unit)? = null

    /**
     * Sends a message to this session.
     *
     * The message is processed asynchronously. Subscribe to events via [on]
     * to receive streaming responses and other session events.
     *
     * @param options The message options including the prompt and optional attachments
     * @return The message ID
     */
    suspend fun send(options: MessageOptions): String {
        val params = buildSendPayload(sessionId, options, json)
        val response = client.request("session.send", params)
        return response.jsonObject["messageId"]!!.jsonPrimitive.content
    }

    /**
     * Sends a message and waits until the session becomes idle.
     *
     * This is a convenience method that combines [send] with waiting for
     * the `session.idle` event. Events are still delivered to handlers
     * registered via [on] while waiting.
     *
     * @param options The message options
     * @param timeoutMs Timeout in milliseconds (default: 60000). Controls how long to wait.
     * @return The final assistant message event, or null if no assistant message was received
     * @throws RuntimeException If the timeout is reached or a session error occurs
     */
    suspend fun sendAndWait(
        options: MessageOptions,
        timeoutMs: Long = 60_000L
    ): SessionEvent? {
        val idleDeferred = CompletableDeferred<Unit>()
        var lastAssistantMessage: SessionEvent? = null
        var sessionError: Exception? = null

        // Register event handler BEFORE calling send to avoid race conditions
        val unsubscribe = on { event ->
            when (event.type) {
                "assistant.message" -> lastAssistantMessage = event
                "session.idle" -> idleDeferred.complete(Unit)
                "session.error" -> {
                    val msg = event.data?.get("message")?.jsonPrimitive?.contentOrNull ?: "Unknown error"
                    sessionError = RuntimeException(msg)
                    idleDeferred.complete(Unit)
                }
            }
        }

        try {
            send(options)

            withTimeout(timeoutMs) {
                idleDeferred.await()
            }

            sessionError?.let { throw it }
            return lastAssistantMessage
        } finally {
            unsubscribe()
        }
    }

    /**
     * Sends a message with [MessageOptions.responseSchema] set to [schema], waits via [sendAndWait],
     * and returns the final assistant content parsed as JSON. The caller's options are not modified.
     *
     * @param options The message options
     * @param schema JSON Schema the response must satisfy
     * @param timeoutMs Timeout in milliseconds (default: 60000)
     * @return The structured response parsed as a [JsonElement]
     * @throws IllegalStateException If the run completes without assistant content
     */
    suspend fun sendAndWaitStructured(
        options: MessageOptions,
        schema: Map<String, Any?>,
        timeoutMs: Long = 60_000L
    ): JsonElement {
        val response = sendAndWait(options.copy(responseSchema = schema), timeoutMs)
        val content = response?.data?.get("content")?.jsonPrimitive?.contentOrNull
        if (content.isNullOrBlank()) {
            throw IllegalStateException("The requested run completed without a structured assistant response.")
        }
        return json.parseToJsonElement(content)
    }

    /**
     * Sends [prompt] requesting structured output and returns the parsed JSON response.
     *
     * @param prompt The message prompt
     * @param schema JSON Schema the response must satisfy
     * @param timeoutMs Timeout in milliseconds (default: 60000)
     * @return The structured response parsed as a [JsonElement]
     */
    suspend fun sendAndWaitStructured(
        prompt: String,
        schema: Map<String, Any?>,
        timeoutMs: Long = 60_000L
    ): JsonElement = sendAndWaitStructured(MessageOptions(prompt = prompt), schema, timeoutMs)

    /**
     * Subscribes to all events from this session.
     *
     * @param handler A callback function that receives all session events
     * @return A function that, when called, unsubscribes the handler
     */
    fun on(handler: SessionEventHandler): () -> Unit {
        eventHandlers.add(handler)
        return { eventHandlers.remove(handler) }
    }

    /**
     * Subscribes to a specific event type from this session.
     *
     * @param eventType The event type to listen for (e.g., "assistant.message", "session.idle")
     * @param handler A callback function that receives events of the specified type
     * @return A function that, when called, unsubscribes the handler
     */
    fun on(eventType: String, handler: SessionEventHandler): () -> Unit {
        typedEventHandlers.computeIfAbsent(eventType) { CopyOnWriteArrayList() }.add(handler)
        return {
            typedEventHandlers[eventType]?.remove(handler)
        }
    }

    /**
     * Retrieves metadata for this session.
     *
     * @return The session metadata as a JsonElement
     */
    suspend fun getMetadata(): JsonElement {
        return client.request("session.getMetadata", buildJsonObject {
            put("sessionId", sessionId)
        })
    }

    /**
     * Retrieves all events and messages from this session's history.
     */
    suspend fun getMessages(): List<SessionEvent> {
        val response = client.request("session.getMessages", buildJsonObject {
            put("sessionId", sessionId)
        })
        val events = response.jsonObject["events"]?.jsonArray ?: return emptyList()
        return events.map { eventElement ->
            val obj = eventElement.jsonObject
            SessionEvent(
                type = obj["type"]?.jsonPrimitive?.content ?: "",
                data = obj["data"]?.jsonObject
            )
        }
    }

    /**
     * Detaches this session and releases all associated resources.
     *
     * Sends `session.detach` (falling back to `session.destroy` on older runtimes); on-disk session
     * state is preserved. After calling this method, the session can no longer be used.
     * To continue the conversation, use [CopilotClient.resumeSession].
     */
    suspend fun destroy() {
        val params = buildJsonObject {
            put("sessionId", sessionId)
        }
        try {
            val result = client.request("session.detach", params)
            val success = ((result as? JsonObject)?.get("success") as? JsonPrimitive)?.booleanOrNull
            if (success == false) {
                val error = ((result as? JsonObject)?.get("error") as? JsonPrimitive)?.contentOrNull ?: "Unknown error"
                throw RuntimeException("Failed to disconnect session $sessionId: $error")
            }
        } catch (e: JsonRpcException) {
            if (!isMethodNotFound(e, "session.detach")) throw e
            client.request("session.destroy", params)
        }
        cancelPendingShellAuthentications()
        pendingExternalTools.keys.toList().forEach { pendingExternalTools.remove(it)?.cancel() }
        eventHandlers.clear()
        typedEventHandlers.clear()
        toolHandlers.clear()
        permissionHandler = null
        userInputHandler = null
        elicitationHandler = null
        exitPlanModeHandler = null
        shellAuthenticationHandler = null
        memoryStoreHandler = null
        hooks = null
        val callback = onDestroyed
        onDestroyed = null
        callback?.invoke()
    }

    /**
     * Aborts the currently processing message in this session.
     *
     * The session remains valid and can continue to be used for new messages.
     */
    suspend fun abort() {
        client.request("session.abort", buildJsonObject {
            put("sessionId", sessionId)
        })
    }

    /**
     * Changes the Auto routing preference without changing the selected model (experimental).
     * The runtime commits it on a later turn that uses the `auto` model.
     *
     * @param autoTier Routing preference (see [AutoTier]), or null to return to default routing
     * @return The acknowledgement (`status`, `effectiveAutoTier`, `pendingAutoTier`,
     *   `activatingAutoTier`, `supersededAutoTier`)
     */
    suspend fun setAutoTier(autoTier: String?): JsonObject {
        val result = client.request("session.model.switchAutoTier", buildJsonObject {
            put("sessionId", sessionId)
            put("autoTier", autoTier)
        })
        return result as? JsonObject ?: JsonObject(emptyMap())
    }

    /**
     * Returns the session's effective enterprise managed settings snapshot.
     */
    suspend fun getManagedSettings(): JsonObject {
        val result = client.request("session.managedSettings.get", buildJsonObject {
            put("sessionId", sessionId)
        })
        return result as? JsonObject ?: JsonObject(emptyMap())
    }

    /**
     * Runs a fresh Policy Helper evaluation and returns the resulting managed settings snapshot.
     */
    suspend fun recheckPolicyHelper(): JsonObject {
        val result = client.request("session.managedSettings.recheckPolicyHelper", buildJsonObject {
            put("sessionId", sessionId)
        })
        return result as? JsonObject ?: JsonObject(emptyMap())
    }

    /**
     * Changes the session's permission mode. A refused change reports `success == false` with the
     * authoritative mode and, when known, a `reason` such as [PermissionModeRefusalReason.MANAGED_POLICY].
     *
     * @param mode The permission mode (see [PermissionMode])
     */
    suspend fun setPermissionMode(mode: String): PermissionModeResult {
        val result = client.request("session.permissions.setMode", buildJsonObject {
            put("sessionId", sessionId)
            put("mode", mode)
        }) as? JsonObject ?: JsonObject(emptyMap())
        return PermissionModeResult(
            success = (result["success"] as? JsonPrimitive)?.booleanOrNull ?: false,
            mode = (result["mode"] as? JsonPrimitive)?.contentOrNull,
            reason = (result["reason"] as? JsonPrimitive)?.contentOrNull
        )
    }

    // ========================================================================
    // Internal Methods
    // ========================================================================

    /**
     * Registers custom tool handlers for this session.
     * @suppress
     */
    internal fun registerTools(tools: List<Tool>?) {
        toolHandlers.clear()
        tools?.forEach { tool ->
            toolHandlers[tool.name] = tool.handler
        }
    }

    /**
     * Retrieves a registered tool handler by name.
     * @suppress
     */
    internal fun getToolHandler(name: String): ToolHandler? = toolHandlers[name]

    /**
     * Registers a handler for permission requests.
     * @suppress
     */
    internal fun registerPermissionHandler(handler: PermissionHandler?) {
        this.permissionHandler = handler
    }

    /**
     * Registers a user input handler.
     * @suppress
     */
    internal fun registerUserInputHandler(handler: UserInputHandler?) {
        this.userInputHandler = handler
    }

    /**
     * Registers hook handlers.
     * @suppress
     */
    internal fun registerHooks(hooks: SessionHooks?) {
        this.hooks = hooks
    }

    /**
     * Registers an elicitation handler.
     * @suppress
     */
    internal fun registerElicitationHandler(handler: ElicitationHandler?) {
        this.elicitationHandler = handler
    }

    /**
     * Registers an exit plan mode handler.
     * @suppress
     */
    internal fun registerExitPlanModeHandler(handler: ExitPlanModeHandler?) {
        this.exitPlanModeHandler = handler
    }

    /**
     * Registers a callback run once after this session is successfully destroyed.
     * @suppress
     */
    internal fun registerOnDestroyed(callback: () -> Unit) {
        this.onDestroyed = callback
    }

    /**
     * Registers a shell authentication handler.
     * @suppress
     */
    internal fun registerShellAuthenticationHandler(handler: ShellAuthenticationHandler?) {
        this.shellAuthenticationHandler = handler
    }

    /**
     * Registers the memory store callback.
     * @suppress
     */
    internal fun registerMemoryStore(handler: MemoryStoreHandler?) {
        this.memoryStoreHandler = handler
    }

    /**
     * Answers a `shellAuthentication.request`: runs the handler as a cancellable child job so a
     * `shellAuthentication.cancel` can release it, and answers `cancel` for invalid requests,
     * missing handlers, failures, and cancelled prompts. Never logs the answer.
     * @suppress
     */
    internal suspend fun handleShellAuthenticationRequest(params: JsonObject): JsonObject {
        val handler = shellAuthenticationHandler ?: return shellAuthenticationCancel()
        val requestId = params.stringMember("requestId")
        val program = params.stringMember("program")
        val prompt = params.stringMember("prompt")
        val inputKind = params.stringMember("inputKind")
        if (requestId == null || program == null || prompt == null || inputKind == null ||
            inputKind !in SHELL_AUTH_INPUT_KINDS
        ) {
            return shellAuthenticationCancel()
        }
        val request = ShellAuthenticationRequest(
            requestId = requestId,
            program = program,
            prompt = prompt,
            inputKind = inputKind,
            command = params.stringMember("command") ?: "",
            cwd = params.stringMember("cwd") ?: ""
        )
        val pending = scope.async(start = CoroutineStart.LAZY) { handler(request, sessionId) }
        if (pendingShellAuthentications.putIfAbsent(requestId, pending) != null) {
            pending.cancel()
            return shellAuthenticationCancel()
        }
        return try {
            pending.start()
            when (val result = pending.await()) {
                is ShellAuthenticationResult.Accept -> buildJsonObject {
                    put("action", "accept")
                    put("value", result.value)
                }
                else -> shellAuthenticationCancel()
            }
        } catch (e: CancellationException) {
            // Rethrow only when this request itself is being cancelled; a cancelled prompt answers "cancel".
            currentCoroutineContext().ensureActive()
            shellAuthenticationCancel()
        } catch (_: Exception) {
            // The handler failed; its error could contain the answer, so it is not reported.
            shellAuthenticationCancel()
        } finally {
            pendingShellAuthentications.remove(requestId, pending)
        }
    }

    /**
     * Cancels a pending shell authentication prompt (`shellAuthentication.cancel`).
     * @suppress
     */
    internal fun cancelShellAuthentication(requestId: String) {
        pendingShellAuthentications.remove(requestId)?.cancel()
    }

    private fun cancelPendingShellAuthentications() {
        for (requestId in pendingShellAuthentications.keys.toList()) {
            cancelShellAuthentication(requestId)
        }
    }

    private fun shellAuthenticationCancel(): JsonObject = buildJsonObject { put("action", "cancel") }

    /**
     * Invokes the memory store callback for a `memoryStore.store` request.
     * @suppress
     */
    internal suspend fun handleMemoryStore(params: JsonObject) {
        val handler = memoryStoreHandler ?: throw IllegalStateException("No memory store is registered")
        val memory = StoredMemory(
            sessionId = params.stringMember("sessionId") ?: sessionId,
            subject = params.stringMember("subject") ?: "",
            fact = params.stringMember("fact") ?: "",
            citations = params.stringMember("citations") ?: "",
            reason = params.stringMember("reason") ?: ""
        )
        try {
            handler(memory)
        } catch (e: CancellationException) {
            throw e
        } catch (_: Exception) {
            throw RuntimeException("Memory store callback failed")
        }
    }

    private fun JsonObject.stringMember(key: String): String? =
        (this[key] as? JsonPrimitive)?.takeIf { it.isString }?.content

    private companion object {
        val SHELL_AUTH_INPUT_KINDS = setOf("secret", "text", "confirm")
    }

    /**
     * Dispatches a session event to all registered handlers.
     * @suppress
     */
    internal fun dispatchEvent(event: SessionEvent) {
        handleBroadcastEvent(event)

        // Typed handlers
        typedEventHandlers[event.type]?.forEach { handler ->
            try {
                handler(event)
            } catch (_: Exception) {
                // Ignore handler errors
            }
        }

        // Wildcard handlers
        eventHandlers.forEach { handler ->
            try {
                handler(event)
            } catch (_: Exception) {
                // Ignore handler errors
            }
        }
    }

    /**
     * Services the protocol 4 broadcast requests: `external_tool.requested` runs the registered
     * tool handler and answers via `session.tools.handlePendingToolCall` (`external_tool.completed`
     * cancels it), and `permission.requested` runs the permission handler and answers via
     * `session.permissions.handlePendingPermissionRequest`.
     */
    private fun handleBroadcastEvent(event: SessionEvent) {
        val data = event.data ?: return
        when (event.type) {
            "external_tool.requested" -> {
                val requestId = (data["requestId"] as? JsonPrimitive)?.contentOrNull ?: return
                val toolName = (data["toolName"] as? JsonPrimitive)?.contentOrNull ?: return
                if ((data["hasHandler"] as? JsonPrimitive)?.booleanOrNull == false) return
                val handler = toolHandlers[toolName] ?: return
                val toolCallId = (data["toolCallId"] as? JsonPrimitive)?.contentOrNull ?: ""
                val arguments = data["arguments"]
                val job = scope.launch(start = CoroutineStart.LAZY) {
                    runExternalTool(requestId, toolCallId, toolName, arguments, handler)
                }
                if (pendingExternalTools.putIfAbsent(requestId, job) == null) job.start() else job.cancel()
            }
            "external_tool.completed" -> {
                val requestId = (data["requestId"] as? JsonPrimitive)?.contentOrNull ?: return
                pendingExternalTools.remove(requestId)?.cancel()
            }
            "permission.requested" -> {
                val requestId = (data["requestId"] as? JsonPrimitive)?.contentOrNull ?: return
                if ((data["resolvedByHook"] as? JsonPrimitive)?.booleanOrNull == true) return
                if (permissionHandler == null) return
                val permissionRequest = data["permissionRequest"] as? JsonObject ?: JsonObject(emptyMap())
                scope.launch { respondToPermissionRequest(requestId, permissionRequest) }
            }
        }
    }

    private suspend fun runExternalTool(
        requestId: String,
        toolCallId: String,
        toolName: String,
        arguments: JsonElement?,
        handler: ToolHandler
    ) {
        val self = currentCoroutineContext().job
        val response = try {
            val result = handler(arguments, ToolInvocation(sessionId, toolCallId, toolName, arguments))
            // Claim the request; it fails once the call was cancelled or completed elsewhere.
            if (!pendingExternalTools.remove(requestId, self)) return
            buildJsonObject {
                put("sessionId", sessionId)
                put("requestId", requestId)
                put("result", externalToolResult(result))
            }
        } catch (e: CancellationException) {
            pendingExternalTools.remove(requestId, self)
            throw e
        } catch (e: Exception) {
            if (!pendingExternalTools.remove(requestId, self)) return
            buildJsonObject {
                put("sessionId", sessionId)
                put("requestId", requestId)
                put("error", e.message ?: e.toString())
            }
        }
        try {
            client.request("session.tools.handlePendingToolCall", response)
        } catch (e: CancellationException) {
            throw e
        } catch (_: Exception) {
            // Connection lost or RPC error: nothing more to report.
        }
    }

    private fun externalToolResult(result: Any?): JsonElement = when (result) {
        null -> JsonPrimitive("")
        is String -> JsonPrimitive(result)
        is JsonObject -> if (result.containsKey("textResultForLlm")) result else JsonPrimitive(result.toString())
        is ToolResultObject -> json.encodeToJsonElement(ToolResultObject.serializer(), result)
        is JsonElement -> JsonPrimitive(result.toString())
        else -> JsonPrimitive(result.toString())
    }

    private suspend fun respondToPermissionRequest(requestId: String, permissionRequest: JsonObject) {
        var decisionContext: Map<String, JsonElement>? = null
        val decision: JsonObject = try {
            val result = handlePermissionRequest(permissionRequest)
            if (result.kind == "no-result") return
            decisionContext = result.decisionContext
            buildJsonObject {
                put("kind", result.kind)
                result.rules?.let { put("rules", JsonArray(it)) }
            }
        } catch (e: CancellationException) {
            throw e
        } catch (_: Exception) {
            buildJsonObject { put("kind", "user-not-available") }
        }
        val context = decisionContext
        try {
            client.request("session.permissions.handlePendingPermissionRequest", buildJsonObject {
                put("sessionId", sessionId)
                put("requestId", requestId)
                put("result", decision)
                context?.let { put("decisionContext", JsonObject(it)) }
            })
        } catch (e: CancellationException) {
            throw e
        } catch (_: Exception) {
            // Connection lost or RPC error: nothing more to report.
        }
    }

    /**
     * Handles a permission request from the server.
     * @suppress
     */
    internal suspend fun handlePermissionRequest(request: JsonObject): PermissionRequestResult {
        val handler = permissionHandler
            ?: return PermissionRequestResult(kind = "denied-no-approval-rule-and-could-not-request-from-user")

        return try {
            val permRequest = PermissionRequest(
                kind = request["kind"]?.jsonPrimitive?.content ?: "",
                toolCallId = request["toolCallId"]?.jsonPrimitive?.contentOrNull
            )
            handler(permRequest, sessionId)
        } catch (_: Exception) {
            PermissionRequestResult(kind = "denied-no-approval-rule-and-could-not-request-from-user")
        }
    }

    /**
     * Handles a user input request from the server.
     * @suppress
     */
    internal suspend fun handleUserInputRequest(request: UserInputRequest): UserInputResponse {
        val handler = userInputHandler
            ?: throw RuntimeException("User input requested but no handler registered")
        return handler(request, sessionId)
    }

    /**
     * Handles an elicitation request from the server.
     * @suppress
     */
    internal suspend fun handleElicitationRequest(context: ElicitationContext): ElicitationResult {
        val handler = elicitationHandler
            ?: return ElicitationResult(action = "dismiss")
        return try {
            handler(context)
        } catch (_: Exception) {
            ElicitationResult(action = "dismiss")
        }
    }

    /**
     * Handles an exit plan mode request from the server.
     * @suppress
     */
    internal suspend fun handleExitPlanModeRequest(request: ExitPlanModeRequest): ExitPlanModeResponse {
        val handler = exitPlanModeHandler
            ?: return ExitPlanModeResponse(approved = true)
        return try {
            handler(request)
        } catch (_: Exception) {
            ExitPlanModeResponse(approved = true)
        }
    }

    /**
     * Handles a hooks invocation from the server.
     * @suppress
     */
    @Suppress("UNCHECKED_CAST")
    internal suspend fun handleHooksInvoke(hookType: String, input: JsonElement?): Map<String, Any?>? {
        val currentHooks = hooks ?: return null
        val handler = currentHooks.getHandler(hookType) ?: return null

        val inputMap: Map<String, Any?> = if (input is JsonObject) {
            input.toMap().mapValues { (_, v) -> jsonElementToAny(v) }
        } else {
            emptyMap()
        }

        return try {
            handler(inputMap, sessionId)
        } catch (_: Exception) {
            null
        }
    }

    /**
     * Converts a JsonElement to a Kotlin Any? value for hook input.
     */
    private fun jsonElementToAny(element: JsonElement): Any? = when (element) {
        is JsonNull -> null
        is JsonPrimitive -> when {
            element.isString -> element.content
            element.booleanOrNull != null -> element.boolean
            element.intOrNull != null -> element.int
            element.longOrNull != null -> element.long
            element.doubleOrNull != null -> element.double
            else -> element.content
        }
        is JsonArray -> element.map { jsonElementToAny(it) }
        is JsonObject -> element.toMap().mapValues { (_, v) -> jsonElementToAny(v) }
    }
}
