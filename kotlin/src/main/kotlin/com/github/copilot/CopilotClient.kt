/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot

import kotlinx.coroutines.*
import kotlinx.serialization.json.*
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.net.Socket
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.regex.Pattern

/**
 * Main client for interacting with the Copilot CLI.
 *
 * The CopilotClient manages the connection to the Copilot CLI server and provides
 * methods to create and manage conversation sessions. It can either spawn a CLI
 * server process or connect to an existing server.
 *
 * Example:
 * ```kotlin
 * val client = CopilotClient()
 * val session = client.createSession(SessionConfig(model = "gpt-4"))
 * session.on { event ->
 *     if (event.type == "assistant.message") {
 *         println(event.data)
 *     }
 * }
 * session.sendAndWait(MessageOptions(prompt = "Hello!"))
 * session.destroy()
 * client.stop()
 * ```
 */
class CopilotClient(
    private val options: CopilotClientOptions = CopilotClientOptions()
) {
    private val json = Json {
        ignoreUnknownKeys = true
        encodeDefaults = false
    }

    private var cliProcess: Process? = null
    private var rpcClient: JsonRpcClient? = null
    private var tcpSocket: Socket? = null
    private var actualPort: Int = 0
    private var actualHost: String = "localhost"
    private var state: ConnectionState = ConnectionState.DISCONNECTED
    private val sessions = ConcurrentHashMap<String, CopilotSession>()
    private val isExternalServer: Boolean
    private var modelsCache: List<ModelInfo>? = null
    private val modelsCacheLock = Any()
    private val lifecycleHandlers = CopyOnWriteArrayList<SessionLifecycleHandler>()
    private val typedLifecycleHandlers = ConcurrentHashMap<String, CopyOnWriteArrayList<SessionLifecycleHandler>>()

    // Coroutine scope for the client - uses SupervisorJob so child failures don't cancel everything
    private val clientScope = CoroutineScope(SupervisorJob() + Dispatchers.Default)

    // Resolved options
    private val resolvedCliPath: String
    private val resolvedCliArgs: List<String>
    private val resolvedCwd: String?
    private val resolvedPort: Int
    private val resolvedUseStdio: Boolean
    private val resolvedLogLevel: String
    private val resolvedAutoStart: Boolean
    private val resolvedAutoRestart: Boolean
    private val resolvedEnv: Map<String, String>?
    private val resolvedGithubToken: String?
    private val resolvedUseLoggedInUser: Boolean
    private val resolvedRequestHandler: CopilotRequestHandler?
    private val resolvedBearerTokenProvider: BearerTokenProvider?

    companion object {
        private val PORT_PATTERN = Pattern.compile("listening on port (\\d+)", Pattern.CASE_INSENSITIVE)
    }

    init {
        // Validate mutually exclusive options
        if (options.cliUrl != null && (options.useStdio == true || options.cliPath != null)) {
            throw IllegalArgumentException("cliUrl is mutually exclusive with useStdio and cliPath")
        }
        if (options.cliUrl != null && (options.githubToken != null || options.useLoggedInUser != null)) {
            throw IllegalArgumentException(
                "githubToken and useLoggedInUser cannot be used with cliUrl (external server manages its own auth)"
            )
        }

        // Parse cliUrl if provided
        if (options.cliUrl != null) {
            val (host, port) = parseCliUrl(options.cliUrl)
            actualHost = host
            actualPort = port
            isExternalServer = true
        } else {
            isExternalServer = false
        }

        // Resolve CLI path from environment or options
        val envCliPath = System.getenv("COPILOT_CLI_PATH")
        resolvedCliPath = options.cliPath ?: envCliPath ?: "copilot"
        resolvedCliArgs = options.cliArgs ?: emptyList()
        resolvedCwd = options.cwd
        resolvedPort = options.port
        resolvedUseStdio = if (options.cliUrl != null) false else (options.useStdio ?: true)
        resolvedLogLevel = options.logLevel
        resolvedAutoStart = options.autoStart
        resolvedAutoRestart = options.autoRestart
        resolvedEnv = options.env
        resolvedGithubToken = options.githubToken
        resolvedUseLoggedInUser = options.useLoggedInUser ?: (if (options.githubToken != null) false else true)
        // Upstream-sync client-side interceptors: consulted by the runtime transport when present.
        resolvedRequestHandler = options.requestHandler
        resolvedBearerTokenProvider = options.bearerTokenProvider
    }

    /**
     * Gets the current connection state.
     */
    fun getState(): ConnectionState = state

    /**
     * Starts the CLI server and establishes a connection.
     *
     * Called automatically when creating a session if [CopilotClientOptions.autoStart] is true.
     */
    suspend fun start() {
        if (state == ConnectionState.CONNECTED) return
        state = ConnectionState.CONNECTING

        try {
            if (!isExternalServer) {
                startCLIServer()
            }
            connectToServer()
            verifyProtocolVersion()

            // Register session filesystem provider if configured
            options.sessionFs?.let { fs ->
                rpcClient?.request("sessionFs.setProvider", buildJsonObject {
                    put("initialCwd", fs.initialCwd)
                    put("sessionStatePath", fs.sessionStatePath)
                    put("conventions", fs.conventions)
                })
            }

            state = ConnectionState.CONNECTED
        } catch (e: Exception) {
            state = ConnectionState.ERROR
            throw e
        }
    }

    /**
     * Stops the CLI server and closes all active sessions.
     *
     * @return List of errors encountered during cleanup. Empty list means success.
     */
    suspend fun stop(): List<String> {
        val errors = mutableListOf<String>()

        // Destroy all active sessions with retry
        for (session in sessions.values) {
            var lastError: Exception? = null
            for (attempt in 1..3) {
                try {
                    session.destroy()
                    lastError = null
                    break
                } catch (e: Exception) {
                    lastError = e
                    if (attempt < 3) {
                        delay(100L * (1 shl (attempt - 1)))
                    }
                }
            }
            if (lastError != null) {
                errors.add("Failed to destroy session ${session.sessionId} after 3 attempts: ${lastError.message}")
            }
        }
        sessions.clear()

        // Close RPC client
        rpcClient?.stop()
        rpcClient = null

        // Clear models cache
        synchronized(modelsCacheLock) {
            modelsCache = null
        }

        // Close TCP socket
        tcpSocket?.let {
            try {
                it.close()
            } catch (e: Exception) {
                errors.add("Failed to close socket: ${e.message}")
            }
            tcpSocket = null
        }

        // Kill CLI process (only if we spawned it)
        if (!isExternalServer) {
            cliProcess?.let {
                try {
                    it.destroyForcibly()
                } catch (e: Exception) {
                    errors.add("Failed to kill CLI process: ${e.message}")
                }
                cliProcess = null
            }
        }

        state = ConnectionState.DISCONNECTED
        if (!isExternalServer) actualPort = 0

        return errors
    }

    /**
     * Forcefully stops the CLI server without graceful cleanup.
     */
    fun forceStop() {
        sessions.clear()
        rpcClient?.stop()
        rpcClient = null
        synchronized(modelsCacheLock) { modelsCache = null }
        try { tcpSocket?.close() } catch (_: Exception) {}
        tcpSocket = null
        if (!isExternalServer) {
            try { cliProcess?.destroyForcibly() } catch (_: Exception) {}
            cliProcess = null
        }
        state = ConnectionState.DISCONNECTED
        if (!isExternalServer) actualPort = 0
    }

    /**
     * Creates a new conversation session.
     */
    suspend fun createSession(config: SessionConfig = SessionConfig()): CopilotSession {
        ensureConnected()
        val client = rpcClient ?: throw IllegalStateException("Client not connected")

        val payload = buildCreateSessionPayload(config)
        val response = client.request("session.create", payload)
        val responseObj = response.jsonObject
        val sessionId = responseObj["sessionId"]!!.jsonPrimitive.content
        val workspacePath = responseObj["workspacePath"]?.jsonPrimitive?.contentOrNull

        val session = CopilotSession(sessionId, client, clientScope, workspacePath)
        session.registerTools(config.tools)
        config.onPermissionRequest?.let { session.registerPermissionHandler(it) }
        config.onUserInputRequest?.let { session.registerUserInputHandler(it) }
        config.onElicitationRequest?.let { session.registerElicitationHandler(it) }
        config.hooks?.let { session.registerHooks(it) }
        sessions[sessionId] = session

        return session
    }

    /**
     * Resumes an existing conversation session by its ID.
     */
    suspend fun resumeSession(
        sessionId: String,
        config: ResumeSessionConfig = ResumeSessionConfig()
    ): CopilotSession {
        ensureConnected()
        val client = rpcClient ?: throw IllegalStateException("Client not connected")

        val payload = buildJsonObject {
            put("sessionId", sessionId)
            config.model?.let { put("model", it) }
            config.reasoningEffort?.let { put("reasoningEffort", json.encodeToJsonElement(ReasoningEffort.serializer(), it)) }
            config.configDir?.let { put("configDir", it) }
            config.tools?.let { tools ->
                put("tools", buildJsonArray {
                    for (tool in tools) {
                        add(buildJsonObject {
                            put("name", tool.name)
                            tool.description?.let { put("description", it) }
                            tool.parameters?.let { put("parameters", anyToJsonElement(it)) }
                        })
                    }
                })
            }
            config.systemMessage?.let { put("systemMessage", json.encodeToJsonElement(SystemMessageConfig.serializer(), it)) }
            config.availableTools?.let { put("availableTools", json.encodeToJsonElement(it)) }
            config.excludedTools?.let { put("excludedTools", json.encodeToJsonElement(it)) }
            config.provider?.let { put("provider", json.encodeToJsonElement(ProviderConfig.serializer(), it)) }
            if (config.onPermissionRequest != null) put("requestPermission", true)
            if (config.onUserInputRequest != null) put("requestUserInput", true)
            if (config.hooks?.hasAnyHook() == true) put("hooks", true)
            config.workingDirectory?.let { put("workingDirectory", it) }
            config.streaming?.let { put("streaming", it) }
            config.mcpServers?.let { put("mcpServers", json.encodeToJsonElement(it)) }
            config.customAgents?.let { put("customAgents", json.encodeToJsonElement(it)) }
            config.skillDirectories?.let { put("skillDirectories", json.encodeToJsonElement(it)) }
            config.disabledSkills?.let { put("disabledSkills", json.encodeToJsonElement(it)) }
            config.infiniteSessions?.let { put("infiniteSessions", json.encodeToJsonElement(InfiniteSessionConfig.serializer(), it)) }
            config.includeSubAgentStreamingEvents?.let { put("includeSubAgentStreamingEvents", JsonPrimitive(it)) }
            config.modelCapabilities?.let { put("modelCapabilities", anyToJsonElement(it)) }
            config.enableConfigDiscovery?.let { put("enableConfigDiscovery", JsonPrimitive(it)) }
            config.gitHubToken?.let { put("gitHubToken", JsonPrimitive(it)) }
            config.commands?.let { put("commands", json.encodeToJsonElement(it)) }
            if (config.onElicitationRequest != null) put("requestElicitation", true)
            config.disableResume?.let { put("disableResume", it) }
        }

        val response = client.request("session.resume", payload)
        val responseObj = response.jsonObject
        val resumedId = responseObj["sessionId"]!!.jsonPrimitive.content
        val workspacePath = responseObj["workspacePath"]?.jsonPrimitive?.contentOrNull

        val session = CopilotSession(resumedId, client, clientScope, workspacePath)
        session.registerTools(config.tools)
        config.onPermissionRequest?.let { session.registerPermissionHandler(it) }
        config.onUserInputRequest?.let { session.registerUserInputHandler(it) }
        config.onElicitationRequest?.let { session.registerElicitationHandler(it) }
        config.hooks?.let { session.registerHooks(it) }
        sessions[resumedId] = session

        return session
    }

    /**
     * Sends a ping request to the server.
     */
    suspend fun ping(message: String? = null): PingResponse {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val params = buildJsonObject {
            message?.let { put("message", it) }
        }
        val result = client.request("ping", params)
        return json.decodeFromJsonElement(PingResponse.serializer(), result)
    }

    /**
     * Gets CLI status including version and protocol information.
     */
    suspend fun getStatus(): GetStatusResponse {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("status.get", JsonObject(emptyMap()))
        return json.decodeFromJsonElement(GetStatusResponse.serializer(), result)
    }

    /**
     * Gets current authentication status.
     */
    suspend fun getAuthStatus(): GetAuthStatusResponse {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("auth.getStatus", JsonObject(emptyMap()))
        return json.decodeFromJsonElement(GetAuthStatusResponse.serializer(), result)
    }

    /**
     * Lists available models with their metadata. Results are cached.
     */
    suspend fun listModels(): List<ModelInfo> {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        synchronized(modelsCacheLock) {
            modelsCache?.let { return it.toList() }
        }

        val result = client.request("models.list", JsonObject(emptyMap()))
        val modelsArray = result.jsonObject["models"]?.jsonArray ?: JsonArray(emptyList())
        val models = modelsArray.map { json.decodeFromJsonElement(ModelInfo.serializer(), it) }

        synchronized(modelsCacheLock) {
            modelsCache = models
        }
        return models.toList()
    }

    /**
     * Lists all available sessions.
     */
    suspend fun listSessions(): List<SessionMetadata> {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("session.list", JsonObject(emptyMap()))
        val sessionsArray = result.jsonObject["sessions"]?.jsonArray ?: JsonArray(emptyList())
        return sessionsArray.map { json.decodeFromJsonElement(SessionMetadata.serializer(), it) }
    }

    /**
     * Gets the ID of the most recently updated session.
     */
    suspend fun getLastSessionId(): String? {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("session.getLastId", JsonObject(emptyMap()))
        return result.jsonObject["sessionId"]?.jsonPrimitive?.contentOrNull
    }

    /**
     * Gets metadata for a specific session.
     */
    suspend fun getSessionMetadata(sessionId: String): JsonObject {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        return client.request("session.getMetadata", buildJsonObject {
            put("sessionId", sessionId)
        }).jsonObject
    }

    /**
     * Deletes a session and its data from disk.
     */
    suspend fun deleteSession(sessionId: String) {
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("session.delete", buildJsonObject {
            put("sessionId", sessionId)
        })
        val success = result.jsonObject["success"]?.jsonPrimitive?.booleanOrNull ?: false
        if (!success) {
            val error = result.jsonObject["error"]?.jsonPrimitive?.contentOrNull ?: "Unknown error"
            throw RuntimeException("Failed to delete session $sessionId: $error")
        }
        sessions.remove(sessionId)
    }

    /**
     * Gets the foreground session ID (TUI+server mode).
     */
    suspend fun getForegroundSessionId(): String? {
        ensureConnected()
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("session.getForeground", JsonObject(emptyMap()))
        return result.jsonObject["sessionId"]?.jsonPrimitive?.contentOrNull
    }

    /**
     * Sets the foreground session (TUI+server mode).
     */
    suspend fun setForegroundSessionId(sessionId: String) {
        ensureConnected()
        val client = rpcClient ?: throw IllegalStateException("Client not connected")
        val result = client.request("session.setForeground", buildJsonObject {
            put("sessionId", sessionId)
        })
        val success = result.jsonObject["success"]?.jsonPrimitive?.booleanOrNull ?: false
        if (!success) {
            val error = result.jsonObject["error"]?.jsonPrimitive?.contentOrNull ?: "Unknown"
            throw RuntimeException("Failed to set foreground session: $error")
        }
    }

    /**
     * Subscribes to all session lifecycle events.
     * @return A function that unsubscribes the handler when called.
     */
    fun onLifecycle(handler: SessionLifecycleHandler): () -> Unit {
        lifecycleHandlers.add(handler)
        return { lifecycleHandlers.remove(handler) }
    }

    /**
     * Subscribes to a specific session lifecycle event type.
     * @return A function that unsubscribes the handler when called.
     */
    fun onLifecycle(eventType: String, handler: SessionLifecycleHandler): () -> Unit {
        typedLifecycleHandlers.computeIfAbsent(eventType) { CopyOnWriteArrayList() }.add(handler)
        return {
            typedLifecycleHandlers[eventType]?.remove(handler)
        }
    }

    // ========================================================================
    // Private Implementation
    // ========================================================================

    private fun parseCliUrl(url: String): Pair<String, Int> {
        val clean = url.replace(Regex("^https?://"), "")

        // Just a port number
        if (clean.matches(Regex("\\d+"))) {
            return "localhost" to clean.toInt()
        }

        val parts = clean.split(":")
        if (parts.size != 2) {
            throw IllegalArgumentException("Invalid cliUrl format: $url. Expected \"host:port\", \"http://host:port\", or \"port\"")
        }

        val host = parts[0].ifEmpty { "localhost" }
        val port = parts[1].toIntOrNull()
            ?: throw IllegalArgumentException("Invalid port in cliUrl: $url")

        if (port <= 0 || port > 65535) {
            throw IllegalArgumentException("Invalid port in cliUrl: $url")
        }

        return host to port
    }

    private suspend fun ensureConnected() {
        if (rpcClient != null) return
        if (resolvedAutoStart) {
            start()
            return
        }
        throw IllegalStateException("Client not connected. Call start() first.")
    }

    private fun anyToJsonElement(value: Any?): JsonElement = when (value) {
        null -> JsonNull
        is JsonElement -> value
        is String -> JsonPrimitive(value)
        is Boolean -> JsonPrimitive(value)
        is Number -> JsonPrimitive(value)
        is Map<*, *> -> buildJsonObject {
            for ((k, v) in value) put(k.toString(), anyToJsonElement(v))
        }
        is Iterable<*> -> buildJsonArray { for (item in value) add(anyToJsonElement(item)) }
        is Array<*> -> buildJsonArray { for (item in value) add(anyToJsonElement(item)) }
        else -> JsonPrimitive(value.toString())
    }

    private fun buildCreateSessionPayload(config: SessionConfig): JsonObject {
        return buildJsonObject {
            config.model?.let { put("model", it) }
            config.sessionId?.let { put("sessionId", it) }
            config.reasoningEffort?.let { put("reasoningEffort", json.encodeToJsonElement(ReasoningEffort.serializer(), it)) }
            config.configDir?.let { put("configDir", it) }
            config.tools?.let { tools ->
                put("tools", buildJsonArray {
                    for (tool in tools) {
                        add(buildJsonObject {
                            put("name", tool.name)
                            tool.description?.let { put("description", it) }
                            tool.parameters?.let { put("parameters", anyToJsonElement(it)) }
                            tool.defer?.let { put("defer", it) }
                        })
                    }
                })
            }
            config.systemMessage?.let { put("systemMessage", json.encodeToJsonElement(SystemMessageConfig.serializer(), it)) }
            config.availableTools?.let { put("availableTools", json.encodeToJsonElement(it)) }
            config.excludedTools?.let { put("excludedTools", json.encodeToJsonElement(it)) }
            config.provider?.let { put("provider", json.encodeToJsonElement(ProviderConfig.serializer(), it)) }
            if (config.onPermissionRequest != null) put("requestPermission", true)
            if (config.onUserInputRequest != null) put("requestUserInput", true)
            config.hooks?.let { hooks ->
                if (hooks.hasAnyHook()) {
                    put("hooks", true)
                    // Surface the specific lifecycle hooks the runtime should invoke.
                    put("hookTypes", buildJsonArray {
                        if (hooks.onPreToolUse != null) add(JsonPrimitive("preToolUse"))
                        if (hooks.onPostToolUse != null) add(JsonPrimitive("postToolUse"))
                        if (hooks.onPreMcpToolCall != null) add(JsonPrimitive("preMcpToolCall"))
                        if (hooks.onUserPromptSubmitted != null) add(JsonPrimitive("userPromptSubmitted"))
                        if (hooks.onUserPromptTransformed != null) add(JsonPrimitive("userPromptTransformed"))
                        if (hooks.onSessionStart != null) add(JsonPrimitive("sessionStart"))
                        if (hooks.onSessionEnd != null) add(JsonPrimitive("sessionEnd"))
                        if (hooks.onErrorOccurred != null) add(JsonPrimitive("errorOccurred"))
                    })
                }
            }
            config.workingDirectory?.let { put("workingDirectory", it) }
            config.streaming?.let { put("streaming", it) }
            config.mcpServers?.let { put("mcpServers", json.encodeToJsonElement(it)) }
            config.customAgents?.let { put("customAgents", json.encodeToJsonElement(it)) }
            config.skillDirectories?.let { put("skillDirectories", json.encodeToJsonElement(it)) }
            config.disabledSkills?.let { put("disabledSkills", json.encodeToJsonElement(it)) }
            config.infiniteSessions?.let { put("infiniteSessions", json.encodeToJsonElement(InfiniteSessionConfig.serializer(), it)) }
            config.includeSubAgentStreamingEvents?.let { put("includeSubAgentStreamingEvents", JsonPrimitive(it)) }
            config.modelCapabilities?.let { put("modelCapabilities", anyToJsonElement(it)) }
            config.enableConfigDiscovery?.let { put("enableConfigDiscovery", JsonPrimitive(it)) }
            config.gitHubToken?.let { put("gitHubToken", JsonPrimitive(it)) }
            config.commands?.let { put("commands", json.encodeToJsonElement(it)) }
            if (config.onElicitationRequest != null) put("requestElicitation", true)
            // Wire sessionIdleTimeoutSeconds (idleTimeout), requestHeaders, imageGeneration / responseFormat
            config.requestHeaders?.let { put("requestHeaders", json.encodeToJsonElement(it)) }
            config.responseFormat?.let { put("responseFormat", JsonPrimitive(it)) }

            // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
            config.enableCitations?.let { put("enableCitations", JsonPrimitive(it)) }
            config.excludedBuiltinAgents?.let { put("excludedBuiltinAgents", json.encodeToJsonElement(it)) }
            config.sessionLimits?.let { limits ->
                put("sessionLimits", buildJsonObject {
                    limits.maxAiCredits?.let { put("maxAiCredits", JsonPrimitive(it)) }
                })
            }
            config.memory?.let { mem: MemoryConfiguration ->
                put("memory", buildJsonObject {
                    mem.enabled?.let { put("enabled", JsonPrimitive(it)) }
                })
            }
            config.otlpProtocol?.let { put("otlpProtocol", JsonPrimitive(it)) }
            config.enableWebSocketResponses?.let { put("enableWebSocketResponses", JsonPrimitive(it)) }
            config.expAssignments?.let { put("expAssignments", json.encodeToJsonElement(it)) }
            config.rewindEnabled?.let { put("rewindEnabled", JsonPrimitive(it)) }
            config.additionalDirectories?.let { put("additionalDirectories", json.encodeToJsonElement(it)) }
            config.disabledMcpServers?.let { put("disabledMcpServers", json.encodeToJsonElement(it)) }
            config.githubMcpToolConfig?.let { put("githubMcpToolConfig", json.encodeToJsonElement(it)) }
            config.canvasProvider?.let { put("canvasProvider", json.encodeToJsonElement(it)) }
            config.customAgentsLocalOnly?.let { put("customAgentsLocalOnly", JsonPrimitive(it)) }
            config.toolSearch?.let { put("toolSearch", json.encodeToJsonElement(it)) }
            config.experimentalMode?.let { put("experimentalMode", JsonPrimitive(it)) }
            config.contentExclusion?.let { put("contentExclusion", JsonPrimitive(it)) }
            // MCP OAuth host token handler: signal to the runtime that a handler is registered.
            if (config.onMcpAuthRequest != null) put("mcpAuthHandler", true)
        }
    }

    private suspend fun verifyProtocolVersion(){
        val expected = getSdkProtocolVersion()
        val pingResult = ping()
        val serverVersion = pingResult.protocolVersion

        if (serverVersion == null) {
            throw RuntimeException(
                "SDK protocol version mismatch: SDK expects version $expected, " +
                    "but server does not report a protocol version. Please update your server to ensure compatibility."
            )
        }
        if (serverVersion != expected) {
            throw RuntimeException(
                "SDK protocol version mismatch: SDK expects version $expected, " +
                    "but server reports version $serverVersion. Please update your SDK or server to ensure compatibility."
            )
        }
    }

    private suspend fun startCLIServer() {
        withContext(Dispatchers.IO) {
            val args = mutableListOf<String>()

            // If cliPath is a .js file, run with node
            if (resolvedCliPath.endsWith(".js")) {
                args.add("node")
                args.add(resolvedCliPath)
            } else {
                args.add(resolvedCliPath)
            }

            // Add extra CLI args
            args.addAll(resolvedCliArgs)
            args.addAll(listOf("--headless", "--no-auto-update", "--log-level", resolvedLogLevel))

            if (resolvedUseStdio) {
                args.add("--stdio")
            } else if (resolvedPort > 0) {
                args.add("--port")
                args.add(resolvedPort.toString())
            }

            if (resolvedGithubToken != null) {
                args.add("--auth-token-env")
                args.add("COPILOT_SDK_AUTH_TOKEN")
            }
            if (!resolvedUseLoggedInUser) {
                args.add("--no-auto-login")
            }

            // Verify CLI exists before spawning
            if (!resolvedCliPath.endsWith(".js")) {
                // For non-JS paths, check if file exists (for JS, node handles it)
                val cliFile = File(resolvedCliPath)
                if (cliFile.isAbsolute && !cliFile.exists()) {
                    throw RuntimeException("Copilot CLI not found at $resolvedCliPath")
                }
            }

            val pb = ProcessBuilder(args)
            resolvedCwd?.let { pb.directory(File(it)) }
            pb.redirectErrorStream(false)

            // Set environment
            val processEnv = pb.environment()
            resolvedEnv?.let {
                processEnv.clear()
                processEnv.putAll(it)
            }
            if (resolvedGithubToken != null) {
                processEnv["COPILOT_SDK_AUTH_TOKEN"] = resolvedGithubToken
            }

            cliProcess = pb.start()

            // Consume stderr in a background thread
            val process = cliProcess!!
            val stderrThread = Thread({
                try {
                    val reader = BufferedReader(InputStreamReader(process.errorStream))
                    var line: String?
                    while (reader.readLine().also { line = it } != null) {
                        System.err.println("[CLI subprocess] $line")
                    }
                } catch (_: Exception) {
                    // Stream closed
                }
            }, "cli-stderr").apply { isDaemon = true }
            stderrThread.start()

            if (!resolvedUseStdio) {
                // TCP mode: read stdout to get port announcement
                val reader = BufferedReader(InputStreamReader(process.inputStream))
                val deadline = System.currentTimeMillis() + 10_000
                var found = false
                var line: String?
                while (reader.readLine().also { line = it } != null) {
                    val matcher = PORT_PATTERN.matcher(line!!)
                    if (matcher.find()) {
                        actualPort = matcher.group(1).toInt()
                        found = true
                        break
                    }
                    if (System.currentTimeMillis() > deadline) {
                        throw RuntimeException("Timeout waiting for CLI server to start")
                    }
                }
                if (!found) {
                    throw RuntimeException("CLI process exited before announcing port")
                }
            }
        }
    }

    private suspend fun connectToServer() {
        if (resolvedUseStdio) {
            connectViaStdio()
        } else {
            connectViaTcp()
        }
    }

    private fun connectViaStdio() {
        val process = cliProcess ?: throw IllegalStateException("CLI process not started")
        rpcClient = JsonRpcClient(process.inputStream, process.outputStream, clientScope)
        setupHandlers()
        rpcClient!!.start()
    }

    private suspend fun connectViaTcp() {
        if (actualPort == 0) throw IllegalStateException("Server port not available")
        withContext(Dispatchers.IO) {
            tcpSocket = Socket(actualHost, actualPort)
            rpcClient = JsonRpcClient(tcpSocket!!.getInputStream(), tcpSocket!!.getOutputStream(), clientScope)
            setupHandlers()
            rpcClient!!.start()
        }
    }

    private fun setupHandlers() {
        val client = rpcClient ?: return

        // Handle session event notifications
        client.setNotificationHandler { method, params ->
            when (method) {
                "session.event" -> handleSessionEventNotification(params)
                "session.lifecycle" -> handleSessionLifecycleNotification(params)
            }
        }

        // Handle incoming requests from server
        client.setRequestHandler("tool.call") { params -> handleToolCallRequest(params) }
        client.setRequestHandler("permission.request") { params -> handlePermissionRequest(params) }
        client.setRequestHandler("userInput.request") { params -> handleUserInputRequest(params) }
        client.setRequestHandler("hooks.invoke") { params -> handleHooksInvoke(params) }
        client.setRequestHandler("elicitation.request") { params -> handleElicitationRequest(params) }

        // Session filesystem provider callbacks. When a provider is registered via
        // sessionFs.setProvider, the CLI drives filesystem access by sending these
        // requests back to the client. Service them against the real filesystem.
        client.setRequestHandler("sessionFs.readFile") { params -> handleSessionFsReadFile(params) }
        client.setRequestHandler("sessionFs.writeFile") { params -> handleSessionFsWriteFile(params) }
        client.setRequestHandler("sessionFs.appendFile") { params -> handleSessionFsAppendFile(params) }
        client.setRequestHandler("sessionFs.exists") { params -> handleSessionFsExists(params) }
        client.setRequestHandler("sessionFs.stat") { params -> handleSessionFsStat(params) }
        client.setRequestHandler("sessionFs.mkdir") { params -> handleSessionFsMkdir(params) }
        client.setRequestHandler("sessionFs.readdir") { params -> handleSessionFsReaddir(params) }
        client.setRequestHandler("sessionFs.readdirWithTypes") { params -> handleSessionFsReaddirWithTypes(params) }
        client.setRequestHandler("sessionFs.rm") { params -> handleSessionFsRm(params) }
        client.setRequestHandler("sessionFs.rename") { params -> handleSessionFsRename(params) }
    }

    private fun handleSessionEventNotification(params: JsonObject) {
        val sessionId = params["sessionId"]?.jsonPrimitive?.contentOrNull ?: return
        val eventElement = params["event"]?.jsonObject ?: return
        val session = sessions[sessionId] ?: return

        val event = SessionEvent(
            type = eventElement["type"]?.jsonPrimitive?.content ?: return,
            data = eventElement["data"]?.jsonObject
        )
        session.dispatchEvent(event)
    }

    private fun handleSessionLifecycleNotification(params: JsonObject) {
        val type = params["type"]?.jsonPrimitive?.contentOrNull ?: return
        val sessionId = params["sessionId"]?.jsonPrimitive?.contentOrNull ?: return

        val event = SessionLifecycleEvent(
            type = type,
            sessionId = sessionId,
            metadata = params["metadata"]?.let {
                json.decodeFromJsonElement(SessionLifecycleEvent.LifecycleMetadata.serializer(), it)
            }
        )

        // Typed handlers
        typedLifecycleHandlers[type]?.forEach { handler ->
            try {
                handler(event)
            } catch (_: Exception) {
                // Ignore handler errors
            }
        }

        // Wildcard handlers
        lifecycleHandlers.forEach { handler ->
            try {
                handler(event)
            } catch (_: Exception) {
                // Ignore handler errors
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    private suspend fun handleToolCallRequest(params: JsonObject): JsonObject {
        val sessionId = params["sessionId"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid tool call payload: missing sessionId")
        val toolCallId = params["toolCallId"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid tool call payload: missing toolCallId")
        val toolName = params["toolName"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid tool call payload: missing toolName")
        val arguments = params["arguments"]

        val session = sessions[sessionId]
            ?: throw RuntimeException("Unknown session $sessionId")

        val handler = session.getToolHandler(toolName)
        if (handler == null) {
            return buildJsonObject {
                put("result", buildJsonObject {
                    put("textResultForLlm", "Tool '$toolName' is not supported by this client instance.")
                    put("resultType", "failure")
                    put("error", "tool '$toolName' not supported")
                    put("toolTelemetry", JsonObject(emptyMap()))
                })
            }
        }

        return try {
            val invocation = ToolInvocation(sessionId, toolCallId, toolName, arguments)
            val result = handler(arguments, invocation)
            buildJsonObject {
                put("result", normalizeToolResult(result))
            }
        } catch (e: Exception) {
            buildJsonObject {
                put("result", buildJsonObject {
                    put("textResultForLlm", "Invoking this tool produced an error. Detailed information is not available.")
                    put("resultType", "failure")
                    put("error", e.message ?: "Unknown error")
                    put("toolTelemetry", JsonObject(emptyMap()))
                })
            }
        }
    }

    private suspend fun handlePermissionRequest(params: JsonObject): JsonObject {
        val sessionId = params["sessionId"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid permission request payload")
        val permissionRequest = params["permissionRequest"]?.jsonObject
            ?: throw RuntimeException("Invalid permission request payload")

        val session = sessions[sessionId]
            ?: throw RuntimeException("Session not found: $sessionId")

        return try {
            val result = session.handlePermissionRequest(permissionRequest)
            buildJsonObject {
                put("result", buildJsonObject {
                    put("kind", result.kind)
                    result.rules?.let { put("rules", json.encodeToJsonElement(it)) }
                })
            }
        } catch (_: Exception) {
            buildJsonObject {
                put("result", buildJsonObject {
                    put("kind", "denied-no-approval-rule-and-could-not-request-from-user")
                })
            }
        }
    }

    private suspend fun handleUserInputRequest(params: JsonObject): JsonObject {
        val sessionId = params["sessionId"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid user input request payload")
        val question = params["question"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid user input request payload")

        val session = sessions[sessionId]
            ?: throw RuntimeException("Session not found: $sessionId")

        val request = UserInputRequest(
            question = question,
            choices = params["choices"]?.jsonArray?.map { it.jsonPrimitive.content },
            allowFreeform = params["allowFreeform"]?.jsonPrimitive?.booleanOrNull
        )

        val result = session.handleUserInputRequest(request)
        return buildJsonObject {
            put("answer", result.answer)
            put("wasFreeform", result.wasFreeform)
        }
    }

    private suspend fun handleHooksInvoke(params: JsonObject): JsonObject {
        val sessionId = params["sessionId"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid hooks invoke payload")
        val hookType = params["hookType"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid hooks invoke payload")
        val input = params["input"]

        val session = sessions[sessionId]
            ?: throw RuntimeException("Session not found: $sessionId")

        val output = session.handleHooksInvoke(hookType, input)
        return buildJsonObject {
            output?.let { put("output", json.encodeToJsonElement(it)) }
        }
    }

    private suspend fun handleElicitationRequest(params: JsonObject): JsonObject {
        val sessionId = params["sessionId"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid elicitation request payload")
        val message = params["message"]?.jsonPrimitive?.content
            ?: throw RuntimeException("Invalid elicitation request payload")

        val session = sessions[sessionId]
            ?: throw RuntimeException("Session not found: $sessionId")

        val context = ElicitationContext(
            sessionId = sessionId,
            message = message,
            requestedSchema = params["requestedSchema"]?.jsonObject?.toMap()?.mapValues { (_, v) ->
                when (v) {
                    is JsonPrimitive -> if (v.isString) v.content else v.toString()
                    else -> v.toString()
                }
            },
            mode = params["mode"]?.jsonPrimitive?.contentOrNull,
            elicitationSource = params["elicitationSource"]?.jsonPrimitive?.contentOrNull,
            url = params["url"]?.jsonPrimitive?.contentOrNull
        )

        val result = session.handleElicitationRequest(context)
        return buildJsonObject {
            put("action", result.action)
            result.content?.let { put("content", json.encodeToJsonElement(it)) }
        }
    }

    // ---- SessionFs provider request handlers ----
    //
    // These service the CLI's sessionFs.* callbacks against the real filesystem
    // using the absolute paths supplied by the CLI. Success returns mirror the
    // generated result shapes; errors are returned in-band as a SessionFsError
    // ({ code, message }) so the CLI can distinguish a missing file (ENOENT)
    // from other failures without the request failing at the JSON-RPC layer.

    private fun sessionFsError(e: Throwable): JsonObject = buildJsonObject {
        val code = if (e is java.nio.file.NoSuchFileException || e is java.io.FileNotFoundException) "ENOENT" else "UNKNOWN"
        put("code", code)
        put("message", e.message ?: e.toString())
    }

    private fun handleSessionFsReadFile(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return try {
            buildJsonObject { put("content", File(path).readText(Charsets.UTF_8)) }
        } catch (e: Exception) {
            buildJsonObject {
                put("content", "")
                put("error", sessionFsError(e))
            }
        }
    }

    private fun handleSessionFsWriteFile(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return try {
            val f = File(path)
            f.parentFile?.mkdirs()
            f.writeText(params["content"]?.jsonPrimitive?.content ?: "", Charsets.UTF_8)
            JsonObject(emptyMap())
        } catch (e: Exception) {
            sessionFsError(e)
        }
    }

    private fun handleSessionFsAppendFile(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return try {
            val f = File(path)
            f.parentFile?.mkdirs()
            f.appendText(params["content"]?.jsonPrimitive?.content ?: "", Charsets.UTF_8)
            JsonObject(emptyMap())
        } catch (e: Exception) {
            sessionFsError(e)
        }
    }

    private fun handleSessionFsExists(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return buildJsonObject { put("exists", File(path).exists()) }
    }

    private fun handleSessionFsStat(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return try {
            val f = File(path)
            if (!f.exists()) throw java.io.FileNotFoundException(path)
            val attrs = java.nio.file.Files.readAttributes(
                f.toPath(), java.nio.file.attribute.BasicFileAttributes::class.java
            )
            buildJsonObject {
                put("isFile", f.isFile)
                put("isDirectory", f.isDirectory)
                put("size", f.length())
                put("mtime", java.time.Instant.ofEpochMilli(attrs.lastModifiedTime().toMillis()).toString())
                put("birthtime", java.time.Instant.ofEpochMilli(attrs.creationTime().toMillis()).toString())
            }
        } catch (e: Exception) {
            val now = java.time.Instant.now().toString()
            buildJsonObject {
                put("isFile", false)
                put("isDirectory", false)
                put("size", 0)
                put("mtime", now)
                put("birthtime", now)
                put("error", sessionFsError(e))
            }
        }
    }

    private fun handleSessionFsMkdir(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        val recursive = params["recursive"]?.jsonPrimitive?.booleanOrNull ?: false
        return try {
            val f = File(path)
            val ok = if (recursive) f.mkdirs() else f.mkdir()
            if (!ok && !f.isDirectory) throw java.io.IOException("Failed to create directory: $path")
            JsonObject(emptyMap())
        } catch (e: Exception) {
            sessionFsError(e)
        }
    }

    private fun handleSessionFsReaddir(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return try {
            val children = File(path).listFiles() ?: throw java.io.FileNotFoundException(path)
            buildJsonObject {
                put("entries", buildJsonArray { children.forEach { add(it.name) } })
            }
        } catch (e: Exception) {
            buildJsonObject {
                put("entries", JsonArray(emptyList()))
                put("error", sessionFsError(e))
            }
        }
    }

    private fun handleSessionFsReaddirWithTypes(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        return try {
            val children = File(path).listFiles() ?: throw java.io.FileNotFoundException(path)
            buildJsonObject {
                put("entries", buildJsonArray {
                    children.forEach { child ->
                        add(buildJsonObject {
                            put("name", child.name)
                            put("type", if (child.isDirectory) "directory" else "file")
                        })
                    }
                })
            }
        } catch (e: Exception) {
            buildJsonObject {
                put("entries", JsonArray(emptyList()))
                put("error", sessionFsError(e))
            }
        }
    }

    private fun handleSessionFsRm(params: JsonObject): JsonObject {
        val path = params["path"]?.jsonPrimitive?.content ?: ""
        val recursive = params["recursive"]?.jsonPrimitive?.booleanOrNull ?: false
        val force = params["force"]?.jsonPrimitive?.booleanOrNull ?: false
        return try {
            val f = File(path)
            if (!f.exists()) {
                if (force) return JsonObject(emptyMap())
                throw java.io.FileNotFoundException(path)
            }
            val ok = if (recursive) f.deleteRecursively() else f.delete()
            if (!ok && !force) throw java.io.IOException("Failed to remove: $path")
            JsonObject(emptyMap())
        } catch (e: Exception) {
            if (force) JsonObject(emptyMap()) else sessionFsError(e)
        }
    }

    private fun handleSessionFsRename(params: JsonObject): JsonObject {
        val src = params["src"]?.jsonPrimitive?.content ?: ""
        val dest = params["dest"]?.jsonPrimitive?.content ?: ""
        return try {
            val d = File(dest)
            d.parentFile?.mkdirs()
            java.nio.file.Files.move(
                File(src).toPath(), d.toPath(),
                java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )
            JsonObject(emptyMap())
        } catch (e: Exception) {
            sessionFsError(e)
        }
    }

    private fun normalizeToolResult(result: Any?): JsonElement {
        if (result == null) {
            return buildJsonObject {
                put("textResultForLlm", "Tool returned no result")
                put("resultType", "failure")
                put("error", "tool returned no result")
                put("toolTelemetry", JsonObject(emptyMap()))
            }
        }

        // If it's already a JsonObject with textResultForLlm, pass through
        if (result is JsonObject && result.containsKey("textResultForLlm")) {
            return result
        }

        // If it's a ToolResultObject, serialize it
        if (result is ToolResultObject) {
            return json.encodeToJsonElement(ToolResultObject.serializer(), result)
        }

        // Everything else gets wrapped as a successful result
        val textResult = when (result) {
            is String -> result
            is JsonElement -> json.encodeToString(JsonElement.serializer(), result)
            else -> result.toString()
        }

        return buildJsonObject {
            put("textResultForLlm", textResult)
            put("resultType", "success")
            put("toolTelemetry", JsonObject(emptyMap()))
        }
    }
}

/**
 * Builds the `session.send` JSON-RPC payload, including the upstream-sync
 * message options (agentMode, displayPrompt, requestHeaders).
 */
internal fun buildSendPayload(sessionId: String, options: MessageOptions, json: Json): JsonObject = buildJsonObject {
    put("sessionId", sessionId)
    put("prompt", options.prompt)
    options.attachments?.let { put("attachments", json.encodeToJsonElement(it)) }
    options.mode?.let { put("mode", it) }
    options.agentMode?.let { put("agentMode", it) }
    options.displayPrompt?.let { put("displayPrompt", it) }
    options.requestHeaders?.let { put("requestHeaders", json.encodeToJsonElement(it)) }
    options.responseFormat?.let { put("responseFormat", json.encodeToJsonElement(it)) }
    options.imageOptions?.let { put("imageOptions", json.encodeToJsonElement(it)) }
}

/** Tool "defer" loading policy values. */
object ToolDefer {
    const val AUTO = "auto"
    const val NEVER = "never"
}

/** System-message section identifiers (used with system-message section overrides). */
object SystemMessageSection {
    const val PREAMBLE = "preamble"
    const val IDENTITY = "identity"
    const val TOOL_INSTRUCTIONS = "tool_instructions"
    const val PRESERVE = "preserve"
}

/** GitHub-anchored attachment type identifiers. */
object GitHubAttachment {
    const val GITHUB_COMMIT = "GitHubCommit"
    const val GITHUB_RELEASE = "GitHubRelease"
    const val GITHUB_ACTIONS_JOB = "GitHubActionsJob"
    const val GITHUB_REPOSITORY = "GitHubRepository"
    const val GITHUB_FILE_DIFF = "GitHubFileDiff"
    const val GITHUB_TREE_COMPARISON = "GitHubTreeComparison"
    const val GITHUB_URL = "GitHubUrl"
    const val GITHUB_FILE = "GitHubFile"
    const val GITHUB_SNIPPET = "GitHubSnippet"
}
