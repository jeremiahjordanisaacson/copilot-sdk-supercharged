/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import com.github.copilot.Types.*
import groovy.transform.CompileStatic

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.regex.Matcher
import java.util.regex.Pattern

/**
 * Main client for interacting with the Copilot CLI.
 *
 * <p>The CopilotClient manages the connection to the Copilot CLI server and provides
 * methods to create and manage conversation sessions. It can either spawn a CLI
 * server process or connect to an existing server.
 *
 * <pre>
 * // Create with defaults (spawns CLI server)
 * def client = new CopilotClient()
 *
 * // Or connect to an existing server
 * def client = new CopilotClient(cliUrl: 'localhost:3000')
 *
 * // Create a session
 * def session = client.createSession(model: 'gpt-4')
 *
 * // Send a message
 * def result = session.sendAndWait('Hello!')
 * println result?.data?.content
 *
 * // Clean up
 * session.destroy()
 * client.stop()
 * </pre>
 */
@CompileStatic
class CopilotClient {
    private static final Pattern PORT_PATTERN = Pattern.compile('listening on port (\\d+)', Pattern.CASE_INSENSITIVE)

    private Process cliProcess
    private JsonRpcClient rpcClient
    private Socket tcpSocket
    private int actualPort
    private String actualHost = 'localhost'
    private ConnectionState state = ConnectionState.DISCONNECTED
    private final ConcurrentHashMap<String, CopilotSession> sessions = new ConcurrentHashMap<>()
    private final boolean isExternalServer
    private boolean useStdio = true
    private boolean autoStart = true
    private boolean autoRestart = true
    private volatile List<ModelInfo> modelsCache
    private final Object modelsCacheLock = new Object()
    private final CopyOnWriteArrayList<Closure> lifecycleHandlers = new CopyOnWriteArrayList<>()
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<Closure>> typedLifecycleHandlers = new ConcurrentHashMap<>()

    // Resolved options
    private String cliPath = 'copilot'
    private String[] cliArgs = [] as String[]
    private String cwd
    private int port
    private String logLevel = 'info'
    private Map<String, String> env
    private String githubToken
    private boolean useLoggedInUser = true
    private SessionFsConfig sessionFsConfig
    private Integer sessionIdleTimeoutSeconds

    // Session config defaults (stored for createSession)
    private List<Tool> defaultTools
    private Closure defaultOnPermissionRequest
    private Closure defaultOnUserInputRequest

    /**
     * Creates a new CopilotClient with default options.
     */
    CopilotClient() {
        this([:])
    }

    /**
     * Creates a new CopilotClient with the given options map.
     *
     * <p>Supported keys:
     * <ul>
     *   <li>{@code cliUrl} - URL of an external CLI server</li>
     *   <li>{@code cliPath} - Path to the CLI binary</li>
     *   <li>{@code cliArgs} - Extra CLI arguments</li>
     *   <li>{@code cwd} - Working directory for CLI process</li>
     *   <li>{@code port} - TCP port (disables stdio)</li>
     *   <li>{@code logLevel} - Logging level</li>
     *   <li>{@code useStdio} - Use stdio transport</li>
     *   <li>{@code autoStart} - Auto-start CLI on first request</li>
     *   <li>{@code autoRestart} - Auto-restart CLI on failure</li>
     *   <li>{@code env} - Environment variables</li>
     *   <li>{@code githubToken} - GitHub auth token</li>
     *   <li>{@code useLoggedInUser} - Use logged-in user for auth</li>
     * </ul>
     */
    CopilotClient(Map<String, Object> options) {
        boolean external = false

        if (options) {
            String cliUrlStr = options.cliUrl as String
            if (cliUrlStr && (options.useStdio != null || options.cliPath != null)) {
                throw new IllegalArgumentException('cliUrl is mutually exclusive with useStdio and cliPath')
            }
            if (cliUrlStr && (options.githubToken != null || options.useLoggedInUser != null)) {
                throw new IllegalArgumentException('githubToken and useLoggedInUser cannot be used with cliUrl')
            }

            if (cliUrlStr) {
                actualPort = parseCliUrl(cliUrlStr)
                external = true
                useStdio = false
            }

            if (options.cliPath) this.cliPath = options.cliPath as String
            if (options.cliArgs) this.cliArgs = options.cliArgs as String[]
            if (options.cwd) this.cwd = options.cwd as String
            if (options.port) {
                this.port = options.port as int
                this.useStdio = false
            }
            if (options.logLevel) this.logLevel = options.logLevel as String
            if (options.useStdio != null) this.useStdio = options.useStdio as boolean
            if (options.autoStart != null) this.autoStart = options.autoStart as boolean
            if (options.autoRestart != null) this.autoRestart = options.autoRestart as boolean
            if (options.env) this.env = options.env as Map<String, String>
            if (options.githubToken) {
                this.githubToken = options.githubToken as String
                this.useLoggedInUser = false
            }
            if (options.useLoggedInUser != null) this.useLoggedInUser = options.useLoggedInUser as boolean
            if (options.sessionFs) {
                this.sessionFsConfig = options.sessionFs instanceof SessionFsConfig
                    ? (SessionFsConfig) options.sessionFs
                    : new SessionFsConfig().with {
                        Map<String, Object> fsMap = (Map<String, Object>) options.sessionFs
                        initialCwd = (String) fsMap.initialCwd
                        sessionStatePath = (String) fsMap.sessionStatePath
                        conventions = (String) fsMap.conventions
                        it
                    }
                validateSessionFsConfig(this.sessionFsConfig)
            }
            if (options.sessionIdleTimeoutSeconds != null) {
                this.sessionIdleTimeoutSeconds = options.sessionIdleTimeoutSeconds as Integer
            }
        }

        this.isExternalServer = external

        // Check env for CLI path
        String envCliPath = System.getenv('COPILOT_CLI_PATH')
        if (envCliPath) {
            this.cliPath = envCliPath
        }
    }

    private int parseCliUrl(String url) {
        String clean = url.replaceAll('^https?://', '')
        if (clean ==~ /\d+/) {
            actualHost = 'localhost'
            return clean as int
        }
        String[] parts = clean.split(':')
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid cliUrl format: ${url}")
        }
        actualHost = parts[0] ?: 'localhost'
        int p = parts[1] as int
        if (p <= 0 || p > 65535) {
            throw new IllegalArgumentException("Invalid port: ${url}")
        }
        p
    }

    /** Starts the CLI server and establishes a connection. */
    void start() throws Exception {
        if (state == ConnectionState.CONNECTED) return
        state = ConnectionState.CONNECTING

        try {
            if (!isExternalServer) {
                startCLIServer()
            }
            connectToServer()
            verifyProtocolVersion()

            if (sessionFsConfig) {
                rpcClient.request('sessionFs.setProvider', [
                    initialCwd     : sessionFsConfig.initialCwd,
                    sessionStatePath: sessionFsConfig.sessionStatePath,
                    conventions    : sessionFsConfig.conventions
                ] as Map<String, Object>)
            }

            state = ConnectionState.CONNECTED
        } catch (Exception e) {
            state = ConnectionState.ERROR
            throw e
        }
    }

    /**
     * Stops the CLI server and closes all active sessions.
     *
     * @return list of errors encountered during cleanup
     */
    List<StopError> stop() {
        List<StopError> errors = []

        sessions.values().each { CopilotSession session ->
            try {
                session.destroy()
            } catch (Exception e) {
                errors << new StopError("Failed to destroy session ${session.sessionId}: ${e.message}")
            }
        }
        sessions.clear()

        if (rpcClient) {
            rpcClient.stop()
            rpcClient = null
        }

        synchronized (modelsCacheLock) {
            modelsCache = null
        }

        if (tcpSocket) {
            try { tcpSocket.close() } catch (IOException e) {
                errors << new StopError("Failed to close socket: ${e.message}")
            }
            tcpSocket = null
        }

        if (cliProcess && !isExternalServer) {
            cliProcess.destroyForcibly()
            cliProcess = null
        }

        state = ConnectionState.DISCONNECTED
        if (!isExternalServer) actualPort = 0
        errors
    }

    /** Forcefully stops the CLI server without graceful cleanup. */
    void forceStop() {
        sessions.clear()
        if (rpcClient) { rpcClient.stop(); rpcClient = null }
        synchronized (modelsCacheLock) { modelsCache = null }
        if (tcpSocket) { try { tcpSocket.close() } catch (IOException ignored) {}; tcpSocket = null }
        if (cliProcess && !isExternalServer) { cliProcess.destroyForcibly(); cliProcess = null }
        state = ConnectionState.DISCONNECTED
        if (!isExternalServer) actualPort = 0
    }

    /**
     * Creates a new conversation session.
     *
     * <p>Accepts a map of config options:
     * <ul>
     *   <li>{@code model} - Model name</li>
     *   <li>{@code tools} - List of Tool instances</li>
     *   <li>{@code systemMessage} - System message</li>
     *   <li>{@code streaming} - Enable streaming</li>
     *   <li>{@code provider} - BYOK provider config</li>
     *   <li>{@code reasoningEffort} - Reasoning effort level</li>
     *   <li>{@code onPermissionRequest} - Closure for permission handling</li>
     *   <li>{@code onUserInputRequest} - Closure for user input handling</li>
     *   <li>{@code workingDirectory} - Working directory</li>
     *   <li>{@code mcpServers} - MCP server configs</li>
     *   <li>{@code customAgents} - Custom agent configs</li>
     *   <li>{@code infiniteSessions} - Infinite session config</li>
     *   <li>{@code skillDirectories} - Skill directory paths</li>
     *   <li>{@code disabledSkills} - Disabled skill names</li>
     *   <li>{@code requestHeaders} - Additional HTTP headers for model requests</li>
     *   <li>{@code responseFormat} - Response format ("text", "image", "json_object")</li>
     *   <li>{@code imageGeneration} - Image generation config map</li>
     * </ul>
     */
    @SuppressWarnings('unchecked')
    CopilotSession createSession(Map<String, Object> config = [:]) throws Exception {
        ensureConnected()

        Map<String, Object> payload = buildCreateSessionPayload(config)
        Map<String, Object> response = rpcClient.request('session.create', payload)

        String sessionId = (String) response.sessionId
        String workspacePath = (String) response.workspacePath
        CopilotSession session = new CopilotSession(sessionId, rpcClient, workspacePath)

        List<Tool> tools = (List<Tool>) config.tools
        session.registerTools(tools)

        Closure permHandler = (Closure) config.onPermissionRequest
        if (permHandler) session.registerPermissionHandler(permHandler)

        Closure inputHandler = (Closure) config.onUserInputRequest
        if (inputHandler) session.registerUserInputHandler(inputHandler)

        Closure elicitationHandler = (Closure) config.onElicitationRequest
        if (elicitationHandler) session.registerElicitationHandler(elicitationHandler)

        List<CommandDefinition> commands = (List<CommandDefinition>) config.commands
        if (commands) session.registerCommands(commands)

        sessions.put(sessionId, session)
        session
    }

    /**
     * Resumes an existing conversation session.
     */
    @SuppressWarnings('unchecked')
    CopilotSession resumeSession(String sessionId, Map<String, Object> config = [:]) throws Exception {
        ensureConnected()

        Map<String, Object> payload = buildCreateSessionPayload(config)
        payload.sessionId = sessionId
        Map<String, Object> response = rpcClient.request('session.resume', payload)

        String resumedId = (String) response.sessionId
        String workspacePath = (String) response.workspacePath
        CopilotSession session = new CopilotSession(resumedId, rpcClient, workspacePath)

        List<Tool> tools = (List<Tool>) config.tools
        session.registerTools(tools)

        Closure permHandler = (Closure) config.onPermissionRequest
        if (permHandler) session.registerPermissionHandler(permHandler)

        Closure inputHandler = (Closure) config.onUserInputRequest
        if (inputHandler) session.registerUserInputHandler(inputHandler)

        Closure elicitationHandler = (Closure) config.onElicitationRequest
        if (elicitationHandler) session.registerElicitationHandler(elicitationHandler)

        List<CommandDefinition> commands = (List<CommandDefinition>) config.commands
        if (commands) session.registerCommands(commands)

        sessions.put(resumedId, session)
        session
    }

    /** Gets the current connection state. */
    ConnectionState getState() { state }

    /** Pings the server. */
    PingResponse ping(String message = null) throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('ping', [message: message ?: ''])
        new PingResponse().with {
            it.message = (String) result.message
            it.timestamp = (result.timestamp as Number).longValue()
            if (result.containsKey('protocolVersion')) {
                it.protocolVersion = (result.protocolVersion as Number).intValue()
            }
            it
        }
    }

    /** Gets CLI status. */
    GetStatusResponse getStatus() throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('status.get', [:])
        new GetStatusResponse().with {
            version = (String) result.version
            protocolVersion = (result.protocolVersion as Number).intValue()
            it
        }
    }

    /** Gets authentication status. */
    @SuppressWarnings('unchecked')
    GetAuthStatusResponse getAuthStatus() throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('auth.getStatus', [:])
        new GetAuthStatusResponse().with {
            isAuthenticated = result.isAuthenticated as boolean
            authType = (String) result.authType
            host = (String) result.host
            login = (String) result.login
            statusMessage = (String) result.statusMessage
            it
        }
    }

    /** Lists available models (cached). */
    @SuppressWarnings('unchecked')
    List<ModelInfo> listModels() throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        synchronized (modelsCacheLock) {
            if (modelsCache != null) return new ArrayList<>(modelsCache)
            Map<String, Object> result = rpcClient.request('models.list', [:])
            List<Map<String, Object>> modelsData = (List<Map<String, Object>>) result.models
            List<ModelInfo> models = modelsData?.collect { Map<String, Object> m ->
                new ModelInfo().with {
                    id = (String) m.id
                    name = (String) m.name
                    it
                }
            } ?: []
            modelsCache = models
            new ArrayList<>(models)
        }
    }

    /** Lists all sessions. */
    @SuppressWarnings('unchecked')
    List<SessionMetadata> listSessions() throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('session.list', [:])
        List<Map<String, Object>> sessionsData = (List<Map<String, Object>>) result.sessions
        sessionsData?.collect { Map<String, Object> s ->
            new SessionMetadata().with {
                sessionId = (String) s.sessionId
                startTime = (String) s.startTime
                modifiedTime = (String) s.modifiedTime
                summary = (String) s.summary
                isRemote = s.isRemote as boolean
                it
            }
        } ?: []
    }

    /** Deletes a session by ID. */
    void deleteSession(String sessionId) throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('session.delete', [sessionId: sessionId])
        if (result.success != true) {
            throw new RuntimeException("Failed to delete session ${sessionId}: ${result.error ?: 'Unknown error'}")
        }
        sessions.remove(sessionId)
    }

    /** Gets foreground session ID (TUI mode). */
    String getForegroundSessionId() throws Exception {
        ensureConnected()
        Map<String, Object> result = rpcClient.request('session.getForeground', [:])
        (String) result.sessionId
    }

    /** Gets the last-used session ID, or null if none. */
    String getLastSessionId() throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('session.getLastId', [:])
        (String) result?.sessionId
    }

    /** Retrieves metadata for a specific session. */
    @SuppressWarnings('unchecked')
    Map<String, Object> getSessionMetadata(String sessionId) throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        rpcClient.request('session.getMetadata', [sessionId: sessionId])
    }

    /** Sets foreground session (TUI mode). */
    void setForegroundSessionId(String sessionId) throws Exception {
        ensureConnected()
        Map<String, Object> result = rpcClient.request('session.setForeground', [sessionId: sessionId])
        if (result.success != true) {
            throw new RuntimeException("Failed to set foreground session: ${result.error ?: 'Unknown'}")
        }
    }

    /**
     * Subscribes to all session lifecycle events.
     *
     * @param handler closure receiving (SessionLifecycleEvent event)
     * @return a Runnable that unsubscribes the handler when called
     */
    Runnable on(Closure handler) {
        lifecycleHandlers.add(handler)
        return { lifecycleHandlers.remove(handler) } as Runnable
    }

    /**
     * Subscribes to a specific lifecycle event type.
     *
     * @param eventType the lifecycle event type
     * @param handler closure receiving (SessionLifecycleEvent event)
     * @return a Runnable that unsubscribes the handler when called
     */
    Runnable on(String eventType, Closure handler) {
        typedLifecycleHandlers.computeIfAbsent(eventType) { new CopyOnWriteArrayList<Closure>() }.add(handler)
        return {
            CopyOnWriteArrayList<Closure> handlers = typedLifecycleHandlers.get(eventType)
            handlers?.remove(handler)
        } as Runnable
    }

    // --- Private methods ---

    private void ensureConnected() throws Exception {
        if (rpcClient) return
        if (autoStart) { start(); return }
        throw new IllegalStateException('Client not connected. Call start() first.')
    }

    @SuppressWarnings('unchecked')
    private Map<String, Object> buildCreateSessionPayload(Map<String, Object> config) {
        Map<String, Object> payload = [:]
        if (config.model) payload.model = config.model
        if (config.sessionId) payload.sessionId = config.sessionId
        if (config.reasoningEffort) payload.reasoningEffort = config.reasoningEffort
        if (config.configDir) payload.configDir = config.configDir
        if (config.tools) {
            List<Tool> tools = (List<Tool>) config.tools
            payload.tools = tools.collect { Tool t ->
                Map<String, Object> def_ = [name: t.name] as Map<String, Object>
                if (t.description) def_.description = t.description
                if (t.parameters) def_.parameters = t.parameters
                def_
            }
        }
        if (config.commands) {
            List<CommandDefinition> commands = (List<CommandDefinition>) config.commands
            payload.commands = commands.collect { CommandDefinition cmd ->
                Map<String, Object> def_ = [name: cmd.name] as Map<String, Object>
                if (cmd.description) def_.description = cmd.description
                def_
            }
        }
        if (config.systemMessage) payload.systemMessage = config.systemMessage
        if (config.availableTools) payload.availableTools = config.availableTools
        if (config.excludedTools) payload.excludedTools = config.excludedTools
        if (config.provider) payload.provider = config.provider
        if (config.modelCapabilities) payload.modelCapabilities = config.modelCapabilities
        if (config.onPermissionRequest) payload.requestPermission = true
        if (config.onUserInputRequest) payload.requestUserInput = true
        if (config.onElicitationRequest) payload.requestElicitation = true
        if (config.workingDirectory) payload.workingDirectory = config.workingDirectory
        if (config.streaming != null) payload.streaming = config.streaming
        if (config.includeSubAgentStreamingEvents != null) {
            payload.includeSubAgentStreamingEvents = config.includeSubAgentStreamingEvents
        } else {
            payload.includeSubAgentStreamingEvents = true
        }
        if (config.mcpServers) payload.mcpServers = config.mcpServers
        if (config.customAgents) payload.customAgents = config.customAgents
        if (config.skillDirectories) payload.skillDirectories = config.skillDirectories
        if (config.disabledSkills) payload.disabledSkills = config.disabledSkills
        if (config.infiniteSessions) payload.infiniteSessions = config.infiniteSessions
        if (config.enableConfigDiscovery != null) payload.enableConfigDiscovery = config.enableConfigDiscovery
        if (config.gitHubToken) payload.gitHubToken = config.gitHubToken
        if (config.requestHeaders) payload.requestHeaders = config.requestHeaders
        if (config.responseFormat) payload.responseFormat = config.responseFormat
        if (config.imageGeneration) payload.imageGeneration = config.imageGeneration
        payload
    }

    private void verifyProtocolVersion() throws Exception {
        int expected = SdkProtocolVersion.get()
        PingResponse pingResp = ping()
        if (pingResp.protocolVersion == null) {
            throw new RuntimeException(
                "SDK protocol version mismatch: SDK expects version ${expected}, " +
                "but server does not report a protocol version."
            )
        }
        if (pingResp.protocolVersion != expected) {
            throw new RuntimeException(
                "SDK protocol version mismatch: SDK expects version ${expected}, " +
                "but server reports version ${pingResp.protocolVersion}."
            )
        }
    }

    private void validateSessionFsConfig(SessionFsConfig config) {
        if (!config.initialCwd) {
            throw new IllegalArgumentException('sessionFs.initialCwd is required')
        }
        if (!config.sessionStatePath) {
            throw new IllegalArgumentException('sessionFs.sessionStatePath is required')
        }
        if (config.conventions != 'windows' && config.conventions != 'posix') {
            throw new IllegalArgumentException("sessionFs.conventions must be either 'windows' or 'posix'")
        }
    }

    private void startCLIServer() throws Exception {
        List<String> args = []

        if (cliPath.endsWith('.js')) {
            args << 'node' << cliPath
        } else {
            args << cliPath
        }

        args.addAll(cliArgs as List)
        args.addAll(['--headless', '--no-auto-update', '--log-level', logLevel])

        if (useStdio) {
            args << '--stdio'
        } else if (port > 0) {
            args << '--port' << port.toString()
        }

        if (githubToken) {
            args << '--auth-token-env' << 'COPILOT_SDK_AUTH_TOKEN'
        }
        if (!useLoggedInUser) {
            args << '--no-auto-login'
        }

        if (sessionIdleTimeoutSeconds != null && sessionIdleTimeoutSeconds > 0) {
            args << '--session-idle-timeout' << sessionIdleTimeoutSeconds.toString()
        }

        ProcessBuilder pb = new ProcessBuilder(args)
        if (cwd) pb.directory(new File(cwd))

        Map<String, String> processEnv = pb.environment()
        if (env) {
            processEnv.clear()
            processEnv.putAll(env)
        }
        if (githubToken) {
            processEnv.put('COPILOT_SDK_AUTH_TOKEN', githubToken)
        }

        pb.redirectErrorStream(false)
        cliProcess = pb.start()

        // Consume stderr in background
        Thread stderrThread = new Thread({
            try {
                cliProcess.errorStream.eachLine { /* discard */ }
            } catch (IOException ignored) {}
        } as Runnable, 'cli-stderr')
        stderrThread.daemon = true
        stderrThread.start()

        if (!useStdio) {
            BufferedReader reader = new BufferedReader(new InputStreamReader(cliProcess.inputStream))
            long deadline = System.currentTimeMillis() + 10_000
            String line
            while ((line = reader.readLine()) != null) {
                Matcher m = PORT_PATTERN.matcher(line)
                if (m.find()) {
                    actualPort = m.group(1) as int
                    return
                }
                if (System.currentTimeMillis() > deadline) {
                    throw new RuntimeException('Timeout waiting for CLI server to start')
                }
            }
            throw new RuntimeException('CLI process exited before announcing port')
        }
    }

    private void connectToServer() throws Exception {
        if (useStdio) {
            connectViaStdio()
        } else {
            connectViaTcp()
        }
    }

    private void connectViaStdio() {
        if (!cliProcess) throw new IllegalStateException('CLI process not started')
        rpcClient = new JsonRpcClient(cliProcess.inputStream, cliProcess.outputStream)
        setupHandlers()
        rpcClient.start()
    }

    private void connectViaTcp() throws Exception {
        if (actualPort == 0) throw new IllegalStateException('Server port not available')
        tcpSocket = new Socket(actualHost, actualPort)
        rpcClient = new JsonRpcClient(tcpSocket.inputStream, tcpSocket.outputStream)
        setupHandlers()
        rpcClient.start()
    }

    @SuppressWarnings('unchecked')
    private void setupHandlers() {
        rpcClient.notificationHandler = { String method, Map<String, Object> params ->
            if (method == 'session.event') {
                String sessionId = (String) params.sessionId
                Map<String, Object> event = (Map<String, Object>) params.event
                CopilotSession session = sessions.get(sessionId)
                if (session && event) {
                    session.dispatchEvent(event)
                }
            } else if (method == 'session.lifecycle') {
                dispatchLifecycleEvent(params)
            }
        }

        rpcClient.setRequestHandler('tool.call') { Map<String, Object> params ->
            handleToolCallRequest(params)
        }
        rpcClient.setRequestHandler('permission.request') { Map<String, Object> params ->
            handlePermissionRequest(params)
        }
        rpcClient.setRequestHandler('userInput.request') { Map<String, Object> params ->
            handleUserInputRequest(params)
        }
    }

    @SuppressWarnings('unchecked')
    private Map<String, Object> handleToolCallRequest(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.sessionId
        String toolCallId = (String) params.toolCallId
        String toolName = (String) params.toolName
        Object arguments = params.arguments

        CopilotSession session = sessions.get(sessionId)
        if (!session) throw new RuntimeException("Unknown session ${sessionId}")

        Closure handler = session.getToolHandler(toolName)
        if (!handler) {
            return [result: [
                textResultForLlm: "Tool '${toolName}' is not supported by this client instance.",
                resultType      : 'failure',
                error           : "tool '${toolName}' not supported",
                toolTelemetry   : [:]
            ]] as Map<String, Object>
        }

        try {
            ToolInvocation invocation = new ToolInvocation(sessionId, toolCallId, toolName, arguments)
            Object result = handler.call(arguments, invocation)
            [result: normalizeToolResult(result)] as Map<String, Object>
        } catch (Exception e) {
            [result: [
                textResultForLlm: 'Invoking this tool produced an error. Detailed information is not available.',
                resultType      : 'failure',
                error           : e.message,
                toolTelemetry   : [:]
            ]] as Map<String, Object>
        }
    }

    private Map<String, Object> handlePermissionRequest(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.sessionId
        CopilotSession session = sessions.get(sessionId)
        if (!session) throw new RuntimeException("Unknown session ${sessionId}")

        try {
            PermissionRequestResult result = session.handlePermissionRequest(params.permissionRequest)
            [result: [kind: result.kind]] as Map<String, Object>
        } catch (Exception e) {
            [result: [kind: 'denied-no-approval-rule-and-could-not-request-from-user']] as Map<String, Object>
        }
    }

    @SuppressWarnings('unchecked')
    private Map<String, Object> handleUserInputRequest(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.sessionId
        CopilotSession session = sessions.get(sessionId)
        if (!session) throw new RuntimeException("Unknown session ${sessionId}")

        UserInputResponse result = session.handleUserInputRequest(params)
        [answer: result.answer, wasFreeform: result.wasFreeform] as Map<String, Object>
    }

    private Object normalizeToolResult(Object result) {
        if (result == null) {
            return [textResultForLlm: 'Tool returned no result', resultType: 'failure',
                    error: 'tool returned no result', toolTelemetry: [:]]
        }
        if (result instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) result
            if (map.containsKey('textResultForLlm')) return result
        }
        String text = result instanceof String ? (String) result : result.toString()
        [textResultForLlm: text, resultType: 'success', toolTelemetry: [:]]
    }

    private void dispatchLifecycleEvent(Map<String, Object> params) {
        SessionLifecycleEvent event = new SessionLifecycleEvent().with {
            type = (String) params.type
            sessionId = (String) params.sessionId
            it
        }

        // Typed handlers
        CopyOnWriteArrayList<Closure> typed = typedLifecycleHandlers.get(event.type)
        typed?.each { Closure h ->
            try { h.call(event) } catch (Exception ignored) {}
        }
        // Wildcard handlers
        lifecycleHandlers.each { Closure h ->
            try { h.call(event) } catch (Exception ignored) {}
        }
    }
}
