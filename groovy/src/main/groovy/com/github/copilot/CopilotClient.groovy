/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import com.github.copilot.Types.*
import groovy.transform.CompileStatic
import groovy.transform.PackageScope

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
    private String copilotHome
    private String tcpConnectionToken
    private Closure requestHandler
    private Closure bearerTokenProvider
    /** Directories to load built-in plugins from. */
    private List<String> builtinPluginDirectories
    /** Use the in-process (FFI) transport instead of a subprocess. */
    private Boolean inProcess
    /** Identity declared on the {@code connect} handshake. */
    private ClientInfo clientInfo
    /** Client mode: {@code 'coding-agent'} (default) or {@code 'empty'}. */
    private String mode = ClientMode.CODING_AGENT
    /** Network callbacks registered with {@code network.setProvider} after connecting. */
    private NetworkConfig network
    /** GitHub token provider registrations keyed by registration ID. */
    private final ConcurrentHashMap<String, GitHubTokenRegistration> gitHubTokenProviders = new ConcurrentHashMap<>()

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
     *   <li>{@code copilotHome} - Configurable data directory, passed as COPILOT_HOME env var</li>
     *   <li>{@code tcpConnectionToken} - Auth token for TCP server connections</li>
     *   <li>{@code clientInfo} - {@link ClientInfo} (or map) declared on the {@code connect} handshake</li>
     *   <li>{@code mode} - {@code 'coding-agent'} (default; {@code 'copilot-cli'} is an alias) or {@code 'empty'}</li>
     *   <li>{@code network} - {@link NetworkConfig} (or map) with proxy resolution/authentication closures</li>
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
            if (options.copilotHome) this.copilotHome = options.copilotHome as String
            if (options.tcpConnectionToken) this.tcpConnectionToken = options.tcpConnectionToken as String
            if (options.requestHandler) this.requestHandler = options.requestHandler as Closure
            if (options.bearerTokenProvider) this.bearerTokenProvider = options.bearerTokenProvider as Closure
            if (options.builtinPluginDirectories) this.builtinPluginDirectories = options.builtinPluginDirectories as List<String>
            if (options.inProcess != null) this.inProcess = options.inProcess as Boolean
            if (options.clientInfo != null) this.clientInfo = toClientInfo(options.clientInfo)
            if (options.mode != null) this.mode = normalizeMode(options.mode as String)
            if (options.network != null) this.network = toNetworkConfig(options.network)
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

    /** Normalizes the {@code mode} option ({@code 'copilot-cli'} is a legacy alias of {@code 'coding-agent'}). */
    private static String normalizeMode(String value) {
        if (value == null || value == ClientMode.CODING_AGENT || value == ClientMode.COPILOT_CLI) return ClientMode.CODING_AGENT
        if (value == ClientMode.EMPTY) return ClientMode.EMPTY
        throw new IllegalArgumentException("Invalid mode '${value}'. Expected 'coding-agent' or 'empty'.")
    }

    @SuppressWarnings('unchecked')
    private static ClientInfo toClientInfo(Object value) {
        if (value instanceof ClientInfo) return (ClientInfo) value
        if (!(value instanceof Map)) throw new IllegalArgumentException('clientInfo must be a ClientInfo or a Map')
        Map<String, Object> map = (Map<String, Object>) value
        ClientInfo info = new ClientInfo()
        info.applicationName = (map.get('applicationName') ?: map.get('editorName')) as String
        info.applicationVersion = (map.get('applicationVersion') ?: map.get('editorVersion')) as String
        info.integrationName = (map.get('integrationName') ?: map.get('extensionName')) as String
        info.integrationVersion = (map.get('integrationVersion') ?: map.get('extensionVersion')) as String
        info
    }

    @SuppressWarnings('unchecked')
    private static NetworkConfig toNetworkConfig(Object value) {
        if (value instanceof NetworkConfig) return (NetworkConfig) value
        if (!(value instanceof Map)) throw new IllegalArgumentException('network must be a NetworkConfig or a Map')
        Map<String, Object> map = (Map<String, Object>) value
        NetworkConfig config = new NetworkConfig()
        config.scope = map.get('scope') as String
        config.resolveProxy = (Closure) map.get('resolveProxy')
        config.authenticateNegotiate = (Closure) map.get('authenticateNegotiate')
        config.authenticateBasic = (Closure) map.get('authenticateBasic')
        config
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
            sendConnectHandshake()
            verifyProtocolVersion()
            registerNetworkProvider()

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
        gitHubTokenProviders.clear()

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
        gitHubTokenProviders.clear()
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
     *   <li>{@code imageGeneration} - {@link ImageGenerationConfig} or map ({@code enabled}, {@code watermark}, {@code saveToDisk})</li>
     *   <li>{@code instructionDirectories} - Custom instruction directory paths</li>
     *   <li>{@code providers} / {@code models} / {@code defaultProvider} / {@code capi} - Named BYOK
     *       providers ({@link NamedProviderConfig}), model definitions ({@link ProviderModelConfig}),
     *       the default connection, and Copilot API options ({@link CapiConfig})</li>
     *   <li>{@code gitHubTokenProvider} - Closure receiving {@link GitHubTokenProviderArgs} and returning a
     *       {@link GitHubTokenProviderResult} (mutually exclusive with {@code gitHubToken})</li>
     *   <li>{@code onShellAuthenticationRequest} - Closure answering shell authentication prompts</li>
     *   <li>{@code permissionMode} - Initial permission mode ({@link PermissionMode})</li>
     *   <li>{@code askUserVariant}, {@code sandboxConfig}, {@code featureFlags}, {@code allowedModels},
     *       {@code authClientIdMetadataUrl}, {@code agentDirectories}, {@code hookDirectories},
     *       {@code trustWorkingDirectory} (create only), {@code autopilot}, {@code autoTier}</li>
     * </ul>
     *
     * <p>The SDK translates these flat options into the grouped SDK protocol 4 payload.
     */
    CopilotSession createSession(Map<String, Object> config = [:]) throws Exception {
        Map<String, Object> cfg = config ?: [:] as Map<String, Object>
        validateSessionOptions(cfg)
        ensureConnected()

        String registrationId = registerGitHubTokenProvider(cfg, cfg.sessionId as String)
        Map<String, Object> payload = buildCreateSessionPayload(cfg, false)
        if (registrationId != null) payload.gitHubTokenProviderRegistrationId = registrationId
        Map<String, Object> wire = groupSessionParams(payload, false)
        applyModeDefaults(wire)
        Map<String, Object> response
        try {
            response = rpcClient.request('session.create', wire)
        } catch (Exception e) {
            if (registrationId != null) gitHubTokenProviders.remove(registrationId)
            throw e
        }

        String sessionId = (String) response.sessionId
        String workspacePath = (String) response.workspacePath
        CopilotSession session = new CopilotSession(sessionId, rpcClient, workspacePath)
        registerSessionHandlers(session, cfg)
        commitGitHubTokenProvider(session, registrationId)
        sessions.put(sessionId, session)
        session
    }

    /**
     * Resumes an existing conversation session. Accepts the {@link #createSession} options plus
     * the resume-only {@code suppressResumeEvent}, {@code continuePendingWork}, and
     * {@code allowTranscriptRecovery}; pass {@code defaultProvider: null} to clear the default provider.
     */
    CopilotSession resumeSession(String sessionId, Map<String, Object> config = [:]) throws Exception {
        Map<String, Object> cfg = config ?: [:] as Map<String, Object>
        validateSessionOptions(cfg)
        ensureConnected()

        String registrationId = registerGitHubTokenProvider(cfg, sessionId)
        Map<String, Object> payload = buildCreateSessionPayload(cfg, true)
        payload.sessionId = sessionId
        if (registrationId != null) payload.gitHubTokenProviderRegistrationId = registrationId
        Map<String, Object> wire = groupSessionParams(payload, true)
        applyModeDefaults(wire)
        Map<String, Object> response
        try {
            response = rpcClient.request('session.resume', wire)
        } catch (Exception e) {
            if (registrationId != null) gitHubTokenProviders.remove(registrationId)
            throw e
        }

        String resumedId = (String) response.sessionId
        String workspacePath = (String) response.workspacePath
        CopilotSession session = new CopilotSession(resumedId, rpcClient, workspacePath)
        registerSessionHandlers(session, cfg)
        commitGitHubTokenProvider(session, registrationId)
        sessions.put(resumedId, session)
        session
    }

    /**
     * Named-argument form of {@link #resumeSession(String, Map)}: Groovy passes named arguments as
     * the leading map, so {@code client.resumeSession(sessionId, suppressResumeEvent: true)} lands here.
     */
    CopilotSession resumeSession(Map<String, Object> config, String sessionId) throws Exception {
        resumeSession(sessionId, config)
    }

    /**
     * Clears the runtime's cached enterprise managed settings for every account so the next
     * session resolution re-fetches server policy ({@code managedSettings.clearCache}).
     */
    void clearManagedSettingsCache() throws Exception {
        ensureConnected()
        rpcClient.request('managedSettings.clearCache', [:] as Map<String, Object>)
    }

    /** Gets the current connection state. */
    ConnectionState getState() { state }

    /** Pings the server. */
    PingResponse ping(String message = null) throws Exception {
        if (!rpcClient) throw new IllegalStateException('Client not connected')
        Map<String, Object> result = rpcClient.request('ping', [message: message ?: ''])
        new PingResponse().with {
            it.message = (String) result.message
            // Protocol 4 runtimes report an ISO-8601 timestamp; older ones reported epoch milliseconds.
            it.timestamp = result.timestamp?.toString()
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
    private Map<String, Object> buildCreateSessionPayload(Map<String, Object> config, boolean resume) {
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
                if (t.defer) def_.defer = t.defer
                if (t.idempotent != null) def_.idempotent = t.idempotent
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
        // An explicit empty list is meaningful (no tools), so only null is skipped.
        if (config.availableTools != null) payload.availableTools = config.availableTools
        if (config.excludedTools) payload.excludedTools = config.excludedTools
        // Legacy single BYOK provider: translated to inference.providers/models by groupSessionParams.
        if (config.provider) payload.provider = toWireMap(config.provider)
        if (config.modelCapabilities) {
            Object capabilities = config.modelCapabilities
            payload.modelCapabilities = capabilities instanceof ModelCapabilities ? ((ModelCapabilities) capabilities).toWire() : capabilities
        }
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
        if (config.imageGeneration) payload.imageGeneration = toWireMap(config.imageGeneration)
        if (config.instructionDirectories) payload.instructionDirectories = config.instructionDirectories

        // Hook registrations advertised to the server (served via hooks.invoke).
        if (config.hooks) {
            Map<String, Object> hooks = (Map<String, Object>) config.hooks
            List<String> hookTypes = []
            if (hooks.onPreToolUse) hookTypes.add('preToolUse')
            if (hooks.onPostToolUse) hookTypes.add('postToolUse')
            if (hooks.onPreMcpToolCall) hookTypes.add('preMcpToolCall')
            if (hooks.onUserPromptSubmitted) hookTypes.add('userPromptSubmitted')
            // Invoked after the user prompt has been transformed.
            if (hooks.onUserPromptTransformed) hookTypes.add('userPromptTransformed')
            if (hooks.onSessionStart) hookTypes.add('sessionStart')
            if (hooks.onSessionEnd) hookTypes.add('sessionEnd')
            if (hooks.onErrorOccurred) hookTypes.add('errorOccurred')
            // Sub-agent lifecycle: before a sub-agent's first turn and after it finishes.
            if (hooks.onSubagentStart) hookTypes.add('subagentStart')
            if (hooks.onSubagentStop) hookTypes.add('subagentStop')
            if (hookTypes) {
                payload.hooks = true
                payload.hookTypes = hookTypes
            }
        }

        // Additional session-config passthroughs.
        if (config.enableCitations != null) payload.enableCitations = config.enableCitations
        if (config.excludedBuiltinAgents) payload.excludedBuiltinAgents = config.excludedBuiltinAgents
        if (config.sessionLimits != null) {
            Object limCfg = config.sessionLimits
            Map<String, Object> limits = [:]
            if (limCfg instanceof SessionLimitsConfig) {
                Double credits = ((SessionLimitsConfig) limCfg).maxAiCredits
                if (credits != null) limits.maxAiCredits = credits
            } else if (limCfg instanceof Map) {
                Object c = ((Map<String, Object>) limCfg).get('maxAiCredits')
                if (c != null) limits.maxAiCredits = c
            }
            payload.sessionLimits = limits
        }
        if (config.memory != null) {
            payload.memory = [enabled: memoryEnabled(config.memory)] as Map<String, Object>
            if (memoryStoreClosure(config.memory) != null) payload.hasMemoryStore = true
        }
        if (config.otlpProtocol) payload.otlpProtocol = config.otlpProtocol
        if (config.expAssignments) payload.expAssignments = config.expAssignments
        // Enable session rewind to restore earlier session points.
        if (config.rewindEnabled != null) payload.rewindEnabled = config.rewindEnabled
        // Extra working directories available to the session.
        if (config.additionalDirectories) payload.additionalDirectories = config.additionalDirectories
        // Names of configured MCP servers to disable for this session.
        if (config.disabledMcpServers) payload.disabledMcpServers = config.disabledMcpServers
        // Configuration for the built-in GitHub MCP tools.
        if (config.githubMcpToolConfig) payload.githubMcpToolConfig = config.githubMcpToolConfig
        // Identity of the canvas provider to register.
        if (config.canvasProvider) payload.canvasProvider = config.canvasProvider
        // Only load workspace-local custom agents.
        if (config.customAgentsLocalOnly != null) payload.customAgentsLocalOnly = config.customAgentsLocalOnly
        // Tool search configuration for large tool sets.
        if (config.toolSearch) payload.toolSearch = config.toolSearch
        // Enable experimental runtime features (experiments.enableExperimentalMode).
        if (config.experimentalMode != null) payload.enableExperimentalMode = config.experimentalMode
        // Respect repository content-exclusion rules.
        if (config.contentExclusion != null) payload.contentExclusion = config.contentExclusion
        if (config.onMcpAuthRequest) payload.mcpAuthHandler = true

        // --- v2.6.0 session options ---
        // Copilot API provider options (github.capi): explicit capi members win over the flat options.
        Map<String, Object> capi = [:]
        if (config.autoTier != null) capi.autoTier = config.autoTier
        if (config.enableWebSocketResponses != null) capi.enableWebSocketResponses = config.enableWebSocketResponses
        if (config.capi != null) capi.putAll(toWireMap(config.capi))
        if (capi) payload.capi = capi
        if (config.askUserVariant) payload.askUserVariant = config.askUserVariant
        if (config.sandboxConfig) payload.sandboxConfig = toWireMap(config.sandboxConfig)
        if (config.featureFlags) payload.featureFlags = config.featureFlags
        if (config.allowedModels != null) payload.allowedModels = config.allowedModels
        if (config.authClientIdMetadataUrl) payload.authClientIdMetadataUrl = config.authClientIdMetadataUrl
        if (config.agentDirectories) payload.agentDirectories = config.agentDirectories
        if (config.hookDirectories) payload.hookDirectories = config.hookDirectories
        if (config.interactiveUser != null) payload.interactiveUser = config.interactiveUser
        if (config.excludedManagedSettings) payload.excludedManagedSettings = config.excludedManagedSettings
        if (config.enforceManagedModelDefaults != null) payload.enforceManagedModelDefaults = config.enforceManagedModelDefaults
        if (config.autopilot) payload.autopilot = toWireMap(config.autopilot)

        // --- v3.0.0 session options (SDK protocol 4) ---
        if (config.providers) payload.providers = ((List<Object>) config.providers).collect { Object p -> toWireMap(p) }
        if (config.models) payload.models = ((List<Object>) config.models).collect { Object m -> toWireMap(m) }
        if (config.defaultProvider != null) {
            payload.defaultProvider = config.defaultProvider
        } else if (resume && config.containsKey('defaultProvider')) {
            // An explicit null clears the default provider on resume.
            payload.defaultProvider = null
        }
        if (config.onShellAuthenticationRequest) payload.requestShellAuthentication = true
        if (config.permissionMode) payload.permissionMode = config.permissionMode
        if (resume) {
            if (config.allowTranscriptRecovery != null) payload.allowTranscriptRecovery = config.allowTranscriptRecovery
            if (config.suppressResumeEvent != null) payload.suppressResumeEvent = config.suppressResumeEvent
            if (config.continuePendingWork != null) payload.continuePendingWork = config.continuePendingWork
        } else {
            if (config.name) payload.name = config.name
            if (config.trustWorkingDirectory != null) payload.trustWorkingDirectory = config.trustWorkingDirectory
            if (config.refreshCustomInstructions != null) payload.refreshCustomInstructions = config.refreshCustomInstructions
        }
        payload
    }

    /** Converts an SDK option object ({@code toMap()} types) or map to its wire map. */
    @SuppressWarnings('unchecked')
    private static Map<String, Object> toWireMap(Object value) {
        if (value instanceof Map) return (Map<String, Object>) value
        if (value instanceof ProviderConfig) return ((ProviderConfig) value).toMap()
        if (value instanceof NamedProviderConfig) return ((NamedProviderConfig) value).toMap()
        if (value instanceof ProviderModelConfig) return ((ProviderModelConfig) value).toMap()
        if (value instanceof CapiConfig) return ((CapiConfig) value).toMap()
        if (value instanceof SandboxConfig) return ((SandboxConfig) value).toMap()
        if (value instanceof ImageGenerationConfig) return ((ImageGenerationConfig) value).toMap()
        if (value instanceof AutopilotOptions) return ((AutopilotOptions) value).toMap()
        throw new IllegalArgumentException("Unsupported session option value: ${value?.getClass()?.name}")
    }

    /** Whether a {@code memory} option ({@link MemoryConfiguration} or map) enables memory (default true). */
    @SuppressWarnings('unchecked')
    private static boolean memoryEnabled(Object memory) {
        if (memory instanceof MemoryConfiguration) return ((MemoryConfiguration) memory).enabled
        if (memory instanceof Map) {
            Object enabled = ((Map<String, Object>) memory).get('enabled')
            return enabled == null || enabled == true
        }
        true
    }

    /** The {@code storeMemory} closure of a {@code memory} option, or null. */
    @SuppressWarnings('unchecked')
    private static Closure memoryStoreClosure(Object memory) {
        if (memory instanceof MemoryConfiguration) return ((MemoryConfiguration) memory).storeMemory
        if (memory instanceof Map) return (Closure) ((Map<String, Object>) memory).get('storeMemory')
        null
    }

    /**
     * Builds the JSON-RPC {@code session.send} payload from a message options map.
     *
     * <p>Protocol 4 accepts only the structured response format: {@code responseSchema} is sent as
     * a {@code json_schema} response format and a map {@code responseFormat} is passed through,
     * while the legacy string {@code responseFormat} ("text", "image", "json_object") is not sent.
     *
     * @param sessionId the session identifier
     * @param options   map with keys: prompt, attachments, mode, responseSchema, responseFormat,
     *                  imageOptions, requestHeaders, agentMode, displayPrompt, source, clientTurnId
     * @return the request payload
     */
    static Map<String, Object> buildSendPayload(String sessionId, Map<String, Object> options) {
        Map<String, Object> params = [sessionId: sessionId] as Map<String, Object>
        params.prompt = options.prompt
        if (options.attachments) params.attachments = options.attachments
        if (options.mode) params.mode = options.mode
        if (options.responseSchema != null) {
            params.responseFormat = [
                type      : 'json_schema',
                jsonSchema: [name: 'response', strict: true, schema: options.responseSchema]
            ] as Map<String, Object>
        } else if (options.responseFormat instanceof Map) {
            params.responseFormat = options.responseFormat
        }
        if (options.imageOptions) params.imageOptions = options.imageOptions
        if (options.requestHeaders) params.requestHeaders = options.requestHeaders
        if (options.agentMode) params.agentMode = options.agentMode
        if (options.displayPrompt) params.displayPrompt = options.displayPrompt
        // Message provenance: 'user', 'system', or 'agent-<id>' (see MessageSource).
        if (options.source) params.source = options.source
        // Opaque caller-chosen ID echoed on the turn's events.
        if (options.clientTurnId) params.clientTurnId = options.clientTurnId
        params
    }

    /** Tool deferral policies for the {@code defer} field. */
    static class ToolDefer {
        static final String AUTO = 'auto'
        static final String NEVER = 'never'
    }

    /** Known system message section identifiers for section overrides. */
    static class SystemMessageSection {
        static final String PREAMBLE = 'preamble'
        static final String IDENTITY = 'identity'
        static final String TOOL_INSTRUCTIONS = 'tool_instructions'
        static final String PRESERVE = 'preserve'
    }

    /** GitHub attachment type constants. */
    static class GitHubAttachment {
        static final String GITHUB_COMMIT = 'GitHubCommit'
        static final String GITHUB_REPOSITORY = 'GitHubRepository'
        static final String GITHUB_PULL_REQUEST = 'GitHubPullRequest'
        static final String GITHUB_ISSUE = 'GitHubIssue'
    }

    // --- SDK protocol 4: grouped session payloads ---

    /** Flat keys that stay at the root of protocol 4 create/resume payloads. */
    private static final Set<String> ROOT_SESSION_KEYS = [
        'sessionId', 'model', 'name', 'clientName', 'workingDirectory', 'configDirectory',
        'enableConfigDiscovery', 'systemMessage', 'diagnostics', 'lifecycleDiagnostics', 'cloud',
        'traceparent', 'tracestate', 'suppressResumeEvent', 'continuePendingWork',
        'allowTranscriptRecovery', 'requestedEnvironmentVariables'
    ] as Set<String>

    /** Flat keys accepted only by {@code session.create}; dropped on resume. */
    private static final Set<String> CREATE_ONLY_SESSION_KEYS = [
        'name', 'cloud', 'trustWorkingDirectory', 'refreshCustomInstructions'
    ] as Set<String>

    /** Flat keys accepted only by {@code session.resume}; dropped on create. */
    private static final Set<String> RESUME_ONLY_SESSION_KEYS = [
        'suppressResumeEvent', 'continuePendingWork', 'allowTranscriptRecovery',
        'requestedEnvironmentVariables', 'openCanvases'
    ] as Set<String>

    /** Option groups of protocol 4 create/resume payloads. */
    private static final Set<String> SESSION_OPTION_GROUPS = [
        'inference', 'tools', 'permissions', 'interaction', 'events', 'agents', 'skills', 'mcp',
        'github', 'filesystem', 'execution', 'instructions', 'hooks', 'git', 'state', 'plugins',
        'canvases', 'extensions', 'experiments', 'policy'
    ] as Set<String>

    /** Legacy provider fields that describe the model rather than the connection. */
    private static final Set<String> PROVIDER_MODEL_FIELDS = [
        'modelId', 'wireModel', 'maxPromptTokens', 'maxContextWindowTokens', 'maxOutputTokens', 'capabilities'
    ] as Set<String>

    /** Flat keys mapped to their protocol 4 {@code [group, member]} location. */
    private static final Map<String, List<String>> GROUPED_SESSION_KEYS = [
        reasoningEffort                   : ['inference', 'reasoningEffort'],
        reasoningSummary                  : ['inference', 'reasoningSummary'],
        contextTier                       : ['inference', 'contextTier'],
        allowMultipleToolCalls            : ['inference', 'allowMultipleToolCalls'],
        enableCitations                   : ['inference', 'enableCitations'],
        modelCapabilities                 : ['inference', 'capabilities'],
        verbosity                         : ['inference', 'verbosity'],
        providers                         : ['inference', 'providers'],
        models                            : ['inference', 'models'],
        defaultProvider                   : ['inference', 'defaultProvider'],
        tools                             : ['tools', 'definitions'],
        availableTools                    : ['tools', 'availableTools'],
        excludedTools                     : ['tools', 'excludedTools'],
        toolSearch                        : ['tools', 'search'],
        largeOutput                       : ['tools', 'largeOutput'],
        imageGeneration                   : ['tools', 'imageGeneration'],
        manageScheduleEnabled             : ['tools', 'manageScheduleEnabled'],
        webSearch                         : ['tools', 'webSearch'],
        shell                             : ['tools', 'shell'],
        maxInlineBinaryBytes              : ['tools', 'maxInlineBinaryBytes'],
        requestPermission                 : ['permissions', 'request'],
        sandboxConfig                     : ['permissions', 'sandbox'],
        sandbox                           : ['permissions', 'sandbox'],
        permissionMode                    : ['permissions', 'mode'],
        sandboxConfigSource               : ['permissions', 'sandboxSource'],
        commands                          : ['interaction', 'commands'],
        askUserVariant                    : ['interaction', 'askUserVariant'],
        requestUserInput                  : ['interaction', 'requestUserInput'],
        requestElicitation                : ['interaction', 'requestElicitation'],
        requestShellAuthentication        : ['interaction', 'requestShellAuthentication'],
        requestExitPlanMode               : ['interaction', 'requestExitPlanMode'],
        requestAutoModeSwitch             : ['interaction', 'requestAutoModeSwitch'],
        requestAutoModeRecovery           : ['interaction', 'requestAutoModeRecovery'],
        streaming                         : ['events', 'streaming'],
        includeSubAgentStreamingEvents    : ['events', 'includeSubAgentStreamingEvents'],
        customAgents                      : ['agents', 'definitions'],
        defaultAgent                      : ['agents', 'default'],
        agent                             : ['agents', 'selected'],
        includedBuiltinAgents             : ['agents', 'includedBuiltinAgents'],
        excludedBuiltinAgents             : ['agents', 'excludedBuiltinAgents'],
        customAgentsLocalOnly             : ['agents', 'localDiscoveryOnly'],
        agentDirectories                  : ['agents', 'directories'],
        skillDirectories                  : ['skills', 'directories'],
        enableSkills                      : ['skills', 'enabled'],
        includedBuiltinSkills             : ['skills', 'includedBuiltinSkills'],
        disabledSkills                    : ['skills', 'disabledSkills'],
        hasSkillProvider                  : ['skills', 'hasProvider'],
        ignoredSkillsLocations            : ['skills', 'ignoredLocations'],
        mcpServers                        : ['mcp', 'servers'],
        disabledMcpServers                : ['mcp', 'disabledServers'],
        requestMcpApps                    : ['mcp', 'enableApps'],
        enableMcpApps                     : ['mcp', 'enableApps'],
        mcpOAuthTokenStorage              : ['mcp', 'oauthTokenStorage'],
        authClientIdMetadataUrl           : ['mcp', 'authClientIdMetadataUrl'],
        envValueMode                      : ['mcp', 'envValueMode'],
        managedMcpServers                 : ['mcp', 'managedServers'],
        allowAllMcpServerInstructions     : ['mcp', 'allowAllServerInstructions'],
        gitHubToken                       : ['github', 'token'],
        gitHubTokenProviderRegistrationId : ['github', 'tokenProviderRegistrationId'],
        githubMcpToolConfig               : ['github', 'mcp'],
        capi                              : ['github', 'capi'],
        enableSessionTelemetry            : ['github', 'telemetryEnabled'],
        enableManagedSettings             : ['github', 'fetchManagedSettings'],
        interactiveUser                   : ['github', 'interactiveUser'],
        enableGitHubTelemetryForwarding   : ['github', 'telemetryForwarding'],
        authInfo                          : ['github', 'authInfo'],
        copilotUrl                        : ['github', 'copilotUrl'],
        additionalDirectories             : ['filesystem', 'additionalDirectories'],
        enableFileChangeTracking          : ['filesystem', 'trackFileChanges'],
        trustWorkingDirectory             : ['filesystem', 'trustWorkingDirectory'],
        sessionLimits                     : ['execution', 'limits'],
        autopilot                         : ['execution', 'autopilot'],
        autopilotContinuation             : ['execution', 'autopilotContinuation'],
        instructionDirectories            : ['instructions', 'directories'],
        organizationCustomInstructions    : ['instructions', 'organizationText'],
        enableOnDemandInstructionDiscovery: ['instructions', 'onDemandDiscovery'],
        skipCustomInstructions            : ['instructions', 'skipCustomInstructions'],
        refreshCustomInstructions         : ['instructions', 'refreshCustomInstructions'],
        hooks                             : ['hooks', 'hasHandlers'],
        hookDirectories                   : ['hooks', 'directories'],
        enableFileHooks                   : ['hooks', 'enableRepositoryHooks'],
        coauthorEnabled                   : ['git', 'coauthorEnabled'],
        enableHostGitOperations           : ['git', 'enableHostGitOperations'],
        infiniteSessions                  : ['state', 'infiniteSessions'],
        memory                            : ['state', 'memory'],
        hasMemoryStore                    : ['state', 'hasMemoryStore'],
        enableSessionStore                : ['state', 'enableCrossSessionStore'],
        skipEmbeddingRetrieval            : ['state', 'skipEmbeddingRetrieval'],
        embeddingCacheStorage             : ['state', 'embeddingCacheStorage'],
        pluginDirectories                 : ['plugins', 'directories'],
        installedPlugins                  : ['plugins', 'installed'],
        canvases                          : ['canvases', 'definitions'],
        canvasProvider                    : ['canvases', 'provider'],
        requestCanvasRenderer             : ['canvases', 'requestRenderer'],
        openCanvases                      : ['canvases', 'open'],
        requestExtensions                 : ['extensions', 'enabled'],
        extensionSdkPath                  : ['extensions', 'sdkPath'],
        extensionInfo                     : ['extensions', 'identity'],
        enableExperimentalMode            : ['experiments', 'enableExperimentalMode'],
        isExperimentalMode                : ['experiments', 'enableExperimentalMode'],
        featureFlags                      : ['experiments', 'flags'],
        expAssignments                    : ['experiments', 'assignments'],
        featureFlagConfiguration          : ['experiments', 'configuration'],
        allowedModels                     : ['policy', 'allowedModels'],
        managedSettings                   : ['policy', 'managedSettings'],
        excludedManagedSettings           : ['policy', 'excludedManagedSettings'],
        enforceManagedModelDefaults       : ['policy', 'enforceManagedModelDefaults']
    ] as Map<String, List<String>>

    /**
     * Converts the flat {@code session.create}/{@code session.resume} payload this SDK builds into
     * the SDK protocol 4 grouped payload. Root keys stay at the top level, known flat keys move to
     * their option group, unknown keys pass through unchanged, null values are skipped (except an
     * explicit {@code defaultProvider} clear on resume), empty groups are omitted, and members of an
     * already-grouped object win over flat keys. A legacy single {@code provider} becomes the named
     * {@code 'default'} connection.
     *
     * @param flat the flat payload
     * @param resume whether the payload is for {@code session.resume}
     * @return the grouped payload
     */
    @PackageScope
    @SuppressWarnings('unchecked')
    static Map<String, Object> groupSessionParams(Map<String, Object> flat, boolean resume) {
        Map<String, Object> root = [:]
        Map<String, Map<String, Object>> groups = [:]
        // Already-grouped objects first, so their members win over flat keys.
        for (Map.Entry<String, Object> entry : flat.entrySet()) {
            if (SESSION_OPTION_GROUPS.contains(entry.key) && entry.value instanceof Map) {
                Map<String, Object> group = groupFor(groups, entry.key)
                for (Map.Entry<Object, Object> member : ((Map<Object, Object>) entry.value).entrySet()) {
                    if (member.value != null) group.put(String.valueOf(member.key), member.value)
                }
            }
        }
        Object legacyProvider = null
        for (Map.Entry<String, Object> entry : flat.entrySet()) {
            String key = entry.key
            Object value = entry.value
            if (SESSION_OPTION_GROUPS.contains(key) && value instanceof Map) continue
            if (key == 'configDir') key = 'configDirectory'
            else if (key == 'disableResume') key = 'suppressResumeEvent'
            if (resume ? CREATE_ONLY_SESSION_KEYS.contains(key) : RESUME_ONLY_SESSION_KEYS.contains(key)) continue
            // A null default provider is an explicit clear on resume; every other null is skipped.
            if (value == null && !(resume && key == 'defaultProvider')) continue
            if (key == 'provider') {
                legacyProvider = value
                continue
            }
            List<String> path = ROOT_SESSION_KEYS.contains(key) ? null : GROUPED_SESSION_KEYS.get(key)
            if (path == null) {
                if (!root.containsKey(key)) root.put(key, value)
            } else {
                Map<String, Object> group = groupFor(groups, path[0])
                if (!group.containsKey(path[1])) group.put(path[1], value)
            }
        }
        Map<String, Object> mcp = groups.get('mcp')
        if (mcp != null && mcp.containsKey('servers') && !mcp.containsKey('envValueMode')) mcp.put('envValueMode', 'direct')
        if (legacyProvider instanceof Map) translateLegacyProvider((Map<Object, Object>) legacyProvider, root.get('model'), groups)
        for (Map.Entry<String, Map<String, Object>> group : groups.entrySet()) {
            if (!group.value.isEmpty()) root.put(group.key, group.value)
        }
        root
    }

    private static Map<String, Object> groupFor(Map<String, Map<String, Object>> groups, String name) {
        Map<String, Object> group = groups.get(name)
        if (group == null) {
            group = [:]
            groups.put(name, group)
        }
        group
    }

    /**
     * Translates a legacy single BYOK {@code provider} into the named {@code 'default'} connection
     * (plus a model definition when model-specific fields are present), selects it as the default
     * provider, and disables Copilot API models as v1 BYOK did. Explicit {@code providers},
     * {@code defaultProvider}, or {@code capi.enabled} win: the legacy provider is then dropped.
     */
    @SuppressWarnings('unchecked')
    private static void translateLegacyProvider(Map<Object, Object> provider, Object model,
                                                Map<String, Map<String, Object>> groups) {
        Map<String, Object> inference = groupFor(groups, 'inference')
        Map<String, Object> github = groupFor(groups, 'github')
        Object capi = github.get('capi')
        if (inference.containsKey('providers') || inference.containsKey('defaultProvider')
                || (capi instanceof Map && ((Map) capi).containsKey('enabled'))) {
            return
        }
        Map<String, Object> connection = [name: 'default'] as Map<String, Object>
        Map<String, Object> modelFields = [:]
        for (Map.Entry<Object, Object> field : provider.entrySet()) {
            String name = String.valueOf(field.key)
            if (field.value == null || name == 'name') continue
            if (PROVIDER_MODEL_FIELDS.contains(name)) {
                modelFields.put(name, field.value)
            } else {
                connection.put(name, field.value)
            }
        }
        inference.put('providers', [connection])
        inference.put('defaultProvider', 'default')
        if (!modelFields.isEmpty() && model != null && !inference.containsKey('models')) {
            Map<String, Object> definition = [provider: 'default', id: model] as Map<String, Object>
            definition.putAll(modelFields)
            inference.put('models', [definition])
        }
        Map<String, Object> capiWire = [:]
        if (capi instanceof Map) capiWire.putAll((Map<String, Object>) capi)
        capiWire.put('enabled', false)
        github.put('capi', capiWire)
    }

    /**
     * Empty mode: applies safe runtime defaults under the caller's values (caller values win).
     * No-op in the default {@code 'coding-agent'} mode.
     */
    private void applyModeDefaults(Map<String, Object> wire) {
        if (mode != ClientMode.EMPTY) return
        putGroupDefault(wire, 'experiments', 'enableExperimentalMode', false)
        putGroupDefault(wire, 'github', 'telemetryEnabled', false)
        putGroupDefault(wire, 'mcp', 'oauthTokenStorage', 'in-memory')
        putGroupDefault(wire, 'state', 'skipEmbeddingRetrieval', true)
        putGroupDefault(wire, 'state', 'embeddingCacheStorage', 'in-memory')
        putGroupDefault(wire, 'state', 'enableCrossSessionStore', false)
        putGroupDefault(wire, 'state', 'memory', [enabled: false] as Map<String, Object>)
        putGroupDefault(wire, 'instructions', 'onDemandDiscovery', false)
        putGroupDefault(wire, 'hooks', 'enableRepositoryHooks', false)
        putGroupDefault(wire, 'git', 'enableHostGitOperations', false)
    }

    @SuppressWarnings('unchecked')
    private static void putGroupDefault(Map<String, Object> wire, String group, String member, Object value) {
        Object existing = wire.get(group)
        Map<String, Object> target
        if (existing instanceof Map) {
            target = (Map<String, Object>) existing
        } else {
            target = [:]
            wire.put(group, target)
        }
        if (!target.containsKey(member)) target.put(member, value)
    }

    /**
     * Validates options before create/resume: empty mode requires {@code availableTools},
     * {@code gitHubTokenProvider} excludes {@code gitHubToken}, and {@code memory.storeMemory}
     * requires memory to be enabled.
     */
    private void validateSessionOptions(Map<String, Object> config) {
        if (mode == ClientMode.EMPTY && config.availableTools == null) {
            throw new IllegalArgumentException("CopilotClient is in mode 'empty' but the session config did not specify " +
                'availableTools. Empty mode requires every session to explicitly opt into the tools it wants.')
        }
        if (config.gitHubTokenProvider != null && config.gitHubToken) {
            throw new IllegalArgumentException('gitHubToken and gitHubTokenProvider are mutually exclusive')
        }
        if (config.memory != null && memoryStoreClosure(config.memory) != null && !memoryEnabled(config.memory)) {
            throw new IllegalArgumentException('memory.storeMemory requires memory.enabled to be true.')
        }
    }

    /** Registers the session's local handlers (tools, permission, input, hooks, shell authentication, memory store). */
    @SuppressWarnings('unchecked')
    private static void registerSessionHandlers(CopilotSession session, Map<String, Object> config) {
        session.registerTools((List<Tool>) config.tools)

        Closure permHandler = (Closure) config.onPermissionRequest
        if (permHandler) session.registerPermissionHandler(permHandler)

        Closure inputHandler = (Closure) config.onUserInputRequest
        if (inputHandler) session.registerUserInputHandler(inputHandler)

        Closure elicitationHandler = (Closure) config.onElicitationRequest
        if (elicitationHandler) session.registerElicitationHandler(elicitationHandler)

        List<CommandDefinition> commands = (List<CommandDefinition>) config.commands
        if (commands) session.registerCommands(commands)

        if (config.hooks instanceof Map) session.registerHooks((Map<String, Object>) config.hooks)

        Closure shellAuthHandler = (Closure) config.onShellAuthenticationRequest
        if (shellAuthHandler) session.registerShellAuthenticationHandler(shellAuthHandler)

        Closure storeMemory = config.memory != null ? memoryStoreClosure(config.memory) : null
        if (storeMemory) session.registerMemoryStore(storeMemory)
    }

    /**
     * Declares the integrating application's identity via {@code connect} before the ping
     * handshake. Skipped when no client info field is set; ignored by older CLIs.
     */
    private void sendConnectHandshake() throws Exception {
        Map<String, Object> wire = clientInfo?.toWire()
        if (wire == null) return
        try {
            rpcClient.request('connect', [clientInfo: wire] as Map<String, Object>)
        } catch (Exception e) {
            if (!JsonRpcClient.isMethodNotFound(e, 'connect')) throw e
        }
    }

    /** Registers the configured network callbacks with {@code network.setProvider} (no-op without callbacks). */
    private void registerNetworkProvider() throws Exception {
        NetworkConfig config = network
        if (config == null || !config.hasCallbacks()) return
        Map<String, Object> params = [:]
        if (config.scope) params.scope = config.scope
        if (config.resolveProxy != null) params.resolveProxy = [:]
        if (config.authenticateNegotiate != null) params.authenticateNegotiate = [:]
        if (config.authenticateBasic != null) params.authenticateBasic = [:]
        rpcClient.request('network.setProvider', params)
    }

    private String registerGitHubTokenProvider(Map<String, Object> config, String sessionId) {
        Object provider = config.gitHubTokenProvider
        if (provider == null) return null
        if (!(provider instanceof Closure)) throw new IllegalArgumentException('gitHubTokenProvider must be a Closure')
        String registrationId = UUID.randomUUID().toString()
        gitHubTokenProviders.put(registrationId, new GitHubTokenRegistration((Closure) provider, sessionId))
        registrationId
    }

    /**
     * Binds a successful create/resume to its provider registration: drops registrations committed
     * by an earlier open of the same session and removes the new one when the session is destroyed.
     */
    private void commitGitHubTokenProvider(CopilotSession session, String registrationId) {
        String sessionId = session.sessionId
        if (sessionId != null) {
            Iterator<Map.Entry<String, GitHubTokenRegistration>> it = gitHubTokenProviders.entrySet().iterator()
            while (it.hasNext()) {
                Map.Entry<String, GitHubTokenRegistration> entry = it.next()
                if (entry.value.committed && sessionId == entry.value.sessionId && entry.key != registrationId) it.remove()
            }
        }
        if (registrationId == null) return
        GitHubTokenRegistration registration = gitHubTokenProviders.get(registrationId)
        if (registration == null) return
        registration.sessionId = sessionId
        registration.committed = true
        ConcurrentHashMap<String, GitHubTokenRegistration> registrations = gitHubTokenProviders
        session.registerOnDestroyed({ registrations.remove(registrationId) } as Runnable)
    }

    @SuppressWarnings('unchecked')
    private Map<String, Object> handleGitHubTokenRequest(Map<String, Object> params) throws Exception {
        Object registrationId = params.registrationId
        GitHubTokenRegistration registration = registrationId instanceof String ? gitHubTokenProviders.get((String) registrationId) : null
        if (registration == null) {
            throw new IllegalStateException("No GitHub token provider registered for registration ID \"${registrationId}\"")
        }
        Object requestedSessionId = params.sessionId
        GitHubTokenProviderArgs args = new GitHubTokenProviderArgs()
        args.host = params.host as String
        args.sessionId = requestedSessionId instanceof String ? (String) requestedSessionId : registration.sessionId
        args.reason = params.reason as String
        Object result = registration.provider.call(args)
        if (result instanceof GitHubTokenProviderResult) return ((GitHubTokenProviderResult) result).toMap()
        if (result instanceof Map) return (Map<String, Object>) result
        throw new IllegalStateException('GitHub token provider returned no result')
    }

    /** A session's GitHub token provider registration. */
    private static final class GitHubTokenRegistration {
        public final Closure provider
        public volatile String sessionId
        public volatile boolean committed

        GitHubTokenRegistration(Closure provider, String sessionId) {
            this.provider = provider
            this.sessionId = sessionId
        }
    }

    private void verifyProtocolVersion() throws Exception {
        int expected = SdkProtocolVersion.get()
        PingResponse pingResp = ping()
        if (pingResp.protocolVersion == null) {
            throw new RuntimeException(
                "SDK protocol version mismatch: SDK supports version ${expected}, " +
                "but server does not report a protocol version. Please update your server to ensure compatibility."
            )
        }
        if (pingResp.protocolVersion != expected) {
            throw new RuntimeException(
                "SDK protocol version mismatch: SDK supports version ${expected}, " +
                "but server reports version ${pingResp.protocolVersion}. " +
                'Please update your SDK or server to ensure compatibility.'
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

        args.addAll(cliArgs as List<String>)
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
        rpcClient.setRequestHandler('hooks.invoke') { Map<String, Object> params ->
            handleHooksInvoke(params)
        }
        rpcClient.setRequestHandler('gitHubToken.getToken') { Map<String, Object> params ->
            handleGitHubTokenRequest(params)
        }
        rpcClient.setRequestHandler('shellAuthentication.request') { Map<String, Object> params ->
            handleShellAuthenticationRequest(params)
        }
        rpcClient.setRequestHandler('shellAuthentication.cancel') { Map<String, Object> params ->
            handleShellAuthenticationCancel(params)
        }
        rpcClient.setRequestHandler('memoryStore.store') { Map<String, Object> params ->
            handleMemoryStoreRequest(params)
        }
        setupNetworkHandlers()
    }

    private CopilotSession sessionFor(Object sessionId) {
        sessionId instanceof String ? sessions.get((String) sessionId) : null
    }

    /** Runs the owning session's hook closure for {@code hooks.invoke}; answers {@code {output}} or {@code {}}. */
    private Map<String, Object> handleHooksInvoke(Map<String, Object> params) throws Exception {
        CopilotSession session = sessionFor(params.sessionId)
        if (session == null) throw new RuntimeException("Unknown session ${params.sessionId}")
        Object output = session.handleHooksInvoke(params.hookType as String, params.input)
        Map<String, Object> result = [:]
        if (output != null) result.output = output
        result
    }

    /** Answers {@code shellAuthentication.request} with the owning session's handler ({@code cancel} when unavailable). */
    private Map<String, Object> handleShellAuthenticationRequest(Map<String, Object> params) {
        CopilotSession session = sessionFor(params.sessionId)
        if (session == null) return [action: 'cancel'] as Map<String, Object>
        session.handleShellAuthenticationRequest(params)
    }

    /** Cancels a pending shell authentication prompt; always answers {@code {}}. */
    private Map<String, Object> handleShellAuthenticationCancel(Map<String, Object> params) {
        CopilotSession session = sessionFor(params.sessionId)
        Object requestId = params.requestId
        if (session != null && requestId instanceof String) session.cancelShellAuthentication((String) requestId)
        [:] as Map<String, Object>
    }

    /** Stores a memory through the owning session's closure and answers with a JSON {@code null} result. */
    private Map<String, Object> handleMemoryStoreRequest(Map<String, Object> params) throws Exception {
        CopilotSession session = sessionFor(params.sessionId)
        if (session == null) throw new IllegalStateException('Memory store owner is unavailable')
        session.handleMemoryStore(params)
        JsonRpcClient.NULL_RESULT
    }

    /** Serves the configured network callbacks. Tokens and passwords are never logged. */
    @SuppressWarnings('unchecked')
    private void setupNetworkHandlers() {
        NetworkConfig config = network
        if (config == null) return
        if (config.resolveProxy != null) {
            Closure resolver = config.resolveProxy
            rpcClient.setRequestHandler('network.resolveProxy') { Map<String, Object> params ->
                NetworkResolveProxyRequest request = new NetworkResolveProxyRequest()
                request.url = (params.url ?: '') as String
                Object proxy = resolver.call(request)
                Map<String, Object> result = [:]
                if (proxy != null) result.proxy = proxy.toString()
                result
            }
        }
        if (config.authenticateNegotiate != null) {
            Closure authenticator = config.authenticateNegotiate
            rpcClient.setRequestHandler('network.authenticateNegotiate') { Map<String, Object> params ->
                NetworkAuthenticateNegotiateRequest request = new NetworkAuthenticateNegotiateRequest()
                request.spn = (params.spn ?: '') as String
                Object token = authenticator.call(request)
                Map<String, Object> result = [:]
                if (token != null) result.token = token.toString()
                result
            }
        }
        if (config.authenticateBasic != null) {
            Closure authenticator = config.authenticateBasic
            rpcClient.setRequestHandler('network.authenticateBasic') { Map<String, Object> params ->
                NetworkAuthenticateBasicRequest request = new NetworkAuthenticateBasicRequest()
                request.proxy = (params.proxy ?: '') as String
                request.realm = params.realm as String
                request.previousRejected = params.previousRejected == true
                Object credentials = authenticator.call(request)
                // Send only the credential fields.
                Map<String, Object> result = [:]
                if (credentials instanceof NetworkBasicCredentials) {
                    NetworkBasicCredentials basic = (NetworkBasicCredentials) credentials
                    if (basic.username != null) result.username = basic.username
                    if (basic.password != null) result.password = basic.password
                } else if (credentials instanceof Map) {
                    Map<String, Object> basic = (Map<String, Object>) credentials
                    if (basic.username != null) result.username = basic.username.toString()
                    if (basic.password != null) result.password = basic.password.toString()
                }
                result
            }
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
