/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;
import com.github.copilot.Types.*;
import com.github.copilot.internal.JsonRpcClient;

import java.io.*;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Main client for interacting with the Copilot CLI.
 *
 * <p>The CopilotClient manages the connection to the Copilot CLI server and provides
 * methods to create and manage conversation sessions. It can either spawn a CLI
 * server process or connect to an existing server.
 *
 * <pre>{@code
 * // Create a client with default options (spawns CLI server)
 * CopilotClient client = new CopilotClient();
 *
 * // Or connect to an existing server
 * CopilotClient client = new CopilotClient(new CopilotClientOptions().cliUrl("localhost:3000"));
 *
 * // Create a session
 * CopilotSession session = client.createSession(new SessionConfig().model("gpt-4"));
 *
 * // Send messages and handle responses
 * session.on(event -> {
 *     if ("assistant.message".equals(event.type)) {
 *         System.out.println(event.data.get("content"));
 *     }
 * });
 * session.sendAndWait(new MessageOptions("Hello!"));
 *
 * // Clean up
 * session.destroy();
 * client.stop();
 * }</pre>
 */
public class CopilotClient {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Pattern PORT_PATTERN = Pattern.compile("listening on port (\\d+)", Pattern.CASE_INSENSITIVE);

    private Process cliProcess;
    private JsonRpcClient rpcClient;
    private Socket tcpSocket;
    private int actualPort;
    private String actualHost = "localhost";
    private ConnectionState state = ConnectionState.DISCONNECTED;
    private final ConcurrentHashMap<String, CopilotSession> sessions = new ConcurrentHashMap<>();
    private final boolean isExternalServer;
    private boolean useStdio = true;
    private boolean autoStart = true;
    private boolean autoRestart = true;
    private volatile List<ModelInfo> modelsCache;
    private final Object modelsCacheLock = new Object();
    private final CopyOnWriteArrayList<SessionLifecycleHandler> lifecycleHandlers = new CopyOnWriteArrayList<>();
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<SessionLifecycleHandler>> typedLifecycleHandlers = new ConcurrentHashMap<>();

    // Resolved options
    private String cliPath = "copilot";
    private String[] cliArgs = new String[0];
    private String cwd;
    private int port;
    private String logLevel = "info";
    private Map<String, String> env;
    private String githubToken;
    private boolean useLoggedInUser = true;
    private Integer sessionIdleTimeoutSeconds;
    private Types.SessionFsConfig sessionFsConfig;
    /** Intercepts outbound LLM inference HTTP/WebSocket requests, when configured. */
    private Types.CopilotRequestHandler requestHandler;
    /** BYOK provider that supplies per-session bearer tokens, when configured. */
    private Types.BearerTokenProvider bearerTokenProvider;
    /** Integrating application identity sent on the {@code connect} handshake, when configured. */
    private Types.ClientInfo clientInfo;
    /** Client mode: {@code "coding-agent"} (default) or {@code "empty"}. */
    private String mode = Types.ClientMode.CODING_AGENT;
    /** Network callbacks registered with {@code network.setProvider} after connecting, when configured. */
    private Types.NetworkConfig network;
    /** Session GitHub token providers keyed by opaque registration ID (served via {@code gitHubToken.getToken}). */
    private final ConcurrentHashMap<String, GitHubTokenRegistration> githubTokenProviders = new ConcurrentHashMap<>();

    /**
     * Creates a new CopilotClient with default options.
     */
    public CopilotClient() {
        this(null);
    }

    /**
     * Creates a new CopilotClient with the given options.
     *
     * @param options the client options
     */
    public CopilotClient(CopilotClientOptions options) {
        boolean external = false;

        if (options != null) {
            // Validate mutually exclusive options
            if (options.getCliUrl() != null && (options.getUseStdio() != null || options.getCliPath() != null)) {
                throw new IllegalArgumentException("cliUrl is mutually exclusive with useStdio and cliPath");
            }
            if (options.getCliUrl() != null && (options.getGithubToken() != null || options.getUseLoggedInUser() != null)) {
                throw new IllegalArgumentException("githubToken and useLoggedInUser cannot be used with cliUrl");
            }

            if (options.getCliUrl() != null) {
                int[] hostPort = parseCliUrl(options.getCliUrl());
                this.actualPort = hostPort[1];
                external = true;
                this.useStdio = false;
            }

            if (options.getCliPath() != null) this.cliPath = options.getCliPath();
            if (options.getCliArgs() != null) this.cliArgs = options.getCliArgs();
            if (options.getCwd() != null) this.cwd = options.getCwd();
            if (options.getPort() > 0) { this.port = options.getPort(); this.useStdio = false; }
            if (options.getLogLevel() != null) this.logLevel = options.getLogLevel();
            if (options.getUseStdio() != null) this.useStdio = options.getUseStdio();
            if (options.getAutoStart() != null) this.autoStart = options.getAutoStart();
            if (options.getAutoRestart() != null) this.autoRestart = options.getAutoRestart();
            if (options.getEnv() != null) this.env = options.getEnv();
            if (options.getGithubToken() != null) {
                this.githubToken = options.getGithubToken();
                this.useLoggedInUser = false;
            }
            if (options.getUseLoggedInUser() != null) this.useLoggedInUser = options.getUseLoggedInUser();
            if (options.getSessionIdleTimeoutSeconds() != null) this.sessionIdleTimeoutSeconds = options.getSessionIdleTimeoutSeconds();
            if (options.getSessionFs() != null) this.sessionFsConfig = options.getSessionFs();
            // Upstream-sync client-side interceptors: consulted by the runtime transport when present.
            if (options.getRequestHandler() != null) this.requestHandler = options.getRequestHandler();
            if (options.getBearerTokenProvider() != null) this.bearerTokenProvider = options.getBearerTokenProvider();
            if (options.getClientInfo() != null) this.clientInfo = options.getClientInfo();
            if (options.getMode() != null) this.mode = normalizeMode(options.getMode());
            if (options.getNetwork() != null) this.network = options.getNetwork();
        }

        this.isExternalServer = external;

        // Check env for CLI path
        String envCliPath = System.getenv("COPILOT_CLI_PATH");
        if (envCliPath != null && !envCliPath.isEmpty()) {
            this.cliPath = envCliPath;
        }
    }

    /** Validates a client mode, mapping the legacy {@code "copilot-cli"} alias to {@code "coding-agent"}. */
    private static String normalizeMode(String mode) {
        if (Types.ClientMode.CODING_AGENT.equals(mode) || Types.ClientMode.COPILOT_CLI.equals(mode)) {
            return Types.ClientMode.CODING_AGENT;
        }
        if (Types.ClientMode.EMPTY.equals(mode)) return Types.ClientMode.EMPTY;
        throw new IllegalArgumentException("mode must be 'coding-agent' or 'empty', got '" + mode + "'");
    }

    private int[] parseCliUrl(String url) {
        String clean = url.replaceAll("^https?://", "");
        if (clean.matches("\\d+")) {
            int p = Integer.parseInt(clean);
            this.actualHost = "localhost";
            return new int[]{0, p};
        }
        String[] parts = clean.split(":");
        if (parts.length != 2) throw new IllegalArgumentException("Invalid cliUrl format: " + url);
        this.actualHost = parts[0].isEmpty() ? "localhost" : parts[0];
        int p = Integer.parseInt(parts[1]);
        if (p <= 0 || p > 65535) throw new IllegalArgumentException("Invalid port: " + url);
        return new int[]{0, p};
    }

    /**
     * Starts the CLI server and establishes a connection.
     */
    public void start() throws Exception {
        if (state == ConnectionState.CONNECTED) return;
        state = ConnectionState.CONNECTING;

        try {
            if (!isExternalServer) {
                startCLIServer();
            }
            connectToServer();
            sendConnectHandshake();
            verifyProtocolVersion();
            registerNetworkProvider();

            // If a session filesystem provider was configured, register it
            if (sessionFsConfig != null) {
                Map<String, Object> fsParams = new HashMap<>();
                if (sessionFsConfig.initialCwd != null) fsParams.put("initialCwd", sessionFsConfig.initialCwd);
                if (sessionFsConfig.sessionStatePath != null) fsParams.put("sessionStatePath", sessionFsConfig.sessionStatePath);
                if (sessionFsConfig.conventions != null) fsParams.put("conventions", sessionFsConfig.conventions);
                rpcClient.request("sessionFs.setProvider", fsParams);
            }

            state = ConnectionState.CONNECTED;
        } catch (Exception e) {
            state = ConnectionState.ERROR;
            throw e;
        }
    }

    /**
     * Stops the CLI server and closes all active sessions.
     *
     * @return list of errors encountered during cleanup
     */
    public List<StopError> stop() {
        List<StopError> errors = new ArrayList<>();

        for (CopilotSession session : sessions.values()) {
            try {
                session.destroy();
            } catch (Exception e) {
                errors.add(new StopError("Failed to destroy session " + session.getSessionId() + ": " + e.getMessage()));
            }
        }
        sessions.clear();
        githubTokenProviders.clear();

        if (rpcClient != null) {
            rpcClient.stop();
            rpcClient = null;
        }

        synchronized (modelsCacheLock) {
            modelsCache = null;
        }

        if (tcpSocket != null) {
            try { tcpSocket.close(); } catch (IOException e) {
                errors.add(new StopError("Failed to close socket: " + e.getMessage()));
            }
            tcpSocket = null;
        }

        if (cliProcess != null && !isExternalServer) {
            cliProcess.destroyForcibly();
            cliProcess = null;
        }

        state = ConnectionState.DISCONNECTED;
        if (!isExternalServer) actualPort = 0;
        return errors;
    }

    /** Forcefully stops the CLI server without graceful cleanup. */
    public void forceStop() {
        sessions.clear();
        githubTokenProviders.clear();
        if (rpcClient != null) { rpcClient.stop(); rpcClient = null; }
        synchronized (modelsCacheLock) { modelsCache = null; }
        if (tcpSocket != null) { try { tcpSocket.close(); } catch (IOException ignored) {} tcpSocket = null; }
        if (cliProcess != null && !isExternalServer) { cliProcess.destroyForcibly(); cliProcess = null; }
        state = ConnectionState.DISCONNECTED;
        if (!isExternalServer) actualPort = 0;
    }

    /**
     * Creates a new conversation session.
     */
    @SuppressWarnings("unchecked")
    public CopilotSession createSession(SessionConfig config) throws Exception {
        if (config == null) config = new SessionConfig();
        validateGitHubTokenOptions(config);
        validateSessionOptions(config);
        ensureConnected();

        String registrationId = registerGitHubTokenProvider(config.getGitHubTokenProvider(), config.getSessionId());
        Map<String, Object> payload = buildCreateSessionPayload(config, false);
        if (registrationId != null) payload.put("gitHubTokenProviderRegistrationId", registrationId);
        Map<String, Object> wire = groupSessionParams(payload, false);
        applyModeDefaults(wire);
        Map<String, Object> response;
        try {
            response = rpcClient.request("session.create", wire);
        } catch (Exception e) {
            if (registrationId != null) githubTokenProviders.remove(registrationId);
            throw e;
        }

        String sessionId = (String) response.get("sessionId");
        String workspacePath = (String) response.get("workspacePath");
        CopilotSession session = new CopilotSession(sessionId, rpcClient, workspacePath);
        registerSessionHandlers(session, config);
        commitGitHubTokenProvider(session, registrationId);
        sessions.put(sessionId, session);
        return session;
    }

    /** Creates a session with default configuration. */
    public CopilotSession createSession() throws Exception {
        return createSession(null);
    }

    /**
     * Resumes an existing conversation session.
     */
    @SuppressWarnings("unchecked")
    public CopilotSession resumeSession(String sessionId, SessionConfig config) throws Exception {
        if (config == null) config = new SessionConfig();
        validateGitHubTokenOptions(config);
        validateSessionOptions(config);
        ensureConnected();

        String registrationId = registerGitHubTokenProvider(config.getGitHubTokenProvider(), sessionId);
        Map<String, Object> payload = buildCreateSessionPayload(config, true);
        payload.put("sessionId", sessionId);
        if (registrationId != null) payload.put("gitHubTokenProviderRegistrationId", registrationId);
        Map<String, Object> wire = groupSessionParams(payload, true);
        applyModeDefaults(wire);
        Map<String, Object> response;
        try {
            response = rpcClient.request("session.resume", wire);
        } catch (Exception e) {
            if (registrationId != null) githubTokenProviders.remove(registrationId);
            throw e;
        }

        String resumedId = (String) response.get("sessionId");
        String workspacePath = (String) response.get("workspacePath");
        CopilotSession session = new CopilotSession(resumedId, rpcClient, workspacePath);
        registerSessionHandlers(session, config);
        commitGitHubTokenProvider(session, registrationId);
        sessions.put(resumedId, session);
        return session;
    }

    public CopilotSession resumeSession(String sessionId) throws Exception {
        return resumeSession(sessionId, null);
    }

    /** Gets the current connection state. */
    public ConnectionState getState() { return state; }

    /** Pings the server. */
    public PingResponse ping(String message) throws Exception {
        if (rpcClient == null) throw new IllegalStateException("Client not connected");
        Map<String, Object> result = rpcClient.request("ping", Map.of("message", message != null ? message : ""));
        PingResponse resp = new PingResponse();
        resp.message = (String) result.get("message");
        // Protocol 4 runtimes report an ISO-8601 timestamp; older ones reported epoch milliseconds.
        Object timestamp = result.get("timestamp");
        if (timestamp instanceof Number) {
            resp.timestamp = ((Number) timestamp).longValue();
        } else if (timestamp instanceof String) {
            try {
                resp.timestamp = java.time.Instant.parse((String) timestamp).toEpochMilli();
            } catch (java.time.format.DateTimeParseException ignored) {
                // Leave the timestamp unset when the runtime reports an unexpected format.
            }
        }
        if (result.containsKey("protocolVersion")) {
            resp.protocolVersion = ((Number) result.get("protocolVersion")).intValue();
        }
        return resp;
    }

    /** Gets CLI status. */
    public GetStatusResponse getStatus() throws Exception {
        if (rpcClient == null) throw new IllegalStateException("Client not connected");
        Map<String, Object> result = rpcClient.request("status.get", Map.of());
        GetStatusResponse resp = new GetStatusResponse();
        resp.version = (String) result.get("version");
        resp.protocolVersion = ((Number) result.get("protocolVersion")).intValue();
        return resp;
    }

    /** Gets authentication status. */
    public GetAuthStatusResponse getAuthStatus() throws Exception {
        if (rpcClient == null) throw new IllegalStateException("Client not connected");
        Map<String, Object> result = rpcClient.request("auth.getStatus", Map.of());
        return MAPPER.convertValue(result, GetAuthStatusResponse.class);
    }

    /** Lists available models (cached). */
    @SuppressWarnings("unchecked")
    public List<ModelInfo> listModels() throws Exception {
        if (rpcClient == null) throw new IllegalStateException("Client not connected");
        synchronized (modelsCacheLock) {
            if (modelsCache != null) return new ArrayList<>(modelsCache);
            Map<String, Object> result = rpcClient.request("models.list", Map.of());
            List<Map<String, Object>> modelsData = (List<Map<String, Object>>) result.get("models");
            List<ModelInfo> models = new ArrayList<>();
            if (modelsData != null) {
                for (Map<String, Object> m : modelsData) {
                    models.add(MAPPER.convertValue(m, ModelInfo.class));
                }
            }
            modelsCache = models;
            return new ArrayList<>(models);
        }
    }

    /** Gets the ID of the most recently updated session. */
    public String getLastSessionId() throws Exception {
        ensureConnected();
        Map<String, Object> result = rpcClient.request("session.getLastId", Map.of());
        return (String) result.get("sessionId");
    }

    /** Gets metadata for a specific session. */
    public SessionMetadata getSessionMetadata(String sessionId) throws Exception {
        ensureConnected();
        Map<String, Object> result = rpcClient.request("session.getMetadata", Map.of("sessionId", sessionId));
        return MAPPER.convertValue(result, SessionMetadata.class);
    }

    /** Lists all sessions. */
    @SuppressWarnings("unchecked")
    public List<SessionMetadata> listSessions() throws Exception {
        if (rpcClient == null) throw new IllegalStateException("Client not connected");
        Map<String, Object> result = rpcClient.request("session.list", Map.of());
        List<Map<String, Object>> sessionsData = (List<Map<String, Object>>) result.get("sessions");
        List<SessionMetadata> list = new ArrayList<>();
        if (sessionsData != null) {
            for (Map<String, Object> s : sessionsData) {
                list.add(MAPPER.convertValue(s, SessionMetadata.class));
            }
        }
        return list;
    }

    /** Deletes a session. */
    public void deleteSession(String sessionId) throws Exception {
        if (rpcClient == null) throw new IllegalStateException("Client not connected");
        Map<String, Object> result = rpcClient.request("session.delete", Map.of("sessionId", sessionId));
        boolean success = Boolean.TRUE.equals(result.get("success"));
        if (!success) {
            throw new RuntimeException("Failed to delete session " + sessionId + ": " + result.getOrDefault("error", "Unknown error"));
        }
        sessions.remove(sessionId);
    }

    /** Gets foreground session ID (TUI mode). */
    public String getForegroundSessionId() throws Exception {
        ensureConnected();
        Map<String, Object> result = rpcClient.request("session.getForeground", Map.of());
        return (String) result.get("sessionId");
    }

    /** Sets foreground session (TUI mode). */
    public void setForegroundSessionId(String sessionId) throws Exception {
        ensureConnected();
        Map<String, Object> result = rpcClient.request("session.setForeground", Map.of("sessionId", sessionId));
        if (!Boolean.TRUE.equals(result.get("success"))) {
            throw new RuntimeException("Failed to set foreground session: " + result.getOrDefault("error", "Unknown"));
        }
    }

    /**
     * Clears the runtime's cached enterprise managed settings for every account so the next
     * session resolution re-fetches server policy.
     */
    public void clearManagedSettingsCache() throws Exception {
        ensureConnected();
        rpcClient.request("managedSettings.clearCache", new HashMap<String, Object>());
    }

    /**
     * Saves a legacy MCP configuration credential in the runtime-owned secret store
     * ({@code mcp.secrets.save}), using the runtime's default configuration directory.
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param secretId identifier of the credential
     * @param value the credential; it is never logged
     * @return the result ({@code usedKeychain} is true when the OS keychain stored the credential)
     * @throws Exception if the request fails
     */
    public McpSecretsSaveResult mcpSecretsSave(String secretId, String value) throws Exception {
        return mcpSecretsSave(secretId, value, null);
    }

    /**
     * Saves a legacy MCP configuration credential in the runtime-owned secret store
     * ({@code mcp.secrets.save}). Requires Copilot CLI 1.0.116 or newer.
     *
     * @param secretId identifier of the credential
     * @param value the credential; it is never logged
     * @param configDir configuration directory, or null for the runtime's default
     * @return the result ({@code usedKeychain} is true when the OS keychain stored the credential
     *         and false when the runtime used its file backend)
     * @throws Exception if the request fails
     */
    public McpSecretsSaveResult mcpSecretsSave(String secretId, String value, String configDir) throws Exception {
        ensureConnected();
        Map<String, Object> params = new HashMap<>();
        params.put("secretId", secretId);
        params.put("value", value);
        if (configDir != null) params.put("configDir", configDir);
        return McpSecretsSaveResult.fromMap(rpcClient.request("mcp.secrets.save", params));
    }

    /**
     * Deletes a legacy MCP configuration credential from the runtime-owned secret store
     * ({@code mcp.secrets.delete}), using the runtime's default configuration directory.
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param secretId identifier of the credential
     * @throws Exception if the request fails
     */
    public void mcpSecretsDelete(String secretId) throws Exception {
        mcpSecretsDelete(secretId, null);
    }

    /**
     * Deletes a legacy MCP configuration credential from the runtime-owned secret store
     * ({@code mcp.secrets.delete}). Requires Copilot CLI 1.0.116 or newer.
     *
     * @param secretId identifier of the credential
     * @param configDir configuration directory, or null for the runtime's default
     * @throws Exception if the request fails
     */
    public void mcpSecretsDelete(String secretId, String configDir) throws Exception {
        ensureConnected();
        Map<String, Object> params = new HashMap<>();
        params.put("secretId", secretId);
        if (configDir != null) params.put("configDir", configDir);
        rpcClient.request("mcp.secrets.delete", params);
    }

    /**
     * Deletes the legacy credentials indexed for an MCP server ({@code mcp.secrets.deleteServer}),
     * using the runtime's default configuration directory. Requires Copilot CLI 1.0.116 or newer.
     *
     * @param serverName the MCP server name
     * @throws Exception if the request fails
     */
    public void mcpSecretsDeleteServer(String serverName) throws Exception {
        mcpSecretsDeleteServer(serverName, null);
    }

    /**
     * Deletes the legacy credentials indexed for an MCP server ({@code mcp.secrets.deleteServer}).
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param serverName the MCP server name
     * @param configDir configuration directory, or null for the runtime's default
     * @throws Exception if the request fails
     */
    public void mcpSecretsDeleteServer(String serverName, String configDir) throws Exception {
        ensureConnected();
        Map<String, Object> params = new HashMap<>();
        params.put("serverName", serverName);
        if (configDir != null) params.put("configDir", configDir);
        rpcClient.request("mcp.secrets.deleteServer", params);
    }

    /**
     * Persists a pending plugin extension permission request as a user-scoped approval, before
     * the host answers that request normally ({@code permissions.extensionApprovals.grant}).
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param sessionId session that owns the pending request
     * @param requestId ID of the pending extension permission request
     * @return whether the request was eligible and persisted, and the stored approval
     * @throws Exception if the request fails
     */
    public PermissionsExtensionApprovalsGrantResult extensionApprovalsGrant(String sessionId, String requestId) throws Exception {
        ensureConnected();
        Map<String, Object> params = new HashMap<>();
        params.put("sessionId", sessionId);
        params.put("requestId", requestId);
        return PermissionsExtensionApprovalsGrantResult.fromMap(
            rpcClient.request("permissions.extensionApprovals.grant", params));
    }

    /**
     * Lists the user-scoped plugin extension approvals of the server's configuration profile
     * ({@code permissions.extensionApprovals.list}). Requires Copilot CLI 1.0.116 or newer.
     *
     * @return the approvals
     * @throws Exception if the request fails
     */
    public PermissionsExtensionApprovalsListResult extensionApprovalsList() throws Exception {
        return extensionApprovalsList(null);
    }

    /**
     * Lists the user-scoped plugin extension approvals ({@code permissions.extensionApprovals.list}).
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param sessionId attached session whose configuration profile to list, or null for the
     *        server's profile
     * @return the approvals
     * @throws Exception if the request fails
     */
    public PermissionsExtensionApprovalsListResult extensionApprovalsList(String sessionId) throws Exception {
        ensureConnected();
        Map<String, Object> params = new HashMap<>();
        if (sessionId != null) params.put("sessionId", sessionId);
        return PermissionsExtensionApprovalsListResult.fromMap(
            rpcClient.request("permissions.extensionApprovals.list", params));
    }

    /**
     * Revokes one user-scoped plugin extension approval ({@code permissions.extensionApprovals.revoke}).
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param extensionName the approval's {@code extensionName}
     * @param installationKey the approval's {@code installationKey}, unchanged
     * @return whether a matching approval was revoked
     * @throws Exception if the request fails
     */
    public PermissionsExtensionApprovalsRevokeResult extensionApprovalsRevoke(String extensionName, String installationKey)
            throws Exception {
        return extensionApprovalsRevoke(extensionName, installationKey, null);
    }

    /**
     * Revokes one user-scoped plugin extension approval ({@code permissions.extensionApprovals.revoke}).
     * Requires Copilot CLI 1.0.116 or newer.
     *
     * @param extensionName the approval's {@code extensionName}
     * @param installationKey the approval's {@code installationKey}, unchanged
     * @param sessionId attached session whose configuration profile to use, or null for the
     *        server's profile
     * @return whether a matching approval was revoked
     * @throws Exception if the request fails
     */
    public PermissionsExtensionApprovalsRevokeResult extensionApprovalsRevoke(
            String extensionName, String installationKey, String sessionId) throws Exception {
        ensureConnected();
        Map<String, Object> params = new HashMap<>();
        params.put("extensionName", extensionName);
        params.put("installationKey", installationKey);
        if (sessionId != null) params.put("sessionId", sessionId);
        return PermissionsExtensionApprovalsRevokeResult.fromMap(
            rpcClient.request("permissions.extensionApprovals.revoke", params));
    }

    /** Subscribes to all session lifecycle events. */
    public Runnable on(SessionLifecycleHandler handler) {
        lifecycleHandlers.add(handler);
        return () -> lifecycleHandlers.remove(handler);
    }

    /** Subscribes to a specific lifecycle event type. */
    public Runnable on(String eventType, SessionLifecycleHandler handler) {
        typedLifecycleHandlers.computeIfAbsent(eventType, k -> new CopyOnWriteArrayList<>()).add(handler);
        return () -> {
            CopyOnWriteArrayList<SessionLifecycleHandler> handlers = typedLifecycleHandlers.get(eventType);
            if (handlers != null) handlers.remove(handler);
        };
    }

    // --- Private methods ---

    private void ensureConnected() throws Exception {
        if (rpcClient != null) return;
        if (autoStart) { start(); return; }
        throw new IllegalStateException("Client not connected. Call start() first.");
    }

    /**
     * Declares the integrating application's identity via {@code connect} before the ping
     * handshake. Skipped when no client info field is set; ignored by older CLIs.
     */
    private void sendConnectHandshake() throws Exception {
        Map<String, Object> wire = clientInfo != null ? clientInfo.toWire() : null;
        if (wire == null) return;
        Map<String, Object> params = new HashMap<>();
        params.put("clientInfo", wire);
        try {
            rpcClient.request("connect", params);
        } catch (Exception e) {
            if (!JsonRpcClient.isMethodNotFound(e, "connect")) throw e;
        }
    }

    private static void validateGitHubTokenOptions(SessionConfig config) {
        if (config.getGitHubTokenProvider() != null && (config.getGitHubToken() != null || config.getAuthToken() != null)) {
            throw new IllegalArgumentException("gitHubToken and gitHubTokenProvider are mutually exclusive");
        }
    }

    private void validateSessionOptions(SessionConfig config) {
        if (Types.ClientMode.EMPTY.equals(mode) && config.getAvailableTools() == null) {
            throw new IllegalArgumentException("CopilotClient is in mode 'empty' but the session config did not specify "
                + "availableTools. Empty mode requires every session to explicitly opt into the tools it wants.");
        }
        Types.MemoryConfiguration memory = config.getMemory();
        if (memory != null && memory.storeMemory != null && !Boolean.TRUE.equals(memory.enabled)) {
            throw new IllegalArgumentException("memory.storeMemory requires memory.enabled to be true.");
        }
    }

    /** Registers the session's local handlers (tools, permission, input, hooks, shell authentication, memory store). */
    private static void registerSessionHandlers(CopilotSession session, SessionConfig config) {
        session.registerTools(config.getTools());
        if (config.getOnPermissionRequest() != null) session.registerPermissionHandler(config.getOnPermissionRequest());
        if (config.getOnUserInputRequest() != null) session.registerUserInputHandler(config.getOnUserInputRequest());
        if (config.getHooks() != null) session.registerHooks(config.getHooks());
        if (config.getOnShellAuthenticationRequest() != null) {
            session.registerShellAuthenticationHandler(config.getOnShellAuthenticationRequest());
        }
        if (config.getMemory() != null && config.getMemory().storeMemory != null) {
            session.registerMemoryStore(config.getMemory().storeMemory);
        }
    }

    /** Registers the configured network callbacks with {@code network.setProvider} (no-op without callbacks). */
    private void registerNetworkProvider() throws Exception {
        if (network == null || !network.hasCallbacks()) return;
        Map<String, Object> params = new HashMap<>();
        if (network.scope != null) params.put("scope", network.scope);
        if (network.resolveProxy != null) params.put("resolveProxy", new HashMap<String, Object>());
        if (network.authenticateNegotiate != null) params.put("authenticateNegotiate", new HashMap<String, Object>());
        if (network.authenticateBasic != null) params.put("authenticateBasic", new HashMap<String, Object>());
        rpcClient.request("network.setProvider", params);
    }

    private String registerGitHubTokenProvider(GitHubTokenProvider provider, String sessionId) {
        if (provider == null) return null;
        String registrationId = UUID.randomUUID().toString();
        githubTokenProviders.put(registrationId, new GitHubTokenRegistration(provider, sessionId));
        return registrationId;
    }

    /**
     * Binds a successful create/resume to its provider registration: drops registrations committed
     * by an earlier open of the same session and removes the new one when the session is destroyed.
     */
    private void commitGitHubTokenProvider(CopilotSession session, String registrationId) {
        String sessionId = session.getSessionId();
        if (sessionId != null) {
            githubTokenProviders.entrySet().removeIf(entry -> entry.getValue().committed
                && sessionId.equals(entry.getValue().sessionId)
                && !entry.getKey().equals(registrationId));
        }
        if (registrationId == null) return;
        GitHubTokenRegistration registration = githubTokenProviders.get(registrationId);
        if (registration == null) return;
        registration.sessionId = sessionId;
        registration.committed = true;
        session.registerOnDestroyed(() -> githubTokenProviders.remove(registrationId));
    }

    private Map<String, Object> handleGitHubTokenRequest(Map<String, Object> params) throws Exception {
        Object registrationId = params.get("registrationId");
        GitHubTokenRegistration registration = registrationId instanceof String
            ? githubTokenProviders.get((String) registrationId) : null;
        if (registration == null) {
            throw new IllegalStateException("No GitHub token provider registered for registration ID \"" + registrationId + "\"");
        }
        Object requestedSessionId = params.get("sessionId");
        GitHubTokenProviderArgs args = new GitHubTokenProviderArgs(
            (String) params.get("host"),
            requestedSessionId instanceof String ? (String) requestedSessionId : registration.sessionId,
            (String) params.get("reason"));
        GitHubTokenProviderResult result = registration.provider.getToken(args);
        if (result == null) {
            throw new IllegalStateException("GitHub token provider returned no result");
        }
        return result.toMap();
    }

    /** A session's GitHub token provider registration. */
    private static final class GitHubTokenRegistration {
        final GitHubTokenProvider provider;
        volatile String sessionId;
        volatile boolean committed;

        GitHubTokenRegistration(GitHubTokenProvider provider, String sessionId) {
            this.provider = provider;
            this.sessionId = sessionId;
        }
    }

    private Map<String, Object> buildCreateSessionPayload(SessionConfig config, boolean resume) {
        Map<String, Object> payload = new HashMap<>();
        if (config.getModel() != null) payload.put("model", config.getModel());
        if (config.getSessionId() != null) payload.put("sessionId", config.getSessionId());
        if (config.getReasoningEffort() != null) payload.put("reasoningEffort", config.getReasoningEffort());
        if (config.getConfigDir() != null) payload.put("configDir", config.getConfigDir());
        if (config.getTools() != null) {
            List<Map<String, Object>> toolDefs = new ArrayList<>();
            for (Types.Tool tool : config.getTools()) {
                Map<String, Object> def = new HashMap<>();
                def.put("name", tool.name);
                if (tool.description != null) def.put("description", tool.description);
                if (tool.parameters != null) def.put("parameters", tool.parameters);
                if (tool.defer != null) def.put("defer", tool.defer);
                if (tool.idempotent != null) def.put("idempotent", tool.idempotent);
                toolDefs.add(def);
            }
            payload.put("tools", toolDefs);
        }
        if (config.getSystemMessage() != null) payload.put("systemMessage", config.getSystemMessage());
        if (config.getAvailableTools() != null) payload.put("availableTools", config.getAvailableTools());
        if (config.getExcludedTools() != null) payload.put("excludedTools", config.getExcludedTools());
        // Legacy single BYOK provider: translated to inference.providers/models by groupSessionParams.
        if (config.getProvider() != null) payload.put("provider", MAPPER.convertValue(config.getProvider(), Map.class));
        if (config.getOnPermissionRequest() != null) payload.put("requestPermission", true);
        if (config.getOnUserInputRequest() != null) payload.put("requestUserInput", true);
        if (config.getHooks() != null && config.getHooks().hasAnyHook()) {
            payload.put("hooks", true);
            // Surface the specific lifecycle hooks the runtime should invoke.
            SessionHooks hooks = config.getHooks();
            List<String> hookTypes = new ArrayList<>();
            if (hooks.getOnPreToolUse() != null) hookTypes.add("preToolUse");
            if (hooks.getOnPostToolUse() != null) hookTypes.add("postToolUse");
            if (hooks.getOnPreMcpToolCall() != null) hookTypes.add("preMcpToolCall");
            if (hooks.getOnUserPromptSubmitted() != null) hookTypes.add("userPromptSubmitted");
            if (hooks.getOnUserPromptTransformed() != null) hookTypes.add("userPromptTransformed");
            if (hooks.getOnSessionStart() != null) hookTypes.add("sessionStart");
            if (hooks.getOnSessionEnd() != null) hookTypes.add("sessionEnd");
            if (hooks.getOnErrorOccurred() != null) hookTypes.add("errorOccurred");
            if (hooks.getOnSubagentStart() != null) hookTypes.add("subagentStart");
            if (hooks.getOnSubagentStop() != null) hookTypes.add("subagentStop");
            payload.put("hookTypes", hookTypes);
        }
        if (config.getWorkingDirectory() != null) payload.put("workingDirectory", config.getWorkingDirectory());
        if (config.getStreaming() != null) payload.put("streaming", config.getStreaming());
        if (config.getMcpServers() != null) payload.put("mcpServers", config.getMcpServers());
        if (config.getCustomAgents() != null) payload.put("customAgents", config.getCustomAgents());
        if (config.getSkillDirectories() != null) payload.put("skillDirectories", config.getSkillDirectories());
        if (config.getDisabledSkills() != null) payload.put("disabledSkills", config.getDisabledSkills());
        if (config.getInfiniteSessions() != null) payload.put("infiniteSessions", config.getInfiniteSessions());
        if (config.getModelCapabilities() != null) payload.put("modelCapabilities", config.getModelCapabilities());
        if (config.getEnableConfigDiscovery() != null) payload.put("enableConfigDiscovery", config.getEnableConfigDiscovery());
        if (config.getIncludeSubAgentStreamingEvents() != null) payload.put("includeSubAgentStreamingEvents", config.getIncludeSubAgentStreamingEvents());
        // Per-session auth token: authToken takes precedence over gitHubToken
        String sessionToken = config.getAuthToken() != null ? config.getAuthToken() : config.getGitHubToken();
        if (sessionToken != null) payload.put("gitHubToken", sessionToken);
        if (config.getCommands() != null && !config.getCommands().isEmpty()) {
            List<Map<String, Object>> cmdDefs = new ArrayList<>();
            for (Types.CommandDefinition cmd : config.getCommands()) {
                Map<String, Object> def = new HashMap<>();
                def.put("name", cmd.name);
                if (cmd.description != null) def.put("description", cmd.description);
                cmdDefs.add(def);
            }
            payload.put("commands", cmdDefs);
        }
        if (config.getOnElicitationRequest() != null) payload.put("requestElicitation", true);
        // Wire requestHeaders and imageGeneration / responseFormat for the session
        if (config.getRequestHeaders() != null) payload.put("requestHeaders", config.getRequestHeaders());
        if (config.getResponseFormat() != null) payload.put("responseFormat", config.getResponseFormat());

        // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
        if (config.getEnableCitations() != null) payload.put("enableCitations", config.getEnableCitations());
        if (config.getExcludedBuiltinAgents() != null) payload.put("excludedBuiltinAgents", config.getExcludedBuiltinAgents());
        if (config.getSessionLimits() != null) {
            Types.SessionLimitsConfig sl = config.getSessionLimits();
            Map<String, Object> slWire = new HashMap<>();
            if (sl.maxAiCredits != null) slWire.put("maxAiCredits", sl.maxAiCredits);
            payload.put("sessionLimits", slWire);
        }
        if (config.getMemory() != null) {
            Types.MemoryConfiguration mem = config.getMemory();
            Map<String, Object> memWire = new HashMap<>();
            if (mem.enabled != null) memWire.put("enabled", mem.enabled);
            payload.put("memory", memWire);
        }
        if (config.getOtlpProtocol() != null) payload.put("otlpProtocol", config.getOtlpProtocol());
        if (config.getExpAssignments() != null) payload.put("expAssignments", config.getExpAssignments());
        if (config.getRewindEnabled() != null) payload.put("rewindEnabled", config.getRewindEnabled());
        if (config.getAdditionalDirectories() != null) payload.put("additionalDirectories", config.getAdditionalDirectories());
        if (config.getDisabledMcpServers() != null) payload.put("disabledMcpServers", config.getDisabledMcpServers());
        if (config.getGithubMcpToolConfig() != null) payload.put("githubMcpToolConfig", config.getGithubMcpToolConfig());
        if (config.getCanvasProvider() != null) payload.put("canvasProvider", config.getCanvasProvider());
        if (config.getCustomAgentsLocalOnly() != null) payload.put("customAgentsLocalOnly", config.getCustomAgentsLocalOnly());
        if (config.getToolSearch() != null) payload.put("toolSearch", config.getToolSearch());
        if (config.getExperimentalMode() != null) payload.put("enableExperimentalMode", config.getExperimentalMode());
        if (config.getContentExclusion() != null) payload.put("contentExclusion", config.getContentExclusion());
        if (config.getInstructionDirectories() != null) payload.put("instructionDirectories", config.getInstructionDirectories());
        // MCP OAuth host token handler: signal to the runtime that a handler is registered.
        if (config.getOnMcpAuthRequest() != null) payload.put("mcpAuthHandler", true);

        // --- v2.6.0 upstream-sync session options ---
        // CAPI provider options (github.capi): explicit CapiConfig members win over the flat setters.
        Map<String, Object> capi = new HashMap<>();
        if (config.getAutoTier() != null) capi.put("autoTier", config.getAutoTier());
        if (config.getEnableWebSocketResponses() != null) capi.put("enableWebSocketResponses", config.getEnableWebSocketResponses());
        if (config.getCapi() != null) capi.putAll(config.getCapi().toMap());
        if (!capi.isEmpty()) payload.put("capi", capi);
        if (config.getAskUserVariant() != null) payload.put("askUserVariant", config.getAskUserVariant());
        if (config.getSandboxConfig() != null) payload.put("sandboxConfig", config.getSandboxConfig().toMap());
        if (config.getFeatureFlags() != null) payload.put("featureFlags", config.getFeatureFlags());
        if (config.getAllowedModels() != null) payload.put("allowedModels", config.getAllowedModels());
        if (config.getAuthClientIdMetadataUrl() != null) payload.put("authClientIdMetadataUrl", config.getAuthClientIdMetadataUrl());
        if (config.getAgentDirectories() != null) payload.put("agentDirectories", config.getAgentDirectories());
        if (config.getHookDirectories() != null) payload.put("hookDirectories", config.getHookDirectories());
        if (config.getInteractiveUser() != null) payload.put("interactiveUser", config.getInteractiveUser());
        if (config.getExcludedManagedSettings() != null) payload.put("excludedManagedSettings", config.getExcludedManagedSettings());
        if (config.getEnforceManagedModelDefaults() != null) payload.put("enforceManagedModelDefaults", config.getEnforceManagedModelDefaults());
        if (config.getImageGeneration() != null) payload.put("imageGeneration", config.getImageGeneration().toMap());
        if (config.getAutopilot() != null) payload.put("autopilot", config.getAutopilot().toMap());

        // --- v3.0.0 session options (SDK protocol 4) ---
        if (config.getProviders() != null) {
            List<Map<String, Object>> providerDefs = new ArrayList<>();
            for (Types.NamedProviderConfig provider : config.getProviders()) providerDefs.add(provider.toMap());
            payload.put("providers", providerDefs);
        }
        if (config.getModels() != null) {
            List<Map<String, Object>> modelDefs = new ArrayList<>();
            for (Types.ProviderModelConfig model : config.getModels()) modelDefs.add(model.toMap());
            payload.put("models", modelDefs);
        }
        if (config.getDefaultProvider() != null) {
            payload.put("defaultProvider", config.getDefaultProvider());
        } else if (resume && config.isDefaultProviderSet()) {
            payload.put("defaultProvider", EXPLICIT_NULL);
        }
        if (config.getOnShellAuthenticationRequest() != null) payload.put("requestShellAuthentication", true);
        if (config.getMemory() != null && config.getMemory().storeMemory != null) payload.put("hasMemoryStore", true);
        if (config.getPermissionMode() != null) payload.put("permissionMode", config.getPermissionMode());
        if (resume) {
            if (config.getAllowTranscriptRecovery() != null) payload.put("allowTranscriptRecovery", config.getAllowTranscriptRecovery());
            if (config.getSuppressResumeEvent() != null) payload.put("suppressResumeEvent", config.getSuppressResumeEvent());
            if (config.getContinuePendingWork() != null) payload.put("continuePendingWork", config.getContinuePendingWork());
        } else {
            if (config.getName() != null) payload.put("name", config.getName());
            if (config.getTrustWorkingDirectory() != null) payload.put("trustWorkingDirectory", config.getTrustWorkingDirectory());
            if (config.getRefreshCustomInstructions() != null) payload.put("refreshCustomInstructions", config.getRefreshCustomInstructions());
        }
        return payload;
    }

    // --- SDK protocol 4: grouped session payloads ---

    /** Serializes as JSON {@code null}: an explicit clear (for example of a default provider) that grouping keeps. */
    static final Object EXPLICIT_NULL = NullNode.getInstance();

    /** Flat keys that stay at the root of protocol 4 create/resume payloads. */
    private static final Set<String> ROOT_SESSION_KEYS = Set.of(
        "sessionId", "model", "name", "clientName", "workingDirectory", "configDirectory",
        "enableConfigDiscovery", "systemMessage", "diagnostics", "lifecycleDiagnostics", "cloud",
        "traceparent", "tracestate", "suppressResumeEvent", "continuePendingWork",
        "allowTranscriptRecovery", "requestedEnvironmentVariables");

    /** Flat keys accepted only by {@code session.create}; dropped on resume. */
    private static final Set<String> CREATE_ONLY_SESSION_KEYS = Set.of(
        "name", "cloud", "trustWorkingDirectory", "refreshCustomInstructions");

    /** Flat keys accepted only by {@code session.resume}; dropped on create. */
    private static final Set<String> RESUME_ONLY_SESSION_KEYS = Set.of(
        "suppressResumeEvent", "continuePendingWork", "allowTranscriptRecovery",
        "requestedEnvironmentVariables", "openCanvases");

    /** Option groups of protocol 4 create/resume payloads. */
    private static final Set<String> SESSION_OPTION_GROUPS = Set.of(
        "inference", "tools", "permissions", "interaction", "events", "agents", "skills", "mcp",
        "github", "filesystem", "execution", "instructions", "hooks", "git", "state", "plugins",
        "canvases", "extensions", "experiments", "policy");

    /** Legacy provider fields that describe the model rather than the connection. */
    private static final Set<String> PROVIDER_MODEL_FIELDS = Set.of(
        "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens", "maxOutputTokens", "capabilities");

    /** Flat keys mapped to their protocol 4 {@code {group, member}} location. */
    private static final Map<String, String[]> GROUPED_SESSION_KEYS = new HashMap<>();

    static {
        String[][] table = {
            {"reasoningEffort", "inference", "reasoningEffort"},
            {"reasoningSummary", "inference", "reasoningSummary"},
            {"contextTier", "inference", "contextTier"},
            {"allowMultipleToolCalls", "inference", "allowMultipleToolCalls"},
            {"enableCitations", "inference", "enableCitations"},
            {"modelCapabilities", "inference", "capabilities"},
            {"verbosity", "inference", "verbosity"},
            {"providers", "inference", "providers"},
            {"models", "inference", "models"},
            {"defaultProvider", "inference", "defaultProvider"},
            {"tools", "tools", "definitions"},
            {"availableTools", "tools", "availableTools"},
            {"excludedTools", "tools", "excludedTools"},
            {"toolSearch", "tools", "search"},
            {"largeOutput", "tools", "largeOutput"},
            {"imageGeneration", "tools", "imageGeneration"},
            {"manageScheduleEnabled", "tools", "manageScheduleEnabled"},
            {"webSearch", "tools", "webSearch"},
            {"shell", "tools", "shell"},
            {"maxInlineBinaryBytes", "tools", "maxInlineBinaryBytes"},
            {"requestPermission", "permissions", "request"},
            {"sandboxConfig", "permissions", "sandbox"},
            {"sandbox", "permissions", "sandbox"},
            {"permissionMode", "permissions", "mode"},
            {"sandboxConfigSource", "permissions", "sandboxSource"},
            {"commands", "interaction", "commands"},
            {"askUserVariant", "interaction", "askUserVariant"},
            {"requestUserInput", "interaction", "requestUserInput"},
            {"requestElicitation", "interaction", "requestElicitation"},
            {"requestShellAuthentication", "interaction", "requestShellAuthentication"},
            {"requestExitPlanMode", "interaction", "requestExitPlanMode"},
            {"requestAutoModeSwitch", "interaction", "requestAutoModeSwitch"},
            {"requestAutoModeRecovery", "interaction", "requestAutoModeRecovery"},
            {"streaming", "events", "streaming"},
            {"includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents"},
            {"customAgents", "agents", "definitions"},
            {"defaultAgent", "agents", "default"},
            {"agent", "agents", "selected"},
            {"includedBuiltinAgents", "agents", "includedBuiltinAgents"},
            {"excludedBuiltinAgents", "agents", "excludedBuiltinAgents"},
            {"customAgentsLocalOnly", "agents", "localDiscoveryOnly"},
            {"agentDirectories", "agents", "directories"},
            {"skillDirectories", "skills", "directories"},
            {"enableSkills", "skills", "enabled"},
            {"includedBuiltinSkills", "skills", "includedBuiltinSkills"},
            {"disabledSkills", "skills", "disabledSkills"},
            {"hasSkillProvider", "skills", "hasProvider"},
            {"ignoredSkillsLocations", "skills", "ignoredLocations"},
            {"mcpServers", "mcp", "servers"},
            {"disabledMcpServers", "mcp", "disabledServers"},
            {"requestMcpApps", "mcp", "enableApps"},
            {"enableMcpApps", "mcp", "enableApps"},
            {"mcpOAuthTokenStorage", "mcp", "oauthTokenStorage"},
            {"authClientIdMetadataUrl", "mcp", "authClientIdMetadataUrl"},
            {"envValueMode", "mcp", "envValueMode"},
            {"managedMcpServers", "mcp", "managedServers"},
            {"allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions"},
            {"gitHubToken", "github", "token"},
            {"gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId"},
            {"githubMcpToolConfig", "github", "mcp"},
            {"capi", "github", "capi"},
            {"enableSessionTelemetry", "github", "telemetryEnabled"},
            {"enableManagedSettings", "github", "fetchManagedSettings"},
            {"interactiveUser", "github", "interactiveUser"},
            {"enableGitHubTelemetryForwarding", "github", "telemetryForwarding"},
            {"authInfo", "github", "authInfo"},
            {"copilotUrl", "github", "copilotUrl"},
            {"additionalDirectories", "filesystem", "additionalDirectories"},
            {"enableFileChangeTracking", "filesystem", "trackFileChanges"},
            {"trustWorkingDirectory", "filesystem", "trustWorkingDirectory"},
            {"sessionLimits", "execution", "limits"},
            {"autopilot", "execution", "autopilot"},
            {"autopilotContinuation", "execution", "autopilotContinuation"},
            {"instructionDirectories", "instructions", "directories"},
            {"organizationCustomInstructions", "instructions", "organizationText"},
            {"enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery"},
            {"skipCustomInstructions", "instructions", "skipCustomInstructions"},
            {"refreshCustomInstructions", "instructions", "refreshCustomInstructions"},
            {"hooks", "hooks", "hasHandlers"},
            {"hookDirectories", "hooks", "directories"},
            {"enableFileHooks", "hooks", "enableRepositoryHooks"},
            {"coauthorEnabled", "git", "coauthorEnabled"},
            {"enableHostGitOperations", "git", "enableHostGitOperations"},
            {"infiniteSessions", "state", "infiniteSessions"},
            {"memory", "state", "memory"},
            {"hasMemoryStore", "state", "hasMemoryStore"},
            {"enableSessionStore", "state", "enableCrossSessionStore"},
            {"skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval"},
            {"embeddingCacheStorage", "state", "embeddingCacheStorage"},
            {"pluginDirectories", "plugins", "directories"},
            {"installedPlugins", "plugins", "installed"},
            {"canvases", "canvases", "definitions"},
            {"canvasProvider", "canvases", "provider"},
            {"requestCanvasRenderer", "canvases", "requestRenderer"},
            {"openCanvases", "canvases", "open"},
            {"requestExtensions", "extensions", "enabled"},
            {"extensionSdkPath", "extensions", "sdkPath"},
            {"extensionInfo", "extensions", "identity"},
            {"enableExperimentalMode", "experiments", "enableExperimentalMode"},
            {"isExperimentalMode", "experiments", "enableExperimentalMode"},
            {"featureFlags", "experiments", "flags"},
            {"expAssignments", "experiments", "assignments"},
            {"featureFlagConfiguration", "experiments", "configuration"},
            {"allowedModels", "policy", "allowedModels"},
            {"managedSettings", "policy", "managedSettings"},
            {"excludedManagedSettings", "policy", "excludedManagedSettings"},
            {"enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults"},
        };
        for (String[] row : table) GROUPED_SESSION_KEYS.put(row[0], new String[]{row[1], row[2]});
    }

    /**
     * Converts the flat {@code session.create}/{@code session.resume} payload this SDK builds into
     * the SDK protocol 4 grouped payload. Root keys stay at the top level, known flat keys move to
     * their option group, unknown keys pass through unchanged, null values are skipped, empty groups
     * are omitted, and members of an already-grouped object win over flat keys. A legacy single
     * {@code provider} becomes the named {@code "default"} connection.
     *
     * @param flat the flat payload
     * @param resume whether the payload is for {@code session.resume}
     * @return the grouped payload
     */
    static Map<String, Object> groupSessionParams(Map<String, Object> flat, boolean resume) {
        Map<String, Object> root = new LinkedHashMap<>();
        Map<String, Map<String, Object>> groups = new LinkedHashMap<>();
        // Already-grouped objects first, so their members win over flat keys.
        for (Map.Entry<String, Object> entry : flat.entrySet()) {
            if (SESSION_OPTION_GROUPS.contains(entry.getKey()) && entry.getValue() instanceof Map) {
                Map<String, Object> group = groups.computeIfAbsent(entry.getKey(), k -> new LinkedHashMap<>());
                for (Map.Entry<?, ?> member : ((Map<?, ?>) entry.getValue()).entrySet()) {
                    if (member.getValue() != null) group.put(String.valueOf(member.getKey()), member.getValue());
                }
            }
        }
        Object legacyProvider = null;
        for (Map.Entry<String, Object> entry : flat.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            if (value == null || (SESSION_OPTION_GROUPS.contains(key) && value instanceof Map)) continue;
            if ("configDir".equals(key)) key = "configDirectory";
            else if ("disableResume".equals(key)) key = "suppressResumeEvent";
            if (resume ? CREATE_ONLY_SESSION_KEYS.contains(key) : RESUME_ONLY_SESSION_KEYS.contains(key)) continue;
            if ("provider".equals(key)) {
                legacyProvider = value;
                continue;
            }
            String[] path = ROOT_SESSION_KEYS.contains(key) ? null : GROUPED_SESSION_KEYS.get(key);
            if (path == null) {
                root.putIfAbsent(key, value);
            } else {
                groups.computeIfAbsent(path[0], k -> new LinkedHashMap<>()).putIfAbsent(path[1], value);
            }
        }
        Map<String, Object> mcp = groups.get("mcp");
        if (mcp != null && mcp.containsKey("servers")) mcp.putIfAbsent("envValueMode", "direct");
        if (legacyProvider instanceof Map) translateLegacyProvider((Map<?, ?>) legacyProvider, root.get("model"), groups);
        for (Map.Entry<String, Map<String, Object>> group : groups.entrySet()) {
            if (!group.getValue().isEmpty()) root.put(group.getKey(), group.getValue());
        }
        return root;
    }

    /**
     * Translates a legacy single BYOK {@code provider} into the named {@code "default"} connection
     * (plus a model definition when model-specific fields are present), selects it as the default
     * provider, and disables Copilot API models as v1 BYOK did. Explicit {@code providers},
     * {@code defaultProvider}, or {@code capi.enabled} win: the legacy provider is then dropped.
     */
    @SuppressWarnings("unchecked")
    private static void translateLegacyProvider(Map<?, ?> provider, Object model, Map<String, Map<String, Object>> groups) {
        Map<String, Object> inference = groups.computeIfAbsent("inference", k -> new LinkedHashMap<>());
        Map<String, Object> github = groups.computeIfAbsent("github", k -> new LinkedHashMap<>());
        Object capi = github.get("capi");
        if (inference.containsKey("providers") || inference.containsKey("defaultProvider")
            || (capi instanceof Map && ((Map<?, ?>) capi).containsKey("enabled"))) {
            return;
        }
        Map<String, Object> connection = new LinkedHashMap<>();
        connection.put("name", "default");
        Map<String, Object> modelFields = new LinkedHashMap<>();
        for (Map.Entry<?, ?> field : provider.entrySet()) {
            String name = String.valueOf(field.getKey());
            if (field.getValue() == null || "name".equals(name)) continue;
            if (PROVIDER_MODEL_FIELDS.contains(name)) {
                modelFields.put(name, field.getValue());
            } else {
                connection.put(name, field.getValue());
            }
        }
        inference.put("providers", List.of(connection));
        inference.put("defaultProvider", "default");
        if (!modelFields.isEmpty() && model != null && !inference.containsKey("models")) {
            Map<String, Object> definition = new LinkedHashMap<>();
            definition.put("provider", "default");
            definition.put("id", model);
            definition.putAll(modelFields);
            inference.put("models", List.of(definition));
        }
        Map<String, Object> capiWire = new LinkedHashMap<>();
        if (capi instanceof Map) capiWire.putAll((Map<String, Object>) capi);
        capiWire.put("enabled", false);
        github.put("capi", capiWire);
    }

    /**
     * Empty mode: applies safe runtime defaults under the caller's values (caller values win).
     * No-op in the default {@code "coding-agent"} mode.
     */
    private void applyModeDefaults(Map<String, Object> wire) {
        if (!Types.ClientMode.EMPTY.equals(mode)) return;
        putGroupDefault(wire, "experiments", "enableExperimentalMode", false);
        putGroupDefault(wire, "github", "telemetryEnabled", false);
        putGroupDefault(wire, "mcp", "oauthTokenStorage", "in-memory");
        putGroupDefault(wire, "state", "skipEmbeddingRetrieval", true);
        putGroupDefault(wire, "state", "embeddingCacheStorage", "in-memory");
        putGroupDefault(wire, "state", "enableCrossSessionStore", false);
        putGroupDefault(wire, "state", "memory", Map.of("enabled", false));
        putGroupDefault(wire, "instructions", "onDemandDiscovery", false);
        putGroupDefault(wire, "hooks", "enableRepositoryHooks", false);
        putGroupDefault(wire, "git", "enableHostGitOperations", false);
    }

    @SuppressWarnings("unchecked")
    private static void putGroupDefault(Map<String, Object> wire, String group, String member, Object value) {
        Object existing = wire.get(group);
        Map<String, Object> target;
        if (existing instanceof Map) {
            target = (Map<String, Object>) existing;
        } else {
            target = new LinkedHashMap<>();
            wire.put(group, target);
        }
        target.putIfAbsent(member, value);
    }

    /**
     * Builds the {@code session.send} JSON-RPC payload, including the upstream-sync
     * message options (agentMode, displayPrompt, requestHeaders, source, clientTurnId, responseSchema).
     * The legacy string {@code responseFormat} is not sent: protocol 4 only accepts the structured format.
     */
    static Map<String, Object> buildSendPayload(String sessionId, MessageOptions options) {
        Map<String, Object> params = new HashMap<>();
        params.put("sessionId", sessionId);
        params.put("prompt", options.getPrompt());
        if (options.getSource() != null) params.put("source", options.getSource());
        if (options.getClientTurnId() != null) params.put("clientTurnId", options.getClientTurnId());
        if (options.getAttachments() != null) params.put("attachments", options.getAttachments());
        if (options.getMode() != null) params.put("mode", options.getMode());
        if (options.getAgentMode() != null) params.put("agentMode", options.getAgentMode());
        if (options.getDisplayPrompt() != null) params.put("displayPrompt", options.getDisplayPrompt());
        if (options.getRequestHeaders() != null) params.put("requestHeaders", options.getRequestHeaders());
        if (options.getResponseSchema() != null) {
            Map<String, Object> jsonSchema = new HashMap<>();
            jsonSchema.put("name", "response");
            jsonSchema.put("strict", true);
            jsonSchema.put("schema", options.getResponseSchema());
            Map<String, Object> responseFormat = new HashMap<>();
            responseFormat.put("type", "json_schema");
            responseFormat.put("jsonSchema", jsonSchema);
            params.put("responseFormat", responseFormat);
        }
        if (options.getImageOptions() != null) params.put("imageOptions", options.getImageOptions());
        return params;
    }

    /** Tool "defer" loading policy values. */
    public static final class ToolDefer {
        public static final String AUTO = "auto";
        public static final String NEVER = "never";
        private ToolDefer() {}
    }

    /** System-message section identifiers (used with system-message section overrides). */
    public static final class SystemMessageSection {
        public static final String PREAMBLE = "preamble";
        public static final String IDENTITY = "identity";
        public static final String TOOL_INSTRUCTIONS = "tool_instructions";
        public static final String PRESERVE = "preserve";
        private SystemMessageSection() {}
    }

    /** GitHub-anchored attachment type identifiers. */
    public static final class GitHubAttachment {
        public static final String GITHUB_COMMIT = "GitHubCommit";
        public static final String GITHUB_RELEASE = "GitHubRelease";
        public static final String GITHUB_ACTIONS_JOB = "GitHubActionsJob";
        public static final String GITHUB_REPOSITORY = "GitHubRepository";
        public static final String GITHUB_FILE_DIFF = "GitHubFileDiff";
        public static final String GITHUB_TREE_COMPARISON = "GitHubTreeComparison";
        public static final String GITHUB_URL = "GitHubUrl";
        public static final String GITHUB_FILE = "GitHubFile";
        public static final String GITHUB_SNIPPET = "GitHubSnippet";
        private GitHubAttachment() {}
    }

    private void verifyProtocolVersion() throws Exception {
        int expected = SdkProtocolVersion.get();
        PingResponse ping = ping(null);
        if (ping.protocolVersion == null) {
            throw new RuntimeException("SDK protocol version mismatch: SDK supports version " + expected
                + ", but server does not report a protocol version. Please update your server to ensure compatibility.");
        }
        if (ping.protocolVersion != expected) {
            throw new RuntimeException("SDK protocol version mismatch: SDK supports version " + expected
                + ", but server reports version " + ping.protocolVersion
                + ". Please update your SDK or server to ensure compatibility.");
        }
    }

    private void startCLIServer() throws Exception {
        List<String> args = new ArrayList<>();

        // If cliPath is a .js file, run with node
        if (cliPath.endsWith(".js")) {
            args.add("node");
            args.add(cliPath);
        } else {
            args.add(cliPath);
        }

        // Add extra CLI args
        args.addAll(Arrays.asList(cliArgs));
        args.addAll(List.of("--headless", "--no-auto-update", "--log-level", logLevel));

        if (useStdio) {
            args.add("--stdio");
        } else if (port > 0) {
            args.add("--port");
            args.add(String.valueOf(port));
        }

        if (githubToken != null) {
            args.add("--auth-token-env");
            args.add("COPILOT_SDK_AUTH_TOKEN");
        }
        if (!useLoggedInUser) {
            args.add("--no-auto-login");
        }

        if (sessionIdleTimeoutSeconds != null && sessionIdleTimeoutSeconds > 0) {
            args.add("--session-idle-timeout");
            args.add(String.valueOf(sessionIdleTimeoutSeconds));
        }

        ProcessBuilder pb = new ProcessBuilder(args);
        if (cwd != null) pb.directory(new File(cwd));

        // Set environment
        Map<String, String> processEnv = pb.environment();
        if (env != null) {
            processEnv.clear();
            processEnv.putAll(env);
        }
        if (githubToken != null) {
            processEnv.put("COPILOT_SDK_AUTH_TOKEN", githubToken);
        }

        pb.redirectErrorStream(false);
        cliProcess = pb.start();

        // Consume stderr in background
        Thread stderrThread = new Thread(() -> {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(cliProcess.getErrorStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    // Optionally forward: System.err.println("[CLI] " + line);
                }
            } catch (IOException ignored) {}
        }, "cli-stderr");
        stderrThread.setDaemon(true);
        stderrThread.start();

        if (!useStdio) {
            // TCP mode: read stdout to get port
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(cliProcess.getInputStream()))) {
                long deadline = System.currentTimeMillis() + 10_000;
                String line;
                while ((line = reader.readLine()) != null) {
                    Matcher m = PORT_PATTERN.matcher(line);
                    if (m.find()) {
                        actualPort = Integer.parseInt(m.group(1));
                        return;
                    }
                    if (System.currentTimeMillis() > deadline) {
                        throw new RuntimeException("Timeout waiting for CLI server to start");
                    }
                }
                throw new RuntimeException("CLI process exited before announcing port");
            }
        }
    }

    private void connectToServer() throws Exception {
        if (useStdio) {
            connectViaStdio();
        } else {
            connectViaTcp();
        }
    }

    private void connectViaStdio() {
        if (cliProcess == null) throw new IllegalStateException("CLI process not started");
        rpcClient = new JsonRpcClient(cliProcess.getInputStream(), cliProcess.getOutputStream());
        setupHandlers();
        rpcClient.start();
    }

    private void connectViaTcp() throws Exception {
        if (actualPort == 0) throw new IllegalStateException("Server port not available");
        tcpSocket = new Socket(actualHost, actualPort);
        rpcClient = new JsonRpcClient(tcpSocket.getInputStream(), tcpSocket.getOutputStream());
        setupHandlers();
        rpcClient.start();
    }

    @SuppressWarnings("unchecked")
    private void setupHandlers() {
        rpcClient.setNotificationHandler((method, params) -> {
            if ("session.event".equals(method)) {
                String sessionId = (String) params.get("sessionId");
                Map<String, Object> event = (Map<String, Object>) params.get("event");
                CopilotSession session = sessions.get(sessionId);
                if (session != null && event != null) {
                    session.dispatchEvent(event);
                }
            } else if ("session.lifecycle".equals(method)) {
                dispatchLifecycleEvent(params);
            }
        });

        rpcClient.setRequestHandler("tool.call", params -> handleToolCallRequest(params));
        rpcClient.setRequestHandler("permission.request", params -> handlePermissionRequest(params));
        rpcClient.setRequestHandler("userInput.request", params -> handleUserInputRequest(params));
        rpcClient.setRequestHandler("hooks.invoke", params -> handleHooksInvoke(params));
        rpcClient.setRequestHandler("gitHubToken.getToken", params -> handleGitHubTokenRequest(params));
        rpcClient.setRequestHandler("shellAuthentication.request", params -> handleShellAuthenticationRequest(params));
        rpcClient.setRequestHandler("shellAuthentication.cancel", params -> handleShellAuthenticationCancel(params));
        rpcClient.setRequestHandler("memoryStore.store", params -> handleMemoryStoreRequest(params));
        setupNetworkHandlers();
    }

    /** Answers {@code shellAuthentication.request} with the owning session's handler ({@code cancel} when unavailable). */
    private Map<String, Object> handleShellAuthenticationRequest(Map<String, Object> params) {
        Object sessionId = params.get("sessionId");
        CopilotSession session = sessionId instanceof String ? sessions.get((String) sessionId) : null;
        if (session == null) {
            Map<String, Object> cancel = new HashMap<>();
            cancel.put("action", "cancel");
            return cancel;
        }
        return session.handleShellAuthenticationRequest(params);
    }

    /** Cancels a pending shell authentication prompt; always answers {@code {}}. */
    private Map<String, Object> handleShellAuthenticationCancel(Map<String, Object> params) {
        Object sessionId = params.get("sessionId");
        Object requestId = params.get("requestId");
        CopilotSession session = sessionId instanceof String ? sessions.get((String) sessionId) : null;
        if (session != null && requestId instanceof String) session.cancelShellAuthentication((String) requestId);
        return new HashMap<>();
    }

    /** Stores a memory through the owning session's callback and answers with a JSON {@code null} result. */
    private Map<String, Object> handleMemoryStoreRequest(Map<String, Object> params) throws Exception {
        Object sessionId = params.get("sessionId");
        CopilotSession session = sessionId instanceof String ? sessions.get((String) sessionId) : null;
        if (session == null) throw new IllegalStateException("Memory store owner is unavailable");
        session.handleMemoryStore(params);
        return JsonRpcClient.NULL_RESULT;
    }

    /** Serves the configured network callbacks. Tokens and passwords are never logged. */
    private void setupNetworkHandlers() {
        Types.NetworkConfig config = network;
        if (config == null) return;
        if (config.resolveProxy != null) {
            Types.ProxyResolver resolver = config.resolveProxy;
            rpcClient.setRequestHandler("network.resolveProxy", params -> {
                Object url = params.get("url");
                String proxy = resolver.resolveProxy(new Types.NetworkResolveProxyRequest(url instanceof String ? (String) url : ""));
                Map<String, Object> result = new HashMap<>();
                if (proxy != null) result.put("proxy", proxy);
                return result;
            });
        }
        if (config.authenticateNegotiate != null) {
            Types.NegotiateAuthenticator authenticator = config.authenticateNegotiate;
            rpcClient.setRequestHandler("network.authenticateNegotiate", params -> {
                Object spn = params.get("spn");
                String token = authenticator.authenticateNegotiate(
                    new Types.NetworkAuthenticateNegotiateRequest(spn instanceof String ? (String) spn : ""));
                Map<String, Object> result = new HashMap<>();
                if (token != null) result.put("token", token);
                return result;
            });
        }
        if (config.authenticateBasic != null) {
            Types.BasicAuthenticator authenticator = config.authenticateBasic;
            rpcClient.setRequestHandler("network.authenticateBasic", params -> {
                Types.NetworkAuthenticateBasicRequest request = new Types.NetworkAuthenticateBasicRequest();
                Object proxy = params.get("proxy");
                Object realm = params.get("realm");
                request.proxy = proxy instanceof String ? (String) proxy : "";
                request.realm = realm instanceof String ? (String) realm : null;
                request.previousRejected = Boolean.TRUE.equals(params.get("previousRejected"));
                Types.NetworkBasicCredentials credentials = authenticator.authenticateBasic(request);
                // Send only the credential fields.
                Map<String, Object> result = new HashMap<>();
                if (credentials != null) {
                    if (credentials.username != null) result.put("username", credentials.username);
                    if (credentials.password != null) result.put("password", credentials.password);
                }
                return result;
            });
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> handleToolCallRequest(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.get("sessionId");
        String toolCallId = (String) params.get("toolCallId");
        String toolName = (String) params.get("toolName");
        Object arguments = params.get("arguments");

        CopilotSession session = sessions.get(sessionId);
        if (session == null) throw new RuntimeException("Unknown session " + sessionId);

        Types.ToolHandler handler = session.getToolHandler(toolName);
        if (handler == null) {
            return Map.of("result", Map.of(
                "textResultForLlm", "Tool '" + toolName + "' is not supported by this client instance.",
                "resultType", "failure",
                "error", "tool '" + toolName + "' not supported",
                "toolTelemetry", Map.of()
            ));
        }

        try {
            ToolInvocation invocation = new ToolInvocation(sessionId, toolCallId, toolName, arguments);
            Object result = handler.handle(arguments, invocation);
            return Map.of("result", normalizeToolResult(result));
        } catch (Exception e) {
            return Map.of("result", Map.of(
                "textResultForLlm", "Invoking this tool produced an error. Detailed information is not available.",
                "resultType", "failure",
                "error", e.getMessage(),
                "toolTelemetry", Map.of()
            ));
        }
    }

    private Map<String, Object> handlePermissionRequest(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.get("sessionId");
        CopilotSession session = sessions.get(sessionId);
        if (session == null) throw new RuntimeException("Unknown session " + sessionId);

        try {
            PermissionRequestResult result = session.handlePermissionRequest(params.get("permissionRequest"));
            return Map.of("result", Map.of("kind", result.kind));
        } catch (Exception e) {
            return Map.of("result", Map.of("kind", "denied-no-approval-rule-and-could-not-request-from-user"));
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> handleUserInputRequest(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.get("sessionId");
        CopilotSession session = sessions.get(sessionId);
        if (session == null) throw new RuntimeException("Unknown session " + sessionId);

        UserInputResponse result = session.handleUserInputRequest(params);
        return result.toMap();
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> handleHooksInvoke(Map<String, Object> params) throws Exception {
        String sessionId = (String) params.get("sessionId");
        String hookType = (String) params.get("hookType");
        Object input = params.get("input");

        CopilotSession session = sessions.get(sessionId);
        if (session == null) throw new RuntimeException("Unknown session " + sessionId);

        Map<String, Object> output = session.handleHooksInvoke(hookType, input);
        Map<String, Object> result = new HashMap<>();
        if (output != null) result.put("output", output);
        return result;
    }

    private Object normalizeToolResult(Object result) {
        if (result == null) {
            return Map.of("textResultForLlm", "Tool returned no result", "resultType", "failure",
                "error", "tool returned no result", "toolTelemetry", Map.of());
        }
        if (result instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) result;
            if (map.containsKey("textResultForLlm")) return result;
        }
        String text = result instanceof String ? (String) result : result.toString();
        return Map.of("textResultForLlm", text, "resultType", "success", "toolTelemetry", Map.of());
    }

    private void dispatchLifecycleEvent(Map<String, Object> params) {
        SessionLifecycleEvent event = new SessionLifecycleEvent();
        event.type = (String) params.get("type");
        event.sessionId = (String) params.get("sessionId");

        // Typed handlers
        CopyOnWriteArrayList<SessionLifecycleHandler> typed = typedLifecycleHandlers.get(event.type);
        if (typed != null) {
            for (SessionLifecycleHandler h : typed) {
                try { h.handle(event); } catch (Exception ignored) {}
            }
        }
        // Wildcard handlers
        for (SessionLifecycleHandler h : lifecycleHandlers) {
            try { h.handle(event); } catch (Exception ignored) {}
        }
    }
}
