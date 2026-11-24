// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import Foundation

/// Main client for interacting with the Copilot CLI.
///
/// The `CopilotClient` manages the connection to the Copilot CLI server and provides
/// methods to create and manage conversation sessions. It can spawn a CLI server process
/// or connect to an existing server.
///
/// Example usage:
/// ```swift
/// let client = CopilotClient()
/// let session = try await client.createSession(SessionConfig(model: "gpt-4"))
///
/// session.on { event in
///     if event.type == "assistant.message" {
///         print(event.data["content"] as? String ?? "")
///     }
/// }
///
/// let response = try await session.sendAndWait(MessageOptions(prompt: "Hello!"))
/// try await session.destroy()
/// try await client.stop()
/// ```
public actor CopilotClient {
    // MARK: - Properties

    private let options: CopilotClientOptions
    private var state: ConnectionState = .disconnected
    private var cliProcess: Process?
    private var rpcClient: JsonRpcClient?
    private var sessions: [String: CopilotSession] = [:]
    private var isExternalServer: Bool = false
    private var modelsCache: [ModelInfo]?
    private var lifecycleHandlers: [UUID: SessionLifecycleHandler] = [:]
    /// GitHub token providers keyed by the registration ID sent with create/resume.
    private var gitHubTokenProviders: [String: GitHubTokenProvider] = [:]

    // Resolved options
    private let resolvedCliPath: String
    private let resolvedUseStdio: Bool
    private let resolvedLogLevel: String
    private let resolvedAutoStart: Bool
    private let resolvedAutoRestart: Bool
    private let resolvedMode: ClientMode

    // MARK: - Init

    /// Creates a new CopilotClient instance.
    ///
    /// - Parameter options: Configuration options for the client.
    /// - Throws: If mutually exclusive options are provided.
    public init(options: CopilotClientOptions = CopilotClientOptions()) {
        // Validate options
        if options.cliUrl != nil && (options.useStdio == true || options.cliPath != nil) {
            fatalError("cliUrl is mutually exclusive with useStdio and cliPath")
        }
        if options.cliUrl != nil
            && (options.githubToken != nil || options.useLoggedInUser != nil)
        {
            fatalError(
                "githubToken and useLoggedInUser cannot be used with cliUrl (external server manages its own auth)"
            )
        }

        self.options = options
        self.isExternalServer = options.cliUrl != nil
        self.resolvedCliPath = options.cliPath ?? "copilot"
        self.resolvedUseStdio = options.cliUrl != nil ? false : (options.useStdio ?? true)
        self.resolvedLogLevel = options.logLevel ?? "info"
        self.resolvedAutoStart = options.autoStart ?? true
        self.resolvedAutoRestart = options.autoRestart ?? true
        // "copilot-cli" is a legacy alias of "coding-agent".
        self.resolvedMode = options.mode == .empty ? .empty : .codingAgent
    }

    // MARK: - Connection State

    /// Gets the current connection state.
    public func getState() -> ConnectionState {
        return state
    }

    // MARK: - Start

    /// Starts the CLI server and establishes a connection.
    ///
    /// If connecting to an external server (via cliUrl), only establishes the connection.
    /// Otherwise, spawns the CLI server process and then connects.
    /// This method is called automatically when creating a session if `autoStart` is true.
    ///
    /// The handshake declares `clientInfo` (`connect`, when configured), verifies that the
    /// server speaks SDK protocol 4, and registers the network callbacks
    /// (`network.setProvider`, when configured).
    public func start() async throws {
        if state == .connected {
            return
        }

        state = .connecting

        do {
            if !isExternalServer {
                try await startCLIServer()
            }

            try await sendClientInfo()
            try await verifyProtocolVersion()
            try await registerNetworkProvider()
            state = .connected
        } catch {
            // Do not keep talking to a server that failed the handshake.
            await forceStop()
            state = .error
            throw error
        }
    }

    // MARK: - Stop

    /// Stops the CLI server and closes all active sessions.
    ///
    /// - Returns: An array of errors encountered during cleanup.
    @discardableResult
    public func stop() async -> [Error] {
        var errors: [Error] = []

        // Destroy all sessions
        for (sessionId, session) in sessions {
            do {
                try await session.destroy()
            } catch {
                errors.append(
                    CopilotError.sessionCleanupFailed(
                        sessionId: sessionId, underlying: error))
            }
        }
        sessions.removeAll()
        gitHubTokenProviders.removeAll()

        // Stop RPC client
        if let rpc = rpcClient {
            await rpc.stop()
            rpcClient = nil
        }

        // Clear cache
        modelsCache = nil

        // Terminate CLI process
        if let process = cliProcess, !isExternalServer {
            process.terminate()
            cliProcess = nil
        }

        state = .disconnected
        return errors
    }

    /// Forcefully stops the CLI server without graceful cleanup.
    public func forceStop() async {
        sessions.removeAll()
        gitHubTokenProviders.removeAll()

        if let rpc = rpcClient {
            await rpc.stop()
            rpcClient = nil
        }

        modelsCache = nil

        if let process = cliProcess, !isExternalServer {
            process.terminate()
            cliProcess = nil
        }

        state = .disconnected
    }

    // MARK: - Create Session

    /// Creates a new conversation session with the Copilot CLI.
    ///
    /// The flat session options are grouped into the SDK protocol 4 payload right before
    /// `session.create` is sent. A failure throws a `JsonRpcError`; its
    /// `policyHelperFailure` exposes a managed-policy Policy Helper failure.
    ///
    /// - Parameter config: Configuration for the session.
    /// - Returns: The created session.
    public func createSession(_ config: SessionConfig = SessionConfig()) async throws
        -> CopilotSession
    {
        try validateSessionOptions(
            gitHubToken: config.gitHubToken,
            gitHubTokenProvider: config.gitHubTokenProvider,
            availableTools: config.availableTools,
            memory: config.memory
        )
        try await ensureConnected()

        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }

        var params: [String: Any] = [:]
        if let model = config.model { params["model"] = model }
        if let sid = config.sessionId { params["sessionId"] = sid }
        if let name = config.name { params["name"] = name }
        if let re = config.reasoningEffort { params["reasoningEffort"] = re.rawValue }
        if let tools = config.tools { params["tools"] = tools.map { $0.toWireFormat() } }
        if let sm = config.systemMessage {
            params["systemMessage"] = encodeToDictionary(sm)
        }
        if let at = config.availableTools { params["availableTools"] = at }
        if let et = config.excludedTools { params["excludedTools"] = et }
        if let provider = config.provider {
            params["provider"] = encodeToDictionary(provider)
        }
        params["requestPermission"] = config.onPermissionRequest != nil
        params["requestUserInput"] = config.onUserInputRequest != nil
        params["hooks"] = config.hooks?.hasAnyHandler ?? false
        params["requestExitPlanMode"] = config.onExitPlanMode != nil
        if config.onShellAuthenticationRequest != nil {
            params["requestShellAuthentication"] = true
        }
        if let wd = config.workingDirectory { params["workingDirectory"] = wd }
        if let streaming = config.streaming { params["streaming"] = streaming }
        if let subAgentEvents = config.includeSubAgentStreamingEvents {
            params["includeSubAgentStreamingEvents"] = subAgentEvents
        }
        if let mcp = config.mcpServers {
            params["mcpServers"] = mcp.mapValues { encodeToDictionary($0) ?? [:] }
        }
        if let agents = config.customAgents {
            params["customAgents"] = agents.map { encodeToDictionary($0) ?? [:] }
        }
        if let cd = config.configDir { params["configDir"] = cd }
        if let sd = config.skillDirectories { params["skillDirectories"] = sd }
        if let ds = config.disabledSkills { params["disabledSkills"] = ds }
        if let inf = config.infiniteSessions {
            params["infiniteSessions"] = encodeToDictionary(inf)
        }
        if let capabilities = config.modelCapabilities {
            params["modelCapabilities"] = capabilities.mapValues { $0.value }
        }
        if let discovery = config.enableConfigDiscovery {
            params["enableConfigDiscovery"] = discovery
        }
        if let token = config.gitHubToken { params["gitHubToken"] = token }
        if let dirs = config.instructionDirectories { params["instructionDirectories"] = dirs }
        if let enableCitations = config.enableCitations {
            params["enableCitations"] = enableCitations
        }
        if let excluded = config.excludedBuiltinAgents {
            params["excludedBuiltinAgents"] = excluded
        }
        if let limits = config.sessionLimits {
            var limitsDict: [String: Any] = [:]
            if let credits = limits.maxAiCredits { limitsDict["maxAiCredits"] = credits }
            params["sessionLimits"] = limitsDict
        }
        if let memory = config.memory {
            params["memory"] = ["enabled": memory.enabled]
            if memory.storeMemory != nil { params["hasMemoryStore"] = true }
        }
        if let otlp = config.otlpProtocol {
            params["otlpProtocol"] = otlp
        }
        if let exp = config.expAssignments {
            params["expAssignments"] = exp.mapValues { $0.value }
        }
        params["mcpAuthHandler"] = config.onMcpAuthRequest != nil

        // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
        if let rewind = config.rewindEnabled {
            params["rewindEnabled"] = rewind
        }
        if let dirs = config.additionalDirectories {
            params["additionalDirectories"] = dirs
        }
        if let servers = config.disabledMcpServers {
            params["disabledMcpServers"] = servers
        }
        if let gh = config.githubMcpToolConfig {
            params["githubMcpToolConfig"] = gh.mapValues { $0.value }
        }
        if let canvas = config.canvasProvider {
            params["canvasProvider"] = canvas.mapValues { $0.value }
        }
        if let localOnly = config.customAgentsLocalOnly {
            params["customAgentsLocalOnly"] = localOnly
        }
        if let toolSearch = config.toolSearch {
            params["toolSearch"] = toolSearch.mapValues { $0.value }
        }
        if let experimental = config.experimentalMode {
            params["enableExperimentalMode"] = experimental
        }
        if let exclusion = config.contentExclusion {
            params["contentExclusion"] = exclusion
        }

        // --- v2.6.0 / v3.0.0 session options (SDK protocol 4) ---
        if let variant = config.askUserVariant { params["askUserVariant"] = variant.rawValue }
        if let sandbox = config.sandboxConfig {
            params["sandboxConfig"] = encodeToDictionary(sandbox)
        }
        if let flags = config.featureFlags { params["featureFlags"] = flags }
        if let allowed = config.allowedModels { params["allowedModels"] = allowed }
        if let url = config.authClientIdMetadataUrl { params["authClientIdMetadataUrl"] = url }
        if let dirs = config.agentDirectories { params["agentDirectories"] = dirs }
        if let dirs = config.hookDirectories { params["hookDirectories"] = dirs }
        if let trust = config.trustWorkingDirectory { params["trustWorkingDirectory"] = trust }
        if let imageGeneration = config.imageGeneration {
            params["imageGeneration"] = encodeToDictionary(imageGeneration)
        }
        if let autopilot = config.autopilot {
            params["autopilot"] = encodeToDictionary(autopilot)
        }
        if let providers = config.providers {
            params["providers"] = providers.map { encodeToDictionary($0) ?? [:] }
        }
        if let models = config.models {
            params["models"] = models.map { encodeToDictionary($0) ?? [:] }
        }
        if let defaultProvider = config.defaultProvider {
            params["defaultProvider"] = defaultProvider
        }
        if let capi = capiWireFormat(
            config.capi, enableWebSocketResponses: config.enableWebSocketResponses)
        {
            params["capi"] = capi
        }
        let registrationId = config.gitHubTokenProvider == nil ? nil : UUID().uuidString
        if let registrationId = registrationId {
            params["gitHubTokenProviderRegistrationId"] = registrationId
        }

        await injectTraceContext(&params)

        let payload = groupForMode(params, isResume: false)
        let response = try await sendSessionRequest(
            method: "session.create", payload: payload, rpc: rpc,
            registrationId: registrationId, tokenProvider: config.gitHubTokenProvider)

        guard let sessionId = response["sessionId"] as? String else {
            if let registrationId = registrationId {
                gitHubTokenProviders.removeValue(forKey: registrationId)
            }
            throw CopilotError.invalidResponse("Missing sessionId in session.create response")
        }
        let workspacePath = response["workspacePath"] as? String

        let session = await makeSession(
            sessionId: sessionId, rpc: rpc, workspacePath: workspacePath,
            registrationId: registrationId)

        if let tools = config.tools {
            await session.registerTools(tools)
        }
        if let handler = config.onPermissionRequest {
            await session.registerPermissionHandler(handler)
        }
        if let handler = config.onUserInputRequest {
            await session.registerUserInputHandler(handler)
        }
        if let hooks = config.hooks {
            await session.registerHooks(hooks)
        }
        if let handler = config.onExitPlanMode {
            await session.registerExitPlanModeHandler(handler)
        }
        await session.registerShellAuthenticationHandler(config.onShellAuthenticationRequest)
        await session.registerMemoryStore(config.memory?.storeMemory)

        sessions[sessionId] = session
        return session
    }

    // MARK: - Resume Session

    /// Resumes an existing conversation session by its ID.
    ///
    /// The flat options are grouped into the SDK protocol 4 payload right before
    /// `session.resume` is sent. A failure throws a `JsonRpcError`; its
    /// `policyHelperFailure` exposes a managed-policy Policy Helper failure.
    ///
    /// - Parameters:
    ///   - sessionId: The ID of the session to resume.
    ///   - config: Optional configuration for the resumed session.
    /// - Returns: The resumed session.
    public func resumeSession(_ sessionId: String, config: ResumeSessionConfig = ResumeSessionConfig()) async throws -> CopilotSession {
        try validateSessionOptions(
            gitHubToken: config.gitHubToken,
            gitHubTokenProvider: config.gitHubTokenProvider,
            availableTools: config.availableTools,
            memory: config.memory
        )
        try await ensureConnected()

        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }

        var params: [String: Any] = ["sessionId": sessionId]
        if let model = config.model { params["model"] = model }
        if let re = config.reasoningEffort { params["reasoningEffort"] = re.rawValue }
        if let tools = config.tools { params["tools"] = tools.map { $0.toWireFormat() } }
        if let sm = config.systemMessage {
            params["systemMessage"] = encodeToDictionary(sm)
        }
        if let at = config.availableTools { params["availableTools"] = at }
        if let et = config.excludedTools { params["excludedTools"] = et }
        if let provider = config.provider {
            params["provider"] = encodeToDictionary(provider)
        }
        params["requestPermission"] = config.onPermissionRequest != nil
        params["requestUserInput"] = config.onUserInputRequest != nil
        params["hooks"] = config.hooks?.hasAnyHandler ?? false
        params["requestExitPlanMode"] = config.onExitPlanMode != nil
        if config.onShellAuthenticationRequest != nil {
            params["requestShellAuthentication"] = true
        }
        if let wd = config.workingDirectory { params["workingDirectory"] = wd }
        if let cd = config.configDir { params["configDir"] = cd }
        if let streaming = config.streaming { params["streaming"] = streaming }
        if let subAgentEvents = config.includeSubAgentStreamingEvents {
            params["includeSubAgentStreamingEvents"] = subAgentEvents
        }
        if let mcp = config.mcpServers {
            params["mcpServers"] = mcp.mapValues { encodeToDictionary($0) ?? [:] }
        }
        if let agents = config.customAgents {
            params["customAgents"] = agents.map { encodeToDictionary($0) ?? [:] }
        }
        if let sd = config.skillDirectories { params["skillDirectories"] = sd }
        if let ds = config.disabledSkills { params["disabledSkills"] = ds }
        if let inf = config.infiniteSessions {
            params["infiniteSessions"] = encodeToDictionary(inf)
        }
        if let capabilities = config.modelCapabilities {
            params["modelCapabilities"] = capabilities.mapValues { $0.value }
        }
        if let discovery = config.enableConfigDiscovery {
            params["enableConfigDiscovery"] = discovery
        }
        if let token = config.gitHubToken { params["gitHubToken"] = token }
        if let dirs = config.instructionDirectories { params["instructionDirectories"] = dirs }
        if let suppress = config.suppressResumeEvent ?? config.disableResume {
            params["suppressResumeEvent"] = suppress
        }

        // --- v2.6.0 / v3.0.0 resume options (SDK protocol 4) ---
        if let memory = config.memory {
            params["memory"] = ["enabled": memory.enabled]
            if memory.storeMemory != nil { params["hasMemoryStore"] = true }
        }
        if let variant = config.askUserVariant { params["askUserVariant"] = variant.rawValue }
        if let sandbox = config.sandboxConfig {
            params["sandboxConfig"] = encodeToDictionary(sandbox)
        }
        if let flags = config.featureFlags { params["featureFlags"] = flags }
        if let allowed = config.allowedModels { params["allowedModels"] = allowed }
        if let url = config.authClientIdMetadataUrl { params["authClientIdMetadataUrl"] = url }
        if let dirs = config.agentDirectories { params["agentDirectories"] = dirs }
        if let dirs = config.hookDirectories { params["hookDirectories"] = dirs }
        if let recovery = config.allowTranscriptRecovery {
            params["allowTranscriptRecovery"] = recovery
        }
        if let imageGeneration = config.imageGeneration {
            params["imageGeneration"] = encodeToDictionary(imageGeneration)
        }
        if let autopilot = config.autopilot {
            params["autopilot"] = encodeToDictionary(autopilot)
        }
        if let providers = config.providers {
            params["providers"] = providers.map { encodeToDictionary($0) ?? [:] }
        }
        if let models = config.models {
            params["models"] = models.map { encodeToDictionary($0) ?? [:] }
        }
        if let defaultProvider = config.defaultProvider {
            params["defaultProvider"] = defaultProvider
        } else if config.clearDefaultProvider == true {
            params["defaultProvider"] = NSNull()
        }
        if let capi = capiWireFormat(config.capi, enableWebSocketResponses: nil) {
            params["capi"] = capi
        }
        let registrationId = config.gitHubTokenProvider == nil ? nil : UUID().uuidString
        if let registrationId = registrationId {
            params["gitHubTokenProviderRegistrationId"] = registrationId
        }

        await injectTraceContext(&params)

        let payload = groupForMode(params, isResume: true)
        let response = try await sendSessionRequest(
            method: "session.resume", payload: payload, rpc: rpc,
            registrationId: registrationId, tokenProvider: config.gitHubTokenProvider)

        guard let resumedSessionId = response["sessionId"] as? String else {
            if let registrationId = registrationId {
                gitHubTokenProviders.removeValue(forKey: registrationId)
            }
            throw CopilotError.invalidResponse("Missing sessionId in session.resume response")
        }
        let workspacePath = response["workspacePath"] as? String

        let session = await makeSession(
            sessionId: resumedSessionId, rpc: rpc, workspacePath: workspacePath,
            registrationId: registrationId)

        if let tools = config.tools {
            await session.registerTools(tools)
        }
        if let handler = config.onPermissionRequest {
            await session.registerPermissionHandler(handler)
        }
        if let handler = config.onUserInputRequest {
            await session.registerUserInputHandler(handler)
        }
        if let hooks = config.hooks {
            await session.registerHooks(hooks)
        }
        if let handler = config.onExitPlanMode {
            await session.registerExitPlanModeHandler(handler)
        }
        await session.registerShellAuthenticationHandler(config.onShellAuthenticationRequest)
        await session.registerMemoryStore(config.memory?.storeMemory)

        sessions[resumedSessionId] = session
        return session
    }

    // MARK: - Ping

    /// Sends a ping to the server and returns the response.
    public func ping(message: String? = nil) async throws -> PingResponse {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }

        var params: [String: Any] = [:]
        if let msg = message { params["message"] = msg }

        let result = try await rpc.sendRequest(method: "ping", params: params)

        guard let msg = result["message"] as? String,
            let ts = result["timestamp"]
        else {
            throw CopilotError.invalidResponse("Invalid ping response")
        }

        return PingResponse(
            message: msg,
            timestamp: "\(ts)",
            protocolVersion: result["protocolVersion"] as? Int
        )
    }

    // MARK: - Status

    /// Gets CLI status including version and protocol information.
    public func getStatus() async throws -> GetStatusResponse {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(method: "status.get", params: [:])
        let data = try JSONSerialization.data(withJSONObject: result, options: [])
        return try JSONDecoder().decode(GetStatusResponse.self, from: data)
    }

    /// Gets current authentication status.
    public func getAuthStatus() async throws -> GetAuthStatusResponse {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(method: "auth.getStatus", params: [:])
        let data = try JSONSerialization.data(withJSONObject: result, options: [])
        return try JSONDecoder().decode(GetAuthStatusResponse.self, from: data)
    }

    // MARK: - List Models

    /// Lists available models with their metadata.
    /// Results are cached after the first successful call.
    public func listModels() async throws -> [ModelInfo] {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }

        if let cached = modelsCache {
            return cached
        }

        let result = try await rpc.sendRequest(method: "models.list", params: [:])

        guard let modelsArray = result["models"] as? [[String: Any]] else {
            throw CopilotError.invalidResponse("Invalid models.list response")
        }

        let data = try JSONSerialization.data(withJSONObject: modelsArray, options: [])
        let models = try JSONDecoder().decode([ModelInfo].self, from: data)
        modelsCache = models
        return models
    }

    // MARK: - Session Management

    /// Gets the ID of the most recently updated session.
    public func getLastSessionId() async throws -> String? {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(method: "session.getLastId", params: [:])
        return result["sessionId"] as? String
    }

    /// Gets metadata for a specific session.
    public func getSessionMetadata(_ sessionId: String) async throws -> [String: Any] {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(
            method: "session.getMetadata", params: ["sessionId": sessionId])
        return result
    }

    /// Deletes a session and its data from disk.
    public func deleteSession(_ sessionId: String) async throws {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(
            method: "session.delete", params: ["sessionId": sessionId])
        let success = result["success"] as? Bool ?? false
        if !success {
            let errorMsg = result["error"] as? String ?? "Unknown error"
            throw CopilotError.sessionDeleteFailed(sessionId: sessionId, message: errorMsg)
        }
        sessions.removeValue(forKey: sessionId)
    }

    /// Lists all available sessions known to the server.
    public func listSessions() async throws -> [SessionMetadata] {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(method: "session.list", params: [:])
        guard let sessionsArray = result["sessions"] as? [[String: Any]] else {
            throw CopilotError.invalidResponse("Invalid session.list response")
        }
        return sessionsArray.compactMap { dict in
            guard let sid = dict["sessionId"] as? String,
                let st = dict["startTime"] as? String,
                let mt = dict["modifiedTime"] as? String
            else { return nil }
            return SessionMetadata(
                sessionId: sid,
                startTime: st,
                modifiedTime: mt,
                summary: dict["summary"] as? String,
                isRemote: dict["isRemote"] as? Bool ?? false
            )
        }
    }

    /// Gets the foreground session ID in TUI+server mode.
    public func getForegroundSessionId() async throws -> String? {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(method: "session.getForeground", params: [:])
        return result["sessionId"] as? String
    }

    /// Sets the foreground session in TUI+server mode.
    public func setForegroundSessionId(_ sessionId: String) async throws {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(
            method: "session.setForeground", params: ["sessionId": sessionId])
        let success = result["success"] as? Bool ?? false
        if !success {
            let errorMsg = result["error"] as? String ?? "Failed to set foreground session"
            throw CopilotError.operationFailed(errorMsg)
        }
    }

    // MARK: - Managed Settings

    /// Clears the runtime's cached managed (enterprise) settings so they are fetched again
    /// (`managedSettings.clearCache`).
    public func clearManagedSettingsCache() async throws {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        _ = try await rpc.sendRequest(method: "managedSettings.clearCache", params: [:])
    }

    // MARK: - MCP Secrets

    /// Saves a legacy MCP configuration credential in the runtime-owned secret store
    /// (`mcp.secrets.save`). Requires Copilot CLI 1.0.116 or newer; older runtimes fail with
    /// a method-not-found `JsonRpcError`.
    ///
    /// - Parameters:
    ///   - secretId: Identifier of the credential.
    ///   - value: The credential. It is sent to the runtime and never logged.
    ///   - configDir: Configuration directory, or nil for the runtime's default.
    /// - Returns: `usedKeychain` is true when the OS keychain stored the credential and false
    ///   for the file backend.
    @discardableResult
    public func mcpSecretsSave(
        secretId: String, value: String, configDir: String? = nil
    ) async throws -> McpSecretsSaveResult {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        var params: [String: Any] = ["secretId": secretId, "value": value]
        if let configDir = configDir { params["configDir"] = configDir }
        let result = try await rpc.sendRequest(method: "mcp.secrets.save", params: params)
        return McpSecretsSaveResult(usedKeychain: result["usedKeychain"] as? Bool ?? false)
    }

    /// Deletes a legacy MCP configuration credential from the runtime-owned secret store
    /// (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
    ///
    /// - Parameters:
    ///   - secretId: Identifier of the credential.
    ///   - configDir: Configuration directory, or nil for the runtime's default.
    public func mcpSecretsDelete(secretId: String, configDir: String? = nil) async throws {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        var params: [String: Any] = ["secretId": secretId]
        if let configDir = configDir { params["configDir"] = configDir }
        _ = try await rpc.sendRequest(method: "mcp.secrets.delete", params: params)
    }

    /// Deletes every legacy credential indexed for an MCP server
    /// (`mcp.secrets.deleteServer`). Requires Copilot CLI 1.0.116 or newer.
    ///
    /// - Parameters:
    ///   - serverName: The MCP server name.
    ///   - configDir: Configuration directory, or nil for the runtime's default.
    public func mcpSecretsDeleteServer(serverName: String, configDir: String? = nil) async throws {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        var params: [String: Any] = ["serverName": serverName]
        if let configDir = configDir { params["configDir"] = configDir }
        _ = try await rpc.sendRequest(method: "mcp.secrets.deleteServer", params: params)
    }

    // MARK: - Plugin Extension Approvals

    /// Persists a pending plugin extension permission request as a user-scoped approval,
    /// before the host answers that request normally (`permissions.extensionApprovals.grant`).
    /// Requires Copilot CLI 1.0.116 or newer.
    ///
    /// - Parameters:
    ///   - sessionId: Session that owns the pending request.
    ///   - requestId: ID of the pending extension permission request.
    /// - Returns: Whether the request was eligible and persisted, and the stored approval.
    @discardableResult
    public func extensionApprovalsGrant(
        sessionId: String, requestId: String
    ) async throws -> PermissionsExtensionApprovalsGrantResult {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        let result = try await rpc.sendRequest(
            method: "permissions.extensionApprovals.grant",
            params: ["sessionId": sessionId, "requestId": requestId])
        return PermissionsExtensionApprovalsGrantResult(
            granted: result["granted"] as? Bool ?? false,
            approval: (result["approval"] as? [String: Any]).map { PermissionsExtensionApproval.from($0) }
        )
    }

    /// Lists the user-scoped plugin extension approvals
    /// (`permissions.extensionApprovals.list`). Requires Copilot CLI 1.0.116 or newer.
    ///
    /// - Parameter sessionId: Attached session whose configuration profile to list, or nil
    ///   for the server's profile.
    public func extensionApprovalsList(
        sessionId: String? = nil
    ) async throws -> PermissionsExtensionApprovalsListResult {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        var params: [String: Any] = [:]
        if let sessionId = sessionId { params["sessionId"] = sessionId }
        let result = try await rpc.sendRequest(
            method: "permissions.extensionApprovals.list", params: params)
        let approvals = (result["approvals"] as? [[String: Any]] ?? []).map {
            PermissionsExtensionApproval.from($0)
        }
        return PermissionsExtensionApprovalsListResult(approvals: approvals)
    }

    /// Revokes one user-scoped plugin extension approval
    /// (`permissions.extensionApprovals.revoke`). Requires Copilot CLI 1.0.116 or newer.
    ///
    /// - Parameters:
    ///   - extensionName: The approval's `extensionName`.
    ///   - installationKey: The approval's `installationKey`, unchanged.
    ///   - sessionId: Attached session whose configuration profile to use, or nil.
    /// - Returns: Whether a matching approval was revoked.
    @discardableResult
    public func extensionApprovalsRevoke(
        extensionName: String, installationKey: String, sessionId: String? = nil
    ) async throws -> PermissionsExtensionApprovalsRevokeResult {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }
        var params: [String: Any] = [
            "extensionName": extensionName, "installationKey": installationKey,
        ]
        if let sessionId = sessionId { params["sessionId"] = sessionId }
        let result = try await rpc.sendRequest(
            method: "permissions.extensionApprovals.revoke", params: params)
        return PermissionsExtensionApprovalsRevokeResult(revoked: result["revoked"] as? Bool ?? false)
    }

    // MARK: - Session Filesystem Provider

    /// Sets the session filesystem provider configuration.
    ///
    /// - Parameters:
    ///   - initialCwd: Optional initial working directory.
    ///   - sessionStatePath: Optional path for session state persistence.
    ///   - conventions: Optional list of convention strings.
    public func setSessionFsProvider(
        initialCwd: String? = nil,
        sessionStatePath: String? = nil,
        conventions: [String]? = nil
    ) async throws {
        guard let rpc = rpcClient else {
            throw CopilotError.notConnected
        }

        var params: [String: Any] = [:]
        if let cwd = initialCwd { params["initialCwd"] = cwd }
        if let path = sessionStatePath { params["sessionStatePath"] = path }
        if let conv = conventions { params["conventions"] = conv }

        _ = try await rpc.sendRequest(method: "sessionFs.setProvider", params: params)
    }

    // MARK: - Lifecycle Event Subscription

    /// Subscribes to all session lifecycle events.
    /// - Returns: An ID that can be used to unsubscribe.
    @discardableResult
    public func onLifecycleEvent(_ handler: @escaping SessionLifecycleHandler) -> UUID {
        let id = UUID()
        lifecycleHandlers[id] = handler
        return id
    }

    /// Unsubscribes from lifecycle events.
    public func removeLifecycleHandler(_ id: UUID) {
        lifecycleHandlers.removeValue(forKey: id)
    }

    // MARK: - Private: CLI Server

    private func startCLIServer() async throws {
        let process = Process()

        var args = options.cliArgs ?? []
        args += ["--headless", "--no-auto-update", "--log-level", resolvedLogLevel]

        if resolvedUseStdio {
            args.append("--stdio")
        } else if let port = options.port, port > 0 {
            args += ["--port", String(port)]
        }

        if let token = options.githubToken {
            args += ["--auth-token-env", "COPILOT_SDK_AUTH_TOKEN"]
            var env = options.env ?? ProcessInfo.processInfo.environment
            env["COPILOT_SDK_AUTH_TOKEN"] = token
            process.environment = env
        } else {
            process.environment = options.env
        }

        let useLoggedIn = options.useLoggedInUser ?? (options.githubToken == nil)
        if !useLoggedIn {
            args.append("--no-auto-login")
        }

        if resolvedCliPath.hasSuffix(".js") {
            // Node entry points (for example `@github/copilot/npm-loader.js`) run with node.
            process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
            process.arguments = ["node", resolvedCliPath] + args
        } else if resolvedCliPath.contains("/") {
            // Native binaries and shims with a shebang (`node_modules/.bin/copilot`) run directly.
            process.executableURL = URL(fileURLWithPath: resolvedCliPath)
            process.arguments = args
        } else {
            // Bare command names (the default "copilot") are resolved on PATH.
            process.executableURL = URL(fileURLWithPath: "/usr/bin/env")
            process.arguments = [resolvedCliPath] + args
        }

        if let cwd = options.cwd {
            process.currentDirectoryURL = URL(fileURLWithPath: cwd)
        }

        let stdinPipe = Pipe()
        let stdoutPipe = Pipe()
        let stderrPipe = Pipe()

        process.standardInput = stdinPipe
        process.standardOutput = stdoutPipe
        process.standardError = stderrPipe

        // Forward stderr to our stderr
        stderrPipe.fileHandleForReading.readabilityHandler = { handle in
            let data = handle.availableData
            if !data.isEmpty {
                if let str = String(data: data, encoding: .utf8) {
                    for line in str.split(separator: "\n") {
                        FileHandle.standardError.write(
                            "[CLI subprocess] \(line)\n".data(using: .utf8)!)
                    }
                }
            }
        }

        try process.run()
        cliProcess = process

        // Set up JSON-RPC client over stdio
        let rpc = JsonRpcClient()
        rpcClient = rpc

        await rpc.start(
            readHandle: stdoutPipe.fileHandleForReading,
            writeHandle: stdinPipe.fileHandleForWriting,
            notificationHandler: { [weak self] method, params in
                guard let self = self else { return }
                await self.handleNotification(method: method, params: params)
            }
        )

        // Register request handlers
        await rpc.setRequestHandler(method: "tool.call") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            return try await self.handleToolCall(params)
        }

        await rpc.setRequestHandler(method: "permission.request") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            return try await self.handlePermissionRequest(params)
        }

        await rpc.setRequestHandler(method: "userInput.request") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            return try await self.handleUserInputRequest(params)
        }

        await rpc.setRequestHandler(method: "hooks.invoke") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            return try await self.handleHooksInvoke(params)
        }

        await rpc.setRequestHandler(method: "exitPlanMode.request") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            return try await self.handleExitPlanModeRequest(params)
        }

        await rpc.setRequestHandler(method: "gitHubToken.getToken") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            return try await self.handleGitHubTokenRequest(params)
        }

        await rpc.setRequestHandler(method: "shellAuthentication.request") { [weak self] params in
            guard let self = self else {
                return ShellAuthenticationResponse.cancel.toWireFormat()
            }
            return await self.handleShellAuthenticationRequest(params)
        }

        await rpc.setRequestHandler(method: "shellAuthentication.cancel") { [weak self] params in
            await self?.handleShellAuthenticationCancel(params)
            return [:]
        }

        await rpc.setRequestHandler(method: "memoryStore.store") { [weak self] params in
            guard let self = self else {
                throw JsonRpcError(code: -32000, message: "Client deallocated")
            }
            try await self.handleMemoryStore(params)
            return nil
        }

        await registerNetworkHandlers(rpc)

        // sessionFs.* provider requests (serviced against the real filesystem)
        await rpc.setRequestHandler(method: "sessionFs.readFile") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsReadFile(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.writeFile") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsWriteFile(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.appendFile") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsAppendFile(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.exists") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsExists(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.stat") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsStat(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.mkdir") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsMkdir(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.readdir") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsReaddir(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.readdirWithTypes") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsReaddirWithTypes(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.rm") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsRm(params)
        }
        await rpc.setRequestHandler(method: "sessionFs.rename") { [weak self] params in
            guard let self = self else { throw JsonRpcError(code: -32000, message: "Client deallocated") }
            return try await self.handleSessionFsRename(params)
        }
    }

    private func verifyProtocolVersion() async throws {
        let pingResult = try await ping()
        let expectedVersion = SDK_PROTOCOL_VERSION

        guard let serverVersion = pingResult.protocolVersion else {
            throw CopilotError.protocolVersionMismatch(
                expected: expectedVersion,
                actual: nil,
                message:
                    "SDK supports version \(expectedVersion), but server does not report a protocol version. "
                    + "Please update your server to ensure compatibility."
            )
        }

        if serverVersion != expectedVersion {
            throw CopilotError.protocolVersionMismatch(
                expected: expectedVersion,
                actual: serverVersion,
                message:
                    "SDK supports version \(expectedVersion), but server reports version \(serverVersion). "
                    + "Please update your SDK or server to ensure compatibility."
            )
        }
    }

    /// Declares `CopilotClientOptions.clientInfo` on the `connect` handshake. Runtimes
    /// without `connect` are tolerated (the ping-based version check still runs).
    private func sendClientInfo() async throws {
        guard let rpc = rpcClient, let clientInfo = options.clientInfo?.toWireFormat() else {
            return
        }
        do {
            _ = try await rpc.sendRequest(method: "connect", params: ["clientInfo": clientInfo])
        } catch let error as JsonRpcError where error.isMethodNotFound {
            // Legacy runtime without `connect`.
        }
    }

    /// Calls `network.setProvider` when any network callback is configured, marking each
    /// registered callback with an empty object.
    private func registerNetworkProvider() async throws {
        guard let rpc = rpcClient, let network = options.network, network.hasCallbacks else {
            return
        }
        var params: [String: Any] = [:]
        if let scope = network.scope { params["scope"] = scope.rawValue }
        if network.resolveProxy != nil { params["resolveProxy"] = [String: Any]() }
        if network.authenticateNegotiate != nil {
            params["authenticateNegotiate"] = [String: Any]()
        }
        if network.authenticateBasic != nil { params["authenticateBasic"] = [String: Any]() }
        _ = try await rpc.sendRequest(method: "network.setProvider", params: params)
    }

    /// Registers the `network.*` request handlers for the configured callbacks. Tokens and
    /// passwords are never logged.
    private func registerNetworkHandlers(_ rpc: JsonRpcClient) async {
        guard let network = options.network else { return }
        if let resolveProxy = network.resolveProxy {
            await rpc.setRequestHandler(method: "network.resolveProxy") { params in
                guard let proxy = try await resolveProxy(params["url"] as? String ?? ""),
                    !proxy.isEmpty
                else { return [:] }
                return ["proxy": proxy]
            }
        }
        if let authenticateNegotiate = network.authenticateNegotiate {
            await rpc.setRequestHandler(method: "network.authenticateNegotiate") { params in
                guard let token = try await authenticateNegotiate(params["spn"] as? String ?? ""),
                    !token.isEmpty
                else { return [:] }
                return ["token": token]
            }
        }
        if let authenticateBasic = network.authenticateBasic {
            await rpc.setRequestHandler(method: "network.authenticateBasic") { params in
                let request = NetworkAuthenticateBasicRequest(
                    proxy: params["proxy"] as? String ?? "",
                    realm: params["realm"] as? String,
                    previousRejected: params["previousRejected"] as? Bool ?? false
                )
                guard let credentials = try await authenticateBasic(request) else { return [:] }
                var result: [String: Any] = [:]
                if let username = credentials.username { result["username"] = username }
                if let password = credentials.password { result["password"] = password }
                return result
            }
        }
    }

    // MARK: - Private: Session Options

    /// Validates options shared by `createSession` and `resumeSession`.
    private func validateSessionOptions(
        gitHubToken: String?,
        gitHubTokenProvider: GitHubTokenProvider?,
        availableTools: [String]?,
        memory: MemoryConfiguration?
    ) throws {
        if gitHubToken != nil && gitHubTokenProvider != nil {
            throw CopilotError.operationFailed(
                "gitHubToken and gitHubTokenProvider are mutually exclusive")
        }
        if resolvedMode == .empty && availableTools == nil {
            throw CopilotError.operationFailed(
                "CopilotClient is in mode 'empty' but the session config did not specify "
                    + "'availableTools'. Empty mode requires every session to opt into the tools it wants."
            )
        }
        if let memory = memory, memory.storeMemory != nil, !memory.enabled {
            throw CopilotError.operationFailed(
                "memory.storeMemory requires memory.enabled to be true.")
        }
    }

    /// Groups the flat payload for SDK protocol 4 and applies the empty-mode defaults under
    /// the caller's values.
    private func groupForMode(_ params: [String: Any], isResume: Bool) -> [String: Any] {
        var payload = groupSessionParams(params, isResume: isResume)
        if resolvedMode == .empty {
            applyEmptyModeSessionDefaults(&payload)
        }
        return payload
    }

    /// The `capi` options, folding in the legacy `enableWebSocketResponses` session option.
    private func capiWireFormat(
        _ capi: CapiSessionOptions?, enableWebSocketResponses: Bool?
    ) -> [String: Any]? {
        var wire = capi.flatMap { encodeToDictionary($0) } ?? [:]
        if let webSocket = enableWebSocketResponses, wire["enableWebSocketResponses"] == nil {
            wire["enableWebSocketResponses"] = webSocket
        }
        return wire.isEmpty ? nil : wire
    }

    /// Sends `session.create` / `session.resume` with the GitHub token provider registered,
    /// since the runtime validates the provider while it creates or resumes the session.
    private func sendSessionRequest(
        method: String,
        payload: [String: Any],
        rpc: JsonRpcClient,
        registrationId: String?,
        tokenProvider: GitHubTokenProvider?
    ) async throws -> [String: Any] {
        if let registrationId = registrationId, let tokenProvider = tokenProvider {
            gitHubTokenProviders[registrationId] = tokenProvider
        }
        do {
            return try await rpc.sendRequest(method: method, params: payload)
        } catch {
            if let registrationId = registrationId {
                gitHubTokenProviders.removeValue(forKey: registrationId)
            }
            throw error
        }
    }

    /// Creates a session that unregisters itself (and its GitHub token provider) from this
    /// client when it is detached or destroyed.
    private func makeSession(
        sessionId: String, rpc: JsonRpcClient, workspacePath: String?, registrationId: String?
    ) async -> CopilotSession {
        let session = CopilotSession(
            sessionId: sessionId, rpcClient: rpc, workspacePath: workspacePath)
        let identity = ObjectIdentifier(session)
        await session.setOnClose { [weak self] in
            await self?.releaseSession(
                sessionId, identity: identity, registrationId: registrationId)
        }
        return session
    }

    private func releaseSession(
        _ sessionId: String, identity: ObjectIdentifier, registrationId: String?
    ) {
        if let current = sessions[sessionId], ObjectIdentifier(current) == identity {
            sessions.removeValue(forKey: sessionId)
        }
        if let registrationId = registrationId {
            gitHubTokenProviders.removeValue(forKey: registrationId)
        }
    }

    private func ensureConnected() async throws {
        if rpcClient == nil {
            if resolvedAutoStart {
                try await start()
            } else {
                throw CopilotError.notConnected
            }
        }
    }

    // MARK: - Private: Notification Handling

    private func handleNotification(method: String, params: [String: Any]) {
        switch method {
        case "session.event":
            handleSessionEventNotification(params)
        case "session.lifecycle":
            handleSessionLifecycleNotification(params)
        default:
            break
        }
    }

    private func handleSessionEventNotification(_ params: [String: Any]) {
        guard let sessionId = params["sessionId"] as? String,
            let eventDict = params["event"] as? [String: Any],
            let event = SessionEvent.from(dict: eventDict)
        else { return }

        if let session = sessions[sessionId] {
            Task {
                await session.dispatchEvent(event)
            }
        }
    }

    private func handleSessionLifecycleNotification(_ params: [String: Any]) {
        let data = try? JSONSerialization.data(withJSONObject: params, options: [])
        guard let data = data,
            let event = try? JSONDecoder().decode(SessionLifecycleEvent.self, from: data)
        else { return }

        for handler in lifecycleHandlers.values {
            handler(event)
        }
    }

    // MARK: - Private: Server Request Handlers

    private func handleToolCall(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let sessionId = params["sessionId"] as? String,
            let toolCallId = params["toolCallId"] as? String,
            let toolName = params["toolName"] as? String
        else {
            throw JsonRpcError(code: -32602, message: "Invalid tool call payload")
        }

        guard let session = sessions[sessionId] else {
            throw JsonRpcError(code: -32000, message: "Unknown session \(sessionId)")
        }

        let handler = await session.getToolHandler(toolName)
        guard let handler = handler else {
            return [
                "result": [
                    "textResultForLlm":
                        "Tool '\(toolName)' is not supported by this client instance.",
                    "resultType": "failure",
                    "error": "tool '\(toolName)' not supported",
                ] as [String: Any]
            ]
        }

        let invocation = ToolInvocation(
            sessionId: sessionId,
            toolCallId: toolCallId,
            toolName: toolName,
            arguments: params["arguments"]
        )

        do {
            let rawResult = try await handler(params["arguments"], invocation)
            let resultObj = normalizeToolResult(rawResult)
            return ["result": encodeToDictionary(resultObj) ?? [:]]
        } catch {
            return [
                "result": [
                    "textResultForLlm":
                        "Invoking this tool produced an error. Detailed information is not available.",
                    "resultType": "failure",
                    "error": error.localizedDescription,
                    "toolTelemetry": [:],
                ] as [String: Any]
            ]
        }
    }

    private func handlePermissionRequest(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let sessionId = params["sessionId"] as? String,
            let requestData = params["permissionRequest"] as? [String: Any]
        else {
            throw JsonRpcError(code: -32602, message: "Invalid permission request payload")
        }

        guard let session = sessions[sessionId] else {
            throw JsonRpcError(code: -32000, message: "Session not found: \(sessionId)")
        }

        do {
            let result = try await session.handlePermissionRequest(requestData)
            return ["result": encodeToDictionary(result) ?? [:]]
        } catch {
            return [
                "result": [
                    "kind": "denied-no-approval-rule-and-could-not-request-from-user"
                ] as [String: Any]
            ]
        }
    }

    private func handleUserInputRequest(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let sessionId = params["sessionId"] as? String,
            let question = params["question"] as? String
        else {
            throw JsonRpcError(code: -32602, message: "Invalid user input request payload")
        }

        guard let session = sessions[sessionId] else {
            throw JsonRpcError(code: -32000, message: "Session not found: \(sessionId)")
        }

        let request = UserInputRequest(
            question: question,
            choices: params["choices"] as? [String],
            allowFreeform: params["allowFreeform"] as? Bool,
            requestId: params["requestId"] as? String,
            imageAttachmentConstraints: (params["imageAttachmentConstraints"] as? [String: Any])
                .map { UserInputImageAttachmentConstraints.from($0) },
            meta: (params["_meta"] as? [String: Any])?.mapValues { AnyCodable($0) }
        )

        let result = try await session.handleUserInputRequest(request)
        return result.toWireFormat()
    }

    private func handleHooksInvoke(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let sessionId = params["sessionId"] as? String,
            let hookType = params["hookType"] as? String
        else {
            throw JsonRpcError(code: -32602, message: "Invalid hooks invoke payload")
        }

        guard let session = sessions[sessionId] else {
            throw JsonRpcError(code: -32000, message: "Session not found: \(sessionId)")
        }

        let output = try await session.handleHooksInvoke(hookType: hookType, input: params["input"])
        if let output = output {
            return ["output": output]
        }
        return ["output": [:] as [String: Any]]
    }

    private func handleExitPlanModeRequest(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let sessionId = params["sessionId"] as? String else {
            throw JsonRpcError(code: -32602, message: "Invalid exit plan mode request payload")
        }

        guard let session = sessions[sessionId] else {
            return ["approved": true]
        }

        do {
            let request = ExitPlanModeRequest(sessionId: sessionId)
            let result = try await session.handleExitPlanModeRequest(request)
            return ["approved": result.approved]
        } catch {
            return ["approved": true]
        }
    }

    /// Answers `gitHubToken.getToken` with the provider registered for the request's
    /// `registrationId`: `{kind: "token", accessToken, tokenType?, expiresIn}` or
    /// `{kind: "cancelled"}`.
    private func handleGitHubTokenRequest(_ params: [String: Any]) async throws -> [String: Any]? {
        let registrationId = params["registrationId"] as? String ?? ""
        guard let provider = gitHubTokenProviders[registrationId] else {
            throw JsonRpcError(
                code: -32000,
                message: "No GitHub token provider registered for \"\(registrationId)\"")
        }
        let result = try await provider(
            GitHubTokenRequest(
                host: params["host"] as? String ?? "",
                sessionId: params["sessionId"] as? String,
                reason: params["reason"] as? String ?? ""
            ))
        return result.toWireFormat()
    }

    /// Answers `shellAuthentication.request` with the session's handler, or
    /// `{action: "cancel"}` when nobody can answer.
    private func handleShellAuthenticationRequest(_ params: [String: Any]) async -> [String: Any]? {
        let request = ShellAuthenticationRequest.from(params)
        guard let session = sessions[request.sessionId] else {
            return ShellAuthenticationResponse.cancel.toWireFormat()
        }
        let response = await session.handleShellAuthenticationRequest(request)
        return response.toWireFormat()
    }

    /// Handles `shellAuthentication.cancel`: the pending request resolves to `cancel`.
    private func handleShellAuthenticationCancel(_ params: [String: Any]) async {
        guard let sessionId = params["sessionId"] as? String,
            let requestId = params["requestId"] as? String,
            let session = sessions[sessionId]
        else { return }
        await session.cancelShellAuthenticationRequest(requestId)
    }

    /// Stores a memory with the session's `MemoryConfiguration.storeMemory` handler
    /// (`memoryStore.store`, replied to with null).
    private func handleMemoryStore(_ params: [String: Any]) async throws {
        let memory = StoredMemory.from(params)
        guard let session = sessions[memory.sessionId] else {
            throw JsonRpcError(code: -32000, message: "Memory store owner is unavailable")
        }
        try await session.handleMemoryStore(memory)
    }

    // MARK: - Private: SessionFs Provider Handlers
    //
    // When a session filesystem provider is registered, the CLI drives
    // filesystem access by sending sessionFs.* requests back to the client.
    // These handlers service those requests against the real filesystem using
    // the absolute paths supplied by the CLI. Errors are returned in-band as
    // { code, message } (never thrown) so the CLI can distinguish a missing
    // file (ENOENT) from other failures without the JSON-RPC call itself
    // failing, matching the reference SDK contract.

    private func sessionFsError(_ error: Error) -> [String: Any] {
        let ns = error as NSError
        let isNotFound =
            (ns.domain == NSCocoaErrorDomain
                && (ns.code == NSFileReadNoSuchFileError || ns.code == NSFileNoSuchFileError))
            || (ns.domain == NSPOSIXErrorDomain && ns.code == Int(ENOENT))
        return ["code": isNotFound ? "ENOENT" : "UNKNOWN", "message": ns.localizedDescription]
    }

    private func sessionFsISODate(_ date: Date) -> String {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime]
        return formatter.string(from: date)
    }

    private func applySessionFsMode(_ mode: Any?, to path: String) {
        guard let modeNum = mode as? NSNumber else { return }
        try? FileManager.default.setAttributes([.posixPermissions: modeNum], ofItemAtPath: path)
    }

    private func handleSessionFsReadFile(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["content": "", "error": ["code": "UNKNOWN", "message": "missing path"]]
        }
        do {
            let content = try String(contentsOfFile: path, encoding: .utf8)
            return ["content": content]
        } catch {
            return ["content": "", "error": sessionFsError(error)]
        }
    }

    private func handleSessionFsWriteFile(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["code": "UNKNOWN", "message": "missing path"]
        }
        let content = params["content"] as? String ?? ""
        do {
            let dir = (path as NSString).deletingLastPathComponent
            if !dir.isEmpty {
                try FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
            }
            try content.write(toFile: path, atomically: true, encoding: .utf8)
            applySessionFsMode(params["mode"], to: path)
            return nil
        } catch {
            return sessionFsError(error)
        }
    }

    private func handleSessionFsAppendFile(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["code": "UNKNOWN", "message": "missing path"]
        }
        let content = params["content"] as? String ?? ""
        do {
            let dir = (path as NSString).deletingLastPathComponent
            if !dir.isEmpty {
                try FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
            }
            if FileManager.default.fileExists(atPath: path),
                let handle = FileHandle(forWritingAtPath: path)
            {
                defer { try? handle.close() }
                try handle.seekToEnd()
                if let data = content.data(using: .utf8) {
                    try handle.write(contentsOf: data)
                }
            } else {
                try content.write(toFile: path, atomically: true, encoding: .utf8)
            }
            applySessionFsMode(params["mode"], to: path)
            return nil
        } catch {
            return sessionFsError(error)
        }
    }

    private func handleSessionFsExists(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else { return ["exists": false] }
        return ["exists": FileManager.default.fileExists(atPath: path)]
    }

    private func handleSessionFsStat(_ params: [String: Any]) async throws -> [String: Any]? {
        let now = sessionFsISODate(Date())
        guard let path = params["path"] as? String else {
            return [
                "isFile": false, "isDirectory": false, "size": 0,
                "mtime": now, "birthtime": now,
                "error": ["code": "UNKNOWN", "message": "missing path"],
            ]
        }
        do {
            let attrs = try FileManager.default.attributesOfItem(atPath: path)
            var isDir: ObjCBool = false
            _ = FileManager.default.fileExists(atPath: path, isDirectory: &isDir)
            let size = (attrs[.size] as? NSNumber)?.intValue ?? 0
            let mtime = (attrs[.modificationDate] as? Date) ?? Date()
            let btime = (attrs[.creationDate] as? Date) ?? mtime
            return [
                "isFile": !isDir.boolValue,
                "isDirectory": isDir.boolValue,
                "size": size,
                "mtime": sessionFsISODate(mtime),
                "birthtime": sessionFsISODate(btime),
            ]
        } catch {
            return [
                "isFile": false, "isDirectory": false, "size": 0,
                "mtime": now, "birthtime": now,
                "error": sessionFsError(error),
            ]
        }
    }

    private func handleSessionFsMkdir(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["code": "UNKNOWN", "message": "missing path"]
        }
        let recursive = params["recursive"] as? Bool ?? false
        do {
            try FileManager.default.createDirectory(atPath: path, withIntermediateDirectories: recursive)
            applySessionFsMode(params["mode"], to: path)
            return nil
        } catch {
            return sessionFsError(error)
        }
    }

    private func handleSessionFsReaddir(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["entries": [String](), "error": ["code": "UNKNOWN", "message": "missing path"]]
        }
        do {
            let entries = try FileManager.default.contentsOfDirectory(atPath: path)
            return ["entries": entries]
        } catch {
            return ["entries": [String](), "error": sessionFsError(error)]
        }
    }

    private func handleSessionFsReaddirWithTypes(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["entries": [[String: Any]](), "error": ["code": "UNKNOWN", "message": "missing path"]]
        }
        do {
            let names = try FileManager.default.contentsOfDirectory(atPath: path)
            let base = path as NSString
            let entries: [[String: Any]] = names.map { name in
                var isDir: ObjCBool = false
                _ = FileManager.default.fileExists(
                    atPath: base.appendingPathComponent(name), isDirectory: &isDir)
                return ["name": name, "type": isDir.boolValue ? "directory" : "file"]
            }
            return ["entries": entries]
        } catch {
            return ["entries": [[String: Any]](), "error": sessionFsError(error)]
        }
    }

    private func handleSessionFsRm(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let path = params["path"] as? String else {
            return ["code": "UNKNOWN", "message": "missing path"]
        }
        let force = params["force"] as? Bool ?? false
        if !FileManager.default.fileExists(atPath: path) {
            if force { return nil }
            return sessionFsError(
                NSError(
                    domain: NSPOSIXErrorDomain, code: Int(ENOENT),
                    userInfo: [NSLocalizedDescriptionKey: "no such file or directory: \(path)"]))
        }
        do {
            try FileManager.default.removeItem(atPath: path)
            return nil
        } catch {
            return sessionFsError(error)
        }
    }

    private func handleSessionFsRename(_ params: [String: Any]) async throws -> [String: Any]? {
        guard let src = params["src"] as? String, let dest = params["dest"] as? String else {
            return ["code": "UNKNOWN", "message": "missing src or dest"]
        }
        do {
            if FileManager.default.fileExists(atPath: dest) {
                try FileManager.default.removeItem(atPath: dest)
            }
            try FileManager.default.moveItem(atPath: src, toPath: dest)
            return nil
        } catch {
            return sessionFsError(error)
        }
    }

    private func injectTraceContext(_ params: inout [String: Any]) async {
        guard let provider = options.onGetTraceContext else { return }
        let ctx = await provider()
        if let tp = ctx.traceparent { params["traceparent"] = tp }
        if let ts = ctx.tracestate { params["tracestate"] = ts }
    }

    // MARK: - Private: Tool Result Normalization

    private func normalizeToolResult(_ result: Any?) -> ToolResultObject {
        if result == nil {
            return ToolResultObject(
                textResultForLlm: "Tool returned no result",
                resultType: .failure,
                error: "tool returned no result"
            )
        }

        if let resultObj = result as? ToolResultObject {
            return resultObj
        }

        if let toolResult = result as? ToolResult {
            return toolResult.toResultObject()
        }

        if let str = result as? String {
            return ToolResultObject(textResultForLlm: str, resultType: .success)
        }

        // Try to JSON-serialize other types
        if let data = try? JSONSerialization.data(
            withJSONObject: result!, options: []),
            let str = String(data: data, encoding: .utf8)
        {
            return ToolResultObject(textResultForLlm: str, resultType: .success)
        }

        return ToolResultObject(
            textResultForLlm: String(describing: result!), resultType: .success)
    }
}

// MARK: - Codable Helpers

/// Encodes a Codable value to a dictionary.
func encodeToDictionary<T: Encodable>(_ value: T) -> [String: Any]? {
    guard let data = try? JSONEncoder().encode(value),
        let dict = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]
    else {
        return nil
    }
    return dict
}

// MARK: - Errors

/// Errors specific to the Copilot SDK.
public enum CopilotError: Error, LocalizedError {
    case notConnected
    case invalidResponse(String)
    case protocolVersionMismatch(expected: Int, actual: Int?, message: String)
    case sessionCleanupFailed(sessionId: String, underlying: Error)
    case sessionDeleteFailed(sessionId: String, message: String)
    case operationFailed(String)
    case timeout(String)

    public var errorDescription: String? {
        switch self {
        case .notConnected:
            return "Client not connected. Call start() first."
        case .invalidResponse(let msg):
            return "Invalid response from server: \(msg)"
        case .protocolVersionMismatch(_, _, let msg):
            return "SDK protocol version mismatch: \(msg)"
        case .sessionCleanupFailed(let sid, let err):
            return "Failed to clean up session \(sid): \(err.localizedDescription)"
        case .sessionDeleteFailed(let sid, let msg):
            return "Failed to delete session \(sid): \(msg)"
        case .operationFailed(let msg):
            return "Operation failed: \(msg)"
        case .timeout(let msg):
            return "Timeout: \(msg)"
        }
    }
}
