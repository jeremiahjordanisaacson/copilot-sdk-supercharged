// ----------------------------------------------------------------------------------------------------
//  Copyright (c) Microsoft Corporation. All rights reserved.
// ----------------------------------------------------------------------------------------------------

import Foundation

/// Represents a single conversation session with the Copilot CLI.
///
/// A session maintains conversation state, handles events, and manages tool execution.
/// Sessions are created via `CopilotClient.createSession()` or resumed via
/// `CopilotClient.resumeSession()`.
///
/// Example:
/// ```swift
/// let session = try await client.createSession()
///
/// session.on { event in
///     if event.type == "assistant.message" {
///         print(event.data["content"] as? String ?? "")
///     }
/// }
///
/// let response = try await session.sendAndWait(MessageOptions(prompt: "Hello!"))
/// try await session.destroy()
/// ```
public actor CopilotSession {
    // MARK: - Properties

    /// The unique identifier for this session.
    public let sessionId: String

    /// Path to the session workspace directory when infinite sessions are enabled.
    public let workspacePath: String?

    private let rpcClient: JsonRpcClient
    private var eventHandlers: [UUID: SessionEventHandler] = [:]
    private var typedEventHandlers: [String: [UUID: SessionEventHandler]] = [:]
    private var toolHandlers: [String: ToolHandler] = [:]
    private var permissionHandler: PermissionHandler?
    private var userInputHandler: UserInputHandler?
    private var hooks: SessionHooks?
    private var exitPlanModeHandler: ExitPlanModeHandler?
    private var shellAuthenticationHandler: ShellAuthenticationHandler?
    private var memoryStoreHandler: MemoryStoreHandler?
    private var pendingShellAuthentications: [String: ShellAuthenticationGate] = [:]
    private var onClose: (@Sendable () async -> Void)?
    private var isClosed = false

    // MARK: - Init

    /// Creates a new CopilotSession.
    /// - Parameters:
    ///   - sessionId: The unique session identifier.
    ///   - rpcClient: The JSON-RPC client for communication.
    ///   - workspacePath: Optional workspace path for infinite sessions.
    init(sessionId: String, rpcClient: JsonRpcClient, workspacePath: String? = nil) {
        self.sessionId = sessionId
        self.rpcClient = rpcClient
        self.workspacePath = workspacePath
    }

    /// Sets the callback invoked once when the session is detached or destroyed.
    func setOnClose(_ handler: @escaping @Sendable () async -> Void) {
        onClose = handler
    }

    // MARK: - Send Message

    /// Sends a message to this session.
    ///
    /// - Parameter options: The message options including prompt and optional attachments.
    /// - Returns: The message ID of the sent message.
    @discardableResult
    public func send(_ options: MessageOptions) async throws -> String {
        var params: [String: Any] = [
            "sessionId": sessionId,
            "prompt": options.prompt,
        ]

        if let attachments = options.attachments {
            params["attachments"] = attachments.map { $0.toDict() }
        }
        if let mode = options.mode {
            params["mode"] = mode
        }
        // The legacy `responseFormat` string is not accepted by SDK protocol 4 runtimes;
        // structured output is requested with `responseSchema`.
        if let schema = options.responseSchema {
            params["responseFormat"] = [
                "type": "json_schema",
                "jsonSchema": ["name": "response", "strict": true, "schema": schema] as [String: Any],
            ] as [String: Any]
        }
        if let source = options.source {
            params["source"] = source.rawValue
        }
        if let clientTurnId = options.clientTurnId {
            params["clientTurnId"] = clientTurnId
        }
        if let imageOptions = options.imageOptions {
            var imageDict: [String: Any] = [:]
            if let size = imageOptions.size { imageDict["size"] = size }
            if let quality = imageOptions.quality { imageDict["quality"] = quality }
            if let style = imageOptions.style { imageDict["style"] = style }
            params["imageOptions"] = imageDict
        }
        if let requestHeaders = options.requestHeaders {
            params["requestHeaders"] = requestHeaders
        }
        if let agentMode = options.agentMode {
            params["agentMode"] = agentMode
        }
        if let displayPrompt = options.displayPrompt {
            params["displayPrompt"] = displayPrompt
        }

        let response = try await rpcClient.sendRequest(method: "session.send", params: params)
        guard let messageId = response["messageId"] as? String else {
            throw CopilotError.invalidResponse("Missing messageId in session.send response")
        }
        return messageId
    }

    /// Sends a message and waits until the session becomes idle.
    ///
    /// This is a convenience method that combines `send` with waiting for the
    /// `session.idle` event. Events are still delivered to handlers while waiting.
    ///
    /// - Parameters:
    ///   - options: The message options.
    ///   - timeout: Timeout in seconds. Defaults to 60 seconds.
    /// - Returns: The final assistant message event, or nil if none was received.
    public func sendAndWait(
        _ options: MessageOptions,
        timeout: TimeInterval = 60
    ) async throws -> SessionEvent? {
        let state = SendAndWaitState()
        let handlerId = UUID()

        return try await withCheckedThrowingContinuation { continuation in
            // Set up a timeout task
            let timeoutTask = Task {
                try await Task.sleep(nanoseconds: UInt64(timeout * 1_000_000_000))
                if state.tryResume() {
                    Task { await self.removeEventHandler(handlerId) }
                    continuation.resume(
                        throwing: CopilotError.timeout(
                            "Timeout after \(timeout)s waiting for session.idle"))
                }
            }

            // Register event handler BEFORE sending to avoid race conditions
            let handler: SessionEventHandler = { event in
                if event.type == "assistant.message" {
                    state.setLastAssistantMessage(event)
                } else if event.type == "session.idle" {
                    if state.tryResume() {
                        timeoutTask.cancel()
                        Task { await self.removeEventHandler(handlerId) }
                        continuation.resume(returning: state.getLastAssistantMessage())
                    }
                } else if event.type == "session.error" {
                    if state.tryResume() {
                        timeoutTask.cancel()
                        Task { await self.removeEventHandler(handlerId) }
                        let msg = event.data["message"] as? String ?? "Unknown session error"
                        continuation.resume(throwing: CopilotError.operationFailed(msg))
                    }
                }
            }

            eventHandlers[handlerId] = handler

            // Now send the message
            Task {
                do {
                    try await self.send(options)
                } catch {
                    if state.tryResume() {
                        timeoutTask.cancel()
                        await self.removeEventHandler(handlerId)
                        continuation.resume(throwing: error)
                    }
                }
            }
        }
    }

    // MARK: - Event Subscription

    /// Subscribes to all events from this session.
    ///
    /// - Parameter handler: A callback function that receives all session events.
    /// - Returns: An ID that can be used to unsubscribe via `removeEventHandler`.
    @discardableResult
    public func on(_ handler: @escaping SessionEventHandler) -> UUID {
        let id = UUID()
        eventHandlers[id] = handler
        return id
    }

    /// Subscribes to a specific event type from this session.
    ///
    /// - Parameters:
    ///   - eventType: The event type string (e.g., "assistant.message", "session.idle").
    ///   - handler: A callback function that receives events of that type.
    /// - Returns: An ID that can be used to unsubscribe.
    @discardableResult
    public func on(_ eventType: String, handler: @escaping SessionEventHandler) -> UUID {
        let id = UUID()
        if typedEventHandlers[eventType] == nil {
            typedEventHandlers[eventType] = [:]
        }
        typedEventHandlers[eventType]![id] = handler
        return id
    }

    /// Removes an event handler by its ID.
    public func removeEventHandler(_ id: UUID) {
        eventHandlers.removeValue(forKey: id)
        for key in typedEventHandlers.keys {
            typedEventHandlers[key]?.removeValue(forKey: id)
        }
    }

    /// Subscribes to live shell output of running tools (`tool.shell_output` events).
    ///
    /// - Parameter handler: Receives each output chunk in publication order.
    /// - Returns: An ID that can be used to unsubscribe via `removeEventHandler`.
    @discardableResult
    public func onShellOutput(_ handler: @escaping @Sendable (ToolShellOutputData) -> Void) -> UUID {
        return on(SessionEventTypes.toolShellOutput) { event in
            if let data = ToolShellOutputData.from(event) {
                handler(data)
            }
        }
    }

    // MARK: - Event Dispatch (Internal)

    /// Dispatches an event to all registered handlers.
    func dispatchEvent(_ event: SessionEvent) {
        // Dispatch to typed handlers
        if let typed = typedEventHandlers[event.type] {
            for handler in typed.values {
                handler(event)
            }
        }

        // Dispatch to wildcard handlers
        for handler in eventHandlers.values {
            handler(event)
        }
    }

    // MARK: - Tool Registration (Internal)

    /// Registers tool handlers for this session.
    func registerTools(_ tools: [Tool]) {
        toolHandlers.removeAll()
        for tool in tools {
            toolHandlers[tool.name] = tool.handler
        }
    }

    /// Gets a tool handler by name.
    func getToolHandler(_ name: String) -> ToolHandler? {
        return toolHandlers[name]
    }

    // MARK: - Permission Handler (Internal)

    /// Registers a permission handler.
    func registerPermissionHandler(_ handler: @escaping PermissionHandler) {
        permissionHandler = handler
    }

    /// Handles a permission request from the server.
    func handlePermissionRequest(_ requestData: [String: Any]) async throws
        -> PermissionRequestResult
    {
        guard let handler = permissionHandler else {
            return .denied
        }

        let request = PermissionRequest(
            kind: requestData["kind"] as? String ?? "unknown",
            toolCallId: requestData["toolCallId"] as? String,
            rawData: requestData
        )

        do {
            return try await handler(request, sessionId)
        } catch {
            return .denied
        }
    }

    // MARK: - User Input Handler (Internal)

    /// Registers a user input handler.
    func registerUserInputHandler(_ handler: @escaping UserInputHandler) {
        userInputHandler = handler
    }

    /// Handles a user input request from the server.
    func handleUserInputRequest(_ request: UserInputRequest) async throws -> UserInputResponse {
        guard let handler = userInputHandler else {
            throw CopilotError.operationFailed(
                "User input requested but no handler registered")
        }
        return try await handler(request, sessionId)
    }

    // MARK: - Hooks (Internal)

    /// Registers hook handlers.
    func registerHooks(_ hooks: SessionHooks) {
        self.hooks = hooks
    }

    // MARK: - Exit Plan Mode Handler (Internal)

    /// Registers an exit plan mode handler.
    func registerExitPlanModeHandler(_ handler: @escaping ExitPlanModeHandler) {
        exitPlanModeHandler = handler
    }

    /// Handles an exit plan mode request from the server.
    func handleExitPlanModeRequest(_ request: ExitPlanModeRequest) async throws -> ExitPlanModeResponse {
        guard let handler = exitPlanModeHandler else {
            return ExitPlanModeResponse(approved: true)
        }
        do {
            return try await handler(request)
        } catch {
            return ExitPlanModeResponse(approved: true)
        }
    }

    // MARK: - Shell Authentication (Internal)

    /// Registers the shell authentication handler.
    func registerShellAuthenticationHandler(_ handler: ShellAuthenticationHandler?) {
        shellAuthenticationHandler = handler
    }

    /// Answers a `shellAuthentication.request`. Resolves to `.cancel` when no handler is
    /// registered, the handler throws, or the runtime cancels the request first.
    func handleShellAuthenticationRequest(_ request: ShellAuthenticationRequest) async
        -> ShellAuthenticationResponse
    {
        guard let handler = shellAuthenticationHandler, !isClosed, !request.requestId.isEmpty
        else {
            return .cancel
        }
        let requestId = request.requestId
        let gate = ShellAuthenticationGate()
        pendingShellAuthentications[requestId]?.cancel()
        pendingShellAuthentications[requestId] = gate
        let response = await withCheckedContinuation {
            (continuation: CheckedContinuation<ShellAuthenticationResponse, Never>) in
            gate.bind(continuation)
            gate.attach(
                Task {
                    let answer = (try? await handler(request)) ?? .cancel
                    gate.resume(answer)
                })
        }
        if pendingShellAuthentications[requestId] === gate {
            pendingShellAuthentications.removeValue(forKey: requestId)
        }
        return response
    }

    /// Handles a `shellAuthentication.cancel`: the pending request resolves to `.cancel`
    /// and any later answer from the handler is ignored.
    func cancelShellAuthenticationRequest(_ requestId: String) {
        pendingShellAuthentications.removeValue(forKey: requestId)?.cancel()
    }

    // MARK: - Memory Store (Internal)

    /// Registers the app-side memory store.
    func registerMemoryStore(_ handler: MemoryStoreHandler?) {
        memoryStoreHandler = handler
    }

    /// Stores a memory with the registered handler (`memoryStore.store`).
    func handleMemoryStore(_ memory: StoredMemory) async throws {
        guard let handler = memoryStoreHandler, !isClosed else {
            throw JsonRpcError(code: -32000, message: "Memory store owner is unavailable")
        }
        try await handler(memory)
    }

    /// Handles a hook invocation from the server.
    func handleHooksInvoke(hookType: String, input: Any?) async throws -> Any? {
        guard let hooks = hooks else {
            return nil
        }

        switch hookType {
        case "preToolUse":
            guard let handler = hooks.onPreToolUse else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(PreToolUseHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "postToolUse":
            guard let handler = hooks.onPostToolUse else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(PostToolUseHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "userPromptSubmitted":
            guard let handler = hooks.onUserPromptSubmitted else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(
                    UserPromptSubmittedHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "userPromptTransformed":
            guard let handler = hooks.onUserPromptTransformed else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(
                    UserPromptSubmittedHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "sessionStart":
            guard let handler = hooks.onSessionStart else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(SessionStartHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "sessionEnd":
            guard let handler = hooks.onSessionEnd else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(SessionEndHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "errorOccurred":
            guard let handler = hooks.onErrorOccurred else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(ErrorOccurredHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "subagentStart":
            guard let handler = hooks.onSubagentStart else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(SubagentStartHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        case "subagentStop":
            guard let handler = hooks.onSubagentStop else { return nil }
            if let inputDict = input as? [String: Any] {
                let data = try JSONSerialization.data(withJSONObject: inputDict, options: [])
                let hookInput = try JSONDecoder().decode(SubagentStopHookInput.self, from: data)
                let output = try await handler(hookInput, sessionId)
                if let output = output {
                    return encodeToDictionary(output)
                }
            }
            return nil

        default:
            return nil
        }
    }

    // MARK: - Get Metadata

    /// Retrieves metadata for this session.
    ///
    /// - Returns: A dictionary containing the session metadata.
    public func getMetadata() async throws -> [String: Any] {
        return try await rpcClient.sendRequest(
            method: "session.getMetadata",
            params: ["sessionId": sessionId]
        )
    }

    // MARK: - Get Messages

    /// Retrieves all events and messages from this session's history.
    ///
    /// - Returns: An array of all session events.
    public func getMessages() async throws -> [SessionEvent] {
        let response = try await rpcClient.sendRequest(
            method: "session.getMessages",
            params: ["sessionId": sessionId]
        )

        guard let events = response["events"] as? [[String: Any]] else {
            throw CopilotError.invalidResponse("Invalid getMessages response")
        }

        return events.compactMap { SessionEvent.from(dict: $0) }
    }

    // MARK: - Destroy

    /// Releases this session without deleting its on-disk state (`session.detach`), so it
    /// can be resumed later with `CopilotClient.resumeSession`. Falls back to the legacy
    /// `session.destroy` on runtimes without `session.detach`.
    ///
    /// After calling this method, the session can no longer be used. All event handlers
    /// and tool handlers are cleared.
    public func detach() async throws {
        if isClosed { return }
        var response: [String: Any] = [:]
        do {
            response = try await rpcClient.sendRequest(
                method: "session.detach", params: ["sessionId": sessionId])
            if response["success"] as? Bool == false {
                response = try await rpcClient.sendRequest(
                    method: "session.detach", params: ["sessionId": sessionId])
            }
        } catch let error as JsonRpcError where error.isMethodNotFound {
            response = try await rpcClient.sendRequest(
                method: "session.destroy", params: ["sessionId": sessionId])
        }
        if response["success"] as? Bool == false {
            let message = response["error"] as? String ?? "Unknown error"
            throw CopilotError.operationFailed("Failed to detach session \(sessionId): \(message)")
        }
        await releaseResources()
    }

    /// Destroys this session and releases all associated resources.
    ///
    /// Equivalent to `detach()`: the on-disk state is kept, so the conversation can be
    /// resumed with `CopilotClient.resumeSession`. To delete it, use
    /// `CopilotClient.deleteSession`.
    public func destroy() async throws {
        try await detach()
    }

    /// Clears local state, resolves pending prompts, and notifies the owning client.
    private func releaseResources() async {
        if isClosed { return }
        isClosed = true
        eventHandlers.removeAll()
        typedEventHandlers.removeAll()
        toolHandlers.removeAll()
        permissionHandler = nil
        userInputHandler = nil
        hooks = nil
        exitPlanModeHandler = nil
        shellAuthenticationHandler = nil
        memoryStoreHandler = nil
        for gate in pendingShellAuthentications.values {
            gate.cancel()
        }
        pendingShellAuthentications.removeAll()
        let close = onClose
        onClose = nil
        await close?()
    }

    // MARK: - Abort

    /// Aborts the currently processing message in this session.
    ///
    /// The session remains valid and can continue to be used for new messages. Failures
    /// arrive as a `JsonRpcError`.
    public func abort() async throws {
        let _ = try await rpcClient.sendRequest(
            method: "session.abort",
            params: ["sessionId": sessionId]
        )
    }

    // MARK: - Model, Policy, and Permission Controls

    /// Changes the Auto routing preference without changing the session's model
    /// (`session.model.switchAutoTier`). Pass nil to return to the provider's default Auto
    /// routing. Requires the session model to be `auto`. Experimental.
    ///
    /// - Returns: The raw result dictionary from the runtime.
    @discardableResult
    public func switchAutoTier(_ tier: AutoTier?) async throws -> [String: Any] {
        var params: [String: Any] = ["sessionId": sessionId]
        if let tier = tier {
            params["autoTier"] = tier.rawValue
        } else {
            params["autoTier"] = NSNull()
        }
        return try await rpcClient.sendRequest(
            method: "session.model.switchAutoTier", params: params)
    }

    /// Re-runs the managed-settings Policy Helper for this session
    /// (`session.managedSettings.recheckPolicyHelper`).
    ///
    /// - Returns: The managed settings snapshot; when the helper still fails it carries
    ///   `policyHelperFailure` (parse it with `PolicyHelperFailure.from(_:)`).
    @discardableResult
    public func recheckPolicyHelper() async throws -> [String: Any] {
        return try await rpcClient.sendRequest(
            method: "session.managedSettings.recheckPolicyHelper",
            params: ["sessionId": sessionId]
        )
    }

    /// Sets the permission mode (`session.permissions.setMode`), for example `"manual"`,
    /// `"assisted"`, or `"allow-all"`. A refused mode returns `success == false` with a
    /// `reason` such as `PermissionModeRefusalReason.managedPolicy`.
    public func setPermissionMode(_ mode: String) async throws -> PermissionModeSetResult {
        let result = try await rpcClient.sendRequest(
            method: "session.permissions.setMode",
            params: ["sessionId": sessionId, "mode": mode]
        )
        return PermissionModeSetResult(
            success: result["success"] as? Bool ?? false,
            mode: result["mode"] as? String ?? mode,
            reason: result["reason"] as? String
        )
    }
}

/// Resolves a pending shell authentication request exactly once, whether the handler
/// answers or the runtime cancels the request first.
private final class ShellAuthenticationGate: @unchecked Sendable {
    private let lock = NSLock()
    private var continuation: CheckedContinuation<ShellAuthenticationResponse, Never>?
    private var task: Task<Void, Never>?
    private var cancelled = false

    /// Binds the continuation that receives the answer (or `.cancel`).
    func bind(_ continuation: CheckedContinuation<ShellAuthenticationResponse, Never>) {
        lock.lock()
        let alreadyCancelled = cancelled
        if !alreadyCancelled { self.continuation = continuation }
        lock.unlock()
        if alreadyCancelled { continuation.resume(returning: .cancel) }
    }

    /// Tracks the task running the handler so a cancel can stop it.
    func attach(_ task: Task<Void, Never>) {
        lock.lock()
        let finished = cancelled || continuation == nil
        if !finished { self.task = task }
        lock.unlock()
        if finished { task.cancel() }
    }

    func resume(_ response: ShellAuthenticationResponse) {
        lock.lock()
        let continuation = self.continuation
        self.continuation = nil
        task = nil
        lock.unlock()
        continuation?.resume(returning: response)
    }

    func cancel() {
        lock.lock()
        cancelled = true
        let continuation = self.continuation
        self.continuation = nil
        let task = self.task
        self.task = nil
        lock.unlock()
        task?.cancel()
        continuation?.resume(returning: .cancel)
    }
}

/// Thread-safe state container for `sendAndWait` to avoid mutating captured vars
/// in concurrently-executing closures.
private final class SendAndWaitState: @unchecked Sendable {
    private let lock = NSLock()
    private var _resumed = false
    private var _lastAssistantMessage: SessionEvent?

    /// Atomically attempts to mark as resumed. Returns true if this call was the one
    /// that flipped the flag (i.e., first caller wins).
    func tryResume() -> Bool {
        lock.lock()
        defer { lock.unlock() }
        if _resumed { return false }
        _resumed = true
        return true
    }

    func setLastAssistantMessage(_ event: SessionEvent) {
        lock.lock()
        defer { lock.unlock() }
        _lastAssistantMessage = event
    }

    func getLastAssistantMessage() -> SessionEvent? {
        lock.lock()
        defer { lock.unlock() }
        return _lastAssistantMessage
    }
}
