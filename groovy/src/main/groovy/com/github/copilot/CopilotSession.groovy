/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import com.github.copilot.Types.*
import groovy.json.JsonOutput
import groovy.transform.CompileStatic

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Future
import java.util.concurrent.RejectedExecutionException
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

/**
 * Represents a single conversation session with the Copilot CLI.
 *
 * <p>A session maintains conversation state, handles events, and manages tool execution.
 * Sessions are created via {@link CopilotClient#createSession} or resumed via
 * {@link CopilotClient#resumeSession}.
 *
 * <pre>
 * def session = client.createSession(model: 'gpt-4')
 *
 * session.on { event ->
 *     if (event.type == 'assistant.message') {
 *         println event.data.content
 *     }
 * }
 *
 * session.sendAndWait('Hello, world!')
 * session.destroy()
 * </pre>
 */
@CompileStatic
class CopilotSession {
    private static final Set<String> SHELL_AUTH_INPUT_KINDS = Collections.unmodifiableSet(['secret', 'text', 'confirm'] as Set<String>)

    /**
     * Runs callbacks that answer the runtime with another RPC (external tools, permissions,
     * commands, elicitation, shell authentication) off the JSON-RPC reader thread.
     */
    private static final ExecutorService CALLBACK_EXECUTOR = Executors.newCachedThreadPool({ Runnable r ->
        Thread t = new Thread(r, 'copilot-session-callback')
        t.daemon = true
        t
    } as ThreadFactory)

    private final String sessionId
    private final JsonRpcClient client
    private final String workspacePath
    private final CopyOnWriteArrayList<Closure> eventHandlers = new CopyOnWriteArrayList<>()
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<Closure>> typedEventHandlers = new ConcurrentHashMap<>()
    private final ConcurrentHashMap<String, Closure> toolHandlers = new ConcurrentHashMap<>()
    private volatile Closure permissionHandler
    private volatile Closure userInputHandler
    private volatile Closure elicitationHandler
    private volatile Closure exitPlanModeHandler
    private volatile Closure shellAuthenticationHandler
    private volatile Closure memoryStoreHandler
    private volatile Map<String, Object> hooks
    private volatile Runnable onDestroyed
    private final ConcurrentHashMap<String, CommandDefinition> commandHandlers = new ConcurrentHashMap<>()
    /** In-flight shell authentication prompts keyed by request ID. */
    private final ConcurrentHashMap<String, PendingCallback> pendingShellAuthentications = new ConcurrentHashMap<>()
    /** In-flight external tool calls keyed by request ID, cancelled on {@code external_tool.completed}. */
    private final ConcurrentHashMap<String, PendingCallback> pendingExternalTools = new ConcurrentHashMap<>()

    CopilotSession(String sessionId, JsonRpcClient client, String workspacePath) {
        this.sessionId = sessionId
        this.client = client
        this.workspacePath = workspacePath
    }

    /** Gets the unique session identifier. */
    String getSessionId() { sessionId }

    /** Gets the workspace path (for infinite sessions). */
    String getWorkspacePath() { workspacePath }

    /**
     * Sends a message to this session.
     *
     * @param prompt the message text (convenience overload)
     * @return the message ID
     */
    String send(String prompt) throws Exception {
        send(prompt: prompt)
    }

    /**
     * Sends a message with full options.
     *
     * @param options map with keys: prompt, attachments, mode, imageOptions, requestHeaders,
     *                agentMode, displayPrompt, source ({@link MessageSource}), clientTurnId (opaque
     *                ID echoed on the turn's events), and responseSchema (a JSON schema map for
     *                structured output, sent as a {@code json_schema} response format)
     * @return the message ID
     */
    @SuppressWarnings('unchecked')
    String send(Map<String, Object> options) throws Exception {
        Map<String, Object> params = CopilotClient.buildSendPayload(sessionId, options)

        Map<String, Object> response = client.request('session.send', params)
        (String) response.messageId
    }

    /**
     * Sends a message and waits until the session becomes idle (default 60s timeout).
     *
     * @param prompt the message text
     * @return the final assistant message event, or null
     */
    SessionEvent sendAndWait(String prompt) throws Exception {
        sendAndWait(prompt: prompt)
    }

    /**
     * Sends a message with options and waits until the session becomes idle.
     *
     * @param options map with keys: prompt, attachments, mode, timeoutMs
     * @return the final assistant message event, or null
     */
    SessionEvent sendAndWait(Map<String, Object> options) throws Exception {
        long timeoutMs = (options.timeoutMs as Long) ?: 60_000L
        CountDownLatch latch = new CountDownLatch(1)
        AtomicReference<SessionEvent> lastMessage = new AtomicReference<>()
        AtomicReference<Exception> sessionError = new AtomicReference<>()

        Runnable unsubscribe = on { SessionEvent event ->
            switch (event.type) {
                case 'assistant.message':
                    lastMessage.set(event)
                    break
                case 'session.idle':
                    latch.countDown()
                    break
                case 'session.error':
                    String msg = event.data?.message?.toString() ?: 'Unknown error'
                    sessionError.set(new RuntimeException(msg))
                    latch.countDown()
                    break
            }
        }

        try {
            send(options)
            if (!latch.await(timeoutMs, TimeUnit.MILLISECONDS)) {
                throw new RuntimeException("Timeout after ${timeoutMs}ms waiting for session.idle")
            }
            if (sessionError.get() != null) {
                throw sessionError.get()
            }
            lastMessage.get()
        } finally {
            unsubscribe.run()
        }
    }

    /**
     * Subscribes to all events from this session.
     *
     * @param handler closure receiving (SessionEvent event)
     * @return a Runnable that unsubscribes the handler when called
     */
    Runnable on(Closure handler) {
        eventHandlers.add(handler)
        return { eventHandlers.remove(handler) } as Runnable
    }

    /**
     * Subscribes to a specific event type.
     *
     * @param eventType the event type (e.g., 'assistant.message')
     * @param handler closure receiving (SessionEvent event)
     * @return a Runnable that unsubscribes the handler when called
     */
    Runnable on(String eventType, Closure handler) {
        typedEventHandlers.computeIfAbsent(eventType) { new CopyOnWriteArrayList<Closure>() }.add(handler)
        return {
            CopyOnWriteArrayList<Closure> handlers = typedEventHandlers.get(eventType)
            handlers?.remove(handler)
        } as Runnable
    }

    /** Retrieves all events and messages from this session's history. */
    @SuppressWarnings('unchecked')
    List<SessionEvent> getMessages() throws Exception {
        Map<String, Object> response = client.request('session.getMessages', [sessionId: sessionId])
        List<Map<String, Object>> events = (List<Map<String, Object>>) response?.events
        if (!events) return []

        events.collect { Map<String, Object> eventMap ->
            new SessionEvent().with {
                type = (String) eventMap.type
                data = eventMap
                it
            }
        }
    }

    /** Retrieves metadata for this session. */
    Map<String, Object> getMetadata() throws Exception {
        client.request('session.getMetadata', [sessionId: sessionId])
    }

    /**
     * Destroys this session and releases resources. Sends {@code session.detach} (falling back to
     * {@code session.destroy} on older runtimes), so on-disk session state is kept for a later resume.
     */
    void destroy() throws Exception {
        Map<String, Object> params = [sessionId: sessionId] as Map<String, Object>
        try {
            Map<String, Object> result = client.request('session.detach', params)
            if (result?.success == false) {
                throw new RuntimeException("Failed to detach session ${sessionId}: ${result.error ?: 'Unknown error'}")
            }
        } catch (Exception e) {
            if (!JsonRpcClient.isMethodNotFound(e, 'session.detach')) throw e
            client.request('session.destroy', params)
        }
        cancelPending(pendingShellAuthentications)
        cancelPending(pendingExternalTools)
        eventHandlers.clear()
        typedEventHandlers.clear()
        toolHandlers.clear()
        commandHandlers.clear()
        permissionHandler = null
        elicitationHandler = null
        exitPlanModeHandler = null
        shellAuthenticationHandler = null
        memoryStoreHandler = null
        hooks = null
        Runnable callback = onDestroyed
        onDestroyed = null
        callback?.run()
    }

    /** Aborts the currently processing message. */
    void abort() throws Exception {
        client.request('session.abort', [sessionId: sessionId])
    }

    /**
     * Changes the Auto routing preference without changing the selected model (experimental).
     * Sends {@code session.model.switchAutoTier}; the runtime commits it on a later turn that uses
     * the {@code auto} model.
     *
     * @param autoTier the routing preference (see {@link AutoTier}), or null to return to default routing
     * @return the acknowledgement ({@code status}, {@code effectiveAutoTier}, {@code pendingAutoTier}, ...)
     */
    Map<String, Object> setAutoTier(String autoTier) throws Exception {
        client.request('session.model.switchAutoTier', [sessionId: sessionId, autoTier: autoTier] as Map<String, Object>)
    }

    /**
     * Returns the session's effective enterprise managed settings snapshot. Parse its
     * {@code policyHelperFailure} member with {@link PolicyHelperFailure#fromMap}.
     *
     * @return the managed settings snapshot
     */
    Map<String, Object> getManagedSettings() throws Exception {
        client.request('session.managedSettings.get', [sessionId: sessionId] as Map<String, Object>)
    }

    /**
     * Runs a fresh Policy Helper evaluation and returns the resulting managed settings snapshot.
     *
     * @return the managed settings snapshot after the recheck settles
     */
    Map<String, Object> recheckPolicyHelper() throws Exception {
        client.request('session.managedSettings.recheckPolicyHelper', [sessionId: sessionId] as Map<String, Object>)
    }

    /**
     * Changes the session's permission mode. A refused change reports {@code success == false} with
     * the authoritative mode and, when known, a {@code reason} such as
     * {@link PermissionModeRefusalReason#MANAGED_POLICY}.
     *
     * @param mode the permission mode (see {@link PermissionMode})
     * @return the result of the change
     */
    PermissionModeResult setPermissionMode(String mode) throws Exception {
        PermissionModeResult.fromMap(client.request('session.permissions.setMode',
            [sessionId: sessionId, mode: mode] as Map<String, Object>))
    }

    // --- Internal methods ---

    void registerTools(List<Tool> tools) {
        toolHandlers.clear()
        tools?.each { Tool tool ->
            toolHandlers.put(tool.name, tool.handler)
        }
    }

    void registerPermissionHandler(Closure handler) {
        this.permissionHandler = handler
    }

    void registerUserInputHandler(Closure handler) {
        this.userInputHandler = handler
    }

    void registerElicitationHandler(Closure handler) {
        this.elicitationHandler = handler
    }

    void registerExitPlanModeHandler(Closure handler) {
        this.exitPlanModeHandler = handler
    }

    void registerCommands(List<CommandDefinition> commands) {
        commandHandlers.clear()
        commands?.each { CommandDefinition cmd ->
            commandHandlers.put(cmd.name, cmd)
        }
    }

    /** Registers the hook closures ({@code onPreToolUse}, {@code onSubagentStart}, ...) served via {@code hooks.invoke}. */
    void registerHooks(Map<String, Object> hooks) {
        this.hooks = hooks
    }

    void registerShellAuthenticationHandler(Closure handler) {
        this.shellAuthenticationHandler = handler
    }

    void registerMemoryStore(Closure handler) {
        this.memoryStoreHandler = handler
    }

    /** Registers a callback run once after this session is successfully destroyed. */
    void registerOnDestroyed(Runnable callback) {
        this.onDestroyed = callback
    }

    /**
     * Invokes the hook closure for {@code hookType} (for example {@code subagentStart} runs
     * {@code onSubagentStart}). Closures receive {@code (input, sessionId)}.
     *
     * @return the hook output map, or null when no hook is registered or it returned no map
     */
    Object handleHooksInvoke(String hookType, Object input) throws Exception {
        Map<String, Object> registered = hooks
        if (registered == null || !hookType) return null
        Object handler = registered.get('on' + hookType.substring(0, 1).toUpperCase() + hookType.substring(1))
        if (!(handler instanceof Closure)) return null
        Closure closure = (Closure) handler
        Object output = closure.maximumNumberOfParameters >= 2 ? closure.call(input, sessionId) : closure.call(input)
        output instanceof Map ? output : null
    }

    /**
     * Answers a {@code shellAuthentication.request}: runs the handler off the request thread so a
     * {@code shellAuthentication.cancel} can release it, and answers {@code cancel} for invalid
     * requests, missing handlers, failures, and cancelled prompts. Never logs the answer.
     */
    @SuppressWarnings('unchecked')
    Map<String, Object> handleShellAuthenticationRequest(Map<String, Object> params) {
        Closure handler = shellAuthenticationHandler
        Object requestId = params.get('requestId')
        Object program = params.get('program')
        Object prompt = params.get('prompt')
        Object inputKind = params.get('inputKind')
        if (handler == null || !(requestId instanceof String) || !(program instanceof String)
                || !(prompt instanceof String) || !SHELL_AUTH_INPUT_KINDS.contains(inputKind)) {
            return shellAuthenticationCancel()
        }
        ShellAuthenticationRequest request = new ShellAuthenticationRequest()
        request.requestId = (String) requestId
        request.program = (String) program
        request.prompt = (String) prompt
        request.inputKind = (String) inputKind
        Object command = params.get('command')
        Object cwd = params.get('cwd')
        request.command = command instanceof String ? (String) command : ''
        request.cwd = cwd instanceof String ? (String) cwd : ''

        PendingCallback pending = new PendingCallback()
        if (pendingShellAuthentications.putIfAbsent(request.requestId, pending) != null) {
            return shellAuthenticationCancel()
        }
        try {
            pending.task = CALLBACK_EXECUTOR.submit({
                try {
                    Object reply = handler.maximumNumberOfParameters >= 2 ? handler.call(request, sessionId) : handler.call(request)
                    pending.result.complete(reply)
                } catch (Throwable t) {
                    pending.result.completeExceptionally(t)
                }
            } as Runnable)
            // A cancel that raced the submit must still interrupt the handler.
            if (pending.cancelled) pending.cancel()
            Object answer = pending.result.get()
            String action = null
            Object value = null
            if (answer instanceof ShellAuthenticationResult) {
                action = ((ShellAuthenticationResult) answer).action
                value = ((ShellAuthenticationResult) answer).value
            } else if (answer instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) answer
                action = map.get('action')?.toString()
                value = map.get('value')
            }
            if (action != 'accept' || value == null) return shellAuthenticationCancel()
            return [action: 'accept', value: value.toString()] as Map<String, Object>
        } catch (Exception ignored) {
            // Cancelled, interrupted, or the handler failed; the error could contain the answer, so it is not reported.
            return shellAuthenticationCancel()
        } finally {
            pendingShellAuthentications.remove(request.requestId, pending)
        }
    }

    /** Cancels a pending shell authentication prompt ({@code shellAuthentication.cancel}). */
    void cancelShellAuthentication(String requestId) {
        if (requestId == null) return
        PendingCallback pending = pendingShellAuthentications.remove(requestId)
        pending?.cancel()
    }

    private static Map<String, Object> shellAuthenticationCancel() {
        [action: 'cancel'] as Map<String, Object>
    }

    /** Invokes the registered memory store closure for a {@code memoryStore.store} request. */
    void handleMemoryStore(Map<String, Object> params) throws Exception {
        Closure handler = memoryStoreHandler
        if (handler == null) throw new IllegalStateException('No memory store is registered')
        StoredMemory memory = new StoredMemory()
        Object owner = params.get('sessionId')
        memory.sessionId = owner instanceof String ? (String) owner : sessionId
        memory.subject = stringOrEmpty(params.get('subject'))
        memory.fact = stringOrEmpty(params.get('fact'))
        memory.citations = stringOrEmpty(params.get('citations'))
        memory.reason = stringOrEmpty(params.get('reason'))
        try {
            handler.call(memory)
        } catch (Exception e) {
            throw new RuntimeException('Memory store callback failed', e)
        }
    }

    private static String stringOrEmpty(Object value) {
        value instanceof String ? (String) value : ''
    }

    private static void cancelPending(ConcurrentHashMap<String, PendingCallback> pending) {
        for (String requestId : new ArrayList<String>(pending.keySet())) {
            PendingCallback callback = pending.remove(requestId)
            callback?.cancel()
        }
    }

    /** An in-flight callback that a cancel or completion notice can interrupt. */
    private static final class PendingCallback {
        public final CompletableFuture<Object> result = new CompletableFuture<Object>()
        public volatile Future<?> task
        public volatile boolean cancelled

        void cancel() {
            cancelled = true
            result.cancel(true)
            Future<?> running = task
            if (running != null) running.cancel(true)
        }
    }

    Closure getToolHandler(String name) {
        toolHandlers.get(name)
    }

    void dispatchEvent(SessionEvent event) {
        // Typed handlers
        CopyOnWriteArrayList<Closure> typed = typedEventHandlers.get(event.type)
        typed?.each { Closure handler ->
            try { handler.call(event) } catch (Exception ignored) {}
        }
        // Wildcard handlers
        eventHandlers.each { Closure handler ->
            try { handler.call(event) } catch (Exception ignored) {}
        }
    }

    @SuppressWarnings('unchecked')
    void dispatchEvent(Map<String, Object> eventMap) {
        String eventType = (String) eventMap.type
        Map<String, Object> eventData = (Map<String, Object>) (eventMap.data ?: [:])

        // Handle command.execute events (off the reader thread: the answer is another RPC)
        if (eventType == 'command.execute') {
            String requestId = (String) eventData.requestId
            String commandName = (String) eventData.commandName
            String command = (String) eventData.command
            String args = (String) eventData.args
            submitCallback { handleCommandExecute(requestId, commandName, command, args) }
        }

        // Handle elicitation.requested events
        if (eventType == 'elicitation.requested' && elicitationHandler) {
            String requestId = (String) eventData.requestId
            String message = (String) eventData.message
            Object requestedSchema = eventData.requestedSchema
            String mode = (String) eventData.mode
            String elicitationSource = (String) eventData.elicitationSource
            String url = (String) eventData.url
            Map<String, Object> meta = eventData._meta instanceof Map ? (Map<String, Object>) eventData._meta : null
            submitCallback { handleElicitationRequest(requestId, message, requestedSchema, mode, elicitationSource, url, meta) }
        }

        handleBroadcastEvent(eventType, eventData)

        SessionEvent event = new SessionEvent().with {
            type = eventType
            data = eventData
            it
        }
        dispatchEvent(event)
    }

    private static void submitCallback(Closure work) {
        try {
            CALLBACK_EXECUTOR.submit(work as Runnable)
        } catch (RejectedExecutionException ignored) {
            // The executor only rejects work while the JVM shuts down.
        }
    }

    /**
     * Services the protocol 4 broadcast requests: {@code external_tool.requested} runs the
     * registered tool handler and answers via {@code session.tools.handlePendingToolCall}
     * ({@code external_tool.completed} cancels it), and {@code permission.requested} runs the
     * permission handler and answers via {@code session.permissions.handlePendingPermissionRequest}.
     */
    private void handleBroadcastEvent(String eventType, Map<String, Object> data) {
        if (eventType == 'external_tool.requested') {
            Object requestId = data.get('requestId')
            Object toolName = data.get('toolName')
            if (!(requestId instanceof String) || !(toolName instanceof String) || data.get('hasHandler') == false) return
            Closure handler = toolHandlers.get((String) toolName)
            if (handler == null) return
            Object toolCallId = data.get('toolCallId')
            startExternalTool((String) requestId, toolCallId instanceof String ? (String) toolCallId : null,
                (String) toolName, data.get('arguments'), handler)
        } else if (eventType == 'external_tool.completed') {
            Object requestId = data.get('requestId')
            if (requestId instanceof String) {
                PendingCallback pending = pendingExternalTools.remove((String) requestId)
                pending?.cancel()
            }
        } else if (eventType == 'permission.requested') {
            Object requestId = data.get('requestId')
            if (!(requestId instanceof String) || data.get('resolvedByHook') == true || permissionHandler == null) return
            String id = (String) requestId
            Object permissionRequest = data.get('permissionRequest')
            submitCallback { respondToPermissionRequest(id, permissionRequest) }
        }
    }

    private void startExternalTool(String requestId, String toolCallId, String toolName, Object arguments, Closure handler) {
        PendingCallback pending = new PendingCallback()
        if (pendingExternalTools.putIfAbsent(requestId, pending) != null) return
        try {
            pending.task = CALLBACK_EXECUTOR.submit({
                runExternalTool(requestId, toolCallId, toolName, arguments, handler, pending)
            } as Runnable)
            // A completion that raced the submit must still interrupt the handler.
            if (pending.cancelled) pending.cancel()
        } catch (RejectedExecutionException ignored) {
            pendingExternalTools.remove(requestId, pending)
        }
    }

    private void runExternalTool(String requestId, String toolCallId, String toolName, Object arguments,
                                 Closure handler, PendingCallback pending) {
        Map<String, Object> response = [sessionId: sessionId, requestId: requestId] as Map<String, Object>
        try {
            if (pending.cancelled) return
            ToolInvocation invocation = new ToolInvocation(sessionId, toolCallId, toolName, arguments)
            Object result = handler.maximumNumberOfParameters >= 2 ? handler.call(arguments, invocation) : handler.call(arguments)
            if (!claimExternalTool(requestId, pending)) return
            response.result = toExternalToolResult(result)
        } catch (Exception e) {
            if (!claimExternalTool(requestId, pending)) return
            response.error = e.message ?: e.toString()
        } finally {
            pendingExternalTools.remove(requestId, pending)
        }
        try {
            client.request('session.tools.handlePendingToolCall', response)
        } catch (Exception ignored) {
            // Connection lost or RPC error: nothing more to report.
        }
    }

    /** Claims the right to answer a request; fails once the call was cancelled or completed elsewhere. */
    private boolean claimExternalTool(String requestId, PendingCallback pending) {
        !pending.cancelled && pendingExternalTools.remove(requestId, pending)
    }

    @SuppressWarnings('unchecked')
    private static Object toExternalToolResult(Object result) {
        if (result == null) return ''
        if (result instanceof CharSequence) return result.toString()
        if (result instanceof Map && ((Map) result).containsKey('textResultForLlm')) return result
        if (result instanceof ToolResultObject) {
            ToolResultObject r = (ToolResultObject) result
            Map<String, Object> wire = [textResultForLlm: r.textResultForLlm ?: '', resultType: r.resultType ?: 'success'] as Map<String, Object>
            if (r.error != null) wire.error = r.error
            if (r.sessionLog != null) wire.sessionLog = r.sessionLog
            if (r.toolTelemetry) wire.toolTelemetry = r.toolTelemetry
            return wire
        }
        JsonOutput.toJson(result)
    }

    private void respondToPermissionRequest(String requestId, Object permissionRequest) {
        Map<String, Object> response = [sessionId: sessionId, requestId: requestId] as Map<String, Object>
        try {
            PermissionRequestResult result = handlePermissionRequest(permissionRequest)
            if (result == null || result.kind == 'no-result') return
            Map<String, Object> decision = [kind: result.kind] as Map<String, Object>
            if (result.rules != null) decision.rules = result.rules
            response.result = decision
            if (result.decisionContext != null) response.decisionContext = result.decisionContext
        } catch (Exception ignored) {
            response.result = [kind: 'user-not-available'] as Map<String, Object>
        }
        try {
            client.request('session.permissions.handlePendingPermissionRequest', response)
        } catch (Exception ignored) {
            // Connection lost or RPC error: nothing more to report.
        }
    }

    private void handleCommandExecute(String requestId, String commandName, String command, String args) {
        CommandDefinition cmd = commandHandlers.get(commandName)
        if (!cmd || !cmd.handler) return

        try {
            CommandContext ctx = new CommandContext().with {
                it.sessionId = this.sessionId
                it.command = command
                it.commandName = commandName
                it.args = args
                it
            }
            cmd.handler.call(ctx)
            client.request('session.commands.handlePendingCommand', [
                sessionId: sessionId,
                requestId: requestId
            ])
        } catch (Exception e) {
            try {
                client.request('session.commands.handlePendingCommand', [
                    sessionId: sessionId,
                    requestId: requestId,
                    error    : e.message ?: e.toString()
                ])
            } catch (Exception ignored) {}
        }
    }

    private void handleElicitationRequest(String requestId, String message, Object requestedSchema,
                                          String mode, String elicitationSource, String url,
                                          Map<String, Object> meta) {
        if (!elicitationHandler) return

        try {
            ElicitationContext ctx = new ElicitationContext().with {
                it.sessionId = this.sessionId
                it.message = message
                it.requestedSchema = requestedSchema
                it.mode = mode
                it.elicitationSource = elicitationSource
                it.url = url
                it.requestId = requestId
                it.meta = meta
                it
            }
            Object result = elicitationHandler.call(ctx)
            Map<String, Object> resultMap
            if (result instanceof ElicitationResult) {
                ElicitationResult er = (ElicitationResult) result
                resultMap = [action: er.action] as Map<String, Object>
                if (er.content) resultMap.content = er.content
                if (er.meta != null) resultMap._meta = er.meta
            } else if (result instanceof Map) {
                resultMap = (Map<String, Object>) result
            } else {
                resultMap = [action: 'cancel'] as Map<String, Object>
            }
            client.request('session.ui.handlePendingElicitation', [
                sessionId: sessionId,
                requestId: requestId,
                result   : resultMap
            ])
        } catch (Exception e) {
            try {
                client.request('session.ui.handlePendingElicitation', [
                    sessionId: sessionId,
                    requestId: requestId,
                    result   : [action: 'cancel']
                ])
            } catch (Exception ignored) {}
        }
    }

    @SuppressWarnings('unchecked')
    PermissionRequestResult handlePermissionRequest(Object request) throws Exception {
        if (permissionHandler == null) {
            return new PermissionRequestResult('denied-no-approval-rule-and-could-not-request-from-user')
        }
        PermissionRequest req = new PermissionRequest()
        if (request instanceof Map) {
            Map<String, Object> map = (Map<String, Object>) request
            req.kind = (String) map.kind
            req.toolCallId = (String) map.toolCallId
            req.details = map
        }
        Object result = permissionHandler.call(req, sessionId)
        if (result instanceof Map) {
            Map<String, Object> map = (Map<String, Object>) result
            PermissionRequestResult converted = new PermissionRequestResult(map.get('kind') as String)
            converted.rules = (List<Object>) map.get('rules')
            converted.decisionContext = (Map<String, Object>) map.get('decisionContext')
            return converted
        }
        if (result instanceof CharSequence) return new PermissionRequestResult(result.toString())
        (PermissionRequestResult) result
    }

    ExitPlanModeResponse handleExitPlanModeRequest(Map<String, Object> params) throws Exception {
        if (exitPlanModeHandler == null) {
            return new ExitPlanModeResponse(true)
        }
        ExitPlanModeRequest req = new ExitPlanModeRequest()
        req.sessionId = (String) params.get('sessionId')
        (ExitPlanModeResponse) exitPlanModeHandler.call(req)
    }

    @SuppressWarnings('unchecked')
    UserInputResponse handleUserInputRequest(Map<String, Object> params) throws Exception {
        if (userInputHandler == null) {
            throw new RuntimeException('User input requested but no handler registered')
        }
        UserInputRequest req = new UserInputRequest().with {
            question = (String) params.question
            choices = params.containsKey('choices') ? (List<String>) params.choices : null
            allowFreeform = (Boolean) params.allowFreeform
            requestId = params.requestId instanceof String ? (String) params.requestId : null
            imageAttachmentConstraints = UserInputImageAttachmentConstraints.fromMap(params.imageAttachmentConstraints)
            meta = params._meta instanceof Map ? (Map<String, Object>) params._meta : null
            it
        }
        (UserInputResponse) userInputHandler.call(req, sessionId)
    }
}
