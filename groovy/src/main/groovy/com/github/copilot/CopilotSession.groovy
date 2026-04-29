/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot

import com.github.copilot.Types.*
import groovy.transform.CompileStatic

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.CountDownLatch
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
    private final String sessionId
    private final JsonRpcClient client
    private final String workspacePath
    private final CopyOnWriteArrayList<Closure> eventHandlers = new CopyOnWriteArrayList<>()
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<Closure>> typedEventHandlers = new ConcurrentHashMap<>()
    private final ConcurrentHashMap<String, Closure> toolHandlers = new ConcurrentHashMap<>()
    private volatile Closure permissionHandler
    private volatile Closure userInputHandler

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
     * @param options map with keys: prompt, attachments, mode, responseFormat, imageOptions
     * @return the message ID
     */
    @SuppressWarnings('unchecked')
    String send(Map<String, Object> options) throws Exception {
        Map<String, Object> params = [sessionId: sessionId] as Map<String, Object>
        params.prompt = options.prompt
        if (options.attachments) params.attachments = options.attachments
        if (options.mode) params.mode = options.mode
        if (options.responseFormat) params.responseFormat = options.responseFormat
        if (options.imageOptions) params.imageOptions = options.imageOptions

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

    /** Destroys this session and releases resources. */
    void destroy() throws Exception {
        client.request('session.destroy', [sessionId: sessionId])
        eventHandlers.clear()
        typedEventHandlers.clear()
        toolHandlers.clear()
        permissionHandler = null
    }

    /** Aborts the currently processing message. */
    void abort() throws Exception {
        client.request('session.abort', [sessionId: sessionId])
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
        SessionEvent event = new SessionEvent().with {
            type = (String) eventMap.type
            data = (Map<String, Object>) (eventMap.data ?: [:])
            it
        }
        dispatchEvent(event)
    }

    PermissionRequestResult handlePermissionRequest(Object request) throws Exception {
        if (permissionHandler == null) {
            return new PermissionRequestResult('denied-no-approval-rule-and-could-not-request-from-user')
        }
        PermissionRequest req = new PermissionRequest()
        if (request instanceof Map) {
            Map<String, Object> map = (Map<String, Object>) request
            req.kind = (String) map.kind
            req.toolCallId = (String) map.toolCallId
        }
        (PermissionRequestResult) permissionHandler.call(req, sessionId)
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
            it
        }
        (UserInputResponse) userInputHandler.call(req, sessionId)
    }
}
