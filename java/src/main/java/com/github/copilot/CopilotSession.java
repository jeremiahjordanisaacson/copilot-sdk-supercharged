/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.copilot.Types.*;
import com.github.copilot.internal.JsonRpcClient;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Represents a single conversation session with the Copilot CLI.
 *
 * <p>A session maintains conversation state, handles events, and manages tool execution.
 * Sessions are created via {@link CopilotClient#createSession} or resumed via
 * {@link CopilotClient#resumeSession}.
 *
 * <pre>{@code
 * CopilotSession session = client.createSession(new SessionConfig().model("gpt-4"));
 *
 * session.on(event -> {
 *     if ("assistant.message".equals(event.type)) {
 *         System.out.println(event.data.get("content"));
 *     }
 * });
 *
 * session.sendAndWait(new MessageOptions("Hello, world!"));
 * session.destroy();
 * }</pre>
 */
public class CopilotSession {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    /** Runs host-owned external tool handlers off the JSON-RPC reader thread. */
    private static final ExecutorService EXTERNAL_TOOL_EXECUTOR = Executors.newCachedThreadPool(r -> {
        Thread t = new Thread(r, "copilot-external-tool");
        t.setDaemon(true);
        return t;
    });

    private final String sessionId;
    private final JsonRpcClient client;
    private final String workspacePath;
    private final CopyOnWriteArrayList<SessionEventHandler> eventHandlers = new CopyOnWriteArrayList<>();
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<SessionEventHandler>> typedEventHandlers = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, ToolHandler> toolHandlers = new ConcurrentHashMap<>();
    /** In-flight external tool calls keyed by request ID, cancelled on {@code external_tool.completed}. */
    private final ConcurrentHashMap<String, PendingExternalTool> pendingExternalTools = new ConcurrentHashMap<>();
    private volatile PermissionHandler permissionHandler;
    private volatile UserInputHandler userInputHandler;
    private volatile SessionHooks hooks;
    private volatile ExitPlanModeHandler exitPlanModeHandler;
    private volatile Runnable onDestroyed;

    CopilotSession(String sessionId, JsonRpcClient client, String workspacePath) {
        this.sessionId = sessionId;
        this.client = client;
        this.workspacePath = workspacePath;
    }

    /** Gets the unique session identifier. */
    public String getSessionId() { return sessionId; }

    /** Gets the workspace path (for infinite sessions). */
    public String getWorkspacePath() { return workspacePath; }

    /**
     * Sends a message to this session.
     *
     * @param options the message options
     * @return the message ID
     */
    @SuppressWarnings("unchecked")
    public String send(MessageOptions options) throws Exception {
        Map<String, Object> params = CopilotClient.buildSendPayload(sessionId, options);
        Map<String, Object> response = client.request("session.send", params);
        return (String) response.get("messageId");
    }

    /**
     * Sends a message and waits until the session becomes idle.
     *
     * @param options the message options
     * @return the final assistant message event, or null
     */
    public SessionEvent sendAndWait(MessageOptions options) throws Exception {
        return sendAndWait(options, 60_000);
    }

    /**
     * Sends a message and waits until the session becomes idle with a custom timeout.
     *
     * @param options the message options
     * @param timeoutMs timeout in milliseconds
     * @return the final assistant message event, or null
     */
    public SessionEvent sendAndWait(MessageOptions options, long timeoutMs) throws Exception {
        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<SessionEvent> lastAssistantMessage = new AtomicReference<>();
        AtomicReference<Exception> sessionError = new AtomicReference<>();

        Runnable unsubscribe = on(event -> {
            if ("assistant.message".equals(event.type)) {
                lastAssistantMessage.set(event);
            } else if ("session.idle".equals(event.type)) {
                latch.countDown();
            } else if ("session.error".equals(event.type)) {
                String msg = event.data != null ? String.valueOf(event.data.get("message")) : "Unknown error";
                sessionError.set(new RuntimeException(msg));
                latch.countDown();
            }
        });

        try {
            send(options);
            if (!latch.await(timeoutMs, TimeUnit.MILLISECONDS)) {
                throw new RuntimeException("Timeout after " + timeoutMs + "ms waiting for session.idle");
            }
            if (sessionError.get() != null) {
                throw sessionError.get();
            }
            return lastAssistantMessage.get();
        } finally {
            unsubscribe.run();
        }
    }

    /**
     * Sends a prompt requesting structured output and returns the parsed JSON response
     * (default 60s timeout).
     *
     * @param prompt the message prompt
     * @param schema the JSON Schema the response must satisfy
     * @return the final assistant content parsed as JSON
     * @throws Exception if the request fails, times out, or no structured content is returned
     */
    public JsonNode sendAndWaitStructured(String prompt, Map<String, Object> schema) throws Exception {
        return sendAndWaitStructured(new MessageOptions(prompt), schema, 60_000);
    }

    /**
     * Sends a prompt requesting structured output and returns the parsed JSON response.
     *
     * @param prompt the message prompt
     * @param schema the JSON Schema the response must satisfy
     * @param timeoutMs timeout in milliseconds
     * @return the final assistant content parsed as JSON
     * @throws Exception if the request fails, times out, or no structured content is returned
     */
    public JsonNode sendAndWaitStructured(String prompt, Map<String, Object> schema, long timeoutMs) throws Exception {
        return sendAndWaitStructured(new MessageOptions(prompt), schema, timeoutMs);
    }

    /**
     * Sends a message requesting structured output and returns the parsed JSON response
     * (default 60s timeout).
     *
     * @param options the message options
     * @param schema the JSON Schema the response must satisfy
     * @return the final assistant content parsed as JSON
     * @throws Exception if the request fails, times out, or no structured content is returned
     */
    public JsonNode sendAndWaitStructured(MessageOptions options, Map<String, Object> schema) throws Exception {
        return sendAndWaitStructured(options, schema, 60_000);
    }

    /**
     * Sends a message with {@code responseSchema} set to {@code schema}, waits via
     * {@link #sendAndWait(MessageOptions, long)}, and returns the final assistant content parsed as JSON.
     * The caller's options are not modified.
     *
     * @param options the message options
     * @param schema the JSON Schema the response must satisfy
     * @param timeoutMs timeout in milliseconds
     * @return the final assistant content parsed as JSON
     * @throws Exception if the request fails, times out, or no structured content is returned
     */
    public JsonNode sendAndWaitStructured(MessageOptions options, Map<String, Object> schema, long timeoutMs) throws Exception {
        if (schema == null) throw new IllegalArgumentException("schema is required");
        SessionEvent response = sendAndWait(new MessageOptions(options).responseSchema(schema), timeoutMs);
        Object content = response != null && response.data != null ? response.data.get("content") : null;
        if (!(content instanceof String) || ((String) content).trim().isEmpty()) {
            throw new IllegalStateException("The requested run completed without a structured assistant response.");
        }
        return MAPPER.readTree((String) content);
    }

    /**
     * Subscribes to all events from this session.
     *
     * @param handler the event handler
     * @return a Runnable that unsubscribes the handler when called
     */
    public Runnable on(SessionEventHandler handler) {
        eventHandlers.add(handler);
        return () -> eventHandlers.remove(handler);
    }

    /**
     * Subscribes to a specific event type.
     *
     * @param eventType the event type (e.g., "assistant.message")
     * @param handler the event handler
     * @return a Runnable that unsubscribes the handler when called
     */
    public Runnable on(String eventType, SessionEventHandler handler) {
        typedEventHandlers.computeIfAbsent(eventType, k -> new CopyOnWriteArrayList<>()).add(handler);
        return () -> {
            CopyOnWriteArrayList<SessionEventHandler> handlers = typedEventHandlers.get(eventType);
            if (handlers != null) handlers.remove(handler);
        };
    }

    /**
     * Retrieves all events and messages from this session's history.
     */
    @SuppressWarnings("unchecked")
    public List<SessionEvent> getMessages() throws Exception {
        Map<String, Object> response = client.request("session.getMessages", Map.of("sessionId", sessionId));
        List<Map<String, Object>> events = (List<Map<String, Object>>) response.get("events");
        if (events == null) return List.of();

        List<SessionEvent> result = new ArrayList<>();
        for (Map<String, Object> eventMap : events) {
            SessionEvent event = new SessionEvent();
            event.type = (String) eventMap.get("type");
            event.data = eventMap;
            result.add(event);
        }
        return result;
    }

    /**
     * Retrieves metadata for this session.
     *
     * @return a map containing the session metadata
     */
    @SuppressWarnings("unchecked")
    public Map<String, Object> getMetadata() throws Exception {
        return client.request("session.getMetadata", Map.of("sessionId", sessionId));
    }

    /**
     * Destroys this session and releases resources. Sends {@code session.detach} (falling back to
     * {@code session.destroy} on older runtimes); on-disk session state is preserved for resume.
     */
    public void destroy() throws Exception {
        Map<String, Object> params = Map.of("sessionId", sessionId);
        try {
            client.request("session.detach", params);
        } catch (Exception e) {
            if (!JsonRpcClient.isMethodNotFound(e, "session.detach")) throw e;
            client.request("session.destroy", params);
        }
        cancelPendingExternalTools();
        eventHandlers.clear();
        typedEventHandlers.clear();
        toolHandlers.clear();
        permissionHandler = null;
        exitPlanModeHandler = null;
        Runnable callback = onDestroyed;
        onDestroyed = null;
        if (callback != null) callback.run();
    }

    /** Aborts the currently processing message. */
    public void abort() throws Exception {
        client.request("session.abort", Map.of("sessionId", sessionId));
    }

    /**
     * Changes the Auto routing preference without changing the selected model (experimental).
     * The runtime commits it on a later turn that uses the {@code auto} model.
     *
     * @param autoTier the routing preference (see {@link Types.AutoTier}), or null to return to default routing
     * @return the acknowledgement ({@code status}, {@code effectiveAutoTier}, {@code pendingAutoTier},
     *         {@code activatingAutoTier}, {@code supersededAutoTier})
     * @throws Exception if the request fails
     */
    public Map<String, Object> setAutoTier(String autoTier) throws Exception {
        Map<String, Object> params = new HashMap<>();
        params.put("sessionId", sessionId);
        params.put("autoTier", autoTier);
        return client.request("session.model.switchAutoTier", params);
    }

    /**
     * Returns the session's effective enterprise managed settings snapshot.
     *
     * @return the managed settings snapshot
     * @throws Exception if the request fails
     */
    public Map<String, Object> getManagedSettings() throws Exception {
        return client.request("session.managedSettings.get", Map.of("sessionId", sessionId));
    }

    /**
     * Runs a fresh Policy Helper evaluation and returns the resulting managed settings snapshot.
     *
     * @return the managed settings snapshot after the recheck settles
     * @throws Exception if the request fails
     */
    public Map<String, Object> recheckPolicyHelper() throws Exception {
        return client.request("session.managedSettings.recheckPolicyHelper", Map.of("sessionId", sessionId));
    }

    // --- Internal methods ---

    void registerTools(List<Tool> tools) {
        toolHandlers.clear();
        if (tools != null) {
            for (Tool tool : tools) {
                toolHandlers.put(tool.name, tool.handler);
            }
        }
    }

    void registerPermissionHandler(PermissionHandler handler) {
        this.permissionHandler = handler;
    }

    void registerUserInputHandler(UserInputHandler handler) {
        this.userInputHandler = handler;
    }

    void registerHooks(SessionHooks hooks) {
        this.hooks = hooks;
    }

    void registerExitPlanModeHandler(ExitPlanModeHandler handler) {
        this.exitPlanModeHandler = handler;
    }

    /** Registers a callback run once after this session is successfully destroyed. */
    void registerOnDestroyed(Runnable callback) {
        this.onDestroyed = callback;
    }

    ExitPlanModeResponse handleExitPlanModeRequest(Map<String, Object> params) throws Exception {
        if (exitPlanModeHandler == null) {
            return new ExitPlanModeResponse(true);
        }
        ExitPlanModeRequest req = new ExitPlanModeRequest();
        req.sessionId = (String) params.get("sessionId");
        req.summary = (String) params.get("summary");
        req.planContent = (String) params.get("planContent");
        @SuppressWarnings("unchecked")
        List<String> actions = (List<String>) params.get("actions");
        req.actions = actions;
        req.recommendedAction = (String) params.get("recommendedAction");
        return exitPlanModeHandler.handle(req);
    }

    ToolHandler getToolHandler(String name) {
        return toolHandlers.get(name);
    }

    void dispatchEvent(SessionEvent event) {
        // Typed handlers
        CopyOnWriteArrayList<SessionEventHandler> typed = typedEventHandlers.get(event.type);
        if (typed != null) {
            for (SessionEventHandler handler : typed) {
                try { handler.handle(event); } catch (Exception ignored) {}
            }
        }
        // Wildcard handlers
        for (SessionEventHandler handler : eventHandlers) {
            try { handler.handle(event); } catch (Exception ignored) {}
        }
    }

    @SuppressWarnings("unchecked")
    void dispatchEvent(Map<String, Object> eventMap) {
        SessionEvent event = new SessionEvent();
        event.type = (String) eventMap.get("type");
        event.data = (Map<String, Object>) eventMap.getOrDefault("data", Map.of());
        handleBroadcastEvent(event);
        dispatchEvent(event);
    }

    /**
     * Services host-owned external tool requests. {@code external_tool.requested} runs the registered
     * handler off the reader thread and answers via {@code session.tools.handlePendingToolCall};
     * {@code external_tool.completed} cancels the in-flight handler so no result is sent for it.
     */
    private void handleBroadcastEvent(SessionEvent event) {
        if (event.data == null) return;
        if ("external_tool.requested".equals(event.type)) {
            Object requestId = event.data.get("requestId");
            Object toolName = event.data.get("toolName");
            if (!(requestId instanceof String) || !(toolName instanceof String)) return;
            ToolHandler handler = toolHandlers.get((String) toolName);
            if (handler == null) return;
            Object toolCallId = event.data.get("toolCallId");
            startExternalTool((String) requestId, toolCallId instanceof String ? (String) toolCallId : null,
                (String) toolName, event.data.get("arguments"), handler);
        } else if ("external_tool.completed".equals(event.type)) {
            Object requestId = event.data.get("requestId");
            if (requestId instanceof String) cancelExternalTool((String) requestId);
        }
    }

    private void startExternalTool(String requestId, String toolCallId, String toolName, Object arguments,
                                   ToolHandler handler) {
        PendingExternalTool pending = new PendingExternalTool();
        if (pendingExternalTools.putIfAbsent(requestId, pending) != null) return;
        try {
            pending.future = EXTERNAL_TOOL_EXECUTOR.submit(
                () -> runExternalTool(requestId, toolCallId, toolName, arguments, handler, pending));
            // A completion that raced the submit must still interrupt the handler.
            if (pending.cancelled) pending.cancel();
        } catch (RejectedExecutionException e) {
            pendingExternalTools.remove(requestId, pending);
        }
    }

    private void runExternalTool(String requestId, String toolCallId, String toolName, Object arguments,
                                 ToolHandler handler, PendingExternalTool pending) {
        Map<String, Object> response = new HashMap<>();
        response.put("sessionId", sessionId);
        response.put("requestId", requestId);
        try {
            if (pending.cancelled) return;
            Object result = handler.handle(arguments, new ToolInvocation(sessionId, toolCallId, toolName, arguments));
            if (!claimExternalTool(requestId, pending)) return;
            response.put("result", toExternalToolResult(result));
        } catch (Exception e) {
            if (!claimExternalTool(requestId, pending)) return;
            response.put("error", e.getMessage() != null ? e.getMessage() : e.toString());
        } finally {
            pendingExternalTools.remove(requestId, pending);
        }
        try {
            client.request("session.tools.handlePendingToolCall", response);
        } catch (Exception ignored) {
            // Connection lost or RPC error: nothing more to report.
        }
    }

    /** Claims the right to answer a request; fails once the call was cancelled or completed elsewhere. */
    private boolean claimExternalTool(String requestId, PendingExternalTool pending) {
        return !pending.cancelled && pendingExternalTools.remove(requestId, pending);
    }

    private void cancelExternalTool(String requestId) {
        PendingExternalTool pending = pendingExternalTools.remove(requestId);
        if (pending != null) pending.cancel();
    }

    private void cancelPendingExternalTools() {
        for (String requestId : new ArrayList<>(pendingExternalTools.keySet())) {
            cancelExternalTool(requestId);
        }
    }

    @SuppressWarnings("unchecked")
    private static Object toExternalToolResult(Object result) {
        if (result == null) return "";
        if (result instanceof String) return result;
        if (result instanceof Map && ((Map<?, ?>) result).containsKey("textResultForLlm")) return result;
        if (result instanceof ToolResultObject) {
            Map<String, Object> wire = MAPPER.convertValue(result, Map.class);
            wire.values().removeIf(Objects::isNull);
            return wire;
        }
        try {
            return MAPPER.writeValueAsString(result);
        } catch (Exception e) {
            return String.valueOf(result);
        }
    }

    /** An in-flight external tool call that can be cancelled by request ID. */
    private static final class PendingExternalTool {
        volatile Future<?> future;
        volatile boolean cancelled;

        void cancel() {
            cancelled = true;
            Future<?> running = future;
            if (running != null) running.cancel(true);
        }
    }

    PermissionRequestResult handlePermissionRequest(Object request) throws Exception {
        if (permissionHandler == null) {
            return new PermissionRequestResult("denied-no-approval-rule-and-could-not-request-from-user");
        }
        PermissionRequest req = new PermissionRequest();
        if (request instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) request;
            req.kind = (String) map.get("kind");
            req.toolCallId = (String) map.get("toolCallId");
        }
        return permissionHandler.handle(req, sessionId);
    }

    UserInputResponse handleUserInputRequest(Map<String, Object> params) throws Exception {
        if (userInputHandler == null) {
            throw new RuntimeException("User input requested but no handler registered");
        }
        UserInputRequest req = new UserInputRequest();
        req.question = (String) params.get("question");
        req.choices = params.containsKey("choices") ? (List<String>) params.get("choices") : null;
        req.allowFreeform = (Boolean) params.get("allowFreeform");
        return userInputHandler.handle(req, sessionId);
    }

    @SuppressWarnings("unchecked")
    Map<String, Object> handleHooksInvoke(String hookType, Object input) throws Exception {
        if (hooks == null) return null;
        SessionHooks.HookHandler handler = hooks.getHandler(hookType);
        if (handler == null) return null;
        Map<String, Object> inputMap = input instanceof Map ? (Map<String, Object>) input : Map.of();
        return handler.handle(inputMap, sessionId);
    }
}
