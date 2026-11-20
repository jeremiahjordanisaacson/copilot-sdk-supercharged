/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.rpc.BearerTokenProvider;
import com.microsoft.copilot.rpc.ExitPlanModeRequest;
import com.microsoft.copilot.rpc.ExitPlanModeResult;
import com.microsoft.copilot.rpc.PermissionRequestResult;
import com.microsoft.copilot.rpc.PermissionRequestResultKind;
import com.microsoft.copilot.rpc.PreToolUseHookOutput;
import com.microsoft.copilot.rpc.SessionHooks;
import com.microsoft.copilot.rpc.SessionLifecycleEvent;
import com.microsoft.copilot.rpc.StoredMemory;
import com.microsoft.copilot.rpc.ToolDefinition;
import com.microsoft.copilot.rpc.ToolResultObject;
import com.microsoft.copilot.rpc.UserInputRequest;
import com.microsoft.copilot.rpc.UserInputResponse;
import com.microsoft.copilot.generated.rpc.GitHubTokenAcquireReason;
import com.microsoft.copilot.generated.rpc.PermissionDecisionContext;
import com.microsoft.copilot.generated.rpc.PermissionDecisionOutcome;
import com.microsoft.copilot.generated.rpc.PermissionDecisionSource;
import com.microsoft.copilot.generated.rpc.PermissionDecisionSurface;
import com.microsoft.copilot.generated.rpc.PermissionResponseCapability;
import com.microsoft.copilot.rpc.GitHubTokenProviderArgs;
import com.microsoft.copilot.rpc.GitHubTokenProviderResult;

/**
 * Unit tests for {@link RpcHandlerDispatcher} focusing on coverage gaps
 * identified by JaCoCo: unknown sessions, missing fields, error paths, and edge
 * cases for each handler method.
 */
@AllowCopilotExperimental
class RpcHandlerDispatcherTest {

    private static final ObjectMapper MAPPER = JsonRpcClient.getObjectMapper();
    private static final int SOCKET_TIMEOUT_MS = 5000;
    private static final String PERSON_ANSWERED_JSON = "{\"source\":\"human_response\","
            + "\"responseCapability\":\"interactive\",\"surface\":\"sdk\",\"outcome\":\"prompted_user\"}";

    private Socket clientSideSocket;
    private Socket serverSideSocket;
    private JsonRpcClient rpc;
    private Map<String, CopilotSession> sessions;
    private CopyOnWriteArrayList<SessionLifecycleEvent> lifecycleEvents;
    private RpcHandlerDispatcher dispatcher;
    private InputStream responseStream;
    private Map<String, BiConsumer<String, JsonNode>> handlers;
    private GitHubTokenProviderRegistry gitHubTokenProviders;

    @BeforeEach
    void setup() throws Exception {
        // Create a socket pair for the JsonRpcClient
        var loopback = InetAddress.getLoopbackAddress();
        try (ServerSocket ss = new ServerSocket(0, 1, loopback)) {
            clientSideSocket = new Socket(loopback, ss.getLocalPort());
            serverSideSocket = ss.accept();
        }
        serverSideSocket.setSoTimeout(SOCKET_TIMEOUT_MS);

        rpc = JsonRpcClient.fromSocket(clientSideSocket);
        responseStream = serverSideSocket.getInputStream();

        sessions = new ConcurrentHashMap<>();
        lifecycleEvents = new CopyOnWriteArrayList<>();
        gitHubTokenProviders = new GitHubTokenProviderRegistry();
        dispatcher = new RpcHandlerDispatcher(sessions, lifecycleEvents::add, null, gitHubTokenProviders,
                sessions.values());
        dispatcher.registerHandlers(rpc);

        // Extract the registered handlers via reflection so we can invoke them directly
        Field f = JsonRpcClient.class.getDeclaredField("notificationHandlers");
        f.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, BiConsumer<String, JsonNode>> h = (Map<String, BiConsumer<String, JsonNode>>) f.get(rpc);
        handlers = h;
    }

    @AfterEach
    void teardown() throws Exception {
        if (rpc != null) {
            rpc.close();
        }
        if (serverSideSocket != null) {
            serverSideSocket.close();
        }
        if (clientSideSocket != null) {
            clientSideSocket.close();
        }
    }

    /** Invoke a registered RPC handler directly. */
    private void invokeHandler(String method, String requestId, JsonNode params) {
        handlers.get(method).accept(requestId, params);
    }

    /** Read a single JSON-RPC response message from the server-side socket. */
    private JsonNode readResponse() throws Exception {
        StringBuilder header = new StringBuilder();
        while (!header.toString().endsWith("\r\n\r\n")) {
            int b = responseStream.read();
            if (b == -1) {
                throw new java.io.IOException("Unexpected end of stream");
            }
            header.append((char) b);
        }
        String headerStr = header.toString().trim();
        int idx = headerStr.indexOf(':');
        int contentLength = Integer.parseInt(headerStr.substring(idx + 1).trim());
        byte[] body = responseStream.readNBytes(contentLength);
        return MAPPER.readTree(body);
    }

    /** Write a JSON-RPC message to the client, as the server would. */
    private void sendToClient(JsonNode message) throws Exception {
        byte[] content = MAPPER.writeValueAsBytes(message);
        var out = serverSideSocket.getOutputStream();
        out.write(("Content-Length: " + content.length + "\r\n\r\n").getBytes(StandardCharsets.US_ASCII));
        out.write(content);
        out.flush();
    }

    /**
     * Send a session.idle event for the session over the connection and return its
     * id.
     */
    private String sendSessionEvent(String sessionId) throws Exception {
        String id = UUID.randomUUID().toString();
        ObjectNode notification = MAPPER.createObjectNode();
        notification.put("jsonrpc", "2.0");
        notification.put("method", "session.event");
        ObjectNode params = notification.putObject("params");
        params.put("sessionId", sessionId);
        ObjectNode event = params.putObject("event");
        event.put("type", "session.idle");
        event.put("id", id);
        event.put("timestamp", "2026-01-01T00:00:00Z");
        event.putObject("data");
        sendToClient(notification);
        return id;
    }

    /** Answer the next request the client sends with a null result. */
    private void answerNextRequest() throws Exception {
        JsonNode request = readResponse();
        ObjectNode response = MAPPER.createObjectNode();
        response.put("jsonrpc", "2.0");
        response.set("id", request.get("id"));
        response.putNull("result");
        sendToClient(response);
    }

    private static void awaitQuietly(CountDownLatch latch) {
        try {
            latch.await(10, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private static void waitUntil(Supplier<Boolean> condition, String message) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
        while (!condition.get()) {
            if (System.nanoTime() > deadline) {
                fail(message);
            }
            Thread.sleep(10);
        }
    }

    /** Create and register a CopilotSession in the sessions map. */
    private CopilotSession createSession(String sessionId) {
        CopilotSession session = new CopilotSession(sessionId, rpc);
        sessions.put(sessionId, session);
        return session;
    }

    @Test
    void gitHubTokenCallbackMapsRequestAndTokenResult() throws Exception {
        AtomicReference<GitHubTokenProviderArgs> received = new AtomicReference<>();
        var registration = gitHubTokenProviders.register(args -> {
            received.set(args);
            return CompletableFuture.completedFuture(GitHubTokenProviderResult.token("secret", 28_800, "bearer"));
        });
        ObjectNode params = MAPPER.createObjectNode();
        params.put("registrationId", registration.id());
        params.put("host", "github.example");
        params.put("sessionId", "session-1");
        params.put("reason", "initial");

        invokeHandler("gitHubToken.getToken", "80", params);

        JsonNode response = readResponse();
        assertEquals("token", response.at("/result/kind").asText());
        assertEquals("secret", response.at("/result/accessToken").asText());
        assertEquals(28_800, response.at("/result/expiresIn").asLong());
        assertEquals("github.example", received.get().host());
        assertEquals("session-1", received.get().sessionId());
        assertEquals(GitHubTokenAcquireReason.INITIAL, received.get().reason());
    }

    @Test
    void gitHubTokenCallbackPreservesCancellationAndErrors() throws Exception {
        var cancelled = gitHubTokenProviders
                .register(args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled()));
        ObjectNode cancelledParams = MAPPER.createObjectNode();
        cancelledParams.put("registrationId", cancelled.id());
        cancelledParams.put("host", "github.com");
        cancelledParams.put("reason", "refresh");
        invokeHandler("gitHubToken.getToken", "81", cancelledParams);
        assertEquals("cancelled", readResponse().at("/result/kind").asText());

        var failed = gitHubTokenProviders.register(
                args -> CompletableFuture.failedFuture(new IllegalStateException("credential service unavailable")));
        ObjectNode failedParams = cancelledParams.deepCopy();
        failedParams.put("registrationId", failed.id());
        invokeHandler("gitHubToken.getToken", "82", failedParams);
        JsonNode error = readResponse();
        assertEquals(-32603, error.at("/error/code").asInt());
        assertTrue(error.at("/error/message").asText().contains("credential service unavailable"));

        failedParams.put("registrationId", "unknown");
        invokeHandler("gitHubToken.getToken", "83", failedParams);
        JsonNode unknown = readResponse();
        assertEquals(-32603, unknown.at("/error/code").asInt());
        assertTrue(unknown.at("/error/message").asText().contains("Unknown GitHub token provider registration"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"throws-error", "throws-exception", "fails-with-error"})
    void gitHubTokenCallbackAnswersAFailedProvider(String failure) throws Exception {
        var registration = gitHubTokenProviders.register(args -> failedTokenProvider(failure, "gh-token-boom"));
        ObjectNode params = MAPPER.createObjectNode();
        params.put("registrationId", registration.id());
        params.put("host", "github.com");
        params.put("reason", "initial");

        invokeHandler("gitHubToken.getToken", "84", params);

        JsonNode error = readResponse();
        assertEquals(-32603, error.at("/error/code").asInt(), error.toString());
        assertTrue(error.at("/error/message").asText().contains("gh-token-boom"), error.toString());
    }

    @ParameterizedTest
    @ValueSource(strings = {"returns-token", "throws-error", "throws-exception", "fails-with-error"})
    void providerTokenCallbackAnswersEveryProviderOutcome(String outcome) throws Exception {
        BearerTokenProvider provider = args -> "returns-token".equals(outcome)
                ? CompletableFuture.completedFuture("tok-ok")
                : failedTokenProvider(outcome, "bearer-boom");
        createSession("s1").registerBearerTokenProviders(Map.of("mi", provider));
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("providerName", "mi");

        invokeHandler("providerToken.getToken", "85", params);

        JsonNode response = readResponse();
        if ("returns-token".equals(outcome)) {
            assertEquals("tok-ok", response.at("/result/token").asText(), response.toString());
        } else {
            assertEquals(-32603, response.at("/error/code").asInt(), response.toString());
            assertTrue(response.at("/error/message").asText().contains("bearer-boom"), response.toString());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"gitHubToken.getToken", "providerToken.getToken"})
    void tokenCallbacksRethrowAFatalProviderError(String method) throws Exception {
        // A fatal error is rethrown, as on every other callback path, not answered.
        var called = new CountDownLatch(1);
        ObjectNode params = MAPPER.createObjectNode();
        if (method.equals("gitHubToken.getToken")) {
            var registration = gitHubTokenProviders.register(args -> {
                called.countDown();
                return failedTokenProvider("throws-fatal-error", "gh-token-fatal");
            });
            params.put("registrationId", registration.id());
            params.put("host", "github.com");
            params.put("reason", "initial");
        } else {
            BearerTokenProvider provider = args -> {
                called.countDown();
                return failedTokenProvider("throws-fatal-error", "bearer-fatal");
            };
            createSession("s1").registerBearerTokenProviders(Map.of("mi", provider));
            params.put("sessionId", "s1");
            params.put("providerName", "mi");
        }

        invokeHandler(method, "86", params);

        assertTrue(called.await(SOCKET_TIMEOUT_MS, TimeUnit.MILLISECONDS), "The provider must be called");
        serverSideSocket.setSoTimeout(500);
        assertThrows(java.net.SocketTimeoutException.class, this::readResponse);
    }

    /**
     * A token provider that fails before returning its future, or returns a failed
     * one.
     */
    private static <T> CompletableFuture<T> failedTokenProvider(String failure, String message) {
        return switch (failure) {
            case "throws-error" -> throw new AssertionError(message);
            case "throws-fatal-error" -> throw new LinkageError(message);
            case "throws-exception" -> throw new IllegalStateException(message);
            case "fails-with-error" -> CompletableFuture.failedFuture(new AssertionError(message));
            default -> throw new IllegalArgumentException(failure);
        };
    }

    // ===== session.event tests =====

    @Test
    void sessionEventWithNullEventNode() throws Exception {
        CopilotSession session = createSession("s1");
        var dispatched = new CopyOnWriteArrayList<>();
        session.on(dispatched::add);

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        // "event" field is absent → eventNode is null

        invokeHandler("session.event", null, params);

        // Give queued event delivery a moment to run
        Thread.sleep(50);
        assertTrue(dispatched.isEmpty(), "No events should be dispatched when eventNode is null");
    }

    @Test
    void sessionEventWithUnknownSession() {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "unknown");
        ObjectNode event = params.putObject("event");
        event.put("type", "assistantMessage");
        event.putObject("data").put("content", "hello");

        // Should not throw — silently skips when session is not found
        assertDoesNotThrow(() -> invokeHandler("session.event", null, params));
    }

    @Test
    void busySessionEventHandlerLeavesTheConnectionAndOtherSessionsWorking() throws Exception {
        CopilotSession busy = createSession("s1");
        CopilotSession other = createSession("s2");
        // Client sessions always deliver on the client's executor. Without one,
        // delivery falls back to the common pool, which can be one worker wide.
        ExecutorService deliveries = Executors.newCachedThreadPool();
        busy.setExecutor(deliveries);
        other.setExecutor(deliveries);
        var handlerRunning = new CountDownLatch(1);
        var releaseHandler = new CountDownLatch(1);
        busy.on(event -> {
            handlerRunning.countDown();
            awaitQuietly(releaseHandler);
        });
        var otherReceived = new CountDownLatch(1);
        other.on(event -> otherReceived.countDown());
        try {
            sendSessionEvent("s1");
            assertTrue(handlerRunning.await(5, TimeUnit.SECONDS), "s1 handler should receive its event");

            CompletableFuture<JsonNode> call = rpc.invoke("session.abort", Map.of("sessionId", "s2"), JsonNode.class);
            answerNextRequest();
            assertDoesNotThrow(() -> call.get(5, TimeUnit.SECONDS),
                    "A response must be read while another session's handler is busy");

            sendSessionEvent("s2");
            assertTrue(otherReceived.await(5, TimeUnit.SECONDS),
                    "Another session's events must be delivered while s1's handler is busy");
        } finally {
            releaseHandler.countDown();
            deliveries.shutdown();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"default", "inline", "rejected"})
    void sessionEventHandlerCanWaitForAnRpcResponse(String executorKind) throws Exception {
        CopilotSession session = createSession("s1");
        if (executorKind.equals("inline")) {
            session.setExecutor(Runnable::run);
        } else if (executorKind.equals("rejected")) {
            session.setExecutor(task -> {
                throw new java.util.concurrent.RejectedExecutionException("Controlled executor rejection");
            });
        }
        var outcome = new CompletableFuture<JsonNode>();
        session.on(event -> {
            try {
                outcome.complete(rpc.invoke("session.abort", Map.of("sessionId", "s1"), JsonNode.class).get(5,
                        TimeUnit.SECONDS));
            } catch (Exception e) {
                outcome.completeExceptionally(e);
            }
        });

        sendSessionEvent("s1");
        answerNextRequest();

        assertDoesNotThrow(() -> outcome.get(10, TimeUnit.SECONDS),
                "A handler waiting for an RPC response must receive it");
    }

    @Test
    void queuedSessionEventsStayOnTheConfiguredExecutor() throws Exception {
        CopilotSession session = createSession("s1");
        var executorThread = new AtomicReference<Thread>();
        ExecutorService deliveries = Executors.newSingleThreadExecutor(task -> {
            Thread thread = new Thread(task, "session-event-test");
            executorThread.set(thread);
            return thread;
        });
        session.setExecutor(deliveries);
        var firstRunning = new CountDownLatch(1);
        var releaseFirst = new CountDownLatch(1);
        var received = new CopyOnWriteArrayList<Thread>();
        var secondDelivered = new CompletableFuture<Void>();
        session.on(event -> {
            received.add(Thread.currentThread());
            if (received.size() == 1) {
                firstRunning.countDown();
                try {
                    releaseFirst.await();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    secondDelivered.completeExceptionally(e);
                }
            } else {
                secondDelivered.complete(null);
            }
        });
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        ObjectNode event = params.putObject("event");
        event.put("type", "session.idle");
        event.put("id", UUID.randomUUID().toString());
        event.put("timestamp", "2026-01-01T00:00:00Z");
        event.putObject("data");
        try {
            invokeHandler("session.event", null, params);
            assertTrue(firstRunning.await(5, TimeUnit.SECONDS), "The first handler must be running");
            invokeHandler("session.event", null, params);
            releaseFirst.countDown();
            secondDelivered.get(5, TimeUnit.SECONDS);
            assertEquals(List.of(executorThread.get(), executorThread.get()), received,
                    "Queued handlers must retain the configured executor's thread");
        } finally {
            releaseFirst.countDown();
            deliveries.shutdown();
            assertTrue(deliveries.awaitTermination(5, TimeUnit.SECONDS), "The event executor must stop");
        }
    }

    @Test
    void eventsQueuedBehindABusyHandlerArriveInOrderOneAtATime() throws Exception {
        CopilotSession session = createSession("s1");
        var received = new CopyOnWriteArrayList<String>();
        var active = new AtomicInteger();
        var overlapped = new AtomicBoolean();
        var firstRunning = new CountDownLatch(1);
        var releaseFirst = new CountDownLatch(1);
        session.on(event -> {
            if (active.incrementAndGet() > 1) {
                overlapped.set(true);
            }
            try {
                received.add(event.getId().toString());
                if (received.size() == 1) {
                    firstRunning.countDown();
                    awaitQuietly(releaseFirst);
                }
            } finally {
                active.decrementAndGet();
            }
        });

        var sent = new ArrayList<String>();
        sent.add(sendSessionEvent("s1"));
        assertTrue(firstRunning.await(5, TimeUnit.SECONDS), "handler should receive the first event");
        for (int i = 1; i < 50; i++) {
            sent.add(sendSessionEvent("s1"));
        }
        releaseFirst.countDown();

        waitUntil(() -> received.size() == sent.size(), "all events should be delivered");
        assertEquals(sent, received, "events must reach the handler in arrival order");
        assertFalse(overlapped.get(), "a session's handler must receive one event at a time");
    }

    @Test
    void handlerErrorDoesNotStopLaterEvents() throws Exception {
        CopilotSession session = createSession("s1");
        var received = new CopyOnWriteArrayList<String>();
        session.on(event -> {
            received.add(event.getId().toString());
            if (received.size() == 1) {
                throw new AssertionError("handler failure");
            }
        });

        sendSessionEvent("s1");
        String second = sendSessionEvent("s1");

        waitUntil(() -> received.contains(second), "an event after a handler error should still be delivered");
    }

    // ===== session.lifecycle tests =====

    @Test
    void lifecycleEventWithMissingTypeAndSessionId() {
        ObjectNode params = MAPPER.createObjectNode();
        // No "type" or "sessionId" fields — defaults to ""

        invokeHandler("session.lifecycle", null, params);

        assertEquals(1, lifecycleEvents.size());
        assertEquals("", lifecycleEvents.get(0).getType());
        assertEquals("", lifecycleEvents.get(0).getSessionId());
    }

    @Test
    void lifecycleEventWithoutMetadata() {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("type", "started");
        params.put("sessionId", "s1");
        // No "metadata" field at all

        invokeHandler("session.lifecycle", null, params);

        assertEquals(1, lifecycleEvents.size());
        assertEquals("started", lifecycleEvents.get(0).getType());
        assertNull(lifecycleEvents.get(0).getMetadata());
    }

    @Test
    void lifecycleEventWithNullMetadata() {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("type", "ended");
        params.put("sessionId", "s2");
        params.putNull("metadata");

        invokeHandler("session.lifecycle", null, params);

        assertEquals(1, lifecycleEvents.size());
        assertEquals("ended", lifecycleEvents.get(0).getType());
        assertNull(lifecycleEvents.get(0).getMetadata());
    }

    // ===== tool.call tests =====

    @Test
    void toolCallWithUnknownSession() throws Exception {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "nonexistent");
        params.put("toolCallId", "tc1");
        params.put("toolName", "my_tool");
        params.putObject("arguments");

        invokeHandler("tool.call", "1", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32602, response.get("error").get("code").asInt());
        assertTrue(response.get("error").get("message").asText().contains("nonexistent"));
    }

    @Test
    void toolCallWithUnknownTool() throws Exception {
        createSession("s1");
        // Don't register any tools

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("toolCallId", "tc1");
        params.put("toolName", "nonexistent_tool");
        params.putObject("arguments");

        invokeHandler("tool.call", "2", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("failure", result.get("resultType").asText());
        assertTrue(result.get("error").asText().contains("nonexistent_tool"));
    }

    @Test
    void toolCallReturnsToolResultObjectDirectly() throws Exception {
        CopilotSession session = createSession("s1");
        var tool = ToolDefinition.create("my_tool", "A test tool", Map.of("type", "object"),
                invocation -> CompletableFuture.completedFuture(ToolResultObject.success("direct result")));
        session.registerTools(List.of(tool));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("toolCallId", "tc1");
        params.put("toolName", "my_tool");
        params.putObject("arguments");

        invokeHandler("tool.call", "3", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("success", result.get("resultType").asText());
        assertEquals("direct result", result.get("textResultForLlm").asText());
    }

    @Test
    void toolCallWithNonStringResult() throws Exception {
        CopilotSession session = createSession("s1");
        var tool = ToolDefinition.create("map_tool", "Returns a map", Map.of("type", "object"),
                invocation -> CompletableFuture.completedFuture(Map.of("key", "value")));
        session.registerTools(List.of(tool));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("toolCallId", "tc1");
        params.put("toolName", "map_tool");
        params.putObject("arguments");

        invokeHandler("tool.call", "4", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("success", result.get("resultType").asText());
        // The map should be serialized to JSON string
        assertNotNull(result.get("textResultForLlm").asText());
    }

    record TaggedResult(String title, java.util.Optional<String> tag) {
    }

    @Test
    void toolCallWithOptionalComponentInResult() throws Exception {
        CopilotSession session = createSession("s1");
        var tool = ToolDefinition.create("optional_tool", "Returns a record with Optional components",
                Map.of("type", "object"),
                invocation -> CompletableFuture.completedFuture(new TaggedResult("a", java.util.Optional.of("b"))));
        session.registerTools(List.of(tool));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("toolCallId", "tc1");
        params.put("toolName", "optional_tool");
        params.putObject("arguments");

        invokeHandler("tool.call", "6", params);

        JsonNode result = readResponse().get("result").get("result");
        assertEquals("success", result.get("resultType").asText());
        assertEquals("{\"title\":\"a\",\"tag\":\"b\"}", result.get("textResultForLlm").asText());
    }

    @Test
    void toolCallHandlerFails() throws Exception {
        CopilotSession session = createSession("s1");
        var tool = ToolDefinition.create("fail_tool", "Fails", Map.of("type", "object"),
                invocation -> CompletableFuture.failedFuture(new RuntimeException("tool error")));
        session.registerTools(List.of(tool));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("toolCallId", "tc1");
        params.put("toolName", "fail_tool");
        params.putObject("arguments");

        invokeHandler("tool.call", "5", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("failure", result.get("resultType").asText());
    }

    // ===== permission.request tests =====

    @Test
    void permissionRequestWithUnknownSession() throws Exception {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "nonexistent");
        params.putObject("permissionRequest");

        invokeHandler("permission.request", "10", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("user-not-available", result.get("kind").asText());
    }

    @Test
    void permissionRequestWithHandler() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerPermissionHandler((request, invocation) -> CompletableFuture
                .completedFuture(new PermissionRequestResult().setKind("allow")));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.putObject("permissionRequest");

        invokeHandler("permission.request", "11", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("allow", result.get("kind").asText());
    }

    @Test
    void permissionRequestHandlerFails() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerPermissionHandler(
                (request, invocation) -> CompletableFuture.failedFuture(new RuntimeException("permission error")));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.putObject("permissionRequest");

        invokeHandler("permission.request", "12", params);

        JsonNode response = readResponse();
        // CopilotSession catches the exception and returns a denied result
        JsonNode result = response.get("result").get("result");
        assertEquals("user-not-available", result.get("kind").asText());
    }

    @Test
    void permissionRequestV2RejectsNoResult() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerPermissionHandler((request, invocation) -> CompletableFuture
                .completedFuture(new PermissionRequestResult().setKind(PermissionRequestResultKind.NO_RESULT)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.putObject("permissionRequest");

        invokeHandler("permission.request", "13", params);

        // V2 protocol does not support NO_RESULT — the handler should fall through
        // to the exception path and respond with denied.
        JsonNode response = readResponse();
        JsonNode result = response.get("result").get("result");
        assertEquals("user-not-available", result.get("kind").asText());
    }

    // ===== userInput.request tests =====

    @Test
    void userInputRequestWithUnknownSession() throws Exception {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "nonexistent");
        params.put("question", "What?");

        invokeHandler("userInput.request", "20", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32602, response.get("error").get("code").asInt());
    }

    @Test
    void userInputRequestWithNullChoicesAndFreeform() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerUserInputHandler((request, invocation) -> CompletableFuture
                .completedFuture(new UserInputResponse().setAnswer("my answer").setWasFreeform(true)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "What is your name?");
        // No "choices" or "allowFreeform" fields

        invokeHandler("userInput.request", "21", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result");
        assertEquals("my answer", result.get("answer").asText());
        assertTrue(result.get("wasFreeform").asBoolean());
    }

    @Test
    void userInputRequestWithNullAnswer() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerUserInputHandler((request, invocation) -> CompletableFuture
                .completedFuture(new UserInputResponse().setAnswer(null).setWasFreeform(false)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "Choose something");

        invokeHandler("userInput.request", "22", params);

        JsonNode response = readResponse();
        JsonNode result = response.get("result");
        // Null answer should be replaced with empty string
        assertEquals("", result.get("answer").asText());
        assertFalse(result.get("wasFreeform").asBoolean());
    }

    @Test
    void userInputRequestWithNoHandler() throws Exception {
        // Session exists but no user input handler registered
        createSession("s1");

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "What?");

        invokeHandler("userInput.request", "23", params);

        JsonNode response = readResponse();
        // No handler → CopilotSession returns failedFuture → dispatcher's
        // .exceptionally() fires
        assertNotNull(response.get("error"));
        assertEquals(-32603, response.get("error").get("code").asInt());
        assertTrue(response.get("error").get("message").asText().contains("User input handler error"));
    }

    @Test
    void userInputRequestHandlerFails() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerUserInputHandler(
                (request, invocation) -> CompletableFuture.failedFuture(new RuntimeException("handler failed")));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "What?");

        invokeHandler("userInput.request", "24", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32603, response.get("error").get("code").asInt());
    }

    @Test
    void userInputRequestHandlerThatThrowsAnErrorIsAnswered() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerUserInputHandler((request, invocation) -> {
            throw new AssertionError("boom");
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "What?");

        invokeHandler("userInput.request", "29", params);

        JsonNode response = readResponse();
        assertEquals(29, response.path("id").asInt());
        assertEquals(-32603, response.at("/error/code").asInt());
    }

    // ===== memoryStore.store tests =====

    @Test
    void memoryStoreInvokesCallbackWithPayload() throws Exception {
        CopilotSession session = createSession("s1");
        AtomicReference<StoredMemory> received = new AtomicReference<>();
        session.registerMemoryStore(item -> {
            received.set(item);
            return CompletableFuture.completedFuture(null);
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("subject", "package manager");
        params.put("fact", "Use pnpm.");
        params.put("citations", "package.json");
        params.put("reason", "The workspace uses pnpm.");
        invokeHandler("memoryStore.store", "90", params);

        JsonNode response = readResponse();
        assertNull(response.get("error"));
        assertTrue(response.has("result"));
        assertTrue(response.get("result").isNull());
        assertEquals(new StoredMemory("s1", "package manager", "Use pnpm.", "package.json", "The workspace uses pnpm."),
                received.get());
    }

    @Test
    void memoryStoreErrorsUseGenericMessages() throws Exception {
        ObjectNode unknownParams = MAPPER.createObjectNode();
        unknownParams.put("sessionId", "missing-session");
        invokeHandler("memoryStore.store", "91", unknownParams);
        assertEquals("Memory store owner is unavailable", readResponse().at("/error/message").asText());

        CopilotSession session = createSession("s1");
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        invokeHandler("memoryStore.store", "92", params);
        assertEquals("No memory store is registered", readResponse().at("/error/message").asText());

        session.registerMemoryStore(
                item -> CompletableFuture.failedFuture(new IllegalStateException("secret backend detail")));
        invokeHandler("memoryStore.store", "93", params);
        JsonNode failure = readResponse();
        assertEquals(-32603, failure.at("/error/code").asInt());
        assertEquals("Memory store callback failed", failure.at("/error/message").asText());
        assertFalse(failure.toString().contains("secret backend detail"));
    }

    @Test
    void memoryStoreCallbackThatThrowsAnErrorIsAnswered() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerMemoryStore(item -> {
            throw new AssertionError("secret assertion detail");
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        invokeHandler("memoryStore.store", "95", params);

        JsonNode response = readResponse();
        assertEquals(95, response.path("id").asInt());
        assertEquals(-32603, response.at("/error/code").asInt());
        assertEquals("Memory store callback failed", response.at("/error/message").asText());
        assertFalse(response.toString().contains("secret assertion detail"));
    }

    @Test
    void memoryStoreCallIsCancelledByCancelRequest() throws Exception {
        CopilotSession session = createSession("s1");
        var entered = new CountDownLatch(1);
        var pending = new CompletableFuture<Void>();
        session.registerMemoryStore(item -> {
            entered.countDown();
            return pending;
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        invokeHandler("memoryStore.store", "94", params);
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        dispatcher.handleCancelRequest(MAPPER.createObjectNode().put("id", 94));

        JsonNode response = readResponse();
        assertEquals(94, response.path("id").asInt());
        assertEquals(-32800, response.at("/error/code").asInt());
        assertTrue(pending.isCancelled());
    }

    @Test
    void userInputRequestPassesRuntimeRequestIdToHandler() throws Exception {
        CopilotSession session = createSession("s1");
        var received = new CompletableFuture<UserInputRequest>();
        session.registerUserInputHandler((request, invocation) -> {
            received.complete(request);
            return CompletableFuture.completedFuture(new UserInputResponse().setAnswer("yes").setWasFreeform(false));
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "Proceed?");
        params.put("requestId", "pending-user-input-1");

        invokeHandler("userInput.request", "25", params);

        JsonNode response = readResponse();
        assertEquals("yes", response.get("result").get("answer").asText());
        UserInputRequest request = received.get(SOCKET_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        assertEquals("pending-user-input-1", request.getRequestId());
        assertEquals("Proceed?", request.getQuestion());
    }

    @Test
    void userInputRequestWithoutRuntimeRequestIdLeavesItNull() throws Exception {
        CopilotSession session = createSession("s1");
        var received = new CompletableFuture<UserInputRequest>();
        session.registerUserInputHandler((request, invocation) -> {
            received.complete(request);
            return CompletableFuture.completedFuture(new UserInputResponse().setAnswer("yes").setWasFreeform(false));
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "Proceed?");

        invokeHandler("userInput.request", "26", params);

        JsonNode response = readResponse();
        assertEquals("yes", response.get("result").get("answer").asText());
        UserInputRequest request = received.get(SOCKET_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        assertNull(request.getRequestId());
    }

    @Test
    void userInputReplyCarriesPersonAnsweredLabelAtTopLevel() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerUserInputHandler(
                (request, invocation) -> CompletableFuture.completedFuture(new UserInputResponse()
                        .setAnswer("Only src/app.ts").setWasFreeform(true).setDecisionContext(personAnswered())));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "Which files?");

        invokeHandler("userInput.request", "36", params);

        JsonNode response = readResponse();
        assertEquals(MAPPER.readTree("{\"answer\":\"Only src/app.ts\",\"wasFreeform\":true,\"decisionContext\":"
                + PERSON_ANSWERED_JSON + "}"), response.get("result"));
    }

    @Test
    void unlabeledUserInputReplyHasNoDecisionContextKey() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerUserInputHandler((request, invocation) -> CompletableFuture
                .completedFuture(new UserInputResponse().setAnswer("Only src/app.ts").setWasFreeform(true)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("question", "Which files?");

        invokeHandler("userInput.request", "37", params);

        JsonNode response = readResponse();
        assertEquals(MAPPER.readTree("{\"answer\":\"Only src/app.ts\",\"wasFreeform\":true}"), response.get("result"));
    }

    // ===== exitPlanMode.request tests =====

    @Test
    void exitPlanModeRequestPassesRuntimeRequestIdToHandler() throws Exception {
        CopilotSession session = createSession("s1");
        var received = new CompletableFuture<ExitPlanModeRequest>();
        session.registerExitPlanModeHandler((request, invocation) -> {
            received.complete(request);
            return CompletableFuture.completedFuture(new ExitPlanModeResult().setApproved(true));
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("summary", "Implement the plan");
        params.putArray("actions").add("interactive").add("autopilot");
        params.put("recommendedAction", "interactive");
        params.put("requestId", "pending-exit-plan-mode-1");

        invokeHandler("exitPlanMode.request", "27", params);

        JsonNode response = readResponse();
        assertTrue(response.get("result").get("approved").asBoolean());
        ExitPlanModeRequest request = received.get(SOCKET_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        assertEquals("pending-exit-plan-mode-1", request.getRequestId());
        assertEquals("Implement the plan", request.getSummary());
        assertEquals("interactive", request.getRecommendedAction());
    }

    @Test
    void exitPlanModeRequestWithoutRuntimeRequestIdLeavesItNull() throws Exception {
        CopilotSession session = createSession("s1");
        var received = new CompletableFuture<ExitPlanModeRequest>();
        session.registerExitPlanModeHandler((request, invocation) -> {
            received.complete(request);
            return CompletableFuture.completedFuture(new ExitPlanModeResult().setApproved(true));
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("summary", "Implement the plan");
        params.putArray("actions").add("interactive").add("autopilot");
        params.put("recommendedAction", "interactive");

        invokeHandler("exitPlanMode.request", "28", params);

        JsonNode response = readResponse();
        assertTrue(response.get("result").get("approved").asBoolean());
        ExitPlanModeRequest request = received.get(SOCKET_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        assertNull(request.getRequestId());
    }

    @Test
    void exitPlanModeRequestHandlerThatThrowsAnErrorIsAnswered() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerExitPlanModeHandler((request, invocation) -> {
            throw new AssertionError("boom");
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("summary", "Implement the plan");

        invokeHandler("exitPlanMode.request", "35", params);

        JsonNode response = readResponse();
        assertEquals(35, response.path("id").asInt());
        assertEquals(-32603, response.at("/error/code").asInt());
    }

    @Test
    void exitPlanModeReplyCarriesPersonAnsweredLabelAtTopLevel() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerExitPlanModeHandler(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult().setApproved(true)
                        .setSelectedAction("interactive").setDecisionContext(personAnswered())));

        invokeHandler("exitPlanMode.request", "38", exitPlanModeParams());

        JsonNode response = readResponse();
        assertEquals(MAPPER.readTree("{\"approved\":true,\"selectedAction\":\"interactive\",\"decisionContext\":"
                + PERSON_ANSWERED_JSON + "}"), response.get("result"));
    }

    @Test
    void unlabeledExitPlanModeReplyHasNoDecisionContextKey() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerExitPlanModeHandler((request, invocation) -> CompletableFuture
                .completedFuture(new ExitPlanModeResult().setApproved(true).setSelectedAction("interactive")));

        invokeHandler("exitPlanMode.request", "39", exitPlanModeParams());

        JsonNode response = readResponse();
        assertEquals(MAPPER.readTree("{\"approved\":true,\"selectedAction\":\"interactive\"}"), response.get("result"));
    }

    private static ObjectNode exitPlanModeParams() {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("summary", "Implement the plan");
        params.putArray("actions").add("interactive").add("autopilot");
        params.put("recommendedAction", "interactive");
        return params;
    }

    private static PermissionDecisionContext personAnswered() {
        return PermissionDecisionContext.builder().source(PermissionDecisionSource.HUMAN_RESPONSE)
                .responseCapability(PermissionResponseCapability.INTERACTIVE).surface(PermissionDecisionSurface.SDK)
                .outcome(PermissionDecisionOutcome.PROMPTED_USER).build();
    }

    // ===== autoModeSwitch.request tests =====

    @Test
    void autoModeSwitchRequestHandlerThatThrowsAnErrorIsAnswered() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerAutoModeSwitchHandler((request, invocation) -> {
            throw new AssertionError("boom");
        });

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("errorCode", "user_weekly_rate_limited");

        invokeHandler("autoModeSwitch.request", "36", params);

        JsonNode response = readResponse();
        assertEquals(36, response.path("id").asInt());
        assertEquals(-32603, response.at("/error/code").asInt());
    }

    // ===== skillProvider.* tests =====

    @Test
    void skillProviderListAndReadSuccess() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerSkillProvider(
                skillProvider(
                        () -> CompletableFuture.completedFuture(
                                List.of(new SkillProviderDescriptor("docs", "Documentation", null, null, "topic"),
                                        new SkillProviderDescriptor("scripted", "Has supporting files", null, null,
                                                null, "/skills/scripted"))),
                        name -> CompletableFuture.completedFuture("# " + name)));

        ObjectNode listParams = MAPPER.createObjectNode();
        listParams.put("sessionId", "s1");
        invokeHandler("skillProvider.list", "60", listParams);

        JsonNode listResponse = readResponse();
        JsonNode descriptor = listResponse.at("/result/skills/0");
        assertEquals("docs", descriptor.path("name").asText());
        assertEquals("Documentation", descriptor.path("description").asText());
        assertEquals("topic", descriptor.path("argumentHint").asText());
        assertFalse(descriptor.has("userInvocable"));
        assertFalse(descriptor.has("disableModelInvocation"));
        assertFalse(descriptor.has("directory"));
        assertEquals("/skills/scripted", listResponse.at("/result/skills/1/directory").asText());

        ObjectNode readParams = MAPPER.createObjectNode();
        readParams.put("sessionId", "s1");
        readParams.put("name", "docs");
        invokeHandler("skillProvider.read", "61", readParams);

        JsonNode readResponse = readResponse();
        assertEquals("# docs", readResponse.at("/result/markdown").asText());
    }

    @Test
    void skillProviderNullListReturnsEmptyList() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerSkillProvider(skillProvider(() -> CompletableFuture.completedFuture(null),
                name -> CompletableFuture.completedFuture("# " + name)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        invokeHandler("skillProvider.list", "62", params);

        JsonNode response = readResponse();
        assertTrue(response.at("/result/skills").isArray());
        assertEquals(0, response.at("/result/skills").size());
    }

    @Test
    void skillProviderReadNullReturnsNullMarkdown() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerSkillProvider(skillProvider(() -> CompletableFuture.completedFuture(List.of()),
                name -> CompletableFuture.completedFuture(null)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("name", "missing");
        invokeHandler("skillProvider.read", "63", params);

        JsonNode response = readResponse();
        assertNull(response.get("error"));
        assertTrue(response.at("/result").has("markdown"));
        assertTrue(response.at("/result/markdown").isNull());
    }

    @Test
    void skillProviderFailuresUseStableMessagesWithoutData() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerSkillProvider(skillProvider(
                () -> CompletableFuture.failedFuture(new IllegalStateException("secret list failure")), name -> {
                    throw new IllegalStateException("secret read failure");
                }));

        ObjectNode listParams = MAPPER.createObjectNode();
        listParams.put("sessionId", "s1");
        invokeHandler("skillProvider.list", "64", listParams);
        JsonNode listResponse = readResponse();
        assertEquals(-32603, listResponse.at("/error/code").asInt());
        assertEquals("Skill provider listSkills failed", listResponse.at("/error/message").asText());
        assertFalse(listResponse.path("error").has("data"));

        ObjectNode readParams = MAPPER.createObjectNode();
        readParams.put("sessionId", "s1");
        readParams.put("name", "docs");
        invokeHandler("skillProvider.read", "65", readParams);
        JsonNode readResponse = readResponse();
        assertEquals(-32603, readResponse.at("/error/code").asInt());
        assertEquals("Skill provider readSkill failed", readResponse.at("/error/message").asText());
        assertFalse(readResponse.path("error").has("data"));
    }

    @Test
    void skillProviderErrorsForUnknownMissingOrClearedProviderHaveNoData() throws Exception {
        ObjectNode unknownParams = MAPPER.createObjectNode();
        unknownParams.put("sessionId", "missing-session");
        invokeHandler("skillProvider.list", "66", unknownParams);
        JsonNode unknownResponse = readResponse();
        assertEquals(-32603, unknownResponse.at("/error/code").asInt());
        assertEquals("No skill provider for session: missing-session", unknownResponse.at("/error/message").asText());
        assertFalse(unknownResponse.path("error").has("data"));

        createSession("s1");
        ObjectNode noProviderParams = MAPPER.createObjectNode();
        noProviderParams.put("sessionId", "s1");
        noProviderParams.put("name", "docs");
        invokeHandler("skillProvider.read", "67", noProviderParams);
        JsonNode noProviderResponse = readResponse();
        assertEquals(-32603, noProviderResponse.at("/error/code").asInt());
        assertEquals("No skill provider for session: s1", noProviderResponse.at("/error/message").asText());
        assertFalse(noProviderResponse.path("error").has("data"));

        CopilotSession session = sessions.get("s1");
        session.registerSkillProvider(skillProvider(() -> CompletableFuture.completedFuture(List.of()),
                name -> CompletableFuture.completedFuture("# " + name)));
        session.clearSkillProvider();
        invokeHandler("skillProvider.list", "68", noProviderParams);
        JsonNode clearedResponse = readResponse();
        assertEquals(-32603, clearedResponse.at("/error/code").asInt());
        assertEquals("No skill provider for session: s1", clearedResponse.at("/error/message").asText());
        assertFalse(clearedResponse.path("error").has("data"));
    }

    @Test
    void skillProviderCallIsCancelledByCancelRequest() throws Exception {
        CopilotSession session = createSession("s1");
        var entered = new CountDownLatch(1);
        var pending = new CompletableFuture<List<SkillProviderDescriptor>>();
        session.registerSkillProvider(skillProvider(() -> {
            entered.countDown();
            return pending;
        }, name -> CompletableFuture.completedFuture(null)));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        invokeHandler("skillProvider.list", "69", params);
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        dispatcher.handleCancelRequest(MAPPER.createObjectNode().put("id", 69));

        JsonNode response = readResponse();
        assertEquals(69, response.path("id").asInt());
        assertEquals(-32800, response.at("/error/code").asInt());
        assertTrue(pending.isCancelled());
    }

    @Test
    void skillProviderCallCancelledBeforeDispatchIsNotStarted() throws Exception {
        CopilotSession session = createSession("s1");
        var calls = new AtomicInteger();
        session.registerSkillProvider(skillProvider(() -> {
            calls.incrementAndGet();
            return CompletableFuture.completedFuture(List.of());
        }, name -> CompletableFuture.completedFuture(null)));
        var blockedExecutor = new CompletableFuture<Void>();
        var cancellingDispatcher = new RpcHandlerDispatcher(sessions, lifecycleEvents::add,
                task -> blockedExecutor.thenRun(task), gitHubTokenProviders, sessions.values());
        cancellingDispatcher.registerHandlers(rpc);

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        invokeHandler("skillProvider.list", "70", params);
        cancellingDispatcher.handleCancelRequest(MAPPER.createObjectNode().put("id", 70));
        blockedExecutor.complete(null);

        assertEquals(-32800, readResponse().at("/error/code").asInt());
        assertEquals(0, calls.get());
    }

    @Test
    void skillProviderCallsAreCancelledWhenTheConnectionCloses() throws Exception {
        CopilotSession session = createSession("s1");
        var entered = new CountDownLatch(1);
        var pending = new CompletableFuture<String>();
        session.registerSkillProvider(skillProvider(() -> CompletableFuture.completedFuture(List.of()), name -> {
            entered.countDown();
            return pending;
        }));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("name", "docs");
        invokeHandler("skillProvider.read", "71", params);
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        dispatcher.cancelCallbacks();

        assertEquals(-32800, readResponse().at("/error/code").asInt());
        assertTrue(pending.isCancelled());
    }

    @Test
    void cancelRequestForAnotherRequestLeavesSkillProviderCallRunning() throws Exception {
        CopilotSession session = createSession("s1");
        var entered = new CountDownLatch(1);
        var pending = new CompletableFuture<String>();
        session.registerSkillProvider(skillProvider(() -> CompletableFuture.completedFuture(List.of()), name -> {
            entered.countDown();
            return pending;
        }));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("name", "docs");
        invokeHandler("skillProvider.read", "72", params);
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        dispatcher.handleCancelRequest(MAPPER.createObjectNode().put("id", 73));
        pending.complete("# docs");

        assertEquals("# docs", readResponse().at("/result/markdown").asText());
    }

    // ===== hooks.invoke tests =====

    @Test
    void hooksInvokeWithUnknownSession() throws Exception {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "nonexistent");
        params.put("hookType", "preToolUse");
        params.putObject("input");

        invokeHandler("hooks.invoke", "30", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32602, response.get("error").get("code").asInt());
    }

    @Test
    void hooksInvokeWithNullOutput() throws Exception {
        CopilotSession session = createSession("s1");
        // Register empty hooks — no specific handler for preToolUse → returns null
        session.registerHooks(new SessionHooks());

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("hookType", "preToolUse");
        params.putObject("input");

        invokeHandler("hooks.invoke", "31", params);

        JsonNode response = readResponse();
        JsonNode output = response.get("result").get("output");
        assertTrue(output == null || output.isNull(), "Output should be null when no hook handler is set");
    }

    @Test
    void hooksInvokeWithNonNullOutput() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerHooks(new SessionHooks().setOnPreToolUse(
                (input, invocation) -> CompletableFuture.completedFuture(PreToolUseHookOutput.allow())));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("hookType", "preToolUse");
        ObjectNode input = params.putObject("input");
        input.put("toolName", "some_tool");
        input.put("toolCallId", "tc1");

        invokeHandler("hooks.invoke", "32", params);

        JsonNode response = readResponse();
        JsonNode output = response.get("result").get("output");
        assertNotNull(output);
        assertEquals("allow", output.get("permissionDecision").asText());
    }

    @Test
    void hooksInvokeHandlerFails() throws Exception {
        CopilotSession session = createSession("s1");
        session.registerHooks(new SessionHooks().setOnPreToolUse(
                (input, invocation) -> CompletableFuture.failedFuture(new RuntimeException("hook error"))));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("hookType", "preToolUse");
        ObjectNode input = params.putObject("input");
        input.put("toolName", "some_tool");
        input.put("toolCallId", "tc1");

        invokeHandler("hooks.invoke", "33", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32603, response.get("error").get("code").asInt());
        assertTrue(response.get("error").get("message").asText().contains("Hooks handler error"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"preToolUse", "userPromptSubmitted"})
    void hooksInvokeHandlerThatThrowsAnErrorIsAnswered(String hookType) throws Exception {
        CopilotSession session = createSession("s1");
        session.registerHooks(new SessionHooks().setOnPreToolUse((input, invocation) -> {
            throw new AssertionError("policy invariant broken");
        }).setOnUserPromptSubmitted((input, invocation) -> {
            throw new AssertionError("policy invariant broken");
        }));

        invokeHandler("hooks.invoke", "35", hookParams("s1", hookType));

        JsonNode response = readResponse();
        assertEquals(35, response.path("id").asInt());
        assertEquals(-32603, response.at("/error/code").asInt());
        String message = response.at("/error/message").asText();
        assertTrue(message.startsWith("Hooks handler error: "), message);
        assertTrue(message.contains("policy invariant broken"), message);
    }

    @ParameterizedTest
    @ValueSource(strings = {"preToolUse", "userPromptSubmitted"})
    void hooksInvokeHandlerThatThrowsAnExceptionIsAnswered(String hookType) throws Exception {
        CopilotSession session = createSession("s1");
        session.registerHooks(new SessionHooks().setOnPreToolUse((input, invocation) -> {
            throw new IllegalStateException("policy service failed");
        }).setOnUserPromptSubmitted((input, invocation) -> {
            throw new IllegalStateException("policy service failed");
        }));

        invokeHandler("hooks.invoke", "36", hookParams("s1", hookType));

        JsonNode response = readResponse();
        assertEquals(36, response.path("id").asInt());
        assertEquals(-32603, response.at("/error/code").asInt());
        String message = response.at("/error/message").asText();
        assertTrue(message.startsWith("Hooks handler error: "), message);
        assertTrue(message.contains("policy service failed"), message);
    }

    @Test
    void hooksInvokeRethrowsAFatalErrorFromTheHook() {
        CopilotSession session = createSession("s1");
        session.registerHooks(new SessionHooks().setOnPreToolUse((input, invocation) -> {
            throw new LinkageError("class failed to load");
        }));

        var error = assertThrows(LinkageError.class,
                () -> session.handleHooksInvoke("preToolUse", hookParams("s1", "preToolUse").get("input")));
        assertEquals("class failed to load", error.getMessage());
    }

    private static ObjectNode hookParams(String sessionId, String hookType) {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", sessionId);
        params.put("hookType", hookType);
        ObjectNode input = params.putObject("input");
        if (hookType.equals("preToolUse")) {
            input.put("toolName", "some_tool");
            input.put("toolCallId", "tc1");
        } else {
            input.put("prompt", "hello");
        }
        return params;
    }

    @Test
    void hooksInvokeWithNoHooksRegistered() throws Exception {
        // Session exists but no hooks registered at all → returns null output
        createSession("s1");

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.put("hookType", "preToolUse");
        params.putObject("input");

        invokeHandler("hooks.invoke", "34", params);

        JsonNode response = readResponse();
        JsonNode output = response.get("result").get("output");
        assertTrue(output == null || output.isNull(), "Output should be null when no hooks registered");
    }

    // ===== systemMessage.transform tests =====

    @Test
    void systemMessageTransformWithUnknownSession() throws Exception {
        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "nonexistent");
        params.putObject("sections");

        invokeHandler("systemMessage.transform", "40", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32602, response.get("error").get("code").asInt());
    }

    @Test
    void systemMessageTransformWithNullSessionId() throws Exception {
        ObjectNode params = MAPPER.createObjectNode();
        // sessionId omitted → null → session lookup returns null → error
        params.putObject("sections");

        invokeHandler("systemMessage.transform", "41", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("error"));
        assertEquals(-32602, response.get("error").get("code").asInt());
    }

    @Test
    void systemMessageTransformKeepsSectionWhenCallbackThrowsOrReturnsNull() throws Exception {
        // A failing callback keeps its own section; other sections still transform
        CopilotSession session = createSession("s1");
        Map<String, Function<String, CompletableFuture<String>>> callbacks = Map.of("identity", content -> {
            throw new IllegalStateException("transform failed");
        }, "tone", content -> null, "safety", content -> CompletableFuture.completedFuture(content + " (transformed)"));
        session.registerTransformCallbacks(callbacks);

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        ObjectNode sections = params.putObject("sections");
        sections.putObject("identity").put("content", "Original identity");
        sections.putObject("tone").put("content", "Original tone");
        sections.putObject("safety").put("content", "Original safety");

        invokeHandler("systemMessage.transform", "44", params);

        JsonNode resultSections = readResponse().get("result").get("sections");
        assertEquals("Original identity", resultSections.get("identity").get("content").asText());
        assertEquals("Original tone", resultSections.get("tone").get("content").asText());
        assertEquals("Original safety (transformed)", resultSections.get("safety").get("content").asText());
    }

    @Test
    void systemMessageTransformKeepsSectionWhenCallbackThrowsAnError() throws Exception {
        // A callback that throws a non-fatal Error keeps its section, as one that
        // throws an exception does, and the request is still answered
        CopilotSession session = createSession("s1");
        Map<String, Function<String, CompletableFuture<String>>> callbacks = Map.of("identity", content -> {
            throw new AssertionError("transform assertion");
        }, "tone", content -> {
            throw new IllegalStateException("transform failed");
        }, "safety", content -> CompletableFuture.completedFuture(content + " (transformed)"));
        session.registerTransformCallbacks(callbacks);

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        ObjectNode sections = params.putObject("sections");
        sections.putObject("identity").put("content", "Original identity");
        sections.putObject("tone").put("content", "Original tone");
        sections.putObject("safety").put("content", "Original safety");

        invokeHandler("systemMessage.transform", "45", params);

        JsonNode response = readResponse();
        assertEquals(45, response.path("id").asInt());
        JsonNode resultSections = response.get("result").get("sections");
        assertEquals("Original identity", resultSections.get("identity").get("content").asText());
        assertEquals("Original tone", resultSections.get("tone").get("content").asText());
        assertEquals("Original safety (transformed)", resultSections.get("safety").get("content").asText());
    }

    @Test
    void systemMessageTransformRethrowsAFatalErrorFromTheCallback() throws Exception {
        // A fatal error is rethrown, as on every other callback path, not answered.
        var called = new CountDownLatch(1);
        CopilotSession session = createSession("s1");
        session.registerTransformCallbacks(Map.of("identity", content -> {
            called.countDown();
            throw new LinkageError("class failed to load");
        }));

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        params.putObject("sections").putObject("identity").put("content", "Original identity");

        invokeHandler("systemMessage.transform", "46", params);

        assertTrue(called.await(SOCKET_TIMEOUT_MS, TimeUnit.MILLISECONDS), "The callback must be called");
        serverSideSocket.setSoTimeout(500);
        assertThrows(java.net.SocketTimeoutException.class, this::readResponse);
    }

    @Test
    void systemMessageTransformWithKnownSessionNoCallbacks() throws Exception {
        // Session without transform callbacks returns the sections unchanged
        createSession("s1");

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        ObjectNode sections = params.putObject("sections");
        ObjectNode sectionData = sections.putObject("identity");
        sectionData.put("content", "Original content");

        invokeHandler("systemMessage.transform", "42", params);

        JsonNode response = readResponse();
        assertNotNull(response.get("result"));
        JsonNode resultSections = response.get("result").get("sections");
        assertNotNull(resultSections);
        assertEquals("Original content", resultSections.get("identity").get("content").asText());
    }

    @Test
    void systemMessageTransformKeepsSectionWhenCallbackCompletesWithNull() throws Exception {
        // A null result keeps the section; an empty string still clears it
        CopilotSession session = createSession("s1");
        Map<String, Function<String, CompletableFuture<String>>> callbacks = Map.of("identity",
                content -> CompletableFuture.completedFuture(null), "tone",
                content -> CompletableFuture.completedFuture(""));
        session.registerTransformCallbacks(callbacks);

        ObjectNode params = MAPPER.createObjectNode();
        params.put("sessionId", "s1");
        ObjectNode sections = params.putObject("sections");
        sections.putObject("identity").put("content", "Original identity");
        sections.putObject("tone").put("content", "Original tone");

        invokeHandler("systemMessage.transform", "43", params);

        JsonNode resultSections = readResponse().get("result").get("sections");
        assertEquals("Original identity", resultSections.get("identity").get("content").asText());
        assertEquals("", resultSections.get("tone").get("content").asText());
    }

    private static SkillProvider skillProvider(Supplier<CompletableFuture<List<SkillProviderDescriptor>>> listSkills,
            Function<String, CompletableFuture<String>> readSkill) {
        return new SkillProvider() {
            @Override
            public CompletableFuture<List<SkillProviderDescriptor>> listSkills() {
                return listSkills.get();
            }

            @Override
            public CompletableFuture<String> readSkill(String name) {
                return readSkill.apply(name);
            }
        };
    }
}
