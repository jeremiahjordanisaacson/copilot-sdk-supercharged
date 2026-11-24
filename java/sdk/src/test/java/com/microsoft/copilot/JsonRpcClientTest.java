/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.generated.SessionEvent;
import com.microsoft.copilot.generated.SessionIdleEvent;
import com.microsoft.copilot.generated.rpc.FeatureFlagAssignmentsUpdate;
import com.microsoft.copilot.generated.rpc.ServerRpc;
import com.microsoft.copilot.generated.rpc.UserSettingsSetParams;
import com.microsoft.copilot.generated.rpc.ShellBuildInteractiveEnvironmentParams;
import com.microsoft.copilot.generated.rpc.TaskAnalyticsClassifyTrajectoryParams;
import com.microsoft.copilot.generated.rpc.LocalModelsWatchParams;
import com.microsoft.copilot.generated.rpc.SandboxProxyCaCheckRelevanceParams;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsConfigureParams;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsReadParams;
import com.microsoft.copilot.generated.rpc.SessionFeatureFlagsUpdateParams;
import com.microsoft.copilot.generated.rpc.SessionRpc;
import com.microsoft.copilot.generated.rpc.SessionMcpCallToolParams;
import com.microsoft.copilot.generated.rpc.SessionMcpStartServerParams;

class JsonRpcClientTest {

    private static final ObjectMapper MAPPER = JsonRpcClient.getObjectMapper();

    private static final class SettingsIntegerValue {
        private final BigInteger value;
        private int calls;

        private SettingsIntegerValue(String value) {
            this.value = new BigInteger(value);
        }

        @JsonValue
        public BigInteger serialize() {
            calls++;
            return value;
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"-9223372036854775809", "18446744073709551617"})
    void settingsSetRejectsNestedOversizedCustomValuesBeforeSending(String integer) throws Exception {
        try (var pair = createSocketPair()) {
            var value = new SettingsIntegerValue(integer);
            var rpc = new ServerRpc(pair.client::invoke);
            var result = rpc.user.settings.set(UserSettingsSetParams.builder()
                    .settings(Map.of("nested", List.of(Map.of("value", value)))).build());
            assertTrue(result.isCompletedExceptionally());
            var error = assertThrows(ExecutionException.class, () -> result.get(5, TimeUnit.SECONDS));
            assertInstanceOf(IOException.class, error.getCause());
            assertEquals(1, value.calls);
            assertEquals(integer, value.value.toString());
            assertReaderResponsive(pair);
        }
    }

    @Test
    void settingsSetPreservesSupportedNumbersAndSerializesCustomValuesOnce() throws Exception {
        try (var pair = createSocketPair()) {
            var maximum = new SettingsIntegerValue("18446744073709551615");
            var settings = new LinkedHashMap<String, Object>();
            settings.put("minimum", BigInteger.valueOf(Long.MIN_VALUE));
            settings.put("maximum", maximum);
            settings.put("floating", new BigDecimal("1e100"));
            settings.put("text", "18446744073709551617");
            settings.put("empty", null);
            var rpc = new ServerRpc(pair.client::invoke);
            var result = rpc.user.settings.set(UserSettingsSetParams.builder().settings(settings).build());
            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            var sent = request.path("params").path("settings");
            assertEquals(BigInteger.valueOf(Long.MIN_VALUE), sent.path("minimum").bigIntegerValue());
            assertEquals(maximum.value, sent.path("maximum").bigIntegerValue());
            assertEquals(1e100, sent.path("floating").doubleValue());
            assertEquals("18446744073709551617", sent.path("text").textValue());
            assertTrue(sent.path("empty").isNull());
            assertEquals(1, maximum.calls);
            assertSame(maximum, settings.get("maximum"));
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + request.path("id") + ",\"result\":null}");
            result.get(5, TimeUnit.SECONDS);
        }
    }

    @Test
    void testLocalModelWatchCancellationAndTimeoutRetireTheOriginalRequest() throws Exception {
        for (boolean timeout : List.of(false, true)) {
            try (var pair = createSocketPair()) {
                pair.serverSide.setSoTimeout(5_000);
                var rpc = new ServerRpc(pair.client::invoke);
                var waiting = rpc.localModels.watch(LocalModelsWatchParams.builder().afterRevision(7L).build());
                var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                assertEquals("localModels.watch", request.path("method").asText());
                if (timeout) {
                    waiting.orTimeout(1, TimeUnit.MILLISECONDS);
                    assertThrows(ExecutionException.class, () -> waiting.get(5, TimeUnit.SECONDS));
                } else {
                    assertTrue(waiting.cancel(false));
                }
                var cancellation = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                assertEquals("$/cancelRequest", cancellation.path("method").asText());
                assertEquals(request.path("id"), cancellation.path("params").path("id"));
                writeRpcMessage(pair.serverSide.getOutputStream(),
                        "{\"jsonrpc\":\"2.0\",\"id\":" + request.path("id") + ",\"result\":null}");
                assertReaderResponsive(pair);
            }
        }
    }

    // ---- Helpers ----

    private record SocketPair(JsonRpcClient client, Socket serverSide,
            ServerSocket serverSocket) implements AutoCloseable {

        @Override
        public void close() throws Exception {
            client.close();
            serverSide.close();
            serverSocket.close();
        }
    }

    private SocketPair createSocketPair() throws Exception {
        var loopback = InetAddress.getLoopbackAddress();
        var serverSocket = new ServerSocket(0, 1, loopback);
        var clientSocket = new Socket(loopback, serverSocket.getLocalPort());
        var serverSide = serverSocket.accept();
        var client = JsonRpcClient.fromSocket(clientSocket);
        return new SocketPair(client, serverSide, serverSocket);
    }

    /** Write a raw JSON-RPC message (with Content-Length header) to a stream. */
    private void writeRpcMessage(OutputStream out, String json) throws IOException {
        byte[] content = json.getBytes(StandardCharsets.UTF_8);
        String header = "Content-Length: " + content.length + "\r\n\r\n";
        out.write(header.getBytes(StandardCharsets.UTF_8));
        out.write(content);
        out.flush();
    }

    /** Read a single JSON-RPC message (Content-Length framed) from a stream. */
    private String readRpcMessage(InputStream in) throws IOException {
        var headerLine = new StringBuilder();
        int contentLength = -1;
        boolean lastWasCR = false;
        boolean inHeaders = true;

        while (inHeaders) {
            int b = in.read();
            if (b == -1)
                throw new IOException("EOF");
            if (b == '\r') {
                lastWasCR = true;
            } else if (b == '\n') {
                String line = headerLine.toString();
                headerLine.setLength(0);
                lastWasCR = false;
                if (line.isEmpty()) {
                    inHeaders = false;
                } else if (line.toLowerCase().startsWith("content-length:")) {
                    contentLength = Integer.parseInt(line.substring(15).trim());
                }
            } else {
                if (lastWasCR) {
                    headerLine.append('\r');
                    lastWasCR = false;
                }
                headerLine.append((char) b);
            }
        }

        byte[] buffer = new byte[contentLength];
        int read = 0;
        while (read < contentLength) {
            int result = in.read(buffer, read, contentLength - read);
            if (result == -1)
                throw new IOException("EOF");
            read += result;
        }
        return new String(buffer, StandardCharsets.UTF_8);
    }

    private void assertReaderResponsive(SocketPair pair) throws Exception {
        var response = pair.client.invoke("fixture.barrier", Map.of(), Boolean.class);
        var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
        writeRpcMessage(pair.serverSide.getOutputStream(),
                "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":true}");
        assertTrue(response.get(5, TimeUnit.SECONDS));
    }

    @Test
    void cancellingTrajectoryClassificationSendsCancelRequest() throws Exception {
        try (var pair = createSocketPair()) {
            var response = pair.client.invoke("taskAnalytics.classifyTrajectory", Map.of("events", List.of()),
                    JsonNode.class);
            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertEquals("taskAnalytics.classifyTrajectory", request.get("method").asText());

            assertTrue(response.cancel(false));

            var cancel = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertEquals("$/cancelRequest", cancel.get("method").asText());
            assertEquals(request.get("id"), cancel.path("params").get("id"));
        }
    }

    @Test
    void trajectoryClassificationAcceptsTypedSessionEvents() throws Exception {
        try (var pair = createSocketPair()) {
            var idle = new SessionIdleEvent();
            idle.setData(SessionIdleEvent.SessionIdleEventData.builder().build());
            List<SessionEvent> events = List.of(idle);
            new ServerRpc(pair.client::invoke).taskAnalytics.classifyTrajectory(
                    TaskAnalyticsClassifyTrajectoryParams.builder().events(events).validateOnly(true).build());

            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertEquals("taskAnalytics.classifyTrajectory", request.get("method").asText());
            assertEquals("session.idle", request.path("params").path("events").path(0).path("type").asText());
        }
    }

    // ---- notify() ----

    private record NullableToolArguments(String value, List<NullableToolArguments> nested) {
    }

    @Test
    @AllowCopilotExperimental
    void testOpaquePojoArgumentsPreserveNullsWithoutAddingOptionalProtocolFields() throws Exception {
        try (var pair = createSocketPair()) {
            var session = new SessionRpc(pair.client::invoke, "tool-session");
            var arguments = new NullableToolArguments(null, List.of(new NullableToolArguments(null, null)));
            var result = session.mcp.callTool(SessionMcpCallToolParams.builder().serverName("fixture")
                    .toolName("inspect").arguments(arguments).build());
            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":{}}");
            result.get(5, TimeUnit.SECONDS);
            assertEquals(MAPPER.readTree("""
                    {"sessionId":"tool-session","serverName":"fixture","toolName":"inspect",
                     "arguments":{"value":null,"nested":[{"value":null,"nested":null}]}}
                    """), request.get("params"));

            var start = session.mcp.startServer(SessionMcpStartServerParams.builder().serverName("fixture").build());
            request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":{}}");
            start.get(5, TimeUnit.SECONDS);
            assertEquals(MAPPER.readTree("""
                    {"sessionId":"tool-session","serverName":"fixture"}
                    """), request.get("params"));

            pair.client.notify("fixture.notification", null);
            request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertFalse(request.has("id"));
            assertFalse(request.has("params"));
        }
    }

    @Test
    void cancellingWaitUsesItsRequestIdAndKeepsOtherRequestsWorking() throws Exception {
        for (var method : List.of("session.sendAndWait", "feedback.upload.read", "fixture.pending")) {
            try (var pair = createSocketPair()) {
                pair.serverSide.setSoTimeout(5000);
                var pending = pair.client.invoke(method, Map.of(), JsonNode.class);
                var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                assertTrue(pending.cancel(false));
                if (!method.equals("fixture.pending")) {
                    var cancellation = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                    assertEquals("$/cancelRequest", cancellation.path("method").asText());
                    assertEquals(request.get("id"), cancellation.path("params").get("id"));
                    assertFalse(cancellation.has("id"));
                }
                assertReaderResponsive(pair);
            }
        }
    }

    @Test
    @AllowCopilotExperimental
    void testFeatureFlagConfigurePreservesExplicitNullOverridesOnWire() throws Exception {
        try (var pair = createSocketPair()) {
            var overrides = new LinkedHashMap<String, Object>();
            overrides.put("cleared", null);
            overrides.put("enabled", false);
            overrides.put("variant", "treatment");
            overrides.put("count", 3);
            var session = new SessionRpc(pair.client::invoke, "feature-flags-session");
            var result = session.featureFlags
                    .configure(SessionFeatureFlagsConfigureParams.builder().isStaff(false).isExperimental(false)
                            .isTeam(false).online(false).streamerMode(false).expFlagOverrides(overrides).build());

            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":{}}");
            assertNotNull(result.get(5, TimeUnit.SECONDS));
            assertEquals("session.featureFlags.configure", request.get("method").asText());
            assertEquals(MAPPER.readTree("""
                    {"sessionId":"feature-flags-session","isStaff":false,"isExperimental":false,
                     "isTeam":false,"online":false,"streamerMode":false,"expFlagOverrides":
                      {"cleared":null,"enabled":false,"variant":"treatment","count":3}}
                    """), request.get("params"));
            assertTrue(overrides.containsKey("cleared"));
            assertNull(overrides.get("cleared"));
            assertEquals(4, overrides.size());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"configure", "update"})
    @AllowCopilotExperimental
    void testFeatureFlagAssignmentsPreserveNestedNullsOnWire(String method) throws Exception {
        try (var pair = createSocketPair()) {
            var parameters = new LinkedHashMap<String, Object>();
            parameters.put("someFlag", null);
            parameters.put("enabled", false);
            parameters.put("variant", "treatment");
            parameters.put("count", 3);
            var response = Map.of("Configs", List.of(Map.of("Id", "assignment", "Parameters", parameters)),
                    "AssignmentContext", "context");
            var updates = new FeatureFlagAssignmentsUpdate[]{
                    FeatureFlagAssignmentsUpdate.builder().response(response).build(),
                    FeatureFlagAssignmentsUpdate.builder().response(null).build(), null};
            var expectedParams = new String[]{"""
                    {"sessionId":"feature-flags-session","assignments":{"response":{
                      "Configs":[{"Id":"assignment","Parameters":{
                        "someFlag":null,"enabled":false,"variant":"treatment","count":3}}],
                      "AssignmentContext":"context"}}}
                    """, """
                    {"sessionId":"feature-flags-session","assignments":{"response":null}}
                    """, """
                    {"sessionId":"feature-flags-session"}
                    """};
            var session = new SessionRpc(pair.client::invoke, "feature-flags-session");
            for (int index = 0; index < updates.length; index++) {
                var result = method.equals("configure")
                        ? session.featureFlags.configure(SessionFeatureFlagsConfigureParams.builder().isStaff(false)
                                .isExperimental(false).isTeam(false).online(false).streamerMode(false)
                                .assignments(updates[index]).build())
                        : session.featureFlags
                                .update(SessionFeatureFlagsUpdateParams.builder().assignments(updates[index]).build());
                var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                writeRpcMessage(pair.serverSide.getOutputStream(),
                        "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":{}}");
                assertNotNull(result.get(5, TimeUnit.SECONDS));
                assertEquals("session.featureFlags." + method, request.get("method").asText());
                var expected = MAPPER.readValue(expectedParams[index], ObjectNode.class);
                if (method.equals("configure")) {
                    expected.put("isStaff", false).put("isExperimental", false).put("isTeam", false)
                            .put("online", false).put("streamerMode", false);
                }
                assertEquals(expected, request.get("params"));
            }
            assertTrue(parameters.containsKey("someFlag"));
            assertNull(parameters.get("someFlag"));
            assertEquals(4, parameters.size());
        }
    }

    @Test
    void sessionCallbackScopeLastsUntilEveryReplyOrConnectionClose() throws Exception {
        try (var pair = createSocketPair()) {
            var first = new CompletableFuture<Void>();
            var second = new CompletableFuture<Void>();
            var third = new CompletableFuture<Void>();
            pair.client.registerMethodHandler("fixture.callback", (id, params) -> {
                switch (id) {
                    case "101" -> first.complete(null);
                    case "102" -> second.complete(null);
                    case "103" -> third.complete(null);
                    default -> fail("Unexpected callback ID: " + id);
                }
            });
            for (int id : List.of(101, 102)) {
                writeRpcMessage(pair.serverSide.getOutputStream(), "{\"jsonrpc\":\"2.0\",\"id\":" + id
                        + ",\"method\":\"fixture.callback\",\"params\":{\"sessionId\":\"shared\"}}");
            }
            first.get(5, TimeUnit.SECONDS);
            second.get(5, TimeUnit.SECONDS);
            assertTrue(pair.client.hasSessionCallback("shared"));
            assertFalse(pair.client.hasSessionCallback("unrelated"));

            pair.client.sendResponse(101L, Map.of());
            assertEquals(101, MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream())).path("id").asInt());
            assertTrue(pair.client.hasSessionCallback("shared"));
            pair.client.sendErrorResponse(102L, -32603, "controlled callback failure");
            assertEquals(102, MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream())).path("id").asInt());
            assertFalse(pair.client.hasSessionCallback("shared"));

            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":103,\"method\":\"fixture.callback\",\"params\":{\"sessionId\":\"shared\"}}");
            third.get(5, TimeUnit.SECONDS);
            assertTrue(pair.client.hasSessionCallback("shared"));
            pair.client.close();
            assertFalse(pair.client.hasSessionCallback("shared"));
        }
    }

    @Test
    void testGeneratedProxyCaQueriesPreserveBooleanResponses() throws Exception {
        try (var pair = createSocketPair()) {
            var server = new ServerRpc(pair.client::invoke);
            for (boolean expected : new boolean[]{true, false}) {
                for (String method : new String[]{"sandbox.proxyCa.checkInstallationSupport",
                        "sandbox.proxyCa.checkRelevance"}) {
                    CompletableFuture<?> result = method.endsWith("checkInstallationSupport")
                            ? server.sandbox.proxyCa.checkInstallationSupport()
                            : server.sandbox.proxyCa.checkRelevance(
                                    SandboxProxyCaCheckRelevanceParams.builder().sandboxConfig(null).build());
                    JsonNode request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                    assertEquals(method, request.get("method").asText());
                    writeRpcMessage(pair.serverSide.getOutputStream(),
                            "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":" + expected + "}");
                    assertEquals(expected, result.get(5, TimeUnit.SECONDS));
                }
            }
        }
    }

    @Test
    void testNotify() throws Exception {
        try (var pair = createSocketPair()) {
            pair.client.notify("test.method", Map.of("key", "value"));

            String msg = readRpcMessage(pair.serverSide.getInputStream());
            JsonNode node = MAPPER.readTree(msg);
            assertEquals("2.0", node.get("jsonrpc").asText());
            assertEquals("test.method", node.get("method").asText());
            assertNull(node.get("id"), "Notification should not have an id");
            assertEquals("value", node.get("params").get("key").asText());
        }
    }

    @Test
    void testCredentialValuesAreRedactedOnlyFromDiagnosticRendering() throws Exception {
        String json = """
                {"jsonrpc":"2.0","result":{"accessToken":"secret","nested":{"gitHubToken":"static"},
                "negotiate":{"token":"spnego"},"basic":{"username":"user","password":"hunter2"}},
                "metadata":{"tokenType":"Bearer"}}
                """;

        String rendered = JsonRpcClient.redactCredentialsForLogging(json);

        assertFalse(rendered.contains("secret"));
        assertFalse(rendered.contains("static"));
        assertFalse(rendered.contains("spnego"));
        assertFalse(rendered.contains("hunter2"));
        assertEquals("<redacted>", MAPPER.readTree(rendered).at("/result/accessToken").asText());
        assertEquals("<redacted>", MAPPER.readTree(rendered).at("/result/nested/gitHubToken").asText());
        assertEquals("<redacted>", MAPPER.readTree(rendered).at("/result/negotiate/token").asText());
        assertEquals("<redacted>", MAPPER.readTree(rendered).at("/result/basic/password").asText());
        assertEquals("Bearer", MAPPER.readTree(rendered).at("/metadata/tokenType").asText());
    }

    @Test
    void testReceivedCredentialValuesAreRedactedFromDiagnostics() throws Exception {
        Logger logger = Logger.getLogger(JsonRpcClient.class.getName());
        Level previousLevel = logger.getLevel();
        var received = new CompletableFuture<String>();
        Handler handler = new Handler() {
            @Override
            public void publish(LogRecord record) {
                String message = record.getMessage();
                if (message != null && message.startsWith("Received: ")) {
                    received.complete(message);
                }
            }

            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }
        };
        handler.setLevel(Level.ALL);
        logger.addHandler(handler);
        logger.setLevel(Level.FINE);
        try (var pair = createSocketPair()) {
            writeRpcMessage(pair.serverSide.getOutputStream(), """
                    {"jsonrpc":"2.0","method":"test.notify","params":{"token":"inbound-secret"}}
                    """);

            String message = received.get(5, TimeUnit.SECONDS);
            assertEquals(List.of(false, "<redacted>"), List.of(message.contains("inbound-secret"),
                    MAPPER.readTree(message.substring("Received: ".length())).at("/params/token").asText()));
        } finally {
            logger.removeHandler(handler);
            logger.setLevel(previousLevel);
        }
    }

    @Test
    void testImageAnswersAreRedactedWithoutChangingWireResponseOrCompletion() throws Exception {
        Logger logger = Logger.getLogger(JsonRpcClient.class.getName());
        Logger dispatcherLogger = Logger.getLogger(RpcHandlerDispatcher.class.getName());
        Level previousLevel = logger.getLevel();
        Level previousDispatcherLevel = dispatcherLogger.getLevel();
        var sent = new CompletableFuture<String>();
        var received = new CompletableFuture<String>();
        var dispatched = new CompletableFuture<String>();
        var completion = new CompletableFuture<JsonNode>();
        String eventId = java.util.UUID.randomUUID().toString();
        Handler handler = new Handler() {
            @Override
            public void publish(LogRecord record) {
                String message = record.getMessage();
                if (message != null && message.startsWith("Sent: ")
                        && message.contains("\"id\":\"image-answer-diagnostics\"")) {
                    sent.complete(message.substring("Sent: ".length()));
                } else if (message != null && message.startsWith("Received: ")
                        && message.contains("\"sessionId\":\"image-answer-diagnostics\"")) {
                    received.complete(message.substring("Received: ".length()));
                } else if (message != null && message.startsWith("Received session.event: ")
                        && message.contains("\"id\":\"" + eventId + "\"")) {
                    dispatched.complete(message.substring("Received session.event: ".length()));
                }
            }

            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }
        };
        handler.setLevel(Level.ALL);
        logger.addHandler(handler);
        logger.setLevel(Level.FINE);
        dispatcherLogger.addHandler(handler);
        dispatcherLogger.setLevel(Level.FINE);
        String imageData = "c2NyZWVuc2hvdC1jb250ZW50";
        var image = Map.of("type", "image", "data", imageData, "mimeType", "image/png");
        var answer = Map.of("answer", "", "wasFreeform", true, "attachments", List.of(image));
        try (var pair = createSocketPair()) {
            pair.serverSide.setSoTimeout(5_000);
            var session = new CopilotSession("image-answer-diagnostics", pair.client);
            var sessions = Map.of(session.getSessionId(), session);
            new RpcHandlerDispatcher(sessions, event -> {
            }, null, new GitHubTokenProviderRegistry(), sessions.values()).registerHandlers(pair.client);
            session.on(event -> completion.complete(MAPPER.valueToTree(event)));
            pair.client.sendResponse("image-answer-diagnostics", answer);
            JsonNode response = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertEquals(imageData, response.at("/result/attachments/0/data").asText());
            ObjectNode completionData = MAPPER.valueToTree(answer);
            completionData.put("requestId", "image-answer-diagnostics");
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    MAPPER.writeValueAsString(Map.of("jsonrpc", "2.0", "method", "session.event", "params",
                            Map.of("sessionId", "image-answer-diagnostics", "event",
                                    Map.of("id", eventId, "type", "user_input.completed", "timestamp",
                                            "2026-01-01T00:00:00Z", "data", completionData)))));
            assertEquals(imageData, completion.get(5, TimeUnit.SECONDS).at("/data/attachments/0/data").asText());

            String sentLog = sent.get(5, TimeUnit.SECONDS);
            String receivedLog = received.get(5, TimeUnit.SECONDS);
            String dispatchedLog = dispatched.get(5, TimeUnit.SECONDS);
            for (String logged : List.of(sentLog, receivedLog, dispatchedLog)) {
                assertFalse(logged.contains(imageData));
            }
            JsonNode sentImage = MAPPER.readTree(sentLog).at("/result/attachments/0");
            JsonNode receivedImage = MAPPER.readTree(receivedLog).at("/params/event/data/attachments/0");
            JsonNode dispatchedImage = MAPPER.readTree(dispatchedLog).at("/data/attachments/0");
            for (JsonNode loggedImage : List.of(sentImage, receivedImage, dispatchedImage)) {
                assertEquals("<redacted>", loggedImage.path("data").asText());
                assertEquals("image", loggedImage.path("type").asText());
                assertEquals("image/png", loggedImage.path("mimeType").asText());
            }
            assertEquals(imageData, image.get("data"));
        } finally {
            logger.removeHandler(handler);
            logger.setLevel(previousLevel);
            dispatcherLogger.removeHandler(handler);
            dispatcherLogger.setLevel(previousDispatcherLevel);
        }
    }

    @Test
    void testHostEnvironmentIsRedactedWithoutChangingWireRequestOrResponse() throws Exception {
        Logger logger = Logger.getLogger(JsonRpcClient.class.getName());
        Level previousLevel = logger.getLevel();
        var sent = new CompletableFuture<String>();
        var received = new CompletableFuture<String>();
        Handler handler = new Handler() {
            @Override
            public void publish(LogRecord record) {
                String message = record.getMessage();
                if (message != null && message.startsWith("Sent: ")
                        && message.contains("\"method\":\"shell.buildInteractiveEnvironment\"")) {
                    sent.complete(message.substring("Sent: ".length()));
                } else if (message != null && message.startsWith("Received: ") && message.contains("\"environment\"")) {
                    received.complete(message.substring("Received: ".length()));
                }
            }

            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }
        };
        handler.setLevel(Level.ALL);
        logger.addHandler(handler);
        logger.setLevel(Level.FINE);
        var environment = Map.of("GITHUB_TOKEN", "fake-github-credential", "OPENAI_API_KEY", "fake-provider-credential",
                "CUSTOM_CREDENTIAL", "fake-custom-credential", "GitHub_Token", "fake-mixed-case-credential", "PATH",
                "/controlled/bin");
        var params = ShellBuildInteractiveEnvironmentParams.builder().hostEnvironment(environment)
                .additionalBlockedNames(List.of()).caseInsensitive(false).build();
        var returnedEnvironment = Map.of("GitHub_Token", "fake-mixed-case-credential", "CUSTOM_CREDENTIAL",
                "fake-custom-credential", "PATH", "/controlled/bin");
        try (var pair = createSocketPair()) {
            pair.serverSide.setSoTimeout(5_000);
            var rpc = new ServerRpc(pair.client::invoke);
            var result = rpc.shell.buildInteractiveEnvironment(params);
            JsonNode request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertEquals("shell.buildInteractiveEnvironment", request.path("method").asText());
            assertEquals(MAPPER.valueToTree(environment), request.at("/params/hostEnvironment"));
            writeRpcMessage(pair.serverSide.getOutputStream(), MAPPER.writeValueAsString(Map.of("jsonrpc", "2.0", "id",
                    request.path("id"), "result", Map.of("environment", returnedEnvironment))));
            var response = result.get(5, TimeUnit.SECONDS);
            assertEquals(returnedEnvironment, response.environment());

            String logged = sent.get(5, TimeUnit.SECONDS);
            assertEquals(MAPPER.valueToTree("<redacted>"), MAPPER.readTree(logged).at("/params/hostEnvironment"));
            for (String value : environment.values()) {
                assertFalse(logged.contains(value), "Host environment values must not appear in diagnostics");
                assertFalse(params.toString().contains(value),
                        "Host environment values must not appear in generated string representations");
            }
            String loggedResponse = received.get(5, TimeUnit.SECONDS);
            assertEquals(MAPPER.valueToTree("<redacted>"), MAPPER.readTree(loggedResponse).at("/result/environment"));
            for (String value : returnedEnvironment.values()) {
                assertFalse(loggedResponse.contains(value),
                        "Returned environment values must not appear in diagnostics");
                assertFalse(response.toString().contains(value),
                        "Returned environment values must not appear in generated string representations");
            }
            assertEquals(environment, params.hostEnvironment());
        } finally {
            logger.removeHandler(handler);
            logger.setLevel(previousLevel);
        }
    }

    @Test
    void testSecretProcessingRequestsAreRedactedWithoutChangingWireParams() throws Exception {
        var requests = new LinkedHashMap<String, Object>();
        requests.put("secrets.filter", Map.of("text", "opaque-sensitive-filter"));
        requests.put("secrets.removeFilterValues", Map.of("values", List.of("opaque-sensitive-remove")));
        requests.put("logging.file.write",
                Map.of("logId", "fixture-log", "operation", "log", "message", "opaque-sensitive-write"));
        requests.put("logging.file.open", Map.of("secretValues", List.of("opaque-sensitive-open")));
        requests.put("mcp.secrets.save", Map.of("secretId", "server/key", "value", "opaque-sensitive-mcp"));
        requests.put("sessions.import", Map.of("jsonl", "{\"token\":\"opaque-sensitive-session-import\"}\n"));
        requests.put("memories.import", Map.of("jsonl", "{\"token\":\"opaque-sensitive-memory-import\"}\n"));
        var sent = new LinkedHashMap<String, CompletableFuture<String>>();
        requests.keySet().forEach(method -> sent.put(method, new CompletableFuture<>()));
        Thread sender = Thread.currentThread();
        Logger logger = Logger.getLogger(JsonRpcClient.class.getName());
        Level previousLevel = logger.getLevel();
        Handler handler = new Handler() {
            @Override
            public void publish(LogRecord record) {
                String message = record.getMessage();
                if (Thread.currentThread() == sender && message != null && message.startsWith("Sent: ")) {
                    sent.forEach((method, diagnostic) -> {
                        if (message.contains("\"method\":\"" + method + "\"")) {
                            diagnostic.complete(message.substring("Sent: ".length()));
                        }
                    });
                }
            }

            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }
        };
        handler.setLevel(Level.ALL);
        logger.addHandler(handler);
        logger.setLevel(Level.FINE);
        try (var pair = createSocketPair()) {
            pair.serverSide.setSoTimeout(5_000);
            for (var entry : requests.entrySet()) {
                var result = pair.client.invoke(entry.getKey(), entry.getValue(), Void.class);
                JsonNode request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
                assertEquals(entry.getKey(), request.path("method").asText());
                assertEquals(MAPPER.valueToTree(entry.getValue()), request.path("params"));
                writeRpcMessage(pair.serverSide.getOutputStream(),
                        "{\"jsonrpc\":\"2.0\",\"id\":" + request.path("id") + ",\"result\":null}");
                result.get(5, TimeUnit.SECONDS);

                String diagnostic = sent.get(entry.getKey()).get(5, TimeUnit.SECONDS);
                assertFalse(diagnostic.contains("opaque-sensitive-"),
                        entry.getKey() + " must not disclose its payload in diagnostics");
                JsonNode rendered = MAPPER.readTree(diagnostic);
                assertEquals("<redacted>", rendered.path("params").asText());
                assertEquals(request.path("method"), rendered.path("method"));
                assertEquals(request.path("id"), rendered.path("id"));
            }
        } finally {
            logger.removeHandler(handler);
            logger.setLevel(previousLevel);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testSecretFilterResponsesAreRedactedWithoutChangingWireValues(boolean cancelBeforeReply) throws Exception {
        String sensitive = "opaque-sensitive-filter-response";
        var received = new LinkedBlockingQueue<String>();
        Logger logger = Logger.getLogger(JsonRpcClient.class.getName());
        Level previousLevel = logger.getLevel();
        Handler handler = new Handler() {
            @Override
            public void publish(LogRecord record) {
                String message = record.getMessage();
                if (message != null && message.startsWith("Received: ")) {
                    received.add(message.substring("Received: ".length()));
                }
            }

            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }
        };
        handler.setLevel(Level.ALL);
        logger.addHandler(handler);
        logger.setLevel(Level.FINE);
        try (var pair = createSocketPair()) {
            pair.serverSide.setSoTimeout(5_000);
            var filtered = pair.client.invoke("secrets.filter", Map.of("text", sensitive), JsonNode.class);
            JsonNode filterRequest = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            var ordinary = pair.client.invoke("fixture.echo", Map.of(), JsonNode.class);
            JsonNode ordinaryRequest = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            writeRpcMessage(pair.serverSide.getOutputStream(), "{\"jsonrpc\":\"2.0\",\"id\":"
                    + ordinaryRequest.path("id") + ",\"result\":{\"value\":\"visible-response\"}}");
            assertEquals("visible-response", ordinary.get(5, TimeUnit.SECONDS).path("value").asText());
            String ordinaryDiagnostic = received.poll(5, TimeUnit.SECONDS);
            assertNotNull(ordinaryDiagnostic);
            assertEquals("visible-response", MAPPER.readTree(ordinaryDiagnostic).at("/result/value").asText());
            if (cancelBeforeReply) {
                assertTrue(filtered.cancel(false));
            }
            writeRpcMessage(pair.serverSide.getOutputStream(), "{\"jsonrpc\":\"2.0\",\"id\":" + filterRequest.path("id")
                    + ",\"result\":{\"value\":\"" + sensitive + "\",\"changed\":false}}");
            String diagnostic = received.poll(5, TimeUnit.SECONDS);
            assertNotNull(diagnostic);
            assertFalse(diagnostic.contains(sensitive), "Filter responses must not disclose unchanged input");
            JsonNode rendered = MAPPER.readTree(diagnostic);
            assertEquals(filterRequest.path("id"), rendered.path("id"));
            if (cancelBeforeReply) {
                assertEquals("<redacted>", rendered.path("result").asText());
                assertTrue(filtered.isCancelled());
            } else {
                assertEquals("<redacted>", rendered.at("/result/value").asText());
                assertFalse(rendered.at("/result/changed").asBoolean());
                JsonNode result = filtered.get(5, TimeUnit.SECONDS);
                assertEquals(sensitive, result.path("value").asText());
                assertFalse(result.path("changed").asBoolean());
            }
        } finally {
            logger.removeHandler(handler);
            logger.setLevel(previousLevel);
        }
    }

    @ParameterizedTest
    @CsvSource({"sessions.import,false,false", "sessions.import,false,true", "sessions.import,true,false",
            "sessions.import,true,true", "memories.import,false,false", "memories.import,false,true",
            "memories.import,true,false", "memories.import,true,true"})
    void testImportResponsesAreRedactedWithoutChangingWireValues(String method, boolean cancelBeforeReply,
            boolean errorResponse) throws Exception {
        String sensitive = "opaque-sensitive-import-response";
        var received = new LinkedBlockingQueue<String>();
        var diagnostics = new LinkedBlockingQueue<LogRecord>();
        Logger logger = Logger.getLogger(JsonRpcClient.class.getName());
        Level previousLevel = logger.getLevel();
        Handler handler = new Handler() {
            @Override
            public void publish(LogRecord record) {
                diagnostics.add(record);
                String message = record.getMessage();
                if (message != null && message.startsWith("Received: ")) {
                    received.add(message.substring("Received: ".length()));
                }
            }

            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }
        };
        handler.setLevel(Level.ALL);
        logger.addHandler(handler);
        logger.setLevel(Level.FINE);
        try (var pair = createSocketPair()) {
            pair.serverSide.setSoTimeout(5_000);
            var imported = pair.client.invoke(method, Map.of("jsonl", "{}\n"), JsonNode.class);
            JsonNode request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            var ordinary = pair.client.invoke("fixture.echo", Map.of(), JsonNode.class);
            JsonNode ordinaryRequest = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            writeRpcMessage(pair.serverSide.getOutputStream(), "{\"jsonrpc\":\"2.0\",\"id\":"
                    + ordinaryRequest.path("id") + ",\"result\":{\"value\":\"visible-response\"}}");
            assertEquals("visible-response", ordinary.get(5, TimeUnit.SECONDS).path("value").asText());
            String ordinaryDiagnostic = received.poll(5, TimeUnit.SECONDS);
            assertNotNull(ordinaryDiagnostic);
            assertEquals("visible-response", MAPPER.readTree(ordinaryDiagnostic).at("/result/value").asText());
            if (cancelBeforeReply) {
                assertTrue(imported.cancel(false));
            }
            String payloadKey = errorResponse ? "error" : "result";
            var payload = errorResponse
                    ? Map.of("code", -32602, "message", sensitive, "data", Map.of("externalId", sensitive))
                    : Map.of("diagnostics", List.of(Map.of("message", sensitive)), "externalId", sensitive);
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    MAPPER.writeValueAsString(Map.of("jsonrpc", "2.0", "id", request.path("id"), payloadKey, payload)));
            String diagnostic = received.poll(5, TimeUnit.SECONDS);
            assertNotNull(diagnostic);
            assertFalse(diagnostic.contains(sensitive), "Import replies must not disclose input or external IDs");
            JsonNode rendered = MAPPER.readTree(diagnostic);
            assertEquals(request.path("id"), rendered.path("id"));
            assertEquals("<redacted>", rendered.path(payloadKey).asText());
            if (cancelBeforeReply) {
                assertTrue(imported.isCancelled());
            } else if (errorResponse) {
                var error = assertThrows(ExecutionException.class, () -> imported.get(5, TimeUnit.SECONDS));
                assertTrue(error.getCause().getMessage().contains(sensitive));
            } else {
                assertEquals(MAPPER.valueToTree(payload), imported.get(5, TimeUnit.SECONDS));
            }
            for (LogRecord record : diagnostics) {
                assertFalse(record.getMessage().contains(sensitive), "Import payloads must not appear in logs");
                if (record.getThrown() != null) {
                    assertFalse(record.getThrown().toString().contains(sensitive),
                            "Import errors must not expose payloads through logged exceptions");
                }
            }
        } finally {
            logger.removeHandler(handler);
            logger.setLevel(previousLevel);
        }
    }

    // ---- isConnected() ----

    @Test
    void testIsConnectedWithSocket() throws Exception {
        try (var pair = createSocketPair()) {
            assertTrue(pair.client.isConnected());
        }
    }

    @Test
    void testIsConnectedWithSocketClosed() throws Exception {
        var pair = createSocketPair();
        pair.client.close();
        assertFalse(pair.client.isConnected());
        pair.serverSide.close();
        pair.serverSocket.close();
    }

    @Test
    void testIsConnectedWithProcess() throws Exception {
        Process proc = new TestProcess();
        try (var client = JsonRpcClient.fromProcess(proc)) {
            assertTrue(client.isConnected());
        }
    }

    @Test
    void testIsConnectedWithProcessDead() throws Exception {
        Process proc = new TestProcess();
        var client = JsonRpcClient.fromProcess(proc);
        proc.destroy();
        proc.waitFor(5, TimeUnit.SECONDS);
        assertFalse(client.isConnected());
        client.close();
    }

    // ---- getProcess() ----

    @Test
    void testGetProcessReturnsProcess() throws Exception {
        Process proc = new TestProcess();
        try (var client = JsonRpcClient.fromProcess(proc)) {
            assertSame(proc, client.getProcess());
        }
    }

    @Test
    void testGetProcessNullForSocket() throws Exception {
        try (var pair = createSocketPair()) {
            assertNull(pair.client.getProcess());
        }
    }

    @Test
    void testCloseHandlerRunsOnceOnRemoteAndExplicitClose() throws Exception {
        try (var pair = createSocketPair()) {
            var closeCount = new AtomicInteger();
            var closed = new CompletableFuture<Void>();
            pair.client.setCloseHandler(() -> {
                closeCount.incrementAndGet();
                closed.complete(null);
            });

            pair.serverSide.close();
            closed.get(5, TimeUnit.SECONDS);
            pair.client.close();

            assertEquals(1, closeCount.get());
        }
    }

    @Test
    void testCloseHandlerRunsWhenRegisteredAfterClose() throws Exception {
        try (var pair = createSocketPair()) {
            pair.client.close();
            var closed = new CompletableFuture<Void>();

            pair.client.setCloseHandler(() -> closed.complete(null));

            closed.get(5, TimeUnit.SECONDS);
        }
    }

    @Test
    void testInvokeFailsAfterRemoteClose() throws Exception {
        try (var pair = createSocketPair()) {
            var closed = new CompletableFuture<Void>();
            pair.client.setCloseHandler(() -> closed.complete(null));

            pair.serverSide.close();
            closed.get(5, TimeUnit.SECONDS);

            var future = pair.client.invoke("test", Map.of(), JsonNode.class);
            var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
            assertInstanceOf(IOException.class, ex.getCause());
        }
    }

    @Test
    void testPendingInvokeFailsBeforeRemoteCloseHandlerRuns() throws Exception {
        try (var pair = createSocketPair()) {
            var future = pair.client.invoke("test", Map.of(), JsonNode.class);
            readRpcMessage(pair.serverSide.getInputStream());

            var closeHandlerObservedFailure = new CompletableFuture<Boolean>();
            pair.client.setCloseHandler(() -> closeHandlerObservedFailure.complete(future.isCompletedExceptionally()));

            pair.serverSide.close();

            assertTrue(closeHandlerObservedFailure.get(5, TimeUnit.SECONDS));
            var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
            assertInstanceOf(IOException.class, ex.getCause());
        }
    }

    // ---- invoke() edge cases ----

    @Test
    void testTextResponseBeyondDefaultJacksonLimit() throws Exception {
        try (var pair = createSocketPair()) {
            var future = pair.client.invoke("fetch.send", Map.of(), JsonNode.class);
            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            var text = "x".repeat(20_000_001);
            writeRpcMessage(pair.serverSide.getOutputStream(), "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id")
                    + ",\"result\":{\"responseMode\":\"text\",\"bodyText\":\"" + text + "\"}}");

            assertEquals(text, future.get(5, TimeUnit.SECONDS).path("bodyText").asText());
            assertReaderResponsive(pair);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    @AllowCopilotExperimental
    void testAbandonedRevisionReadCancelsItsTransportRequest(boolean timeout) throws Exception {
        try (var pair = createSocketPair()) {
            pair.serverSide.setSoTimeout(2000);
            var session = new SessionRpc(pair.client::invoke, "feature-flags-session");
            var pending = session.featureFlags.read(SessionFeatureFlagsReadParams.builder().afterRevision(7L).build());
            var request = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));

            if (timeout) {
                var failure = assertThrows(ExecutionException.class,
                        () -> pending.orTimeout(0, TimeUnit.MILLISECONDS).get(5, TimeUnit.SECONDS));
                assertInstanceOf(TimeoutException.class, failure.getCause());
            } else {
                assertTrue(pending.cancel(false));
            }

            var cancellation = MAPPER.readTree(readRpcMessage(pair.serverSide.getInputStream()));
            assertEquals("$/cancelRequest", cancellation.get("method").asText());
            assertEquals(request.get("id"), cancellation.at("/params/id"));
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + request.get("id") + ",\"result\":{}}");
            assertReaderResponsive(pair);
        }
    }

    @Test
    void testInvokeWithVoidPrimitive() throws Exception {
        try (var pair = createSocketPair()) {
            CompletableFuture<Void> future = pair.client.invoke("test", Map.of(), void.class);

            String request = readRpcMessage(pair.serverSide.getInputStream());
            long id = MAPPER.readTree(request).get("id").asLong();

            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"result\":{\"any\":\"thing\"}}");

            assertNull(future.get(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void testInvokeWithSendFailure() throws Exception {
        var loopback = InetAddress.getLoopbackAddress();
        var serverSocket = new ServerSocket(0, 1, loopback);
        var clientSocket = new Socket(loopback, serverSocket.getLocalPort());
        var serverSide = serverSocket.accept();
        var client = JsonRpcClient.fromSocket(clientSocket);

        // Close the client socket so write will fail
        clientSocket.close();

        CompletableFuture<?> future = client.invoke("test", Map.of(), JsonNode.class);

        var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
        assertInstanceOf(IOException.class, ex.getCause());
        client.close();
        serverSide.close();
        serverSocket.close();
    }

    @Test
    void testInvokeWithDeserializationError() throws Exception {
        try (var pair = createSocketPair()) {
            // Integer cannot be deserialized from a JSON object
            CompletableFuture<Integer> future = pair.client.invoke("test", Map.of(), Integer.class);

            String request = readRpcMessage(pair.serverSide.getInputStream());
            long id = MAPPER.readTree(request).get("id").asLong();

            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"result\":{\"complex\":\"object\"}}");

            var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
            // CompletableFuture unwraps CompletionException, so cause is the
            // underlying JsonProcessingException
            assertInstanceOf(com.fasterxml.jackson.core.JsonProcessingException.class, ex.getCause());
        }
    }

    // ---- handleMessage: response handling ----

    @Test
    void testResponseWithUnknownId() throws Exception {
        try (var pair = createSocketPair()) {
            // Response for an id that has no pending request - silently ignored
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":99999,\"result\":{\"ok\":true}}");

            assertReaderResponsive(pair);
            // No exception, just silently dropped
        }
    }

    @Test
    void testErrorResponseWithoutMessage() throws Exception {
        try (var pair = createSocketPair()) {
            CompletableFuture<?> future = pair.client.invoke("test", Map.of(), JsonNode.class);

            String request = readRpcMessage(pair.serverSide.getInputStream());
            long id = MAPPER.readTree(request).get("id").asLong();

            // Error with code but no message field
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"error\":{\"code\":-32600}}");

            var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
            var rpcEx = assertInstanceOf(JsonRpcException.class, ex.getCause());
            assertEquals("Unknown error", rpcEx.getMessage());
            assertEquals(-32600, rpcEx.getCode());
        }
    }

    @Test
    void testErrorResponseWithoutCode() throws Exception {
        try (var pair = createSocketPair()) {
            CompletableFuture<?> future = pair.client.invoke("test", Map.of(), JsonNode.class);

            String request = readRpcMessage(pair.serverSide.getInputStream());
            long id = MAPPER.readTree(request).get("id").asLong();

            // Error with message but no code field
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"error\":{\"message\":\"bad request\"}}");

            var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
            var rpcEx = assertInstanceOf(JsonRpcException.class, ex.getCause());
            assertEquals("bad request", rpcEx.getMessage());
            assertEquals(-1, rpcEx.getCode());
        }
    }

    // ---- handleMessage: server method calls ----

    @Test
    void testNoHandlerForNotification() throws Exception {
        try (var pair = createSocketPair()) {
            // Notification (no id) for unregistered method -- silently logged
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"method\":\"unknown.method\",\"params\":{}}");

            assertReaderResponsive(pair);
        }
    }

    @Test
    void testNoHandlerForRequestSendsErrorResponse() throws Exception {
        try (var pair = createSocketPair()) {
            // Request (with id) for unregistered method -> -32601 Method not found
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"unknown.method\",\"params\":{}}");

            String response = readRpcMessage(pair.serverSide.getInputStream());
            JsonNode node = MAPPER.readTree(response);
            assertEquals("2.0", node.get("jsonrpc").asText());
            assertTrue(node.has("error"));
            assertEquals(-32601, node.get("error").get("code").asInt());
            assertTrue(node.get("error").get("message").asText().contains("Method not found"));
        }
    }

    @Test
    void testHandlerThrowsExceptionWithId() throws Exception {
        try (var pair = createSocketPair()) {
            pair.client.registerMethodHandler("fail.method", (id, params) -> {
                throw new RuntimeException("handler error");
            });

            // Request with id - handler throws -> -32603 Internal error
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"fail.method\",\"params\":{}}");

            String response = readRpcMessage(pair.serverSide.getInputStream());
            JsonNode node = MAPPER.readTree(response);
            assertTrue(node.has("error"));
            assertEquals(-32603, node.get("error").get("code").asInt());
            assertEquals("handler error", node.get("error").get("message").asText());
        }
    }

    @Test
    void testHandlerThrowsExceptionWithoutId() throws Exception {
        try (var pair = createSocketPair()) {
            var handled = new CompletableFuture<Void>();
            pair.client.registerMethodHandler("fail.notify", (id, params) -> {
                handled.complete(null);
                throw new RuntimeException("notify error");
            });

            // Notification (no id) - handler throws -> just logged, no error response
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"method\":\"fail.notify\",\"params\":{}}");

            handled.get(5, TimeUnit.SECONDS);
            assertReaderResponsive(pair);
        }
    }

    @Test
    void testMethodCallWithNullId() throws Exception {
        try (var pair = createSocketPair()) {
            var received = new CompletableFuture<String>();
            pair.client.registerMethodHandler("test.null.id", (id, params) -> {
                received.complete(id);
            });

            // Explicit null id - should be treated as notification
            writeRpcMessage(pair.serverSide.getOutputStream(),
                    "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"test.null.id\",\"params\":{}}");

            assertNull(received.get(5, TimeUnit.SECONDS));
        }
    }

    // ---- handleMessage: edge cases ----

    @Test
    void testInvalidJson() throws Exception {
        try (var pair = createSocketPair()) {
            var first = pair.client.invoke("fetch.send", Map.of(), JsonNode.class);
            readRpcMessage(pair.serverSide.getInputStream());
            var second = pair.client.invoke("fetch.create", Map.of(), JsonNode.class);
            readRpcMessage(pair.serverSide.getInputStream());

            writeRpcMessage(pair.serverSide.getOutputStream(), "not valid json {{{");

            for (var pending : List.of(first, second)) {
                var failure = assertThrows(ExecutionException.class, () -> pending.get(5, TimeUnit.SECONDS));
                assertInstanceOf(JsonProcessingException.class, failure.getCause());
            }
            assertReaderResponsive(pair);
        }
    }

    @Test
    void testMessageWithNeitherResponseNorMethod() throws Exception {
        try (var pair = createSocketPair()) {
            // JSON object with id but no result/error/method - silently ignored
            writeRpcMessage(pair.serverSide.getOutputStream(), "{\"jsonrpc\":\"2.0\",\"id\":1}");

            assertReaderResponsive(pair);
        }
    }

    // ---- reader: header parsing edge cases ----

    @Test
    void testReaderWithUnknownHeader() throws Exception {
        try (var pair = createSocketPair()) {
            var received = new CompletableFuture<JsonNode>();
            pair.client.registerMethodHandler("test.header", (id, params) -> {
                received.complete(params);
            });

            // Send a message with an extra header before Content-Length
            var out = pair.serverSide.getOutputStream();
            String json = "{\"jsonrpc\":\"2.0\",\"method\":\"test.header\",\"params\":{\"ok\":true}}";
            byte[] content = json.getBytes(StandardCharsets.UTF_8);
            String msg = "X-Custom-Header: value\r\nContent-Length: " + content.length + "\r\n\r\n";
            out.write(msg.getBytes(StandardCharsets.UTF_8));
            out.write(content);
            out.flush();

            JsonNode params = received.get(5, TimeUnit.SECONDS);
            assertTrue(params.get("ok").asBoolean());
        }
    }

    @Test
    void testReaderWithMissingContentLength() throws Exception {
        try (var pair = createSocketPair()) {
            var received = new CompletableFuture<JsonNode>();
            pair.client.registerMethodHandler("test.after", (id, params) -> {
                received.complete(params);
            });

            var out = pair.serverSide.getOutputStream();

            // First: send a message with no Content-Length header (just blank line) -
            // should skip
            out.write("X-Only-Header: no-length\r\n\r\n".getBytes(StandardCharsets.UTF_8));
            out.flush();

            // Then: send a proper message that should be received
            String json = "{\"jsonrpc\":\"2.0\",\"method\":\"test.after\",\"params\":{\"ok\":true}}";
            byte[] content = json.getBytes(StandardCharsets.UTF_8);
            String proper = "Content-Length: " + content.length + "\r\n\r\n";
            out.write(proper.getBytes(StandardCharsets.UTF_8));
            out.write(content);
            out.flush();

            JsonNode params = received.get(5, TimeUnit.SECONDS);
            assertTrue(params.get("ok").asBoolean());
        }
    }

    // ---- close() ----

    @Test
    void testCloseWithPendingRequests() throws Exception {
        var pair = createSocketPair();
        CompletableFuture<?> future = pair.client.invoke("test", Map.of(), JsonNode.class);
        // Read the outgoing request to avoid blocking
        readRpcMessage(pair.serverSide.getInputStream());

        // Close without responding - should cancel pending request
        pair.client.close();

        var ex = assertThrows(ExecutionException.class, () -> future.get(5, TimeUnit.SECONDS));
        assertInstanceOf(IOException.class, ex.getCause());

        pair.serverSide.close();
        pair.serverSocket.close();
    }
}
