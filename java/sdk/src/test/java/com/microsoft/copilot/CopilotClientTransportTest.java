/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.ClientObservabilityOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.Channels;
import java.nio.channels.Pipe;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.ffi.FfiRuntimeHost;
import com.microsoft.copilot.generated.rpc.EntraAuthConfig;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.InProcessRuntimeConnection;
import com.microsoft.copilot.rpc.OwnedRuntimeConnection;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.SpawnedStdioRuntimeConnection;
import com.microsoft.copilot.rpc.SpawnedTcpRuntimeConnection;
import com.microsoft.copilot.rpc.TelemetryConfig;
import com.microsoft.copilot.rpc.TcpRuntimeConnection;

/**
 * Unit tests for transport selection through {@link RuntimeConnection}: the
 * in-process code path, {@code COPILOT_SDK_DEFAULT_CONNECTION} resolution, the
 * separation of process configuration from runtime startup options, and
 * protocol-version admission.
 */
@AllowCopilotExperimental
class CopilotClientTransportTest {

    @ParameterizedTest
    @ValueSource(strings = {"", "static"})
    void githubCredentialsAreMutuallyExclusiveByPresence(String token) throws Exception {
        try (var runtime = new FakeInProcessRuntime();
                var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                        .setRuntime(RuntimeConnection.forInProcess()))) {
            client.setInProcessTransportFactory(runtime::open);
            var github = new com.microsoft.copilot.rpc.SessionGitHubOptions().setToken(token)
                    .setTokenProvider(args -> CompletableFuture
                            .completedFuture(com.microsoft.copilot.rpc.GitHubTokenProviderResult.cancelled()));
            for (boolean resume : List.of(false, true)) {
                var failure = assertThrows(java.util.concurrent.ExecutionException.class, () -> {
                    if (resume)
                        client.resumeSession("conflicting",
                                new ResumeSessionOptions().setGitHub(github).setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(10, TimeUnit.SECONDS);
                    else
                        client.createSession(new CreateSessionOptions().setGitHub(github).setPermissions(
                                new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(10, TimeUnit.SECONDS);
                });
                assertTrue(rootMessage(failure).contains("mutually exclusive"), rootMessage(failure));
                assertFalse(runtime.opened.get());
            }
        }
    }

    @ParameterizedTest
    @ValueSource(ints = {3, 5})
    void incompatibleProtocolRejectsCreateAndResumeBeforeAdmission(int version) throws Exception {
        for (boolean resume : List.of(false, true)) {
            try (var runtime = new FakeInProcessRuntime();
                    var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                            .setRuntime(RuntimeConnection.forInProcess()))) {
                runtime.protocolVersion = version;
                client.setInProcessTransportFactory(runtime::open);
                var failure = assertThrows(java.util.concurrent.ExecutionException.class, () -> {
                    if (resume)
                        client.resumeSession("incompatible",
                                new ResumeSessionOptions().setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(10, TimeUnit.SECONDS);
                    else
                        client.createSession(new CreateSessionOptions().setPermissions(
                                new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(10, TimeUnit.SECONDS);
                });
                assertTrue(rootMessage(failure).contains("version"), rootMessage(failure));
                assertTrue(rootMessage(failure).contains(Integer.toString(version)), rootMessage(failure));
                assertFalse(runtime.sessionRequested.get());
            }
        }
    }

    @Test
    void defaultFactoryResolvesEnvironmentWithoutStartingRuntime() throws Exception {
        for (String value : new String[]{null, "", "StDiO", "InProcess", "unsupported"}) {
            var builder = new ProcessBuilder(
                    java.nio.file.Path.of(System.getProperty("java.home"), "bin", "java").toString(), "-cp",
                    System.getProperty("surefire.test.class.path", System.getProperty("java.class.path"))
                            + java.io.File.pathSeparator + System.getProperty("jdk.module.path", "")
                            + java.io.File.pathSeparator
                            + java.nio.file.Path.of(RuntimeConnection.class.getProtectionDomain().getCodeSource()
                                    .getLocation().toURI()),
                    DefaultRuntimeProbe.class.getName()).redirectErrorStream(true);
            if (value == null)
                builder.environment().remove("COPILOT_SDK_DEFAULT_CONNECTION");
            else
                builder.environment().put("COPILOT_SDK_DEFAULT_CONNECTION", value);
            var process = builder.start();
            try {
                assertTrue(process.waitFor(20, TimeUnit.SECONDS), "Default runtime factory must not start or connect");
                String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
                assertEquals(0, process.exitValue(), output);
                assertTrue(output.contains("unsupported".equals(value)
                        ? "invalid"
                        : "StDiO".equals(value) ? "SpawnedStdioRuntimeConnection" : "InProcessRuntimeConnection"),
                        output);
            } finally {
                process.destroyForcibly();
            }
        }
    }

    public static final class DefaultRuntimeProbe {
        public static void main(String[] args) {
            try {
                var runtime = RuntimeConnection.forDefault("owned-home", 0);
                if (!"owned-home".equals(runtime.getBaseDirectory()) || runtime.getSessionIdleTimeoutSeconds() != 0)
                    throw new AssertionError("Owned settings were not retained");
                var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
                try (var client = new CopilotClient(options)) {
                    if (options.getRuntime() != null)
                        throw new AssertionError("Default selection must not rewrite caller options");
                    if (client.getRuntimeConnection().getClass() != runtime.getClass())
                        throw new AssertionError("Client and factory selected different defaults");
                }
                System.out.println(runtime.getClass().getSimpleName());
            } catch (IllegalArgumentException error) {
                if (!error.getMessage().contains("COPILOT_SDK_DEFAULT_CONNECTION"))
                    throw error;
                System.out.println("invalid");
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void legacyPingFallbackRequiresNoEntraConfiguration(boolean entra) throws Exception {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forInProcess());
        if (entra) {
            options.setEntra(EntraAuthConfig.builder().clientId("host-entra-client").build());
        }
        try (var runtime = new FakeInProcessRuntime(); var client = new CopilotClient(options)) {
            runtime.rejectConnect = true;
            client.setInProcessTransportFactory(runtime::open);
            if (entra) {
                var failure = assertThrows(java.util.concurrent.ExecutionException.class,
                        () -> client.start().get(10, TimeUnit.SECONDS));
                assertTrue(failure.getCause().getMessage().contains("does not support Entra client configuration"));
                assertFalse(runtime.pingReceived.get());
            } else {
                client.start().get(10, TimeUnit.SECONDS);
                assertTrue(runtime.pingReceived.get());
            }
        }
    }

    // ===== In-process routing =====

    @Test
    void inProcessConnectionStartsThroughInProcessRuntimeHost() throws Exception {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forInProcess());
        try (var runtime = new FakeInProcessRuntime(); var client = new CopilotClient(options)) {
            client.setInProcessTransportFactory(runtime::open);

            client.start().get(30, TimeUnit.SECONDS);

            assertTrue(runtime.opened.get(), "The in-process runtime must be used for an in-process connection");
            assertInstanceOf(InProcessRuntimeConnection.class, client.getRuntimeConnection());
            assertFalse(runtime.connectionTokenReceived.get(),
                    "In-process connections must not send a TCP authentication token");

            client.stop().get(30, TimeUnit.SECONDS);
            assertTrue(runtime.closed.get(), "Stopping the client must close the in-process runtime host");
        }
    }

    @Test
    void inProcessStartupFailurePropagates() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forInProcess());
        try (var client = new CopilotClient(options)) {
            client.setInProcessTransportFactory(opts -> {
                throw new IOException("no runtime available");
            });
            var failure = assertThrows(Exception.class, () -> client.start().get(30, TimeUnit.SECONDS));
            assertTrue(rootMessage(failure).contains("no runtime available"));
        }
    }

    @ParameterizedTest
    @ValueSource(longs = {0, 42, 0xffff_ffffL})
    void inProcessUiThreadRequestWaitsForScheduler(long actionId) throws Exception {
        var nativeHost = mock(FfiRuntimeHost.class);
        var started = new CountDownLatch(1);
        var completion = new CompletableFuture<Void>();
        var connection = RuntimeConnection.forInProcess().setRunOnUiThread(action -> {
            action.run();
            started.countDown();
            return completion;
        });
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(connection);
        try (var runtime = new FakeInProcessRuntime(); var client = new CopilotClient(options)) {
            client.setInProcessTransportFactory(opts -> {
                var transport = runtime.open(opts);
                return new CopilotClient.InProcessTransport(transport.receiveStream(), transport.sendStream(),
                        nativeHost);
            });
            client.start().get(30, TimeUnit.SECONDS);
            verify(nativeHost).configureUiThread(true);

            var request = FakeInProcessRuntime.MAPPER.createObjectNode();
            request.put("jsonrpc", "2.0");
            request.put("id", 17);
            request.put("method", "uiThread.run");
            request.putObject("params").put("actionId", actionId);
            FakeInProcessRuntime.writeMessage(runtime.runtimeOutput, request);
            try {
                assertTrue(started.await(5, TimeUnit.SECONDS), "The UI scheduler was not called");
                verify(nativeHost).runUiAction((int) actionId);
                assertFalse(runtime.callbackResponse.isDone(), "UI RPC completed before its scheduler");
            } finally {
                completion.complete(null);
            }
            var response = runtime.callbackResponse.get(5, TimeUnit.SECONDS);
            assertEquals(17, response.path("id").asInt());
            assertTrue(response.has("result") && response.get("result").isNull(), response.toString());
        }
    }

    @ParameterizedTest
    @ValueSource(longs = {-1, 0x1_0000_0000L})
    void inProcessUiThreadRequestRejectsOutOfRangeActionIds(long actionId) throws Exception {
        var nativeHost = mock(FfiRuntimeHost.class);
        var scheduled = new AtomicBoolean();
        var connection = RuntimeConnection.forInProcess().setRunOnUiThread(action -> {
            scheduled.set(true);
            action.run();
            return CompletableFuture.completedFuture(null);
        });
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(connection);
        try (var runtime = new FakeInProcessRuntime(); var client = new CopilotClient(options)) {
            client.setInProcessTransportFactory(opts -> {
                var transport = runtime.open(opts);
                return new CopilotClient.InProcessTransport(transport.receiveStream(), transport.sendStream(),
                        nativeHost);
            });
            client.start().get(30, TimeUnit.SECONDS);

            var request = FakeInProcessRuntime.MAPPER.createObjectNode();
            request.put("jsonrpc", "2.0");
            request.put("id", 17);
            request.put("method", "uiThread.run");
            request.putObject("params").put("actionId", actionId);
            FakeInProcessRuntime.writeMessage(runtime.runtimeOutput, request);
            var response = runtime.callbackResponse.get(5, TimeUnit.SECONDS);
            assertEquals(17, response.path("id").asInt());
            assertEquals(-32603, response.path("error").path("code").asInt(), response.toString());
            assertFalse(scheduled.get(), "Invalid action ID reached the UI scheduler");
            verify(nativeHost, never()).runUiAction(anyInt());
        }
    }

    @Test
    void cliTransportDoesNotUseTheInProcessRuntime() throws Exception {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("127.0.0.1", 1));
        try (var client = new CopilotClient(options)) {
            client.setInProcessTransportFactory(opts -> {
                throw new AssertionError("The in-process runtime must not be used for a CLI transport");
            });

            assertThrows(Exception.class, () -> client.start().get(30, TimeUnit.SECONDS));
            assertInstanceOf(TcpRuntimeConnection.class, client.getRuntimeConnection());
        }
    }

    @Test
    void stdioOptionsSelectStdioConnection() {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio("/usr/local/bin/copilot")))) {
            var connection = assertInstanceOf(SpawnedStdioRuntimeConnection.class, client.getRuntimeConnection());
            assertEquals("/usr/local/bin/copilot", connection.getPath());
        }
    }

    @Test
    void processOptionsAreOnlyExposedOnSpawnedConnections() {
        var processMethods = List.of("getCliPath", "setCliPath", "getCliArgs", "setCliArgs", "getCwd", "setCwd",
                "getPath", "setPath", "getArguments", "setArguments", "getEnvironment", "setEnvironment",
                "getWorkingDirectory", "setWorkingDirectory");
        for (var type : List.of(CopilotClientOptions.class, RuntimeConnection.class, InProcessRuntimeConnection.class,
                TcpRuntimeConnection.class)) {
            for (var method : type.getMethods()) {
                assertFalse(processMethods.contains(method.getName()),
                        type.getSimpleName() + " must not expose " + method.getName());
            }
        }
        var legacySelectors = List.of("getPort", "setPort", "isUseStdio", "hasUseStdio", "setUseStdio",
                "getTcpConnectionToken", "setTcpConnectionToken");
        for (var method : CopilotClientOptions.class.getMethods()) {
            assertFalse(legacySelectors.contains(method.getName()), "Legacy selector: " + method.getName());
        }
    }

    @Test
    void explicitConnectionSelectsInProcess() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forInProcess());
        try (var client = new CopilotClient(options)) {
            assertInstanceOf(InProcessRuntimeConnection.class, client.getRuntimeConnection());
        }
    }

    @Test
    void spawnedTcpConnectionSelectsTcp() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedTcp().setPort(4321).setConnectionToken("secret"));
        try (var client = new CopilotClient(options)) {
            var connection = assertInstanceOf(SpawnedTcpRuntimeConnection.class, client.getRuntimeConnection());
            assertEquals(4321, connection.getPort());
            assertEquals("secret", connection.getConnectionToken());
        }
    }

    @Test
    void existingTcpConnectionPreservesHostAndPort() {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 3000)))) {
            var connection = assertInstanceOf(TcpRuntimeConnection.class, client.getRuntimeConnection());
            assertEquals("localhost", connection.getHost());
            assertEquals(3000, connection.getPort());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"github.token", "github.useLoggedInUser", "observability.logLevel",
            "observability.telemetry", "remote"})
    void attachedRuntimeRejectsRuntimeOnlyOptions(String option) {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 4321));
        switch (option) {
            case "github.token" -> options.getGitHub().setToken("owned-token");
            case "github.useLoggedInUser" -> options.getGitHub().setUseLoggedInUser(true);
            case "observability.logLevel" -> options.getObservability().setLogLevel("debug");
            case "observability.telemetry" -> options.getObservability().setTelemetry(new TelemetryConfig());
            case "remote" -> options.setRemote(true);
            default -> throw new AssertionError("Unexpected option: " + option);
        }
        var error = assertThrows(IllegalArgumentException.class, () -> new CopilotClient(options));
        assertTrue(error.getMessage().contains(option), error.getMessage());
        options.setRuntime(RuntimeConnection.forInProcess());
        try (var client = new CopilotClient(options)) {
            assertInstanceOf(InProcessRuntimeConnection.class, client.getRuntimeConnection());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"getBaseDirectory", "getSessionIdleTimeoutSeconds"})
    void attachedRuntimeDoesNotExposeOwnedStartupOptions(String getter) throws NoSuchMethodException {
        assertThrows(NoSuchMethodException.class, () -> TcpRuntimeConnection.class.getMethod(getter));
        assertThrows(NoSuchMethodException.class, () -> CopilotClientOptions.class.getMethod(getter));
        OwnedRuntimeConnection.class.getMethod(getter);
    }

    @Test
    void attachedRuntimeRejectsExplicitInfoLogLevel() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 4321))
                .setObservability(new ClientObservabilityOptions().setLogLevel("info"));
        var error = assertThrows(IllegalArgumentException.class, () -> new CopilotClient(options));
        assertTrue(error.getMessage().contains("logLevel"), error.getMessage());
    }

    @Test
    void attachedRuntimeAcceptsDefaultsAndClientCallbacks() throws Exception {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 4321)).setAutoStart(false).setRemote(false)
                .setExecutor(Runnable::run);
        options.getObservability().setLogLevel("debug").setLogLevel(null);
        options.getGitHub().setUseLoggedInUser(false).clearUseLoggedInUser();
        options.getModels().setOnListModels(() -> CompletableFuture.completedFuture(List.of()));
        try (var client = new CopilotClient(options)) {
            assertTrue(client.listModels().get(5, TimeUnit.SECONDS).isEmpty());
            assertInstanceOf(TcpRuntimeConnection.class, client.getRuntimeConnection());
        }
    }

    @Test
    void connectionCanBeReusedForSeveralClients() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio("/opt/copilot"));
        try (var first = new CopilotClient(options); var second = new CopilotClient(options)) {
            assertInstanceOf(SpawnedStdioRuntimeConnection.class, first.getRuntimeConnection());
            assertInstanceOf(SpawnedStdioRuntimeConnection.class, second.getRuntimeConnection());
        }
    }

    @Test
    void inProcessAcceptsTypedConfiguration() throws Exception {
        var connection = RuntimeConnection.forInProcess().setBaseDirectory("owned-home")
                .setSessionIdleTimeoutSeconds(17);
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(connection)
                .setObservability(new ClientObservabilityOptions().setLogLevel("debug")
                        .setTelemetry(new TelemetryConfig().setExporterType("file").setFilePath("/telemetry.jsonl")));
        try (var runtime = new FakeInProcessRuntime(); var client = new CopilotClient(options)) {
            connection.setBaseDirectory("another-home").setSessionIdleTimeoutSeconds(43);
            client.setInProcessTransportFactory(received -> {
                var selected = assertInstanceOf(InProcessRuntimeConnection.class, received.getRuntime());
                assertEquals("owned-home", selected.getBaseDirectory());
                assertEquals(Integer.valueOf(17), selected.getSessionIdleTimeoutSeconds());
                assertEquals("debug", received.getObservability().getLogLevel());
                assertEquals("/telemetry.jsonl", received.getObservability().getTelemetry().getFilePath());
                return runtime.open(received);
            });
            client.start().get(30, TimeUnit.SECONDS);
            assertTrue(runtime.opened.get());
        }
    }

    @Test
    void e2eContextUsesTypedInProcessConfiguration() throws Exception {
        try (var context = E2ETestContext.create()) {
            var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(RuntimeConnection.forInProcess());

            try (var client = context.createClient(options)) {
                assertInstanceOf(InProcessRuntimeConnection.class, client.getRuntimeConnection());
                assertEquals(context.getProxyUrl(), options.getInternalTestOptions().getCopilotApiUrl());
                assertEquals(context.getHomeDir().toString(),
                        com.microsoft.copilot.RuntimeSettings.baseDirectory(options.getRuntime()));
                assertEquals(context.getWorkDir().toString(), context.newSessionConfig().getWorkingDirectory());
            }
        }
    }

    private static String rootMessage(Throwable error) {
        Throwable cause = error;
        while (cause.getCause() != null) {
            cause = cause.getCause();
        }
        return String.valueOf(cause.getMessage());
    }

    /**
     * Minimal loopback stand-in for the in-process runtime: it speaks just enough
     * JSON-RPC for {@link CopilotClient#start()} to complete, so the test can
     * assert that the client wires its transport to the in-process host rather than
     * to a child process.
     */
    private static final class FakeInProcessRuntime implements AutoCloseable {

        private static final ObjectMapper MAPPER = new ObjectMapper();

        private final AtomicBoolean opened = new AtomicBoolean();
        private final AtomicBoolean closed = new AtomicBoolean();
        private final AtomicBoolean pingReceived = new AtomicBoolean();
        private final AtomicBoolean connectionTokenReceived = new AtomicBoolean();
        private volatile boolean rejectConnect;
        private volatile int protocolVersion = SdkProtocolVersion.get();
        private final AtomicBoolean sessionRequested = new AtomicBoolean();
        private final CompletableFuture<JsonNode> callbackResponse = new CompletableFuture<>();
        private final BytePipe toClient;
        private final BytePipe toRuntime;
        private final InputStream runtimeInput;
        private final OutputStream runtimeOutput;
        private final Thread responder;

        FakeInProcessRuntime() throws IOException {
            this.toClient = new BytePipe();
            this.toRuntime = new BytePipe();
            this.runtimeInput = toRuntime.inputStream();
            this.runtimeOutput = toClient.outputStream();
            this.responder = new Thread(this::respondToRequests, "fake-inprocess-runtime");
            this.responder.setDaemon(true);
            this.responder.start();
        }

        CopilotClient.InProcessTransport open(CopilotClientOptions options) {
            opened.set(true);
            return new CopilotClient.InProcessTransport(toClient.inputStream(), toRuntime.outputStream(), this::close);
        }

        @Override
        public void close() {
            if (!closed.compareAndSet(false, true)) {
                return;
            }
            toRuntime.close();
            toClient.close();
        }

        private void respondToRequests() {
            try {
                while (!closed.get()) {
                    JsonNode request = readMessage(runtimeInput);
                    if (request == null) {
                        return;
                    }
                    if (!request.hasNonNull("id")) {
                        continue;
                    }
                    if (!request.has("method")) {
                        callbackResponse.complete(request);
                        continue;
                    }
                    var response = MAPPER.createObjectNode();
                    response.put("jsonrpc", "2.0");
                    response.set("id", request.get("id"));
                    var method = request.path("method").asText();
                    if ("connect".equals(method)) {
                        connectionTokenReceived.set(request.path("params").has("token"));
                    }
                    if ("session.create".equals(method) || "session.resume".equals(method))
                        sessionRequested.set(true);
                    if ("connect".equals(method) && rejectConnect) {
                        response.putObject("error").put("code", -32601).put("message", "Method not found");
                    } else {
                        var result = response.putObject("result");
                        if ("ping".equals(method)) {
                            pingReceived.set(true);
                        }
                        if ("connect".equals(method) || "ping".equals(method)) {
                            result.put("protocolVersion", protocolVersion);
                        }
                    }
                    writeMessage(runtimeOutput, response);
                }
            } catch (IOException e) {
                // The streams are closed when the client shuts down.
            }
        }

        private static JsonNode readMessage(InputStream in) throws IOException {
            int contentLength = -1;
            var line = new ByteArrayOutputStream();
            while (true) {
                int b = in.read();
                if (b == -1) {
                    return null;
                }
                if (b == '\n') {
                    String header = line.toString(StandardCharsets.UTF_8).trim();
                    line.reset();
                    if (header.isEmpty()) {
                        break;
                    }
                    if (header.toLowerCase(Locale.ROOT).startsWith("content-length:")) {
                        contentLength = Integer.parseInt(header.substring(header.indexOf(':') + 1).trim());
                    }
                } else if (b != '\r') {
                    line.write(b);
                }
            }
            if (contentLength < 0) {
                throw new IOException("Missing Content-Length header");
            }
            byte[] body = in.readNBytes(contentLength);
            if (body.length != contentLength) {
                return null;
            }
            return MAPPER.readTree(body);
        }

        private static void writeMessage(OutputStream out, JsonNode message) throws IOException {
            byte[] body = MAPPER.writeValueAsBytes(message);
            out.write(("Content-Length: " + body.length + "\r\n\r\n").getBytes(StandardCharsets.UTF_8));
            out.write(body);
            out.flush();
        }
    }

    /**
     * Duplex byte channel used by {@link FakeInProcessRuntime} to emulate the
     * streams of an in-process runtime.
     */
    private static final class BytePipe {

        private final Pipe pipe;

        BytePipe() throws IOException {
            this.pipe = Pipe.open();
        }

        InputStream inputStream() {
            return Channels.newInputStream(pipe.source());
        }

        OutputStream outputStream() {
            return Channels.newOutputStream(pipe.sink());
        }

        void close() {
            closeQuietly(pipe.sink());
            closeQuietly(pipe.source());
        }

        private static void closeQuietly(Closeable closeable) {
            try {
                closeable.close();
            } catch (IOException e) {
                // Nothing useful to do while tearing down a test pipe.
            }
        }
    }
}
