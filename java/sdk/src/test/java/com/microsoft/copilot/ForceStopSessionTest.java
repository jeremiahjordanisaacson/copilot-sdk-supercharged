/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.SessionEventOptions;
import com.microsoft.copilot.rpc.SessionGitHubOptions;
import com.microsoft.copilot.rpc.SessionGitOptions;
import com.microsoft.copilot.rpc.SessionHookOptions;
import com.microsoft.copilot.rpc.SessionInteractionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import com.microsoft.copilot.rpc.SessionToolOptions;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.SessionEvent;
import com.microsoft.copilot.generated.SessionIdleEvent;
import com.microsoft.copilot.rpc.AutoModeSwitchResponse;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.GitHubTokenProviderResult;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.SessionHooks;
import com.microsoft.copilot.rpc.ToolDefinition;

class ForceStopSessionTest {

    public static void main(String[] args) throws Exception {
        var tests = new ForceStopSessionTest();
        tests.commonPoolExecutorDeliversCompletionEvents();
        tests.admittedHandlerCanWaitForForceStopWithTheDefaultExecutor();
        if (Runtime.version().feature() >= 25) {
            tests.forceStopEventuallyInterruptsOwnedHandlers();
        }
    }

    @CopilotResponse
    public record Answer(int value) {
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void shutdownReturnsItsFutureWhileInlineStartupWaitsForConnect(boolean force) throws Exception {
        var callers = Executors.newFixedThreadPool(2);
        try (var server = new Server(); var client = server.createClient(Runnable::run)) {
            server.holdConnect = true;
            var starting = callers.submit(client::start);
            var connect = server.heldConnects.poll(5, TimeUnit.SECONDS);
            assertNotNull(connect, "Inline startup must be waiting for the connect response");
            boolean connectReleased = false;
            try {
                var shutdown = callers.submit(() -> force ? client.forceStop() : client.stop());
                var stopping = shutdown.get(2, TimeUnit.SECONDS);
                if (!force) {
                    server.completeConnect(connect);
                    connectReleased = true;
                    starting.get(5, TimeUnit.SECONDS).get(5, TimeUnit.SECONDS);
                } else {
                    assertThrows(ExecutionException.class,
                            () -> starting.get(5, TimeUnit.SECONDS).get(5, TimeUnit.SECONDS));
                }
                stopping.get(5, TimeUnit.SECONDS);
            } finally {
                if (!connectReleased && !starting.isDone()) {
                    try {
                        server.completeConnect(connect);
                    } catch (IOException error) {
                        if (!server.peerClosed.await(5, TimeUnit.SECONDS)) {
                            throw error;
                        }
                    }
                }
            }
        } finally {
            callers.shutdownNow();
            assertTrue(callers.awaitTermination(5, TimeUnit.SECONDS), "The owned startup callers must stop");
        }
    }

    @Test
    void implicitInlineStartupReturnsItsFailureWhenForceStopped() throws Exception {
        var caller = Executors.newSingleThreadExecutor();
        try (var server = new Server(); var client = server.createClient(Runnable::run)) {
            server.holdConnect = true;
            var creating = caller.submit(() -> client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))));
            assertNotNull(server.heldConnects.poll(5, TimeUnit.SECONDS),
                    "Implicit startup must be waiting for the connect response");

            client.forceStop().get(5, TimeUnit.SECONDS);

            var result = creating.get(5, TimeUnit.SECONDS);
            assertThrows(ExecutionException.class, () -> result.get(5, TimeUnit.SECONDS));
        } finally {
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS), "The owned startup caller must stop");
        }
    }

    @Test
    void startAfterTransportLossConnectsAgain() throws Exception {
        var listener = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"));
        try (var lostServer = new Server(listener); var client = lostServer.createClient()) {
            var lostSession = createSession(client);
            // The runtime closes the connection after a successful start.
            lostServer.socket.shutdownOutput();
            assertThrows(ExecutionException.class, () -> lostSession.getMessages().get(5, TimeUnit.SECONDS));
            assertEquals(ConnectionState.DISCONNECTED, client.getState());

            try (var server = new Server(listener)) {
                client.start().get(5, TimeUnit.SECONDS);

                assertEquals(ConnectionState.CONNECTED, client.getState());
                client.ping("replacement").get(5, TimeUnit.SECONDS);
                // The lost connection's session no longer claims its ID.
                client.resumeSession(lostSession.getSessionId(),
                        new ResumeSessionOptions().setPermissions(
                                new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(5, TimeUnit.SECONDS);
                client.forceStop().get(5, TimeUnit.SECONDS);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void startAfterProcessExitWithOpenTransportConnectsAgain(boolean recoverFromPendingPing) throws Exception {
        try (var lostServer = new Server();
                var server = new Server();
                var lostPipe = new Socket("127.0.0.1", lostServer.listener.getLocalPort());
                var pipe = new Socket("127.0.0.1", server.listener.getLocalPort());
                var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                        .setRuntime(RuntimeConnection.forSpawnedStdio()))) {
            var alive = new AtomicBoolean(true);
            var lostProcess = mock(Process.class);
            when(lostProcess.isAlive()).thenAnswer(invocation -> alive.get());
            when(lostProcess.getInputStream()).thenReturn(lostPipe.getInputStream());
            when(lostProcess.getOutputStream()).thenReturn(lostPipe.getOutputStream());
            when(lostProcess.descendants()).thenAnswer(invocation -> java.util.stream.Stream.empty());
            var process = mock(Process.class);
            var replacementAlive = new AtomicBoolean(true);
            when(process.isAlive()).thenAnswer(invocation -> replacementAlive.get());
            doAnswer(invocation -> {
                replacementAlive.set(false);
                return null;
            }).when(process).destroy();
            when(process.getInputStream()).thenReturn(pipe.getInputStream());
            when(process.getOutputStream()).thenReturn(pipe.getOutputStream());
            when(process.descendants()).thenAnswer(invocation -> java.util.stream.Stream.empty());
            var managerOptions = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(RuntimeConnection.forSpawnedStdio());
            var manager = spy(new CliServerManager(managerOptions, managerOptions.getRuntime()));
            doReturn(new CliServerManager.ProcessInfo(lostProcess, null),
                    new CliServerManager.ProcessInfo(process, null)).when(manager).startCliServer();
            Field managerField = CopilotClient.class.getDeclaredField("serverManager");
            managerField.setAccessible(true);
            managerField.set(client, manager);
            var lostSession = createSession(client);
            var replacementStarted = new CountDownLatch(1);

            try {
                client.start().get(5, TimeUnit.SECONDS);
                verify(manager, times(1)).startCliServer();
                var recovering = CompletableFuture.completedFuture((Void) null);
                if (recoverFromPendingPing) {
                    lostServer.holdPing = true;
                    var ping = client.ping("pending at process exit");
                    assertNotNull(lostServer.heldPings.poll(5, TimeUnit.SECONDS),
                            "The ping must be pending before the process exits");
                    recovering = ping.handle((result, error) -> {
                        assertNotNull(error, "Process retirement must fail the pending ping");
                        awaitRelease(replacementStarted);
                        client.start().orTimeout(5, TimeUnit.SECONDS).join();
                        return null;
                    });
                }
                // Model a descendant retaining the pipe after its runtime process exits.
                alive.set(false);
                client.start().get(5, TimeUnit.SECONDS);
                replacementStarted.countDown();
                recovering.get(5, TimeUnit.SECONDS);

                assertThrows(IllegalStateException.class, () -> lostSession.send("retired"));
                verify(manager, times(2)).startCliServer();
                client.ping("replacement").get(5, TimeUnit.SECONDS);
                client.resumeSession(lostSession.getSessionId(),
                        new ResumeSessionOptions().setPermissions(
                                new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get(5, TimeUnit.SECONDS);
            } finally {
                replacementStarted.countDown();
                client.forceStop().get(5, TimeUnit.SECONDS);
            }
        }
    }

    @Test
    void startInsideTransportFailureContinuationConnectsAgain() throws Exception {
        try (var lostServer = new Server(); var client = lostServer.createClient()) {
            client.start().get(5, TimeUnit.SECONDS);
            lostServer.holdPing = true;
            var ping = client.ping("pending at transport loss");
            assertNotNull(lostServer.heldPings.poll(5, TimeUnit.SECONDS), "The ping must be pending before EOF");
            var recovering = ping.handle((result, error) -> {
                assertNotNull(error, "Transport loss must fail the pending ping");
                // Bound an uninterruptible join so a deadlock fails without leaking the reader.
                client.start().orTimeout(5, TimeUnit.SECONDS).join();
                return null;
            });

            try (var server = new Server(lostServer.listener)) {
                lostServer.socket.shutdownOutput();
                recovering.get(10, TimeUnit.SECONDS);

                assertEquals(ConnectionState.CONNECTED, client.getState());
                client.ping("replacement").get(5, TimeUnit.SECONDS);
                client.forceStop().get(5, TimeUnit.SECONDS);
            }
        }
    }

    @Test
    void localCloseFailsPendingRequestsWithBusyCommonPool(@org.junit.jupiter.api.io.TempDir java.nio.file.Path temp)
            throws Exception {
        var output = temp.resolve("close-probe.log");
        var javaBin = java.nio.file.Path.of(System.getProperty("java.home"), "bin", "java").toString();
        var classpath = System.getProperty("surefire.test.class.path", System.getProperty("java.class.path"))
                + java.io.File.pathSeparator + System.getProperty("jdk.module.path", "");
        var probe = new ProcessBuilder(javaBin, "-Djava.util.concurrent.ForkJoinPool.common.parallelism=2", "-cp",
                classpath, BusyCommonPoolCloseProbe.class.getName()).redirectErrorStream(true)
                .redirectOutput(output.toFile()).start();
        try {
            assertTrue(probe.waitFor(20, TimeUnit.SECONDS), () -> "Close probe did not exit; output: " + output);
            assertEquals(0, probe.exitValue(), java.nio.file.Files.readString(output));
        } finally {
            probe.destroyForcibly();
            assertTrue(probe.waitFor(5, TimeUnit.SECONDS), "The owned close probe must stop");
        }
    }

    public static final class BusyCommonPoolCloseProbe {
        public static void main(String[] args) throws Exception {
            var entered = new CountDownLatch(2);
            var release = new CountDownLatch(1);
            var workers = new CompletableFuture<?>[2];
            for (int i = 0; i < workers.length; i++) {
                workers[i] = CompletableFuture.runAsync(() -> {
                    entered.countDown();
                    awaitRelease(release);
                });
            }
            var readerRelease = new CountDownLatch(1);
            try (var rpc = JsonRpcClient.fromStreams(new InputStream() {
                @Override
                public int read() {
                    awaitRelease(readerRelease);
                    return -1;
                }

                @Override
                public void close() {
                    readerRelease.countDown();
                }
            }, java.io.OutputStream.nullOutputStream())) {
                assertTrue(entered.await(5, TimeUnit.SECONDS), "Both common-pool workers must be occupied");
                var pending = rpc.invoke("ping", Map.of(), Object.class);
                rpc.close();
                assertThrows(ExecutionException.class, () -> pending.get(5, TimeUnit.SECONDS));
            } finally {
                readerRelease.countDown();
                release.countDown();
                CompletableFuture.allOf(workers).get(5, TimeUnit.SECONDS);
            }
        }
    }

    @Test
    void createCapturedBeforeRestartCannotReplaceNewSessionRouting() throws Exception {
        var caller = Executors.newSingleThreadExecutor();
        var captured = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var events = new AtomicInteger();
        try (var lostServer = new Server(); var client = lostServer.createClient()) {
            client.start().get(5, TimeUnit.SECONDS);
            var config = new CreateSessionOptions() {
                @Override
                public String getSessionId() {
                    captured.countDown();
                    awaitRelease(release);
                    return "replacement-session";
                }
            }.setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var creating = caller.submit(() -> client.createSession(config));
            try {
                assertTrue(captured.await(5, TimeUnit.SECONDS),
                        "The create must capture its connection before registration");
                lostServer.socket.shutdownOutput();
                assertThrows(ExecutionException.class, () -> client.ping("lost connection").get(5, TimeUnit.SECONDS));
                try (var server = new Server(lostServer.listener)) {
                    client.start().get(5, TimeUnit.SECONDS);
                    var replacement = client
                            .createSession(new CreateSessionOptions().setSessionId("replacement-session")
                                    .setPermissions(
                                            new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                                    .setHooks(new SessionHookOptions().setHandlers(new SessionHooks().setOnSessionEnd(
                                            (input, invocation) -> CompletableFuture.completedFuture(null)))))
                            .get(5, TimeUnit.SECONDS);
                    try (var subscription = replacement.on(event -> events.incrementAndGet())) {
                        release.countDown();
                        assertThrows(ExecutionException.class,
                                () -> creating.get(5, TimeUnit.SECONDS).get(5, TimeUnit.SECONDS));
                        server.emitIdle(replacement.getSessionId());
                        awaitCount(events, 1, "A stale create must preserve replacement events");
                        server.invokeSessionEnd(replacement.getSessionId());
                    }
                    client.forceStop().get(5, TimeUnit.SECONDS);
                }
            } finally {
                release.countDown();
            }
        } finally {
            release.countDown();
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS), "The owned create caller must stop");
        }
    }

    @Test
    void delayedOptionsFailureCannotRemoveReplacementSessionRouting() throws Exception {
        var caller = Executors.newSingleThreadExecutor();
        var patchEntered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var events = new AtomicInteger();
        try (var lostServer = new Server(); var client = lostServer.createClient(Runnable::run)) {
            var config = new CreateSessionOptions().setGit(new SessionGitOptions() {
                @Override
                public java.util.Optional<Boolean> getCoauthorEnabled() {
                    patchEntered.countDown();
                    awaitRelease(release);
                    return java.util.Optional.of(true);
                }
            }).setSessionId("replacement-session")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var creating = caller.submit(() -> client.createSession(config));
            try {
                assertTrue(patchEntered.await(5, TimeUnit.SECONDS),
                        "The old create must reach its post-create options patch");
                client.forceStop().get(5, TimeUnit.SECONDS);
                try (var server = new Server(lostServer.listener)) {
                    client.start().get(5, TimeUnit.SECONDS);
                    var replacement = client
                            .createSession(new CreateSessionOptions().setSessionId("replacement-session")
                                    .setPermissions(
                                            new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                                    .setGit(new SessionGitOptions().setCoauthorEnabled(false))
                                    .setHooks(new SessionHookOptions().setHandlers(new SessionHooks().setOnSessionEnd(
                                            (input, invocation) -> CompletableFuture.completedFuture(null)))))
                            .get(5, TimeUnit.SECONDS);
                    try (var subscription = replacement.on(event -> events.incrementAndGet())) {
                        release.countDown();
                        assertThrows(ExecutionException.class,
                                () -> creating.get(5, TimeUnit.SECONDS).get(5, TimeUnit.SECONDS));
                        server.emitIdle(replacement.getSessionId());
                        awaitCount(events, 1, "An old options failure must preserve replacement events");
                        server.invokeSessionEnd(replacement.getSessionId());
                    }
                    client.forceStop().get(5, TimeUnit.SECONDS);
                }
            } finally {
                release.countDown();
            }
        } finally {
            release.countDown();
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS), "The owned create caller must stop");
        }
    }

    @Test
    void concurrentFailedResumesDuringGracefulStopRetireDisplacedWaits() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var resident = createSession(client);
            var waiting = resident.sendAndWait(new MessageOptions().setPrompt("pending response"), 0);
            assertNotNull(server.sends.poll(5, TimeUnit.SECONDS), "The legacy wait must be admitted");
            client.ping("send acknowledgement barrier").get(5, TimeUnit.SECONDS);
            server.holdResume = true;
            var config = new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var first = client.resumeSession(resident.getSessionId(), config);
            var firstRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(firstRequest, "The first resume must be admitted");
            var second = client.resumeSession(resident.getSessionId(), config);
            var secondRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(secondRequest, "The second resume must be admitted");
            server.holdDetachResponse = true;
            var stopping = client.stop();
            var detach = server.heldDetaches.poll(5, TimeUnit.SECONDS);
            assertNotNull(detach, "Graceful stop must be draining the second provisional session");
            try {
                for (var request : List.of(firstRequest, secondRequest)) {
                    server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "error",
                            Map.of("code", -32603, "message", "controlled resume rejection")));
                    var resuming = request == firstRequest ? first : second;
                    assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
                }
            } finally {
                server.completeDetach(detach, "success");
                stopping.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 3, TimeUnit.SECONDS);
            }

            var error = assertThrows(ExecutionException.class, () -> waiting.get(5, TimeUnit.SECONDS));
            assertEquals("Session closed before response completed", error.getCause().getMessage());
            assertThrows(IllegalStateException.class, () -> resident.on(event -> {
            }));
        }
    }

    @Test
    void failedResumeDuringGracefulStopPreservesResidentCallbacksUntilDetachCompletes() throws Exception {
        var events = new AtomicInteger();
        var hooks = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient()) {
            var resident = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setHooks(new SessionHookOptions()
                            .setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                hooks.incrementAndGet();
                                return CompletableFuture.completedFuture(null);
                            }))))
                    .get(5, TimeUnit.SECONDS);
            try (var subscription = resident.on(event -> events.incrementAndGet())) {
                server.holdResume = true;
                var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
                var resume = server.heldResumes.poll(5, TimeUnit.SECONDS);
                assertNotNull(resume, "Warm resume must be pending before stop");
                server.holdDetachResponse = true;
                var stopping = client.stop();
                var detach = server.heldDetaches.poll(5, TimeUnit.SECONDS);
                assertNotNull(detach, "Graceful stop must be draining the provisional session");
                try {
                    server.writeMessage(Map.of("jsonrpc", "2.0", "id", resume.get("id"), "error",
                            Map.of("code", -32603, "message", "controlled resume rejection")));
                    assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
                    server.emitIdle(resident.getSessionId());
                    awaitCount(events, 1, "The resident must receive terminal events while detach drains");
                    server.invokeSessionEnd(resident.getSessionId());
                    assertEquals(1, hooks.get(), "The resident's session-end hook must remain routed");
                } finally {
                    server.completeDetach(detach, "success");
                    stopping.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 3, TimeUnit.SECONDS);
                }
                assertThrows(IllegalStateException.class, () -> resident.send("after stop"));
            }
        }
    }

    @Test
    void correlatedCompletionPrecedesLaterForceStopListener() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var stopped = new CompletableFuture<Void>();
        try (var server = new Server(); var client = server.createClient()) {
            server.completionSupported = true;
            var session = createSession(client);
            try (var first = session.on(SessionIdleEvent.class, event -> {
                entered.countDown();
                awaitRelease(release);
            })) {
                var waiting = session.sendAndWait(new MessageOptions().setPrompt("question"), 0);
                var request = server.completions.poll(5, TimeUnit.SECONDS);
                assertNotNull(request);
                try (var later = session.on(SessionIdleEvent.class, event -> {
                    try {
                        client.forceStop().join();
                        stopped.complete(null);
                    } catch (Throwable failure) {
                        stopped.completeExceptionally(failure);
                    }
                })) {
                    try {
                        var idle = server.emitEvent(session.getSessionId(), "session.idle", Map.of());
                        assertTrue(entered.await(5, TimeUnit.SECONDS));
                        server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result", Map.of(
                                "messageId", "owned-request", "aborted", false, "completionEventId", idle.get("id"))));
                        client.ping("completion response read barrier").get(5, TimeUnit.SECONDS);
                        release.countDown();
                        stopped.get(5, TimeUnit.SECONDS);
                        assertNull(waiting.get(5, TimeUnit.SECONDS),
                                "A later shutdown listener must preserve the successful completion");
                    } finally {
                        release.countDown();
                    }
                }
            }
        }
    }

    @Test
    void forceStopAfterGracefulStopRetiresDrainingSession() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        var laterCalls = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            var session = createSession(client);
            try (var first = session.on(event -> {
                entered.countDown();
                awaitRelease(release);
            }); var second = session.on(event -> laterCalls.incrementAndGet())) {
                try {
                    server.emitIdle(session.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS));
                    client.stop().get(5, TimeUnit.SECONDS);
                    client.forceStop().get(5, TimeUnit.SECONDS);
                    assertEquals(1, release.getCount(), "Shutdown must not join an admitted handler");
                } finally {
                    release.countDown();
                    awaitDeliveries(deliveries, "The draining handler must finish after its release");
                }
                assertEquals(0, laterCalls.get(), "Force stop must retire sessions removed by graceful stop");
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"resume", "create", "delete"})
    void forceStopRetiresDrainingSessionsRemovedFromTheMap(String operation) throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        var laterCalls = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            var session = createSession(client);
            try (var first = session.on(event -> {
                entered.countDown();
                awaitRelease(release);
            }); var second = session.on(event -> laterCalls.incrementAndGet())) {
                try {
                    server.emitIdle(session.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS));
                    server.emitIdle(session.getSessionId());
                    closeOnOwnThread(session).get(5, TimeUnit.SECONDS);
                    switch (operation) {
                        case "resume" -> client
                                .resumeSession(session.getSessionId(), new ResumeSessionOptions().setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(5, TimeUnit.SECONDS);
                        case "create" -> client
                                .createSession(
                                        new CreateSessionOptions().setSessionId(session.getSessionId())
                                                .setPermissions(new SessionPermissionOptions()
                                                        .setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(5, TimeUnit.SECONDS);
                        case "delete" -> client.deleteSession(session.getSessionId()).get(5, TimeUnit.SECONDS);
                        default -> throw new IllegalArgumentException("Unexpected operation: " + operation);
                    }
                    client.forceStop().get(5, TimeUnit.SECONDS);
                    assertEquals(1, release.getCount(), "Force stop must not join an admitted handler");
                } finally {
                    release.countDown();
                    awaitDeliveries(deliveries, "The displaced session's handler must finish after its release");
                }
                assertEquals(0, laterCalls.get(),
                        "Force stop must retire the displaced session's remaining subscribers and queued events");
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"create", "resume", "empty"})
    void rejectedSessionAttemptsReleaseConfiguredHandlers(String operation) throws Exception {
        try (var server = new Server();
                var client = new CopilotClient(new CopilotClientOptions()
                        .setRuntime(RuntimeConnection.forTcp("127.0.0.1", server.listener.getLocalPort()))
                        .setMode(operation.equals("empty")
                                ? CopilotClientMode.EMPTY
                                : CopilotClientMode.CODING_AGENT))) {
            server.rejectSessionRequests = true;
            var handler = rejectedSessionHandler(client, operation);
            awaitCollected(handler, "A rejected " + operation + " must release its configured handler");
            assertNotNull(client.ping("client remains reachable").get(5, TimeUnit.SECONDS));
            assertEquals(0, server.detaches.get(), "Abandoned attempts must retire without a detach RPC");
        }
    }

    private static WeakReference<Consumer<SessionEvent>> rejectedSessionHandler(CopilotClient client, String operation)
            throws Exception {
        Consumer<SessionEvent> handler = new Consumer<>() {
            @Override
            public void accept(SessionEvent event) {
                fail("A rejected session must not receive events");
            }
        };
        var reference = new WeakReference<>(handler);
        var attempt = operation.equals("resume")
                ? client.resumeSession("rejected-session", new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                        .setEvents(new SessionEventOptions().setOnEvent(handler)))
                : client.createSession(new CreateSessionOptions().setSessionId("rejected-session")
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                        .setEvents(new SessionEventOptions().setOnEvent(handler)));
        var failure = assertThrows(ExecutionException.class, () -> attempt.get(5, TimeUnit.SECONDS));
        assertTrue(
                failure.getCause().getMessage()
                        .contains(operation.equals("empty") ? "availableTools" : "controlled session rejection"),
                failure.toString());
        return reference;
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void resumeRetiresDisplacedHandleOnlyAfterSuccess(boolean succeeds) throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var drained = new CompletableFuture<Void>();
        var deliveries = Executors.newSingleThreadExecutor();
        var calls = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            var original = createSession(client);
            try (var subscription = original.on(event -> {
                if (calls.incrementAndGet() == 1) {
                    entered.countDown();
                    awaitRelease(release);
                } else {
                    drained.complete(null);
                }
            })) {
                try {
                    server.emitIdle(original.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS));
                    server.emitIdle(original.getSessionId());
                    client.ping("queued event read barrier").get(5, TimeUnit.SECONDS);
                    server.holdResumeResponse = true;
                    var resuming = client.resumeSession(original.getSessionId(),
                            new ResumeSessionOptions().setPermissions(
                                    new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
                    var request = server.heldResumes.poll(5, TimeUnit.SECONDS);
                    assertNotNull(request, "Resume must displace the original before its outcome is decided");
                    if (succeeds) {
                        server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                                Map.of("sessionId", original.getSessionId())));
                        var replacement = resuming.get(5, TimeUnit.SECONDS);
                        assertEquals(1, release.getCount(), "Resume must not wait for an admitted callback");
                        assertThrows(IllegalStateException.class, () -> original.send("displaced handle"));
                        original.close();
                        assertEquals(0, server.detaches.get(), "Local retirement must not detach the replacement");
                        var delivered = new CountDownLatch(1);
                        try (var replacementSubscription = replacement.on(event -> delivered.countDown())) {
                            server.emitIdle(replacement.getSessionId());
                            client.ping("replacement event read barrier").get(5, TimeUnit.SECONDS);
                            release.countDown();
                            drained.get(5, TimeUnit.SECONDS);
                            assertTrue(delivered.await(5, TimeUnit.SECONDS), "The replacement must remain registered");
                            assertNotNull(replacement.send("replacement still attached").get(5, TimeUnit.SECONDS));
                        }
                    } else {
                        server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "error",
                                Map.of("code", -32603, "message", "controlled resume failure")));
                        var failure = assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
                        assertTrue(failure.getCause().getMessage().contains("controlled resume failure"),
                                failure.toString());
                        assertNotNull(original.send("restored handle").get(5, TimeUnit.SECONDS));
                        release.countDown();
                        drained.get(5, TimeUnit.SECONDS);
                    }
                } finally {
                    release.countDown();
                    awaitDeliveries(deliveries, "Accepted callbacks must finish after their release");
                }
                assertEquals(2, calls.get(), "Retirement must drain both accepted callbacks");
                if (succeeds) {
                    assertThrows(IllegalStateException.class, () -> original.on(event -> fail("Retired callback")));
                } else {
                    try (var restoredSubscription = original.on(event -> {
                    })) {
                        assertEquals(0, server.detaches.get(), "A failed resume must leave the original attached");
                    }
                }
            }
        } finally {
            deliveries.shutdownNow();
        }
    }

    @ParameterizedTest
    @CsvSource({"false,false", "false,true", "true,false", "true,true"})
    void cancelledResumeLetsTheRuntimeDecideRollback(boolean runtimeCommits, boolean timeout) throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var original = createSession(client);
            server.holdResumeResponse = true;
            var resuming = client.resumeSession(original.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            var request = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(request, "The resume must be pending when it is abandoned");
            if (timeout) {
                resuming.orTimeout(1, TimeUnit.MILLISECONDS);
            } else {
                assertTrue(resuming.cancel(true));
            }
            assertEquals(request.get("id"), server.cancellations.poll(5, TimeUnit.SECONDS),
                    "Abandoning the caller's future must ask the runtime to cancel the resume");
            try (var pending = original.on(event -> {
            })) {
                // Displacement alone must not terminate the original before the runtime
                // answers.
            }
            if (runtimeCommits) {
                // The runtime finished the resume anyway, so the client must commit it and
                // retire the original.
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                        Map.of("sessionId", original.getSessionId())));
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
                while (true) {
                    try (var probe = original.on(event -> {
                    })) {
                        assertTrue(System.nanoTime() < deadline, "A committed resume must retire the original");
                    } catch (IllegalStateException retired) {
                        break;
                    }
                    TimeUnit.MILLISECONDS.sleep(10);
                }
                assertEquals(0, server.detaches.get(), "Committing a cancelled resume must not detach it");
            } else {
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "error",
                        Map.of("code", -32800, "message", "Session resume was cancelled")));
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
                while (true) {
                    try {
                        assertNotNull(original.send("restored handle").get(5, TimeUnit.SECONDS));
                        break;
                    } catch (IllegalStateException stillDisplaced) {
                        assertTrue(System.nanoTime() < deadline, "A cancelled resume must restore the original");
                    }
                    TimeUnit.MILLISECONDS.sleep(10);
                }
                var delivered = new CountDownLatch(1);
                try (var subscription = original.on(event -> delivered.countDown())) {
                    server.emitIdle(original.getSessionId());
                    assertTrue(delivered.await(5, TimeUnit.SECONDS),
                            "Rollback must route the session's events to the original again");
                }
                assertEquals(0, server.detaches.get(), "A cancelled resume must leave the original attached");
            }
        }
    }

    @Test
    void overlappingResumeRetiresTheFullDisplacedChainBeforeTheEarlierAttemptFails() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var original = createSession(client);
            server.holdOptions = true;
            var earlier = client.resumeSession(original.getSessionId(),
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var options = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(options, "The earlier resume must wait for its option update");
            var current = client
                    .resumeSession(original.getSessionId(),
                            new ResumeSessionOptions().setPermissions(
                                    new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get(5, TimeUnit.SECONDS);

            assertThrows(IllegalStateException.class, () -> original.send("displaced original"));
            assertThrows(IllegalStateException.class, () -> original.on(event -> fail("Retired callback")));
            original.close();
            assertEquals(0, server.detaches.get(), "Closing the original must not detach the successful replacement");

            server.writeMessage(Map.of("jsonrpc", "2.0", "id", options.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled option update failure")));
            var failure = assertThrows(ExecutionException.class, () -> earlier.get(5, TimeUnit.SECONDS));
            assertTrue(failure.getCause().getMessage().contains("controlled option update failure"),
                    failure.toString());
            assertEquals(0, server.detaches.get(), "The earlier failure must not detach the successful replacement");
            var delivered = new CountDownLatch(1);
            try (var subscription = current.on(event -> delivered.countDown())) {
                server.emitIdle(current.getSessionId());
                assertTrue(delivered.await(5, TimeUnit.SECONDS),
                        "The earlier attempt's rollback must preserve the successful registration");
                assertNotNull(current.send("current handle remains attached").get(5, TimeUnit.SECONDS));
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void overlappingPrefixResumesKeepTheCanonicalOwnerUsable(boolean publishBeforeRetirement) throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var original = createSession(client);
            String canonicalId = original.getSessionId();
            String prefix = canonicalId.substring(0, 7);
            server.holdResumeResponse = true;
            server.holdOptions = true;
            var first = client.resumeSession(prefix,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var firstRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(firstRequest);
            var middle = client.resumeSession(prefix, new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            var middleRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(middleRequest);
            var last = client.resumeSession(prefix,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var lastRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(lastRequest);

            server.writeMessage(
                    Map.of("jsonrpc", "2.0", "id", lastRequest.get("id"), "result", Map.of("sessionId", canonicalId)));
            var lastOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(lastOptions, "The last attempt must publish before the first reclaims the canonical ID");
            JsonNode firstOptions = null;
            if (publishBeforeRetirement) {
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", firstRequest.get("id"), "result",
                        Map.of("sessionId", canonicalId)));
                firstOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
                assertNotNull(firstOptions, "The first attempt must reclaim ownership before retirement starts");
            }
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", lastOptions.get("id"), "result", Map.of()));
            var lastHandle = last.get(5, TimeUnit.SECONDS);
            if (!publishBeforeRetirement) {
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", firstRequest.get("id"), "result",
                        Map.of("sessionId", canonicalId)));
                firstOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
                assertNotNull(firstOptions, "The first attempt must receive its canonical ID after retirement");
            }
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", firstOptions.get("id"), "result", Map.of()));
            var firstHandle = first.get(5, TimeUnit.SECONDS);
            var owner = publishBeforeRetirement ? firstHandle : lastHandle;
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", middleRequest.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled superseded resume failure")));
            assertThrows(ExecutionException.class, () -> middle.get(5, TimeUnit.SECONDS));

            assertNotNull(owner.send("canonical owner remains usable").get(5, TimeUnit.SECONDS));
            var delivered = new CountDownLatch(1);
            try (var subscription = owner.on(event -> delivered.countDown())) {
                server.emitIdle(canonicalId);
                assertTrue(delivered.await(5, TimeUnit.SECONDS), "Canonical events must route to the surviving owner");
            }
            assertEquals(0, server.detaches.get(), "Displaced cleanup must not detach the canonical owner");
        }
    }

    @Test
    void failedCanonicalResumeRestoresTheSuccessfulOverlappingPrefixResume() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var original = createSession(client);
            String canonicalId = original.getSessionId();
            String prefix = canonicalId.substring(0, 7);
            server.holdResumeResponse = true;
            server.holdOptions = true;
            var earlier = client.resumeSession(prefix,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var earlierRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(earlierRequest);
            var later = client.resumeSession(prefix,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var laterRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(laterRequest);

            server.writeMessage(
                    Map.of("jsonrpc", "2.0", "id", laterRequest.get("id"), "result", Map.of("sessionId", canonicalId)));
            var laterOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(laterOptions);
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", earlierRequest.get("id"), "result",
                    Map.of("sessionId", canonicalId)));
            var earlierOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(earlierOptions, "The earlier attempt must reclaim the canonical ID before retirement");
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", laterOptions.get("id"), "result", Map.of()));
            var successful = later.get(5, TimeUnit.SECONDS);
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", earlierOptions.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled canonical options failure")));
            var failure = assertThrows(ExecutionException.class,
                    () -> earlier.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 5, TimeUnit.SECONDS));
            assertTrue(failure.getCause().getMessage().contains("controlled canonical options failure"),
                    failure.toString());
            assertEquals(0, server.detaches.get(), "A failed canonical owner must not detach its successful fallback");
            var delivered = new CountDownLatch(1);
            try (var subscription = successful.on(event -> delivered.countDown())) {
                server.emitIdle(canonicalId);
                assertTrue(delivered.await(5, TimeUnit.SECONDS), "Rollback must restore the successful event owner");
                assertNotNull(successful.send("successful fallback remains attached").get(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test
    void failedOptionsUpdatePreservesAnAcceptedResumeAwaitingOptions() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.holdResumeResponse = true;
            server.holdOptions = true;
            String sessionId = "accepted-resume";
            var earlier = client.resumeSession(sessionId,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var earlierRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(earlierRequest);
            server.writeMessage(
                    Map.of("jsonrpc", "2.0", "id", earlierRequest.get("id"), "result", Map.of("sessionId", sessionId)));
            var earlierOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(earlierOptions, "The accepted predecessor must still be awaiting its options update");

            var later = client.resumeSession(sessionId,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var laterRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(laterRequest);
            server.writeMessage(
                    Map.of("jsonrpc", "2.0", "id", laterRequest.get("id"), "result", Map.of("sessionId", sessionId)));
            var laterOptions = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(laterOptions);
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", laterOptions.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled options failure")));
            var failure = assertThrows(ExecutionException.class,
                    () -> later.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 5, TimeUnit.SECONDS));
            assertTrue(failure.getCause().getMessage().contains("controlled options failure"), failure.toString());
            assertEquals(0, server.detaches.get(), "Rollback must not detach the accepted predecessor");

            server.writeMessage(Map.of("jsonrpc", "2.0", "id", earlierOptions.get("id"), "result", Map.of()));
            var successful = earlier.get(5, TimeUnit.SECONDS);
            assertNotNull(successful.send("accepted predecessor remains attached").get(5, TimeUnit.SECONDS));
            var delivered = new CountDownLatch(1);
            try (var subscription = successful.on(event -> delivered.countDown())) {
                server.emitIdle(sessionId);
                assertTrue(delivered.await(5, TimeUnit.SECONDS), "Rollback must restore the accepted event owner");
            }
            successful.close();
            assertEquals(1, server.detaches.get(), "The surviving owner must eventually detach its attachment");
        }
    }

    @Test
    void failedOptionsUpdateDetachesInsteadOfRestoringAnUnfinishedResume() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.holdResumeResponse = true;
            server.holdOptions = true;
            String sessionId = "unfinished-resume";
            var earlier = client.resumeSession(sessionId, new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            var earlierRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(earlierRequest);
            var later = client.resumeSession(sessionId,
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true)));
            var laterRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(laterRequest);
            server.writeMessage(
                    Map.of("jsonrpc", "2.0", "id", laterRequest.get("id"), "result", Map.of("sessionId", sessionId)));
            var options = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(options);
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", options.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled options failure")));
            var laterFailure = assertThrows(ExecutionException.class,
                    () -> later.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 5, TimeUnit.SECONDS));
            assertTrue(laterFailure.getCause().getMessage().contains("controlled options failure"),
                    laterFailure.toString());
            assertEquals(1, server.detaches.get(),
                    "An unfinished fallback cannot own cleanup of an accepted attachment");
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", earlierRequest.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled resume failure")));
            var earlierFailure = assertThrows(ExecutionException.class, () -> earlier.get(5, TimeUnit.SECONDS));
            assertTrue(earlierFailure.getCause().getMessage().contains("controlled resume failure"),
                    earlierFailure.toString());
            assertEquals(1, server.detaches.get(), "The rejected attempt must not detach again");
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void failedOptionsUpdatePreservesAdmittedToolResponses(boolean toolFails) throws Exception {
        var started = new CountDownLatch(1);
        var toolFuture = new CompletableFuture<Object>();
        try (var server = new Server(); var client = server.createClient()) {
            var original = createSession(client);
            server.holdOptions = true;
            server.holdToolReplies = true;
            var resuming = client.resumeSession(original.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setGit(new SessionGitOptions().setCoauthorEnabled(true))
                    .setTools(new SessionToolOptions().setDefinitions(List
                            .of(ToolDefinition.create("pending_tool", "Waits for a result", Map.of(), invocation -> {
                                started.countDown();
                                return toolFuture;
                            })))));
            var options = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(options);
            server.emitEvent(original.getSessionId(), "external_tool.requested",
                    Map.of("requestId", "rollback-tool", "sessionId", original.getSessionId(), "toolCallId", "call",
                            "toolName", "pending_tool", "arguments", Map.of()));
            assertTrue(started.await(5, TimeUnit.SECONDS), "The provisional owner must admit the tool request");
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", options.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled options failure")));
            var failure = assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
            assertTrue(failure.getCause().getMessage().contains("controlled options failure"), failure.toString());
            assertEquals(0, server.detaches.get(), "Rollback must keep the original attached");
            assertFalse(toolFuture.isCancelled(), "Non-detaching rollback must preserve the admitted tool future");
            if (toolFails) {
                toolFuture.completeExceptionally(new IllegalStateException("controlled tool failure"));
            } else {
                toolFuture.complete("completed after rollback");
            }
            var reply = server.toolReplies.poll(5, TimeUnit.SECONDS);
            assertNotNull(reply, "The provisional owner's admitted tool must still answer the runtime");
            assertEquals("rollback-tool", reply.path("params").path("requestId").asText());
            if (toolFails) {
                assertTrue(reply.path("params").path("error").asText().contains("controlled tool failure"),
                        reply.toString());
            } else {
                assertEquals("completed after rollback",
                        reply.path("params").path("result").path("textResultForLlm").asText());
            }
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", reply.get("id"), "result", Map.of()));
            assertNotNull(original.send("original remains attached").get(5, TimeUnit.SECONDS));
            var delivered = new CountDownLatch(1);
            try (var subscription = original.on(event -> delivered.countDown())) {
                server.emitIdle(original.getSessionId());
                assertTrue(delivered.await(5, TimeUnit.SECONDS), "Rollback must restore event routing");
            }
        }
    }

    @Test
    void lateResumeCompletionPreservesTheCurrentTokenProvider() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var original = createSession(client);
            assertNotNull(server.creates.poll(5, TimeUnit.SECONDS));
            server.holdOptions = true;
            var first = client.resumeSession(original.getSessionId(),
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGit(new SessionGitOptions().setCoauthorEnabled(true))
                            .setGitHub(new SessionGitHubOptions().setTokenProvider(args -> CompletableFuture
                                    .completedFuture(GitHubTokenProviderResult.token("first-token", 3600)))));
            var options = server.heldOptions.poll(5, TimeUnit.SECONDS);
            assertNotNull(options, "The first resume must wait for its option update");
            assertNotNull(server.creates.poll(5, TimeUnit.SECONDS));
            var current = client
                    .resumeSession(original.getSessionId(), new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGitHub(new SessionGitHubOptions().setTokenProvider(args -> CompletableFuture
                                    .completedFuture(GitHubTokenProviderResult.token("current-token", 3600)))))
                    .get(5, TimeUnit.SECONDS);
            var request = server.creates.poll(5, TimeUnit.SECONDS);
            assertNotNull(request);
            String registrationId = request.path("params").path("github").path("tokenProviderRegistrationId").asText();
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", options.get("id"), "result", Map.of()));
            first.get(5, TimeUnit.SECONDS);

            var reply = server.invokeToken(registrationId, current.getSessionId());
            assertFalse(reply.has("error"), "An obsolete resume must not remove the current provider: " + reply);
            assertEquals("current-token", reply.path("result").path("accessToken").asText());
            assertNotNull(current.send("current handle remains usable").get(5, TimeUnit.SECONDS));
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"success", "failure", "delivery-failure"})
    void resumePreservesAdmittedExternalToolResponses(String outcome) throws Exception {
        var deliveries = Executors.newSingleThreadExecutor();
        var started = new CountDownLatch(1);
        var toolFuture = new CompletableFuture<Object>();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            server.holdToolReplies = true;
            var original = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setDefinitions(List
                            .of(ToolDefinition.create("pending_tool", "Waits for a result", Map.of(), invocation -> {
                                started.countDown();
                                return toolFuture;
                            })))))
                    .get(5, TimeUnit.SECONDS);
            server.emitEvent(original.getSessionId(), "external_tool.requested",
                    Map.of("requestId", "resumed-tool", "sessionId", original.getSessionId(), "toolCallId", "call",
                            "toolName", "pending_tool", "arguments", Map.of()));
            assertTrue(started.await(5, TimeUnit.SECONDS));
            deliveries.submit(() -> {
            }).get(5, TimeUnit.SECONDS);

            var replacement = client.resumeSession(original.getSessionId(),
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setContinuePendingWork(true))
                    .get(5, TimeUnit.SECONDS);
            assertFalse(toolFuture.isCancelled(), "Resume must preserve the admitted tool future");
            assertThrows(IllegalStateException.class, () -> original.send("displaced handle"));
            if (outcome.equals("failure")) {
                toolFuture.completeExceptionally(new IllegalStateException("controlled tool failure"));
            } else {
                toolFuture.complete("completed after resume");
            }
            var reply = server.toolReplies.poll(5, TimeUnit.SECONDS);
            assertNotNull(reply, "The admitted handler must still answer the runtime after resume");
            assertEquals(original.getSessionId(), reply.path("params").path("sessionId").asText());
            assertEquals("resumed-tool", reply.path("params").path("requestId").asText());
            if (outcome.equals("failure")) {
                assertEquals("controlled tool failure", reply.path("params").path("error").asText());
            } else {
                assertEquals("completed after resume",
                        reply.path("params").path("result").path("textResultForLlm").asText());
            }
            if (outcome.equals("delivery-failure")) {
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", reply.get("id"), "error",
                        Map.of("code", -32603, "message", "controlled delivery failure")));
                reply = server.toolReplies.poll(5, TimeUnit.SECONDS);
                assertNotNull(reply, "An undelivered result must still answer with an error after resume");
                assertEquals("resumed-tool", reply.path("params").path("requestId").asText());
                assertEquals("controlled delivery failure", reply.path("params").path("error").asText());
            }
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", reply.get("id"), "result", Map.of()));
            assertNotNull(replacement.send("replacement remains usable").get(5, TimeUnit.SECONDS));
        } finally {
            awaitDeliveries(deliveries, "The admitted tool worker must finish");
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"stop", "force", "completion", "connection-close", "close", "close-after-second-resume"})
    void cancellationAfterResumeReachesTheDisplacedHandlesPendingTools(String cancellation) throws Exception {
        var deliveries = Executors.newSingleThreadExecutor();
        var started = new CountDownLatch(1);
        var toolFuture = new CompletableFuture<Object>();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            var original = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setDefinitions(List
                            .of(ToolDefinition.create("pending_tool", "Waits for a result", Map.of(), invocation -> {
                                started.countDown();
                                return toolFuture;
                            })))))
                    .get(5, TimeUnit.SECONDS);
            server.emitEvent(original.getSessionId(), "external_tool.requested",
                    Map.of("requestId", "resumed-tool", "sessionId", original.getSessionId(), "toolCallId", "call",
                            "toolName", "pending_tool", "arguments", Map.of()));
            assertTrue(started.await(5, TimeUnit.SECONDS));
            deliveries.submit(() -> {
            }).get(5, TimeUnit.SECONDS);
            var replacement = client.resumeSession(original.getSessionId(),
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setContinuePendingWork(true))
                    .get(5, TimeUnit.SECONDS);
            original.close();
            assertFalse(toolFuture.isCancelled(), "Resume must not cancel the admitted handler");
            if (cancellation.equals("close-after-second-resume")) {
                replacement = client.resumeSession(original.getSessionId(),
                        new ResumeSessionOptions()
                                .setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                                .setContinuePendingWork(true))
                        .get(5, TimeUnit.SECONDS);
            }

            switch (cancellation) {
                case "stop" -> client.stop().get(5, TimeUnit.SECONDS);
                case "force" -> client.forceStop().get(5, TimeUnit.SECONDS);
                case "close", "close-after-second-resume" -> closeOnOwnThread(replacement).get(5, TimeUnit.SECONDS);
                case "completion" -> server.emitEvent(original.getSessionId(), "external_tool.completed",
                        Map.of("requestId", "resumed-tool"));
                case "connection-close" -> server.socket.shutdownOutput();
                default -> throw new IllegalArgumentException(cancellation);
            }
            assertThrows(CancellationException.class, () -> toolFuture.get(5, TimeUnit.SECONDS));
            assertEquals(cancellation.equals("stop") || cancellation.startsWith("close") ? 1 : 0, server.detaches.get(),
                    "Only graceful stop or close of the current handle may detach");
        } finally {
            awaitDeliveries(deliveries, "The admitted tool worker must finish");
        }
    }

    @Test
    void deletingSessionRetiresItsHandleWithoutDetachingAReplacement() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            String sessionId = session.getSessionId();
            client.deleteSession(sessionId).get(5, TimeUnit.SECONDS);
            assertThrows(IllegalStateException.class, () -> session.on(event -> fail("Deleted session callback")));
            var replacement = client
                    .resumeSession(sessionId,
                            new ResumeSessionOptions().setPermissions(
                                    new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get(5, TimeUnit.SECONDS);
            session.close();
            assertNotNull(replacement.send("replacement still attached").get(5, TimeUnit.SECONDS));
            assertEquals(0, server.detaches.get(), "Closing a deleted handle must not detach its replacement");
        }
    }

    @Test
    void deletingSessionAllowsToolCancellationCleanupToCallRpc() throws Exception {
        var deliveries = Executors.newSingleThreadExecutor();
        var started = new CountDownLatch(1);
        var cleanup = new CompletableFuture<Void>();
        var toolFuture = new CompletableFuture<Object>();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            toolFuture.whenComplete((result, error) -> {
                try {
                    client.ping("tool cancellation cleanup").get(5, TimeUnit.SECONDS);
                    cleanup.complete(null);
                } catch (Exception failure) {
                    cleanup.completeExceptionally(failure);
                }
            });
            var session = client.createSession(new CreateSessionOptions().setSessionId("delete-pending-tool")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setDefinitions(
                            List.of(ToolDefinition.create("blocked_tool", "Blocks", Map.of(), invocation -> {
                                started.countDown();
                                return toolFuture;
                            })))))
                    .get(5, TimeUnit.SECONDS);
            server.emitEvent(session.getSessionId(), "external_tool.requested",
                    Map.of("requestId", "pending-tool", "sessionId", session.getSessionId(), "toolCallId", "call",
                            "toolName", "blocked_tool", "arguments", Map.of()));
            assertTrue(started.await(5, TimeUnit.SECONDS));
            // The single worker's barrier runs after the tool future has been attached.
            deliveries.submit(() -> {
            }).get(5, TimeUnit.SECONDS);

            var deletion = client.deleteSession(session.getSessionId());
            cleanup.get(5, TimeUnit.SECONDS);
            deletion.get(5, TimeUnit.SECONDS);
            assertTrue(toolFuture.isCancelled());
            assertNotNull(client.ping("reader remains reachable").get(5, TimeUnit.SECONDS));
        } finally {
            awaitDeliveries(deliveries, "The tool worker must finish");
        }
    }

    @Test
    void deletingSessionDoesNotRetireOrUnregisterConcurrentReplacement() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            String sessionId = session.getSessionId();
            server.holdDeleteResponse = true;
            var deletion = client.deleteSession(sessionId);
            var request = server.heldDeletes.poll(5, TimeUnit.SECONDS);
            assertNotNull(request, "Deletion must be pending before creating the replacement");
            var replacement = client
                    .createSession(new CreateSessionOptions().setSessionId(sessionId)
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get(5, TimeUnit.SECONDS);
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result", Map.of("success", true)));
            deletion.get(5, TimeUnit.SECONDS);

            var delivered = new CountDownLatch(1);
            try (var subscription = replacement.on(SessionIdleEvent.class, event -> delivered.countDown())) {
                assertNotNull(replacement.send("replacement still attached").get(5, TimeUnit.SECONDS));
                server.emitIdle(sessionId);
                assertTrue(delivered.await(5, TimeUnit.SECONDS), "The replacement must remain registered");
            }
            assertThrows(IllegalStateException.class, () -> session.on(event -> fail("Deleted session callback")));
            session.close();
            assertEquals(0, server.detaches.get(), "Retiring the deleted handle must not detach its replacement");
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"before-delete", "after-delete", "replacement"})
    void deletingSessionRetiresResidentDisplacedByFailedResume(String ordering) throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var resident = createSession(client);
            String sessionId = resident.getSessionId();
            server.holdResumeResponse = true;
            var resuming = client.resumeSession(sessionId, new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            var resumeRequest = server.heldResumes.poll(5, TimeUnit.SECONDS);
            assertNotNull(resumeRequest, "Resume must displace the resident before deletion starts");
            server.holdDeleteResponse = true;
            var deletion = client.deleteSession(sessionId);
            var deleteRequest = server.heldDeletes.poll(5, TimeUnit.SECONDS);
            assertNotNull(deleteRequest, "Deletion must capture the provisional resume handle");

            CopilotSession replacement = ordering.equals("replacement")
                    ? client.createSession(new CreateSessionOptions().setSessionId(sessionId)
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                            .get(5, TimeUnit.SECONDS)
                    : null;
            if (!ordering.equals("before-delete")) {
                server.writeMessage(
                        Map.of("jsonrpc", "2.0", "id", deleteRequest.get("id"), "result", Map.of("success", true)));
                deletion.get(5, TimeUnit.SECONDS);
            }
            server.writeMessage(Map.of("jsonrpc", "2.0", "id", resumeRequest.get("id"), "error",
                    Map.of("code", -32603, "message", "controlled resume failure")));
            var failure = assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
            assertTrue(failure.getCause().getMessage().contains("controlled resume failure"), failure.toString());
            if (ordering.equals("before-delete")) {
                server.writeMessage(
                        Map.of("jsonrpc", "2.0", "id", deleteRequest.get("id"), "result", Map.of("success", true)));
                deletion.get(5, TimeUnit.SECONDS);
            }

            assertThrows(IllegalStateException.class, () -> resident.on(event -> fail("Deleted resident callback")));
            if (replacement != null) {
                var delivered = new CountDownLatch(1);
                try (var subscription = replacement.on(SessionIdleEvent.class, event -> delivered.countDown())) {
                    server.emitIdle(sessionId);
                    assertTrue(delivered.await(5, TimeUnit.SECONDS), "A newer replacement must remain registered");
                }
            } else {
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", 9_000_001L, "method", "hooks.invoke", "params",
                        Map.of("sessionId", sessionId, "hookType", "sessionEnd", "input",
                                Map.of("sessionId", sessionId, "timestamp", 0, "cwd", ".", "reason", "user_exit"))));
                var reply = server.hookReplies.poll(5, TimeUnit.SECONDS);
                assertNotNull(reply, "The SDK must acknowledge the session-end hook");
                assertEquals("Unknown session " + sessionId, reply.path("error").path("message").asText(),
                        "A failed resume must not restore a deleted registration");
            }
            assertEquals(0, server.detaches.get(), "Local retirement must not detach a newer handle");
        }
    }

    private static void awaitCollected(WeakReference<?> reference, String message) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (reference.get() != null && System.nanoTime() < deadline) {
            System.gc();
            Thread.sleep(10);
        }
        assertNull(reference.get(), message);
    }

    void forceStopEventuallyInterruptsOwnedHandlers() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var interrupted = new CountDownLatch(1);
        var finished = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            try (var subscription = session.on(event -> {
                entered.countDown();
                try {
                    release.await();
                } catch (InterruptedException error) {
                    interrupted.countDown();
                    Thread.currentThread().interrupt();
                } finally {
                    finished.countDown();
                }
            })) {
                try {
                    server.emitIdle(session.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS));
                    client.forceStop().get(5, TimeUnit.SECONDS);
                    assertEquals(1, release.getCount(), "Force stop must not join the owned handler");
                    assertTrue(interrupted.await(CopilotClient.AUTOCLOSEABLE_TIMEOUT_SECONDS + 5, TimeUnit.SECONDS),
                            "Forced shutdown must eventually interrupt a stuck SDK-owned handler");
                } finally {
                    release.countDown();
                    assertTrue(finished.await(5, TimeUnit.SECONDS), "The owned handler must finish");
                }
            }
        }
    }

    @Test
    void handlerErrorDoesNotStrandCorrelatedCompletionWithoutTimeout() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.completionSupported = true;
            try (var session = createSession(client); var subscription = session.on(SessionIdleEvent.class, event -> {
                throw new AssertionError("terminal handler failed");
            })) {
                var waiting = session.sendAndWait(new MessageOptions().setPrompt("question"), 0);
                var request = server.completions.poll(5, TimeUnit.SECONDS);
                assertNotNull(request);
                var idle = server.emitEvent(session.getSessionId(), "session.idle", Map.of());
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                        Map.of("messageId", "owned-request", "aborted", false, "completionEventId", idle.get("id"))));
                assertNull(waiting.get(5, TimeUnit.SECONDS),
                        "A failed user handler must not strand the internal completion boundary");
            }
        }
    }

    @ParameterizedTest
    @CsvSource({"false,false", "false,true", "true,false", "true,true"})
    void handlerFailureDoesNotStrandLegacyCompletionWithoutTimeout(boolean typed, boolean fatal) throws Exception {
        var failures = new AtomicInteger();
        try (var server = new Server();
                var client = server.createClient();
                var session = createSession(client);
                var subscription = session.on(event -> {
                    if (event instanceof com.microsoft.copilot.generated.AssistantMessageEvent
                            || event instanceof SessionIdleEvent) {
                        failures.incrementAndGet();
                        if (fatal) {
                            throw new AssertionError("response handler failed");
                        }
                        throw new IllegalStateException("response handler failed");
                    }
                })) {
            var options = new MessageOptions().setPrompt("question");
            CompletableFuture<?> waiting = typed
                    ? session.sendAndWait(options, Answer.class, 0)
                    : session.sendAndWait(options, 0);
            assertEquals("question", server.sends.poll(5, TimeUnit.SECONDS),
                    "The unsupported completion RPC must fall back to a legacy send");
            server.emitEvent(session.getSessionId(), "assistant.message",
                    Map.of("messageId", "answer", "content", "{\"value\":42}", "originatingMessageId", "question"));
            server.emitIdle(session.getSessionId());

            var response = waiting.get(5, TimeUnit.SECONDS);
            if (typed) {
                assertEquals(new Answer(42), response);
            } else {
                assertEquals("{\"value\":42}",
                        assertInstanceOf(com.microsoft.copilot.generated.AssistantMessageEvent.class, response)
                                .getData().content());
            }
            assertEquals(2, failures.get(),
                    "Both response tracking and terminal observers must survive handler failures");
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void gracefulCloseDrainsAcceptedEventsWithoutJoiningTheBusyHandler(boolean stopClient) throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var delivered = new java.util.concurrent.LinkedBlockingQueue<String>();
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            try (var first = session.on(event -> {
                entered.countDown();
                awaitRelease(release);
            }); var second = session.on(event -> delivered.add(event.getId().toString()))) {
                try {
                    var a = server.emitEvent(session.getSessionId(), "session.idle", Map.of());
                    assertTrue(entered.await(5, TimeUnit.SECONDS));
                    var b = server.emitEvent(session.getSessionId(), "session.idle", Map.of());
                    if (stopClient) {
                        client.stop().get(5, TimeUnit.SECONDS);
                    } else {
                        closeOnOwnThread(session).get(5, TimeUnit.SECONDS);
                    }
                    assertEquals(1, release.getCount(), "Close must not join an admitted event handler");
                    release.countDown();
                    assertEquals(a.get("id"), delivered.poll(5, TimeUnit.SECONDS));
                    assertEquals(b.get("id"), delivered.poll(5, TimeUnit.SECONDS),
                            "Events accepted before the detach ACK must reach every subscriber");
                } finally {
                    release.countDown();
                }
            }
        }
    }

    @Test
    void admittedHandlerCanWaitForForceStopWithTheDefaultExecutor() throws Exception {
        var stopped = new CompletableFuture<Void>();
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            try (var subscription = session.on(event -> {
                try {
                    client.forceStop().join();
                    stopped.complete(null);
                } catch (Throwable failure) {
                    stopped.completeExceptionally(failure);
                }
            })) {
                server.emitIdle(session.getSessionId());
                stopped.get(5, TimeUnit.SECONDS);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void admittedHandlerCanWaitForGracefulStopWithInlineExecutors(boolean rejected) throws Exception {
        var reject = new AtomicBoolean();
        Executor executor = task -> {
            if (reject.get()) {
                throw new java.util.concurrent.RejectedExecutionException("Controlled executor rejection");
            }
            task.run();
        };
        var stopped = new CompletableFuture<Void>();
        try (var server = new Server(); var client = server.createClient(executor)) {
            var session = createSession(client);
            try (var subscription = session.on(event -> {
                try {
                    client.stop().get(5, TimeUnit.SECONDS);
                    stopped.complete(null);
                } catch (Exception failure) {
                    stopped.completeExceptionally(failure);
                }
            })) {
                reject.set(rejected);
                server.emitIdle(session.getSessionId());
                stopped.get(10, TimeUnit.SECONDS);
            }
        }
    }

    @Test
    void commonPoolExecutorDeliversCompletionEvents() throws Exception {
        try (var server = new Server();
                var client = server.createClient(java.util.concurrent.ForkJoinPool.commonPool())) {
            server.completionSupported = true;
            try (var session = createSession(client)) {
                var waiting = session.sendAndWait(new MessageOptions().setPrompt("question"), 0);
                var request = server.completions.poll(5, TimeUnit.SECONDS);
                assertNotNull(request);
                var idle = server.emitEvent(session.getSessionId(), "session.idle", Map.of());
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                        Map.of("messageId", "owned-request", "aborted", false, "completionEventId", idle.get("id"))));
                assertNull(waiting.get(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test
    void failedResumePreservesClosingResidentBeforeDetach() throws Exception {
        var deliveries = Executors.newSingleThreadExecutor();
        var delivered = new CountDownLatch(1);
        var hooks = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            server.holdDetachResponse = true;
            var session = client.createSession(new CreateSessionOptions().setSessionId("closing-resident")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setHooks(new SessionHookOptions()
                            .setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                hooks.incrementAndGet();
                                return CompletableFuture.completedFuture(null);
                            }))))
                    .get(5, TimeUnit.SECONDS);
            try (var subscription = session.on(SessionIdleEvent.class, event -> delivered.countDown())) {
                var closing = closeOnOwnThread(session);
                var detach = server.heldDetaches.poll(5, TimeUnit.SECONDS);
                assertNotNull(detach, "Close must reach detach before resume");
                try {
                    server.rejectSessionRequests = true;
                    var resuming = client.resumeSession(session.getSessionId(),
                            new ResumeSessionOptions().setPermissions(
                                    new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
                    var failure = assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
                    assertTrue(failure.getCause().getMessage().contains("controlled session rejection"),
                            failure.toString());

                    server.invokeSessionEnd(session.getSessionId());
                    assertEquals(1, hooks.get(), "Failed resume must preserve the closing resident's hooks");
                    server.emitIdle(session.getSessionId());
                    assertTrue(delivered.await(5, TimeUnit.SECONDS),
                            "Failed resume must preserve event routing until detach completes");
                } finally {
                    server.completeDetach(detach, "success");
                    closing.get(5, TimeUnit.SECONDS);
                }
                assertEquals(1, server.detaches.get(), "Graceful close must still detach once");
            }
        } finally {
            awaitDeliveries(deliveries, "The event worker must finish");
        }
    }

    @Test
    void gracefulClosePreservesEventsAndSessionEndHooksUntilDetachCompletes() throws Exception {
        var events = new AtomicInteger();
        var delivered = new java.util.concurrent.LinkedBlockingQueue<Integer>();
        var hooks = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient()) {
            server.emitDetachCallbacks = true;
            var session = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setHooks(new SessionHookOptions()
                            .setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                assertEquals("user_exit", input.reason());
                                hooks.incrementAndGet();
                                return CompletableFuture.completedFuture(null);
                            }))))
                    .get(5, TimeUnit.SECONDS);
            try (var subscription = session.on(event -> delivered.add(events.incrementAndGet()))) {
                session.close();
                assertEquals(1, server.detaches.get());
                assertEquals(1, hooks.get(), "Ordinary detach must still invoke the session-end hook");
                assertEquals(1, delivered.poll(5, TimeUnit.SECONDS),
                        "Ordinary detach must deliver events before callback retirement");
                server.emitIdle(session.getSessionId());
                client.ping("retired detach callbacks barrier").get(5, TimeUnit.SECONDS);
                assertEquals(1, events.get(), "Detach completion must retire subsequent callbacks");
                assertThrows(IllegalStateException.class, () -> session.on(event -> fail("Retired handler")));
                session.close();
                assertEquals(1, server.detaches.get(), "Repeated close must not detach again");
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void resumeSettlementPreservesTheRuntimeCallbackOwnerWhileResidentCloses(boolean unsupportedRecovery)
            throws Exception {
        var events = new AtomicInteger();
        var hooks = new AtomicInteger();
        var resumedEvents = new AtomicInteger();
        var resumedHooks = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient()) {
            var resident = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setHooks(new SessionHookOptions()
                            .setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                assertEquals("user_exit", input.reason());
                                hooks.incrementAndGet();
                                return CompletableFuture.completedFuture(null);
                            }))))
                    .get(5, TimeUnit.SECONDS);
            String sessionId = resident.getSessionId();
            try (var subscription = resident.on(event -> events.incrementAndGet())) {
                server.holdResumeResponse = true;
                server.holdDetachResponse = true;
                var resuming = client.resumeSession(sessionId, new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                        .setInteraction(new SessionInteractionOptions().setAutoModeRecovery(unsupportedRecovery))
                        .setEvents(new SessionEventOptions().setOnEvent(event -> resumedEvents.incrementAndGet()))
                        .setHooks(new ResumeSessionOptions()
                                .setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                                .setInteraction(
                                        new SessionInteractionOptions().setAutoModeRecovery(unsupportedRecovery))
                                .setEvents(
                                        new SessionEventOptions().setOnEvent(event -> resumedEvents.incrementAndGet()))
                                .getHooks().setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                    resumedHooks.incrementAndGet();
                                    return CompletableFuture.completedFuture(null);
                                })))
                        .setInteraction(new ResumeSessionOptions()
                                .setPermissions(
                                        new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                                .setInteraction(
                                        new SessionInteractionOptions().setAutoModeRecovery(unsupportedRecovery))
                                .setEvents(
                                        new SessionEventOptions().setOnEvent(event -> resumedEvents.incrementAndGet()))
                                .getInteraction().setOnAutoModeSwitchRequest((request, invocation) -> CompletableFuture
                                        .completedFuture(AutoModeSwitchResponse.NO))));
                var resumeRequest = server.resumes.poll(5, TimeUnit.SECONDS);
                assertNotNull(resumeRequest, "Resume must be pending before the resident closes");
                var closing = CompletableFuture.runAsync(resident::close);
                try (var finishDetach = (AutoCloseable) () -> {
                    var detachRequest = server.heldDetaches.poll(5, TimeUnit.SECONDS);
                    assertNotNull(detachRequest, "Graceful close must send detach");
                    server.completeDetach(detachRequest, "success");
                    closing.get(5, TimeUnit.SECONDS);
                }) {
                    assertTrue(server.detachEntered.await(5, TimeUnit.SECONDS), "Graceful close must await detach");
                    assertFalse(closing.isDone());
                    if (unsupportedRecovery) {
                        server.writeMessage(Map.of("jsonrpc", "2.0", "id", resumeRequest.get("id"), "result",
                                Map.of("sessionId", sessionId)));
                    } else {
                        server.writeMessage(Map.of("jsonrpc", "2.0", "id", resumeRequest.get("id"), "error",
                                Map.of("code", -32000, "message", "resume failed")));
                    }
                    var error = assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
                    if (unsupportedRecovery) {
                        assertInstanceOf(UnsupportedOperationException.class, error.getCause());
                    } else {
                        assertTrue(error.getCause().getMessage().contains("resume failed"));
                    }

                    server.emitIdle(sessionId);
                    // Delivery is asynchronous; wait for the owner that must receive the event.
                    awaitCount(unsupportedRecovery ? resumedEvents : events, 1,
                            "An accepted resume must keep its callbacks even without recovery support");
                    server.invokeSessionEnd(sessionId);
                    assertEquals(unsupportedRecovery ? 0 : 1, hooks.get());
                    assertEquals(unsupportedRecovery ? 0 : 1, events.get());
                    assertEquals(unsupportedRecovery ? 1 : 0, resumedHooks.get());
                    assertEquals(unsupportedRecovery ? 1 : 0, resumedEvents.get());
                    assertFalse(closing.isDone(), "Callbacks must complete before detach is acknowledged");
                }
                server.emitIdle(sessionId);
                client.ping("restored resident callback retirement barrier").get(5, TimeUnit.SECONDS);
                if (unsupportedRecovery) {
                    awaitCount(resumedEvents, 2,
                            "The retired resident must not remove an accepted resume's callback owner");
                }
                assertEquals(unsupportedRecovery ? 0 : 1, events.get(),
                        "Detach acknowledgement must still retire the resident callbacks");
                assertEquals(unsupportedRecovery ? 2 : 0, resumedEvents.get(),
                        "The retired resident must not remove an accepted resume's callback owner");
                assertEquals(1, server.detaches.get(), "Rejected resume must not detach the resident a second time");
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"success", "rejected", "rpc-error"})
    void timedOutClosePreservesEventsUntilDetachAcknowledgement(String outcome) throws Exception {
        var events = new AtomicInteger();
        var delivered = new java.util.concurrent.LinkedBlockingQueue<Integer>();
        var hooks = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient()) {
            server.holdDetachResponse = true;
            var session = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setHooks(new SessionHookOptions()
                            .setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                assertEquals("user_exit", input.reason());
                                hooks.incrementAndGet();
                                return CompletableFuture.completedFuture(null);
                            }))))
                    .get(5, TimeUnit.SECONDS);
            try (var subscription = session.on(event -> delivered.add(events.incrementAndGet()))) {
                var closing = closeOnOwnThread(session);
                var detach = server.heldDetaches.poll(5, TimeUnit.SECONDS);
                assertNotNull(detach, "The peer must receive the actual detach request");
                try (var acknowledgement = (AutoCloseable) () -> {
                    server.completeDetach(detach, outcome);
                    client.ping("detach acknowledgement barrier").get(5, TimeUnit.SECONDS);
                }) {
                    server.emitIdle(session.getSessionId());
                    assertEquals(1, delivered.poll(5, TimeUnit.SECONDS));
                    var error = assertThrows(ExecutionException.class, () -> closing.get(10, TimeUnit.SECONDS));
                    var closeError = assertInstanceOf(IllegalStateException.class, error.getCause());
                    assertEquals("Failed to detach session " + session.getSessionId(), closeError.getMessage());
                    assertInstanceOf(TimeoutException.class, closeError.getCause());

                    server.emitIdle(session.getSessionId());
                    assertEquals(2, delivered.poll(5, TimeUnit.SECONDS),
                            "A caller timeout must not retire callbacks before the detach ACK");
                    server.invokeSessionEnd(session.getSessionId());
                    assertEquals(1, hooks.get(), "The session-end hook must survive the caller's timeout too");
                    session.close();
                    assertEquals(1, server.detaches.get(), "Timed-out close must not issue another detach");
                }
                server.emitIdle(session.getSessionId());
                client.ping("after detach acknowledgement barrier").get(5, TimeUnit.SECONDS);
                assertEquals(2, events.get(), "The actual detach ACK must retire callbacks");
            }
        }
    }

    @Test
    void interruptedCloseKeepsCallbacksAndLateAcknowledgementDoesNotRetireReplacement() throws Exception {
        var events = new AtomicInteger();
        var replacementEvents = new AtomicInteger();
        var delivered = new java.util.concurrent.LinkedBlockingQueue<Integer>();
        var replacementDelivered = new java.util.concurrent.LinkedBlockingQueue<Integer>();
        var closeFailure = new AtomicReference<RuntimeException>();
        var interrupted = new AtomicBoolean();
        try (var server = new Server(); var client = server.createClient()) {
            server.holdDetachResponse = true;
            var session = createSession(client);
            var closing = new Thread(() -> {
                try {
                    session.close();
                } catch (RuntimeException error) {
                    closeFailure.set(error);
                    interrupted.set(Thread.currentThread().isInterrupted());
                }
            }, "interrupted-session-close");
            try (var subscription = session.on(event -> delivered.add(events.incrementAndGet()))) {
                var detach = new AtomicReference<JsonNode>();
                try (var join = (AutoCloseable) () -> {
                    closing.join(10000);
                    assertFalse(closing.isAlive(), "The owned close thread must stop");
                }; var acknowledgement = (AutoCloseable) () -> {
                    if (detach.get() != null) {
                        server.completeDetach(detach.get(), "success");
                    }
                }) {
                    closing.start();
                    detach.set(server.heldDetaches.poll(5, TimeUnit.SECONDS));
                    assertNotNull(detach.get(), "The peer must admit detach before interruption");
                    closing.interrupt();
                    closing.join(5000);
                    assertFalse(closing.isAlive(), "Interrupted close must finish without waiting for the ACK");
                    var error = assertInstanceOf(IllegalStateException.class, closeFailure.get());
                    assertEquals("Failed to detach session " + session.getSessionId(), error.getMessage());
                    assertInstanceOf(InterruptedException.class, error.getCause());
                    assertTrue(interrupted.get(), "Close must restore the caller's interrupt flag");
                    server.emitIdle(session.getSessionId());
                    assertEquals(1, delivered.poll(5, TimeUnit.SECONDS),
                            "Interrupting the caller must not retire pending detach callbacks");

                    var replacement = client
                            .resumeSession(session.getSessionId(),
                                    new ResumeSessionOptions().setPermissions(
                                            new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                            .get(5, TimeUnit.SECONDS);
                    try (var replacementSubscription = replacement
                            .on(event -> replacementDelivered.add(replacementEvents.incrementAndGet()))) {
                        server.emitIdle(replacement.getSessionId());
                        assertEquals(1, replacementDelivered.poll(5, TimeUnit.SECONDS));
                        server.completeDetach(detach.get(), "success");
                        detach.set(null);
                        server.emitIdle(replacement.getSessionId());
                        assertEquals(2, replacementDelivered.poll(5, TimeUnit.SECONDS),
                                "An old detach completion must not retire the replacement's callbacks");
                        assertEquals(1, events.get(), "New-generation events must not reach old subscriptions");
                        client.forceStop().get(5, TimeUnit.SECONDS);
                    }
                }
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void transportFailureSettlesPendingDetach(boolean timeoutFirst) throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.holdDetachResponse = true;
            var session = createSession(client);
            var closing = closeOnOwnThread(session);
            assertNotNull(server.heldDetaches.poll(5, TimeUnit.SECONDS), "Detach must be pending on the real peer");
            if (timeoutFirst) {
                var error = assertThrows(ExecutionException.class, () -> closing.get(10, TimeUnit.SECONDS));
                assertInstanceOf(TimeoutException.class, error.getCause().getCause());
            }
            server.socket.shutdownOutput();
            if (!timeoutFirst) {
                var error = assertThrows(ExecutionException.class, () -> closing.get(5, TimeUnit.SECONDS));
                var closeError = assertInstanceOf(IllegalStateException.class, error.getCause());
                var rpcError = assertInstanceOf(ExecutionException.class, closeError.getCause());
                assertInstanceOf(IOException.class, rpcError.getCause());
                assertEquals("Client closed", rpcError.getCause().getMessage());
            }
            var pingError = assertThrows(ExecutionException.class,
                    () -> client.ping("closed transport barrier").get(5, TimeUnit.SECONDS));
            assertInstanceOf(IOException.class, pingError.getCause());
            assertThrows(IllegalStateException.class, () -> session.send("after transport failure"));
            session.close();
            assertEquals(1, server.detaches.get(), "A settled failed detach must not be sent again");
            client.forceStop().get(5, TimeUnit.SECONDS);
        }
    }

    @Test
    void forceStopAfterCloseTimeoutDropsUnadmittedCallbacks() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        var laterCalls = new AtomicInteger();
        var errorCalls = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            server.holdDetachResponse = true;
            var session = createSession(client);
            session.setEventErrorHandler((event, error) -> errorCalls.incrementAndGet());
            try (var first = session.on(event -> {
                entered.countDown();
                awaitRelease(release);
                throw new IllegalStateException("Admitted callback failed after forced retirement");
            }); var second = session.on(event -> laterCalls.incrementAndGet())) {
                var closing = closeOnOwnThread(session);
                assertNotNull(server.heldDetaches.poll(5, TimeUnit.SECONDS));
                var error = assertThrows(ExecutionException.class, () -> closing.get(10, TimeUnit.SECONDS));
                assertInstanceOf(TimeoutException.class, error.getCause().getCause());
                try (var gate = (AutoCloseable) () -> {
                    release.countDown();
                    awaitDeliveries(deliveries, "The admitted callback must finish after its release");
                }) {
                    server.emitIdle(session.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS), "Post-timeout callback must still be admitted");
                    client.forceStop().get(5, TimeUnit.SECONDS);
                    assertEquals(1, release.getCount(), "Force stop must not join the admitted callback");
                }
                assertEquals(0, laterCalls.get(), "Force stop must drop the remaining dispatch snapshot");
                assertEquals(0, errorCalls.get(), "Force stop must also retire the error callback");
                session.close();
                assertEquals(1, server.detaches.get(), "Force stop must not resend the pending detach");
            }
        }
    }

    @Test
    void forceStopTerminatesRetainedSessionsWithoutDetachRpc() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            client.forceStop().get(5, TimeUnit.SECONDS);

            assertThrows(IllegalStateException.class, () -> session.on(event -> fail("Retired handler")));
            assertThrows(IllegalStateException.class,
                    () -> session.on(SessionIdleEvent.class, event -> fail("Retired typed handler")));
            assertThrows(IllegalStateException.class, () -> session.send("after force stop"));
            session.close();
            assertEquals(0, server.detaches.get(), "Local retirement must not detach over RPC");
        }
    }

    @Test
    void forceStopDropsRemainingSnapshotHandlersWithoutJoiningAnAdmittedHandler() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        var laterCalls = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            var session = createSession(client);
            try (var first = session.on(event -> {
                entered.countDown();
                awaitRelease(release);
            }); var second = session.on(event -> laterCalls.incrementAndGet())) {
                try {
                    server.emitIdle(session.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS), "The first handler must be admitted");
                    client.forceStop().get(5, TimeUnit.SECONDS);
                    assertEquals(1, release.getCount(), "Force stop must not require releasing the user handler");
                } finally {
                    release.countDown();
                    awaitDeliveries(deliveries, "The admitted handler must finish after its release");
                }
                assertEquals(0, laterCalls.get(), "Retirement must discard the retained dispatch snapshot");
            }
        }
    }

    @Test
    void forceStopRetiresErrorCallbacksWhileGracefulCloseAwaitsDetach() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        var errorCalls = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(deliveries)) {
            server.holdDetachResponse = true;
            var session = createSession(client);
            session.setEventErrorHandler((event, error) -> errorCalls.incrementAndGet());
            try (var first = session.on(event -> {
                entered.countDown();
                awaitRelease(release);
                throw new IllegalStateException("Admitted handler failed after forced retirement");
            })) {
                try {
                    server.emitIdle(session.getSessionId());
                    assertTrue(entered.await(5, TimeUnit.SECONDS), "The first snapshot handler must be admitted");
                    var closing = closeOnOwnThread(session);
                    assertTrue(server.detachEntered.await(5, TimeUnit.SECONDS), "Graceful close must await detach");
                    client.forceStop().get(5, TimeUnit.SECONDS);
                    var detachError = assertThrows(ExecutionException.class, () -> closing.get(5, TimeUnit.SECONDS));
                    assertInstanceOf(IllegalStateException.class, detachError.getCause());
                    assertEquals(1, release.getCount(), "Shutdown must not join the admitted callback");
                    release.countDown();
                    awaitDeliveries(deliveries, "The admitted handler must finish after its release");
                    assertEquals(0, errorCalls.get(), "Force stop must discard the unadmitted error callback");
                } finally {
                    release.countDown();
                    awaitDeliveries(deliveries, "The admitted handler must finish after forced cleanup");
                }
            }
        }
    }

    @ParameterizedTest
    @CsvSource({"false, false, false", "false, true, false", "true, false, false", "true, true, false",
            "false, false, true", "false, true, true", "true, false, true", "true, true, true"})
    void retirementPreservesTerminalOutcomeWhileSchedulerHandoffIsBlocked(boolean terminalError, boolean force,
            boolean firstFallback) throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            server.completionSupported = firstFallback;
            var occupying = session.sendAndWait(new MessageOptions().setPrompt("occupy scheduler"), 0);
            var continuation = occupying.whenComplete((value, error) -> {
                entered.countDown();
                awaitRelease(release);
            });
            try (var gate = (AutoCloseable) release::countDown) {
                client.ping("first send acknowledgment").get(5, TimeUnit.SECONDS);
                if (firstFallback) {
                    var request = server.completions.poll(5, TimeUnit.SECONDS);
                    assertNotNull(request, "The occupying call must use the supported completion RPC");
                    server.completionSupported = false;
                    server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                            Map.of("messageId", "occupying-request", "aborted", false)));
                } else {
                    server.emitEvent(session.getSessionId(), "user.message",
                            Map.of("messageId", "occupy scheduler", "content", "occupy scheduler"));
                    server.emitIdle(session.getSessionId());
                }
                assertTrue(entered.await(5, TimeUnit.SECONDS), "A continuation must occupy the session scheduler");

                var completed = session.sendAndWait(new MessageOptions().setPrompt("completed terminal"), 0);
                if (firstFallback) {
                    assertEquals("completed terminal", server.sends.poll(5, TimeUnit.SECONDS),
                            "The first fallback must admit its legacy wait before the terminal event");
                }
                client.ping("second send acknowledgment").get(5, TimeUnit.SECONDS);
                var terminalDelivered = new CompletableFuture<Void>();
                try (var subscription = session.on(event -> {
                    if (event instanceof SessionIdleEvent
                            || event instanceof com.microsoft.copilot.generated.SessionErrorEvent) {
                        terminalDelivered.complete(null);
                    }
                })) {
                    if (terminalError) {
                        server.emitEvent(session.getSessionId(), "session.error",
                                Map.of("errorType", "query", "message", "original terminal failure"));
                    } else {
                        server.emitEvent(session.getSessionId(), "assistant.message",
                                Map.of("messageId", "terminal-message", "content", "completed before retirement",
                                        "originatingMessageId", "completed terminal"));
                        server.emitIdle(session.getSessionId());
                    }
                    terminalDelivered.get(5, TimeUnit.SECONDS);
                }
                assertFalse(completed.isDone(), "The received terminal outcome must still await scheduler handoff");

                if (force) {
                    client.forceStop().get(5, TimeUnit.SECONDS);
                } else {
                    closeOnOwnThread(session).get(5, TimeUnit.SECONDS);
                }
                if (terminalError) {
                    var error = assertThrows(ExecutionException.class, () -> completed.get(5, TimeUnit.SECONDS));
                    assertEquals("Session error: original terminal failure", error.getCause().getMessage());
                } else {
                    assertEquals("completed before retirement", completed.get(5, TimeUnit.SECONDS).getData().content());
                }
                assertEquals(1, release.getCount(), "Retirement must not join the admitted continuation");
            }
            continuation.get(5, TimeUnit.SECONDS);
        }
    }

    @Test
    void forceStopFailsAcknowledgedPlainAndStructuredWaitsWithNoTimeout() throws Exception {
        var entered = new CountDownLatch(2);
        var release = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            var plain = session.sendAndWait(new MessageOptions().setPrompt("plain"), 0);
            var structured = session.sendAndWait(new MessageOptions().setPrompt("structured"), Answer.class, 0);
            assertNotNull(server.sends.poll(5, TimeUnit.SECONDS), "First legacy send must be admitted");
            assertNotNull(server.sends.poll(5, TimeUnit.SECONDS), "Second legacy send must be admitted");
            client.ping("send acknowledgment barrier").get(5, TimeUnit.SECONDS);
            assertFalse(plain.isDone());
            assertFalse(structured.isDone());

            var plainContinuation = plain.whenComplete((value, error) -> {
                entered.countDown();
                awaitRelease(release);
            });
            var structuredContinuation = structured.whenComplete((value, error) -> {
                entered.countDown();
                awaitRelease(release);
            });
            try (var gate = (AutoCloseable) release::countDown) {
                client.forceStop().get(5, TimeUnit.SECONDS);
                assertTrue(entered.await(5, TimeUnit.SECONDS),
                        "Both waits must retire even while their application continuations are blocked");
                var plainError = assertThrows(ExecutionException.class, () -> plain.get(5, TimeUnit.SECONDS));
                var structuredError = assertThrows(ExecutionException.class, () -> structured.get(5, TimeUnit.SECONDS));
                assertInstanceOf(IllegalStateException.class, plainError.getCause());
                assertInstanceOf(IllegalStateException.class, structuredError.getCause());
                assertTrue(plainError.getCause().getMessage().contains("closed"));
                assertTrue(structuredError.getCause().getMessage().contains("closed"));
            }
            assertThrows(ExecutionException.class, () -> plainContinuation.get(5, TimeUnit.SECONDS));
            assertThrows(ExecutionException.class, () -> structuredContinuation.get(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void gracefulCloseFailsAcknowledgedWaitsWithoutJoiningTheirContinuations() throws Exception {
        var entered = new CountDownLatch(2);
        var release = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            var plain = session.sendAndWait(new MessageOptions().setPrompt("plain"), 0);
            var structured = session.sendAndWait(new MessageOptions().setPrompt("structured"), Answer.class, 0);
            assertNotNull(server.sends.poll(5, TimeUnit.SECONDS), "First legacy send must be admitted");
            assertNotNull(server.sends.poll(5, TimeUnit.SECONDS), "Second legacy send must be admitted");
            client.ping("send acknowledgment barrier").get(5, TimeUnit.SECONDS);
            assertFalse(plain.isDone());
            assertFalse(structured.isDone());

            var plainContinuation = plain.whenComplete((value, error) -> {
                entered.countDown();
                awaitRelease(release);
            });
            var structuredContinuation = structured.whenComplete((value, error) -> {
                entered.countDown();
                awaitRelease(release);
            });
            try (var gate = (AutoCloseable) release::countDown) {
                closeOnOwnThread(session).get(5, TimeUnit.SECONDS);
                assertTrue(entered.await(5, TimeUnit.SECONDS), "Retirement must admit both blocked continuations");
                var plainError = assertThrows(ExecutionException.class, () -> plain.get(5, TimeUnit.SECONDS));
                var structuredError = assertThrows(ExecutionException.class, () -> structured.get(5, TimeUnit.SECONDS));
                assertInstanceOf(IllegalStateException.class, plainError.getCause());
                assertInstanceOf(IllegalStateException.class, structuredError.getCause());
                assertEquals("Session closed before response completed", plainError.getCause().getMessage());
                assertEquals("Session closed before response completed", structuredError.getCause().getMessage());
                assertEquals(1, server.detaches.get(), "Graceful close must still acknowledge one detach");
                session.close();
                assertEquals(1, server.detaches.get(), "Repeated close must not detach again");
            }
            assertThrows(ExecutionException.class, () -> plainContinuation.get(5, TimeUnit.SECONDS));
            assertThrows(ExecutionException.class, () -> structuredContinuation.get(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void duplicateUntypedSubscriptionsStillDispatchOnceAndShareUnsubscription() throws Exception {
        var calls = new AtomicInteger();
        var delivered = new java.util.concurrent.LinkedBlockingQueue<Integer>();
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            Consumer<SessionEvent> handler = event -> delivered.add(calls.incrementAndGet());
            try (var first = session.on(handler); var second = session.on(handler)) {
                server.emitIdle(session.getSessionId());
                assertEquals(1, delivered.poll(5, TimeUnit.SECONDS));
                assertEquals(1, calls.get());

                first.close();
                var afterUnsubscribe = new CompletableFuture<Void>();
                try (var observer = session.on(event -> afterUnsubscribe.complete(null))) {
                    server.emitIdle(session.getSessionId());
                    afterUnsubscribe.get(5, TimeUnit.SECONDS);
                }
                assertEquals(1, calls.get());
                client.forceStop().get(5, TimeUnit.SECONDS);
            }
        }
    }

    @Test
    void queuedEventsRetainTheirOriginalSubscribersButHonorUnsubscription() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        var removedCalls = new AtomicInteger();
        var received = new java.util.concurrent.CopyOnWriteArrayList<String>();
        var delivered = new CompletableFuture<Void>();
        try (var server = new Server();
                var client = server.createClient(deliveries);
                var session = createSession(client);
                var first = session.on(SessionIdleEvent.class, event -> {
                    entered.countDown();
                    awaitRelease(release);
                });
                var removed = session.on(event -> removedCalls.incrementAndGet())) {
            try {
                server.emitIdle(session.getSessionId());
                assertTrue(entered.await(5, TimeUnit.SECONDS), "The first handler must be running");
                server.emitIdle(session.getSessionId());
                client.ping("queued event read barrier").get(5, TimeUnit.SECONDS);
                removed.close();
                try (var later = session.on(event -> {
                    received.add(event.getId().toString());
                    delivered.complete(null);
                })) {
                    var fresh = server.emitEvent(session.getSessionId(), "session.idle", Map.of());
                    release.countDown();
                    delivered.get(5, TimeUnit.SECONDS);
                    assertEquals(List.of(fresh.get("id")), received,
                            "A new subscriber must not receive events accepted before registration");
                    assertEquals(0, removedCalls.get(), "Unsubscription must still suppress queued callbacks");
                }
            } finally {
                release.countDown();
            }
        } finally {
            awaitDeliveries(deliveries, "The event executor must stop");
        }
    }

    @Test
    void legacyWaitIgnoresErrorsQueuedBeforeItsSubscription() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var deliveries = Executors.newCachedThreadPool();
        try (var server = new Server();
                var client = server.createClient(deliveries);
                var session = createSession(client);
                var first = session.on(SessionIdleEvent.class, event -> {
                    entered.countDown();
                    awaitRelease(release);
                })) {
            try {
                server.emitIdle(session.getSessionId());
                assertTrue(entered.await(5, TimeUnit.SECONDS), "The first handler must be running");
                server.emitEvent(session.getSessionId(), "session.error",
                        Map.of("errorType", "query", "message", "stale failure"));
                client.ping("stale error read barrier").get(5, TimeUnit.SECONDS);

                var waiting = session.sendAndWait(new MessageOptions().setPrompt("new request"), 0);
                assertEquals("new request", server.sends.poll(5, TimeUnit.SECONDS),
                        "The legacy wait must subscribe before its response events arrive");
                server.emitEvent(session.getSessionId(), "assistant.message", Map.of("messageId", "new response",
                        "content", "{\"value\":42}", "originatingMessageId", "new request"));
                server.emitIdle(session.getSessionId());
                release.countDown();

                assertEquals("{\"value\":42}", waiting.get(5, TimeUnit.SECONDS).getData().content());
            } finally {
                release.countDown();
            }
        } finally {
            awaitDeliveries(deliveries, "The event executor must stop");
        }
    }

    @Test
    void stopDetachesSessionsEvenWhenTheCallersExecutorIsOccupied() throws Exception {
        var executor = Executors.newSingleThreadExecutor();
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient(executor)) {
            var session = createSession(client);
            var waiting = session.sendAndWait(new MessageOptions().setPrompt("pending legacy response"), 0);
            client.ping("legacy send acknowledged").get(5, TimeUnit.SECONDS);
            assertFalse(waiting.isDone());
            executor.execute(() -> {
                entered.countDown();
                awaitRelease(release);
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS), "The caller's executor must be occupied");

            client.stop().get(8, TimeUnit.SECONDS);
            executor.shutdownNow();
            assertThrows(IllegalStateException.class, () -> session.send("after stop"));
            var error = assertThrows(ExecutionException.class, () -> waiting.get(5, TimeUnit.SECONDS));
            assertEquals("Session closed before response completed", error.getCause().getMessage());
            assertEquals(1, server.detaches.get(), "Detach must not wait for the blocked executor");
        } finally {
            release.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS), "The owned executor must stop");
        }
    }

    @Test
    void stopDoesNotJoinToolCancellationContinuations() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var tool = new CompletableFuture<Object>();
        var continuation = tool.whenComplete((value, error) -> {
            entered.countDown();
            awaitRelease(release);
        });
        try (var server = new Server(); var client = server.createClient(Runnable::run)) {
            var session = client
                    .createSession(new CreateSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setTools(new SessionToolOptions().setDefinitions(List.of(
                                    ToolDefinition.create("blocked_tool", "Blocks", Map.of(), invocation -> tool)))))
                    .get(5, TimeUnit.SECONDS);
            var otherSession = createSession(client);
            server.emitEvent(session.getSessionId(), "external_tool.requested",
                    Map.of("requestId", "pending-tool", "sessionId", session.getSessionId(), "toolCallId", "tool-call",
                            "toolName", "blocked_tool", "arguments", Map.of()));
            client.ping("tool attached barrier").get(5, TimeUnit.SECONDS);

            var stopCall = CompletableFuture.supplyAsync(client::stop);
            try {
                var stopping = stopCall.get(2, TimeUnit.SECONDS);
                assertTrue(entered.await(5, TimeUnit.SECONDS), "Retirement must cancel the application-owned future");
                // Sessions retire on separate cleanup threads; stop completing is the barrier.
                stopping.get(8, TimeUnit.SECONDS);
                assertEquals(1, release.getCount(), "Shutdown must not join an application cancellation continuation");
                assertThrows(IllegalStateException.class, () -> session.send("after stop"));
                assertThrows(IllegalStateException.class, () -> otherSession.send("after stop"));
            } finally {
                release.countDown();
                stopCall.get(5, TimeUnit.SECONDS).get(8, TimeUnit.SECONDS);
                assertThrows(ExecutionException.class, () -> continuation.get(5, TimeUnit.SECONDS));
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    @AllowCopilotExperimental
    void stopReturnsBeforeAnInlineDetachWriteUnblocks(boolean rejected) throws Exception {
        var reject = new AtomicBoolean();
        Executor executor = task -> {
            if (reject.get()) {
                throw new java.util.concurrent.RejectedExecutionException("Controlled executor rejection");
            }
            task.run();
        };
        var block = new AtomicBoolean();
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        try (var server = new Server();
                var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                        .setRuntime(RuntimeConnection.forInProcess()).setExecutor(executor))) {
            client.setInProcessTransportFactory(options -> {
                var socket = new Socket("127.0.0.1", server.listener.getLocalPort());
                var output = new FilterOutputStream(socket.getOutputStream()) {
                    @Override
                    public void write(byte[] bytes, int offset, int length) throws IOException {
                        if (block.get()) {
                            entered.countDown();
                            awaitRelease(release);
                        }
                        out.write(bytes, offset, length);
                    }
                };
                return new CopilotClient.InProcessTransport(socket.getInputStream(), output, null);
            });
            var session = createSession(client);
            reject.set(rejected);
            block.set(true);
            var stopCall = CompletableFuture.supplyAsync(client::stop);
            try {
                assertTrue(entered.await(5, TimeUnit.SECONDS), "The detach send must reach the blocked transport");
                var stopping = stopCall.get(2, TimeUnit.SECONDS);
                stopping.get(8, TimeUnit.SECONDS);
                assertEquals(1, release.getCount(), "The detach deadline must not require unblocking the write");
                assertThrows(IllegalStateException.class, () -> session.send("after stop"));
            } finally {
                release.countDown();
                stopCall.get(5, TimeUnit.SECONDS).get(8, TimeUnit.SECONDS);
            }
        }
    }

    @Test
    void oldStopDoesNotClearSessionsOrTokenProvidersFromANewConnection() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var delivered = new CountDownLatch(1);
        var toolEntered = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient()) {
            var oldSession = createSession(client);
            server.holdPing = true;
            var ping = client.ping("hold old cleanup");
            assertNotNull(server.heldPings.poll(5, TimeUnit.SECONDS), "The old request must remain pending at cleanup");
            var continuation = ping.whenComplete((value, error) -> {
                entered.countDown();
                awaitRelease(release);
            });
            var stopping = client.stop();
            try (var nextServer = new Server(server.listener)) {
                try {
                    assertTrue(entered.await(5, TimeUnit.SECONDS), "The old transport cleanup must be paused");
                    var replacement = client.createSession(new CreateSessionOptions()
                            .setSessionId(oldSession.getSessionId())
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setTools(new SessionToolOptions().setDefinitions(List
                                    .of(ToolDefinition.create("replacement_tool", "Blocks", Map.of(), invocation -> {
                                        toolEntered.countDown();
                                        return new CompletableFuture<>();
                                    }))))
                            .setGitHub(new SessionGitHubOptions().setTokenProvider(args -> CompletableFuture
                                    .completedFuture(GitHubTokenProviderResult.token("replacement-token", 3600)))))
                            .get(5, TimeUnit.SECONDS);
                    var request = nextServer.creates.poll(5, TimeUnit.SECONDS);
                    assertNotNull(request);
                    try (var subscription = replacement.on(event -> delivered.countDown())) {
                        release.countDown();
                        stopping.get(5, TimeUnit.SECONDS);
                        assertThrows(ExecutionException.class, () -> continuation.get(5, TimeUnit.SECONDS));
                        nextServer.emitIdle(replacement.getSessionId());
                        assertTrue(delivered.await(5, TimeUnit.SECONDS),
                                "An old stop must preserve the new session's routing");
                        var tokenReply = nextServer.invokeToken(
                                request.path("params").path("github").path("tokenProviderRegistrationId").asText(),
                                replacement.getSessionId());
                        assertFalse(tokenReply.has("error"),
                                "An old stop must preserve new token providers: " + tokenReply);
                        assertEquals("replacement-token", tokenReply.path("result").path("accessToken").asText());
                        nextServer.emitEvent(replacement.getSessionId(), "external_tool.requested",
                                Map.of("requestId", "replacement-request", "sessionId", replacement.getSessionId(),
                                        "toolCallId", "replacement-call", "toolName", "replacement_tool", "arguments",
                                        Map.of()));
                        assertTrue(toolEntered.await(5, TimeUnit.SECONDS),
                                "An old transport close must not retire the replacement's tools");
                    }
                    client.stop().get(5, TimeUnit.SECONDS);
                } finally {
                    release.countDown();
                    stopping.get(5, TimeUnit.SECONDS);
                }
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void callerCompletionOfStopCannotSuppressRegistryCleanup(boolean timeout) throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.holdDetachResponse = true;
            var session = createSession(client);
            try (var nextServer = new Server(server.listener)) {
                var stopping = client.stop();
                assertNotNull(server.heldDetaches.poll(5, TimeUnit.SECONDS),
                        "The server must hold detach before caller completion");
                if (timeout) {
                    var error = assertThrows(ExecutionException.class,
                            () -> stopping.orTimeout(1, TimeUnit.MILLISECONDS).get(5, TimeUnit.SECONDS));
                    assertInstanceOf(TimeoutException.class, error.getCause());
                } else {
                    assertTrue(stopping.cancel(false));
                }
                assertTrue(server.peerClosed.await(8, TimeUnit.SECONDS), "Private shutdown must still close transport");
                client.start().get(5, TimeUnit.SECONDS);
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
                JsonNode reply = null;
                do {
                    var observed = nextServer.sessionEndReply(session.getSessionId(), deadline - System.nanoTime());
                    if (observed == null) {
                        break;
                    }
                    reply = observed;
                    if (reply.has("error")) {
                        break;
                    }
                    TimeUnit.MILLISECONDS.sleep(10);
                } while (System.nanoTime() < deadline);
                assertNotNull(reply, "The new peer must receive a response about the retired session");
                assertEquals("Unknown session " + session.getSessionId(), reply.path("error").path("message").asText(),
                        "Caller completion must not retain old session registrations: " + reply);
                client.stop().get(5, TimeUnit.SECONDS);
            }
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void stopRoutesCallbacksUntilDetachSettlesWithInlineExecutors(boolean rejected) throws Exception {
        var reject = new AtomicBoolean();
        Executor executor = task -> {
            if (reject.get()) {
                throw new java.util.concurrent.RejectedExecutionException("Controlled executor rejection");
            }
            task.run();
        };
        var events = new AtomicInteger();
        var eventDelivered = new CountDownLatch(1);
        var hooks = new AtomicInteger();
        try (var server = new Server(); var client = server.createClient(executor)) {
            server.holdDetachResponse = true;
            var session = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setHooks(new SessionHookOptions()
                            .setHandlers(new SessionHooks().setOnSessionEnd((input, invocation) -> {
                                hooks.incrementAndGet();
                                return CompletableFuture.completedFuture(null);
                            }))))
                    .get(5, TimeUnit.SECONDS);
            try (var subscription = session.on(event -> {
                events.incrementAndGet();
                eventDelivered.countDown();
            })) {
                reject.set(rejected);
                var stopping = client.stop();
                var detach = server.heldDetaches.poll(5, TimeUnit.SECONDS);
                assertNotNull(detach, "The peer must receive detach before emitting its final callbacks");
                server.emitIdle(session.getSessionId());
                client.ping("stop callback routing barrier").get(5, TimeUnit.SECONDS);
                assertTrue(eventDelivered.await(5, TimeUnit.SECONDS),
                        "Stop must retain event routing until detach settles");
                assertEquals(1, events.get(), "Stop must deliver the event exactly once");
                server.invokeSessionEnd(session.getSessionId());
                assertEquals(1, hooks.get(), "Stop must retain hook routing until detach settles");
                server.completeDetach(detach, "success");
                stopping.get(5, TimeUnit.SECONDS);
                assertThrows(IllegalStateException.class, () -> session.on(event -> fail("Retired handler")));
            }
        }
    }

    @Test
    void stopSharesOneDetachDeadlineWithoutHoldingTheCallersExecutor() throws Exception {
        var executor = Executors.newSingleThreadExecutor();
        try (var server = new Server(); var client = server.createClient(executor)) {
            server.holdDetachResponse = true;
            for (int i = 0; i < 3; i++) {
                createSession(client);
            }
            long started = System.nanoTime();
            var stopping = client.stop();
            for (int i = 0; i < 3; i++) {
                assertNotNull(server.heldDetaches.poll(2, TimeUnit.SECONDS),
                        "Every detach must be sent without waiting for an earlier one");
            }
            CompletableFuture.runAsync(() -> {
            }, executor).get(2, TimeUnit.SECONDS);
            stopping.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 3, TimeUnit.SECONDS);
            assertTrue(
                    System.nanoTime() - started < TimeUnit.SECONDS.toNanos(2 * CopilotSession.DETACH_TIMEOUT_SECONDS),
                    "Unanswered detaches must share one deadline");
            assertEquals(3, server.detaches.get());
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    void stopReturnsWhileASendHoldsTheSessionMonitor() throws Exception {
        var held = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient()) {
            var session = createSession(client);
            // A legacy sendAndWait fallback holds this monitor while it writes
            // session.send.
            var holder = new Thread(() -> {
                synchronized (session) {
                    held.countDown();
                    awaitRelease(release);
                }
            }, "session-monitor-holder");
            holder.setDaemon(true);
            holder.start();
            try {
                assertTrue(held.await(5, TimeUnit.SECONDS), "The session monitor must be held");
                var stopping = CompletableFuture.supplyAsync(client::stop).get(2, TimeUnit.SECONDS);
                assertFalse(stopping.isDone(), "Retirement must still be waiting for the monitor");
                release.countDown();
                stopping.get(CopilotSession.DETACH_TIMEOUT_SECONDS + 3, TimeUnit.SECONDS);
                assertThrows(IllegalStateException.class, () -> session.send("after stop"));
                assertEquals(1, server.detaches.get());
            } finally {
                release.countDown();
                holder.join(5000);
            }
        }
    }

    @Test
    void stopRetiresTheSessionAnInterruptedResumeRestores() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            var original = client
                    .createSession(new CreateSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setGitHub(new SessionGitHubOptions().setTokenProvider(args -> CompletableFuture
                                    .completedFuture(GitHubTokenProviderResult.token("original-token", 3600)))))
                    .get(5, TimeUnit.SECONDS);
            var create = server.creates.poll(5, TimeUnit.SECONDS);
            assertNotNull(create);
            String registrationId = create.path("params").path("github").path("tokenProviderRegistrationId").asText();
            assertFalse(registrationId.isEmpty(), "The original session must register its token provider");

            server.holdResume = true;
            var resuming = client.resumeSession(original.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            assertNotNull(server.heldResumes.poll(5, TimeUnit.SECONDS), "The resume must be pending at stop");

            client.stop().get(CopilotSession.DETACH_TIMEOUT_SECONDS + 3, TimeUnit.SECONDS);
            assertThrows(ExecutionException.class, () -> resuming.get(5, TimeUnit.SECONDS));
            assertThrows(IllegalStateException.class, () -> original.send("after stop"),
                    "The session the failed resume restored must be retired");

            try (var nextServer = new Server(server.listener)) {
                createSession(client);
                var reply = nextServer.invokeToken(registrationId, original.getSessionId());
                assertTrue(reply.has("error"), "A stopped session's token provider must not survive: " + reply);
                client.stop().get(CopilotSession.DETACH_TIMEOUT_SECONDS + 3, TimeUnit.SECONDS);
            }
        }
    }

    private static CopilotSession createSession(CopilotClient client) throws Exception {
        return client
                .createSession(new CreateSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                .get(5, TimeUnit.SECONDS);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void sendAndWaitUsesOnlyTheRuntimeOwnedResponse(boolean typed) throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.completionSupported = true;
            try (var session = createSession(client)) {
                var idles = new AtomicInteger();
                try (var subscription = session.on(SessionIdleEvent.class, event -> idles.incrementAndGet())) {
                    var options = new MessageOptions().setPrompt("question");
                    var waiting = typed
                            ? session.sendAndWait(options, Answer.class, 0)
                            : session.sendAndWait(options, 0);
                    var request = server.completions.poll(5, TimeUnit.SECONDS);
                    assertNotNull(request, "The existing helper must use the private completion RPC");
                    assertEquals("question", request.path("params").path("prompt").asText());
                    server.emitEvent(session.getSessionId(), "session.error",
                            Map.of("errorType", "diagnostic", "message", "unrelated error"));
                    server.emitIdle(session.getSessionId());
                    var response = server.emitEvent(session.getSessionId(), "assistant.message",
                            Map.of("messageId", "owned-response", "content", "{\"value\":42}"));
                    var idle = server.emitEvent(session.getSessionId(), "session.autopilot_stopped",
                            Map.of("reason", "task_complete", "requestId", "owned-request"));
                    server.writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                            Map.of("messageId", "owned-request", "response", response, "aborted", false,
                                    "completionEventId", idle.get("id"))));
                    Object result = waiting.get(5, TimeUnit.SECONDS);
                    if (typed) {
                        assertEquals(new Answer(42), result);
                    } else {
                        var assistant = assertInstanceOf(com.microsoft.copilot.generated.AssistantMessageEvent.class,
                                result);
                        assertEquals(response.get("id"), assistant.getId().toString());
                        assertEquals("{\"value\":42}", assistant.getData().content());
                    }
                    assertEquals(1, idles.get());
                }
            }
        }
    }

    @Test
    void cancellingSendAndWaitCancelsOnlyItsPrivateRequest() throws Exception {
        try (var server = new Server(); var client = server.createClient()) {
            server.completionSupported = true;
            try (var session = createSession(client)) {
                var waiting = session.sendAndWait(new MessageOptions().setPrompt("question"), 0);
                var request = server.completions.poll(5, TimeUnit.SECONDS);
                assertNotNull(request);
                assertTrue(waiting.cancel(true));
                assertEquals(request.get("id"), server.cancellations.poll(5, TimeUnit.SECONDS));
                assertEquals(0, server.detaches.get(), "Cancelling a wait must not close the session");
                assertNotNull(client.ping("still connected").get(5, TimeUnit.SECONDS));
            }
        }
    }

    /**
     * Waits for event delivery on the client's executor to finish once its handler
     * is released.
     */
    /**
     * Waits for an asynchronously delivered handler count. An RPC round trip orders
     * reads on the connection, not handler completion on the delivery executor.
     */
    private static void awaitCount(AtomicInteger count, int expected, String message) throws InterruptedException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (count.get() < expected && System.nanoTime() < deadline) {
            Thread.sleep(10);
        }
        assertEquals(expected, count.get(), message);
    }

    private static void awaitDeliveries(ExecutorService deliveries, String message) throws InterruptedException {
        deliveries.shutdown();
        assertTrue(deliveries.awaitTermination(5, TimeUnit.SECONDS), message);
    }

    /**
     * Closes the session from a caller thread of its own. On JDK 25 the default
     * async pool is the common pool even when it is one worker wide, and an
     * admitted handler may be holding that worker.
     */
    private static CompletableFuture<Void> closeOnOwnThread(CopilotSession session) {
        var closing = new CompletableFuture<Void>();
        var thread = new Thread(() -> {
            try {
                session.close();
                closing.complete(null);
            } catch (Throwable failure) {
                closing.completeExceptionally(failure);
            }
        }, "force-stop-session-close");
        thread.setDaemon(true);
        thread.start();
        return closing;
    }

    private static void awaitRelease(CountDownLatch release) {
        boolean interrupted = false;
        try {
            while (true) {
                try {
                    release.await();
                    return;
                } catch (InterruptedException error) {
                    interrupted = true;
                }
            }
        } finally {
            if (interrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }

    @Test
    void toolResultFailureIsAnsweredWhileAnotherSessionToolOccupiesExecutor() throws Exception {
        var executor = Executors.newSingleThreadExecutor();
        var toolEntered = new CountDownLatch(1);
        var releaseTool = new CountDownLatch(1);
        try (var server = new Server(); var client = server.createClient(executor)) {
            server.holdToolReplies = true;
            var first = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setDefinitions(List.of(ToolDefinition.create("delivers",
                            "Returns a result", Map.of(), invocation -> CompletableFuture.completedFuture("ok"))))))
                    .get(5, TimeUnit.SECONDS);
            var second = client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setDefinitions(List
                            .of(ToolDefinition.create("blocks", "Waits for the first session", Map.of(), invocation -> {
                                toolEntered.countDown();
                                awaitRelease(releaseTool);
                                return new CompletableFuture<>();
                            })))))
                    .get(5, TimeUnit.SECONDS);
            try {
                server.emitEvent(first.getSessionId(), "external_tool.requested",
                        Map.of("requestId", "request-delivers", "sessionId", first.getSessionId(), "toolCallId",
                                "call-delivers", "toolName", "delivers", "arguments", Map.of()));
                var delivery = server.toolReplies.poll(5, TimeUnit.SECONDS);
                assertNotNull(delivery, "The first tool result must reach the transport");
                assertEquals("ok", delivery.path("params").path("result").path("textResultForLlm").asText());

                server.emitEvent(second.getSessionId(), "external_tool.requested",
                        Map.of("requestId", "request-blocks", "sessionId", second.getSessionId(), "toolCallId",
                                "call-blocks", "toolName", "blocks", "arguments", Map.of()));
                assertTrue(toolEntered.await(5, TimeUnit.SECONDS), "The second tool must occupy the sole worker");
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", delivery.get("id"), "error",
                        Map.of("code", -32603, "message", "tool result rejected")));

                var answer = server.toolReplies.poll(5, TimeUnit.SECONDS);
                assertNotNull(answer, "The failed result must be answered without waiting for the blocked handler");
                var params = answer.path("params");
                assertEquals(first.getSessionId(), params.path("sessionId").asText());
                assertEquals("request-delivers", params.path("requestId").asText());
                assertEquals("tool result rejected", params.path("error").asText());
                assertTrue(params.path("result").isMissingNode() || params.path("result").isNull(), params.toString());
                server.writeMessage(Map.of("jsonrpc", "2.0", "id", answer.get("id"), "result", Map.of()));
            } finally {
                releaseTool.countDown();
            }
        } finally {
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS), "The owned handler executor must stop");
        }
    }

    private static final class Server implements AutoCloseable {

        private static final ObjectMapper MAPPER = new ObjectMapper();
        private final ServerSocket listener;
        private final Thread thread;
        private final AtomicInteger detaches = new AtomicInteger();
        private final CountDownLatch detachEntered = new CountDownLatch(1);
        private final CountDownLatch peerClosed = new CountDownLatch(1);
        private final java.util.concurrent.BlockingQueue<JsonNode> heldDetaches = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> heldDeletes = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> heldResumes = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> heldOptions = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> hookReplies = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> creates = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> heldPings = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> heldConnects = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> toolReplies = new java.util.concurrent.LinkedBlockingQueue<>();
        private volatile Socket socket;
        private volatile boolean closed;
        private volatile boolean emitDetachCallbacks;
        private volatile boolean holdDetachResponse;
        private volatile boolean holdDeleteResponse;
        private volatile boolean holdResumeResponse;
        private volatile boolean holdOptions;
        private volatile boolean completionSupported;
        private volatile boolean rejectSessionRequests;
        private volatile boolean holdPing;
        private volatile boolean holdResume;
        private volatile boolean holdConnect;
        private volatile boolean holdToolReplies;
        private final java.util.concurrent.BlockingQueue<JsonNode> completions = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> cancellations = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<JsonNode> resumes = new java.util.concurrent.LinkedBlockingQueue<>();
        private final java.util.concurrent.BlockingQueue<String> sends = new java.util.concurrent.LinkedBlockingQueue<>();
        private volatile Exception failure;

        Server() throws IOException {
            this(new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1")));
        }

        Server(ServerSocket listener) {
            this.listener = listener;
            thread = new Thread(this::serve, "force-stop-session-server");
            thread.setDaemon(true);
            thread.start();
        }

        CopilotClient createClient() {
            return createClient(null);
        }

        CopilotClient createClient(Executor executor) {
            return new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(RuntimeConnection.forTcp("127.0.0.1", listener.getLocalPort())).setExecutor(executor));
        }

        void emitIdle(String sessionId) throws IOException {
            emitEvent(sessionId, "session.idle", Map.of());
        }

        void completeConnect(JsonNode request) throws IOException {
            writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "result",
                    Map.of("protocolVersion", SdkProtocolVersion.get())));
        }

        void completeDetach(JsonNode request, String outcome) throws IOException {
            var response = MAPPER.createObjectNode().put("jsonrpc", "2.0");
            response.set("id", request.get("id"));
            switch (outcome) {
                case "success" -> response.set("result", MAPPER.valueToTree(Map.of("success", true)));
                case "rejected" -> response.set("result",
                        MAPPER.valueToTree(Map.of("success", false, "error", "controlled detach rejection")));
                case "rpc-error" -> response.set("error",
                        MAPPER.valueToTree(Map.of("code", -32603, "message", "controlled detach RPC error")));
                default -> throw new IllegalArgumentException("Unexpected detach outcome: " + outcome);
            }
            writeMessage(response);
        }

        void invokeSessionEnd(String sessionId) throws Exception {
            var reply = sessionEndReply(sessionId, TimeUnit.SECONDS.toNanos(5));
            assertNotNull(reply, "The SDK must acknowledge the session-end hook");
            assertFalse(reply.has("error"), "Session-end hook failed: " + reply);
        }

        JsonNode sessionEndReply(String sessionId, long timeoutNanos) throws Exception {
            writeMessage(Map.of("jsonrpc", "2.0", "id", 9_000_001L, "method", "hooks.invoke", "params",
                    Map.of("sessionId", sessionId, "hookType", "sessionEnd", "input",
                            Map.of("sessionId", sessionId, "timestamp", 0, "cwd", ".", "reason", "user_exit"))));
            var reply = hookReplies.poll(Math.max(0, timeoutNanos), TimeUnit.NANOSECONDS);
            return reply;
        }

        JsonNode invokeToken(String registrationId, String sessionId) throws Exception {
            writeMessage(Map.of("jsonrpc", "2.0", "id", 9_000_001L, "method", "gitHubToken.getToken", "params",
                    Map.of("registrationId", registrationId, "sessionId", sessionId, "host", "github.com", "reason",
                            "refresh")));
            var reply = hookReplies.poll(5, TimeUnit.SECONDS);
            assertNotNull(reply, "The SDK must respond to the token request");
            return reply;
        }

        Map<String, Object> emitEvent(String sessionId, String type, Map<String, Object> data) throws IOException {
            assertNotNull(socket, "Session creation must establish the connection before events");
            var event = Map.<String, Object>of("type", type, "id", java.util.UUID.randomUUID().toString(), "timestamp",
                    "2026-10-05T00:00:00Z", "data", data);
            writeMessage(Map.of("jsonrpc", "2.0", "method", "session.event", "params",
                    Map.of("sessionId", sessionId, "event", event)));
            return event;
        }

        private void serve() {
            try (Socket accepted = listener.accept()) {
                socket = accepted;
                JsonNode pendingDetachId = null;
                while (!closed) {
                    JsonNode request = readMessage(accepted.getInputStream());
                    if (request == null) {
                        return;
                    }
                    if (accepted.isOutputShutdown()) {
                        continue;
                    }
                    JsonNode params = request.path("params");
                    if (!request.has("method") && request.path("id").asLong() == 9_000_001L) {
                        hookReplies.add(request);
                        continue;
                    }
                    if (request.path("method").asText().equals("session.sendAndWait")) {
                        if (completionSupported) {
                            completions.add(request);
                        } else {
                            writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "error",
                                    Map.of("code", -32601, "message", "Method not found")));
                        }
                        continue;
                    }
                    if (request.path("method").asText().equals("$/cancelRequest")) {
                        cancellations.add(params.get("id"));
                        continue;
                    }
                    if (rejectSessionRequests && (request.path("method").asText().equals("session.create")
                            || request.path("method").asText().equals("session.resume"))) {
                        writeMessage(Map.of("jsonrpc", "2.0", "id", request.get("id"), "error",
                                Map.of("code", -32603, "message", "controlled session rejection")));
                        continue;
                    }
                    if (holdConnect && request.path("method").asText().equals("connect")) {
                        heldConnects.add(request);
                        continue;
                    }
                    if (holdResumeResponse && request.path("method").asText().equals("session.resume")) {
                        resumes.add(request);
                        heldResumes.add(request);
                        continue;
                    }
                    if (holdResume && request.path("method").asText().equals("session.resume")) {
                        heldResumes.add(request);
                        continue;
                    }
                    if (holdOptions && request.path("method").asText().equals("session.options.update")) {
                        heldOptions.add(request);
                        continue;
                    }
                    if (holdPing && request.path("method").asText().equals("ping")) {
                        heldPings.add(request);
                        continue;
                    }
                    if (holdToolReplies
                            && request.path("method").asText().equals("session.tools.handlePendingToolCall")) {
                        toolReplies.add(request);
                        continue;
                    }
                    if (holdDetachResponse && request.path("method").asText().equals("session.detach")) {
                        detaches.incrementAndGet();
                        heldDetaches.add(request);
                        detachEntered.countDown();
                        continue;
                    }
                    if (holdDeleteResponse && request.path("method").asText().equals("session.delete")) {
                        heldDeletes.add(request);
                        continue;
                    }
                    if (!request.has("method") && request.path("id").asLong() == 9_000_000L) {
                        if (pendingDetachId == null || request.has("error")) {
                            throw new IOException("Unexpected detach hook response: " + request);
                        }
                        var response = MAPPER.createObjectNode().put("jsonrpc", "2.0");
                        response.set("id", pendingDetachId);
                        response.set("result", MAPPER.valueToTree(Map.of("success", true)));
                        pendingDetachId = null;
                        writeMessage(response);
                        continue;
                    }
                    if (emitDetachCallbacks && request.path("method").asText().equals("session.detach")) {
                        detaches.incrementAndGet();
                        pendingDetachId = request.get("id");
                        String sessionId = params.path("sessionId").asText();
                        emitIdle(sessionId);
                        var hookInput = Map.of("sessionId", sessionId, "timestamp", 0, "cwd", ".", "reason",
                                "user_exit");
                        writeMessage(Map.of("jsonrpc", "2.0", "id", 9_000_000L, "method", "hooks.invoke", "params",
                                Map.of("sessionId", sessionId, "hookType", "sessionEnd", "input", hookInput)));
                        continue;
                    }
                    Object result = switch (request.path("method").asText()) {
                        case "connect" -> Map.of("protocolVersion", SdkProtocolVersion.get());
                        case "session.create", "session.resume" -> {
                            creates.add(request);
                            yield Map.of("sessionId", params.path("sessionId").asText());
                        }
                        case "session.send" -> {
                            sends.add(params.path("prompt").asText());
                            yield Map.of("messageId", params.path("prompt").asText());
                        }
                        case "session.detach" -> {
                            detaches.incrementAndGet();
                            yield Map.of("success", true);
                        }
                        case "session.delete" -> Map.of("success", true);
                        case "session.options.update" -> Map.of();
                        case "ping" -> Map.of("message", params.path("message").asText(), "protocolVersion",
                                SdkProtocolVersion.get());
                        default -> throw new IOException("Unexpected RPC: " + request.path("method"));
                    };
                    var response = MAPPER.createObjectNode().put("jsonrpc", "2.0");
                    response.set("id", request.get("id"));
                    response.set("result", MAPPER.valueToTree(result));
                    writeMessage(response);
                }
            } catch (Exception error) {
                if (!closed) {
                    failure = error;
                }
            } finally {
                peerClosed.countDown();
            }
        }

        private static JsonNode readMessage(InputStream input) throws IOException {
            var header = new StringBuilder();
            while (!header.toString().endsWith("\r\n\r\n")) {
                int value = input.read();
                if (value < 0) {
                    if (header.isEmpty()) {
                        return null;
                    }
                    throw new IOException("Truncated RPC header");
                }
                header.append((char) value);
                if (header.length() > 8192) {
                    throw new IOException("Oversized RPC header");
                }
            }
            int length;
            try {
                length = Integer.parseInt(header.substring(header.indexOf(":") + 1).trim());
            } catch (NumberFormatException error) {
                throw new IOException("Invalid Content-Length", error);
            }
            if (length < 0 || length > 1_048_576) {
                throw new IOException("Invalid Content-Length: " + length);
            }
            byte[] body = input.readNBytes(length);
            if (body.length != length) {
                throw new IOException("Truncated RPC body");
            }
            return MAPPER.readTree(body);
        }

        private synchronized void writeMessage(Object message) throws IOException {
            byte[] body = MAPPER.writeValueAsBytes(message);
            var output = socket.getOutputStream();
            output.write(("Content-Length: " + body.length + "\r\n\r\n").getBytes(StandardCharsets.UTF_8));
            output.write(body);
            output.flush();
        }

        @Override
        public void close() throws Exception {
            closed = true;
            listener.close();
            if (socket != null) {
                socket.close();
            }
            thread.join(5000);
            assertFalse(thread.isAlive(), "The RPC fixture must stop");
            assertNull(failure, "RPC fixture failure: " + failure);
        }
    }
}
