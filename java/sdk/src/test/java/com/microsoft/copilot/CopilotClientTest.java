/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.ClientGitHubOptions;
import com.microsoft.copilot.rpc.ClientModelOptions;
import com.microsoft.copilot.rpc.CreateSessionInstructionOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.SessionInstructionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import com.microsoft.copilot.rpc.SessionSkillOptions;
import com.microsoft.copilot.rpc.SessionToolOptions;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.microsoft.copilot.generated.ExternalToolRequestedEvent;
import com.microsoft.copilot.generated.PermissionRequestedEvent;
import com.microsoft.copilot.generated.rpc.SessionOptionsUpdateResult;
import com.microsoft.copilot.rpc.CloudSessionOptions;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.generated.rpc.Model;
import com.microsoft.copilot.generated.rpc.ModelCapabilities;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesLimits;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesSupports;
import com.microsoft.copilot.rpc.CreateSessionRequest;
import com.microsoft.copilot.rpc.CreateSessionResponse;
import com.microsoft.copilot.rpc.DeleteSessionResponse;
import com.microsoft.copilot.rpc.GetModelsResponse;
import com.microsoft.copilot.rpc.GitHubTokenProviderResult;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.PermissionInvocation;
import com.microsoft.copilot.rpc.PermissionRequestResult;
import com.microsoft.copilot.rpc.PermissionRequestResultKind;
import com.microsoft.copilot.rpc.PingResponse;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.ResumeSessionResponse;
import com.microsoft.copilot.rpc.SendMessageResponse;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.SessionLifecycleEvent;
import com.microsoft.copilot.rpc.SessionLifecycleEventTypes;
import com.microsoft.copilot.rpc.ToolDefinition;

import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.io.BufferedReader;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Tests for CopilotClient.
 *
 * Note: These tests require the Copilot CLI to be installed. Set the
 * COPILOT_CLI_PATH environment variable to the path to the CLI, or run 'npm
 * install' in the nodejs directory.
 */
public class CopilotClientTest {

    private static String cliPath;

    @BeforeAll
    static void setup() {
        cliPath = TestUtil.findCliPath();
    }

    @Test
    void testStopDoesNotThrowWhenRuntimeShutdownFails() throws Exception {
        var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));
        var rpc = mock(JsonRpcClient.class);
        when(rpc.invoke(eq("runtime.shutdown"), any(), eq(Void.class)))
                .thenReturn(CompletableFuture.failedFuture(new RuntimeException("shutdown failed")));
        var process = mock(Process.class);
        when(process.isAlive()).thenReturn(true);
        when(process.getOutputStream()).thenReturn(OutputStream.nullOutputStream());
        when(process.destroyForcibly()).thenReturn(process);
        when(process.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(true);

        setConnectionFuture(client, rpc, process);

        assertDoesNotThrow(() -> client.stop().get());

        verify(rpc).invoke(eq("runtime.shutdown"), eq(Map.of()), eq(Void.class));
        verify(rpc).close();
        verify(process).destroyForcibly();
    }

    @Test
    void testForceStopAndExternalStopDoNotRequestRuntimeShutdown() throws Exception {
        var forceClient = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));
        var forceRpc = mock(JsonRpcClient.class);
        var process = mock(Process.class);
        when(process.isAlive()).thenReturn(true);
        when(process.destroyForcibly()).thenReturn(process);
        when(process.waitFor(anyLong(), any(TimeUnit.class))).thenReturn(true);
        setConnectionFuture(forceClient, forceRpc, process);

        forceClient.forceStop().get();

        verify(forceRpc, never()).invoke(eq("runtime.shutdown"), any(), eq(Void.class));
        verify(process).destroyForcibly();

        var externalClient = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));
        var externalRpc = mock(JsonRpcClient.class);
        setConnectionFuture(externalClient, externalRpc, null);

        externalClient.stop().get();

        verify(externalRpc, never()).invoke(eq("runtime.shutdown"), any(), eq(Void.class));
    }

    @Test
    @SuppressWarnings("unchecked")
    void testForceStopCancelsPendingExternalTools() throws Exception {
        var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));
        var rpc = mock(JsonRpcClient.class);
        setConnectionFuture(client, rpc, null);
        var session = new CopilotSession("force-stop-session", rpc);
        var toolFuture = new CompletableFuture<Object>();
        var started = new CountDownLatch(1);
        var lateStarted = new CountDownLatch(1);
        var invocations = new AtomicInteger();
        session.registerTools(List.of(ToolDefinition.create("blocked_tool", "Blocks", Map.of(), invocation -> {
            if (invocations.incrementAndGet() == 1) {
                started.countDown();
            } else {
                lateStarted.countDown();
            }
            return toolFuture;
        })));
        Field sessionsField = CopilotClient.class.getDeclaredField("sessions");
        sessionsField.setAccessible(true);
        var sessions = (Map<String, CopilotSession>) sessionsField.get(client);
        sessions.put(session.getSessionId(), session);

        var requested = new ExternalToolRequestedEvent();
        requested.setData(ExternalToolRequestedEvent.ExternalToolRequestedEventData.builder()
                .requestId("request-force-stop").sessionId(session.getSessionId()).toolCallId("tool-call-force-stop")
                .toolName("blocked_tool").providerId(null).arguments(Map.of()).workingDirectory(null).traceparent(null)
                .tracestate(null).build());
        session.dispatchEvent(requested);
        assertTrue(started.await(1, TimeUnit.SECONDS));

        client.forceStop().get();

        assertThrows(CancellationException.class, () -> toolFuture.get(1, TimeUnit.SECONDS));
        assertTrue(sessions.isEmpty());

        var lateRequest = new ExternalToolRequestedEvent();
        lateRequest.setData(ExternalToolRequestedEvent.ExternalToolRequestedEventData.builder()
                .requestId("request-after-force-stop").sessionId(session.getSessionId())
                .toolCallId("tool-call-after-force-stop").toolName("blocked_tool").providerId(null).arguments(Map.of())
                .workingDirectory(null).traceparent(null).tracestate(null).build());
        session.dispatchEvent(lateRequest);
        assertFalse(lateStarted.await(100, TimeUnit.MILLISECONDS));
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"program-parent"})
    void testPermissionHandlerReceivesDerivedPromptContext(String programToolCallId) throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var session = new CopilotSession("permission-session", rpc);
        session.setManagedSettingsEnabled(true);
        var invocation = new AtomicReference<PermissionInvocation>();
        var invoked = new CountDownLatch(1);
        session.registerPermissionHandler((request, context) -> {
            invocation.set(context);
            invoked.countDown();
            return CompletableFuture
                    .completedFuture(new PermissionRequestResult().setKind(PermissionRequestResultKind.NO_RESULT));
        });

        var assistedApprovalAssessment = com.microsoft.copilot.generated.AssistedApprovalAssessmentCompleted.builder()
                .recommendation(com.microsoft.copilot.generated.AssistedApprovalCompletedRecommendation.REQUIREAPPROVAL)
                .build();
        assistedApprovalAssessment.setRationale("The command modifies protected files.");
        var event = new PermissionRequestedEvent();
        event.setData(PermissionRequestedEvent.PermissionRequestedEventData.builder()
                .programToolCallId(programToolCallId).requestId("permission-1")
                .permissionRequest(Map.of("kind", "shell", "managedApprovalRequired", true, "fullCommandText", "ls"))
                .promptRequest(Map.of("kind", "commands", "fullCommandText", "ls", "intention", "List files",
                        "commandIdentifiers", List.of("ls"), "canOfferSessionApproval", false, "assistedApproval",
                        Map.of("recommendation", "approve")))
                .assistedApprovalAssessment(assistedApprovalAssessment).permissionMode(null).agentMode(null)
                .ownerSessionId(null).ownerAgentId(null).riskAssessment(null).resolvedByHook(null)
                .recoveryEpisodeId(null).fusion(null).build());

        session.dispatchEvent(event);

        assertTrue(invoked.await(1, TimeUnit.SECONDS));
        var observed = invocation.get();
        assertTrue(observed.isManagedSettingsEnabled());
        assertEquals(programToolCallId, observed.getProgramToolCallId());
        assertNotNull(observed.getPromptRequest());
        assertNull(observed.getPromptRequest().getManagedApprovalRequired());
        assertEquals("commands", observed.getPromptRequest().getKind());
        assertEquals(Map.of("recommendation", "approve"),
                observed.getPromptRequest().getExtensionData().get("assistedApproval"));
        var assessment = assertInstanceOf(com.microsoft.copilot.generated.AssistedApprovalAssessmentCompleted.class,
                observed.getAssistedApprovalAssessment());
        assertEquals("requireApproval", assessment.getRecommendation().getValue());
        assertEquals("The command modifies protected files.", assessment.getRationale());
    }

    @Test
    @SuppressWarnings("unchecked")
    void testDeleteSessionReleasesGitHubTokenProvider() throws Exception {
        var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false));
        var rpc = mock(JsonRpcClient.class);
        when(rpc.invoke(eq("session.delete"), any(), eq(DeleteSessionResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(new DeleteSessionResponse(true, null)));
        when(rpc.invoke(eq("session.detach"), any(), eq(CopilotSession.SessionDetachResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(new CopilotSession.SessionDetachResponse(true, null)));
        setConnectionFuture(client, rpc, null);

        var registry = new GitHubTokenProviderRegistry();
        var registration = registry
                .register(args -> CompletableFuture.completedFuture(GitHubTokenProviderResult.cancelled()));
        var session = new CopilotSession("delete-session", rpc);
        session.setGitHubTokenProviderRegistration(registration);
        Field sessionsField = CopilotClient.class.getDeclaredField("sessions");
        sessionsField.setAccessible(true);
        ((Map<String, CopilotSession>) sessionsField.get(client)).put(session.getSessionId(), session);

        try {
            client.deleteSession(session.getSessionId()).join();
            assertNull(registry.get(registration.id()));
        } finally {
            session.close();
            client.close();
        }
    }

    @Test
    @AllowCopilotExperimental
    void cloudSessionWithSkillProviderIsRejectedBeforeConnectingOrCallingProvider() throws Exception {
        var providerCalls = new AtomicInteger();
        SkillProvider provider = new SkillProvider() {
            @Override
            public CompletableFuture<List<SkillProviderDescriptor>> listSkills() {
                providerCalls.incrementAndGet();
                return CompletableFuture.completedFuture(List.of());
            }

            @Override
            public CompletableFuture<String> readSkill(String name) {
                providerCalls.incrementAndGet();
                return CompletableFuture.completedFuture(null);
            }
        };

        try (var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            var config = new CreateSessionOptions().setCloud(new CloudSessionOptions())
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setSkills(new SessionSkillOptions().setProvider(provider));

            ExecutionException error = assertThrows(ExecutionException.class, () -> client.createSession(config).get());
            assertInstanceOf(IllegalArgumentException.class, error.getCause());
            assertEquals("Skill providers are not supported for cloud sessions.", error.getCause().getMessage());
            assertEquals(0, providerCalls.get());
        }
    }

    @Test
    @AllowCopilotExperimental
    void emptyModeCreateWithSkillProviderSendsFlagsAndRegistersBeforeRpc() throws Exception {
        var client = new CopilotClient(new CopilotClientOptions().setAutoStart(false).setMode(CopilotClientMode.EMPTY)
                .setRuntime(RuntimeConnection.forDefault().setBaseDirectory(System.getProperty("java.io.tmpdir"))));
        var rpc = mock(JsonRpcClient.class);
        setConnectionFuture(client, rpc, null);
        SkillProvider provider = testSkillProvider();
        var createRequest = new AtomicReference<CreateSessionRequest>();
        var optionsPatch = new AtomicReference<ObjectNode>();

        when(rpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenAnswer(invocation -> {
            CreateSessionRequest request = invocation.getArgument(1);
            createRequest.set(request);
            assertTrue(request.getSkills().getHasProvider());
            assertFalse(request.getSkills().getEnabled());
            assertSame(provider, registeredSkillProvider(client, "skill-provider-session"));
            return CompletableFuture
                    .completedFuture(new CreateSessionResponse("skill-provider-session", null, null, null));
        });
        when(rpc.invoke(eq("session.options.update"), any(), eq(SessionOptionsUpdateResult.class)))
                .thenAnswer(invocation -> {
                    optionsPatch.set(invocation.getArgument(1));
                    return CompletableFuture.completedFuture(null);
                });

        try {
            var config = new CreateSessionOptions().setSessionId("skill-provider-session")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setAvailableTools(List.of()))
                    .setSkills(new SessionSkillOptions().setProvider(provider));

            CopilotSession session = client.createSession(config).get();

            assertSame(provider, session.getSkillProvider());
            assertNotNull(createRequest.get());
            assertTrue(createRequest.get().getSkills().getHasProvider());
            assertFalse(createRequest.get().getSkills().getEnabled());
            assertNotNull(optionsPatch.get());
            assertEquals("skill-provider-session", optionsPatch.get().path("sessionId").asText());
            assertTrue(optionsPatch.get().path("plugins").path("installed").isArray());
            assertEquals(0, optionsPatch.get().path("plugins").path("installed").size());
            assertTrue(optionsPatch.get().path("skills").path("includedBuiltinSkills").isArray());
            assertEquals(0, optionsPatch.get().path("skills").path("includedBuiltinSkills").size());
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    void createAndResumeOptionsPatchesUseTheirReturnedSessionIds() throws Exception {
        var client = new CopilotClient(new CopilotClientOptions().setAutoStart(false).setMode(CopilotClientMode.EMPTY)
                .setRuntime(RuntimeConnection.forDefault().setBaseDirectory(System.getProperty("java.io.tmpdir"))));
        var rpc = mock(JsonRpcClient.class);
        setConnectionFuture(client, rpc, null);
        var optionsPatches = new ArrayList<ObjectNode>();

        when(rpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenReturn(CompletableFuture
                .completedFuture(new CreateSessionResponse("create-options-session", null, null, null)));
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(CompletableFuture
                .completedFuture(new ResumeSessionResponse("resume-options-session", null, null, null)));
        when(rpc.invoke(eq("session.options.update"), any(), eq(SessionOptionsUpdateResult.class)))
                .thenAnswer(invocation -> {
                    optionsPatches.add(invocation.getArgument(1));
                    return CompletableFuture.completedFuture(null);
                });

        try {
            client.createSession(new CreateSessionOptions().setSessionId("create-options-session")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setAvailableTools(List.of()))
                    .setInstructions(new CreateSessionInstructionOptions().setSkipCustomInstructions(false))).get();
            client.resumeSession("resume-options-session",
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setTools(new SessionToolOptions().setAvailableTools(List.of()))
                            .setInstructions(new SessionInstructionOptions().setSkipCustomInstructions(false)))
                    .get();

            assertEquals(2, optionsPatches.size());
            assertEquals("create-options-session", optionsPatches.get(0).path("sessionId").asText());
            assertFalse(optionsPatches.get(0).path("skipCustomInstructions").asBoolean());
            assertEquals("resume-options-session", optionsPatches.get(1).path("sessionId").asText());
            assertFalse(optionsPatches.get(1).path("skipCustomInstructions").asBoolean());
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    @AllowCopilotExperimental
    void emptyModeResumeValidationFailureKeepsResidentSkillProvider() throws Exception {
        var client = new CopilotClient(new CopilotClientOptions().setAutoStart(false).setMode(CopilotClientMode.EMPTY)
                .setRuntime(RuntimeConnection.forDefault().setBaseDirectory(System.getProperty("java.io.tmpdir"))));
        var rpc = mock(JsonRpcClient.class);
        setConnectionFuture(client, rpc, null);
        SkillProvider resident = testSkillProvider();
        when(rpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenReturn(
                CompletableFuture.completedFuture(new CreateSessionResponse("resident-session", null, null, null)));
        when(rpc.invoke(eq("session.options.update"), any(), eq(Void.class)))
                .thenReturn(CompletableFuture.completedFuture(null));

        try {
            client.createSession(new CreateSessionOptions().setSessionId("resident-session")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(new SessionToolOptions().setAvailableTools(List.of()))
                    .setSkills(new SessionSkillOptions().setProvider(resident))).get();

            var resumeConfig = new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setSkills(new SessionSkillOptions().setProvider(testSkillProvider()));
            var error = assertThrows(ExecutionException.class,
                    () -> client.resumeSession("resident-session", resumeConfig).get());

            assertInstanceOf(IllegalArgumentException.class, error.getCause());
            assertSame(resident, registeredSkillProvider(client, "resident-session"));
            verify(rpc, never()).invoke(eq("session.resume"), any(), any());
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    @AllowCopilotExperimental
    void failedResumeDoesNotEvictAConcurrentResume() throws Exception {
        var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false).setRuntime(
                        RuntimeConnection.forDefault().setBaseDirectory(System.getProperty("java.io.tmpdir"))));
        var rpc = mock(JsonRpcClient.class);
        setConnectionFuture(client, rpc, null);
        when(rpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenReturn(
                CompletableFuture.completedFuture(new CreateSessionResponse("shared-session", null, null, null)));
        when(rpc.invoke(eq("session.options.update"), any(), eq(Void.class)))
                .thenReturn(CompletableFuture.completedFuture(null));
        var firstResume = new CompletableFuture<ResumeSessionResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(firstResume,
                CompletableFuture.completedFuture(new ResumeSessionResponse("shared-session", null, null, null)));

        try {
            client.createSession(new CreateSessionOptions().setSessionId("shared-session")
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setSkills(new SessionSkillOptions().setProvider(testSkillProvider()))).get();
            var failing = client.resumeSession("shared-session",
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setSkills(new SessionSkillOptions().setProvider(testSkillProvider())));
            SkillProvider winner = testSkillProvider();
            client.resumeSession("shared-session",
                    new ResumeSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setSkills(new SessionSkillOptions().setProvider(winner)))
                    .get();

            firstResume.completeExceptionally(new RuntimeException("resume failed"));

            assertThrows(ExecutionException.class, failing::get);
            assertSame(winner, registeredSkillProvider(client, "shared-session"));
        } finally {
            client.forceStop().get();
        }
    }

    private static CopilotClient sessionTrackingClient(JsonRpcClient rpc, boolean detachSucceeds) throws Exception {
        var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false).setRuntime(
                        RuntimeConnection.forDefault().setBaseDirectory(System.getProperty("java.io.tmpdir"))));
        setConnectionFuture(client, rpc, null);
        when(rpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenReturn(
                CompletableFuture.completedFuture(new CreateSessionResponse("tracked-session", null, null, null)));
        when(rpc.invoke(eq("session.options.update"), any(), eq(Void.class)))
                .thenReturn(CompletableFuture.completedFuture(null));
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(
                CompletableFuture.completedFuture(new ResumeSessionResponse("tracked-session", null, null, null)));
        when(rpc.invoke(eq("session.detach"), any(), eq(CopilotSession.SessionDetachResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(
                        new CopilotSession.SessionDetachResponse(detachSucceeds, detachSucceeds ? null : "busy")));
        return client;
    }

    @Test
    void overlappingFailedResumesRestoreResidentInEitherCompletionOrder() throws Exception {
        for (boolean admissionOrder : new boolean[]{true, false}) {
            var rpc = mock(JsonRpcClient.class);
            var client = sessionTrackingClient(rpc, true);
            var firstResponse = new CompletableFuture<ResumeSessionResponse>();
            var secondResponse = new CompletableFuture<ResumeSessionResponse>();
            when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(firstResponse,
                    secondResponse);
            try {
                var resident = client.createSession(new CreateSessionOptions().setSessionId("tracked-session")
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get();
                var callbacks = new AtomicInteger();
                resident.on(event -> callbacks.incrementAndGet());
                var config = new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
                var first = client.resumeSession(resident.getSessionId(), config);
                var second = client.resumeSession(resident.getSessionId(), config);

                if (admissionOrder) {
                    firstResponse.completeExceptionally(new IllegalStateException("first resume rejected"));
                    assertThrows(ExecutionException.class, first::get);
                    assertDoesNotThrow(() -> resident.on(event -> {
                    }).close());
                    secondResponse.completeExceptionally(new IllegalStateException("second resume rejected"));
                } else {
                    secondResponse.completeExceptionally(new IllegalStateException("second resume rejected"));
                    assertThrows(ExecutionException.class, second::get);
                    firstResponse.completeExceptionally(new IllegalStateException("first resume rejected"));
                }

                assertThrows(ExecutionException.class, first::get);
                assertThrows(ExecutionException.class, second::get);
                assertSame(resident, trackedSession(client, resident.getSessionId()));
                trackedSession(client, resident.getSessionId())
                        .dispatchEvent(new com.microsoft.copilot.generated.SessionIdleEvent());
                assertEquals(1, callbacks.get(), "The restored resident must retain its original callbacks");
                assertDoesNotThrow(() -> resident.on(event -> {
                }).close());
            } finally {
                firstResponse.completeExceptionally(new IllegalStateException("test cleanup"));
                secondResponse.completeExceptionally(new IllegalStateException("test cleanup"));
                client.forceStop().get();
            }
        }
    }

    @Test
    void overlappingFailedResumesDoNotRestoreRetiredProvisional() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var firstResponse = new CompletableFuture<ResumeSessionResponse>();
        var secondResponse = new CompletableFuture<ResumeSessionResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(firstResponse,
                secondResponse);
        try {
            var config = new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var first = client.resumeSession("tracked-session", config);
            var provisional = trackedSession(client, "tracked-session");
            var second = client.resumeSession("tracked-session", config);

            firstResponse.completeExceptionally(new IllegalStateException("first resume rejected"));
            assertThrows(ExecutionException.class, first::get);
            assertThrows(IllegalStateException.class, () -> provisional.on(event -> {
            }));
            secondResponse.completeExceptionally(new IllegalStateException("second resume rejected"));

            assertThrows(ExecutionException.class, second::get);
            assertNull(trackedSession(client, "tracked-session"));
        } finally {
            firstResponse.completeExceptionally(new IllegalStateException("test cleanup"));
            secondResponse.completeExceptionally(new IllegalStateException("test cleanup"));
            client.forceStop().get();
        }
    }

    @Test
    void overlappingFailedResumesPreserveResidentCallbacksDuringGracefulStop() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var firstResponse = new CompletableFuture<ResumeSessionResponse>();
        var secondResponse = new CompletableFuture<ResumeSessionResponse>();
        var detach = new CompletableFuture<CopilotSession.SessionDetachResponse>();
        var detachStarted = new CountDownLatch(1);
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(firstResponse,
                secondResponse);
        when(rpc.invoke(eq("session.detach"), any(), eq(CopilotSession.SessionDetachResponse.class)))
                .thenAnswer(invocation -> {
                    detachStarted.countDown();
                    return detach;
                });
        CompletableFuture<Void> stopping = null;
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var callbacks = new AtomicInteger();
            resident.on(event -> callbacks.incrementAndGet());
            var config = new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var first = client.resumeSession(resident.getSessionId(), config);
            var second = client.resumeSession(resident.getSessionId(), config);
            stopping = client.stop();
            assertTrue(detachStarted.await(5, TimeUnit.SECONDS), "Stop must submit the provisional detach");

            firstResponse.completeExceptionally(new IllegalStateException("first resume rejected"));
            assertThrows(ExecutionException.class, first::get);
            secondResponse.completeExceptionally(new IllegalStateException("second resume rejected"));
            assertThrows(ExecutionException.class, second::get);

            assertSame(resident, trackedSession(client, resident.getSessionId()));
            trackedSession(client, resident.getSessionId())
                    .dispatchEvent(new com.microsoft.copilot.generated.SessionIdleEvent());
            assertEquals(1, callbacks.get(), "Stop must keep restored callbacks until detach completes");
            detach.complete(new CopilotSession.SessionDetachResponse(true, null));
            stopping.get(5, TimeUnit.SECONDS);
            assertNull(trackedSession(client, resident.getSessionId()));
            assertThrows(IllegalStateException.class, () -> resident.on(event -> {
            }));
        } finally {
            firstResponse.completeExceptionally(new IllegalStateException("test cleanup"));
            secondResponse.completeExceptionally(new IllegalStateException("test cleanup"));
            detach.complete(new CopilotSession.SessionDetachResponse(true, null));
            if (stopping != null) {
                stopping.get(5, TimeUnit.SECONDS);
            }
            client.forceStop().get();
        }
    }

    @Test
    void overlappingFailedResumesRetireResidentAfterDeletion() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var firstResponse = new CompletableFuture<ResumeSessionResponse>();
        var secondResponse = new CompletableFuture<ResumeSessionResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(firstResponse,
                secondResponse);
        when(rpc.invoke(eq("session.delete"), any(), eq(DeleteSessionResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(new DeleteSessionResponse(true, null)));
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var config = new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
            var first = client.resumeSession(resident.getSessionId(), config);
            var second = client.resumeSession(resident.getSessionId(), config);
            client.deleteSession(resident.getSessionId()).get();

            firstResponse.completeExceptionally(new IllegalStateException("first resume rejected"));
            assertThrows(ExecutionException.class, first::get);
            assertThrows(IllegalStateException.class, () -> resident.on(event -> {
            }));
            secondResponse.completeExceptionally(new IllegalStateException("second resume rejected"));

            assertThrows(ExecutionException.class, second::get);
            assertNull(trackedSession(client, resident.getSessionId()));
        } finally {
            firstResponse.completeExceptionally(new IllegalStateException("test cleanup"));
            secondResponse.completeExceptionally(new IllegalStateException("test cleanup"));
            client.forceStop().get();
        }
    }

    @Test
    void closeStopsTrackingSessionButKeepsResumedReplacement() throws Exception {
        var client = sessionTrackingClient(mock(JsonRpcClient.class), true);
        var resumeConfig = new ResumeSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));

        try {
            CopilotSession created = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            assertSame(created, trackedSession(client, "tracked-session"));
            created.close();
            assertNull(trackedSession(client, "tracked-session"));

            CopilotSession replaced = client.resumeSession("tracked-session", resumeConfig).get();
            CopilotSession replacement = client.resumeSession("tracked-session", resumeConfig).get();
            assertSame(replacement, trackedSession(client, "tracked-session"));
            replaced.close();
            assertSame(replacement, trackedSession(client, "tracked-session"));
            replacement.close();
            assertNull(trackedSession(client, "tracked-session"));
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    void failedResumeDoesNotRestoreAClosedResident() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var response = new CompletableFuture<ResumeSessionResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(response);
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            resident.close();
            response.completeExceptionally(new IllegalStateException("resume rejected"));

            assertThrows(ExecutionException.class, resuming::get);
            assertNull(trackedSession(client, resident.getSessionId()));
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    void failedResumeDoesNotRestoreADeletedResident() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var response = new CompletableFuture<ResumeSessionResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(response);
        when(rpc.invoke(eq("session.delete"), any(), eq(DeleteSessionResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(new DeleteSessionResponse(true, null)));
        when(rpc.invoke(eq("session.sendAndWait"), any(), eq(com.fasterxml.jackson.databind.JsonNode.class)))
                .thenReturn(CompletableFuture.failedFuture(new JsonRpcException(-32601, "Method not found")));
        when(rpc.invoke(eq("session.send"), any(), eq(SendMessageResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(new SendMessageResponse("pending-message")));
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var waiting = resident.sendAndWait(new MessageOptions().setPrompt("pending response"), 0);
            var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            client.deleteSession(resident.getSessionId()).get();
            response.completeExceptionally(new IllegalStateException("resume rejected"));

            assertThrows(ExecutionException.class, resuming::get);
            assertNull(trackedSession(client, resident.getSessionId()));
            assertThrows(IllegalStateException.class, () -> resident.on(event -> {
            }));
            var error = assertThrows(ExecutionException.class, () -> waiting.get(5, TimeUnit.SECONDS));
            assertEquals("Session closed before response completed", error.getCause().getMessage());
        } finally {
            response.completeExceptionally(new IllegalStateException("test cleanup"));
            client.forceStop().get();
        }
    }

    @Test
    void failedResumeKeepsAResidentWhoseDetachFailed() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, false);
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class)))
                .thenReturn(CompletableFuture.failedFuture(new IllegalStateException("resume rejected")));
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            assertThrows(IllegalStateException.class, resident::close);

            var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            assertThrows(ExecutionException.class, resuming::get);
            assertSame(resident, trackedSession(client, resident.getSessionId()));
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    void failedResumeDoesNotRestoreResidentAfterForceStop() throws Exception {
        for (boolean failedDetach : new boolean[]{false, true}) {
            var rpc = mock(JsonRpcClient.class);
            var client = sessionTrackingClient(rpc, false);
            var response = new CompletableFuture<ResumeSessionResponse>();
            when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(response);
            try {
                var resident = client.createSession(new CreateSessionOptions().setSessionId("tracked-session")
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                        .get();
                if (failedDetach) {
                    assertThrows(IllegalStateException.class, resident::close);
                }
                var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
                client.forceStop().get();
                response.completeExceptionally(new IllegalStateException("resume rejected"));

                assertThrows(ExecutionException.class, resuming::get);
                assertNull(trackedSession(client, resident.getSessionId()));
                assertThrows(IllegalStateException.class, () -> resident.on(event -> {
                }));
            } finally {
                response.completeExceptionally(new IllegalStateException("test cleanup"));
                client.forceStop().get();
            }
        }
    }

    @Test
    void failedResumeDuringForceStopRetiresDisplacedResident() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var response = new CompletableFuture<ResumeSessionResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(response);
        when(rpc.invoke(eq("session.sendAndWait"), any(), eq(com.fasterxml.jackson.databind.JsonNode.class)))
                .thenReturn(CompletableFuture.failedFuture(new JsonRpcException(-32601, "Method not found")));
        when(rpc.invoke(eq("session.send"), any(), eq(SendMessageResponse.class)))
                .thenReturn(CompletableFuture.completedFuture(new SendMessageResponse("pending-message")));
        var stopped = new CompletableFuture<Void>();
        var stopping = new Thread(() -> {
            try {
                client.forceStop().get();
                stopped.complete(null);
            } catch (Throwable error) {
                stopped.completeExceptionally(error);
            }
        }, "force-stop-pending-resume");
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var waiting = resident.sendAndWait(new MessageOptions().setPrompt("pending response"), 0);
            var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            var provisional = trackedSession(client, resident.getSessionId());
            synchronized (provisional) {
                stopping.start();
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
                while (stopping.getState() != Thread.State.BLOCKED && !stopped.isDone()
                        && System.nanoTime() < deadline) {
                    Thread.sleep(1);
                }
                assertEquals(Thread.State.BLOCKED, stopping.getState(),
                        "Force stop must reach provisional retirement before the resume rejection");
                response.completeExceptionally(new IllegalStateException("resume rejected"));
                assertThrows(ExecutionException.class, resuming::get);
            }
            stopped.get(5, TimeUnit.SECONDS);
            assertNull(trackedSession(client, resident.getSessionId()));
            assertThrows(IllegalStateException.class, () -> resident.on(event -> {
            }));
            var error = assertThrows(ExecutionException.class, () -> waiting.get(5, TimeUnit.SECONDS));
            assertEquals("Session closed before response completed", error.getCause().getMessage());
        } finally {
            response.completeExceptionally(new IllegalStateException("test cleanup"));
            stopping.join(5000);
            assertFalse(stopping.isAlive(), "The owned force-stop thread must exit");
            client.forceStop().get();
        }
    }

    @Test
    void staleResumeDoesNotDisplaceResidentOnReplacementConnection() throws Exception {
        var oldRpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(oldRpc, true);
        var field = CopilotClient.class.getDeclaredField("connectionFuture");
        field.setAccessible(true);
        var oldConnection = ((CompletableFuture<?>) field.get(client)).get();
        var startup = new CompletableFuture<Object>();
        setConnectionFuture(client, startup);
        when(oldRpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class)))
                .thenReturn(CompletableFuture.failedFuture(new IllegalStateException("old transport closed")));
        CompletableFuture<Void> stopping = null;
        try {
            var resuming = client.resumeSession("tracked-session", new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            stopping = client.stop();
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (client.getState() != ConnectionState.DISCONNECTED && System.nanoTime() < deadline) {
                Thread.sleep(1);
            }
            assertEquals(ConnectionState.DISCONNECTED, client.getState(),
                    "Cleanup must detach the old startup before installing its replacement");
            var nextRpc = mock(JsonRpcClient.class);
            setConnectionFuture(client, nextRpc, null);
            when(nextRpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenReturn(
                    CompletableFuture.completedFuture(new CreateSessionResponse("tracked-session", null, null, null)));
            when(nextRpc.invoke(eq("session.options.update"), any(), eq(Void.class)))
                    .thenReturn(CompletableFuture.completedFuture(null));
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            startup.complete(oldConnection);
            assertThrows(ExecutionException.class, resuming::get);
            stopping.get(5, TimeUnit.SECONDS);

            assertSame(resident, trackedSession(client, resident.getSessionId()));
            assertDoesNotThrow(() -> resident.on(event -> {
            }).close());
            verify(oldRpc, never()).invoke(eq("session.resume"), any(), any());
        } finally {
            startup.complete(oldConnection);
            if (stopping != null) {
                stopping.get(5, TimeUnit.SECONDS);
            }
            client.forceStop().get();
        }
    }

    @Test
    void failedResumeRestoresResidentOnConnectionAfterForceStop() throws Exception {
        var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setAutoStart(false).setExecutor(Runnable::run)
                .setRuntime(RuntimeConnection.forDefault().setBaseDirectory(System.getProperty("java.io.tmpdir"))));
        var rpc = mock(JsonRpcClient.class);
        when(rpc.invoke(eq("session.create"), any(), eq(CreateSessionResponse.class))).thenReturn(
                CompletableFuture.completedFuture(new CreateSessionResponse("restarted-session", null, null, null)));
        when(rpc.invoke(eq("session.options.update"), any(), eq(Void.class)))
                .thenReturn(CompletableFuture.completedFuture(null));
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class)))
                .thenReturn(CompletableFuture.failedFuture(new IllegalStateException("resume rejected")));
        try {
            client.forceStop().get();
            setConnectionFuture(client, rpc, null);
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("restarted-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));

            assertThrows(ExecutionException.class, resuming::get);
            assertSame(resident, trackedSession(client, resident.getSessionId()));
            assertDoesNotThrow(() -> resident.on(event -> {
            }).close());
        } finally {
            client.forceStop().get();
        }
    }

    @Test
    void failedResumeKeepsAClosingResidentUntilDetachSucceeds() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var response = new CompletableFuture<ResumeSessionResponse>();
        var detach = new CompletableFuture<CopilotSession.SessionDetachResponse>();
        when(rpc.invoke(eq("session.resume"), any(), eq(ResumeSessionResponse.class))).thenReturn(response);
        when(rpc.invoke(eq("session.detach"), any(), eq(CopilotSession.SessionDetachResponse.class)))
                .thenReturn(detach);
        try {
            var resident = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var resuming = client.resumeSession(resident.getSessionId(), new ResumeSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)));
            Thread.currentThread().interrupt();
            try {
                var error = assertThrows(IllegalStateException.class, resident::close);
                assertInstanceOf(InterruptedException.class, error.getCause());
            } finally {
                Thread.interrupted();
            }

            response.completeExceptionally(new IllegalStateException("resume rejected"));
            assertThrows(ExecutionException.class, resuming::get);
            assertSame(resident, trackedSession(client, resident.getSessionId()));

            detach.complete(new CopilotSession.SessionDetachResponse(true, null));
            assertNull(trackedSession(client, resident.getSessionId()));
        } finally {
            detach.complete(new CopilotSession.SessionDetachResponse(true, null));
            client.forceStop().get();
        }
    }

    @Test
    void interruptedCloseReleasesTrackingWhenDetachLaterSucceeds() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var client = sessionTrackingClient(rpc, true);
        var detach = new CompletableFuture<CopilotSession.SessionDetachResponse>();
        when(rpc.invoke(eq("session.detach"), any(), eq(CopilotSession.SessionDetachResponse.class)))
                .thenReturn(detach);
        try {
            var session = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            Thread.currentThread().interrupt();
            try {
                var error = assertThrows(IllegalStateException.class, session::close);
                assertInstanceOf(InterruptedException.class, error.getCause());
                assertTrue(Thread.currentThread().isInterrupted());
            } finally {
                Thread.interrupted();
            }
            assertSame(session, trackedSession(client, session.getSessionId()));

            detach.complete(new CopilotSession.SessionDetachResponse(true, null));

            assertNull(trackedSession(client, session.getSessionId()));
        } finally {
            detach.complete(new CopilotSession.SessionDetachResponse(true, null));
            client.forceStop().get();
        }
    }

    @Test
    void failedCloseKeepsSessionTracked() throws Exception {
        var client = sessionTrackingClient(mock(JsonRpcClient.class), false);

        try {
            CopilotSession session = client
                    .createSession(new CreateSessionOptions().setSessionId("tracked-session")
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();
            var error = assertThrows(IllegalStateException.class, session::close);
            assertTrue(error.getMessage().contains("busy"), error.getMessage());
            assertSame(session, trackedSession(client, "tracked-session"));
        } finally {
            client.forceStop().get();
        }
    }

    @SuppressWarnings("unchecked")
    private static CopilotSession trackedSession(CopilotClient client, String sessionId) throws Exception {
        Field sessionsField = CopilotClient.class.getDeclaredField("sessions");
        sessionsField.setAccessible(true);
        return ((Map<String, CopilotSession>) sessionsField.get(client)).get(sessionId);
    }

    @Test
    void testClientConstruction() {
        var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT));
        assertEquals(ConnectionState.DISCONNECTED, client.getState());
        client.close();
    }

    @Test
    void testClientConstructionWithOptions() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio("/path/to/cli"))
                .setObservability(new com.microsoft.copilot.rpc.ClientObservabilityOptions().setLogLevel("debug"))
                .setAutoStart(false);

        var client = new CopilotClient(options);
        assertEquals(ConnectionState.DISCONNECTED, client.getState());
        client.close();
    }

    @Test
    void testTcpConnectionDoesNotMutateOptions() {
        var connection = RuntimeConnection.forTcp("localhost", 3000).setConnectionToken("original-token");
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(connection);
        try (var client = new CopilotClient(options)) {
            assertSame(connection, options.getRuntime());
            var clientConnection = assertInstanceOf(com.microsoft.copilot.rpc.TcpRuntimeConnection.class,
                    client.getRuntimeConnection());
            assertNotSame(connection, clientConnection);
            assertEquals("localhost", clientConnection.getHost());
            assertEquals(3000, clientConnection.getPort());
            assertEquals("original-token", clientConnection.getConnectionToken());
            connection.setConnectionToken("replacement-token");
            assertEquals("original-token", clientConnection.getConnectionToken());
        }
    }

    @Test
    void testTcpConnectionConstruction() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 4321));

        var client = new CopilotClient(options);
        assertEquals(ConnectionState.DISCONNECTED, client.getState());
        assertInstanceOf(com.microsoft.copilot.rpc.TcpRuntimeConnection.class, client.getRuntimeConnection());
        client.close();
    }

    @Test
    void testIpv6TcpConnectionPreservesHost() throws Exception {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("::1", 4321)))) {
            Field hostField = CopilotClient.class.getDeclaredField("optionsHost");
            hostField.setAccessible(true);
            assertEquals("::1", hostField.get(client));
        }
    }

    @Test
    void testStartAndConnectUsingStdio() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath(cliPath)))) {
            client.start().get();
            assertEquals(ConnectionState.CONNECTED, client.getState());

            PingResponse pong = client.ping("test message").get();
            assertEquals("pong: test message", pong.message());
            assertNotNull(pong.timestamp());

            client.stop().get();
            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testShouldReportErrorWithStderrWhenCliFailsToStart() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(
                RuntimeConnection.forSpawnedStdio(cliPath).setArgs(List.of("--nonexistent-flag-for-testing")));

        try (var client = new CopilotClient(options)) {
            Exception ex = assertThrows(Exception.class, () -> client.start().get());
            Throwable root = ex instanceof ExecutionException && ex.getCause() != null ? ex.getCause() : ex;
            String message = root.getMessage();
            assertNotNull(message);
            assertTrue(message.contains("--nonexistent-flag-for-testing"),
                    "Error should include the CLI's invalid-flag diagnostic: " + message);
        }
    }

    @Test
    void testStartupWriteFailureDrainsStderrBeforeKillingChild(@TempDir Path tempDir) throws Exception {
        try (var controlServer = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))) {
            controlServer.setSoTimeout(10000);
            var script = tempDir.resolve("startup-error.js");
            Files.writeString(script, """
                    const fs = require('node:fs');
                    const control = require('node:net').createConnection(%d, '127.0.0.1');
                    control.once('data', () => {
                        fs.writeSync(2, 'error: --nonexistent-flag-for-testing\\n');
                        fs.closeSync(2);
                        control.end('stderr-written\\n', () => process.exit(1));
                    });
                    control.once('connect', () => control.write('started\\n'));
                    """.formatted(controlServer.getLocalPort()));
            var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                    .setRuntime(RuntimeConnection.forSpawnedStdio(script.toString()));
            var manager = spy(new CliServerManager(options, options.getRuntime()));
            var child = new AtomicReference<Process>();
            var failedOutput = new AtomicReference<OutputStream>();
            var controlSocket = new AtomicReference<Socket>();
            var controlReader = new AtomicReference<BufferedReader>();
            var writeFailure = new IOException("Controlled startup write failure");
            doAnswer(invocation -> {
                CliServerManager.ProcessInfo info = (CliServerManager.ProcessInfo) invocation.callRealMethod();
                child.set(info.process());
                // Closing fd 0 does not reliably fail the first parent write on Windows.
                var output = new FilterOutputStream(info.process().getOutputStream()) {
                    @Override
                    public void write(int value) throws IOException {
                        throw writeFailure;
                    }

                    @Override
                    public void write(byte[] bytes, int offset, int length) throws IOException {
                        throw writeFailure;
                    }

                    @Override
                    public void close() throws IOException {
                        // No write succeeds, so there is nothing to flush on this failed transport.
                        out.close();
                    }
                };
                failedOutput.set(output);
                var transportProcess = mock(Process.class, org.mockito.AdditionalAnswers.delegatesTo(info.process()));
                doReturn(output).when(transportProcess).getOutputStream();
                return new CliServerManager.ProcessInfo(transportProcess, info.port());
            }).when(manager).startCliServer();
            doAnswer(invocation -> {
                Process process = invocation.getArgument(0);
                var socket = controlServer.accept();
                controlSocket.set(socket);
                socket.setSoTimeout(10000);
                var reader = new BufferedReader(new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
                controlReader.set(reader);
                assertEquals("started", reader.readLine());
                // Closing the Java writer deterministically fails the startup write on every
                // OS.
                process.getOutputStream().close();
                return invocation.callRealMethod();
            }).when(manager).connectToServer(any(Process.class), isNull(), isNull());
            doAnswer(invocation -> {
                // Release stderr only when startup drains it, while the child is still alive.
                assertTrue(child.get().isAlive(), "Startup must drain stderr before killing the controlled child.");
                var control = controlSocket.get().getOutputStream();
                control.write(1);
                control.flush();
                // The real join waits for the reader, not for fixture notification delivery.
                assertEquals("stderr-written", controlReader.get().readLine());
                return invocation.callRealMethod();
            }).when(manager).awaitStderrReader();

            try (AutoCloseable childCleanup = () -> {
                var process = child.get();
                if (process != null && process.isAlive()) {
                    process.destroyForcibly();
                    assertTrue(process.waitFor(10, TimeUnit.SECONDS), "Could not reap the controlled child.");
                }
            }; var client = new CopilotClient(options); AutoCloseable outputCleanup = () -> {
                var output = failedOutput.get();
                if (output != null) {
                    output.close();
                }
            }; AutoCloseable controlCleanup = () -> {
                var socket = controlSocket.get();
                if (socket != null) {
                    socket.close();
                }
            }) {
                Field managerField = CopilotClient.class.getDeclaredField("serverManager");
                managerField.setAccessible(true);
                managerField.set(client, manager);

                var error = assertThrows(ExecutionException.class, () -> client.start().get(10, TimeUnit.SECONDS));
                var cause = error.getCause();
                assertTrue(cause.getMessage().contains("--nonexistent-flag-for-testing"),
                        "Startup lost the child's stderr: " + cause);
                assertInstanceOf(IOException.class, cause);
                Throwable original = cause;
                while (original.getCause() != null) {
                    original = original.getCause();
                }
                assertSame(writeFailure, original, "Startup must preserve the actual failed transport write.");
                assertNotSame(cause, original, "The startup error must retain its original transport failure.");
                assertFalse(child.get().isAlive(), "Failed startup retained its child process.");
            }
        }
    }

    @Test
    void testProtocolMismatchTerminatesLiveChildBeforeDrainingStderr(@TempDir Path tempDir) throws Exception {
        var script = tempDir.resolve("incompatible-server.js");
        Files.writeString(script, """
                let input = Buffer.alloc(0);
                process.stdin.on('data', chunk => {
                    input = Buffer.concat([input, chunk]);
                    const headerEnd = input.indexOf('\\r\\n\\r\\n');
                    if (headerEnd < 0) return;
                    const length = Number(/Content-Length:\\s*(\\d+)/i.exec(input.toString())[1]);
                    if (input.length < headerEnd + 4 + length) return;
                    const request = JSON.parse(input.subarray(headerEnd + 4, headerEnd + 4 + length));
                    const response = JSON.stringify({
                        jsonrpc: '2.0', id: request.id, result: { protocolVersion: 1 }
                    });
                    process.stdout.write(`Content-Length: ${Buffer.byteLength(response)}\\r\\n\\r\\n${response}`);
                    input = input.subarray(headerEnd + 4 + length);
                });
                setInterval(() => {}, 1000);
                """);
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio(script.toString()));
        var manager = spy(new CliServerManager(options, options.getRuntime()));
        var child = new AtomicReference<Process>();
        doAnswer(invocation -> {
            child.set(invocation.getArgument(0));
            return invocation.callRealMethod();
        }).when(manager).connectToServer(any(Process.class), isNull(), isNull());
        doAnswer(invocation -> {
            assertFalse(child.get().isAlive(), "A rejected live server must exit before its stderr reader is joined.");
            return invocation.callRealMethod();
        }).when(manager).awaitStderrReader();

        try (var client = new CopilotClient(options)) {
            Field managerField = CopilotClient.class.getDeclaredField("serverManager");
            managerField.setAccessible(true);
            managerField.set(client, manager);

            var error = assertThrows(ExecutionException.class, () -> client.start().get(10, TimeUnit.SECONDS));
            assertTrue(error.getCause().getMessage().contains("SDK protocol version mismatch"),
                    "Startup must report the incompatible protocol: " + error.getCause());
            assertFalse(child.get().isAlive(), "Failed startup retained its child process.");
            verify(manager).awaitStderrReader();
        } finally {
            if (child.get() != null && child.get().isAlive()) {
                child.get().destroyForcibly();
                assertTrue(child.get().waitFor(10, TimeUnit.SECONDS), "Could not reap the incompatible server.");
            }
        }
    }

    @Test
    void testStartAndConnectUsingTcp() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedTcp().setPath(cliPath)))) {
            client.start().get();
            assertEquals(ConnectionState.CONNECTED, client.getState());

            PingResponse pong = client.ping("test message").get();
            assertEquals("pong: test message", pong.message());

            client.stop().get();
        }
    }

    @Test
    void testForceStopWithoutCleanup() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio(cliPath)))) {
            client.createSession(new CreateSessionOptions()
                    .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))).get();
            client.forceStop().get();

            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testGitHubTokenOptionAccepted() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio("/path/to/cli"))
                .setGitHub(new ClientGitHubOptions().setToken("gho_test_token"));

        assertEquals("gho_test_token", options.getGitHub().getToken());
    }

    @Test
    void testUseLoggedInUserDefaultsToNull() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath("/path/to/cli"));

        assertTrue(options.getGitHub().getUseLoggedInUser().isEmpty());
    }

    @Test
    void testExplicitUseLoggedInUserFalse() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio("/path/to/cli"))
                .setGitHub(new ClientGitHubOptions().setUseLoggedInUser(false));

        assertEquals(Optional.of(false), options.getGitHub().getUseLoggedInUser());
    }

    @Test
    void testExplicitUseLoggedInUserTrueWithGitHubToken() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio("/path/to/cli"))
                .setGitHub(new ClientGitHubOptions().setToken("gho_test_token").setUseLoggedInUser(true));

        assertEquals(Optional.of(true), options.getGitHub().getUseLoggedInUser());
    }

    @Test
    void testGitHubTokenWithTcpConnectionThrows() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 8080))
                .setGitHub(new ClientGitHubOptions().setToken("gho_test_token"));

        assertThrows(IllegalArgumentException.class, () -> new CopilotClient(options));
    }

    @Test
    void testUseLoggedInUserWithTcpConnectionThrows() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forTcp("localhost", 8080))
                .setGitHub(new ClientGitHubOptions().setUseLoggedInUser(false));

        assertThrows(IllegalArgumentException.class, () -> new CopilotClient(options));
    }

    @Test
    void testSessionIdleTimeoutSecondsDefaultsToNull() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);

        assertTrue(com.microsoft.copilot.RuntimeSettings.idleTimeout(options.getRuntime()).isEmpty());
    }

    @Test
    void testSessionIdleTimeoutSecondsOptionAccepted() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forDefault().setSessionIdleTimeoutSeconds(600));

        assertEquals(600, RuntimeSettings.idleTimeout(options.getRuntime()).getAsInt());
    }

    @Test
    void testEmptyTcpConnectionTokenThrows() {
        for (var connection : List.of(RuntimeConnection.forSpawnedTcp().setConnectionToken(""),
                RuntimeConnection.forTcp("localhost", 3000).setConnectionToken(""))) {
            var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(connection);
            assertThrows(IllegalArgumentException.class, () -> new CopilotClient(options));
        }
    }

    @Test
    void testTcpConnectionTokenAcceptedInTcpMode() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedTcp().setConnectionToken("my-token"));

        // Should not throw
        try (var client = new CopilotClient(options)) {
            assertNotNull(client);
        }
    }

    @Test
    void testCopilotHomeOptionSetOnOptions() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forDefault().setBaseDirectory("/custom/home"));

        assertEquals("/custom/home", RuntimeSettings.baseDirectory(options.getRuntime()));
    }

    // ===== onLifecycle tests =====

    /**
     * Gets the internal LifecycleEventManager from a CopilotClient via reflection
     * so we can dispatch events for testing.
     */
    private static LifecycleEventManager getLifecycleManager(CopilotClient client) throws Exception {
        Field f = CopilotClient.class.getDeclaredField("lifecycleManager");
        f.setAccessible(true);
        return (LifecycleEventManager) f.get(client);
    }

    private static SessionLifecycleEvent lifecycleEvent(String type) {
        var e = new SessionLifecycleEvent();
        e.setType(type);
        e.setSessionId("test-session-id");
        return e;
    }

    @Test
    void testOnLifecycleWildcardReceivesAllEvents() throws Exception {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            var received = new ArrayList<SessionLifecycleEvent>();
            client.onLifecycle(received::add);

            LifecycleEventManager mgr = getLifecycleManager(client);
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.CREATED));
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.DELETED));

            assertEquals(2, received.size());
            assertEquals(SessionLifecycleEventTypes.CREATED, received.get(0).getType());
            assertEquals(SessionLifecycleEventTypes.DELETED, received.get(1).getType());
        }
    }

    @Test
    void testOnLifecycleTypedReceivesOnlyMatchingEvents() throws Exception {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            var received = new ArrayList<SessionLifecycleEvent>();
            client.onLifecycle(SessionLifecycleEventTypes.CREATED, received::add);

            LifecycleEventManager mgr = getLifecycleManager(client);
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.CREATED));
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.DELETED));

            assertEquals(1, received.size());
            assertEquals(SessionLifecycleEventTypes.CREATED, received.get(0).getType());
        }
    }

    @Test
    void testOnLifecycleUnsubscribeStopsDelivery() throws Exception {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            var received = new ArrayList<SessionLifecycleEvent>();
            AutoCloseable sub = client.onLifecycle(received::add);

            LifecycleEventManager mgr = getLifecycleManager(client);
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.CREATED));
            assertEquals(1, received.size());

            sub.close();

            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.DELETED));
            assertEquals(1, received.size(), "Should not receive events after unsubscribe");
        }
    }

    @Test
    void testOnLifecycleTypedUnsubscribeStopsDelivery() throws Exception {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            var received = new ArrayList<SessionLifecycleEvent>();
            AutoCloseable sub = client.onLifecycle(SessionLifecycleEventTypes.UPDATED, received::add);

            LifecycleEventManager mgr = getLifecycleManager(client);
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.UPDATED));
            assertEquals(1, received.size());

            sub.close();

            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.UPDATED));
            assertEquals(1, received.size(), "Should not receive events after unsubscribe");
        }
    }

    @Test
    void testOnLifecycleMultipleHandlers() throws Exception {
        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT))) {
            var wildcard = new ArrayList<SessionLifecycleEvent>();
            var typed = new ArrayList<SessionLifecycleEvent>();

            client.onLifecycle(wildcard::add);
            client.onLifecycle(SessionLifecycleEventTypes.CREATED, typed::add);

            LifecycleEventManager mgr = getLifecycleManager(client);
            mgr.dispatch(lifecycleEvent(SessionLifecycleEventTypes.CREATED));

            assertEquals(1, wildcard.size());
            assertEquals(1, typed.size());
        }
    }

    // ===== getState() coverage =====

    @Test
    void testGetStateDoesNotReportConnectedWhenStartFailsDuringStateRead() throws Exception {
        var failure = new IOException("Startup failed");
        // Complete after the first status observation, whichever status getState reads
        // first.
        var future = new CompletableFuture<Object>() {
            @Override
            public boolean isDone() {
                boolean done = super.isDone();
                completeExceptionally(failure);
                return done;
            }

            @Override
            public boolean isCompletedExceptionally() {
                boolean failed = super.isCompletedExceptionally();
                completeExceptionally(failure);
                return failed;
            }
        };

        try (var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            setConnectionFuture(client, future);

            ConnectionState state = client.getState();
            assertTrue(state == ConnectionState.CONNECTING || state == ConnectionState.ERROR,
                    "Failed startup must not report CONNECTED, was: " + state);
            assertTrue(future.isCompletedExceptionally());
            assertEquals(ConnectionState.ERROR, client.getState());
        }
    }

    @Test
    void testGetStateErrorAfterFailedStart() throws Exception {
        // Use a non-existent CLI path to trigger a startup failure
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath("/nonexistent/path/to/cli"))
                .setAutoStart(false);

        try (var client = new CopilotClient(options)) {
            // Manually start to trigger the error
            CompletableFuture<Void> startFuture = client.start();

            // Wait for the start to fail
            try {
                startFuture.get();
            } catch (ExecutionException e) {
                // Expected
            }

            assertEquals(ConnectionState.ERROR, client.getState());
        }
    }

    @Test
    void testGetStateConnectingDuringStart() throws Exception {
        // Use a non-existent CLI path; the future won't complete immediately
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath("/nonexistent/path/to/cli"))
                .setAutoStart(false);

        try (var client = new CopilotClient(options)) {
            // Start is async - grab state before completion
            client.start();

            // The state should be either CONNECTING or ERROR depending on timing
            ConnectionState state = client.getState();
            assertTrue(state == ConnectionState.CONNECTING || state == ConnectionState.ERROR,
                    "State should be CONNECTING or ERROR, was: " + state);
        }
    }

    @Test
    void testGetStateDisconnectedAfterRuntimeConnectionCloses() throws Exception {
        var runtimeOutput = new PipedOutputStream();
        var rpc = JsonRpcClient.fromStreams(new PipedInputStream(runtimeOutput), OutputStream.nullOutputStream());

        try (var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            setConnectionFuture(client, rpc, null);
            assertEquals(ConnectionState.CONNECTED, client.getState());

            // The runtime goes away without the client being stopped.
            runtimeOutput.close();
            assertTrue(rpc.awaitPeerEof(5, TimeUnit.SECONDS), "Reader should observe the runtime's EOF");

            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testGetStateFromAnotherThreadDuringPendingRequestCompletion() throws Exception {
        var stateReader = Executors.newSingleThreadExecutor();
        try (var runtimeOutput = new PipedOutputStream();
                var rpc = JsonRpcClient.fromStreams(new PipedInputStream(runtimeOutput),
                        OutputStream.nullOutputStream());
                var client = new CopilotClient(
                        new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            setConnectionFuture(client, rpc, null);
            assertEquals(ConnectionState.CONNECTED, client.getState());
            var ping = client.ping("pending");
            assertFalse(ping.isDone(), "The request must remain pending until the runtime disconnects");
            var stateDuringCompletion = ping.handle((response, error) -> {
                assertNotNull(error, "Disconnection should fail the pending request");
                return assertDoesNotThrow(() -> stateReader.submit(client::getState).get(5, TimeUnit.SECONDS),
                        "A completion handler must be able to wait for another thread to read getState()");
            });

            runtimeOutput.close();

            assertEquals(ConnectionState.DISCONNECTED, stateDuringCompletion.get(10, TimeUnit.SECONDS));
            assertTrue(rpc.awaitPeerEof(5, TimeUnit.SECONDS), "Reader should finish after completing the request");
        } finally {
            stateReader.shutdownNow();
            assertTrue(stateReader.awaitTermination(5, TimeUnit.SECONDS), "State reader should stop");
        }
    }

    // ===== ensureConnected throws when autoStart=false and not connected =====

    @Test
    void testEnsureConnectedThrowsWhenNotStartedAndAutoStartDisabled() {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false);

        try (var client = new CopilotClient(options)) {
            // Calling ping (which calls ensureConnected) without start() should throw
            assertThrows(IllegalStateException.class, () -> client.ping("test"));
        }
    }

    // ===== close() idempotency =====

    @Test
    void testCloseIsIdempotent() {
        var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT));

        // First close
        client.close();
        // Second close should not throw
        assertDoesNotThrow(() -> client.close());
    }

    @Test
    void testCloseAfterFailedStart() throws Exception {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath("/nonexistent/path/to/cli"))
                .setAutoStart(false);
        var client = new CopilotClient(options);

        CompletableFuture<Void> startFuture = client.start();
        try {
            startFuture.get();
        } catch (ExecutionException e) {
            // Expected
        }

        // close() after a failed start should not throw
        assertDoesNotThrow(() -> client.close());
    }

    // ===== stop() with no connection =====

    @Test
    void testStopWithNoConnectionCompletes() throws Exception {
        try (var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            // stop() without start() should complete without error
            client.stop().get();
            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testForceStopWithNoConnectionCompletes() throws Exception {
        try (var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            // forceStop() without start() should complete without error
            client.forceStop().get();
            assertEquals(ConnectionState.DISCONNECTED, client.getState());
        }
    }

    @Test
    void testCloseSessionAfterStoppingClientDoesNotThrow() throws Exception {
        assertNotNull(cliPath, "Copilot CLI not found in PATH or COPILOT_CLI_PATH");

        try (var client = new CopilotClient(new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath(cliPath)))) {
            var session = client
                    .createSession(new CreateSessionOptions()
                            .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get();

            // Stop the client first (which closes the RPC connection)
            client.stop().get();

            // Then close the session - should not throw even though RPC is closed
            assertDoesNotThrow(() -> session.close(), "Closing session after client.stop() should not throw exception");

            // Verify session is terminated
            assertThrows(IllegalStateException.class, () -> session.send("test"),
                    "Session should be terminated after close()");
        }
    }

    // ===== start() idempotency =====

    @Test
    void testStartIsIdempotentSingleConnectionAttempt() throws Exception {
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setRuntime(RuntimeConnection.forSpawnedStdio().setPath("/nonexistent/path/to/cli"))
                .setAutoStart(false);

        try (var client = new CopilotClient(options)) {
            client.start();
            client.start();

            // Both calls should result in the same state (single connection attempt)
            ConnectionState state = client.getState();
            assertTrue(state == ConnectionState.CONNECTING || state == ConnectionState.ERROR,
                    "State should be CONNECTING or ERROR after start(), was: " + state);
        }
    }

    // ===== required options =====

    @Test
    void testNullOptionsRejected() {
        assertThrows(IllegalArgumentException.class, () -> new CopilotClient(null));
    }

    // ===== OnListModels =====

    @Test
    void testListModels_WithCustomHandler_CallsHandler() throws Exception {
        var customModels = new ArrayList<Model>();
        var model = testModel("my-custom-model");
        customModels.add(model);

        var callCount = new int[]{0};
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> {
                    callCount[0]++;
                    return CompletableFuture.completedFuture(new ArrayList<>(customModels));
                }));

        try (var client = new CopilotClient(options)) {
            var models = client.listModels().get();
            assertEquals(1, callCount[0]);
            assertEquals(1, models.size());
            assertEquals("my-custom-model", models.get(0).id());
        }
    }

    @Test
    void testListModels_WithCustomHandler_CachesResults() throws Exception {
        var customModels = new ArrayList<Model>();
        var model = testModel("cached-model");
        customModels.add(model);

        var callCount = new int[]{0};
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> {
                    callCount[0]++;
                    return CompletableFuture.completedFuture(new ArrayList<>(customModels));
                }));

        try (var client = new CopilotClient(options)) {
            client.listModels().get();
            client.listModels().get();
            assertEquals(1, callCount[0], "Handler should be called only once due to caching");
        }
    }

    @Test
    void testListModels_WithCustomHandler_WorksWithoutStart() throws Exception {
        var customModels = new ArrayList<Model>();
        var model = testModel("no-start-model");
        customModels.add(model);

        var callCount = new int[]{0};
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> {
                    callCount[0]++;
                    return CompletableFuture.completedFuture(new ArrayList<>(customModels));
                }));

        // No start() needed when onListModels is provided
        try (var client = new CopilotClient(options)) {
            var models = client.listModels().get();
            assertEquals(1, callCount[0]);
            assertEquals(1, models.size());
            assertEquals("no-start-model", models.get(0).id());
        }
    }

    @Test
    void testListModels_WithCustomHandler_ConcurrentCallsShareOneFetch() throws Exception {
        var model = testModel("concurrent-model");
        var gate = new CompletableFuture<List<Model>>();
        var callCount = new AtomicInteger();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> {
                    callCount.incrementAndGet();
                    return gate;
                }));

        try (var client = new CopilotClient(options)) {
            var calls = List.of(client.listModels(), client.listModels(), client.listModels());
            assertEquals(1, callCount.get(), "Calls made while the first fetch is in flight should wait for it");

            gate.complete(List.of(model));

            for (var call : calls) {
                assertEquals("concurrent-model", call.get(5, TimeUnit.SECONDS).get(0).id());
            }
            client.listModels().get(5, TimeUnit.SECONDS);
            assertEquals(1, callCount.get(), "Handler should be called only once");
        }
    }

    @Test
    void testListModels_WithCustomHandler_RetriesOneQueuedCallAfterFailure() throws Exception {
        var model = testModel("retried-model");
        var firstFetch = new CompletableFuture<List<Model>>();
        var retryFetch = new CompletableFuture<List<Model>>();
        var retryStarted = new CompletableFuture<Void>();
        var callCount = new AtomicInteger();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> {
                    if (callCount.incrementAndGet() == 1) {
                        return firstFetch;
                    }
                    retryStarted.complete(null);
                    return retryFetch;
                }));

        try (var client = new CopilotClient(options)) {
            var first = client.listModels();
            var second = client.listModels();
            var third = client.listModels();
            assertEquals(1, callCount.get());

            firstFetch.completeExceptionally(new RuntimeException("transient failure"));

            var error = assertThrows(ExecutionException.class, () -> first.get(5, TimeUnit.SECONDS));
            assertEquals("transient failure", error.getCause().getMessage());
            retryStarted.get(5, TimeUnit.SECONDS);
            assertEquals(2, callCount.get(), "The next queued call should retry the failed fetch");
            assertFalse(third.isDone(), "Only one queued call should retry at a time");

            retryFetch.complete(List.of(model));

            assertEquals("retried-model", second.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals("retried-model", third.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals(2, callCount.get());
        }
    }

    @Test
    void testListModels_WithCustomHandler_StopRetiresPendingFetches() throws Exception {
        var oldModel = testModel("old-model");
        var newModel = testModel("new-model");
        var oldFetch = new CompletableFuture<List<Model>>();
        var newFetch = new CompletableFuture<List<Model>>();
        var callCount = new AtomicInteger();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setModels(
                new ClientModelOptions().setOnListModels(() -> callCount.incrementAndGet() == 1 ? oldFetch : newFetch));

        try (var client = new CopilotClient(options)) {
            var oldCall = client.listModels();
            var queuedOldCall = client.listModels();
            client.stop().get(5, TimeUnit.SECONDS);

            var newCall = client.listModels();
            assertEquals(2, callCount.get(), "A post-stop call must not wait for the old fetch");
            oldFetch.complete(List.of(oldModel));
            assertEquals("old-model", oldCall.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals("old-model", queuedOldCall.get(5, TimeUnit.SECONDS).get(0).id());
            var queuedNewCall = client.listModels();
            assertFalse(queuedNewCall.isDone(), "An old fetch must not populate the new cache");

            newFetch.complete(List.of(newModel));
            assertEquals("new-model", newCall.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals("new-model", queuedNewCall.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals("new-model", client.listModels().get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals(2, callCount.get());
        }
    }

    @Test
    void testListModels_WithCustomHandler_FailureCallbackCanRetrySynchronously() throws Exception {
        var model = testModel("retried-model");
        var firstFetch = new CompletableFuture<List<Model>>();
        var callCount = new AtomicInteger();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> callCount.incrementAndGet() == 1
                        ? firstFetch
                        : CompletableFuture.completedFuture(List.of(model))));

        try (var client = new CopilotClient(options)) {
            var retried = client.listModels()
                    .exceptionally(error -> client.listModels().orTimeout(5, TimeUnit.SECONDS).join());

            firstFetch.completeExceptionally(new RuntimeException("transient failure"));

            assertEquals("retried-model", retried.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals(2, callCount.get());
        }
    }

    @Test
    void testListModels_WithCustomHandler_QueuedCallbackCanJoinFirstCall() throws Exception {
        var model = testModel("shared-model");
        var fetch = new CompletableFuture<List<Model>>();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> fetch));

        try (var client = new CopilotClient(options)) {
            var first = client.listModels();
            var second = client.listModels();
            var joined = second.thenApply(ignored -> first.copy().orTimeout(5, TimeUnit.SECONDS).join());

            fetch.complete(List.of(model));

            assertEquals("shared-model", joined.get(5, TimeUnit.SECONDS).get(0).id());
        }
    }

    @Test
    void testListModels_WithCustomHandler_CancellationDoesNotReleaseFetch() throws Exception {
        var model = testModel("shared-model");
        var fetch = new CompletableFuture<List<Model>>();
        var callCount = new AtomicInteger();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
                .setModels(new ClientModelOptions().setOnListModels(() -> {
                    callCount.incrementAndGet();
                    return fetch;
                }));

        try (var client = new CopilotClient(options)) {
            client.listModels().cancel(true);
            var second = client.listModels();
            assertEquals(1, callCount.get());
            assertFalse(second.isDone());

            fetch.complete(List.of(model));

            assertEquals("shared-model", second.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals(1, callCount.get());
        }
    }

    @Test
    void testListModels_ConcurrentCallsShareOneModelsListRequest() throws Exception {
        var model = testModel("server-model");
        var response = new CompletableFuture<GetModelsResponse>();
        var rpc = mock(JsonRpcClient.class);
        when(rpc.invoke(eq("models.list"), any(), eq(GetModelsResponse.class))).thenReturn(response);

        try (var client = new CopilotClient(
                new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setAutoStart(false))) {
            setConnectionFuture(client, rpc, null);

            var first = client.listModels();
            var second = client.listModels();
            verify(rpc, times(1)).invoke(eq("models.list"), any(), eq(GetModelsResponse.class));

            response.complete(new GetModelsResponse().setModels(List.of(model)));

            assertEquals("server-model", first.get(5, TimeUnit.SECONDS).get(0).id());
            assertEquals("server-model", second.get(5, TimeUnit.SECONDS).get(0).id());
            verify(rpc, times(1)).invoke(eq("models.list"), any(), eq(GetModelsResponse.class));
        }
    }

    private static Model testModel(String id) {
        return Model.builder().id(id).name(id)
                .capabilities(ModelCapabilities.builder().supports(ModelCapabilitiesSupports.builder().build())
                        .limits(ModelCapabilitiesLimits.builder().maxContextWindowTokens(8192L).build()).build())
                .build();
    }

    private static void setConnectionFuture(CopilotClient client, JsonRpcClient rpc, Process process) throws Exception {
        var connectionClass = Class.forName("com.microsoft.copilot.CopilotClient$Connection");
        var constructor = connectionClass.getDeclaredConstructor(JsonRpcClient.class, Process.class,
                com.microsoft.copilot.generated.rpc.ServerRpc.class, AutoCloseable.class);
        constructor.setAccessible(true);
        var connection = constructor.newInstance(rpc, process, null, null);

        setConnectionFuture(client, CompletableFuture.completedFuture(connection));
    }

    private static void setConnectionFuture(CopilotClient client, CompletableFuture<?> future) throws Exception {
        Field field = CopilotClient.class.getDeclaredField("connectionFuture");
        field.setAccessible(true);
        field.set(client, future);
    }

    @AllowCopilotExperimental
    private static SkillProvider testSkillProvider() {
        return new SkillProvider() {
            @Override
            public CompletableFuture<List<SkillProviderDescriptor>> listSkills() {
                return CompletableFuture.completedFuture(List.of());
            }

            @Override
            public CompletableFuture<String> readSkill(String name) {
                return CompletableFuture.completedFuture(null);
            }
        };
    }

    @AllowCopilotExperimental
    @SuppressWarnings("unchecked")
    private static SkillProvider registeredSkillProvider(CopilotClient client, String sessionId) throws Exception {
        Field sessionsField = CopilotClient.class.getDeclaredField("sessions");
        sessionsField.setAccessible(true);
        Map<String, CopilotSession> sessions = (Map<String, CopilotSession>) sessionsField.get(client);
        CopilotSession session = sessions.get(sessionId);
        return session != null ? session.getSkillProvider() : null;
    }
}
