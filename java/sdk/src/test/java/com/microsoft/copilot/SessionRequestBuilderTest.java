/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CreateSessionFilesystemOptions;
import com.microsoft.copilot.rpc.CreateSessionInstructionOptions;
import com.microsoft.copilot.rpc.SessionAgentOptions;
import com.microsoft.copilot.rpc.SessionEventOptions;
import com.microsoft.copilot.rpc.SessionExecutionOptions;
import com.microsoft.copilot.rpc.SessionExperimentOptions;
import com.microsoft.copilot.rpc.SessionFilesystemOptions;
import com.microsoft.copilot.rpc.SessionGitHubOptions;
import com.microsoft.copilot.rpc.SessionGitOptions;
import com.microsoft.copilot.rpc.SessionHookOptions;
import com.microsoft.copilot.rpc.SessionInferenceOptions;
import com.microsoft.copilot.rpc.SessionInstructionOptions;
import com.microsoft.copilot.rpc.SessionInteractionOptions;
import com.microsoft.copilot.rpc.SessionMcpOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import com.microsoft.copilot.rpc.SessionPluginOptions;
import com.microsoft.copilot.rpc.SessionSkillOptions;
import com.microsoft.copilot.rpc.SessionStateOptions;
import com.microsoft.copilot.rpc.SessionToolOptions;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.junit.jupiter.api.Test;

import com.microsoft.copilot.generated.rpc.DiagnosticLogLevel;
import com.microsoft.copilot.generated.rpc.DiagnosticSourcesConfiguration;
import com.microsoft.copilot.generated.rpc.DiagnosticsConfiguration;
import com.microsoft.copilot.generated.rpc.McpDiagnosticSourceConfiguration;
import com.microsoft.copilot.generated.rpc.PermissionMode;
import com.microsoft.copilot.generated.rpc.ProtocolExternalToolDefinition;
import com.microsoft.copilot.generated.rpc.SessionPermissionsSetModeResult;
import com.microsoft.copilot.generated.rpc.SessionOpenOptions;
import com.microsoft.copilot.generated.rpc.SandboxConfig;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
import com.microsoft.copilot.generated.rpc.AutopilotOptions;
import com.microsoft.copilot.rpc.AskUserVariant;
import com.microsoft.copilot.rpc.AutoModeSwitchResponse;
import com.microsoft.copilot.rpc.CloudSessionOptions;
import com.microsoft.copilot.rpc.CloudSessionRepository;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotExpAssignmentResponse;
import com.microsoft.copilot.rpc.CreateSessionRequest;
import com.microsoft.copilot.rpc.CreateSessionResponse;
import com.microsoft.copilot.rpc.DefaultAgentConfig;
import com.microsoft.copilot.rpc.ElicitationHandler;
import com.microsoft.copilot.rpc.ElicitationResult;
import com.microsoft.copilot.rpc.ElicitationResultAction;
import com.microsoft.copilot.rpc.ExitPlanModeResult;
import com.microsoft.copilot.rpc.ExpConfigEntry;
import com.microsoft.copilot.rpc.GitHubMcpToolConfig;
import com.microsoft.copilot.rpc.GitHubTokenProviderResult;
import com.microsoft.copilot.rpc.LargeToolOutputConfig;
import com.microsoft.copilot.rpc.MemoryConfiguration;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.ResumeSessionRequest;
import com.microsoft.copilot.rpc.ResumeSessionResponse;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.SessionHooks;
import com.microsoft.copilot.rpc.ToolDefinition;
import com.microsoft.copilot.rpc.UserInputResponse;

/**
 * Unit tests for {@link SessionRequestBuilder} branch coverage.
 * <p>
 * Exercises branches in buildCreateRequest, buildResumeRequest, and
 * configureSession that are not reached by E2E tests.
 */
public class SessionRequestBuilderTest {

    @Test
    @SuppressWarnings("deprecation")
    void externalToolDefinitionBuilderPreservesWireFields() throws Exception {
        var definition = ProtocolExternalToolDefinition.builder().name("lookup").description("Description")
                .skipPermission(true).isTerminal(true).metadata(Map.of("custom", "kept")).build();
        assertNull(definition.hasHandler());
        var mapper = JsonRpcClient.getObjectMapper();
        assertEquals(mapper.readTree(
                "{\"name\":\"lookup\",\"description\":\"Description\",\"skipPermission\":true,\"isTerminal\":true,\"metadata\":{\"custom\":\"kept\"}}"),
                mapper.readTree(mapper.writeValueAsBytes(definition)));
    }

    @Test
    void sessionOpenOptionsBuilderPreservesParallelToolChoiceOnTheWire() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (Boolean choice : new Boolean[]{null, false, true}) {
            var options = SessionOpenOptions.builder()
                    .inference(com.microsoft.copilot.generated.rpc.SessionOpenInferenceOptions.builder()
                            .allowMultipleToolCalls(choice).build())
                    .build();
            var wire = mapper.readTree(mapper.writeValueAsBytes(options));
            if (choice == null) {
                assertFalse(wire.path("inference").has("allowMultipleToolCalls"), wire.toString());
            } else {
                assertEquals(choice.booleanValue(), wire.path("inference").path("allowMultipleToolCalls").asBoolean(),
                        wire.toString());
                assertTrue(wire.path("inference").has("allowMultipleToolCalls"), wire.toString());
            }
            assertEquals(choice,
                    mapper.treeToValue(wire, SessionOpenOptions.class).inference().allowMultipleToolCalls());
        }
    }

    @Test
    void toolFilterPrecedenceIsOmittedInAllModes() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (var mode : new CopilotClientMode[]{CopilotClientMode.CODING_AGENT, CopilotClientMode.EMPTY}) {
            for (var request : new Object[]{
                    SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "s1", mode),
                    SessionRequestBuilder.buildResumeRequest("s1", new ResumeSessionOptions(), mode),
                    SessionRequestBuilder.buildCreateRequest(null, "s1", mode),
                    SessionRequestBuilder.buildResumeRequest("s1", null, mode)}) {
                var json = mapper.readTree(mapper.writeValueAsBytes(request));
                assertFalse(json.has("toolFilterPrecedence"), mode + " must use the runtime default");
            }
        }
    }

    @Test
    void transcriptRecoveryOverridesAndResponseInAllModes() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (var mode : new CopilotClientMode[]{CopilotClientMode.CODING_AGENT, CopilotClientMode.EMPTY}) {
            for (Boolean choice : new Boolean[]{null, false, true}) {
                var config = new ResumeSessionOptions();
                if (choice != null) {
                    config.setAllowTranscriptRecovery(choice);
                }
                var request = SessionRequestBuilder.buildResumeRequest("s1", config.clone(), mode);
                var requestJson = mapper.readTree(mapper.writeValueAsBytes(request));
                if (choice == null) {
                    assertFalse(requestJson.has("allowTranscriptRecovery"));
                } else {
                    assertTrue(requestJson.has("allowTranscriptRecovery"));
                    assertEquals(choice.booleanValue(), requestJson.path("allowTranscriptRecovery").asBoolean());
                }
            }
        }

        var response = mapper
                .readValue(
                        "{\"sessionId\":\"s1\",\"transcriptRecovery\":{\"plannedBackupPath\":\"backup.jsonl\","
                                + "\"invalidLineNumbers\":[3,4],\"sessionStartMoved\":true}}",
                        ResumeSessionResponse.class);
        assertNotNull(response.transcriptRecovery());
        assertEquals("backup.jsonl", response.transcriptRecovery().plannedBackupPath());
        assertEquals(List.of(3, 4), response.transcriptRecovery().invalidLineNumbers());
        assertTrue(response.transcriptRecovery().sessionStartMoved());
        assertNull(mapper.readValue("{\"sessionId\":\"s1\"}", ResumeSessionResponse.class).transcriptRecovery());
        assertNull(new ResumeSessionResponse("s1", null, null, null).transcriptRecovery());
    }

    @Test
    void clearingTranscriptRecoveryRestoresRuntimeDefaultInAllModes() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (boolean choice : new boolean[]{false, true}) {
            var config = new ResumeSessionOptions().setAllowTranscriptRecovery(choice);
            assertEquals(choice, config.getAllowTranscriptRecovery().orElseThrow());
            assertSame(config, config.clearAllowTranscriptRecovery());
            assertTrue(config.getAllowTranscriptRecovery().isEmpty());
            assertFalse(mapper.readTree(mapper.writeValueAsBytes(config)).has("allowTranscriptRecovery"));

            var cli = SessionRequestBuilder.buildResumeRequest("s1", config.clone(), CopilotClientMode.CODING_AGENT);
            var empty = SessionRequestBuilder.buildResumeRequest("s1", config.clone(), CopilotClientMode.EMPTY);
            assertFalse(mapper.readTree(mapper.writeValueAsBytes(cli)).has("allowTranscriptRecovery"));
            assertFalse(mapper.readTree(mapper.writeValueAsBytes(empty)).has("allowTranscriptRecovery"));
        }
    }

    @Test
    void customizationDirectoriesArePreservedOnCreateAndResume() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (int kind = 0; kind < 3; kind++) {
            List<String> roots = kind == 0 ? null : kind == 1 ? List.of() : List.of("relative-root", "other-root");
            var directories = new HashMap<String, List<String>>();
            for (var type : List.of("skill", "agent", "instruction", "hook")) {
                directories.put(type + "Directories",
                        roots == null ? null : roots.stream().map(root -> root + "-" + type).toList());
            }
            var create = SessionRequestBuilder.buildCreateRequest(
                    new CreateSessionOptions().setWorkingDirectory("request-cwd").setEnableConfigDiscovery(false)
                            .setHooks(new SessionHookOptions().setEnableRepositoryHooks(false)
                                    .setDirectories(directories.get("hookDirectories")))
                            .setSkills(new SessionSkillOptions().setDirectories(directories.get("skillDirectories")))
                            .setAgents(new SessionAgentOptions().setDirectories(directories.get("agentDirectories")))
                            .setInstructions(new CreateSessionInstructionOptions()
                                    .setDirectories(directories.get("instructionDirectories"))),
                    "created");
            var resume = SessionRequestBuilder.buildResumeRequest("resumed", new ResumeSessionOptions()
                    .setWorkingDirectory("resume-cwd").setEnableConfigDiscovery(false)
                    .setHooks(new SessionHookOptions().setEnableRepositoryHooks(false)
                            .setDirectories(directories.get("hookDirectories")))
                    .setSkills(new SessionSkillOptions().setDirectories(directories.get("skillDirectories")))
                    .setAgents(new SessionAgentOptions().setDirectories(directories.get("agentDirectories")))
                    .setInstructions(
                            new SessionInstructionOptions().setDirectories(directories.get("instructionDirectories"))));
            for (var request : List.of(create, resume)) {
                var wire = mapper.readTree(mapper.writeValueAsBytes(request));
                for (var entry : directories.entrySet()) {
                    if (entry.getValue() == null) {
                        assertFalse(wire.path(entry.getKey().replace("Directories", "s")).has("directories"),
                                wire.toString());
                    } else {
                        assertEquals(mapper.valueToTree(entry.getValue()),
                                wire.path(entry.getKey().replace("Directories", "s")).get("directories"));
                    }
                }
                assertFalse(wire.path("enableConfigDiscovery").asBoolean(true));
                assertFalse(wire.path("hooks").path("enableRepositoryHooks").asBoolean(true));
                assertFalse(wire.path("filesystem").has("additionalDirectories"));
                assertFalse(wire.path("agents").has("definitions"));
                assertFalse(wire.path("hooks").path("hasHandlers").asBoolean());
            }
        }
    }

    @Test
    void customizationDirectoryClonesOwnTheirLists() {
        var agents = new ArrayList<>(List.of("agents"));
        var hooks = new ArrayList<>(List.of("hooks"));
        var create = new CreateSessionOptions().setAgents(new SessionAgentOptions().setDirectories(agents))
                .setHooks(new SessionHookOptions().setDirectories(hooks));
        var resume = new ResumeSessionOptions().setAgents(new SessionAgentOptions().setDirectories(agents))
                .setHooks(new SessionHookOptions().setDirectories(hooks));
        var createClone = create.clone();
        var resumeClone = resume.clone();
        agents.add("added-agents");
        hooks.add("added-hooks");
        assertEquals(List.of("agents"), create.getAgents().getDirectories());
        assertEquals(List.of("hooks"), create.getHooks().getDirectories());
        assertEquals(List.of("agents"), resume.getAgents().getDirectories());
        assertEquals(List.of("hooks"), resume.getHooks().getDirectories());
        assertEquals(List.of("agents"), createClone.getAgents().getDirectories());
        assertEquals(List.of("hooks"), createClone.getHooks().getDirectories());
        assertEquals(List.of("agents"), resumeClone.getAgents().getDirectories());
        assertEquals(List.of("hooks"), resumeClone.getHooks().getDirectories());
        assertThrows(UnsupportedOperationException.class,
                () -> createClone.getAgents().getDirectories().add("mutation"));
        assertThrows(UnsupportedOperationException.class,
                () -> resumeClone.getHooks().getDirectories().add("mutation"));
        assertThrows(UnsupportedOperationException.class, () -> create.getAgents().getDirectories().add("mutation"));
        assertThrows(UnsupportedOperationException.class, () -> resume.getHooks().getDirectories().add("mutation"));
    }

    // =========================================================================
    // buildCreateRequest
    // =========================================================================

    @Test
    void testBuildCreateRequestNullConfig() {
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(null, "test-session-id");
        assertNotNull(request);
        assertEquals("test-session-id", request.getSessionId());
        assertNull(request.getModel());
        assertTrue(request.getPermissions().getRequest(), "requestPermission should be true even for null config");
        assertEquals("direct", request.getMcp().getEnvValueMode(),
                "envValueMode should be 'direct' even for null config");
        assertNull(request.getSkills().getHasProvider());
    }

    @Test
    void testGitHubTokenProviderRegistrationWireFieldHasExactCasing() throws Exception {
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "create-session");
        create.setGitHub(create.getGitHub().setTokenProviderRegistrationId("create-registration"));
        var resume = SessionRequestBuilder.buildResumeRequest("resume-session", new ResumeSessionOptions());
        resume.setGitHub(resume.getGitHub().setTokenProviderRegistrationId("resume-registration"));
        var mapper = JsonRpcClient.getObjectMapper();

        assertEquals("create-registration", mapper.readTree(mapper.writeValueAsBytes(create)).path("github")
                .path("tokenProviderRegistrationId").asText());
        assertEquals("resume-registration", mapper.readTree(mapper.writeValueAsBytes(resume)).path("github")
                .path("tokenProviderRegistrationId").asText());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(create)).path("github").has("token"));
    }

    @Test
    void testGitHubTokenProviderResultRedactsToken() {
        var result = GitHubTokenProviderResult.token("do-not-print", 28_800);
        assertFalse(result.toString().contains("do-not-print"));
    }

    @Test
    void askUserVariantIsForwardedAndSerializedForCreateAndColdResume() throws Exception {
        var createRequest = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions()
                        .setInteraction(new SessionInteractionOptions().setAskUserVariant(AskUserVariant.ELICITATION)),
                "create-session");
        var resumeRequest = SessionRequestBuilder.buildResumeRequest("resume-session", new ResumeSessionOptions()
                .setInteraction(new SessionInteractionOptions().setAskUserVariant(AskUserVariant.LEGACY)));
        var mapper = JsonRpcClient.getObjectMapper();

        assertEquals(AskUserVariant.ELICITATION, createRequest.getInteraction().getAskUserVariant());
        assertEquals("elicitation", mapper.readTree(mapper.writeValueAsBytes(createRequest)).path("interaction")
                .path("askUserVariant").asText());
        assertEquals(AskUserVariant.LEGACY, resumeRequest.getInteraction().getAskUserVariant());
        assertEquals("legacy", mapper.readTree(mapper.writeValueAsBytes(resumeRequest)).path("interaction")
                .path("askUserVariant").asText());
    }

    @Test
    void askUserVariantDefaultsToOmittedLegacyBehavior() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var createRequest = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "create-session");
        var resumeRequest = SessionRequestBuilder.buildResumeRequest("resume-session", new ResumeSessionOptions());

        assertNull(createRequest.getInteraction().getAskUserVariant());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(createRequest)).path("interaction").has("askUserVariant"));
        assertNull(resumeRequest.getInteraction().getAskUserVariant());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(resumeRequest)).path("interaction").has("askUserVariant"));
    }

    @Test
    @AllowCopilotExperimental
    void skillProviderFlagIsForwardedAndSerializedForCreateAndResume() throws Exception {
        SkillProvider provider = testSkillProvider();
        var mapper = JsonRpcClient.getObjectMapper();

        var createRequest = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setSkills(new SessionSkillOptions().setProvider(provider)),
                "skill-provider-create");
        var resumeRequest = SessionRequestBuilder.buildResumeRequest("skill-provider-resume",
                new ResumeSessionOptions().setSkills(new SessionSkillOptions().setProvider(provider)));

        assertTrue(createRequest.getSkills().getHasProvider());
        assertTrue(resumeRequest.getSkills().getHasProvider());
        assertTrue(mapper.readTree(mapper.writeValueAsBytes(createRequest)).path("skills").path("hasProvider")
                .asBoolean());
        assertTrue(mapper.readTree(mapper.writeValueAsBytes(resumeRequest)).path("skills").path("hasProvider")
                .asBoolean());
    }

    @Test
    void skillProviderFlagIsOmittedWhenUnset() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var createRequest = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(),
                "skill-provider-create");
        var resumeRequest = SessionRequestBuilder.buildResumeRequest("skill-provider-resume",
                new ResumeSessionOptions());

        assertNull(createRequest.getSkills().getHasProvider());
        assertNull(resumeRequest.getSkills().getHasProvider());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(createRequest)).path("skills").has("hasProvider"));
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(resumeRequest)).path("skills").has("hasProvider"));
    }

    @Test
    void diagnosticsAreForwardedAndOmittedForCreateAndColdResume() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var createRequest = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setDiagnostics(diagnostics(DiagnosticLogLevel.DEBUG)).clone(),
                "diagnostics-create");
        var resumeRequest = SessionRequestBuilder.buildResumeRequest("diagnostics-resume",
                new ResumeSessionOptions().setDiagnostics(diagnostics(DiagnosticLogLevel.TRACE)).clone());
        var defaultCreateRequest = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(),
                "diagnostics-default-create");
        var defaultResumeRequest = SessionRequestBuilder.buildResumeRequest("diagnostics-default-resume",
                new ResumeSessionOptions());

        assertEquals(DiagnosticLogLevel.DEBUG, createRequest.getDiagnostics().sources().mcp().level());
        assertEquals("debug", mapper.readTree(mapper.writeValueAsBytes(createRequest)).path("diagnostics")
                .path("sources").path("mcp").path("level").asText());
        assertEquals(DiagnosticLogLevel.TRACE, resumeRequest.getDiagnostics().sources().mcp().level());
        assertEquals("trace", mapper.readTree(mapper.writeValueAsBytes(resumeRequest)).path("diagnostics")
                .path("sources").path("mcp").path("level").asText());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(defaultCreateRequest)).has("diagnostics"));
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(defaultResumeRequest)).has("diagnostics"));
    }

    private static DiagnosticsConfiguration diagnostics(DiagnosticLogLevel level) {
        return DiagnosticsConfiguration.builder().sources(DiagnosticSourcesConfiguration.builder()
                .mcp(McpDiagnosticSourceConfiguration.builder().level(level).build()).build()).build();
    }

    @Test
    void askUserVariantAcceptsOnlySupportedWireValues() {
        assertEquals(AskUserVariant.LEGACY, AskUserVariant.fromValue("legacy"));
        assertEquals(AskUserVariant.ELICITATION, AskUserVariant.fromValue("elicitation"));
        assertThrows(IllegalArgumentException.class, () -> AskUserVariant.fromValue("ELICITATION"));
        assertThrows(IllegalArgumentException.class, () -> AskUserVariant.fromValue("unsupported"));
    }

    @Test
    void testBuildCreateRequestHooksNonNullButEmpty() {
        // Hooks object exists but hasHooks() returns false
        var config = new CreateSessionOptions().setHooks(new SessionHookOptions().setHandlers(new SessionHooks()));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertNull(request.getHooks().getHasHandlers(), "Should be null when hooks are empty");
    }

    @Test
    void testBuildCreateRequestHooksWithHandler() {
        var hooks = new SessionHooks().setOnPreToolUse((input, inv) -> CompletableFuture.completedFuture(null));
        var config = new CreateSessionOptions().setHooks(new SessionHookOptions().setHandlers(hooks));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertTrue(request.getHooks().getHasHandlers(), "Should be true when hooks have handlers");
    }

    @Test
    void testBuildCreateRequestSetsEnvValueModeToDirect() {
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(),
                "test-session-id");
        assertEquals("direct", request.getMcp().getEnvValueMode());
    }

    @Test
    void testBuildCreateRequestAlwaysSetsRequestPermissionTrue() {
        // No permission handler set - requestPermission should still be true
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(),
                "test-session-id");
        assertTrue(request.getPermissions().getRequest(),
                "requestPermission should always be true to enable deny-by-default behavior");
    }

    @Test
    void testBuildRequestsResolveAndSerializeCustomAgentsLocalOnly() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();

        var explicitCreate = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setAgents(new SessionAgentOptions().setLocalDiscoveryOnly(false)),
                "create-explicit");
        var explicitResume = SessionRequestBuilder.buildResumeRequest("resume-explicit",
                new ResumeSessionOptions().setAgents(new SessionAgentOptions().setLocalDiscoveryOnly(false)));
        assertFalse(explicitCreate.getAgents().getLocalDiscoveryOnly());
        assertFalse(explicitResume.getAgents().getLocalDiscoveryOnly());
        assertTrue(mapper.writeValueAsString(explicitCreate).contains("\"localDiscoveryOnly\":false"));
        assertTrue(mapper.writeValueAsString(explicitResume).contains("\"localDiscoveryOnly\":false"));

        var emptyCreate = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "create-empty",
                CopilotClientMode.EMPTY);
        var emptyResume = SessionRequestBuilder.buildResumeRequest("resume-empty", new ResumeSessionOptions(),
                CopilotClientMode.EMPTY);
        assertTrue(emptyCreate.getAgents().getLocalDiscoveryOnly());
        assertTrue(emptyResume.getAgents().getLocalDiscoveryOnly());
        assertTrue(mapper.writeValueAsString(emptyCreate).contains("\"localDiscoveryOnly\":true"));
        assertTrue(mapper.writeValueAsString(emptyResume).contains("\"localDiscoveryOnly\":true"));

        var cliCreate = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "create-cli");
        var cliResume = SessionRequestBuilder.buildResumeRequest("resume-cli", new ResumeSessionOptions());
        assertNull(cliCreate.getAgents().getLocalDiscoveryOnly());
        assertNull(cliResume.getAgents().getLocalDiscoveryOnly());
        assertFalse(mapper.writeValueAsString(cliCreate).contains("\"customAgentsLocalOnly\""));
        assertFalse(mapper.writeValueAsString(cliResume).contains("\"customAgentsLocalOnly\""));
    }

    @Test
    void testBuildCreateRequestSetsClientName() {
        var config = new CreateSessionOptions().setClientName("my-app");
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals("my-app", request.getClientName());
    }

    @Test
    void testBuildCreateRequestSetsAdditionalDirectories() {
        var config = new CreateSessionOptions().setFilesystem(new CreateSessionFilesystemOptions()
                .setAdditionalDirectories(List.of("/repo/shared", "/repo/generated")));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals(List.of("/repo/shared", "/repo/generated"), request.getFilesystem().getAdditionalDirectories());
    }

    @Test
    void testBuildCreateRequestSetsReasoningSummary() {
        var config = new CreateSessionOptions()
                .setInference(new SessionInferenceOptions().setReasoningSummary("concise"));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals("concise", request.getInference().getReasoningSummary());
    }

    @Test
    void testBuildCreateRequestSetsEnableExperimentalMode() {
        var config = new CreateSessionOptions()
                .setExperiments(new SessionExperimentOptions().setEnableExperimentalMode(false));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertFalse(request.getExperiments().getEnableExperimentalMode());
    }

    @Test
    void testBuildCreateRequestOmitsEnableExperimentalModeWhenNotSet() {
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(),
                "test-session-id");
        assertNull(request.getExperiments().getEnableExperimentalMode());
    }

    @Test
    void testBuildCreateRequestDefaultsEnableExperimentalModeFalseInEmptyMode() {
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "sid-empty",
                CopilotClientMode.EMPTY);
        assertFalse(request.getExperiments().getEnableExperimentalMode());
    }

    @Test
    void testBuildCreateRequestSetsContextTier() {
        var config = new CreateSessionOptions()
                .setInference(new SessionInferenceOptions().setContextTier("long_context"));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals("long_context", request.getInference().getContextTier());
    }

    @Test
    void testBuildCreateRequestSetsPluginDirectoriesAndLargeOutput() throws Exception {
        var largeOutput = new LargeToolOutputConfig().setEnabled(true).setMaxSizeBytes(1024L)
                .setOutputDirectory("/tmp/out");
        var config = new CreateSessionOptions()
                .setPlugins(new SessionPluginOptions().setDirectories(List.of("/plugins/a")))
                .setMcp(new SessionMcpOptions().setDisabledServers(List.of("local-files", "remote-github")))
                .setTools(new SessionToolOptions().setLargeOutput(largeOutput));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals(List.of("/plugins/a"), request.getPlugins().getDirectories());
        assertEquals(List.of("local-files", "remote-github"), request.getMcp().getDisabledServers());
        assertEquals(largeOutput, request.getTools().getLargeOutput());
        assertTrue(JsonRpcClient.getObjectMapper().writeValueAsString(request)
                .contains("\"disabledServers\":[\"local-files\",\"remote-github\"]"));
    }

    @Test
    void testBuildRequestsForwardSandboxConfig() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (boolean enabled : new boolean[]{true, false}) {
            var sandbox = SandboxConfig.builder().enabled(enabled).userPolicy(null).addCurrentWorkingDirectory(false)
                    .sandboxMcpServers(true).sandboxLspServers(null).allowBypass(null).managedMcpRoutingLocked(null)
                    .managedLspRoutingLocked(null).auth(null).credentials(null).allowDevToolAccess(null).build();
            var create = SessionRequestBuilder.buildCreateRequest(
                    new CreateSessionOptions().setPermissions(new SessionPermissionOptions().setSandbox(sandbox)),
                    "sid-sandbox");
            var resume = SessionRequestBuilder.buildResumeRequest("sid-sandbox",
                    new ResumeSessionOptions().setPermissions(new SessionPermissionOptions().setSandbox(sandbox)));
            assertSame(sandbox, create.getPermissions().getSandbox());
            assertSame(sandbox, resume.getPermissions().getSandbox());
            for (var request : List.of(create, resume)) {
                var json = mapper.readTree(mapper.writeValueAsBytes(request));
                var config = json.path("permissions").get("sandbox");
                assertEquals(enabled, config.get("enabled").asBoolean());
                assertFalse(config.get("addCurrentWorkingDirectory").asBoolean());
                assertTrue(config.get("sandboxMcpServers").asBoolean());
            }
        }
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "sid-sandbox");
        var resume = SessionRequestBuilder.buildResumeRequest("sid-sandbox", new ResumeSessionOptions());
        assertNull(create.getPermissions().getSandbox());
        assertNull(resume.getPermissions().getSandbox());
        for (var request : List.of(create, resume)) {
            assertFalse(mapper.readTree(mapper.writeValueAsBytes(request)).path("permissions").has("sandbox"));
        }
    }

    @Test
    void testBuildCreateRequestSetsMemory() {
        var memory = new MemoryConfiguration().setEnabled(true);
        var config = new CreateSessionOptions().setState(new SessionStateOptions().setMemory(memory));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals(memory, request.getState().getMemory());
    }

    @Test
    void testBuildCreateRequestOmitsMemoryWhenNotSet() {
        var config = new CreateSessionOptions();
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertNull(request.getState().getMemory());
    }

    @Test
    void testBuildCreateRequestForwardsEnableSessionTelemetryWhenFalse() {
        var config = new CreateSessionOptions().setGitHub(new SessionGitHubOptions().setTelemetryEnabled(false));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertFalse(request.getGitHub().getTelemetryEnabled());
    }

    @Test
    void testBuildCreateRequestOmitsEnableSessionTelemetryWhenNotSet() {
        var config = new CreateSessionOptions();
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertNull(request.getGitHub().getTelemetryEnabled());
    }

    @Test
    void testBuildCreateRequestPassesThroughNullMcpOAuthTokenStorage() {
        var config = new CreateSessionOptions();
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertNull(request.getMcp().getOauthTokenStorage());
    }

    @Test
    void testBuildCreateRequestForwardsExplicitMcpOAuthTokenStorage() {
        var config = new CreateSessionOptions().setMcp(new SessionMcpOptions().setOauthTokenStorage("persistent"));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals("persistent", request.getMcp().getOauthTokenStorage());
    }

    @Test
    void testBuildCreateRequestForwardsSessionPolicyOptions() {
        var sessionLimits = SessionLimitsConfig.builder().maxAiCredits(30.0).build();
        var autopilot = AutopilotOptions.builder().continuationEnabled(false).maxContinues(0L).build();
        var config = new CreateSessionOptions()
                .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(List.of("explore")))
                .setInference(new SessionInferenceOptions().setEnableCitations(true))
                .setFilesystem(new CreateSessionFilesystemOptions().setTrackFileChanges(true))
                .setExecution(new SessionExecutionOptions().setLimits(sessionLimits))
                .setExecution(new CreateSessionOptions()
                        .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(List.of("explore")))
                        .setInference(new SessionInferenceOptions().setEnableCitations(true))
                        .setFilesystem(new CreateSessionFilesystemOptions().setTrackFileChanges(true))
                        .setExecution(new SessionExecutionOptions().setLimits(sessionLimits)).getExecution()
                        .setAutopilot(autopilot));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "session-policy");

        assertEquals(List.of("explore"), request.getAgents().getExcludedBuiltInAgents());
        assertTrue(request.getInference().getEnableCitations());
        assertTrue(request.getFilesystem().getTrackFileChanges());
        assertSame(sessionLimits, request.getExecution().getLimits());
        assertSame(autopilot, request.getExecution().getAutopilot());
    }

    @Test
    void testBuildCreateRequestNullConfigHasNullMcpOAuthTokenStorage() {
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(null, "test-session-id");
        assertNull(request.getMcp().getOauthTokenStorage());
    }

    // =========================================================================
    // buildResumeRequest
    // =========================================================================

    @Test
    void testBuildResumeRequestNullConfig() {
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-1", null);
        assertEquals("sid-1", request.getSessionId());
        assertNull(request.getModel());
        assertNull(request.getContinuePendingWork());
        assertTrue(request.getPermissions().getRequest(), "requestPermission should be true even for null config");
        assertEquals("direct", request.getMcp().getEnvValueMode(),
                "envValueMode should be 'direct' even for null config");
        assertNull(request.getSkills().getHasProvider());
    }

    @Test
    void continuePendingWorkIsForwardedAndSerializedOnResume() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (boolean enabled : new boolean[]{true, false}) {
            var config = new ResumeSessionOptions().setContinuePendingWork(enabled);
            assertEquals(enabled, config.getContinuePendingWork().orElseThrow());

            var request = SessionRequestBuilder.buildResumeRequest("sid-pending", config.clone());
            assertEquals(enabled, request.getContinuePendingWork());
            var serialized = mapper.readTree(mapper.writeValueAsBytes(request));
            assertTrue(serialized.has("continuePendingWork"));
            assertEquals(enabled, serialized.get("continuePendingWork").booleanValue());
        }
    }

    @Test
    void continuePendingWorkIsOmittedWhenUnsetOrCleared() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var config = new ResumeSessionOptions();
        assertTrue(config.getContinuePendingWork().isEmpty());
        var defaultRequest = SessionRequestBuilder.buildResumeRequest("sid-pending-default", config);
        assertNull(defaultRequest.getContinuePendingWork());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(defaultRequest)).has("continuePendingWork"));

        config.setContinuePendingWork(true).clearContinuePendingWork();
        var clearedRequest = SessionRequestBuilder.buildResumeRequest("sid-pending-cleared", config);
        assertTrue(config.getContinuePendingWork().isEmpty());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(clearedRequest)).has("continuePendingWork"));
    }

    @Test
    void nullableContinuePendingWorkPreservesExplicitValuesAndClearsDefault() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var config = new ResumeSessionOptions();
        for (Boolean enabled : new Boolean[]{true, false}) {
            assertSame(config, config.setContinuePendingWork(enabled));
            var request = SessionRequestBuilder.buildResumeRequest("sid-pending", config.clone());
            assertEquals(enabled, request.getContinuePendingWork());
            assertEquals(enabled,
                    mapper.readTree(mapper.writeValueAsBytes(request)).path("continuePendingWork").booleanValue());
        }

        assertSame(config, config.setContinuePendingWork((Boolean) null));
        assertTrue(config.getContinuePendingWork().isEmpty());
        var request = SessionRequestBuilder.buildResumeRequest("sid-pending-default", config.clone());
        assertNull(request.getContinuePendingWork());
        assertFalse(mapper.readTree(mapper.writeValueAsBytes(request)).has("continuePendingWork"));
    }

    @Test
    void continuePendingWorkIsOmittedWhenRequestIsCleared() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (boolean enabled : new boolean[]{true, false}) {
            var request = new ResumeSessionRequest();
            request.setContinuePendingWork(enabled);
            assertEquals(enabled, request.getContinuePendingWork());

            request.clearContinuePendingWork();
            assertNull(request.getContinuePendingWork());
            assertFalse(mapper.readTree(mapper.writeValueAsBytes(request)).has("continuePendingWork"));
        }
    }

    @Test
    void testBuildResumeRequestForwardsEnableSessionTelemetryWhenFalse() {
        var config = new ResumeSessionOptions().setGitHub(new SessionGitHubOptions().setTelemetryEnabled(false));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-1", config);
        assertFalse(request.getGitHub().getTelemetryEnabled());
    }

    @Test
    void testBuildResumeRequestOmitsEnableSessionTelemetryWhenNotSet() {
        var config = new ResumeSessionOptions();
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-1", config);
        assertNull(request.getGitHub().getTelemetryEnabled());
    }

    @Test
    void testBuildResumeRequestSetsEnableExperimentalMode() {
        var config = new ResumeSessionOptions()
                .setExperiments(new SessionExperimentOptions().setEnableExperimentalMode(true));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-1", config);
        assertTrue(request.getExperiments().getEnableExperimentalMode());
    }

    @Test
    void testBuildResumeRequestOmitsEnableExperimentalModeWhenNotSet() {
        var config = new ResumeSessionOptions();
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-1", config);
        assertNull(request.getExperiments().getEnableExperimentalMode());
    }

    @Test
    void testBuildResumeRequestDefaultsEnableExperimentalModeFalseInEmptyMode() {
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-empty", new ResumeSessionOptions(),
                CopilotClientMode.EMPTY);
        assertFalse(request.getExperiments().getEnableExperimentalMode());
    }

    @Test
    void testBuildSessionRequestsWithTools() throws Exception {
        var tool = ToolDefinition.create("my_tool", "A tool", Map.of("type", "object"),
                inv -> CompletableFuture.completedFuture("result"));
        var deferred = ToolDefinition.create("approval", "A deferred tool", Map.of("type", "object"), null);
        var tools = List.of(tool, deferred);
        var mapper = JsonRpcClient.getObjectMapper();
        for (var mode : new CopilotClientMode[]{CopilotClientMode.CODING_AGENT, CopilotClientMode.EMPTY}) {
            for (var request : new Object[]{SessionRequestBuilder.buildCreateRequest(
                    new CreateSessionOptions().setTools(new SessionToolOptions().setDefinitions(tools)), "sid-1", mode),
                    SessionRequestBuilder.buildResumeRequest("sid-2",
                            new ResumeSessionOptions().setTools(new SessionToolOptions().setDefinitions(tools)),
                            mode)}) {
                var json = mapper.readTree(mapper.writeValueAsBytes(request));
                assertEquals(2, json.path("tools").path("definitions").size());
                assertEquals("my_tool", json.at("/tools/definitions/0/name").textValue());
                assertEquals(mapper.getNodeFactory().booleanNode(true), json.at("/tools/definitions/0/hasHandler"));
                assertEquals(mapper.getNodeFactory().booleanNode(false), json.at("/tools/definitions/1/hasHandler"));
                assertFalse(json.at("/tools/definitions/0").has("handler"));
            }
        }
    }

    @Test
    void testBuildResumeRequestWithUserInputHandler() {
        var config = new ResumeSessionOptions().setInteraction(new SessionInteractionOptions()
                .setOnUserInputRequest((req, inv) -> CompletableFuture.completedFuture(new UserInputResponse())));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-3", config);

        assertTrue(request.getInteraction().getRequestUserInput());
    }

    @Test
    void testBuildResumeRequestHooksNonNullButEmpty() {
        var config = new ResumeSessionOptions().setHooks(new SessionHookOptions().setHandlers(new SessionHooks()));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-4", config);

        assertNull(request.getHooks().getHasHandlers(), "Should be null when hooks are empty");
    }

    @Test
    void testBuildResumeRequestHooksWithHandler() {
        var hooks = new SessionHooks().setOnSessionEnd((input, inv) -> CompletableFuture.completedFuture(null));
        var config = new ResumeSessionOptions().setHooks(new SessionHookOptions().setHandlers(hooks));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-5", config);

        assertTrue(request.getHooks().getHasHandlers(), "Should be true when hooks have handlers");
    }

    @Test
    void testBuildResumeRequestDisableResume() {
        var config = new ResumeSessionOptions().setSuppressResumeEvent(true);

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-6", config);

        assertTrue(request.getSuppressResumeEvent());
    }

    @Test
    void testBuildResumeRequestStreaming() {
        var config = new ResumeSessionOptions().setEvents(new SessionEventOptions().setStreaming(true));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-7", config);

        assertTrue(request.getEvents().getStreaming());
    }

    @Test
    void testBuildResumeRequestSetsEnvValueModeToDirect() {
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-8", new ResumeSessionOptions());
        assertEquals("direct", request.getMcp().getEnvValueMode());
    }

    @Test
    void testBuildResumeRequestAlwaysSetsRequestPermissionTrue() {
        // No permission handler set - requestPermission should still be true
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-9", new ResumeSessionOptions());
        assertTrue(request.getPermissions().getRequest(),
                "requestPermission should always be true to enable deny-by-default behavior");
    }

    @Test
    void testBuildResumeRequestSetsClientName() {
        var config = new ResumeSessionOptions().setClientName("my-app");
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-10", config);
        assertEquals("my-app", request.getClientName());
    }

    @Test
    void testBuildResumeRequestSetsAdditionalDirectories() {
        var config = new ResumeSessionOptions()
                .setFilesystem(new SessionFilesystemOptions().setAdditionalDirectories(List.of("/repo/resumed")));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-additional-directories", config);
        assertEquals(List.of("/repo/resumed"), request.getFilesystem().getAdditionalDirectories());
    }

    @Test
    void testBuildCreateRequestPropagatesGranularMultitenancyFields() {
        var config = new CreateSessionOptions()
                .setState(new SessionStateOptions().setSkipEmbeddingRetrieval(true).setEnableCrossSessionStore(true))
                .setInstructions(new CreateSessionInstructionOptions().setOrganizationText("Create org instructions")
                        .setOnDemandDiscovery(false))
                .setHooks(new SessionHookOptions().setEnableRepositoryHooks(true))
                .setGit(new SessionGitOptions().setEnableHostGitOperations(false))
                .setSkills(new SessionSkillOptions().setEnabled(false));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertTrue(request.getState().getSkipEmbeddingRetrieval());
        assertEquals("Create org instructions", request.getInstructions().getOrganizationText());
        assertFalse(request.getInstructions().getOnDemandDiscovery());
        assertTrue(request.getHooks().getEnableRepositoryHooks());
        assertFalse(request.getGit().getEnableHostGitOperations());
        assertTrue(request.getState().getEnableCrossSessionStore());
        assertFalse(request.getSkills().getEnabled());
    }

    @Test
    void testBuildResumeRequestPropagatesGranularMultitenancyFields() {
        var config = new ResumeSessionOptions()
                .setState(new SessionStateOptions().setSkipEmbeddingRetrieval(false).setEnableCrossSessionStore(false))
                .setInstructions(new SessionInstructionOptions().setOrganizationText("Resume org instructions")
                        .setOnDemandDiscovery(true))
                .setHooks(new SessionHookOptions().setEnableRepositoryHooks(true))
                .setGit(new SessionGitOptions().setEnableHostGitOperations(true))
                .setSkills(new SessionSkillOptions().setEnabled(true));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-11", config);

        assertFalse(request.getState().getSkipEmbeddingRetrieval());
        assertEquals("Resume org instructions", request.getInstructions().getOrganizationText());
        assertTrue(request.getInstructions().getOnDemandDiscovery());
        assertTrue(request.getHooks().getEnableRepositoryHooks());
        assertTrue(request.getGit().getEnableHostGitOperations());
        assertFalse(request.getState().getEnableCrossSessionStore());
        assertTrue(request.getSkills().getEnabled());
    }

    @Test
    void testBuildResumeRequestPassesThroughNullMcpOAuthTokenStorage() {
        var config = new ResumeSessionOptions();
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-12", config);
        assertNull(request.getMcp().getOauthTokenStorage());
    }

    @Test
    void testBuildResumeRequestForwardsExplicitMcpOAuthTokenStorage() {
        var config = new ResumeSessionOptions().setMcp(new SessionMcpOptions().setOauthTokenStorage("persistent"));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-13", config);
        assertEquals("persistent", request.getMcp().getOauthTokenStorage());
    }

    @Test
    void testBuildResumeRequestForwardsSessionPolicyOptions() {
        var sessionLimits = SessionLimitsConfig.builder().maxAiCredits(30.0).build();
        var autopilot = AutopilotOptions.builder().continuationEnabled(null).maxContinues(2L).build();
        var config = new ResumeSessionOptions()
                .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(List.of("explore")))
                .setInference(new SessionInferenceOptions().setEnableCitations(true))
                .setFilesystem(new SessionFilesystemOptions().setTrackFileChanges(true))
                .setExecution(new SessionExecutionOptions().setLimits(sessionLimits))
                .setExecution(new ResumeSessionOptions()
                        .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(List.of("explore")))
                        .setInference(new SessionInferenceOptions().setEnableCitations(true))
                        .setFilesystem(new SessionFilesystemOptions().setTrackFileChanges(true))
                        .setExecution(new SessionExecutionOptions().setLimits(sessionLimits)).getExecution()
                        .setAutopilot(autopilot));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-policy", config);

        assertEquals(List.of("explore"), request.getAgents().getExcludedBuiltInAgents());
        assertTrue(request.getInference().getEnableCitations());
        assertTrue(request.getFilesystem().getTrackFileChanges());
        assertSame(sessionLimits, request.getExecution().getLimits());
        assertSame(autopilot, request.getExecution().getAutopilot());
    }

    @Test
    void testBuildResumeRequestNullConfigHasNullMcpOAuthTokenStorage() {
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-14", null);
        assertNull(request.getMcp().getOauthTokenStorage());
    }

    @Test
    void testBuildResumeRequestSetsReasoningSummary() {
        var config = new ResumeSessionOptions().setInference(new SessionInferenceOptions().setReasoningSummary("none"));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-15", config);
        assertEquals("none", request.getInference().getReasoningSummary());
    }

    @Test
    void testBuildResumeRequestSetsContextTier() {
        var config = new ResumeSessionOptions().setInference(new SessionInferenceOptions().setContextTier("default"));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-15", config);
        assertEquals("default", request.getInference().getContextTier());
    }

    @Test
    void testBuildResumeRequestSetsPluginDirectoriesAndLargeOutput() throws Exception {
        var largeOutput = new LargeToolOutputConfig().setEnabled(false).setMaxSizeBytes(2048L)
                .setOutputDirectory("/tmp/resume");
        var config = new ResumeSessionOptions()
                .setPlugins(new SessionPluginOptions().setDirectories(List.of("/plugins/r")))
                .setMcp(new SessionMcpOptions().setDisabledServers(List.of("local-files-r")))
                .setTools(new SessionToolOptions().setLargeOutput(largeOutput));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-16", config);
        assertEquals(List.of("/plugins/r"), request.getPlugins().getDirectories());
        assertEquals(List.of("local-files-r"), request.getMcp().getDisabledServers());
        assertEquals(largeOutput, request.getTools().getLargeOutput());
        assertTrue(JsonRpcClient.getObjectMapper().writeValueAsString(request)
                .contains("\"disabledServers\":[\"local-files-r\"]"));
    }

    @Test
    void testBuildResumeRequestSetsMemory() {
        var memory = new MemoryConfiguration().setEnabled(false);
        var config = new ResumeSessionOptions().setState(new SessionStateOptions().setMemory(memory));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-mem", config);
        assertEquals(memory, request.getState().getMemory());
    }

    @Test
    void testBuildResumeRequestOmitsMemoryWhenNotSet() {
        var config = new ResumeSessionOptions();
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-mem", config);
        assertNull(request.getState().getMemory());
    }

    // =========================================================================
    // configureSession (ResumeSessionOptions overload)
    // =========================================================================

    @Test
    void testConfigureResumeSessionNullConfig() throws Exception {
        var session = createTestSession();
        // Should not throw
        SessionRequestBuilder.configureSession(session, (ResumeSessionOptions) null);
    }

    @Test
    void testConfigureResumeSessionWithTools() throws Exception {
        var session = createTestSession();
        var tool = ToolDefinition.create("resume_tool", "desc", Map.of(),
                inv -> CompletableFuture.completedFuture("ok"));
        var config = new ResumeSessionOptions().setTools(new SessionToolOptions().setDefinitions(List.of(tool)));

        SessionRequestBuilder.configureSession(session, config);

        assertNotNull(session.getTool("resume_tool"));
    }

    @Test
    void testConfigureResumeSessionWithUserInputHandler() throws Exception {
        var session = createTestSession();
        var config = new ResumeSessionOptions().setInteraction(new SessionInteractionOptions()
                .setOnUserInputRequest((req, inv) -> CompletableFuture.completedFuture(new UserInputResponse())));

        SessionRequestBuilder.configureSession(session, config);

        // Handler was registered — verify by calling handleUserInputRequest
        // (package-private)
        var response = session.handleUserInputRequest(new com.microsoft.copilot.rpc.UserInputRequest()).get();
        assertNotNull(response);
    }

    @Test
    void testConfigureResumeSessionWithHooks() throws Exception {
        var session = createTestSession();
        var hooks = new SessionHooks().setOnPreToolUse((input, inv) -> CompletableFuture.completedFuture(null));
        var config = new ResumeSessionOptions().setHooks(new SessionHookOptions().setHandlers(hooks));

        SessionRequestBuilder.configureSession(session, config);

        // Hooks registered — handleHooksInvoke should dispatch preToolUse
        var mapper = JsonRpcClient.getObjectMapper();
        var input = mapper.valueToTree(Map.of("toolName", "test_tool"));
        var result = session.handleHooksInvoke("preToolUse", input).get();
        assertNull(result); // handler returns null
    }

    // =========================================================================
    // Helper
    // =========================================================================

    private CopilotSession createTestSession() throws Exception {
        var constructor = CopilotSession.class.getDeclaredConstructor(String.class, JsonRpcClient.class, String.class);
        constructor.setAccessible(true);
        return constructor.newInstance("builder-test-session", null, null);
    }

    @Test
    void testBuildCreateRequestWithAgent() {
        var config = new CreateSessionOptions().setAgents(new SessionAgentOptions().setSelected("my-agent"));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        assertEquals("my-agent", request.getAgents().getSelected());
    }

    @Test
    void testBuildResumeRequestWithAgent() {
        var config = new ResumeSessionOptions().setAgents(new SessionAgentOptions().setSelected("my-agent"));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("session-id", config);
        assertEquals("my-agent", request.getAgents().getSelected());
    }

    // =========================================================================
    // extractTransformCallbacks
    // =========================================================================

    @Test
    void extractTransformCallbacks_nullSystemMessage_returnsNull() {
        ExtractedTransforms result = SessionRequestBuilder.extractTransformCallbacks(null);
        assertNull(result.wireSystemMessage());
        assertNull(result.transformCallbacks());
    }

    @Test
    void extractTransformCallbacks_appendMode_returnsOriginalConfig() {
        var config = new com.microsoft.copilot.rpc.SystemMessageConfig()
                .setMode(com.microsoft.copilot.SystemMessageMode.APPEND).setContent("extra content");
        ExtractedTransforms result = SessionRequestBuilder.extractTransformCallbacks(config);
        assertSame(config, result.wireSystemMessage());
        assertNull(result.transformCallbacks());
    }

    @Test
    void extractTransformCallbacks_customizeModeNoTransforms_returnsOriginalConfig() {
        var sections = Map.of("tone", new com.microsoft.copilot.rpc.SectionOverride()
                .setAction(com.microsoft.copilot.rpc.SectionOverrideAction.REMOVE));
        var config = new com.microsoft.copilot.rpc.SystemMessageConfig()
                .setMode(com.microsoft.copilot.SystemMessageMode.CUSTOMIZE).setSections(sections);
        ExtractedTransforms result = SessionRequestBuilder.extractTransformCallbacks(config);
        assertSame(config, result.wireSystemMessage());
        assertNull(result.transformCallbacks());
    }

    @Test
    void extractTransformCallbacks_customizeModeWithTransform_extractsCallbacks() {
        var transformFn = (java.util.function.Function<String, CompletableFuture<String>>) content -> CompletableFuture
                .completedFuture(content + " modified");
        var sections = Map.of("identity", new com.microsoft.copilot.rpc.SectionOverride().setTransform(transformFn));
        var config = new com.microsoft.copilot.rpc.SystemMessageConfig()
                .setMode(com.microsoft.copilot.SystemMessageMode.CUSTOMIZE).setSections(sections);

        ExtractedTransforms result = SessionRequestBuilder.extractTransformCallbacks(config);

        // Wire config should be different from original
        assertNotSame(config, result.wireSystemMessage());
        // Callbacks should be extracted
        assertNotNull(result.transformCallbacks());
        assertTrue(result.transformCallbacks().containsKey("identity"));
        // Wire config should have transform action instead of callback
        assertNotNull(result.wireSystemMessage().getSections());
        var wireSection = result.wireSystemMessage().getSections().get("identity");
        assertNotNull(wireSection);
        assertEquals(com.microsoft.copilot.rpc.SectionOverrideAction.TRANSFORM, wireSection.getAction());
        assertNull(wireSection.getTransform());
    }

    @Test
    void buildCreateRequestWithSessionId_usesProvidedSessionId() {
        var config = new CreateSessionOptions();
        config.setSessionId("config-session-id");

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "my-session-id");

        assertEquals("my-session-id", request.getSessionId());
    }

    @Test
    void configureSessionWithNullConfig_returnsEarly() {
        // configureSession with null config should return without error
        CopilotSession session = new CopilotSession("session-1", null);
        // Covers the null config early-return branch (L219-220)
        assertDoesNotThrow(() -> SessionRequestBuilder.configureSession(session, (CreateSessionOptions) null));
    }

    @Test
    @AllowCopilotExperimental
    void configureSessionWithSkillProvider_registersProvider() {
        CopilotSession session = new CopilotSession("session-1", null);
        SkillProvider provider = testSkillProvider();

        SessionRequestBuilder.configureSession(session,
                new CreateSessionOptions().setSkills(new SessionSkillOptions().setProvider(provider)));

        assertSame(provider, session.getSkillProvider());
    }

    @Test
    void configureSessionWithCommands_registersCommands() {
        CopilotSession session = new CopilotSession("session-1", null);

        var cmd = new com.microsoft.copilot.rpc.CommandDefinition().setName("deploy")
                .setHandler(ctx -> CompletableFuture.completedFuture(null));
        var config = new CreateSessionOptions()
                .setInteraction(new SessionInteractionOptions().setCommands(List.of(cmd)));

        // Covers config.getCommands() != null branch (L235-236)
        SessionRequestBuilder.configureSession(session, config);
        // If no exception thrown, the branch was covered
    }

    @Test
    void configureSessionWithElicitationHandler_registersHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        ElicitationHandler handler = (context) -> CompletableFuture
                .completedFuture(new ElicitationResult().setAction(ElicitationResultAction.CANCEL));
        var config = new CreateSessionOptions()
                .setInteraction(new SessionInteractionOptions().setOnElicitationRequest(handler));

        // Covers config.getOnElicitationRequest() != null branch (L238-239)
        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureSessionWithOnEvent_registersEventHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        var config = new CreateSessionOptions().setEvents(new SessionEventOptions().setOnEvent(event -> {
        }));

        // Covers config.getOnEvent() != null branch (L241-242)
        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureResumedSessionWithCommands_registersCommands() {
        CopilotSession session = new CopilotSession("session-1", null);

        var cmd = new com.microsoft.copilot.rpc.CommandDefinition().setName("rollback")
                .setHandler(ctx -> CompletableFuture.completedFuture(null));
        var config = new ResumeSessionOptions()
                .setInteraction(new SessionInteractionOptions().setCommands(List.of(cmd)));

        // Covers ResumeSessionOptions.getCommands() != null branch (L271-272)
        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureResumedSessionWithElicitationHandler_registersHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        ElicitationHandler handler = (context) -> CompletableFuture
                .completedFuture(new ElicitationResult().setAction(ElicitationResultAction.CANCEL));
        var config = new ResumeSessionOptions()
                .setInteraction(new SessionInteractionOptions().setOnElicitationRequest(handler));

        // Covers ResumeSessionOptions.getOnElicitationRequest() != null branch
        // (L274-275)
        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureResumedSessionWithOnEvent_registersEventHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        var config = new ResumeSessionOptions().setEvents(new SessionEventOptions().setOnEvent(event -> {
        }));

        // Covers ResumeSessionOptions.getOnEvent() != null branch (L277-278)
        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    @AllowCopilotExperimental
    void configureResumedSessionWithSkillProvider_registersProvider() {
        CopilotSession session = new CopilotSession("session-1", null);
        SkillProvider provider = testSkillProvider();

        SessionRequestBuilder.configureSession(session,
                new ResumeSessionOptions().setSkills(new SessionSkillOptions().setProvider(provider)));

        assertSame(provider, session.getSkillProvider());
    }

    @Test
    void testBuildCreateRequestWithDefaultAgent() {
        var defaultAgent = new DefaultAgentConfig().setExcludedTools(List.of("secret_tool"));
        var config = new CreateSessionOptions().setAgents(new SessionAgentOptions().setDefault(defaultAgent));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertNotNull(request.getAgents().getDefault());
        assertEquals(List.of("secret_tool"), request.getAgents().getDefault().getExcludedTools());
    }

    @Test
    void testBuildCreateRequestWithGitHubToken() {
        var config = new CreateSessionOptions().setGitHub(new SessionGitHubOptions().setToken("ghp_per_session_token"));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertEquals("ghp_per_session_token", request.getGitHub().getToken());
    }

    @Test
    void testBuildResumeRequestWithDefaultAgent() {
        var defaultAgent = new DefaultAgentConfig().setExcludedTools(List.of("secret_tool"));
        var config = new ResumeSessionOptions().setAgents(new SessionAgentOptions().setDefault(defaultAgent));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("test-session", config);

        assertNotNull(request.getAgents().getDefault());
        assertEquals(List.of("secret_tool"), request.getAgents().getDefault().getExcludedTools());
    }

    @Test
    void testBuildResumeRequestWithGitHubToken() {
        var config = new ResumeSessionOptions().setGitHub(new SessionGitHubOptions().setToken("ghp_per_session_token"));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("test-session", config);

        assertEquals("ghp_per_session_token", request.getGitHub().getToken());
    }

    // =========================================================================
    // instructionDirectories propagation
    // =========================================================================

    @Test
    void testBuildCreateRequestPropagatesInstructionDirectories() {
        var dirs = List.of("/path/to/instructions", "/another/path");
        var config = new CreateSessionOptions()
                .setInstructions(new CreateSessionInstructionOptions().setDirectories(dirs));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertEquals(dirs, request.getInstructions().getDirectories());
    }

    @Test
    void testBuildResumeRequestPropagatesInstructionDirectories() {
        var dirs = List.of("/resume/instructions", "/other/dir");
        var config = new ResumeSessionOptions().setInstructions(new SessionInstructionOptions().setDirectories(dirs));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-inst", config);

        assertEquals(dirs, request.getInstructions().getDirectories());
    }

    // =========================================================================
    // enableSessionTelemetry serialization
    // =========================================================================

    @Test
    void testCreateRequestSerializesEnableSessionTelemetryWhenFalse() throws Exception {
        var config = new CreateSessionOptions().setGitHub(new SessionGitHubOptions().setTelemetryEnabled(false));
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);
        assertTrue(json.contains("\"telemetryEnabled\":false"),
                "enableSessionTelemetry should be serialized when set to false");
    }

    @Test
    void testCreateRequestOmitsEnableSessionTelemetryWhenNull() throws Exception {
        var config = new CreateSessionOptions();
        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);
        assertFalse(json.contains("enableSessionTelemetry"), "enableSessionTelemetry should be omitted when null");
    }

    @Test
    void testResumeRequestSerializesEnableSessionTelemetryWhenFalse() throws Exception {
        var config = new ResumeSessionOptions().setGitHub(new SessionGitHubOptions().setTelemetryEnabled(false));
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-tel", config);
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);
        assertTrue(json.contains("\"telemetryEnabled\":false"),
                "enableSessionTelemetry should be serialized when set to false");
    }

    @Test
    void testResumeRequestOmitsEnableSessionTelemetryWhenNull() throws Exception {
        var config = new ResumeSessionOptions();
        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("sid-tel", config);
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);
        assertFalse(json.contains("enableSessionTelemetry"), "enableSessionTelemetry should be omitted when null");
    }

    // =========================================================================
    // Mode handler request flags
    // =========================================================================

    @Test
    void testBuildCreateRequestWithExitPlanModeHandler() {
        var config = new CreateSessionOptions().setInteraction(new SessionInteractionOptions().setOnExitPlanModeRequest(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult())));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertTrue(request.getInteraction().getRequestExitPlanMode());
    }

    @Test
    void testBuildCreateRequestWithAutoModeSwitchHandler() {
        var config = new CreateSessionOptions()
                .setInteraction(new SessionInteractionOptions().setOnAutoModeSwitchRequest(
                        (request, invocation) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertTrue(request.getInteraction().getRequestAutoModeSwitch());
    }

    @Test
    void testBuildCreateRequestWithoutModeHandlers() {
        var config = new CreateSessionOptions();

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertNull(request.getInteraction().getRequestExitPlanMode());
        assertNull(request.getInteraction().getRequestAutoModeSwitch());
    }

    @Test
    void testBuildResumeRequestWithExitPlanModeHandler() {
        var config = new ResumeSessionOptions().setInteraction(new SessionInteractionOptions().setOnExitPlanModeRequest(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult())));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("session-1", config);

        assertTrue(request.getInteraction().getRequestExitPlanMode());
    }

    @Test
    void testBuildResumeRequestWithAutoModeSwitchHandler() {
        var config = new ResumeSessionOptions()
                .setInteraction(new SessionInteractionOptions().setOnAutoModeSwitchRequest(
                        (request, invocation) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("session-1", config);

        assertTrue(request.getInteraction().getRequestAutoModeSwitch());
    }

    @Test
    void configureSessionWithExitPlanModeHandler_registersHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        var config = new CreateSessionOptions().setInteraction(new SessionInteractionOptions().setOnExitPlanModeRequest(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult())));

        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureSessionWithAutoModeSwitchHandler_registersHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        var config = new CreateSessionOptions()
                .setInteraction(new SessionInteractionOptions().setOnAutoModeSwitchRequest(
                        (request, invocation) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureResumedSessionWithExitPlanModeHandler_registersHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        var config = new ResumeSessionOptions().setInteraction(new SessionInteractionOptions().setOnExitPlanModeRequest(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult())));

        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void configureResumedSessionWithAutoModeSwitchHandler_registersHandler() {
        CopilotSession session = new CopilotSession("session-1", null);

        var config = new ResumeSessionOptions()
                .setInteraction(new SessionInteractionOptions().setOnAutoModeSwitchRequest(
                        (request, invocation) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        SessionRequestBuilder.configureSession(session, config);
    }

    @Test
    void testCreateRequestSerializesModeFlags() throws Exception {
        var config = new CreateSessionOptions().setInteraction(new SessionInteractionOptions()
                .setOnExitPlanModeRequest((r, i) -> CompletableFuture.completedFuture(new ExitPlanModeResult()))
                .setOnAutoModeSwitchRequest((r, i) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);

        assertTrue(json.contains("\"requestExitPlanMode\":true"));
        assertTrue(json.contains("\"requestAutoModeSwitch\":true"));
    }

    @Test
    void testResumeRequestSerializesModeFlags() throws Exception {
        var config = new ResumeSessionOptions().setInteraction(new SessionInteractionOptions()
                .setOnExitPlanModeRequest((r, i) -> CompletableFuture.completedFuture(new ExitPlanModeResult()))
                .setOnAutoModeSwitchRequest((r, i) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        ResumeSessionRequest request = SessionRequestBuilder.buildResumeRequest("session-1", config);
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);

        assertTrue(json.contains("\"requestExitPlanMode\":true"));
        assertTrue(json.contains("\"requestAutoModeSwitch\":true"));
    }

    // =========================================================================
    // Cloud session options wiring
    // =========================================================================

    @Test
    void testBuildCreateRequestPropagatesCloudSessionOptions() throws Exception {
        var cloud = new CloudSessionOptions()
                .setRepository(new CloudSessionRepository().setOwner("my-org").setName("my-repo").setBranch("main"));
        var config = new CreateSessionOptions().setCloud(cloud);

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");

        assertNotNull(request.getCloud());
        assertEquals("my-org", request.getCloud().getRepository().getOwner());
        assertEquals("my-repo", request.getCloud().getRepository().getName());
        assertEquals("main", request.getCloud().getRepository().getBranch());
    }

    @Test
    void testBuildCreateRequestOmitsCloudWhenNull() throws Exception {
        var config = new CreateSessionOptions();

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);

        assertNull(request.getCloud());
        assertFalse(json.contains("\"cloud\""), "cloud should be omitted when null");
    }

    @Test
    void testCloudSessionOptionsSerializesCorrectly() throws Exception {
        var cloud = new CloudSessionOptions()
                .setRepository(new CloudSessionRepository().setOwner("acme").setName("widgets").setBranch("feature-1"));
        var config = new CreateSessionOptions().setCloud(cloud);

        CreateSessionRequest request = SessionRequestBuilder.buildCreateRequest(config, "test-session-id");
        var mapper = JsonRpcClient.getObjectMapper();
        var json = mapper.writeValueAsString(request);

        assertTrue(json.contains("\"cloud\""));
        assertTrue(json.contains("\"owner\":\"acme\""));
        assertTrue(json.contains("\"name\":\"widgets\""));
        assertTrue(json.contains("\"branch\":\"feature-1\""));
    }

    // =========================================================================
    // ExP assignment injection wiring
    // =========================================================================

    @Test
    void testBuildRequestsPropagateAndSerializeExpAssignments() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var createAssignments = new CopilotExpAssignmentResponse()
                .setConfigs(List.of(new ExpConfigEntry().setId("exp-create")));
        var resumeAssignments = new CopilotExpAssignmentResponse()
                .setConfigs(List.of(new ExpConfigEntry().setId("exp-resume")));

        var createConfig = new CreateSessionOptions()
                .setExperiments(new SessionExperimentOptions().setAssignments(createAssignments));
        CreateSessionRequest createRequest = SessionRequestBuilder.buildCreateRequest(createConfig, "session-1");
        assertEquals(createAssignments, createRequest.getExperiments().getAssignments());
        var createJson = mapper.writeValueAsString(createRequest);
        assertTrue(createJson.contains("\"assignments\""));
        assertTrue(createJson.contains("\"Id\":\"exp-create\""));

        var resumeConfig = new ResumeSessionOptions()
                .setExperiments(new SessionExperimentOptions().setAssignments(resumeAssignments));
        ResumeSessionRequest resumeRequest = SessionRequestBuilder.buildResumeRequest("session-1", resumeConfig);
        assertEquals(resumeAssignments, resumeRequest.getExperiments().getAssignments());
        var resumeJson = mapper.writeValueAsString(resumeRequest);
        assertTrue(resumeJson.contains("\"assignments\""));
        assertTrue(resumeJson.contains("\"Id\":\"exp-resume\""));
    }

    @Test
    void testBuiltInAgentAllowlistPreservesOmissionEmptySubsetAndClone() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (var mode : CopilotClientMode.values()) {
            for (List<String> agents : java.util.Arrays.asList(null, List.<String>of(), List.of("explore"))) {
                var original = agents == null ? null : new ArrayList<>(agents);
                var create = new CreateSessionOptions()
                        .setAgents(new SessionAgentOptions().setIncludedBuiltInAgents(original));
                var resume = new ResumeSessionOptions()
                        .setAgents(new SessionAgentOptions().setIncludedBuiltInAgents(original));
                if (original != null) {
                    original.add("task");
                }
                var createClone = create.clone();
                var resumeClone = resume.clone();
                create.setAgents(create.getAgents().setIncludedBuiltInAgents(List.of("task")));
                resume.setAgents(resume.getAgents().setIncludedBuiltInAgents(List.of("task")));
                var createJson = mapper.valueToTree(SessionRequestBuilder.buildCreateRequest(createClone, "s1", mode));
                var resumeJson = mapper.valueToTree(SessionRequestBuilder.buildResumeRequest("s1", resumeClone, mode));
                for (var payload : List.of(createJson, resumeJson)) {
                    if (agents == null) {
                        assertFalse(payload.path("agents").has("includedBuiltinAgents"), payload.toString());
                    } else {
                        assertEquals(mapper.valueToTree(agents), payload.path("agents").get("includedBuiltinAgents"));
                    }
                }
            }
            var createNull = mapper.valueToTree(SessionRequestBuilder.buildCreateRequest(null, "s1", mode));
            var resumeNull = mapper.valueToTree(SessionRequestBuilder.buildResumeRequest("s1", null, mode));
            assertFalse(createNull.path("agents").has("includedBuiltinAgents"));
            assertFalse(resumeNull.path("agents").has("includedBuiltinAgents"));
        }
    }

    @Test
    void testBuildRequestsOmitExpAssignmentsWhenUnset() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();

        CreateSessionRequest createRequest = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(),
                "session-1");
        assertNull(createRequest.getExperiments().getAssignments());
        var createJson = mapper.writeValueAsString(createRequest);
        assertFalse(createJson.contains("\"assignments\""), "expAssignments should be omitted when null");

        ResumeSessionRequest resumeRequest = SessionRequestBuilder.buildResumeRequest("session-1",
                new ResumeSessionOptions());
        assertNull(resumeRequest.getExperiments().getAssignments());
        var resumeJson = mapper.writeValueAsString(resumeRequest);
        assertFalse(resumeJson.contains("\"assignments\""), "expAssignments should be omitted when null");
    }

    @Test
    void testBuildRequestsPropagateAndSerializeFeatureFlags() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var flags = Map.of("ENABLED_TEST_FLAG", true, "DISABLED_TEST_FLAG", false);

        var createConfig = new CreateSessionOptions().setExperiments(new SessionExperimentOptions().setFlags(flags));
        CreateSessionRequest createRequest = SessionRequestBuilder.buildCreateRequest(createConfig, "session-1");
        assertEquals(flags, createRequest.getExperiments().getFlags());
        var createJson = mapper.readTree(mapper.writeValueAsString(createRequest));
        assertTrue(createJson.path("experiments").path("flags").path("ENABLED_TEST_FLAG").asBoolean());
        assertFalse(createJson.path("experiments").path("flags").path("DISABLED_TEST_FLAG").asBoolean());

        var resumeConfig = new ResumeSessionOptions().setExperiments(new SessionExperimentOptions().setFlags(flags));
        ResumeSessionRequest resumeRequest = SessionRequestBuilder.buildResumeRequest("session-1", resumeConfig);
        assertEquals(flags, resumeRequest.getExperiments().getFlags());
        var resumeJson = mapper.readTree(mapper.writeValueAsString(resumeRequest));
        assertTrue(resumeJson.path("experiments").path("flags").path("ENABLED_TEST_FLAG").asBoolean());
        assertFalse(resumeJson.path("experiments").path("flags").path("DISABLED_TEST_FLAG").asBoolean());
    }

    @Test
    void testClonePreservesAndForwardsExpAssignments() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        var createAssignments = new CopilotExpAssignmentResponse()
                .setConfigs(List.of(new ExpConfigEntry().setId("exp-create")));
        var resumeAssignments = new CopilotExpAssignmentResponse()
                .setConfigs(List.of(new ExpConfigEntry().setId("exp-resume")));

        var createConfig = new CreateSessionOptions()
                .setExperiments(new SessionExperimentOptions().setAssignments(createAssignments));
        CreateSessionOptions createClone = createConfig.clone();
        assertEquals(createAssignments, createClone.getExperiments().getAssignments());
        CreateSessionRequest createRequest = SessionRequestBuilder.buildCreateRequest(createClone, "session-1");
        assertEquals(createAssignments, createRequest.getExperiments().getAssignments());
        assertTrue(mapper.writeValueAsString(createRequest).contains("\"Id\":\"exp-create\""));

        var resumeConfig = new ResumeSessionOptions()
                .setExperiments(new SessionExperimentOptions().setAssignments(resumeAssignments));
        ResumeSessionOptions resumeClone = resumeConfig.clone();
        assertEquals(resumeAssignments, resumeClone.getExperiments().getAssignments());
        ResumeSessionRequest resumeRequest = SessionRequestBuilder.buildResumeRequest("session-1", resumeClone);
        assertEquals(resumeAssignments, resumeRequest.getExperiments().getAssignments());
        assertTrue(mapper.writeValueAsString(resumeRequest).contains("\"Id\":\"exp-resume\""));
    }

    @Test
    void githubMcpToolConfigIsMappedAndSerializedForCreateAndResume() throws Exception {
        var config = new GitHubMcpToolConfig().setEnableAllTools(true).setAdditionalToolsets(List.of("repos"))
                .setAdditionalTools(List.of("get_issue")).setEnableInsidersMode(true).setDisableFormDeferral(true);
        var createRequest = SessionRequestBuilder.buildCreateRequest(
                new CreateSessionOptions().setGitHub(new SessionGitHubOptions().setMcp(config)), "session-1");
        var resumeRequest = SessionRequestBuilder.buildResumeRequest("session-1",
                new ResumeSessionOptions().setGitHub(new SessionGitHubOptions().setMcp(config)));

        assertSame(config, createRequest.getGitHub().getMcp());
        assertSame(config, resumeRequest.getGitHub().getMcp());
        var mapper = JsonRpcClient.getObjectMapper();
        assertTrue(mapper.valueToTree(createRequest).path("github").has("mcp"));
        assertTrue(mapper.valueToTree(resumeRequest).path("github").has("mcp"));
        assertFalse(
                mapper.valueToTree(SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "session-2"))
                        .path("github").has("mcp"));
    }

    @Test
    @AllowCopilotExperimental
    void permissionModeIsMappedAndSerializedForCreateAndResume() throws Exception {
        var mapper = JsonRpcClient.getObjectMapper();
        for (var mode : PermissionMode.values()) {
            var createRequest = SessionRequestBuilder.buildCreateRequest(
                    new CreateSessionOptions().setPermissions(new SessionPermissionOptions().setMode(mode)),
                    "session-1");
            var resumeRequest = SessionRequestBuilder.buildResumeRequest("session-1",
                    new ResumeSessionOptions().setPermissions(new SessionPermissionOptions().setMode(mode)));

            assertEquals(mode, createRequest.getPermissions().getMode());
            assertEquals(mode, resumeRequest.getPermissions().getMode());
            var expected = "\"mode\":\"" + mode.getValue() + "\"";
            assertTrue(mapper.writeValueAsString(createRequest).contains(expected));
            assertTrue(mapper.writeValueAsString(resumeRequest).contains(expected));
        }
        assertFalse(mapper
                .writeValueAsString(SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions(), "session-2"))
                .contains("permissionMode"));
        assertFalse(mapper
                .writeValueAsString(SessionRequestBuilder.buildResumeRequest("session-2", new ResumeSessionOptions()))
                .contains("permissionMode"));

        var outcome = "{\"sessionId\":\"s1\",\"permissionModeResult\":{\"success\":false,\"mode\":\"manual\"}}";
        for (var result : new SessionPermissionsSetModeResult[]{
                mapper.readValue(outcome, CreateSessionResponse.class).permissionModeResult(),
                mapper.readValue(outcome, ResumeSessionResponse.class).permissionModeResult()}) {
            assertNotNull(result);
            assertFalse(result.success());
            assertEquals(PermissionMode.MANUAL, result.mode());
        }
        assertNull(mapper.readValue("{\"sessionId\":\"s1\"}", CreateSessionResponse.class).permissionModeResult());
        assertNull(new ResumeSessionResponse("s1", null, null, null, null).permissionModeResult());
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
}
