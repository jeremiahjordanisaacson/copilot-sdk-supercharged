/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CreateSessionFilesystemOptions;
import com.microsoft.copilot.rpc.CreateSessionInstructionOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.SessionAgentOptions;
import com.microsoft.copilot.rpc.SessionExecutionOptions;
import com.microsoft.copilot.rpc.SessionFilesystemOptions;
import com.microsoft.copilot.rpc.SessionGitOptions;
import com.microsoft.copilot.rpc.SessionHookOptions;
import com.microsoft.copilot.rpc.SessionInferenceOptions;
import com.microsoft.copilot.rpc.SessionInstructionOptions;
import com.microsoft.copilot.rpc.SessionSkillOptions;
import com.microsoft.copilot.rpc.SessionStateOptions;
import com.microsoft.copilot.rpc.SpawnedTcpRuntimeConnection;
import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

import com.microsoft.copilot.generated.SessionEvent;
import com.microsoft.copilot.generated.rpc.SandboxConfig;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
import com.microsoft.copilot.rpc.AutoModeSwitchResponse;
import com.microsoft.copilot.rpc.AskUserVariant;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.DefaultAgentConfig;
import com.microsoft.copilot.rpc.ExitPlanModeResult;
import com.microsoft.copilot.rpc.InfiniteSessionConfig;
import com.microsoft.copilot.rpc.LargeToolOutputConfig;
import com.microsoft.copilot.rpc.MemoryConfiguration;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.generated.rpc.Model;
import com.microsoft.copilot.generated.rpc.ModelCapabilities;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesLimits;
import com.microsoft.copilot.generated.rpc.ModelCapabilitiesSupports;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.SystemMessageConfig;
import com.microsoft.copilot.rpc.TelemetryConfig;

class ConfigCloneTest {

    @Test
    void copilotClientOptionsCloneBasic() {
        CopilotClientOptions original = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        var runtime = RuntimeConnection.forSpawnedTcp().setPath("/usr/local/bin/copilot").setPort(9000)
                .setConnectionToken("my-token-123").setArgs(List.of("--extra")).setEnvironment(Map.of("KEY", "value"))
                .setBaseDirectory("/custom/copilot/home").setSessionIdleTimeoutSeconds(600);
        original.setRuntime(runtime);
        original.setObservability(original.getObservability().setLogLevel("debug"));
        original.setGitHub(original.getGitHub().setToken("ghp_test"));
        original.setGitHub(original.getGitHub().setUseLoggedInUser(false));
        original.setRemote(true);

        CopilotClientOptions cloned = original.clone();
        var clonedRuntime = assertInstanceOf(SpawnedTcpRuntimeConnection.class, cloned.getRuntime());

        assertNotSame(runtime, clonedRuntime);
        assertEquals(runtime.getPath(), clonedRuntime.getPath());
        assertEquals(original.getObservability().getLogLevel(), cloned.getObservability().getLogLevel());
        assertEquals(runtime.getPort(), clonedRuntime.getPort());
        assertEquals(original.getGitHub().getToken(), cloned.getGitHub().getToken());
        assertEquals(original.getGitHub().getUseLoggedInUser(), cloned.getGitHub().getUseLoggedInUser());
        assertEquals(runtime.getBaseDirectory(), clonedRuntime.getBaseDirectory());
        assertEquals(original.isRemote(), cloned.isRemote());
        assertEquals(runtime.getSessionIdleTimeoutSeconds(), clonedRuntime.getSessionIdleTimeoutSeconds());
        assertEquals(runtime.getConnectionToken(), clonedRuntime.getConnectionToken());

        runtime.getArgs().set(0, "--changed");
        runtime.setEnvironment(Map.of("KEY", "changed")).setBaseDirectory("/different/home");
        assertEquals(List.of("--extra"), clonedRuntime.getArgs());
        assertEquals(Map.of("KEY", "value"), clonedRuntime.getEnvironment());
        assertEquals("/custom/copilot/home", clonedRuntime.getBaseDirectory());
    }

    @Test
    void copilotClientOptionsOnListModelsCloned() {
        CopilotClientOptions original = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        List<Model> models = List.of(Model.builder().id("test").name("Test")
                .capabilities(ModelCapabilities.builder().supports(ModelCapabilitiesSupports.builder().build())
                        .limits(ModelCapabilitiesLimits.builder().maxContextWindowTokens(8192L).build()).build())
                .build());
        original.setModels(original.getModels().setOnListModels(() -> CompletableFuture.completedFuture(models)));

        CopilotClientOptions cloned = original.clone();

        assertNotNull(cloned.getModels().getOnListModels());
        assertSame(original.getModels().getOnListModels(), cloned.getModels().getOnListModels());
    }

    @Test
    void sessionConfigCloneBasic() {
        CreateSessionOptions original = new CreateSessionOptions();
        original.setSessionId("my-session");
        original.setClientName("my-app");
        original.setModel("gpt-4o");
        original.setInference(original.getInference().setReasoningSummary("detailed"));
        original.setInference(original.getInference().setContextTier("long_context"));
        original.setInteraction(original.getInteraction().setAskUserVariant(AskUserVariant.ELICITATION));
        original.setPlugins(original.getPlugins().setDirectories(List.of("/plugins/a", "/plugins/b")));
        original.setMcp(original.getMcp().setDisabledServers(List.of("local-files", "remote-github")));
        original.setPermissions(original.getPermissions()
                .setSandbox(SandboxConfig.builder().enabled(true).userPolicy(null).addCurrentWorkingDirectory(false)
                        .sandboxMcpServers(true).sandboxLspServers(null).allowBypass(null).managedMcpRoutingLocked(null)
                        .managedLspRoutingLocked(null).auth(null).credentials(null).allowDevToolAccess(null).build()));
        original.setTools(original.getTools().setLargeOutput(
                new LargeToolOutputConfig().setEnabled(true).setMaxSizeBytes(1024L).setOutputDirectory("/tmp/out")));
        original.setState(original.getState().setMemory(new MemoryConfiguration().setEnabled(true)));
        original.setEvents(original.getEvents().setStreaming(true));
        original.getAgents().setLocalDiscoveryOnly(false);
        original.getGit().setCoauthorEnabled(true);
        original.getTools().setManageScheduleEnabled(true);
        original.getInstructions().setSkipCustomInstructions(false);

        CreateSessionOptions cloned = original.clone();

        assertEquals(original.getSessionId(), cloned.getSessionId());
        assertEquals(original.getClientName(), cloned.getClientName());
        assertEquals(original.getModel(), cloned.getModel());
        assertEquals(original.getInference().getReasoningSummary(), cloned.getInference().getReasoningSummary());
        assertEquals(original.getInference().getContextTier(), cloned.getInference().getContextTier());
        assertEquals(original.getInteraction().getAskUserVariant(), cloned.getInteraction().getAskUserVariant());
        assertEquals(original.getPlugins().getDirectories(), cloned.getPlugins().getDirectories());
        assertEquals(original.getMcp().getDisabledServers(), cloned.getMcp().getDisabledServers());
        assertSame(original.getPermissions().getSandbox(), cloned.getPermissions().getSandbox());
        assertEquals(original.getTools().getLargeOutput(), cloned.getTools().getLargeOutput());
        assertEquals(original.getState().getMemory(), cloned.getState().getMemory());
        assertEquals(original.getEvents().isStreaming(), cloned.getEvents().isStreaming());
        assertEquals(Optional.of(false), cloned.getAgents().getLocalDiscoveryOnly());
        assertEquals(Optional.of(true), cloned.getGit().getCoauthorEnabled());
        assertEquals(Optional.of(true), cloned.getTools().getManageScheduleEnabled());
        assertEquals(Optional.of(false), cloned.getInstructions().getSkipCustomInstructions());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void sandboxConfigCloned(boolean enabled) {
        var sandbox = SandboxConfig.builder().enabled(enabled).userPolicy(null).addCurrentWorkingDirectory(false)
                .sandboxMcpServers(true).sandboxLspServers(null).allowBypass(null).managedMcpRoutingLocked(null)
                .managedLspRoutingLocked(null).auth(null).credentials(null).allowDevToolAccess(null).build();
        var create = new CreateSessionOptions();
        assertSame(create, create.setPermissions(create.getPermissions().setSandbox(sandbox)));
        assertSame(sandbox, create.clone().getPermissions().getSandbox());
        var resume = new ResumeSessionOptions();
        assertSame(resume, resume.setPermissions(resume.getPermissions().setSandbox(sandbox)));
        assertSame(sandbox, resume.clone().getPermissions().getSandbox());
        assertNull(new CreateSessionOptions().clone().getPermissions().getSandbox());
        assertNull(new ResumeSessionOptions().clone().getPermissions().getSandbox());
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(booleans = {true, false})
    void sessionConfigRefreshCustomInstructionsCloned(Boolean refresh) {
        var original = new CreateSessionOptions();
        assertTrue(original.getInstructions().getRefreshCustomInstructions().isEmpty());
        if (refresh != null) {
            assertSame(original,
                    original.setInstructions(original.getInstructions().setRefreshCustomInstructions(refresh)));
        }

        var cloned = original.clone();
        assertEquals(Optional.ofNullable(refresh), cloned.getInstructions().getRefreshCustomInstructions());
        assertSame(cloned.getInstructions(), cloned.getInstructions().clearRefreshCustomInstructions());
        assertTrue(cloned.getInstructions().getRefreshCustomInstructions().isEmpty());
        assertEquals(Optional.ofNullable(refresh), original.getInstructions().getRefreshCustomInstructions());
    }

    @Test
    void sessionConfigListIndependence() {
        CreateSessionOptions original = new CreateSessionOptions();
        List<String> toolList = new ArrayList<>();
        toolList.add("grep");
        toolList.add("bash");
        original.setTools(original.getTools().setAvailableTools(toolList));
        original.setInstructions(
                original.getInstructions().setDirectories(new ArrayList<>(List.of("/path/a", "/path/b"))));
        original.setMcp(original.getMcp().setDisabledServers(new ArrayList<>(List.of("local-files"))));

        CreateSessionOptions cloned = original.clone();

        // Mutate the original list directly to test independence
        toolList.add("web");

        // The cloned config should be unaffected by mutations to the original list
        assertEquals(2, cloned.getTools().getAvailableTools().size());
        assertEquals(3, original.getTools().getAvailableTools().size());
        assertEquals(List.of("/path/a", "/path/b"), cloned.getInstructions().getDirectories());
        assertEquals(List.of("local-files"), cloned.getMcp().getDisabledServers());
    }

    @Test
    void sessionConfigAgentAndOnEventCloned() {
        Consumer<SessionEvent> handler = event -> {
        };
        CreateSessionOptions original = new CreateSessionOptions();
        original.setAgents(original.getAgents().setSelected("my-agent"));
        original.setEvents(original.getEvents().setOnEvent(handler));

        CreateSessionOptions cloned = original.clone();

        assertEquals("my-agent", cloned.getAgents().getSelected());
        assertSame(handler, cloned.getEvents().getOnEvent());
    }

    @Test
    void sessionConfigSessionPolicyOptionsCloned() {
        var sessionLimits = SessionLimitsConfig.builder().maxAiCredits(30.0).build();
        var excludedAgents = new ArrayList<>(List.of("explore"));
        CreateSessionOptions original = new CreateSessionOptions()
                .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(excludedAgents))
                .setInference(new SessionInferenceOptions().setEnableCitations(true))
                .setFilesystem(new CreateSessionFilesystemOptions().setTrackFileChanges(true))
                .setExecution(new SessionExecutionOptions().setLimits(sessionLimits));

        CreateSessionOptions cloned = original.clone();
        excludedAgents.add("task");

        assertEquals(List.of("explore"), cloned.getAgents().getExcludedBuiltInAgents());
        assertTrue(cloned.getInference().getEnableCitations().orElse(false));
        assertTrue(cloned.getFilesystem().getTrackFileChanges().orElse(false));
        assertSame(sessionLimits, cloned.getExecution().getLimits());
    }

    @Test
    void resumeSessionConfigCloneBasic() {
        ResumeSessionOptions original = new ResumeSessionOptions();
        original.setModel("o1");
        original.setInference(original.getInference().setReasoningSummary("none"));
        original.setInference(original.getInference().setContextTier("long_context"));
        original.setInteraction(original.getInteraction().setAskUserVariant(AskUserVariant.LEGACY));
        original.setPlugins(original.getPlugins().setDirectories(List.of("/plugins/r")));
        original.setMcp(original.getMcp().setDisabledServers(List.of("local-files-r")));
        original.setTools(original.getTools().setLargeOutput(new LargeToolOutputConfig().setEnabled(false)
                .setMaxSizeBytes(2048L).setOutputDirectory("/tmp/resume")));
        original.setState(original.getState().setMemory(new MemoryConfiguration().setEnabled(false)));
        original.setEvents(original.getEvents().setStreaming(false));

        ResumeSessionOptions cloned = original.clone();

        assertEquals(original.getModel(), cloned.getModel());
        assertEquals(original.getInference().getReasoningSummary(), cloned.getInference().getReasoningSummary());
        assertEquals(original.getInference().getContextTier(), cloned.getInference().getContextTier());
        assertEquals(original.getInteraction().getAskUserVariant(), cloned.getInteraction().getAskUserVariant());
        assertEquals(original.getPlugins().getDirectories(), cloned.getPlugins().getDirectories());
        assertEquals(original.getMcp().getDisabledServers(), cloned.getMcp().getDisabledServers());
        assertEquals(original.getTools().getLargeOutput(), cloned.getTools().getLargeOutput());
        assertEquals(original.getState().getMemory(), cloned.getState().getMemory());
        assertEquals(original.getEvents().isStreaming(), cloned.getEvents().isStreaming());
    }

    @Test
    void resumeSessionConfigAgentAndOnEventCloned() {
        Consumer<SessionEvent> handler = event -> {
        };
        ResumeSessionOptions original = new ResumeSessionOptions();
        original.setAgents(original.getAgents().setSelected("my-agent"));
        original.setEvents(original.getEvents().setOnEvent(handler));

        ResumeSessionOptions cloned = original.clone();

        assertEquals("my-agent", cloned.getAgents().getSelected());
        assertSame(handler, cloned.getEvents().getOnEvent());
    }

    @Test
    void resumeSessionConfigSessionPolicyOptionsCloned() {
        var sessionLimits = SessionLimitsConfig.builder().maxAiCredits(30.0).build();
        var excludedAgents = new ArrayList<>(List.of("explore"));
        ResumeSessionOptions original = new ResumeSessionOptions()
                .setAgents(new SessionAgentOptions().setExcludedBuiltInAgents(excludedAgents))
                .setInference(new SessionInferenceOptions().setEnableCitations(true))
                .setFilesystem(new SessionFilesystemOptions().setTrackFileChanges(true))
                .setExecution(new SessionExecutionOptions().setLimits(sessionLimits));

        ResumeSessionOptions cloned = original.clone();
        excludedAgents.add("task");

        assertEquals(List.of("explore"), cloned.getAgents().getExcludedBuiltInAgents());
        assertTrue(cloned.getInference().getEnableCitations().orElse(false));
        assertTrue(cloned.getFilesystem().getTrackFileChanges().orElse(false));
        assertSame(sessionLimits, cloned.getExecution().getLimits());
    }

    @Test
    void messageOptionsCloneBasic() {
        MessageOptions original = new MessageOptions();
        original.setPrompt("What is 2+2?");
        original.setMode("immediate");

        MessageOptions cloned = original.clone();

        assertEquals(original.getPrompt(), cloned.getPrompt());
        assertEquals(original.getMode(), cloned.getMode());
    }

    @Test
    void sessionConfigEnableSessionTelemetryCopied() {
        CreateSessionOptions original = new CreateSessionOptions();
        original.setGitHub(original.getGitHub().setTelemetryEnabled(false));

        CreateSessionOptions cloned = original.clone();

        assertFalse(cloned.getGitHub().getTelemetryEnabled().orElse(true));
    }

    @Test
    void sessionConfigEnableSessionTelemetryDefaultIsNull() {
        CreateSessionOptions original = new CreateSessionOptions();

        CreateSessionOptions cloned = original.clone();

        assertTrue(cloned.getGitHub().getTelemetryEnabled().isEmpty());
    }

    @Test
    void sessionConfigGranularMultitenancyFieldsCopied() {
        CreateSessionOptions original = new CreateSessionOptions()
                .setState(new SessionStateOptions().setSkipEmbeddingRetrieval(true)
                        .setEmbeddingCacheStorage("persistent").setEnableCrossSessionStore(true))
                .setInstructions(new CreateSessionInstructionOptions().setOrganizationText("Org instructions")
                        .setOnDemandDiscovery(false))
                .setHooks(new SessionHookOptions().setEnableRepositoryHooks(true))
                .setGit(new SessionGitOptions().setEnableHostGitOperations(false))
                .setSkills(new SessionSkillOptions().setEnabled(false));

        CreateSessionOptions cloned = original.clone();

        assertTrue(cloned.getState().getSkipEmbeddingRetrieval().orElse(false));
        assertEquals("Org instructions", cloned.getInstructions().getOrganizationText());
        assertFalse(cloned.getInstructions().getOnDemandDiscovery().orElse(true));
        assertEquals("persistent", cloned.getState().getEmbeddingCacheStorage());
        assertTrue(cloned.getHooks().getEnableRepositoryHooks().orElse(false));
        assertFalse(cloned.getGit().getEnableHostGitOperations().orElse(true));
        assertTrue(cloned.getState().getEnableCrossSessionStore().orElse(false));
        assertFalse(cloned.getSkills().getEnabled().orElse(true));
    }

    @Test
    @AllowCopilotExperimental
    void sessionConfigSkillProviderCloned() {
        SkillProvider provider = testSkillProvider();
        CreateSessionOptions original = new CreateSessionOptions()
                .setSkills(new SessionSkillOptions().setProvider(provider));

        CreateSessionOptions cloned = original.clone();

        assertSame(provider, cloned.getSkills().getProvider());
    }

    @Test
    void resumeSessionConfigEnableSessionTelemetryCopied() {
        ResumeSessionOptions original = new ResumeSessionOptions();
        original.setGitHub(original.getGitHub().setTelemetryEnabled(false));

        ResumeSessionOptions cloned = original.clone();

        assertFalse(cloned.getGitHub().getTelemetryEnabled().orElse(true));
    }

    @Test
    void resumeSessionConfigEnableSessionTelemetryDefaultIsNull() {
        ResumeSessionOptions original = new ResumeSessionOptions();

        ResumeSessionOptions cloned = original.clone();

        assertTrue(cloned.getGitHub().getTelemetryEnabled().isEmpty());
    }

    @Test
    void resumeSessionConfigGranularMultitenancyFieldsCopied() {
        ResumeSessionOptions original = new ResumeSessionOptions()
                .setState(new SessionStateOptions().setSkipEmbeddingRetrieval(false)
                        .setEmbeddingCacheStorage("persistent").setEnableCrossSessionStore(false))
                .setInstructions(new SessionInstructionOptions().setOrganizationText("Resume org instructions")
                        .setOnDemandDiscovery(true))
                .setHooks(new SessionHookOptions().setEnableRepositoryHooks(false))
                .setGit(new SessionGitOptions().setEnableHostGitOperations(true))
                .setSkills(new SessionSkillOptions().setEnabled(true));

        ResumeSessionOptions cloned = original.clone();

        assertFalse(cloned.getState().getSkipEmbeddingRetrieval().orElse(true));
        assertEquals("Resume org instructions", cloned.getInstructions().getOrganizationText());
        assertTrue(cloned.getInstructions().getOnDemandDiscovery().orElse(false));
        assertEquals("persistent", cloned.getState().getEmbeddingCacheStorage());
        assertFalse(cloned.getHooks().getEnableRepositoryHooks().orElse(true));
        assertTrue(cloned.getGit().getEnableHostGitOperations().orElse(false));
        assertFalse(cloned.getState().getEnableCrossSessionStore().orElse(true));
        assertTrue(cloned.getSkills().getEnabled().orElse(false));
    }

    @Test
    @AllowCopilotExperimental
    void resumeSessionConfigSkillProviderCloned() {
        SkillProvider provider = testSkillProvider();
        ResumeSessionOptions original = new ResumeSessionOptions()
                .setSkills(new SessionSkillOptions().setProvider(provider));

        ResumeSessionOptions cloned = original.clone();

        assertSame(provider, cloned.getSkills().getProvider());
    }

    @Test
    void clonePreservesNullFields() {
        CopilotClientOptions opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        CopilotClientOptions optsClone = opts.clone();
        assertNull(optsClone.getRuntime());

        CreateSessionOptions cfg = new CreateSessionOptions();
        CreateSessionOptions cfgClone = cfg.clone();
        assertNull(cfgClone.getModel());

        MessageOptions msg = new MessageOptions();
        MessageOptions msgClone = msg.clone();
        assertNull(msgClone.getMode());
    }

    @Test
    void copilotClientOptionsGitHubToken() {
        CopilotClientOptions opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        opts.setGitHub(opts.getGitHub().setToken("test-token"));
        assertEquals("test-token", opts.getGitHub().getToken());
    }

    @Test
    void copilotClientOptionsSetTelemetry() {
        var telemetry = new TelemetryConfig().setOtlpEndpoint("http://localhost:4318");
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        opts.setObservability(opts.getObservability().setTelemetry(telemetry));
        assertSame(telemetry, opts.getObservability().getTelemetry());
    }

    @Test
    void copilotClientOptionsClearUseLoggedInUser() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        opts.setGitHub(opts.getGitHub().setUseLoggedInUser(true));
        opts.getGitHub().clearUseLoggedInUser();
        assertTrue(opts.getGitHub().getUseLoggedInUser().isEmpty());
    }

    @Test
    void resumeSessionConfigAllSetters() {
        var config = new ResumeSessionOptions();

        var sysMsg = new SystemMessageConfig();
        config.setSystemMessage(sysMsg);
        assertSame(sysMsg, config.getSystemMessage());

        config.setTools(config.getTools().setAvailableTools(List.of("bash", "read_file")));
        assertEquals(List.of("bash", "read_file"), config.getTools().getAvailableTools());

        config.setTools(config.getTools().setExcludedTools(List.of("write_file")));
        assertEquals(List.of("write_file"), config.getTools().getExcludedTools());

        config.setInference(config.getInference().setReasoningEffort("high"));
        assertEquals("high", config.getInference().getReasoningEffort());

        config.setWorkingDirectory("/project/src");
        assertEquals("/project/src", config.getWorkingDirectory());

        config.setConfigDirectory("/home/user/.config/copilot");
        assertEquals("/home/user/.config/copilot", config.getConfigDirectory());

        config.setSkills(config.getSkills().setDirectories(List.of("/skills/custom")));
        assertEquals(List.of("/skills/custom"), config.getSkills().getDirectories());

        config.setSkills(config.getSkills().setDisabledSkills(List.of("some-skill")));
        assertEquals(List.of("some-skill"), config.getSkills().getDisabledSkills());

        var infiniteConfig = new InfiniteSessionConfig().setEnabled(true);
        config.setState(config.getState().setInfiniteSessions(infiniteConfig));
        assertSame(infiniteConfig, config.getState().getInfiniteSessions());
    }

    @Test
    void sessionConfigNewFieldsCloned() {
        CreateSessionOptions original = new CreateSessionOptions();
        original.setGitHub(original.getGitHub().setToken("ghp_per_session_token"));
        original.setFilesystem(
                original.getFilesystem().setAdditionalDirectories(new java.util.ArrayList<>(List.of("/repo/shared"))));
        DefaultAgentConfig defaultAgent = new DefaultAgentConfig().setExcludedTools(List.of("secret_tool"));
        original.setAgents(original.getAgents().setDefault(defaultAgent));

        CreateSessionOptions cloned = original.clone();

        assertEquals("ghp_per_session_token", cloned.getGitHub().getToken());
        assertEquals(List.of("/repo/shared"), cloned.getFilesystem().getAdditionalDirectories());
        assertNotSame(original.getFilesystem().getAdditionalDirectories(),
                cloned.getFilesystem().getAdditionalDirectories());
        assertSame(defaultAgent, cloned.getAgents().getDefault());
    }

    @Test
    void resumeSessionConfigNewFieldsCloned() {
        ResumeSessionOptions original = new ResumeSessionOptions();
        original.setGitHub(original.getGitHub().setToken("ghp_per_session_token"));
        original.setFilesystem(
                original.getFilesystem().setAdditionalDirectories(new java.util.ArrayList<>(List.of("/repo/resumed"))));
        DefaultAgentConfig defaultAgent = new DefaultAgentConfig().setExcludedTools(List.of("secret_tool"));
        original.setAgents(original.getAgents().setDefault(defaultAgent));

        ResumeSessionOptions cloned = original.clone();

        assertEquals("ghp_per_session_token", cloned.getGitHub().getToken());
        assertEquals(List.of("/repo/resumed"), cloned.getFilesystem().getAdditionalDirectories());
        assertNotSame(original.getFilesystem().getAdditionalDirectories(),
                cloned.getFilesystem().getAdditionalDirectories());
        assertSame(defaultAgent, cloned.getAgents().getDefault());
    }

    @Test
    void copilotClientOptionsSessionIdleTimeoutCloned() {
        CopilotClientOptions original = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        original.setRuntime(RuntimeConnection.forDefault().setSessionIdleTimeoutSeconds(600));

        CopilotClientOptions cloned = original.clone();

        assertEquals(600, com.microsoft.copilot.RuntimeSettings.idleTimeout(cloned.getRuntime()).getAsInt());
    }

    @Test
    void sessionConfigCloneCopiesModeSwitchHandlers() {
        CreateSessionOptions original = new CreateSessionOptions();
        original.setInteraction(original.getInteraction().setOnExitPlanModeRequest(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult())));
        original.setInteraction(original.getInteraction().setOnAutoModeSwitchRequest(
                (request, invocation) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        CreateSessionOptions cloned = original.clone();

        assertSame(original.getInteraction().getOnExitPlanModeRequest(),
                cloned.getInteraction().getOnExitPlanModeRequest());
        assertSame(original.getInteraction().getOnAutoModeSwitchRequest(),
                cloned.getInteraction().getOnAutoModeSwitchRequest());
    }

    @Test
    void resumeSessionConfigCloneCopiesModeSwitchHandlers() {
        ResumeSessionOptions original = new ResumeSessionOptions();
        original.setInteraction(original.getInteraction().setOnExitPlanModeRequest(
                (request, invocation) -> CompletableFuture.completedFuture(new ExitPlanModeResult())));
        original.setInteraction(original.getInteraction().setOnAutoModeSwitchRequest(
                (request, invocation) -> CompletableFuture.completedFuture(AutoModeSwitchResponse.NO)));

        ResumeSessionOptions cloned = original.clone();

        assertSame(original.getInteraction().getOnExitPlanModeRequest(),
                cloned.getInteraction().getOnExitPlanModeRequest());
        assertSame(original.getInteraction().getOnAutoModeSwitchRequest(),
                cloned.getInteraction().getOnAutoModeSwitchRequest());
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
