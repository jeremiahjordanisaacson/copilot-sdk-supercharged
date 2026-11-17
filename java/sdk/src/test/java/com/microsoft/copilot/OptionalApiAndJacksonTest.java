/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.OwnedRuntimeConnection;
import com.microsoft.copilot.rpc.RuntimeConnection;
import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.CustomAgentConfig;
import com.microsoft.copilot.rpc.InfiniteSessionConfig;
import com.microsoft.copilot.rpc.InputOptions;
import com.microsoft.copilot.rpc.ModelCapabilitiesOverride;
import com.microsoft.copilot.rpc.ProviderConfig;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.SessionUiCapabilities;
import com.microsoft.copilot.rpc.TelemetryConfig;
import com.microsoft.copilot.rpc.UserInputRequest;
import org.junit.jupiter.api.Test;

/**
 * Validates that every {@code clearXxx()} method resets its field to absent,
 * that Optional-returning getters report the correct state, and that Jackson
 * omits cleared fields from serialized output.
 */
class OptionalApiAndJacksonTest {

    private static final ObjectMapper MAPPER = JsonRpcClient.getObjectMapper();

    // ── CopilotClientOptions ──────────────────────────────────────────

    @Test
    void copilotClientOptions_clearSessionIdleTimeoutSeconds() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        opts.setRuntime(RuntimeConnection.forDefault().setSessionIdleTimeoutSeconds(120));
        assertFalse(com.microsoft.copilot.RuntimeSettings.idleTimeout(opts.getRuntime()).isEmpty());

        ((OwnedRuntimeConnection<?>) opts.getRuntime()).setSessionIdleTimeoutSeconds(null);
        assertTrue(com.microsoft.copilot.RuntimeSettings.idleTimeout(opts.getRuntime()).isEmpty());
    }

    @Test
    void copilotClientOptions_clearUseLoggedInUser() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        opts.setGitHub(opts.getGitHub().setUseLoggedInUser(true));
        assertTrue(opts.getGitHub().getUseLoggedInUser().isPresent());

        opts.getGitHub().clearUseLoggedInUser();
        assertTrue(opts.getGitHub().getUseLoggedInUser().isEmpty());
    }

    // ── CreateSessionOptions ─────────────────────────────────────────────────

    @Test
    void sessionConfig_clearEnableSessionTelemetry() {
        var cfg = new CreateSessionOptions();
        cfg.setGitHub(cfg.getGitHub().setTelemetryEnabled(true));
        assertTrue(cfg.getGitHub().getTelemetryEnabled().isPresent());

        cfg.getGitHub().clearTelemetryEnabled();
        assertTrue(cfg.getGitHub().getTelemetryEnabled().isEmpty());
    }

    @Test
    void sessionConfig_clearEnableConfigDiscovery() {
        var cfg = new CreateSessionOptions();
        cfg.setEnableConfigDiscovery(false);
        assertTrue(cfg.getEnableConfigDiscovery().isPresent());

        cfg.clearEnableConfigDiscovery();
        assertTrue(cfg.getEnableConfigDiscovery().isEmpty());
    }

    @Test
    void sessionConfig_clearIncludeSubAgentStreamingEvents() {
        var cfg = new CreateSessionOptions();
        cfg.setEvents(cfg.getEvents().setIncludeSubAgentStreamingEvents(true));
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().isPresent());

        cfg.getEvents().clearIncludeSubAgentStreamingEvents();
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().isEmpty());
    }

    // ── ResumeSessionOptions ───────────────────────────────────────────

    @Test
    void resumeSessionConfig_clearEnableSessionTelemetry() {
        var cfg = new ResumeSessionOptions();
        cfg.setGitHub(cfg.getGitHub().setTelemetryEnabled(false));
        assertTrue(cfg.getGitHub().getTelemetryEnabled().isPresent());

        cfg.getGitHub().clearTelemetryEnabled();
        assertTrue(cfg.getGitHub().getTelemetryEnabled().isEmpty());
    }

    @Test
    void resumeSessionConfig_clearEnableConfigDiscovery() {
        var cfg = new ResumeSessionOptions();
        cfg.setEnableConfigDiscovery(true);
        assertTrue(cfg.getEnableConfigDiscovery().isPresent());

        cfg.clearEnableConfigDiscovery();
        assertTrue(cfg.getEnableConfigDiscovery().isEmpty());
    }

    @Test
    void resumeSessionConfig_clearIncludeSubAgentStreamingEvents() {
        var cfg = new ResumeSessionOptions();
        cfg.setEvents(cfg.getEvents().setIncludeSubAgentStreamingEvents(false));
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().isPresent());

        cfg.getEvents().clearIncludeSubAgentStreamingEvents();
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().isEmpty());
    }

    // ── InfiniteSessionConfig ─────────────────────────────────────────

    @Test
    void infiniteSessionConfig_clearEnabled() {
        var cfg = new InfiniteSessionConfig();
        cfg.setEnabled(true);
        assertTrue(cfg.getEnabled().isPresent());

        cfg.clearEnabled();
        assertTrue(cfg.getEnabled().isEmpty());
    }

    @Test
    void infiniteSessionConfig_clearBackgroundCompactionThreshold() {
        var cfg = new InfiniteSessionConfig();
        cfg.setBackgroundCompactionThreshold(0.75);
        assertFalse(cfg.getBackgroundCompactionThreshold().isEmpty());

        cfg.clearBackgroundCompactionThreshold();
        assertTrue(cfg.getBackgroundCompactionThreshold().isEmpty());
    }

    @Test
    void infiniteSessionConfig_clearBufferExhaustionThreshold() {
        var cfg = new InfiniteSessionConfig();
        cfg.setBufferExhaustionThreshold(0.9);
        assertFalse(cfg.getBufferExhaustionThreshold().isEmpty());

        cfg.clearBufferExhaustionThreshold();
        assertTrue(cfg.getBufferExhaustionThreshold().isEmpty());
    }

    // ── InputOptions ──────────────────────────────────────────────────

    @Test
    void inputOptions_clearMinLength() {
        var opts = new InputOptions();
        opts.setMinLength(5);
        assertFalse(opts.getMinLength().isEmpty());

        opts.clearMinLength();
        assertTrue(opts.getMinLength().isEmpty());
    }

    @Test
    void inputOptions_clearMaxLength() {
        var opts = new InputOptions();
        opts.setMaxLength(100);
        assertFalse(opts.getMaxLength().isEmpty());

        opts.clearMaxLength();
        assertTrue(opts.getMaxLength().isEmpty());
    }

    // ── ModelCapabilitiesOverride.Supports ─────────────────────────────

    @Test
    void supports_clearVision() {
        var s = new ModelCapabilitiesOverride.Supports();
        s.setVision(true);
        assertTrue(s.getVision().isPresent());

        s.clearVision();
        assertTrue(s.getVision().isEmpty());
    }

    @Test
    void supports_clearReasoningEffort() {
        var s = new ModelCapabilitiesOverride.Supports();
        s.setReasoningEffort(false);
        assertTrue(s.getReasoningEffort().isPresent());

        s.clearReasoningEffort();
        assertTrue(s.getReasoningEffort().isEmpty());
    }

    // ── ModelCapabilitiesOverride.Limits ───────────────────────────────

    @Test
    void limits_clearMaxPromptTokens() {
        var l = new ModelCapabilitiesOverride.Limits();
        l.setMaxPromptTokens(4096);
        assertFalse(l.getMaxPromptTokens().isEmpty());

        l.clearMaxPromptTokens();
        assertTrue(l.getMaxPromptTokens().isEmpty());
    }

    @Test
    void limits_clearMaxOutputTokens() {
        var l = new ModelCapabilitiesOverride.Limits();
        l.setMaxOutputTokens(1024);
        assertFalse(l.getMaxOutputTokens().isEmpty());

        l.clearMaxOutputTokens();
        assertTrue(l.getMaxOutputTokens().isEmpty());
    }

    @Test
    void limits_clearMaxContextWindowTokens() {
        var l = new ModelCapabilitiesOverride.Limits();
        l.setMaxContextWindowTokens(16384);
        assertFalse(l.getMaxContextWindowTokens().isEmpty());

        l.clearMaxContextWindowTokens();
        assertTrue(l.getMaxContextWindowTokens().isEmpty());
    }

    // ── ProviderConfig ────────────────────────────────────────────────

    @Test
    void providerConfig_clearMaxPromptTokens() {
        var cfg = new ProviderConfig();
        cfg.setMaxPromptTokens(2048);
        assertFalse(cfg.getMaxPromptTokens().isEmpty());

        cfg.clearMaxPromptTokens();
        assertTrue(cfg.getMaxPromptTokens().isEmpty());
    }

    @Test
    void providerConfig_clearMaxOutputTokens() {
        var cfg = new ProviderConfig();
        cfg.setMaxOutputTokens(512);
        assertFalse(cfg.getMaxOutputTokens().isEmpty());

        cfg.clearMaxOutputTokens();
        assertTrue(cfg.getMaxOutputTokens().isEmpty());
    }

    // ── TelemetryConfig ───────────────────────────────────────────────

    @Test
    void telemetryConfig_clearCaptureContent() {
        var cfg = new TelemetryConfig();
        cfg.setCaptureContent(true);
        assertTrue(cfg.getCaptureContent().isPresent());

        cfg.clearCaptureContent();
        assertTrue(cfg.getCaptureContent().isEmpty());
    }

    // ── SessionUiCapabilities ─────────────────────────────────────────

    @Test
    void sessionUiCapabilities_clearElicitation() {
        var caps = new SessionUiCapabilities();
        caps.setElicitation(true);
        assertTrue(caps.getElicitation().isPresent());

        caps.clearElicitation();
        assertTrue(caps.getElicitation().isEmpty());
    }

    // ── CustomAgentConfig ─────────────────────────────────────────────

    @Test
    void customAgentConfig_clearInfer() {
        var cfg = new CustomAgentConfig();
        cfg.setInfer(true);
        assertTrue(cfg.getInfer().isPresent());

        cfg.clearInfer();
        assertTrue(cfg.getInfer().isEmpty());
    }

    // ── UserInputRequest ──────────────────────────────────────────────

    @Test
    void userInputRequest_clearAllowFreeform() {
        var req = new UserInputRequest();
        req.setAllowFreeform(false);
        assertTrue(req.getAllowFreeform().isPresent());

        req.clearAllowFreeform();
        assertTrue(req.getAllowFreeform().isEmpty());
    }

    // ── Value retrieval through Optional getters ────────────────────────

    @Test
    void copilotClientOptions_sessionIdleTimeoutSecondsValue() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        assertTrue(com.microsoft.copilot.RuntimeSettings.idleTimeout(opts.getRuntime()).isEmpty());

        opts.setRuntime(RuntimeConnection.forDefault().setSessionIdleTimeoutSeconds(300));
        assertEquals(300, com.microsoft.copilot.RuntimeSettings.idleTimeout(opts.getRuntime()).getAsInt());

        opts.setRuntime(RuntimeConnection.forDefault().setSessionIdleTimeoutSeconds(0));
        assertTrue(com.microsoft.copilot.RuntimeSettings.idleTimeout(opts.getRuntime()).isPresent());
        assertEquals(0, com.microsoft.copilot.RuntimeSettings.idleTimeout(opts.getRuntime()).getAsInt());
    }

    @Test
    void copilotClientOptions_useLoggedInUserValue() {
        var opts = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT);
        assertTrue(opts.getGitHub().getUseLoggedInUser().isEmpty());

        opts.setGitHub(opts.getGitHub().setUseLoggedInUser(true));
        assertEquals(Boolean.TRUE, opts.getGitHub().getUseLoggedInUser().get());

        opts.setGitHub(opts.getGitHub().setUseLoggedInUser(false));
        assertEquals(Boolean.FALSE, opts.getGitHub().getUseLoggedInUser().get());
    }

    @Test
    void sessionConfig_enableSessionTelemetryValue() {
        var cfg = new CreateSessionOptions();
        assertFalse(cfg.getGitHub().getTelemetryEnabled().orElse(false));

        cfg.setGitHub(cfg.getGitHub().setTelemetryEnabled(true));
        assertTrue(cfg.getGitHub().getTelemetryEnabled().orElse(false));

        cfg.setGitHub(cfg.getGitHub().setTelemetryEnabled(false));
        assertFalse(cfg.getGitHub().getTelemetryEnabled().orElse(true));
    }

    @Test
    void sessionConfig_enableConfigDiscoveryValue() {
        var cfg = new CreateSessionOptions();
        assertTrue(cfg.getEnableConfigDiscovery().isEmpty());

        cfg.setEnableConfigDiscovery(true);
        assertTrue(cfg.getEnableConfigDiscovery().get());

        cfg.setEnableConfigDiscovery(false);
        assertFalse(cfg.getEnableConfigDiscovery().get());
    }

    @Test
    void sessionConfig_includeSubAgentStreamingEventsValue() {
        var cfg = new CreateSessionOptions();
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().isEmpty());

        cfg.setEvents(cfg.getEvents().setIncludeSubAgentStreamingEvents(true));
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().get());
    }

    @Test
    void sessionConfig_granularMultitenancyFieldsValue() {
        var cfg = new CreateSessionOptions();
        assertTrue(cfg.getState().getSkipEmbeddingRetrieval().isEmpty());
        assertNull(cfg.getInstructions().getOrganizationText());
        assertTrue(cfg.getInstructions().getOnDemandDiscovery().isEmpty());
        assertNull(cfg.getState().getEmbeddingCacheStorage());
        assertTrue(cfg.getHooks().getEnableRepositoryHooks().isEmpty());
        assertTrue(cfg.getGit().getEnableHostGitOperations().isEmpty());
        assertTrue(cfg.getState().getEnableCrossSessionStore().isEmpty());
        assertTrue(cfg.getSkills().getEnabled().isEmpty());

        cfg.setState(cfg.getState().setSkipEmbeddingRetrieval(true));
        cfg.setInstructions(cfg.getInstructions().setOrganizationText("Org instructions"));
        cfg.setInstructions(cfg.getInstructions().setOnDemandDiscovery(false));
        cfg.setState(cfg.getState().setEmbeddingCacheStorage("persistent"));
        cfg.setHooks(cfg.getHooks().setEnableRepositoryHooks(true));
        cfg.setGit(cfg.getGit().setEnableHostGitOperations(false));
        cfg.setState(cfg.getState().setEnableCrossSessionStore(true));
        cfg.setSkills(cfg.getSkills().setEnabled(false));

        assertTrue(cfg.getState().getSkipEmbeddingRetrieval().get());
        assertEquals("Org instructions", cfg.getInstructions().getOrganizationText());
        assertFalse(cfg.getInstructions().getOnDemandDiscovery().get());
        assertEquals("persistent", cfg.getState().getEmbeddingCacheStorage());
        assertTrue(cfg.getHooks().getEnableRepositoryHooks().get());
        assertFalse(cfg.getGit().getEnableHostGitOperations().get());
        assertTrue(cfg.getState().getEnableCrossSessionStore().get());
        assertFalse(cfg.getSkills().getEnabled().get());
    }

    @Test
    void resumeSessionConfig_enableSessionTelemetryValue() {
        var cfg = new ResumeSessionOptions();
        assertTrue(cfg.getGitHub().getTelemetryEnabled().isEmpty());

        cfg.setGitHub(cfg.getGitHub().setTelemetryEnabled(true));
        assertTrue(cfg.getGitHub().getTelemetryEnabled().get());

        cfg.setGitHub(cfg.getGitHub().setTelemetryEnabled(false));
        assertFalse(cfg.getGitHub().getTelemetryEnabled().get());
    }

    @Test
    void resumeSessionConfig_enableConfigDiscoveryValue() {
        var cfg = new ResumeSessionOptions();
        assertTrue(cfg.getEnableConfigDiscovery().isEmpty());

        cfg.setEnableConfigDiscovery(true);
        assertTrue(cfg.getEnableConfigDiscovery().get());
    }

    @Test
    void resumeSessionConfig_includeSubAgentStreamingEventsValue() {
        var cfg = new ResumeSessionOptions();
        assertTrue(cfg.getEvents().getIncludeSubAgentStreamingEvents().isEmpty());

        cfg.setEvents(cfg.getEvents().setIncludeSubAgentStreamingEvents(false));
        assertFalse(cfg.getEvents().getIncludeSubAgentStreamingEvents().get());
    }

    @Test
    void resumeSessionConfig_granularMultitenancyFieldsValue() {
        var cfg = new ResumeSessionOptions();
        assertTrue(cfg.getState().getSkipEmbeddingRetrieval().isEmpty());
        assertNull(cfg.getInstructions().getOrganizationText());
        assertTrue(cfg.getInstructions().getOnDemandDiscovery().isEmpty());
        assertNull(cfg.getState().getEmbeddingCacheStorage());
        assertTrue(cfg.getHooks().getEnableRepositoryHooks().isEmpty());
        assertTrue(cfg.getGit().getEnableHostGitOperations().isEmpty());
        assertTrue(cfg.getState().getEnableCrossSessionStore().isEmpty());
        assertTrue(cfg.getSkills().getEnabled().isEmpty());

        cfg.setState(cfg.getState().setSkipEmbeddingRetrieval(false));
        cfg.setInstructions(cfg.getInstructions().setOrganizationText("Resume org instructions"));
        cfg.setInstructions(cfg.getInstructions().setOnDemandDiscovery(true));
        cfg.setState(cfg.getState().setEmbeddingCacheStorage("persistent"));
        cfg.setHooks(cfg.getHooks().setEnableRepositoryHooks(false));
        cfg.setGit(cfg.getGit().setEnableHostGitOperations(true));
        cfg.setState(cfg.getState().setEnableCrossSessionStore(false));
        cfg.setSkills(cfg.getSkills().setEnabled(true));

        assertFalse(cfg.getState().getSkipEmbeddingRetrieval().get());
        assertEquals("Resume org instructions", cfg.getInstructions().getOrganizationText());
        assertTrue(cfg.getInstructions().getOnDemandDiscovery().get());
        assertEquals("persistent", cfg.getState().getEmbeddingCacheStorage());
        assertFalse(cfg.getHooks().getEnableRepositoryHooks().get());
        assertTrue(cfg.getGit().getEnableHostGitOperations().get());
        assertFalse(cfg.getState().getEnableCrossSessionStore().get());
        assertTrue(cfg.getSkills().getEnabled().get());
    }

    @Test
    void infiniteSessionConfig_thresholdValues() {
        var cfg = new InfiniteSessionConfig();
        assertTrue(cfg.getBackgroundCompactionThreshold().isEmpty());
        assertTrue(cfg.getBufferExhaustionThreshold().isEmpty());

        cfg.setBackgroundCompactionThreshold(0.6);
        cfg.setBufferExhaustionThreshold(0.85);
        assertEquals(0.6, cfg.getBackgroundCompactionThreshold().getAsDouble(), 0.001);
        assertEquals(0.85, cfg.getBufferExhaustionThreshold().getAsDouble(), 0.001);
    }

    @Test
    void infiniteSessionConfig_enabledValue() {
        var cfg = new InfiniteSessionConfig();
        assertTrue(cfg.getEnabled().isEmpty());

        cfg.setEnabled(true);
        assertTrue(cfg.getEnabled().get());

        cfg.setEnabled(false);
        assertFalse(cfg.getEnabled().get());
    }

    @Test
    void inputOptions_minAndMaxLengthValues() {
        var opts = new InputOptions();
        assertTrue(opts.getMinLength().isEmpty());
        assertTrue(opts.getMaxLength().isEmpty());

        opts.setMinLength(1);
        opts.setMaxLength(255);
        assertEquals(1, opts.getMinLength().getAsInt());
        assertEquals(255, opts.getMaxLength().getAsInt());
    }

    @Test
    void supports_visionAndReasoningEffortValues() {
        var s = new ModelCapabilitiesOverride.Supports();
        assertTrue(s.getVision().isEmpty());
        assertTrue(s.getReasoningEffort().isEmpty());

        s.setVision(true);
        s.setReasoningEffort(false);
        assertTrue(s.getVision().get());
        assertFalse(s.getReasoningEffort().get());
    }

    @Test
    void limits_tokenValues() {
        var l = new ModelCapabilitiesOverride.Limits();
        assertTrue(l.getMaxPromptTokens().isEmpty());
        assertTrue(l.getMaxOutputTokens().isEmpty());
        assertTrue(l.getMaxContextWindowTokens().isEmpty());

        l.setMaxPromptTokens(4096);
        l.setMaxOutputTokens(1024);
        l.setMaxContextWindowTokens(16384);
        assertEquals(4096, l.getMaxPromptTokens().getAsInt());
        assertEquals(1024, l.getMaxOutputTokens().getAsInt());
        assertEquals(16384, l.getMaxContextWindowTokens().getAsInt());
    }

    @Test
    void providerConfig_tokenValues() {
        var cfg = new ProviderConfig();
        assertTrue(cfg.getMaxPromptTokens().isEmpty());
        assertTrue(cfg.getMaxOutputTokens().isEmpty());

        cfg.setMaxPromptTokens(8192);
        cfg.setMaxOutputTokens(2048);
        assertEquals(8192, cfg.getMaxPromptTokens().getAsInt());
        assertEquals(2048, cfg.getMaxOutputTokens().getAsInt());
    }

    @Test
    void telemetryConfig_captureContentValue() {
        var cfg = new TelemetryConfig();
        assertTrue(cfg.getCaptureContent().isEmpty());

        cfg.setCaptureContent(true);
        assertTrue(cfg.getCaptureContent().get());

        cfg.setCaptureContent(false);
        assertFalse(cfg.getCaptureContent().get());
    }

    @Test
    void sessionUiCapabilities_elicitationValue() {
        var caps = new SessionUiCapabilities();
        assertTrue(caps.getElicitation().isEmpty());
        assertFalse(caps.getElicitation().orElse(false));

        caps.setElicitation(true);
        assertTrue(caps.getElicitation().orElse(false));
    }

    @Test
    void customAgentConfig_inferValue() {
        var cfg = new CustomAgentConfig();
        assertTrue(cfg.getInfer().isEmpty());

        cfg.setInfer(true);
        assertTrue(cfg.getInfer().get());

        cfg.setInfer(false);
        assertFalse(cfg.getInfer().get());
    }

    @Test
    void userInputRequest_allowFreeformValue() {
        var req = new UserInputRequest();
        assertTrue(req.getAllowFreeform().isEmpty());

        req.setAllowFreeform(true);
        assertTrue(req.getAllowFreeform().get());

        req.setAllowFreeform(false);
        assertFalse(req.getAllowFreeform().get());
    }

    // ── JSON deserialization into Optional-returning classes ───────────

    @Test
    void jackson_deserializeSupportsWithFields() throws Exception {
        String json = "{\"vision\":true,\"reasoningEffort\":false}";
        var supports = MAPPER.readValue(json, ModelCapabilitiesOverride.Supports.class);
        assertTrue(supports.getVision().get());
        assertFalse(supports.getReasoningEffort().get());
    }

    @Test
    void jackson_deserializeSupportsEmpty() throws Exception {
        String json = "{}";
        var supports = MAPPER.readValue(json, ModelCapabilitiesOverride.Supports.class);
        assertTrue(supports.getVision().isEmpty());
        assertTrue(supports.getReasoningEffort().isEmpty());
    }

    @Test
    void jackson_deserializeLimitsWithFields() throws Exception {
        String json = "{\"max_prompt_tokens\":4096,\"max_output_tokens\":1024,\"max_context_window_tokens\":16384}";
        var limits = MAPPER.readValue(json, ModelCapabilitiesOverride.Limits.class);
        assertEquals(4096, limits.getMaxPromptTokens().getAsInt());
        assertEquals(1024, limits.getMaxOutputTokens().getAsInt());
        assertEquals(16384, limits.getMaxContextWindowTokens().getAsInt());
    }

    @Test
    void jackson_deserializeLimitsEmpty() throws Exception {
        String json = "{}";
        var limits = MAPPER.readValue(json, ModelCapabilitiesOverride.Limits.class);
        assertTrue(limits.getMaxPromptTokens().isEmpty());
        assertTrue(limits.getMaxOutputTokens().isEmpty());
        assertTrue(limits.getMaxContextWindowTokens().isEmpty());
    }

    @Test
    void jackson_deserializeInfiniteSessionConfigWithFields() throws Exception {
        String json = "{\"enabled\":true,\"backgroundCompactionThreshold\":0.7,\"bufferExhaustionThreshold\":0.9}";
        var cfg = MAPPER.readValue(json, InfiniteSessionConfig.class);
        assertTrue(cfg.getEnabled().get());
        assertEquals(0.7, cfg.getBackgroundCompactionThreshold().getAsDouble(), 0.001);
        assertEquals(0.9, cfg.getBufferExhaustionThreshold().getAsDouble(), 0.001);
    }

    @Test
    void jackson_deserializeInfiniteSessionConfigEmpty() throws Exception {
        String json = "{}";
        var cfg = MAPPER.readValue(json, InfiniteSessionConfig.class);
        assertTrue(cfg.getEnabled().isEmpty());
        assertTrue(cfg.getBackgroundCompactionThreshold().isEmpty());
        assertTrue(cfg.getBufferExhaustionThreshold().isEmpty());
    }

    // ── Jackson serialization roundtrip ───────────────────────────────
    //
    // Classes whose fields carry @JsonProperty (InfiniteSessionConfig,
    // ModelCapabilitiesOverride inner classes) are serialized via field
    // access: Jackson writes the field when set and omits it when cleared.
    //
    // Classes without @JsonProperty on fields (CreateSessionOptions,
    // CopilotClientOptions, TelemetryConfig, ProviderConfig) are normally
    // copied to wire DTOs by SessionRequestBuilder. Their scalar getters can
    // still be serialized directly, while @JsonIgnore on Optional-returning
    // getters prevents Jackson from attempting to serialize Optional wrappers.

    @Test
    void jackson_sessionConfigEmbeddingCacheStorageSerialized() throws Exception {
        var cfg = new CreateSessionOptions();
        cfg.setState(cfg.getState().setEmbeddingCacheStorage("persistent"));

        String withField = MAPPER.writeValueAsString(cfg);
        assertTrue(withField.contains("\"embeddingCacheStorage\":\"persistent\""));

        cfg.getState().clearEmbeddingCacheStorage();

        String cleared = MAPPER.writeValueAsString(cfg);
        assertFalse(cleared.contains("embeddingCacheStorage"));
    }

    @Test
    void jackson_resumeSessionConfigEmbeddingCacheStorageSerialized() throws Exception {
        var cfg = new ResumeSessionOptions();
        cfg.setState(cfg.getState().setEmbeddingCacheStorage("persistent"));

        String withField = MAPPER.writeValueAsString(cfg);
        assertTrue(withField.contains("\"embeddingCacheStorage\":\"persistent\""));

        cfg.getState().clearEmbeddingCacheStorage();

        String cleared = MAPPER.writeValueAsString(cfg);
        assertFalse(cleared.contains("embeddingCacheStorage"));
    }

    @Test
    void jackson_infiniteSessionConfigClearedFieldsOmitted() throws Exception {
        var cfg = new InfiniteSessionConfig();
        cfg.setEnabled(true);
        cfg.setBackgroundCompactionThreshold(0.75);
        cfg.setBufferExhaustionThreshold(0.9);

        String withFields = MAPPER.writeValueAsString(cfg);
        assertTrue(withFields.contains("enabled"));
        assertTrue(withFields.contains("backgroundCompactionThreshold"));
        assertTrue(withFields.contains("bufferExhaustionThreshold"));

        cfg.clearEnabled();
        cfg.clearBackgroundCompactionThreshold();
        cfg.clearBufferExhaustionThreshold();

        String cleared = MAPPER.writeValueAsString(cfg);
        assertFalse(cleared.contains("enabled"));
        assertFalse(cleared.contains("backgroundCompactionThreshold"));
        assertFalse(cleared.contains("bufferExhaustionThreshold"));
    }

    @Test
    void jackson_modelCapabilitiesOverrideSupportsClearedFieldsOmitted() throws Exception {
        var supports = new ModelCapabilitiesOverride.Supports();
        supports.setVision(true);
        supports.setReasoningEffort(false);

        String withFields = MAPPER.writeValueAsString(supports);
        assertTrue(withFields.contains("vision"));
        assertTrue(withFields.contains("reasoningEffort"));

        supports.clearVision();
        supports.clearReasoningEffort();

        String cleared = MAPPER.writeValueAsString(supports);
        assertFalse(cleared.contains("vision"));
        assertFalse(cleared.contains("reasoningEffort"));
    }

    @Test
    void jackson_modelCapabilitiesOverrideLimitsClearedFieldsOmitted() throws Exception {
        var limits = new ModelCapabilitiesOverride.Limits();
        limits.setMaxPromptTokens(2048);
        limits.setMaxOutputTokens(512);
        limits.setMaxContextWindowTokens(16384);

        String withFields = MAPPER.writeValueAsString(limits);
        assertTrue(withFields.contains("max_prompt_tokens"));
        assertTrue(withFields.contains("max_output_tokens"));
        assertTrue(withFields.contains("max_context_window_tokens"));

        limits.clearMaxPromptTokens();
        limits.clearMaxOutputTokens();
        limits.clearMaxContextWindowTokens();

        String cleared = MAPPER.writeValueAsString(limits);
        assertFalse(cleared.contains("max_prompt_tokens"));
        assertFalse(cleared.contains("max_output_tokens"));
        assertFalse(cleared.contains("max_context_window_tokens"));
    }
}
