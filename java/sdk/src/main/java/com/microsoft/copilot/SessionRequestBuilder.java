/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CreateSessionRequest;
import com.microsoft.copilot.rpc.MemoryConfiguration;
import com.microsoft.copilot.rpc.NamedProviderConfig;
import com.microsoft.copilot.rpc.BearerTokenProvider;
import com.microsoft.copilot.rpc.CommandWireDefinition;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.ResumeSessionRequest;
import com.microsoft.copilot.rpc.SectionOverride;
import com.microsoft.copilot.rpc.SectionOverrideAction;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.StoredMemory;
import com.microsoft.copilot.rpc.SystemMessageConfig;
import com.microsoft.copilot.rpc.ToolDefinition;
import com.microsoft.copilot.rpc.ToolDefer;
import com.microsoft.copilot.rpc.ToolNamespace;
import com.microsoft.copilot.generated.rpc.ProtocolExternalToolDefer;
import com.microsoft.copilot.generated.rpc.ProtocolExternalToolDefinition;
import com.microsoft.copilot.generated.rpc.ProtocolExternalToolNamespace;

/**
 * Builds JSON-RPC request objects from session configuration.
 * <p>
 * This class handles the conversion of SDK configuration objects
 * ({@link CreateSessionOptions}, {@link ResumeSessionOptions}) to JSON-RPC
 * request objects for session creation and resumption.
 */
final class SessionRequestBuilder {

    private SessionRequestBuilder() {
        // Utility class
    }

    /**
     * Returns the app's store-memory callback, or {@code null} when none is set.
     *
     * @throws IllegalArgumentException
     *             if a callback is set but memory is not enabled
     */
    static Function<StoredMemory, CompletableFuture<Void>> requestedMemoryStore(MemoryConfiguration memory) {
        var storeMemory = memory != null ? memory.getStoreMemory() : null;
        if (storeMemory != null && !memory.getEnabled()) {
            throw new IllegalArgumentException("memory.storeMemory requires memory.enabled to be true.");
        }
        return storeMemory;
    }

    /**
     * Extracts transform callbacks from a {@link SystemMessageConfig} and returns a
     * wire-safe copy of the config alongside the extracted callbacks.
     * <p>
     * When the system message mode is {@link SystemMessageMode#CUSTOMIZE} and some
     * sections have {@link SectionOverride#getTransform() transform} callbacks set,
     * this method:
     * <ol>
     * <li>Removes the callbacks from the wire config (they must not be
     * serialized).</li>
     * <li>Replaces each transform section with
     * {@link SectionOverrideAction#TRANSFORM} in the wire config.</li>
     * <li>Returns the callbacks so they can be registered with the session.</li>
     * </ol>
     *
     * @param systemMessage
     *            the system message config, may be {@code null}
     * @return an {@link ExtractedTransforms} containing the wire-safe config and
     *         any extracted callbacks
     */
    static ExtractedTransforms extractTransformCallbacks(SystemMessageConfig systemMessage) {
        if (systemMessage == null || systemMessage.getMode() != SystemMessageMode.CUSTOMIZE
                || systemMessage.getSections() == null) {
            return new ExtractedTransforms(systemMessage, null);
        }

        Map<String, Function<String, CompletableFuture<String>>> callbacks = new HashMap<>();
        Map<String, SectionOverride> wireSections = new HashMap<>();

        for (Map.Entry<String, SectionOverride> entry : systemMessage.getSections().entrySet()) {
            String sectionId = entry.getKey();
            SectionOverride override = entry.getValue();

            if (override.getTransform() != null) {
                callbacks.put(sectionId, override.getTransform());
                wireSections.put(sectionId, new SectionOverride().setAction(SectionOverrideAction.TRANSFORM));
            } else {
                wireSections.put(sectionId, override);
            }
        }

        if (callbacks.isEmpty()) {
            return new ExtractedTransforms(systemMessage, null);
        }

        // Build a wire-safe copy of the system message with callbacks removed
        var wireConfig = new SystemMessageConfig().setMode(systemMessage.getMode())
                .setContent(systemMessage.getContent()).setSections(wireSections);

        return new ExtractedTransforms(wireConfig, callbacks);
    }

    static List<ProtocolExternalToolDefinition> toProtocolExternalTools(List<ToolDefinition> tools) {
        if (tools == null) {
            return null;
        }
        return tools.stream().map(SessionRequestBuilder::toProtocolExternalTool).toList();
    }

    static boolean requiresNullToolNameCompatibility(List<ToolDefinition> tools) {
        return tools != null && tools.stream()
                .anyMatch(tool -> tool.name() == null || tool.namespace() != null && tool.namespace().name() == null);
    }

    static ArrayNode toProtocolExternalToolsPreservingNullNames(List<ToolDefinition> tools) {
        ArrayNode result = JsonRpcClient.getObjectMapper().createArrayNode();
        for (ToolDefinition tool : tools) {
            ObjectNode wire = JsonRpcClient.getObjectMapper().valueToTree(toProtocolExternalTool(tool, true));
            if (tool.name() == null) {
                wire.remove("name");
            }
            if (tool.namespace() != null && tool.namespace().name() == null) {
                ((ObjectNode) wire.path("namespace")).remove("name");
            }
            result.add(wire);
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static ProtocolExternalToolDefinition toProtocolExternalTool(ToolDefinition tool) {
        return toProtocolExternalTool(tool, false);
    }

    @SuppressWarnings("unchecked")
    private static ProtocolExternalToolDefinition toProtocolExternalTool(ToolDefinition tool,
            boolean substituteRequiredNulls) {
        Map<String, Object> parameters = null;
        if (tool.parameters() instanceof Map<?, ?> map) {
            parameters = (Map<String, Object>) map;
        } else if (tool.parameters() != null) {
            parameters = JsonRpcClient.getObjectMapper().convertValue(tool.parameters(),
                    new TypeReference<Map<String, Object>>() {
                    });
        }
        return ProtocolExternalToolDefinition.builder()
                .name(tool.name() != null || !substituteRequiredNulls ? tool.name() : "")
                .description(tool.description() != null ? tool.description() : "").parameters(parameters)
                .overridesBuiltInTool(tool.overridesBuiltInTool()).skipPermission(tool.skipPermission())
                .hasHandler(tool.handler() != null).defer(toProtocolDefer(tool.defer())).isTerminal(tool.isTerminal())
                .metadata(tool.metadata()).namespace(toProtocolNamespace(tool.namespace(), substituteRequiredNulls))
                .idempotent(tool.idempotent()).build();
    }

    private static ProtocolExternalToolNamespace toProtocolNamespace(ToolNamespace namespace) {
        return toProtocolNamespace(namespace, false);
    }

    private static ProtocolExternalToolNamespace toProtocolNamespace(ToolNamespace namespace,
            boolean substituteRequiredNulls) {
        return namespace == null
                ? null
                : ProtocolExternalToolNamespace.builder()
                        .name(namespace.name() != null || !substituteRequiredNulls ? namespace.name() : "")
                        .description(namespace.description()).build();
    }

    private static ProtocolExternalToolDefer toProtocolDefer(ToolDefer defer) {
        if (defer == null || defer == ToolDefer.NONE) {
            return null;
        }
        return switch (defer) {
            case AUTO -> ProtocolExternalToolDefer.AUTO;
            case NEVER -> ProtocolExternalToolDefer.NEVER;
            case NONE -> null;
        };
    }

    /**
     * Builds a CreateSessionRequest from the given configuration.
     *
     * @param config
     *            the session configuration (may be null)
     * @param sessionId
     *            the pre-generated session ID to use
     * @return the built request object
     */
    static CreateSessionRequest buildCreateRequest(CreateSessionOptions config, String sessionId) {
        return buildCreateRequest(config, sessionId, CopilotClientMode.CODING_AGENT);
    }

    static CreateSessionRequest buildCreateRequest(CreateSessionOptions config, String sessionId,
            CopilotClientMode mode) {
        var request = new CreateSessionRequest();
        // Always request permission callbacks to enable deny-by-default behavior
        request.setPermissions(request.getPermissions().setRequest(true));
        // Always send envValueMode=direct for MCP servers
        request.setMcp(request.getMcp().setEnvValueMode("direct"));
        request.setSessionId(sessionId);
        if (config == null) {
            request.setAgents(request.getAgents().setLocalDiscoveryOnly(resolveCustomAgentsLocalOnly(null, mode)));
            return request;
        }

        request.setModel(config.getModel());
        request.setName(config.getName());
        request.setClientName(config.getClientName());
        request.setInference(request.getInference().setReasoningEffort(config.getInference().getReasoningEffort()));
        config.getInference().getAllowMultipleToolCalls().ifPresent(request.getInference()::setAllowMultipleToolCalls);
        request.setInference(request.getInference().setReasoningSummary(config.getInference().getReasoningSummary()));
        request.setInference(request.getInference().setContextTier(config.getInference().getContextTier()));
        request.setInteraction(request.getInteraction().setAskUserVariant(config.getInteraction().getAskUserVariant()));
        request.setTools(request.getTools().setDefinitions(config.getTools().getDefinitions()));
        request.setSystemMessage(config.getSystemMessage());
        request.setTools(request.getTools().setAvailableTools(config.getTools().getAvailableTools()));
        request.setTools(request.getTools().setExcludedTools(config.getTools().getExcludedTools()));
        request.setAgents(request.getAgents().setExcludedBuiltInAgents(config.getAgents().getExcludedBuiltInAgents()));
        request.setAgents(request.getAgents().setIncludedBuiltInAgents(config.getAgents().getIncludedBuiltInAgents()));
        if (config.getInference().hasDefaultProvider()) {
            request.getInference().setDefaultProvider(config.getInference().getDefaultProvider());
        }
        request.setGitHub(request.getGitHub().setCapi(config.getGitHub().getCapi()));
        request.setInference(request.getInference().setProviders(config.getInference().getProviders()));
        request.setInference(request.getInference().setModels(config.getInference().getModels()));
        config.getGitHub().getTelemetryEnabled().ifPresent(request.getGitHub()::setTelemetryEnabled);
        config.getInference().getEnableCitations().ifPresent(request.getInference()::setEnableCitations);
        config.getFilesystem().getTrackFileChanges().ifPresent(request.getFilesystem()::setTrackFileChanges);
        request.setExecution(request.getExecution().setLimits(config.getExecution().getLimits()));
        request.setExecution(request.getExecution().setAutopilot(config.getExecution().getAutopilot()));
        experimentalModeForMode(mode, config.getExperiments().getEnableExperimentalMode().orElse(null))
                .ifPresent(request.getExperiments()::setEnableExperimentalMode);
        if (config.getInteraction().getOnUserInputRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestUserInput(true));
        }
        if (config.getInteraction().getOnShellAuthenticationRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestShellAuthentication(true));
        }
        if (config.getHooks().getHandlers() != null && config.getHooks().getHandlers().hasHooks()) {
            request.setHooks(request.getHooks().setHasHandlers(true));
        }
        request.setWorkingDirectory(config.getWorkingDirectory());
        request.setFilesystem(
                request.getFilesystem().setAdditionalDirectories(config.getFilesystem().getAdditionalDirectories()));
        if (config.getEvents().isStreaming()) {
            request.setEvents(request.getEvents().setStreaming(true));
        }
        config.getEvents().getIncludeSubAgentStreamingEvents()
                .ifPresent(request.getEvents()::setIncludeSubAgentStreamingEvents);
        request.setMcp(request.getMcp().setServers(config.getMcp().getServers()));
        request.setDiagnostics(config.getDiagnostics());
        request.setMcp(request.getMcp().setOauthTokenStorage(config.getMcp().getOauthTokenStorage()));
        request.setMcp(request.getMcp().setAuthClientIdMetadataUrl(config.getMcp().getAuthClientIdMetadataUrl()));
        request.setAgents(request.getAgents().setDefinitions(config.getAgents().getDefinitions()));
        request.setAgents(request.getAgents().setLocalDiscoveryOnly(
                resolveCustomAgentsLocalOnly(config.getAgents().getLocalDiscoveryOnly().orElse(null), mode)));
        request.setAgents(request.getAgents().setDefault(config.getAgents().getDefault()));
        request.setAgents(request.getAgents().setSelected(config.getAgents().getSelected()));
        request.setState(request.getState().setInfiniteSessions(config.getState().getInfiniteSessions()));
        request.setSkills(request.getSkills().setDirectories(config.getSkills().getDirectories()));
        request.setAgents(request.getAgents().setDirectories(config.getAgents().getDirectories()));
        request.setHooks(request.getHooks().setDirectories(config.getHooks().getDirectories()));
        request.setInstructions(request.getInstructions().setDirectories(config.getInstructions().getDirectories()));
        request.setPlugins(request.getPlugins().setDirectories(config.getPlugins().getDirectories()));
        request.setTools(request.getTools().setLargeOutput(config.getTools().getLargeOutput()));
        request.setTools(request.getTools().setSearch(config.getTools().getSearch()));
        request.setTools(request.getTools().setImageGeneration(config.getTools().getImageGeneration()));
        request.setState(request.getState().setMemory(config.getState().getMemory()));
        request.setSkills(request.getSkills().setDisabledSkills(config.getSkills().getDisabledSkills()));
        request.setMcp(request.getMcp().setDisabledServers(config.getMcp().getDisabledServers()));
        request.setPermissions(request.getPermissions().setSandbox(config.getPermissions().getSandbox()));
        request.setConfigDirectory(config.getConfigDirectory());
        config.getEnableConfigDiscovery().ifPresent(request::setEnableConfigDiscovery);
        config.getState().getSkipEmbeddingRetrieval().ifPresent(request.getState()::setSkipEmbeddingRetrieval);
        if (config.getInstructions().getOrganizationText() != null) {
            request.setInstructions(
                    request.getInstructions().setOrganizationText(config.getInstructions().getOrganizationText()));
        }
        config.getInstructions().getOnDemandDiscovery().ifPresent(request.getInstructions()::setOnDemandDiscovery);
        config.getInstructions().getRefreshCustomInstructions()
                .ifPresent(request.getInstructions()::setRefreshCustomInstructions);
        request.setFilesystem(
                request.getFilesystem().setTrustWorkingDirectory(config.getFilesystem().getTrustWorkingDirectory()));
        config.getHooks().getEnableRepositoryHooks().ifPresent(request.getHooks()::setEnableRepositoryHooks);
        config.getGit().getEnableHostGitOperations().ifPresent(request.getGit()::setEnableHostGitOperations);
        config.getState().getEnableCrossSessionStore().ifPresent(request.getState()::setEnableCrossSessionStore);
        config.getSkills().getEnabled().ifPresent(request.getSkills()::setEnabled);
        if (config.getSkills().getProvider() != null) {
            request.setSkills(request.getSkills().setHasProvider(true));
        }
        if (config.getState().getMemory() != null && config.getState().getMemory().getStoreMemory() != null) {
            request.setState(request.getState().setHasMemoryStore(true));
        }
        if (config.getState().getEmbeddingCacheStorage() != null) {
            request.setState(request.getState().setEmbeddingCacheStorage(config.getState().getEmbeddingCacheStorage()));
        }
        request.setInference(request.getInference().setCapabilities(config.getInference().getCapabilities()));

        if (config.getInteraction().getCommands() != null) {
            var wireCommands = config.getInteraction().getCommands().stream()
                    .map(c -> new CommandWireDefinition(c.getName(), c.getDescription()))
                    .collect(java.util.stream.Collectors.toList());
            request.setInteraction(request.getInteraction().setCommands(wireCommands));
        }
        if (config.getInteraction().getOnElicitationRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestElicitation(true));
        }
        if (config.getMcp().isEnableApps()) {
            request.setMcp(request.getMcp().setEnableApps(true));
        }
        request.setGitHub(request.getGitHub().setMcp(config.getGitHub().getMcp()));
        if (config.getInteraction().getOnExitPlanModeRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestExitPlanMode(true));
        }
        if (config.getInteraction().getOnAutoModeSwitchRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestAutoModeSwitch(true));
        }
        if (config.getInteraction().isAutoModeRecovery()) {
            request.setInteraction(request.getInteraction().setRequestAutoModeRecovery(true));
        }
        request.setGitHub(request.getGitHub().setToken(config.getGitHub().getToken()));
        request.setCloud(config.getCloud());
        request.setExperiments(request.getExperiments().setFlags(config.getExperiments().getFlags()));
        request.setExperiments(request.getExperiments().setAssignments(config.getExperiments().getAssignments()));
        config.getGitHub().getFetchManagedSettings().ifPresent(request.getGitHub()::setFetchManagedSettings);
        request.setPolicy(
                request.getPolicy().setExcludedManagedSettings(config.getPolicy().getExcludedManagedSettings()));
        config.getPolicy().getEnforceManagedModelDefaults()
                .ifPresent(request.getPolicy()::setEnforceManagedModelDefaults);
        config.getGitHub().getInteractiveUser().ifPresent(request.getGitHub()::setInteractiveUser);
        config.getPermissions().getMode().ifPresent(request.getPermissions()::setMode);
        request.setPolicy(request.getPolicy().setManagedSettings(config.getPolicy().getManagedSettings()));

        return request;
    }

    /**
     * Builds a ResumeSessionRequest from the given session ID and configuration.
     *
     * @param sessionId
     *            the ID of the session to resume
     * @param config
     *            the resume configuration (may be null)
     * @return the built request object
     */
    static ResumeSessionRequest buildResumeRequest(String sessionId, ResumeSessionOptions config) {
        return buildResumeRequest(sessionId, config, CopilotClientMode.CODING_AGENT);
    }

    static ResumeSessionRequest buildResumeRequest(String sessionId, ResumeSessionOptions config,
            CopilotClientMode mode) {
        var request = new ResumeSessionRequest();
        request.setSessionId(sessionId);
        // Always request permission callbacks to enable deny-by-default behavior
        request.setPermissions(request.getPermissions().setRequest(true));
        // Always send envValueMode=direct for MCP servers
        request.setMcp(request.getMcp().setEnvValueMode("direct"));

        if (config == null) {
            request.setAgents(request.getAgents().setLocalDiscoveryOnly(resolveCustomAgentsLocalOnly(null, mode)));
            return request;
        }

        request.setModel(config.getModel());
        config.getContinuePendingWork().ifPresent(request::setContinuePendingWork);
        config.getAllowTranscriptRecovery().ifPresent(request::setAllowTranscriptRecovery);
        request.setClientName(config.getClientName());
        request.setInference(request.getInference().setReasoningEffort(config.getInference().getReasoningEffort()));
        config.getInference().getAllowMultipleToolCalls().ifPresent(request.getInference()::setAllowMultipleToolCalls);
        request.setInference(request.getInference().setReasoningSummary(config.getInference().getReasoningSummary()));
        request.setInference(request.getInference().setContextTier(config.getInference().getContextTier()));
        request.setInteraction(request.getInteraction().setAskUserVariant(config.getInteraction().getAskUserVariant()));
        request.setTools(request.getTools().setDefinitions(config.getTools().getDefinitions()));
        request.setSystemMessage(config.getSystemMessage());
        request.setTools(request.getTools().setAvailableTools(config.getTools().getAvailableTools()));
        request.setTools(request.getTools().setExcludedTools(config.getTools().getExcludedTools()));
        request.setAgents(request.getAgents().setExcludedBuiltInAgents(config.getAgents().getExcludedBuiltInAgents()));
        request.setAgents(request.getAgents().setIncludedBuiltInAgents(config.getAgents().getIncludedBuiltInAgents()));
        if (config.getInference().hasDefaultProvider()) {
            request.getInference().setDefaultProvider(config.getInference().getDefaultProvider());
        }
        request.setGitHub(request.getGitHub().setCapi(config.getGitHub().getCapi()));
        request.setInference(request.getInference().setProviders(config.getInference().getProviders()));
        request.setInference(request.getInference().setModels(config.getInference().getModels()));
        config.getGitHub().getTelemetryEnabled().ifPresent(request.getGitHub()::setTelemetryEnabled);
        config.getInference().getEnableCitations().ifPresent(request.getInference()::setEnableCitations);
        config.getFilesystem().getTrackFileChanges().ifPresent(request.getFilesystem()::setTrackFileChanges);
        request.setExecution(request.getExecution().setLimits(config.getExecution().getLimits()));
        request.setExecution(request.getExecution().setAutopilot(config.getExecution().getAutopilot()));
        experimentalModeForMode(mode, config.getExperiments().getEnableExperimentalMode().orElse(null))
                .ifPresent(request.getExperiments()::setEnableExperimentalMode);
        if (config.getInteraction().getOnUserInputRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestUserInput(true));
        }
        if (config.getInteraction().getOnShellAuthenticationRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestShellAuthentication(true));
        }
        if (config.getHooks().getHandlers() != null && config.getHooks().getHandlers().hasHooks()) {
            request.setHooks(request.getHooks().setHasHandlers(true));
        }
        request.setWorkingDirectory(config.getWorkingDirectory());
        request.setFilesystem(
                request.getFilesystem().setAdditionalDirectories(config.getFilesystem().getAdditionalDirectories()));
        request.setConfigDirectory(config.getConfigDirectory());
        config.getEnableConfigDiscovery().ifPresent(request::setEnableConfigDiscovery);
        config.getState().getSkipEmbeddingRetrieval().ifPresent(request.getState()::setSkipEmbeddingRetrieval);
        if (config.getInstructions().getOrganizationText() != null) {
            request.setInstructions(
                    request.getInstructions().setOrganizationText(config.getInstructions().getOrganizationText()));
        }
        config.getInstructions().getOnDemandDiscovery().ifPresent(request.getInstructions()::setOnDemandDiscovery);
        config.getHooks().getEnableRepositoryHooks().ifPresent(request.getHooks()::setEnableRepositoryHooks);
        config.getGit().getEnableHostGitOperations().ifPresent(request.getGit()::setEnableHostGitOperations);
        config.getState().getEnableCrossSessionStore().ifPresent(request.getState()::setEnableCrossSessionStore);
        config.getSkills().getEnabled().ifPresent(request.getSkills()::setEnabled);
        if (config.getSkills().getProvider() != null) {
            request.setSkills(request.getSkills().setHasProvider(true));
        }
        if (config.getState().getMemory() != null && config.getState().getMemory().getStoreMemory() != null) {
            request.setState(request.getState().setHasMemoryStore(true));
        }
        if (config.getState().getEmbeddingCacheStorage() != null) {
            request.setState(request.getState().setEmbeddingCacheStorage(config.getState().getEmbeddingCacheStorage()));
        }
        if (config.isSuppressResumeEvent()) {
            request.setSuppressResumeEvent(true);
        }
        if (config.getEvents().isStreaming()) {
            request.setEvents(request.getEvents().setStreaming(true));
        }
        config.getEvents().getIncludeSubAgentStreamingEvents()
                .ifPresent(request.getEvents()::setIncludeSubAgentStreamingEvents);
        request.setMcp(request.getMcp().setServers(config.getMcp().getServers()));
        request.setDiagnostics(config.getDiagnostics());
        request.setMcp(request.getMcp().setOauthTokenStorage(config.getMcp().getOauthTokenStorage()));
        request.setMcp(request.getMcp().setAuthClientIdMetadataUrl(config.getMcp().getAuthClientIdMetadataUrl()));
        request.setAgents(request.getAgents().setDefinitions(config.getAgents().getDefinitions()));
        request.setAgents(request.getAgents().setLocalDiscoveryOnly(
                resolveCustomAgentsLocalOnly(config.getAgents().getLocalDiscoveryOnly().orElse(null), mode)));
        request.setAgents(request.getAgents().setDefault(config.getAgents().getDefault()));
        request.setAgents(request.getAgents().setSelected(config.getAgents().getSelected()));
        request.setSkills(request.getSkills().setDirectories(config.getSkills().getDirectories()));
        request.setAgents(request.getAgents().setDirectories(config.getAgents().getDirectories()));
        request.setHooks(request.getHooks().setDirectories(config.getHooks().getDirectories()));
        request.setInstructions(request.getInstructions().setDirectories(config.getInstructions().getDirectories()));
        request.setPlugins(request.getPlugins().setDirectories(config.getPlugins().getDirectories()));
        request.setTools(request.getTools().setLargeOutput(config.getTools().getLargeOutput()));
        request.setTools(request.getTools().setSearch(config.getTools().getSearch()));
        request.setTools(request.getTools().setImageGeneration(config.getTools().getImageGeneration()));
        request.setState(request.getState().setMemory(config.getState().getMemory()));
        request.setSkills(request.getSkills().setDisabledSkills(config.getSkills().getDisabledSkills()));
        request.setMcp(request.getMcp().setDisabledServers(config.getMcp().getDisabledServers()));
        request.setPermissions(request.getPermissions().setSandbox(config.getPermissions().getSandbox()));
        request.setState(request.getState().setInfiniteSessions(config.getState().getInfiniteSessions()));
        request.setInference(request.getInference().setCapabilities(config.getInference().getCapabilities()));

        if (config.getInteraction().getCommands() != null) {
            var wireCommands = config.getInteraction().getCommands().stream()
                    .map(c -> new CommandWireDefinition(c.getName(), c.getDescription()))
                    .collect(java.util.stream.Collectors.toList());
            request.setInteraction(request.getInteraction().setCommands(wireCommands));
        }
        if (config.getInteraction().getOnElicitationRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestElicitation(true));
        }
        if (config.getMcp().isEnableApps()) {
            request.setMcp(request.getMcp().setEnableApps(true));
        }
        request.setGitHub(request.getGitHub().setMcp(config.getGitHub().getMcp()));
        if (config.getInteraction().getOnExitPlanModeRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestExitPlanMode(true));
        }
        if (config.getInteraction().getOnAutoModeSwitchRequest() != null) {
            request.setInteraction(request.getInteraction().setRequestAutoModeSwitch(true));
        }
        if (config.getInteraction().isAutoModeRecovery()) {
            request.setInteraction(request.getInteraction().setRequestAutoModeRecovery(true));
        }
        request.setGitHub(request.getGitHub().setToken(config.getGitHub().getToken()));
        request.setExperiments(request.getExperiments().setFlags(config.getExperiments().getFlags()));
        request.setExperiments(request.getExperiments().setAssignments(config.getExperiments().getAssignments()));
        config.getGitHub().getFetchManagedSettings().ifPresent(request.getGitHub()::setFetchManagedSettings);
        request.setPolicy(
                request.getPolicy().setExcludedManagedSettings(config.getPolicy().getExcludedManagedSettings()));
        config.getPolicy().getEnforceManagedModelDefaults()
                .ifPresent(request.getPolicy()::setEnforceManagedModelDefaults);
        config.getGitHub().getInteractiveUser().ifPresent(request.getGitHub()::setInteractiveUser);
        config.getPermissions().getMode().ifPresent(request.getPermissions()::setMode);
        request.setPolicy(request.getPolicy().setManagedSettings(config.getPolicy().getManagedSettings()));

        return request;
    }

    private static Boolean resolveCustomAgentsLocalOnly(Boolean customAgentsLocalOnly, CopilotClientMode mode) {
        if (customAgentsLocalOnly != null) {
            return customAgentsLocalOnly;
        }
        return mode == CopilotClientMode.EMPTY ? true : null;
    }

    private static Optional<Boolean> experimentalModeForMode(CopilotClientMode mode, Boolean supplied) {
        if (mode == CopilotClientMode.EMPTY) {
            return Optional.of(supplied != null ? supplied : false);
        }
        return Optional.ofNullable(supplied);
    }

    /**
     * Configures a session with handlers from the given config.
     *
     * @param session
     *            the session to configure
     * @param config
     *            the session configuration
     */
    static void configureSession(CopilotSession session, CreateSessionOptions config) {
        if (config == null) {
            return;
        }

        if (config.getTools().getDefinitions() != null) {
            session.registerTools(config.getTools().getDefinitions());
        }
        if (config.getPermissions().getOnRequest() != null) {
            session.registerPermissionHandler(config.getPermissions().getOnRequest());
        }
        session.setManagedSettingsEnabled(config.getGitHub().getFetchManagedSettings().orElse(false)
                || config.getPolicy().getManagedSettings() != null);
        if (config.getMcp().getOnAuthRequest() != null) {
            session.registerMcpAuthHandler(config.getMcp().getOnAuthRequest());
        }
        if (config.getInteraction().getOnUserInputRequest() != null) {
            session.registerUserInputHandler(config.getInteraction().getOnUserInputRequest());
        }
        if (config.getInteraction().getOnShellAuthenticationRequest() != null) {
            session.registerShellAuthenticationHandler(config.getInteraction().getOnShellAuthenticationRequest());
        }
        if (config.getSkills().getProvider() != null) {
            session.registerSkillProvider(config.getSkills().getProvider());
        }
        var memoryStore = config.getState().getMemory() != null ? config.getState().getMemory().getStoreMemory() : null;
        if (memoryStore != null) {
            session.registerMemoryStore(memoryStore);
        }
        if (config.getHooks().getHandlers() != null) {
            session.registerHooks(config.getHooks().getHandlers());
        }
        if (config.getInteraction().getCommands() != null) {
            session.registerCommands(config.getInteraction().getCommands());
        }
        if (config.getInteraction().getOnElicitationRequest() != null) {
            session.registerElicitationHandler(config.getInteraction().getOnElicitationRequest());
        }
        Map<String, BearerTokenProvider> bearerTokenProviders = collectBearerTokenProviders(
                config.getInference().getProviders());
        if (!bearerTokenProviders.isEmpty()) {
            session.registerBearerTokenProviders(bearerTokenProviders);
        }
        if (config.getInteraction().getOnExitPlanModeRequest() != null) {
            session.registerExitPlanModeHandler(config.getInteraction().getOnExitPlanModeRequest());
        }
        if (config.getInteraction().getOnAutoModeSwitchRequest() != null) {
            session.registerAutoModeSwitchHandler(config.getInteraction().getOnAutoModeSwitchRequest());
        }
        if (config.getEvents().getOnEvent() != null) {
            session.on(config.getEvents().getOnEvent());
        }
    }

    /**
     * Configures a resumed session with handlers from the given config.
     *
     * @param session
     *            the session to configure
     * @param config
     *            the resume session configuration
     */
    static void configureSession(CopilotSession session, ResumeSessionOptions config) {
        if (config == null) {
            return;
        }

        if (config.getTools().getDefinitions() != null) {
            session.registerTools(config.getTools().getDefinitions());
        }
        if (config.getPermissions().getOnRequest() != null) {
            session.registerPermissionHandler(config.getPermissions().getOnRequest());
        }
        session.setManagedSettingsEnabled(config.getGitHub().getFetchManagedSettings().orElse(false)
                || config.getPolicy().getManagedSettings() != null);
        if (config.getMcp().getOnAuthRequest() != null) {
            session.registerMcpAuthHandler(config.getMcp().getOnAuthRequest());
        }
        if (config.getInteraction().getOnUserInputRequest() != null) {
            session.registerUserInputHandler(config.getInteraction().getOnUserInputRequest());
        }
        if (config.getInteraction().getOnShellAuthenticationRequest() != null) {
            session.registerShellAuthenticationHandler(config.getInteraction().getOnShellAuthenticationRequest());
        }
        if (config.getSkills().getProvider() != null) {
            session.registerSkillProvider(config.getSkills().getProvider());
        }
        var memoryStore = config.getState().getMemory() != null ? config.getState().getMemory().getStoreMemory() : null;
        if (memoryStore != null) {
            session.registerMemoryStore(memoryStore);
        }
        if (config.getHooks().getHandlers() != null) {
            session.registerHooks(config.getHooks().getHandlers());
        }
        if (config.getInteraction().getCommands() != null) {
            session.registerCommands(config.getInteraction().getCommands());
        }
        if (config.getInteraction().getOnElicitationRequest() != null) {
            session.registerElicitationHandler(config.getInteraction().getOnElicitationRequest());
        }
        Map<String, BearerTokenProvider> bearerTokenProviders = collectBearerTokenProviders(
                config.getInference().getProviders());
        if (!bearerTokenProviders.isEmpty()) {
            session.registerBearerTokenProviders(bearerTokenProviders);
        }
        if (config.getInteraction().getOnExitPlanModeRequest() != null) {
            session.registerExitPlanModeHandler(config.getInteraction().getOnExitPlanModeRequest());
        }
        if (config.getInteraction().getOnAutoModeSwitchRequest() != null) {
            session.registerAutoModeSwitchHandler(config.getInteraction().getOnAutoModeSwitchRequest());
        }
        if (config.getEvents().getOnEvent() != null) {
            session.on(config.getEvents().getOnEvent());
        }
    }

    private static Map<String, BearerTokenProvider> collectBearerTokenProviders(List<NamedProviderConfig> providers) {
        Map<String, BearerTokenProvider> bearerTokenProviders = new HashMap<>();
        if (providers != null) {
            for (NamedProviderConfig namedProvider : providers) {
                if (namedProvider != null && namedProvider.getName() != null
                        && namedProvider.getBearerTokenProvider() != null) {
                    bearerTokenProviders.put(namedProvider.getName(), namedProvider.getBearerTokenProvider());
                }
            }
        }
        return bearerTokenProviders;
    }
}
