/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot;

import com.github.copilot.Types.*;
import java.util.List;
import java.util.Map;

/**
 * Configuration for creating a session.
 */
public class SessionConfig {
    private String sessionId;
    private String model;
    private String reasoningEffort;
    private String configDir;
    private List<Tool> tools;
    private SystemMessageConfig systemMessage;
    private List<String> availableTools;
    private List<String> excludedTools;
    private ProviderConfig provider;
    private PermissionHandler onPermissionRequest;
    private UserInputHandler onUserInputRequest;
    private SessionHooks hooks;
    private String workingDirectory;
    private Boolean streaming;
    /** Include sub-agent streaming events in the event stream. Defaults to true. */
    private Boolean includeSubAgentStreamingEvents;
    private Map<String, MCPServerConfig> mcpServers;
    private List<CustomAgentConfig> customAgents;
    private List<String> skillDirectories;
    private List<String> disabledSkills;
    private InfiniteSessionConfig infiniteSessions;
    /** Per-property overrides for model capabilities, deep-merged over runtime defaults. */
    private Map<String, Object> modelCapabilities;
    /** When true, automatically discovers MCP server configurations from the working directory. Defaults to false. */
    private Boolean enableConfigDiscovery;
    /** GitHub token for authentication. When set on session config, overrides the client-level token for this session only. */
    private String gitHubToken;
    /** Slash commands registered for this session. */
    private List<CommandDefinition> commands;
    /** Handler for elicitation requests from the server. */
    private ElicitationHandler onElicitationRequest;
    /** Per-session auth token. When set, overrides the client-level token for this session only. Alias for gitHubToken. */
    private String authToken;
    /** Additional HTTP headers sent with each model request. */
    private Map<String, String> requestHeaders;
    /** Response format for image generation ("text", "image", "json_object"). */
    private String responseFormat;
    /** Custom instruction directory paths. */
    private List<String> instructionDirectories;
    /** Enable inline source citations in assistant responses. */
    private Boolean enableCitations;
    /** Built-in agent names to exclude from this session. */
    private List<String> excludedBuiltinAgents;
    /** Per-session AI-credit spending limits. */
    private SessionLimitsConfig sessionLimits;
    /** Opt-in persistent session memory configuration. */
    private MemoryConfiguration memory;
    /** OTLP telemetry export protocol ("grpc" or "http/protobuf"). */
    private String otlpProtocol;
    /** Stream model responses over a WebSocket transport when true. */
    private Boolean enableWebSocketResponses;
    /** Experiment assignment overrides forwarded to the runtime. */
    private Map<String, Object> expAssignments;
    /** Handler invoked when an MCP server requests an OAuth host token. */
    private McpAuthHandler onMcpAuthRequest;
    /** Enable session rewind to restore earlier session points. */
    private Boolean rewindEnabled;
    /** Extra working directories available to the session. */
    private List<String> additionalDirectories;
    /** Names of configured MCP servers to disable for this session. */
    private List<String> disabledMcpServers;
    /** Configuration for the built-in GitHub MCP tools. */
    private Map<String, Object> githubMcpToolConfig;
    /** Identity of the canvas provider to register. */
    private Map<String, Object> canvasProvider;
    /** Only load workspace-local custom agents. */
    private Boolean customAgentsLocalOnly;
    /** Tool search configuration for large tool sets. */
    private Map<String, Object> toolSearch;
    /** Enable experimental runtime features. */
    private Boolean experimentalMode;
    /** Respect repository content-exclusion rules. */
    private Boolean contentExclusion;
    /** Auto-model routing preference ({@code "efficiency"}, {@code "balance"}, {@code "intelligence"}, {@code "fast"}); sent under {@code capi}. Experimental. */
    private String autoTier;
    /** Model-facing shape of the built-in ask_user tool ({@code "legacy"} or {@code "elicitation"}). */
    private String askUserVariant;
    /** Acquires short-lived GitHub tokens for this session on demand. Mutually exclusive with gitHubToken. */
    private GitHubTokenProvider gitHubTokenProvider;
    /** Sandbox configuration applied before MCP servers or session tools start. */
    private SandboxConfig sandboxConfig;
    /** Host-resolved feature-flag values; sent whenever provided, even if empty. */
    private Map<String, Boolean> featureFlags;
    /** Exact model IDs allowed by the host. */
    private List<String> allowedModels;
    /** OAuth Client ID Metadata Document URL identifying the host for MCP authorization. */
    private String authClientIdMetadataUrl;
    /** Session name applied at creation (create only). */
    private String name;
    /** Explicit discovery-only custom-agent roots. */
    private List<String> agentDirectories;
    /** Explicit discovery-only file-hook roots. */
    private List<String> hookDirectories;
    /** Declares that a human drives this session's conversation. */
    private Boolean interactiveUser;
    /** Managed-setting paths this host handles externally or does not support. */
    private List<String> excludedManagedSettings;
    /** Enforce locked managed model controls for this session. */
    private Boolean enforceManagedModelDefaults;
    /** Grants trust to the working directory at creation: {@code "session"} or {@code "user"} (create only). */
    private String trustWorkingDirectory;
    /** Invalidate the custom-instruction discovery cache before creating the session (create only). */
    private Boolean refreshCustomInstructions;
    /** Allow the runtime to recover a damaged transcript during resume (resume only). */
    private Boolean allowTranscriptRecovery;
    /** Session-level image generation opt-in. */
    private ImageGenerationConfig imageGeneration;
    /** Autopilot continuation policy. */
    private AutopilotOptions autopilot;
    /** Named BYOK provider connections ({@code inference.providers}). */
    private List<NamedProviderConfig> providers;
    /** BYOK model definitions served by the named providers ({@code inference.models}). */
    private List<ProviderModelConfig> models;
    /** Connection that serves unqualified model names ({@code inference.defaultProvider}). */
    private String defaultProvider;
    /** Whether {@link #defaultProvider(String)} was called, so an explicit null clears the default on resume. */
    private boolean defaultProviderSet;
    /** Copilot API (CAPI) options ({@code github.capi}); members win over autoTier/enableWebSocketResponses. */
    private CapiConfig capi;
    /** Answers secret, text, or confirmation prompts from programs run by shell tools. */
    private ShellAuthenticationHandler onShellAuthenticationRequest;
    /** Skip emitting the {@code session.resume} event (resume only). */
    private Boolean suppressResumeEvent;
    /** Continue tool calls and permission prompts pending when the session was suspended (resume only). */
    private Boolean continuePendingWork;
    /** Initial permission mode ({@code "manual"}, {@code "assisted"}, {@code "allow-all"}). */
    private String permissionMode;

    public SessionConfig() {}

    // Builder-style setters
    public SessionConfig sessionId(String sessionId) { this.sessionId = sessionId; return this; }
    public SessionConfig model(String model) { this.model = model; return this; }
    public SessionConfig reasoningEffort(String reasoningEffort) { this.reasoningEffort = reasoningEffort; return this; }
    public SessionConfig configDir(String configDir) { this.configDir = configDir; return this; }
    public SessionConfig tools(List<Tool> tools) { this.tools = tools; return this; }
    public SessionConfig systemMessage(SystemMessageConfig systemMessage) { this.systemMessage = systemMessage; return this; }
    public SessionConfig availableTools(List<String> availableTools) { this.availableTools = availableTools; return this; }
    public SessionConfig excludedTools(List<String> excludedTools) { this.excludedTools = excludedTools; return this; }
    public SessionConfig provider(ProviderConfig provider) { this.provider = provider; return this; }
    public SessionConfig onPermissionRequest(PermissionHandler handler) { this.onPermissionRequest = handler; return this; }
    public SessionConfig onUserInputRequest(UserInputHandler handler) { this.onUserInputRequest = handler; return this; }
    public SessionConfig hooks(SessionHooks hooks) { this.hooks = hooks; return this; }
    public SessionConfig workingDirectory(String dir) { this.workingDirectory = dir; return this; }
    public SessionConfig streaming(boolean streaming) { this.streaming = streaming; return this; }
    public SessionConfig includeSubAgentStreamingEvents(boolean include) { this.includeSubAgentStreamingEvents = include; return this; }
    public SessionConfig mcpServers(Map<String, MCPServerConfig> servers) { this.mcpServers = servers; return this; }
    public SessionConfig customAgents(List<CustomAgentConfig> agents) { this.customAgents = agents; return this; }
    public SessionConfig skillDirectories(List<String> dirs) { this.skillDirectories = dirs; return this; }
    public SessionConfig disabledSkills(List<String> skills) { this.disabledSkills = skills; return this; }
    public SessionConfig infiniteSessions(InfiniteSessionConfig config) { this.infiniteSessions = config; return this; }
    public SessionConfig modelCapabilities(Map<String, Object> capabilities) { this.modelCapabilities = capabilities; return this; }
    public SessionConfig enableConfigDiscovery(boolean enable) { this.enableConfigDiscovery = enable; return this; }
    public SessionConfig gitHubToken(String token) { this.gitHubToken = token; return this; }
    public SessionConfig commands(List<CommandDefinition> commands) { this.commands = commands; return this; }
    public SessionConfig onElicitationRequest(ElicitationHandler handler) { this.onElicitationRequest = handler; return this; }
    public SessionConfig authToken(String token) { this.authToken = token; return this; }
    public SessionConfig requestHeaders(Map<String, String> headers) { this.requestHeaders = headers; return this; }
    public SessionConfig responseFormat(String format) { this.responseFormat = format; return this; }
    public SessionConfig instructionDirectories(List<String> dirs) { this.instructionDirectories = dirs; return this; }
    public SessionConfig enableCitations(boolean enable) { this.enableCitations = enable; return this; }
    public SessionConfig excludedBuiltinAgents(List<String> agents) { this.excludedBuiltinAgents = agents; return this; }
    public SessionConfig sessionLimits(SessionLimitsConfig limits) { this.sessionLimits = limits; return this; }
    public SessionConfig memory(MemoryConfiguration memory) { this.memory = memory; return this; }
    public SessionConfig otlpProtocol(String protocol) { this.otlpProtocol = protocol; return this; }
    public SessionConfig enableWebSocketResponses(boolean enable) { this.enableWebSocketResponses = enable; return this; }
    public SessionConfig expAssignments(Map<String, Object> assignments) { this.expAssignments = assignments; return this; }
    public SessionConfig onMcpAuthRequest(McpAuthHandler handler) { this.onMcpAuthRequest = handler; return this; }
    public SessionConfig rewindEnabled(boolean enable) { this.rewindEnabled = enable; return this; }
    public SessionConfig additionalDirectories(List<String> dirs) { this.additionalDirectories = dirs; return this; }
    public SessionConfig disabledMcpServers(List<String> servers) { this.disabledMcpServers = servers; return this; }
    public SessionConfig githubMcpToolConfig(Map<String, Object> config) { this.githubMcpToolConfig = config; return this; }
    public SessionConfig canvasProvider(Map<String, Object> provider) { this.canvasProvider = provider; return this; }
    public SessionConfig customAgentsLocalOnly(boolean localOnly) { this.customAgentsLocalOnly = localOnly; return this; }
    public SessionConfig toolSearch(Map<String, Object> toolSearch) { this.toolSearch = toolSearch; return this; }
    public SessionConfig experimentalMode(boolean enable) { this.experimentalMode = enable; return this; }
    public SessionConfig contentExclusion(boolean enable) { this.contentExclusion = enable; return this; }
    public SessionConfig autoTier(String autoTier) { this.autoTier = autoTier; return this; }
    public SessionConfig askUserVariant(String variant) { this.askUserVariant = variant; return this; }
    public SessionConfig gitHubTokenProvider(GitHubTokenProvider provider) { this.gitHubTokenProvider = provider; return this; }
    public SessionConfig sandboxConfig(SandboxConfig sandboxConfig) { this.sandboxConfig = sandboxConfig; return this; }
    public SessionConfig featureFlags(Map<String, Boolean> flags) { this.featureFlags = flags; return this; }
    public SessionConfig allowedModels(List<String> models) { this.allowedModels = models; return this; }
    public SessionConfig authClientIdMetadataUrl(String url) { this.authClientIdMetadataUrl = url; return this; }
    public SessionConfig name(String name) { this.name = name; return this; }
    public SessionConfig agentDirectories(List<String> dirs) { this.agentDirectories = dirs; return this; }
    public SessionConfig hookDirectories(List<String> dirs) { this.hookDirectories = dirs; return this; }
    public SessionConfig interactiveUser(boolean interactive) { this.interactiveUser = interactive; return this; }
    public SessionConfig excludedManagedSettings(List<String> paths) { this.excludedManagedSettings = paths; return this; }
    public SessionConfig enforceManagedModelDefaults(boolean enforce) { this.enforceManagedModelDefaults = enforce; return this; }
    public SessionConfig trustWorkingDirectory(String trust) { this.trustWorkingDirectory = trust; return this; }
    public SessionConfig refreshCustomInstructions(boolean refresh) { this.refreshCustomInstructions = refresh; return this; }
    public SessionConfig allowTranscriptRecovery(boolean allow) { this.allowTranscriptRecovery = allow; return this; }
    public SessionConfig imageGeneration(ImageGenerationConfig config) { this.imageGeneration = config; return this; }
    public SessionConfig autopilot(AutopilotOptions options) { this.autopilot = options; return this; }
    public SessionConfig providers(List<NamedProviderConfig> providers) { this.providers = providers; return this; }
    public SessionConfig models(List<ProviderModelConfig> models) { this.models = models; return this; }
    /** Sets the default provider connection; passing null clears a previously set default on resume. */
    public SessionConfig defaultProvider(String name) { this.defaultProvider = name; this.defaultProviderSet = true; return this; }
    public SessionConfig capi(CapiConfig capi) { this.capi = capi; return this; }
    public SessionConfig onShellAuthenticationRequest(ShellAuthenticationHandler handler) { this.onShellAuthenticationRequest = handler; return this; }
    public SessionConfig suppressResumeEvent(boolean suppress) { this.suppressResumeEvent = suppress; return this; }
    public SessionConfig continuePendingWork(boolean continuePending) { this.continuePendingWork = continuePending; return this; }
    public SessionConfig permissionMode(String mode) { this.permissionMode = mode; return this; }

    // Getters
    public String getSessionId() { return sessionId; }
    public String getModel() { return model; }
    public String getReasoningEffort() { return reasoningEffort; }
    public String getConfigDir() { return configDir; }
    public List<Tool> getTools() { return tools; }
    public SystemMessageConfig getSystemMessage() { return systemMessage; }
    public List<String> getAvailableTools() { return availableTools; }
    public List<String> getExcludedTools() { return excludedTools; }
    public ProviderConfig getProvider() { return provider; }
    public PermissionHandler getOnPermissionRequest() { return onPermissionRequest; }
    public UserInputHandler getOnUserInputRequest() { return onUserInputRequest; }
    public SessionHooks getHooks() { return hooks; }
    public String getWorkingDirectory() { return workingDirectory; }
    public Boolean getStreaming() { return streaming; }
    public Boolean getIncludeSubAgentStreamingEvents() { return includeSubAgentStreamingEvents; }
    public Map<String, MCPServerConfig> getMcpServers() { return mcpServers; }
    public List<CustomAgentConfig> getCustomAgents() { return customAgents; }
    public List<String> getSkillDirectories() { return skillDirectories; }
    public List<String> getDisabledSkills() { return disabledSkills; }
    public InfiniteSessionConfig getInfiniteSessions() { return infiniteSessions; }
    public Map<String, Object> getModelCapabilities() { return modelCapabilities; }
    public Boolean getEnableConfigDiscovery() { return enableConfigDiscovery; }
    public String getGitHubToken() { return gitHubToken; }
    public List<CommandDefinition> getCommands() { return commands; }
    public ElicitationHandler getOnElicitationRequest() { return onElicitationRequest; }
    public String getAuthToken() { return authToken; }
    public Map<String, String> getRequestHeaders() { return requestHeaders; }
    public String getResponseFormat() { return responseFormat; }
    public List<String> getInstructionDirectories() { return instructionDirectories; }
    public Boolean getEnableCitations() { return enableCitations; }
    public List<String> getExcludedBuiltinAgents() { return excludedBuiltinAgents; }
    public SessionLimitsConfig getSessionLimits() { return sessionLimits; }
    public MemoryConfiguration getMemory() { return memory; }
    public String getOtlpProtocol() { return otlpProtocol; }
    public Boolean getEnableWebSocketResponses() { return enableWebSocketResponses; }
    public Map<String, Object> getExpAssignments() { return expAssignments; }
    public McpAuthHandler getOnMcpAuthRequest() { return onMcpAuthRequest; }
    public Boolean getRewindEnabled() { return rewindEnabled; }
    public List<String> getAdditionalDirectories() { return additionalDirectories; }
    public List<String> getDisabledMcpServers() { return disabledMcpServers; }
    public Map<String, Object> getGithubMcpToolConfig() { return githubMcpToolConfig; }
    public Map<String, Object> getCanvasProvider() { return canvasProvider; }
    public Boolean getCustomAgentsLocalOnly() { return customAgentsLocalOnly; }
    public Map<String, Object> getToolSearch() { return toolSearch; }
    public Boolean getExperimentalMode() { return experimentalMode; }
    public Boolean getContentExclusion() { return contentExclusion; }
    public String getAutoTier() { return autoTier; }
    public String getAskUserVariant() { return askUserVariant; }
    public GitHubTokenProvider getGitHubTokenProvider() { return gitHubTokenProvider; }
    public SandboxConfig getSandboxConfig() { return sandboxConfig; }
    public Map<String, Boolean> getFeatureFlags() { return featureFlags; }
    public List<String> getAllowedModels() { return allowedModels; }
    public String getAuthClientIdMetadataUrl() { return authClientIdMetadataUrl; }
    public String getName() { return name; }
    public List<String> getAgentDirectories() { return agentDirectories; }
    public List<String> getHookDirectories() { return hookDirectories; }
    public Boolean getInteractiveUser() { return interactiveUser; }
    public List<String> getExcludedManagedSettings() { return excludedManagedSettings; }
    public Boolean getEnforceManagedModelDefaults() { return enforceManagedModelDefaults; }
    public String getTrustWorkingDirectory() { return trustWorkingDirectory; }
    public Boolean getRefreshCustomInstructions() { return refreshCustomInstructions; }
    public Boolean getAllowTranscriptRecovery() { return allowTranscriptRecovery; }
    public ImageGenerationConfig getImageGeneration() { return imageGeneration; }
    public AutopilotOptions getAutopilot() { return autopilot; }
    public List<NamedProviderConfig> getProviders() { return providers; }
    public List<ProviderModelConfig> getModels() { return models; }
    public String getDefaultProvider() { return defaultProvider; }
    public boolean isDefaultProviderSet() { return defaultProviderSet; }
    public CapiConfig getCapi() { return capi; }
    public ShellAuthenticationHandler getOnShellAuthenticationRequest() { return onShellAuthenticationRequest; }
    public Boolean getSuppressResumeEvent() { return suppressResumeEvent; }
    public Boolean getContinuePendingWork() { return continuePendingWork; }
    public String getPermissionMode() { return permissionMode; }
}
