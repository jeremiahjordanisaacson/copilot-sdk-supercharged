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
}
