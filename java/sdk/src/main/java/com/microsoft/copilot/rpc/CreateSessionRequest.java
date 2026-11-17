/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import com.microsoft.copilot.generated.rpc.DiagnosticsConfiguration;

/**
 * Internal request object for creating a new session.
 * <p>
 * This is a low-level class for JSON-RPC communication. For creating sessions,
 * use
 * {@link com.microsoft.copilot.CopilotClient#createSession(CreateSessionOptions)}.
 *
 * @see com.microsoft.copilot.CopilotClient#createSession(CreateSessionOptions)
 * @see CreateSessionOptions
 * @since 1.0.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class CreateSessionRequest {

    @JsonProperty("model")
    private String model;

    @JsonProperty("sessionId")
    private String sessionId;

    @JsonProperty("name")
    private String name;

    @JsonProperty("clientName")
    private String clientName;

    @JsonProperty("systemMessage")
    private SystemMessageConfig systemMessage;

    @JsonProperty("workingDirectory")
    private String workingDirectory;

    @JsonProperty("diagnostics")
    private DiagnosticsConfiguration diagnostics;

    @JsonProperty("remoteSession")
    private String remoteSession;

    @JsonProperty("cloud")
    private CloudSessionOptions cloud;

    /** Gets the model name. @return the model */
    public String getModel() {
        return model;
    }

    /** Sets the model name. @param model the model */
    public void setModel(String model) {
        this.model = model;
    }

    /** Gets the session ID. @return the session ID */
    public String getSessionId() {
        return sessionId;
    }

    /** Sets the session ID. @param sessionId the session ID */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /** Gets the create-time session name. @return the session name */
    public String getName() {
        return name;
    }

    /** Sets the create-time session name. @param name the session name */
    public void setName(String name) {
        this.name = name;
    }

    /** Gets the client name. @return the client name */
    public String getClientName() {
        return clientName;
    }

    /** Sets the client name. @param clientName the client name */
    public void setClientName(String clientName) {
        this.clientName = clientName;
    }

    /** Gets the system message config. @return the config */
    public SystemMessageConfig getSystemMessage() {
        return systemMessage;
    }

    /** Sets the system message config. @param systemMessage the config */
    public void setSystemMessage(SystemMessageConfig systemMessage) {
        this.systemMessage = systemMessage;
    }

    /** Gets working directory. @return the working directory */
    public String getWorkingDirectory() {
        return workingDirectory;
    }

    /** Sets working directory. @param workingDirectory the working directory */
    public void setWorkingDirectory(String workingDirectory) {
        this.workingDirectory = workingDirectory;
    }

    /**
     * Gets the diagnostics configuration. @return the level, or {@code null} when
     * unset
     */
    public DiagnosticsConfiguration getDiagnostics() {
        return diagnostics;
    }

    /** Sets the diagnostics configuration. @param diagnostics the level */
    public void setDiagnostics(DiagnosticsConfiguration diagnostics) {
        this.diagnostics = diagnostics;
    }

    /** Gets the remote session mode. @return the remote session mode */
    public String getRemoteSession() {
        return remoteSession;
    }

    /**
     * Sets the remote session mode. @param remoteSession the remote session mode
     */
    public void setRemoteSession(String remoteSession) {
        this.remoteSession = remoteSession;
    }

    /** Gets the cloud session options. @return the cloud session options */
    public CloudSessionOptions getCloud() {
        return cloud;
    }

    /** Sets the cloud session options. @param cloud the cloud session options */
    public void setCloud(CloudSessionOptions cloud) {
        this.cloud = cloud;
    }

    @JsonProperty("inference")
    private SessionInferenceRequestOptions inference = new SessionInferenceRequestOptions();
    /**
     * Returns the inference options.
     *
     * @return the options
     */
    public SessionInferenceRequestOptions getInference() {
        return inference;
    }
    /**
     * Sets the inference options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setInference(SessionInferenceRequestOptions value) {
        inference = value == null ? new SessionInferenceRequestOptions() : value;
        return this;
    }

    @JsonProperty("interaction")
    private SessionInteractionRequestOptions interaction = new SessionInteractionRequestOptions();
    /**
     * Returns the interaction options.
     *
     * @return the options
     */
    public SessionInteractionRequestOptions getInteraction() {
        return interaction;
    }
    /**
     * Sets the interaction options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setInteraction(SessionInteractionRequestOptions value) {
        interaction = value == null ? new SessionInteractionRequestOptions() : value;
        return this;
    }

    @JsonProperty("tools")
    private SessionToolRequestOptions tools = new SessionToolRequestOptions();
    /**
     * Returns the tools options.
     *
     * @return the options
     */
    public SessionToolRequestOptions getTools() {
        return tools;
    }
    /**
     * Sets the tools options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setTools(SessionToolRequestOptions value) {
        tools = value == null ? new SessionToolRequestOptions() : value;
        return this;
    }

    @JsonProperty("agents")
    private SessionAgentRequestOptions agents = new SessionAgentRequestOptions();
    /**
     * Returns the agents options.
     *
     * @return the options
     */
    public SessionAgentRequestOptions getAgents() {
        return agents;
    }
    /**
     * Sets the agents options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setAgents(SessionAgentRequestOptions value) {
        agents = value == null ? new SessionAgentRequestOptions() : value;
        return this;
    }

    @JsonProperty("github")
    private SessionGitHubRequestOptions github = new SessionGitHubRequestOptions();
    /**
     * Returns the github options.
     *
     * @return the options
     */
    @JsonProperty("github")
    public SessionGitHubRequestOptions getGitHub() {
        return github;
    }
    /**
     * Sets the github options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setGitHub(SessionGitHubRequestOptions value) {
        github = value == null ? new SessionGitHubRequestOptions() : value;
        return this;
    }

    @JsonProperty("filesystem")
    private SessionFilesystemRequestOptions filesystem = new SessionFilesystemRequestOptions();
    /**
     * Returns the filesystem options.
     *
     * @return the options
     */
    public SessionFilesystemRequestOptions getFilesystem() {
        return filesystem;
    }
    /**
     * Sets the filesystem options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setFilesystem(SessionFilesystemRequestOptions value) {
        filesystem = value == null ? new SessionFilesystemRequestOptions() : value;
        return this;
    }

    @JsonProperty("execution")
    private SessionExecutionRequestOptions execution = new SessionExecutionRequestOptions();
    /**
     * Returns the execution options.
     *
     * @return the options
     */
    public SessionExecutionRequestOptions getExecution() {
        return execution;
    }
    /**
     * Sets the execution options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setExecution(SessionExecutionRequestOptions value) {
        execution = value == null ? new SessionExecutionRequestOptions() : value;
        return this;
    }

    @JsonProperty("permissions")
    private SessionPermissionRequestOptions permissions = new SessionPermissionRequestOptions();
    /**
     * Returns the permissions options.
     *
     * @return the options
     */
    public SessionPermissionRequestOptions getPermissions() {
        return permissions;
    }
    /**
     * Sets the permissions options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setPermissions(SessionPermissionRequestOptions value) {
        permissions = value == null ? new SessionPermissionRequestOptions() : value;
        return this;
    }

    @JsonProperty("hooks")
    private SessionHookRequestOptions hooks = new SessionHookRequestOptions();
    /**
     * Returns the hooks options.
     *
     * @return the options
     */
    public SessionHookRequestOptions getHooks() {
        return hooks;
    }
    /**
     * Sets the hooks options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setHooks(SessionHookRequestOptions value) {
        hooks = value == null ? new SessionHookRequestOptions() : value;
        return this;
    }

    @JsonProperty("events")
    private SessionEventRequestOptions events = new SessionEventRequestOptions();
    /**
     * Returns the events options.
     *
     * @return the options
     */
    public SessionEventRequestOptions getEvents() {
        return events;
    }
    /**
     * Sets the events options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setEvents(SessionEventRequestOptions value) {
        events = value == null ? new SessionEventRequestOptions() : value;
        return this;
    }

    @JsonProperty("mcp")
    private SessionMcpRequestOptions mcp = new SessionMcpRequestOptions();
    /**
     * Returns the mcp options.
     *
     * @return the options
     */
    public SessionMcpRequestOptions getMcp() {
        return mcp;
    }
    /**
     * Sets the mcp options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setMcp(SessionMcpRequestOptions value) {
        mcp = value == null ? new SessionMcpRequestOptions() : value;
        return this;
    }

    @JsonProperty("state")
    private SessionStateRequestOptions state = new SessionStateRequestOptions();
    /**
     * Returns the state options.
     *
     * @return the options
     */
    public SessionStateRequestOptions getState() {
        return state;
    }
    /**
     * Sets the state options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setState(SessionStateRequestOptions value) {
        state = value == null ? new SessionStateRequestOptions() : value;
        return this;
    }

    @JsonProperty("skills")
    private SessionSkillRequestOptions skills = new SessionSkillRequestOptions();
    /**
     * Returns the skills options.
     *
     * @return the options
     */
    public SessionSkillRequestOptions getSkills() {
        return skills;
    }
    /**
     * Sets the skills options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setSkills(SessionSkillRequestOptions value) {
        skills = value == null ? new SessionSkillRequestOptions() : value;
        return this;
    }

    @JsonProperty("instructions")
    private SessionInstructionRequestOptions instructions = new SessionInstructionRequestOptions();
    /**
     * Returns the instructions options.
     *
     * @return the options
     */
    public SessionInstructionRequestOptions getInstructions() {
        return instructions;
    }
    /**
     * Sets the instructions options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setInstructions(SessionInstructionRequestOptions value) {
        instructions = value == null ? new SessionInstructionRequestOptions() : value;
        return this;
    }

    @JsonProperty("plugins")
    private SessionPluginRequestOptions plugins = new SessionPluginRequestOptions();
    /**
     * Returns the plugins options.
     *
     * @return the options
     */
    public SessionPluginRequestOptions getPlugins() {
        return plugins;
    }
    /**
     * Sets the plugins options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setPlugins(SessionPluginRequestOptions value) {
        plugins = value == null ? new SessionPluginRequestOptions() : value;
        return this;
    }

    @JsonProperty("configDirectory")
    private String configDirectory;

    @JsonProperty("enableConfigDiscovery")
    private Boolean enableConfigDiscovery;

    /** Gets config configDirectory. @return the config configDirectory */
    public String getConfigDirectory() {
        return configDirectory;
    }

    /**
     * Sets config configDirectory. @param configDirectory the config
     * configDirectory
     */
    public CreateSessionRequest setConfigDirectory(String configDirectory) {
        this.configDirectory = configDirectory;
        return this;
    }

    /** Gets enable config discovery flag. @return the flag */
    public Boolean getEnableConfigDiscovery() {
        return enableConfigDiscovery;
    }

    /** Sets enable config discovery flag. @param enableConfigDiscovery the flag */
    public CreateSessionRequest setEnableConfigDiscovery(boolean enableConfigDiscovery) {
        this.enableConfigDiscovery = enableConfigDiscovery;
        return this;
    }

    /**
     * Clears the discovery setting, reverting to the default behavior.
     */
    public void clearEnableConfigDiscovery() {
        this.enableConfigDiscovery = null;
    }

    @JsonProperty("git")
    private SessionGitRequestOptions git = new SessionGitRequestOptions();
    /**
     * Returns the git options.
     *
     * @return the options
     */
    public SessionGitRequestOptions getGit() {
        return git;
    }
    /**
     * Sets the git options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setGit(SessionGitRequestOptions value) {
        git = value == null ? new SessionGitRequestOptions() : value;
        return this;
    }

    @JsonProperty("experiments")
    private SessionExperimentRequestOptions experiments = new SessionExperimentRequestOptions();
    /**
     * Returns the experiments options.
     *
     * @return the options
     */
    public SessionExperimentRequestOptions getExperiments() {
        return experiments;
    }
    /**
     * Sets the experiments options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setExperiments(SessionExperimentRequestOptions value) {
        experiments = value == null ? new SessionExperimentRequestOptions() : value;
        return this;
    }

    @JsonProperty("policy")
    private SessionPolicyRequestOptions policy = new SessionPolicyRequestOptions();
    /**
     * Returns the policy options.
     *
     * @return the options
     */
    public SessionPolicyRequestOptions getPolicy() {
        return policy;
    }
    /**
     * Sets the policy options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionRequest setPolicy(SessionPolicyRequestOptions value) {
        policy = value == null ? new SessionPolicyRequestOptions() : value;
        return this;
    }

}
