/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

import com.microsoft.copilot.generated.rpc.DiagnosticsConfiguration;

/**
 * Internal request object for resuming an existing session.
 * <p>
 * This is a low-level class for JSON-RPC communication. For resuming sessions,
 * use
 * {@link com.microsoft.copilot.CopilotClient#resumeSession(String, ResumeSessionOptions)}.
 *
 * @see com.microsoft.copilot.CopilotClient#resumeSession(String,
 *      ResumeSessionOptions)
 * @see ResumeSessionOptions
 * @since 1.0.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class ResumeSessionRequest {

    @JsonProperty("sessionId")
    private String sessionId;

    @JsonProperty("clientName")
    private String clientName;

    @JsonProperty("model")
    private String model;

    @JsonProperty("continuePendingWork")
    private Boolean continuePendingWork;

    @JsonProperty("allowTranscriptRecovery")
    private Boolean allowTranscriptRecovery;

    @JsonProperty("systemMessage")
    private SystemMessageConfig systemMessage;

    @JsonProperty("workingDirectory")
    private String workingDirectory;

    @JsonProperty("suppressResumeEvent")
    private Boolean suppressResumeEvent;

    @JsonProperty("diagnostics")
    private DiagnosticsConfiguration diagnostics;

    @JsonProperty("remoteSession")
    private String remoteSession;

    /** Gets the session ID. @return the session ID */
    public String getSessionId() {
        return sessionId;
    }

    /** Sets the session ID. @param sessionId the session ID */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /** Gets the client name. @return the client name */
    public String getClientName() {
        return clientName;
    }

    /** Sets the client name. @param clientName the client name */
    public void setClientName(String clientName) {
        this.clientName = clientName;
    }

    /** Gets the model name. @return the model */
    public String getModel() {
        return model;
    }

    /** Sets the model name. @param model the model */
    public void setModel(String model) {
        this.model = model;
    }

    /**
     * Gets the pending-work continuation choice.
     *
     * @return the choice, or {@code null} if omitted
     * @see ResumeSessionOptions#getContinuePendingWork()
     */
    public Boolean getContinuePendingWork() {
        return continuePendingWork;
    }

    /**
     * Sets the pending-work continuation choice.
     *
     * @param continuePendingWork
     *            the choice
     * @see #getContinuePendingWork()
     */
    public void setContinuePendingWork(boolean continuePendingWork) {
        this.continuePendingWork = continuePendingWork;
    }

    /** Restores the runtime's default pending-work continuation behavior. */
    public void clearContinuePendingWork() {
        continuePendingWork = null;
    }

    /** @return whether transcript repair was requested, or null if omitted */
    public Boolean getAllowTranscriptRecovery() {
        return allowTranscriptRecovery;
    }

    /**
     * @param allowTranscriptRecovery
     *            whether transcript repair is allowed
     */
    public void setAllowTranscriptRecovery(boolean allowTranscriptRecovery) {
        this.allowTranscriptRecovery = allowTranscriptRecovery;
    }

    /** Gets the system message config. @return the system message config */
    public SystemMessageConfig getSystemMessage() {
        return systemMessage;
    }

    /**
     * Sets the system message config. @param systemMessage the system message
     * config
     */
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

    /** Gets disable resume flag. @return the flag */
    public Boolean getSuppressResumeEvent() {
        return suppressResumeEvent;
    }

    /** Sets disable resume flag. @param suppressResumeEvent the flag */
    public void setSuppressResumeEvent(boolean suppressResumeEvent) {
        this.suppressResumeEvent = suppressResumeEvent;
    }

    /**
     * Clears the disableResume setting, reverting to the default behavior.
     */
    public void clearSuppressResumeEvent() {
        this.suppressResumeEvent = null;
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
    public ResumeSessionRequest setInference(SessionInferenceRequestOptions value) {
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
    public ResumeSessionRequest setInteraction(SessionInteractionRequestOptions value) {
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
    public ResumeSessionRequest setTools(SessionToolRequestOptions value) {
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
    public ResumeSessionRequest setAgents(SessionAgentRequestOptions value) {
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
    public ResumeSessionRequest setGitHub(SessionGitHubRequestOptions value) {
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
    public ResumeSessionRequest setFilesystem(SessionFilesystemRequestOptions value) {
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
    public ResumeSessionRequest setExecution(SessionExecutionRequestOptions value) {
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
    public ResumeSessionRequest setPermissions(SessionPermissionRequestOptions value) {
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
    public ResumeSessionRequest setHooks(SessionHookRequestOptions value) {
        hooks = value == null ? new SessionHookRequestOptions() : value;
        return this;
    }

    @JsonProperty("configDirectory")
    private String configDirectory;

    @JsonProperty("enableConfigDiscovery")
    private Boolean enableConfigDiscovery;

    @JsonProperty("extensionSources")
    private com.microsoft.copilot.generated.rpc.ExtensionSources extensionSources;

    @JsonProperty("extensionPaths")
    private List<String> extensionPaths;

    /** Gets exact extension entry modules. @return the selected paths */
    public List<String> getExtensionPaths() {
        return extensionPaths;
    }

    /**
     * Sets exact entry modules.
     *
     * @param value
     *            the selected paths
     * @return this request
     */
    public ResumeSessionRequest setExtensionPaths(List<String> value) {
        extensionPaths = value;
        return this;
    }

    @JsonProperty("extensions")
    private SessionExtensionOptions extensions;

    /**
     * Gets extension activation and SDK injection.
     *
     * @return the choices, or {@code null} when omitted
     */
    public SessionExtensionOptions getExtensions() {
        return extensions;
    }

    /**
     * Sets existing session extension choices.
     *
     * @param value
     *            the choices, or {@code null} to omit
     * @return this request
     */
    public ResumeSessionRequest setExtensions(SessionExtensionOptions value) {
        extensions = value;
        return this;
    }

    /** Gets extension source overrides. @return the overrides */
    public com.microsoft.copilot.generated.rpc.ExtensionSources getExtensionSources() {
        return extensionSources;
    }

    /**
     * Sets extension source overrides.
     *
     * @param value
     *            the overrides
     * @return this request
     */
    public ResumeSessionRequest setExtensionSources(com.microsoft.copilot.generated.rpc.ExtensionSources value) {
        extensionSources = value;
        return this;
    }

    /** Gets config configDirectory. @return the config configDirectory */
    public String getConfigDirectory() {
        return configDirectory;
    }

    /**
     * Sets config configDirectory. @param configDirectory the config
     * configDirectory
     */
    public ResumeSessionRequest setConfigDirectory(String configDirectory) {
        this.configDirectory = configDirectory;
        return this;
    }

    /** Gets enable config discovery flag. @return the flag */
    public Boolean getEnableConfigDiscovery() {
        return enableConfigDiscovery;
    }

    /** Sets enable config discovery flag. @param enableConfigDiscovery the flag */
    public ResumeSessionRequest setEnableConfigDiscovery(boolean enableConfigDiscovery) {
        this.enableConfigDiscovery = enableConfigDiscovery;
        return this;
    }

    /**
     * Clears the discovery setting, reverting to the default behavior.
     */
    public void clearEnableConfigDiscovery() {
        this.enableConfigDiscovery = null;
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
    public ResumeSessionRequest setState(SessionStateRequestOptions value) {
        state = value == null ? new SessionStateRequestOptions() : value;
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
    public ResumeSessionRequest setInstructions(SessionInstructionRequestOptions value) {
        instructions = value == null ? new SessionInstructionRequestOptions() : value;
        return this;
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
    public ResumeSessionRequest setGit(SessionGitRequestOptions value) {
        git = value == null ? new SessionGitRequestOptions() : value;
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
    public ResumeSessionRequest setSkills(SessionSkillRequestOptions value) {
        skills = value == null ? new SessionSkillRequestOptions() : value;
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
    public ResumeSessionRequest setEvents(SessionEventRequestOptions value) {
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
    public ResumeSessionRequest setMcp(SessionMcpRequestOptions value) {
        mcp = value == null ? new SessionMcpRequestOptions() : value;
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
    public ResumeSessionRequest setPlugins(SessionPluginRequestOptions value) {
        plugins = value == null ? new SessionPluginRequestOptions() : value;
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
    public ResumeSessionRequest setExperiments(SessionExperimentRequestOptions value) {
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
    public ResumeSessionRequest setPolicy(SessionPolicyRequestOptions value) {
        policy = value == null ? new SessionPolicyRequestOptions() : value;
        return this;
    }

}
