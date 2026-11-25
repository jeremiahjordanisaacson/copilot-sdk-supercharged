/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonIgnore;

import com.microsoft.copilot.generated.rpc.DiagnosticsConfiguration;

/**
 * Configuration for resuming an existing Copilot session.
 * <p>
 * This class provides options for configuring a resumed session, including tool
 * registration, provider configuration, and streaming. All setter methods
 * return {@code this} for method chaining.
 *
 * <h2>Example Usage</h2>
 *
 * <pre>{@code
 * var config = new ResumeSessionOptions().setEvents(new SessionEventOptions().setStreaming(true))
 * 		.setTools(new SessionToolOptions().setDefinitions(List.of(myTool)));
 *
 * var session = client.resumeSession(sessionId, config).get();
 * }</pre>
 *
 * @see com.microsoft.copilot.CopilotClient#resumeSession(String,
 *      ResumeSessionOptions)
 * @since 1.0.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ResumeSessionOptions {

    private String clientName;
    private String model;
    private SystemMessageConfig systemMessage;
    private Boolean continuePendingWork;
    private Boolean allowTranscriptRecovery;
    private String workingDirectory;
    private boolean suppressResumeEvent;
    private DiagnosticsConfiguration diagnostics;

    /**
     * Gets the AI model to use.
     *
     * @return the model name
     */
    public String getModel() {
        return model;
    }

    /**
     * Sets the AI model to use for the resumed session.
     * <p>
     * Can change the model when resuming an existing session.
     *
     * @param model
     *            the model name
     * @return this config for method chaining
     */
    public ResumeSessionOptions setModel(String model) {
        this.model = model;
        return this;
    }

    /**
     * Gets whether pending work should continue after resuming the session. When
     * {@code false} (the default), pending work is treated as interrupted on
     * resume. Completed tool results already durably recorded by the runtime are
     * preserved.
     * <p>
     * When {@code true}, pending permission requests and external tool calls wait
     * for caller-supplied results via the corresponding low-level RPC methods.
     * Resuming does not re-emit restored permission requests or re-invoke
     * permission handlers or already-dispatched external tool callbacks. If a
     * pending call was recorded as SDK-handler-backed and resume registers a
     * handler for that tool, the abandoned callback is interrupted, never replayed.
     * Handlerless calls remain pending. To supply an abandoned callback's result
     * manually, resume without a handler for it. Older requests without recorded
     * callback ownership also remain pending.
     *
     * @return the explicit choice, or empty to use the runtime default
     */
    @JsonIgnore
    public Optional<Boolean> getContinuePendingWork() {
        return Optional.ofNullable(continuePendingWork);
    }

    /**
     * Sets whether to continue pending work when resuming the session.
     *
     * @param continuePendingWork
     *            whether to continue pending work
     * @return this config for method chaining
     * @see #getContinuePendingWork()
     */
    public ResumeSessionOptions setContinuePendingWork(boolean continuePendingWork) {
        this.continuePendingWork = continuePendingWork;
        return this;
    }

    /**
     * Clears the pending-work choice so the runtime default applies.
     *
     * @return this config for method chaining
     */
    public ResumeSessionOptions clearContinuePendingWork() {
        this.continuePendingWork = null;
        return this;
    }

    /**
     * Gets the explicit transcript repair choice on resume.
     *
     * @return the choice, or empty for the runtime default (true in all modes)
     */
    @JsonIgnore
    public Optional<Boolean> getAllowTranscriptRecovery() {
        return Optional.ofNullable(allowTranscriptRecovery);
    }

    /**
     * Sets whether to repair a damaged transcript on resume. Defaults to true in
     * all modes. Set false to reject recovery. Recovery can discard a torn tail;
     * inspect the returned session's report.
     *
     * @param allowTranscriptRecovery
     *            whether repair is allowed
     * @return this config
     */
    public ResumeSessionOptions setAllowTranscriptRecovery(boolean allowTranscriptRecovery) {
        this.allowTranscriptRecovery = allowTranscriptRecovery;
        return this;
    }

    /**
     * Clears the transcript repair choice so the runtime default (true) applies.
     *
     * @return this config for method chaining
     */
    public ResumeSessionOptions clearAllowTranscriptRecovery() {
        this.allowTranscriptRecovery = null;
        return this;
    }

    /**
     * Gets the client name used to identify the application using the SDK.
     *
     * @return the client name, or {@code null} if not set
     */
    public String getClientName() {
        return clientName;
    }

    /**
     * Sets the client name to identify the application using the SDK.
     * <p>
     * This value is included in the User-Agent header for API requests.
     *
     * @param clientName
     *            the client name
     * @return this config for method chaining
     */
    public ResumeSessionOptions setClientName(String clientName) {
        this.clientName = clientName;
        return this;
    }

    /**
     * Gets the system message configuration.
     *
     * @return the system message config
     */
    public SystemMessageConfig getSystemMessage() {
        return systemMessage;
    }

    /**
     * Sets the system message configuration.
     * <p>
     * The system message controls the behavior and personality of the assistant.
     *
     * @param systemMessage
     *            the system message configuration
     * @return this config for method chaining
     * @see SystemMessageConfig
     */
    public ResumeSessionOptions setSystemMessage(SystemMessageConfig systemMessage) {
        this.systemMessage = systemMessage;
        return this;
    }

    /**
     * Gets the working directory for the session.
     *
     * @return the working directory path
     */
    public String getWorkingDirectory() {
        return workingDirectory;
    }

    /**
     * Sets the working directory for the session.
     *
     * @param workingDirectory
     *            the working directory path
     * @return this config for method chaining
     */
    public ResumeSessionOptions setWorkingDirectory(String workingDirectory) {
        this.workingDirectory = workingDirectory;
        return this;
    }

    /**
     * Returns whether the resume event is disabled.
     *
     * @return {@code true} if the session.resume event is suppressed
     */
    public boolean isSuppressResumeEvent() {
        return suppressResumeEvent;
    }

    /**
     * Controls whether persisted pending work may continue on resume.
     *
     * @param continuePendingWork
     *            explicit setting, or {@code null} for the default
     * @return this config instance
     * @see #getContinuePendingWork()
     */
    public ResumeSessionOptions setContinuePendingWork(Boolean continuePendingWork) {
        this.continuePendingWork = continuePendingWork;
        return this;
    }

    /**
     * Sets whether to disable the session.resume event.
     * <p>
     * When true, the session.resume event is not emitted.
     *
     * @param suppressResumeEvent
     *            {@code true} to suppress the resume event
     * @return this config for method chaining
     */
    public ResumeSessionOptions setSuppressResumeEvent(boolean suppressResumeEvent) {
        this.suppressResumeEvent = suppressResumeEvent;
        return this;
    }

    /**
     * Gets the session-scoped diagnostics configuration.
     *
     * @return the diagnostic level, or {@code null} to preserve the current level
     */
    public DiagnosticsConfiguration getDiagnostics() {
        return diagnostics;
    }

    /**
     * Sets the session-scoped diagnostics configuration. Leave it unset when
     * resuming a resident session to preserve its current diagnostic level.
     *
     * @param diagnostics
     *            the diagnostic level
     * @return this config for method chaining
     */
    public ResumeSessionOptions setDiagnostics(DiagnosticsConfiguration diagnostics) {
        this.diagnostics = diagnostics;
        return this;
    }

    @JsonProperty("tools")
    private SessionToolOptions tools = new SessionToolOptions();
    /**
     * Returns the tools options.
     *
     * @return the options
     */
    public SessionToolOptions getTools() {
        return tools;
    }
    /**
     * Sets the tools options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setTools(SessionToolOptions value) {
        tools = value == null ? new SessionToolOptions() : value;
        return this;
    }

    @JsonProperty("agents")
    private SessionAgentOptions agents = new SessionAgentOptions();
    /**
     * Returns the agents options.
     *
     * @return the options
     */
    public SessionAgentOptions getAgents() {
        return agents;
    }
    /**
     * Sets the agents options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setAgents(SessionAgentOptions value) {
        agents = value == null ? new SessionAgentOptions() : value;
        return this;
    }

    @JsonProperty("inference")
    private SessionInferenceOptions inference = new SessionInferenceOptions();
    /**
     * Returns the inference options.
     *
     * @return the options
     */
    public SessionInferenceOptions getInference() {
        return inference;
    }
    /**
     * Sets the inference options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setInference(SessionInferenceOptions value) {
        inference = value == null ? new SessionInferenceOptions() : value;
        return this;
    }

    @JsonProperty("github")
    private SessionGitHubOptions github = new SessionGitHubOptions();
    /**
     * Returns the github options.
     *
     * @return the options
     */
    @JsonProperty("github")
    public SessionGitHubOptions getGitHub() {
        return github;
    }
    /**
     * Sets the github options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setGitHub(SessionGitHubOptions value) {
        github = value == null ? new SessionGitHubOptions() : value;
        return this;
    }

    @JsonProperty("filesystem")
    private SessionFilesystemOptions filesystem = new SessionFilesystemOptions();
    /**
     * Returns the filesystem options.
     *
     * @return the options
     */
    public SessionFilesystemOptions getFilesystem() {
        return filesystem;
    }
    /**
     * Sets the filesystem options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setFilesystem(SessionFilesystemOptions value) {
        filesystem = value == null ? new SessionFilesystemOptions() : value;
        return this;
    }

    @JsonProperty("execution")
    private SessionExecutionOptions execution = new SessionExecutionOptions();
    /**
     * Returns the execution options.
     *
     * @return the options
     */
    public SessionExecutionOptions getExecution() {
        return execution;
    }
    /**
     * Sets the execution options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setExecution(SessionExecutionOptions value) {
        execution = value == null ? new SessionExecutionOptions() : value;
        return this;
    }

    @JsonProperty("experiments")
    private SessionExperimentOptions experiments = new SessionExperimentOptions();
    /**
     * Returns the experiments options.
     *
     * @return the options
     */
    public SessionExperimentOptions getExperiments() {
        return experiments;
    }
    /**
     * Sets the experiments options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setExperiments(SessionExperimentOptions value) {
        experiments = value == null ? new SessionExperimentOptions() : value;
        return this;
    }

    @JsonProperty("instructions")
    private SessionInstructionOptions instructions = new SessionInstructionOptions();
    /**
     * Returns the instructions options.
     *
     * @return the options
     */
    public SessionInstructionOptions getInstructions() {
        return instructions;
    }
    /**
     * Sets the instructions options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setInstructions(SessionInstructionOptions value) {
        instructions = value == null ? new SessionInstructionOptions() : value;
        return this;
    }

    @JsonProperty("git")
    private SessionGitOptions git = new SessionGitOptions();
    /**
     * Returns the git options.
     *
     * @return the options
     */
    public SessionGitOptions getGit() {
        return git;
    }
    /**
     * Sets the git options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setGit(SessionGitOptions value) {
        git = value == null ? new SessionGitOptions() : value;
        return this;
    }

    @JsonProperty("interaction")
    private SessionInteractionOptions interaction = new SessionInteractionOptions();
    /**
     * Returns the interaction options.
     *
     * @return the options
     */
    public SessionInteractionOptions getInteraction() {
        return interaction;
    }
    /**
     * Sets the interaction options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setInteraction(SessionInteractionOptions value) {
        interaction = value == null ? new SessionInteractionOptions() : value;
        return this;
    }

    @JsonProperty("permissions")
    private SessionPermissionOptions permissions = new SessionPermissionOptions();
    /**
     * Returns the permissions options.
     *
     * @return the options
     */
    public SessionPermissionOptions getPermissions() {
        return permissions;
    }
    /**
     * Sets the permissions options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setPermissions(SessionPermissionOptions value) {
        permissions = value == null ? new SessionPermissionOptions() : value;
        return this;
    }

    @JsonProperty("mcp")
    private SessionMcpOptions mcp = new SessionMcpOptions();
    /**
     * Returns the mcp options.
     *
     * @return the options
     */
    public SessionMcpOptions getMcp() {
        return mcp;
    }
    /**
     * Sets the mcp options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setMcp(SessionMcpOptions value) {
        mcp = value == null ? new SessionMcpOptions() : value;
        return this;
    }

    @JsonProperty("hooks")
    private SessionHookOptions hooks = new SessionHookOptions();
    /**
     * Returns the hooks options.
     *
     * @return the options
     */
    public SessionHookOptions getHooks() {
        return hooks;
    }
    /**
     * Sets the hooks options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setHooks(SessionHookOptions value) {
        hooks = value == null ? new SessionHookOptions() : value;
        return this;
    }

    private String configDirectory;
    private Boolean enableConfigDiscovery;
    @JsonProperty("extensions")
    private SessionExtensionOptions extensions = new SessionExtensionOptions();
    private com.microsoft.copilot.generated.rpc.ExtensionSources extensionSources;
    private List<String> extensionPaths;

    /**
     * Gets exact extension entry modules.
     *
     * @return the selected entries, or null to remove previous explicit entries
     */
    public List<String> getExtensionPaths() {
        return extensionPaths;
    }

    /**
     * Replaces host-trusted standalone modules without enabling extensions or
     * granting trust. Discovered entries and filesystem aliases are unsupported;
     * exact discovered overlaps fail per entry. Relative paths resolve against this
     * resume request's effective working directory. Omission or an empty list
     * removes previous explicit entries.
     *
     * @param value
     *            the exact module paths
     * @return this options instance
     */
    public ResumeSessionOptions setExtensionPaths(List<String> value) {
        extensionPaths = value;
        return this;
    }

    /**
     * Gets existing extension activation and SDK injection choices.
     *
     * @return the options
     */
    public SessionExtensionOptions getExtensions() {
        return extensions;
    }

    /**
     * Sets extension activation and SDK injection. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setExtensions(SessionExtensionOptions value) {
        extensions = value == null ? new SessionExtensionOptions() : value;
        return this;
    }

    /**
     * Gets extension source overrides.
     *
     * @return the overrides, or null for baseline sources
     */
    public com.microsoft.copilot.generated.rpc.ExtensionSources getExtensionSources() {
        return extensionSources;
    }

    /**
     * Selects sources without enabling extensions. Warm resumes cannot change
     * overrides; cold omission restores defaults. Requires a supporting runtime.
     *
     * @param value
     *            the source overrides
     * @return this options instance
     */
    public ResumeSessionOptions setExtensionSources(com.microsoft.copilot.generated.rpc.ExtensionSources value) {
        extensionSources = value;
        return this;
    }

    /**
     * Gets the configuration directory path.
     *
     * @return the configuration directory path
     */
    public String getConfigDirectory() {
        return configDirectory;
    }

    /**
     * Sets the configuration directory path.
     * <p>
     * Override the default configuration directory location.
     *
     * @param configDirectory
     *            the configuration directory path
     * @return this config for method chaining
     */
    public ResumeSessionOptions setConfigDirectory(String configDirectory) {
        this.configDirectory = configDirectory;
        return this;
    }

    /**
     * Gets whether automatic configuration discovery is enabled.
     *
     * @return an {@link java.util.Optional} containing {@code true} to enable
     *         discovery or {@code false} to disable it, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getEnableConfigDiscovery() {
        return Optional.ofNullable(enableConfigDiscovery);
    }

    /**
     * Enables runtime discovery of supported configuration. Explicitly supplied
     * configuration takes precedence over discovered values.
     *
     * @param enableConfigDiscovery
     *            {@code true} to enable discovery, {@code false} to disable
     * @return this config for method chaining
     */
    public ResumeSessionOptions setEnableConfigDiscovery(boolean enableConfigDiscovery) {
        this.enableConfigDiscovery = enableConfigDiscovery;
        return this;
    }
    /**
     * Clears the discovery setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public ResumeSessionOptions clearEnableConfigDiscovery() {
        this.enableConfigDiscovery = null;
        return this;
    }

    @JsonProperty("state")
    private SessionStateOptions state = new SessionStateOptions();
    /**
     * Returns the state options.
     *
     * @return the options
     */
    public SessionStateOptions getState() {
        return state;
    }
    /**
     * Sets the state options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setState(SessionStateOptions value) {
        state = value == null ? new SessionStateOptions() : value;
        return this;
    }

    @JsonProperty("skills")
    private SessionSkillOptions skills = new SessionSkillOptions();
    /**
     * Returns the skills options.
     *
     * @return the options
     */
    public SessionSkillOptions getSkills() {
        return skills;
    }
    /**
     * Sets the skills options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setSkills(SessionSkillOptions value) {
        skills = value == null ? new SessionSkillOptions() : value;
        return this;
    }

    @JsonProperty("events")
    private SessionEventOptions events = new SessionEventOptions();
    /**
     * Returns the events options.
     *
     * @return the options
     */
    public SessionEventOptions getEvents() {
        return events;
    }
    /**
     * Sets the events options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setEvents(SessionEventOptions value) {
        events = value == null ? new SessionEventOptions() : value;
        return this;
    }

    @JsonProperty("plugins")
    private SessionPluginOptions plugins = new SessionPluginOptions();
    /**
     * Returns the plugins options.
     *
     * @return the options
     */
    public SessionPluginOptions getPlugins() {
        return plugins;
    }
    /**
     * Sets the plugins options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setPlugins(SessionPluginOptions value) {
        plugins = value == null ? new SessionPluginOptions() : value;
        return this;
    }

    @JsonProperty("policy")
    private SessionPolicyOptions policy = new SessionPolicyOptions();
    /**
     * Returns the policy options.
     *
     * @return the options
     */
    public SessionPolicyOptions getPolicy() {
        return policy;
    }
    /**
     * Sets the policy options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public ResumeSessionOptions setPolicy(SessionPolicyOptions value) {
        policy = value == null ? new SessionPolicyOptions() : value;
        return this;
    }

    /**
     * Copies options and their mutable group collections.
     *
     * @return the independent options copy
     */
    @Override
    public ResumeSessionOptions clone() {
        ResumeSessionOptions copy = new ResumeSessionOptions();
        copy.clientName = this.clientName;
        copy.model = this.model;
        copy.systemMessage = this.systemMessage;
        copy.continuePendingWork = this.continuePendingWork;
        copy.allowTranscriptRecovery = this.allowTranscriptRecovery;
        copy.workingDirectory = this.workingDirectory;
        copy.suppressResumeEvent = this.suppressResumeEvent;
        copy.diagnostics = this.diagnostics;
        copy.tools = this.tools.clone();
        copy.agents = this.agents.clone();
        copy.inference = this.inference.clone();
        copy.github = this.github.clone();
        copy.filesystem = this.filesystem.clone();
        copy.execution = this.execution.clone();
        copy.experiments = this.experiments.clone();
        copy.instructions = this.instructions.clone();
        copy.git = this.git.clone();
        copy.interaction = this.interaction.clone();
        copy.permissions = this.permissions.clone();
        copy.mcp = this.mcp.clone();
        copy.hooks = this.hooks.clone();
        copy.configDirectory = this.configDirectory;
        copy.enableConfigDiscovery = this.enableConfigDiscovery;
        copy.extensions = this.extensions.clone();
        copy.extensionSources = this.extensionSources;
        copy.extensionPaths = this.extensionPaths == null ? null : new ArrayList<>(this.extensionPaths);
        copy.state = this.state.clone();
        copy.skills = this.skills.clone();
        copy.events = this.events.clone();
        copy.plugins = this.plugins.clone();
        copy.policy = this.policy.clone();
        return copy;
    }

}
