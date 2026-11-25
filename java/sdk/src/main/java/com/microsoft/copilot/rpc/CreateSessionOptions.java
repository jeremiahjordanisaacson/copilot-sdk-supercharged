/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.Optional;
import com.fasterxml.jackson.annotation.JsonProperty;

import com.fasterxml.jackson.annotation.JsonInclude;

import com.microsoft.copilot.generated.rpc.DiagnosticsConfiguration;
import com.microsoft.copilot.generated.rpc.ExtensionSources;

/**
 * Configuration for creating a new Copilot session.
 * <p>
 * This class provides options for customizing session behavior, including model
 * selection, tool registration, system message customization, and more. All
 * setter methods return {@code this} for method chaining.
 *
 * <h2>Example Usage</h2>
 *
 * <pre>{@code
 * var config = new CreateSessionOptions().setModel("gpt-5")
 * 		.setSystemMessage(new SystemMessageConfig().setMode(SystemMessageMode.APPEND)
 * 				.setContent("Be concise in your responses."))
 * 		.setEvents(new SessionEventOptions().setStreaming(true));
 *
 * var session = client.createSession(config).get();
 * }</pre>
 *
 * @see com.microsoft.copilot.CopilotClient#createSession(CreateSessionOptions)
 * @since 1.0.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CreateSessionOptions {

    private String sessionId;
    private String name;
    private String clientName;
    private String model;
    private SystemMessageConfig systemMessage;
    private String workingDirectory;
    private DiagnosticsConfiguration diagnostics;
    private CloudSessionOptions cloud;

    /**
     * Gets the custom session ID.
     *
     * @return the session ID, or {@code null} to generate automatically
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * Sets a custom session ID.
     * <p>
     * If not provided, a unique session ID will be generated automatically.
     *
     * @param sessionId
     *            the custom session ID
     * @return this config instance for method chaining
     */
    public CreateSessionOptions setSessionId(String sessionId) {
        this.sessionId = sessionId;
        return this;
    }

    /**
     * Gets the create-time session name.
     *
     * @return the session name, or {@code null} if the session is unnamed
     */
    public String getName() {
        return name;
    }

    /**
     * Names the session at creation, as {@code session.name.set} would, and is
     * validated the same way: non-empty, no leading or trailing whitespace, at most
     * 100 UTF-16 units, no control characters, and no double quotes. A name also
     * makes the session persist from the start, so it can be listed and resumed
     * without ever running a turn. Local sessions only: cloud creation rejects it.
     *
     * @param name
     *            the session name
     * @return this config instance for method chaining
     */
    public CreateSessionOptions setName(String name) {
        this.name = name;
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
     * @return this config instance for method chaining
     */
    public CreateSessionOptions setClientName(String clientName) {
        this.clientName = clientName;
        return this;
    }

    /**
     * Gets the AI model to use.
     *
     * @return the model name
     */
    public String getModel() {
        return model;
    }

    /**
     * Sets the AI model to use for this session.
     * <p>
     * Examples: "gpt-5", "claude-sonnet-4.5", "o3-mini".
     *
     * @param model
     *            the model name
     * @return this config instance for method chaining
     */
    public CreateSessionOptions setModel(String model) {
        this.model = model;
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
     * Use {@link com.microsoft.copilot.SystemMessageMode#APPEND} to add
     * instructions while preserving default behavior, or
     * {@link com.microsoft.copilot.SystemMessageMode#REPLACE} to fully customize.
     *
     * @param systemMessage
     *            the system message configuration
     * @return this config instance for method chaining
     * @see SystemMessageConfig
     */
    public CreateSessionOptions setSystemMessage(SystemMessageConfig systemMessage) {
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
     * @return this config instance for method chaining
     */
    public CreateSessionOptions setWorkingDirectory(String workingDirectory) {
        this.workingDirectory = workingDirectory;
        return this;
    }

    /**
     * Gets the session-scoped diagnostics configuration.
     *
     * @return the diagnostic level, or {@code null} when diagnostics are disabled
     */
    public DiagnosticsConfiguration getDiagnostics() {
        return diagnostics;
    }

    /**
     * Sets the session-scoped diagnostics configuration. Diagnostics are off by
     * default. Debug and trace entries can contain MCP payloads, tool arguments,
     * paths, and server stderr. Do not upload entries as telemetry or export them
     * without deliberate host action.
     *
     * @param diagnostics
     *            the diagnostic level
     * @return this config for method chaining
     */
    public CreateSessionOptions setDiagnostics(DiagnosticsConfiguration diagnostics) {
        this.diagnostics = diagnostics;
        return this;
    }

    /**
     * Gets the cloud session options.
     * <p>
     * When set, creates a remote session in the cloud instead of a local session.
     * The optional repository is associated with the cloud session.
     *
     * @return the cloud session options, or {@code null} if not set
     * @since 1.5.0
     */
    public CloudSessionOptions getCloud() {
        return cloud;
    }

    /**
     * Sets the cloud session options.
     * <p>
     * When set, creates a remote session in the cloud instead of a local session.
     * The optional repository is associated with the cloud session.
     *
     * @param cloud
     *            the cloud session options
     * @return this config instance for method chaining
     * @since 1.5.0
     */
    public CreateSessionOptions setCloud(CloudSessionOptions cloud) {
        this.cloud = cloud;
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
    public CreateSessionOptions setInference(SessionInferenceOptions value) {
        inference = value == null ? new SessionInferenceOptions() : value;
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
    public CreateSessionOptions setInteraction(SessionInteractionOptions value) {
        interaction = value == null ? new SessionInteractionOptions() : value;
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
    public CreateSessionOptions setTools(SessionToolOptions value) {
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
    public CreateSessionOptions setAgents(SessionAgentOptions value) {
        agents = value == null ? new SessionAgentOptions() : value;
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
    public CreateSessionOptions setGitHub(SessionGitHubOptions value) {
        github = value == null ? new SessionGitHubOptions() : value;
        return this;
    }

    @JsonProperty("filesystem")
    private CreateSessionFilesystemOptions filesystem = new CreateSessionFilesystemOptions();
    /**
     * Returns the filesystem options.
     *
     * @return the options
     */
    public CreateSessionFilesystemOptions getFilesystem() {
        return filesystem;
    }
    /**
     * Sets the filesystem options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionOptions setFilesystem(CreateSessionFilesystemOptions value) {
        filesystem = value == null ? new CreateSessionFilesystemOptions() : value;
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
    public CreateSessionOptions setExecution(SessionExecutionOptions value) {
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
    public CreateSessionOptions setExperiments(SessionExperimentOptions value) {
        experiments = value == null ? new SessionExperimentOptions() : value;
        return this;
    }

    @JsonProperty("instructions")
    private CreateSessionInstructionOptions instructions = new CreateSessionInstructionOptions();
    /**
     * Returns the instructions options.
     *
     * @return the options
     */
    public CreateSessionInstructionOptions getInstructions() {
        return instructions;
    }
    /**
     * Sets the instructions options. Null restores an empty group.
     *
     * @param value
     *            the options
     * @return this options instance
     */
    public CreateSessionOptions setInstructions(CreateSessionInstructionOptions value) {
        instructions = value == null ? new CreateSessionInstructionOptions() : value;
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
    public CreateSessionOptions setGit(SessionGitOptions value) {
        git = value == null ? new SessionGitOptions() : value;
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
    public CreateSessionOptions setPermissions(SessionPermissionOptions value) {
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
    public CreateSessionOptions setMcp(SessionMcpOptions value) {
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
    public CreateSessionOptions setHooks(SessionHookOptions value) {
        hooks = value == null ? new SessionHookOptions() : value;
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
    public CreateSessionOptions setEvents(SessionEventOptions value) {
        events = value == null ? new SessionEventOptions() : value;
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
    public CreateSessionOptions setState(SessionStateOptions value) {
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
    public CreateSessionOptions setSkills(SessionSkillOptions value) {
        skills = value == null ? new SessionSkillOptions() : value;
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
    public CreateSessionOptions setPlugins(SessionPluginOptions value) {
        plugins = value == null ? new SessionPluginOptions() : value;
        return this;
    }

    private String configDirectory;
    private Boolean enableConfigDiscovery;
    @JsonProperty("extensions")
    private SessionExtensionOptions extensions = new SessionExtensionOptions();
    private ExtensionSources extensionSources;

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
    public CreateSessionOptions setExtensions(SessionExtensionOptions value) {
        extensions = value == null ? new SessionExtensionOptions() : value;
        return this;
    }

    /**
     * Gets extension source overrides.
     *
     * @return the overrides, or null for existing source behavior
     */
    public ExtensionSources getExtensionSources() {
        return extensionSources;
    }

    /**
     * Selects sources without enabling extensions or granting trust. Warm resumes
     * cannot change overrides. Cold omission restores defaults. Requires runtime
     * support.
     *
     * @param value
     *            the source overrides
     * @return this options instance
     */
    public CreateSessionOptions setExtensionSources(ExtensionSources value) {
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
    public CreateSessionOptions setConfigDirectory(String configDirectory) {
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
    public CreateSessionOptions setEnableConfigDiscovery(boolean enableConfigDiscovery) {
        this.enableConfigDiscovery = enableConfigDiscovery;
        return this;
    }
    /**
     * Clears the discovery setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public CreateSessionOptions clearEnableConfigDiscovery() {
        this.enableConfigDiscovery = null;
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
    public CreateSessionOptions setPolicy(SessionPolicyOptions value) {
        policy = value == null ? new SessionPolicyOptions() : value;
        return this;
    }

    /**
     * Copies options and their mutable group collections.
     *
     * @return the independent options copy
     */
    @Override
    public CreateSessionOptions clone() {
        CreateSessionOptions copy = new CreateSessionOptions();
        copy.sessionId = this.sessionId;
        copy.clientName = this.clientName;
        copy.model = this.model;
        copy.systemMessage = this.systemMessage;
        copy.workingDirectory = this.workingDirectory;
        copy.diagnostics = this.diagnostics;
        copy.cloud = this.cloud;
        copy.inference = this.inference.clone();
        copy.interaction = this.interaction.clone();
        copy.tools = this.tools.clone();
        copy.agents = this.agents.clone();
        copy.github = this.github.clone();
        copy.filesystem = this.filesystem.clone();
        copy.execution = this.execution.clone();
        copy.experiments = this.experiments.clone();
        copy.instructions = this.instructions.clone();
        copy.git = this.git.clone();
        copy.permissions = this.permissions.clone();
        copy.mcp = this.mcp.clone();
        copy.hooks = this.hooks.clone();
        copy.events = this.events.clone();
        copy.state = this.state.clone();
        copy.skills = this.skills.clone();
        copy.plugins = this.plugins.clone();
        copy.configDirectory = this.configDirectory;
        copy.enableConfigDiscovery = this.enableConfigDiscovery;
        copy.extensions = this.extensions.clone();
        copy.extensionSources = this.extensionSources;
        copy.policy = this.policy.clone();
        return copy;
    }

}
