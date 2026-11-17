/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Session construction options.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenOptions(
    /** Optional stable session identifier to use for a new session. */
    @JsonProperty("sessionId") String sessionId,
    /** Optional human-friendly session name. */
    @JsonProperty("name") String name,
    /** Initial model identifier. */
    @JsonProperty("model") String model,
    /** Identifier of the client driving the session. */
    @JsonProperty("clientName") String clientName,
    /** Structured client kind used for runtime behavior gates. */
    @JsonProperty("clientKind") String clientKind,
    /** Identifier sent to LSP-style integrations. */
    @JsonProperty("lspClientName") String lspClientName,
    /** Stable integration identifier for analytics. */
    @JsonProperty("integrationId") String integrationId,
    /** Working directory to anchor the session. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Whether this session supports remote steering. */
    @JsonProperty("remoteSteerable") Boolean remoteSteerable,
    /** Telemetry-only remote exporting flag. */
    @JsonProperty("remoteExporting") Boolean remoteExporting,
    /** Telemetry-only remote-defaulted flag. */
    @JsonProperty("remoteDefaultedOn") Boolean remoteDefaultedOn,
    /** Parent session ID for detached child telemetry rollup. */
    @JsonProperty("detachedFromSpawningParentSessionId") String detachedFromSpawningParentSessionId,
    /** Parent engagement ID for detached child telemetry rollup. */
    @JsonProperty("detachedFromSpawningParentEngagementId") String detachedFromSpawningParentEngagementId,
    /** Capabilities enabled for this session. */
    @JsonProperty("sessionCapabilities") List<SessionCapability> sessionCapabilities,
    /** Model tuning and provider catalog. */
    @JsonProperty("inference") SessionOpenInferenceOptions inference,
    /** MCP servers, OAuth, and renderer capability. */
    @JsonProperty("mcp") SessionOpenMcpOptions mcp,
    /** Feature experimentation and trusted assignments. */
    @JsonProperty("experiments") SessionOpenExperimentOptions experiments,
    /** GitHub integration and session credentials. */
    @JsonProperty("github") SessionOpenGitHubOptions gitHub,
    /** Host-injected session policy. */
    @JsonProperty("policy") SessionOpenPolicyOptions policy,
    /** Project directory access and change tracking. */
    @JsonProperty("filesystem") SessionOpenFilesystemOptions filesystem,
    /** Tool declarations, exposure, and behavior. */
    @JsonProperty("tools") SessionOpenToolOptions tools,
    /** Agent definitions, selection, and source filters. */
    @JsonProperty("agents") SessionOpenAgentOptions agents,
    /** Permission decisions and sandboxing. */
    @JsonProperty("permissions") SessionOpenPermissionOptions permissions,
    /** Skill loading, sources, and provider capability. */
    @JsonProperty("skills") SessionOpenSkillOptions skills,
    /** Instruction sources and discovery. */
    @JsonProperty("instructions") SessionOpenInstructionOptions instructions,
    /** Callback and filesystem hook sources. */
    @JsonProperty("hooks") SessionOpenHookOptions hooks,
    /** Explicit plugin sources. */
    @JsonProperty("plugins") SessionOpenPluginOptions plugins,
    /** Host Git context and commit attribution. */
    @JsonProperty("git") SessionOpenGitOptions git,
    /** Event delivery and streaming. */
    @JsonProperty("events") SessionOpenEventOptions events,
    /** Human input and mode-switch callback capabilities. */
    @JsonProperty("interaction") SessionOpenInteractionOptions interaction,
    /** Execution accounting and autonomous continuation. */
    @JsonProperty("execution") SessionOpenExecutionOptions execution,
    /** Context retention and session-state storage. */
    @JsonProperty("state") SessionOpenStateOptions state,
    /** Override Copilot configuration directory. */
    @JsonProperty("configDirectory") String configDirectory,
    /** Enables runtime discovery of supported configuration. Explicitly supplied
configuration takes precedence; explicit feature roots remain independent. */
    @JsonProperty("enableConfigDiscovery") Boolean enableConfigDiscovery
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenOptions {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    public static final class Builder {
        private Builder() {}
        private String sessionId;
        private String name;
        private String model;
        private String clientName;
        private String clientKind;
        private String lspClientName;
        private String integrationId;
        private String workingDirectory;
        private Boolean remoteSteerable;
        private Boolean remoteExporting;
        private Boolean remoteDefaultedOn;
        private String detachedFromSpawningParentSessionId;
        private String detachedFromSpawningParentEngagementId;
        private List<SessionCapability> sessionCapabilities;
        private SessionOpenInferenceOptions inference;
        private SessionOpenMcpOptions mcp;
        private SessionOpenExperimentOptions experiments;
        private SessionOpenGitHubOptions gitHub;
        private SessionOpenPolicyOptions policy;
        private SessionOpenFilesystemOptions filesystem;
        private SessionOpenToolOptions tools;
        private SessionOpenAgentOptions agents;
        private SessionOpenPermissionOptions permissions;
        private SessionOpenSkillOptions skills;
        private SessionOpenInstructionOptions instructions;
        private SessionOpenHookOptions hooks;
        private SessionOpenPluginOptions plugins;
        private SessionOpenGitOptions git;
        private SessionOpenEventOptions events;
        private SessionOpenInteractionOptions interaction;
        private SessionOpenExecutionOptions execution;
        private SessionOpenStateOptions state;
        private String configDirectory;
        private Boolean enableConfigDiscovery;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder name(String value) {
            this.name = value;
            return this;
        }

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder model(String value) {
            this.model = value;
            return this;
        }

        /**
         * Sets the {@code clientName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder clientName(String value) {
            this.clientName = value;
            return this;
        }

        /**
         * Sets the {@code clientKind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder clientKind(String value) {
            this.clientKind = value;
            return this;
        }

        /**
         * Sets the {@code lspClientName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder lspClientName(String value) {
            this.lspClientName = value;
            return this;
        }

        /**
         * Sets the {@code integrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder integrationId(String value) {
            this.integrationId = value;
            return this;
        }

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code remoteSteerable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder remoteSteerable(Boolean value) {
            this.remoteSteerable = value;
            return this;
        }

        /**
         * Sets the {@code remoteExporting} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder remoteExporting(Boolean value) {
            this.remoteExporting = value;
            return this;
        }

        /**
         * Sets the {@code remoteDefaultedOn} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder remoteDefaultedOn(Boolean value) {
            this.remoteDefaultedOn = value;
            return this;
        }

        /**
         * Sets the {@code detachedFromSpawningParentSessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder detachedFromSpawningParentSessionId(String value) {
            this.detachedFromSpawningParentSessionId = value;
            return this;
        }

        /**
         * Sets the {@code detachedFromSpawningParentEngagementId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder detachedFromSpawningParentEngagementId(String value) {
            this.detachedFromSpawningParentEngagementId = value;
            return this;
        }

        /**
         * Sets the {@code sessionCapabilities} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionCapabilities(List<SessionCapability> value) {
            this.sessionCapabilities = value;
            return this;
        }

        /**
         * Sets the {@code inference} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder inference(SessionOpenInferenceOptions value) {
            this.inference = value;
            return this;
        }

        /**
         * Sets the {@code mcp} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder mcp(SessionOpenMcpOptions value) {
            this.mcp = value;
            return this;
        }

        /**
         * Sets the {@code experiments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder experiments(SessionOpenExperimentOptions value) {
            this.experiments = value;
            return this;
        }

        /**
         * Sets the {@code github} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitHub(SessionOpenGitHubOptions value) {
            this.gitHub = value;
            return this;
        }

        /**
         * Sets the {@code policy} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder policy(SessionOpenPolicyOptions value) {
            this.policy = value;
            return this;
        }

        /**
         * Sets the {@code filesystem} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder filesystem(SessionOpenFilesystemOptions value) {
            this.filesystem = value;
            return this;
        }

        /**
         * Sets the {@code tools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder tools(SessionOpenToolOptions value) {
            this.tools = value;
            return this;
        }

        /**
         * Sets the {@code agents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agents(SessionOpenAgentOptions value) {
            this.agents = value;
            return this;
        }

        /**
         * Sets the {@code permissions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder permissions(SessionOpenPermissionOptions value) {
            this.permissions = value;
            return this;
        }

        /**
         * Sets the {@code skills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skills(SessionOpenSkillOptions value) {
            this.skills = value;
            return this;
        }

        /**
         * Sets the {@code instructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder instructions(SessionOpenInstructionOptions value) {
            this.instructions = value;
            return this;
        }

        /**
         * Sets the {@code hooks} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hooks(SessionOpenHookOptions value) {
            this.hooks = value;
            return this;
        }

        /**
         * Sets the {@code plugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugins(SessionOpenPluginOptions value) {
            this.plugins = value;
            return this;
        }

        /**
         * Sets the {@code git} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder git(SessionOpenGitOptions value) {
            this.git = value;
            return this;
        }

        /**
         * Sets the {@code events} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder events(SessionOpenEventOptions value) {
            this.events = value;
            return this;
        }

        /**
         * Sets the {@code interaction} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder interaction(SessionOpenInteractionOptions value) {
            this.interaction = value;
            return this;
        }

        /**
         * Sets the {@code execution} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder execution(SessionOpenExecutionOptions value) {
            this.execution = value;
            return this;
        }

        /**
         * Sets the {@code state} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder state(SessionOpenStateOptions value) {
            this.state = value;
            return this;
        }

        /**
         * Sets the {@code configDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configDirectory(String value) {
            this.configDirectory = value;
            return this;
        }

        /**
         * Sets the {@code enableConfigDiscovery} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableConfigDiscovery(Boolean value) {
            this.enableConfigDiscovery = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenOptions build() {
            return new SessionOpenOptions(sessionId, name, model, clientName, clientKind, lspClientName, integrationId, workingDirectory, remoteSteerable, remoteExporting, remoteDefaultedOn, detachedFromSpawningParentSessionId, detachedFromSpawningParentEngagementId, sessionCapabilities, inference, mcp, experiments, gitHub, policy, filesystem, tools, agents, permissions, skills, instructions, hooks, plugins, git, events, interaction, execution, state, configDirectory, enableConfigDiscovery);
        }
    }
}
