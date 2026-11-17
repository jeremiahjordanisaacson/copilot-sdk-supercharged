/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Patch of mutable session options to apply to the running session.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOptionsUpdateParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Session-only user preferences. Omitted preferences keep their current values. */
    @JsonProperty("settings") SessionSettingsPatch settings,
    /** The model ID to use for assistant turns. */
    @JsonProperty("model") String model,
    /** Identifier of the client driving the session. */
    @JsonProperty("clientName") String clientName,
    /** Identifier sent to LSP-style integrations. */
    @JsonProperty("lspClientName") String lspClientName,
    /** Stable integration identifier used for analytics and rate-limit attribution. */
    @JsonProperty("integrationId") String integrationId,
    /** Absolute working-directory path for shell tools. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Replaces the session's capability set with the given list. Use to enable or disable capabilities mid-session (e.g., remove `memory` for reproducible scripted runs). Omit the field to leave the existing capability set unchanged. */
    @JsonProperty("sessionCapabilities") List<SessionCapability> sessionCapabilities,
    /** Tool declarations, exposure, and behavior. */
    @JsonProperty("tools") SessionUpdateToolOptions tools,
    /** Model tuning and provider catalog. */
    @JsonProperty("inference") SessionUpdateInferenceOptions inference,
    /** Feature experimentation and trusted assignments. */
    @JsonProperty("experiments") SessionUpdateExperimentOptions experiments,
    /** GitHub integration and session credentials. */
    @JsonProperty("github") SessionUpdateGitHubOptions gitHub,
    /** Agent definitions, selection, and source filters. */
    @JsonProperty("agents") SessionUpdateAgentOptions agents,
    /** Permission decisions and sandboxing. */
    @JsonProperty("permissions") SessionUpdatePermissionOptions permissions,
    /** MCP servers, OAuth, and renderer capability. */
    @JsonProperty("mcp") SessionUpdateMcpOptions mcp,
    /** Skill loading, sources, and provider capability. */
    @JsonProperty("skills") SessionUpdateSkillOptions skills,
    /** Instruction sources and discovery. */
    @JsonProperty("instructions") SessionUpdateInstructionOptions instructions,
    /** Callback and filesystem hook sources. */
    @JsonProperty("hooks") SessionUpdateHookOptions hooks,
    /** Explicit plugin sources. */
    @JsonProperty("plugins") SessionUpdatePluginOptions plugins,
    /** Host Git context and commit attribution. */
    @JsonProperty("git") SessionUpdateGitOptions git,
    /** Context retention and session-state storage. */
    @JsonProperty("state") SessionUpdateStateOptions state,
    /** Event delivery and streaming. */
    @JsonProperty("events") SessionUpdateEventOptions events,
    /** Human input and mode-switch callback capabilities. */
    @JsonProperty("interaction") SessionUpdateInteractionOptions interaction,
    /** Execution accounting and autonomous continuation. */
    @JsonProperty("execution") SessionUpdateExecutionOptions execution,
    /** Host-injected session policy. */
    @JsonProperty("policy") SessionUpdatePolicyOptions policy
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOptionsUpdateParams {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String sessionId;
        private SessionSettingsPatch settings;
        private String model;
        private String clientName;
        private String lspClientName;
        private String integrationId;
        private String workingDirectory;
        private List<SessionCapability> sessionCapabilities;
        private SessionUpdateToolOptions tools;
        private SessionUpdateInferenceOptions inference;
        private SessionUpdateExperimentOptions experiments;
        private SessionUpdateGitHubOptions gitHub;
        private SessionUpdateAgentOptions agents;
        private SessionUpdatePermissionOptions permissions;
        private SessionUpdateMcpOptions mcp;
        private SessionUpdateSkillOptions skills;
        private SessionUpdateInstructionOptions instructions;
        private SessionUpdateHookOptions hooks;
        private SessionUpdatePluginOptions plugins;
        private SessionUpdateGitOptions git;
        private SessionUpdateStateOptions state;
        private SessionUpdateEventOptions events;
        private SessionUpdateInteractionOptions interaction;
        private SessionUpdateExecutionOptions execution;
        private SessionUpdatePolicyOptions policy;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code settings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder settings(SessionSettingsPatch value) {
            this.settings = value;
            return this;
        }

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
        public Builder clientName(String value) {
            this.clientName = value;
            return this;
        }

        /**
         * Sets the {@code lspClientName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code sessionCapabilities} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionCapabilities(List<SessionCapability> value) {
            this.sessionCapabilities = value;
            return this;
        }

        /**
         * Sets the {@code tools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder tools(SessionUpdateToolOptions value) {
            this.tools = value;
            return this;
        }

        /**
         * Sets the {@code inference} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder inference(SessionUpdateInferenceOptions value) {
            this.inference = value;
            return this;
        }

        /**
         * Sets the {@code experiments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder experiments(SessionUpdateExperimentOptions value) {
            this.experiments = value;
            return this;
        }

        /**
         * Sets the {@code github} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder gitHub(SessionUpdateGitHubOptions value) {
            this.gitHub = value;
            return this;
        }

        /**
         * Sets the {@code agents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder agents(SessionUpdateAgentOptions value) {
            this.agents = value;
            return this;
        }

        /**
         * Sets the {@code permissions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder permissions(SessionUpdatePermissionOptions value) {
            this.permissions = value;
            return this;
        }

        /**
         * Sets the {@code mcp} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder mcp(SessionUpdateMcpOptions value) {
            this.mcp = value;
            return this;
        }

        /**
         * Sets the {@code skills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder skills(SessionUpdateSkillOptions value) {
            this.skills = value;
            return this;
        }

        /**
         * Sets the {@code instructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder instructions(SessionUpdateInstructionOptions value) {
            this.instructions = value;
            return this;
        }

        /**
         * Sets the {@code hooks} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder hooks(SessionUpdateHookOptions value) {
            this.hooks = value;
            return this;
        }

        /**
         * Sets the {@code plugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder plugins(SessionUpdatePluginOptions value) {
            this.plugins = value;
            return this;
        }

        /**
         * Sets the {@code git} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder git(SessionUpdateGitOptions value) {
            this.git = value;
            return this;
        }

        /**
         * Sets the {@code state} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder state(SessionUpdateStateOptions value) {
            this.state = value;
            return this;
        }

        /**
         * Sets the {@code events} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder events(SessionUpdateEventOptions value) {
            this.events = value;
            return this;
        }

        /**
         * Sets the {@code interaction} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder interaction(SessionUpdateInteractionOptions value) {
            this.interaction = value;
            return this;
        }

        /**
         * Sets the {@code execution} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder execution(SessionUpdateExecutionOptions value) {
            this.execution = value;
            return this;
        }

        /**
         * Sets the {@code policy} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder policy(SessionUpdatePolicyOptions value) {
            this.policy = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOptionsUpdateParams build() {
            return new SessionOptionsUpdateParams(sessionId, settings, model, clientName, lspClientName, integrationId, workingDirectory, sessionCapabilities, tools, inference, experiments, gitHub, agents, permissions, mcp, skills, instructions, hooks, plugins, git, state, events, interaction, execution, policy);
        }
    }
}
