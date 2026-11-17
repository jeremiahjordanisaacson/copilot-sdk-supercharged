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
import javax.annotation.processing.Generated;

/**
 * Prompt-safe durable identity and live status for a direct workflow agent.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record WorkflowAgentSummary(
    /** Stable direct-agent identifier. */
    @JsonProperty("agentId") String agentId,
    /** Tool-call identifier that launched the agent. */
    @JsonProperty("toolCallId") String toolCallId,
    /** Owning workflow run identifier. */
    @JsonProperty("runId") String runId,
    /** Phase identifier active when the agent was launched, or null. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("phaseId") String phaseId,
    /** Friendly, non-unique name intended for display */
    @JsonProperty("label") String label,
    /** Friendly, non-unique name intended for display */
    @JsonProperty("displayName") String displayName,
    /** Registered agent type. */
    @JsonProperty("agentType") String agentType,
    /** Current durable or live agent status. */
    @JsonProperty("status") String status,
    /** Requested and resolved model observations, when available. */
    @JsonProperty("model") WorkflowAgentModelObservation model,
    /** Requested and resolved reasoning-effort observations, when available. */
    @JsonProperty("reasoningEffort") WorkflowAgentReasoningEffortObservation reasoningEffort,
    /** Requested and resolved context observations, when available. */
    @JsonProperty("context") WorkflowAgentContextObservation context,
    /** Epoch milliseconds when the agent started. */
    @JsonProperty("startedAt") Long startedAt,
    /** Epoch milliseconds when the agent completed. */
    @JsonProperty("completedAt") Long completedAt,
    /** Accumulated active agent time in milliseconds. */
    @JsonProperty("activeMs") Long activeMs,
    /** Known AI-credit spend, in nano-AIU, produced by this direct agent and its descendants. Omitted for historical agents when unattributed charges prevent a known zero. */
    @JsonProperty("consumedNanoAiu") Long consumedNanoAiu,
    /** Prompt-safe live activity text. */
    @JsonProperty("activity") String activity
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public WorkflowAgentSummary {}

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
        private String agentId;
        private String toolCallId;
        private String runId;
        private String phaseId;
        private boolean $presence3;
        private String label;
        private String displayName;
        private String agentType;
        private String status;
        private WorkflowAgentModelObservation model;
        private WorkflowAgentReasoningEffortObservation reasoningEffort;
        private WorkflowAgentContextObservation context;
        private Long startedAt;
        private Long completedAt;
        private Long activeMs;
        private Long consumedNanoAiu;
        private String activity;

        /**
         * Sets the {@code agentId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder agentId(String value) {
            this.agentId = value;
            return this;
        }

        /**
         * Sets the {@code toolCallId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder toolCallId(String value) {
            this.toolCallId = value;
            return this;
        }

        /**
         * Sets the {@code runId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder runId(String value) {
            this.runId = value;
            return this;
        }

        /**
         * Sets the {@code phaseId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder phaseId(String value) {
            this.phaseId = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code label} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder label(String value) {
            this.label = value;
            return this;
        }

        /**
         * Sets the {@code displayName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder displayName(String value) {
            this.displayName = value;
            return this;
        }

        /**
         * Sets the {@code agentType} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder agentType(String value) {
            this.agentType = value;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder status(String value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder model(WorkflowAgentModelObservation value) {
            this.model = value;
            return this;
        }

        /**
         * Sets the {@code reasoningEffort} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder reasoningEffort(WorkflowAgentReasoningEffortObservation value) {
            this.reasoningEffort = value;
            return this;
        }

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder context(WorkflowAgentContextObservation value) {
            this.context = value;
            return this;
        }

        /**
         * Sets the {@code startedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder startedAt(Long value) {
            this.startedAt = value;
            return this;
        }

        /**
         * Sets the {@code completedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder completedAt(Long value) {
            this.completedAt = value;
            return this;
        }

        /**
         * Sets the {@code activeMs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder activeMs(Long value) {
            this.activeMs = value;
            return this;
        }

        /**
         * Sets the {@code consumedNanoAiu} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder consumedNanoAiu(Long value) {
            this.consumedNanoAiu = value;
            return this;
        }

        /**
         * Sets the {@code activity} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder activity(String value) {
            this.activity = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public WorkflowAgentSummary build() {
            if (agentId == null) throw new IllegalStateException("Missing required property: agentId");
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            if (runId == null) throw new IllegalStateException("Missing required property: runId");
            if (!$presence3) throw new IllegalStateException("Missing required property: phaseId");
            if (label == null) throw new IllegalStateException("Missing required property: label");
            if (agentType == null) throw new IllegalStateException("Missing required property: agentType");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (activeMs == null) throw new IllegalStateException("Missing required property: activeMs");
            return new WorkflowAgentSummary(agentId, toolCallId, runId, phaseId, label, displayName, agentType, status, model, reasoningEffort, context, startedAt, completedAt, activeMs, consumedNanoAiu, activity);
        }
    }
}
