/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * Current state of a background sidekick agent.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIBackgroundAgentTask(
    /** Task kind, always agent. */
    @JsonProperty("type") String type,
    /** Registry task identity. */
    @JsonProperty("id") String id,
    /** Tool call associated with the agent. */
    @JsonProperty("toolCallId") String toolCallId,
    /** Friendly display name. */
    @JsonProperty("displayName") String displayName,
    /** Task description. */
    @JsonProperty("description") String description,
    /** Current lifecycle status. */
    @JsonProperty("status") String status,
    /** Start time in milliseconds since the Unix epoch. */
    @JsonProperty("startedAt") Double startedAt,
    /** Completion time in milliseconds since the Unix epoch. */
    @JsonProperty("completedAt") Double completedAt,
    /** Accumulated active time in milliseconds. */
    @JsonProperty("activeTimeMs") Double activeTimeMs,
    /** Start of the current active period in milliseconds since the Unix epoch. */
    @JsonProperty("activeStartedAt") Double activeStartedAt,
    /** Failure text, when present. */
    @JsonProperty("error") String error,
    /** Agent type. */
    @JsonProperty("agentType") String agentType,
    /** Most recent prompt. */
    @JsonProperty("prompt") String prompt,
    /** Completed result, when present. */
    @JsonProperty("result") String result,
    /** Requested model override. */
    @JsonProperty("modelOverride") String modelOverride,
    /** Model selected by the runtime. */
    @JsonProperty("resolvedModel") String resolvedModel,
    /** Synchronous or background execution. */
    @JsonProperty("executionMode") String executionMode,
    /** Whether the task can move to background execution. */
    @JsonProperty("canPromoteToBackground") Boolean canPromoteToBackground,
    /** Most recent agent response. */
    @JsonProperty("latestResponse") String latestResponse,
    /** Idle start time in milliseconds since the Unix epoch. */
    @JsonProperty("idleSince") Double idleSince,
    /** Event source when the task derives from history. */
    @JsonProperty("source") String source
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIBackgroundAgentTask {}

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
        private String type;
        private String id;
        private String toolCallId;
        private String displayName;
        private String description;
        private String status;
        private Double startedAt;
        private Double completedAt;
        private Double activeTimeMs;
        private Double activeStartedAt;
        private String error;
        private String agentType;
        private String prompt;
        private String result;
        private String modelOverride;
        private String resolvedModel;
        private String executionMode;
        private Boolean canPromoteToBackground;
        private String latestResponse;
        private Double idleSince;
        private String source;

        /**
         * Sets the {@code type} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder type(String value) {
            this.type = value;
            return this;
        }

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder id(String value) {
            this.id = value;
            return this;
        }

        /**
         * Sets the {@code toolCallId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder toolCallId(String value) {
            this.toolCallId = value;
            return this;
        }

        /**
         * Sets the {@code displayName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder displayName(String value) {
            this.displayName = value;
            return this;
        }

        /**
         * Sets the {@code description} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(String value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code startedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder startedAt(Double value) {
            this.startedAt = value;
            return this;
        }

        /**
         * Sets the {@code completedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder completedAt(Double value) {
            this.completedAt = value;
            return this;
        }

        /**
         * Sets the {@code activeTimeMs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder activeTimeMs(Double value) {
            this.activeTimeMs = value;
            return this;
        }

        /**
         * Sets the {@code activeStartedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder activeStartedAt(Double value) {
            this.activeStartedAt = value;
            return this;
        }

        /**
         * Sets the {@code error} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder error(String value) {
            this.error = value;
            return this;
        }

        /**
         * Sets the {@code agentType} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agentType(String value) {
            this.agentType = value;
            return this;
        }

        /**
         * Sets the {@code prompt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder prompt(String value) {
            this.prompt = value;
            return this;
        }

        /**
         * Sets the {@code result} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder result(String value) {
            this.result = value;
            return this;
        }

        /**
         * Sets the {@code modelOverride} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder modelOverride(String value) {
            this.modelOverride = value;
            return this;
        }

        /**
         * Sets the {@code resolvedModel} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder resolvedModel(String value) {
            this.resolvedModel = value;
            return this;
        }

        /**
         * Sets the {@code executionMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder executionMode(String value) {
            this.executionMode = value;
            return this;
        }

        /**
         * Sets the {@code canPromoteToBackground} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder canPromoteToBackground(Boolean value) {
            this.canPromoteToBackground = value;
            return this;
        }

        /**
         * Sets the {@code latestResponse} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder latestResponse(String value) {
            this.latestResponse = value;
            return this;
        }

        /**
         * Sets the {@code idleSince} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder idleSince(Double value) {
            this.idleSince = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(String value) {
            this.source = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIBackgroundAgentTask build() {
            if (type == null) throw new IllegalStateException("Missing required property: type");
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (startedAt == null) throw new IllegalStateException("Missing required property: startedAt");
            if (agentType == null) throw new IllegalStateException("Missing required property: agentType");
            if (prompt == null) throw new IllegalStateException("Missing required property: prompt");
            return new UIBackgroundAgentTask(type, id, toolCallId, displayName, description, status, startedAt, completedAt, activeTimeMs, activeStartedAt, error, agentType, prompt, result, modelOverride, resolvedModel, executionMode, canPromoteToBackground, latestResponse, idleSince, source);
        }
    }
}
