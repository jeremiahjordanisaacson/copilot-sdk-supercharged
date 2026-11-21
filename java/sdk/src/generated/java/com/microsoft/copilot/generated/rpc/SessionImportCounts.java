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
 * Counts of prepared or imported session records.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionImportCounts(
    /** Number of sessions created. */
    @JsonProperty("sessions") Long sessions,
    /** Number of message records. */
    @JsonProperty("messages") Long messages,
    /** Number of tool call records. */
    @JsonProperty("toolCalls") Long toolCalls,
    /** Number of tool result records. */
    @JsonProperty("toolResults") Long toolResults,
    /** Number of inline attachments. */
    @JsonProperty("attachments") Long attachments
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionImportCounts {}

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
        private Long sessions;
        private Long messages;
        private Long toolCalls;
        private Long toolResults;
        private Long attachments;

        /**
         * Sets the {@code sessions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessions(Long value) {
            this.sessions = value;
            return this;
        }

        /**
         * Sets the {@code messages} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder messages(Long value) {
            this.messages = value;
            return this;
        }

        /**
         * Sets the {@code toolCalls} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder toolCalls(Long value) {
            this.toolCalls = value;
            return this;
        }

        /**
         * Sets the {@code toolResults} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder toolResults(Long value) {
            this.toolResults = value;
            return this;
        }

        /**
         * Sets the {@code attachments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder attachments(Long value) {
            this.attachments = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionImportCounts build() {
            if (sessions == null) throw new IllegalStateException("Missing required property: sessions");
            if (messages == null) throw new IllegalStateException("Missing required property: messages");
            if (toolCalls == null) throw new IllegalStateException("Missing required property: toolCalls");
            if (toolResults == null) throw new IllegalStateException("Missing required property: toolResults");
            if (attachments == null) throw new IllegalStateException("Missing required property: attachments");
            return new SessionImportCounts(sessions, messages, toolCalls, toolResults, attachments);
        }
    }
}
