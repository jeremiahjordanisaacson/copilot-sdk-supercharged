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
 * A stored Markdown research report in the bound session's authoritative state directory.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ResearchStoredReport(
    /** The file name without its final .md extension. */
    @JsonProperty("id") String id,
    /** The identifier with hyphens replaced by spaces. */
    @JsonProperty("topic") String topic,
    /** The file modification time as a UTC ISO timestamp. */
    @JsonProperty("timestamp") String timestamp,
    /** The full report path on the runtime host. */
    @JsonProperty("filePath") String filePath
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ResearchStoredReport {}

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
        private String id;
        private String topic;
        private String timestamp;
        private String filePath;

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder id(String value) {
            this.id = value;
            return this;
        }

        /**
         * Sets the {@code topic} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder topic(String value) {
            this.topic = value;
            return this;
        }

        /**
         * Sets the {@code timestamp} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder timestamp(String value) {
            this.timestamp = value;
            return this;
        }

        /**
         * Sets the {@code filePath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder filePath(String value) {
            this.filePath = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ResearchStoredReport build() {
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (topic == null) throw new IllegalStateException("Missing required property: topic");
            if (timestamp == null) throw new IllegalStateException("Missing required property: timestamp");
            if (filePath == null) throw new IllegalStateException("Missing required property: filePath");
            return new ResearchStoredReport(id, topic, timestamp, filePath);
        }
    }
}
