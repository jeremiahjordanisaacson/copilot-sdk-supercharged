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
 * Context retention and session-state storage.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenStateOptions(
    /** Optional trajectory output file path. */
    @JsonProperty("trajectoryFile") String trajectoryFile,
    /** Override directory for session event logs. */
    @JsonProperty("eventsLogDirectory") String eventsLogDirectory,
    /** Whether subagent callback events should be forwarded into the session event log sink. */
    @JsonProperty("eventsLogIncludesSubagents") Boolean eventsLogIncludesSubagents,
    /** Memory configuration for this session. */
    @JsonProperty("memory") MemoryConfiguration memory
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenStateOptions {}

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
        private String trajectoryFile;
        private String eventsLogDirectory;
        private Boolean eventsLogIncludesSubagents;
        private MemoryConfiguration memory;

        /**
         * Sets the {@code trajectoryFile} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trajectoryFile(String value) {
            this.trajectoryFile = value;
            return this;
        }

        /**
         * Sets the {@code eventsLogDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder eventsLogDirectory(String value) {
            this.eventsLogDirectory = value;
            return this;
        }

        /**
         * Sets the {@code eventsLogIncludesSubagents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder eventsLogIncludesSubagents(Boolean value) {
            this.eventsLogIncludesSubagents = value;
            return this;
        }

        /**
         * Sets the {@code memory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder memory(MemoryConfiguration value) {
            this.memory = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenStateOptions build() {
            return new SessionOpenStateOptions(trajectoryFile, eventsLogDirectory, eventsLogIncludesSubagents, memory);
        }
    }
}
