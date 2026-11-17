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
public record SessionUpdateStateOptions(
    /** Optional path for trajectory output. */
    @JsonProperty("trajectoryFile") String trajectoryFile,
    /** Override directory for the session-events log. When unset, the runtime's default events log directory is used. */
    @JsonProperty("eventsLogDirectory") String eventsLogDirectory,
    /** Whether subagent callback events should be forwarded into the session event log sink. */
    @JsonProperty("eventsLogIncludesSubagents") Boolean eventsLogIncludesSubagents,
    /** Whether to skip embedding retrieval pipeline initialization and execution. */
    @JsonProperty("skipEmbeddingRetrieval") Boolean skipEmbeddingRetrieval,
    /** Whether to enable cross-session store writes and reads. */
    @JsonProperty("enableCrossSessionStore") Boolean enableCrossSessionStore
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateStateOptions {}

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
        private Boolean skipEmbeddingRetrieval;
        private Boolean enableCrossSessionStore;

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
         * Sets the {@code skipEmbeddingRetrieval} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skipEmbeddingRetrieval(Boolean value) {
            this.skipEmbeddingRetrieval = value;
            return this;
        }

        /**
         * Sets the {@code enableCrossSessionStore} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableCrossSessionStore(Boolean value) {
            this.enableCrossSessionStore = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateStateOptions build() {
            return new SessionUpdateStateOptions(trajectoryFile, eventsLogDirectory, eventsLogIncludesSubagents, skipEmbeddingRetrieval, enableCrossSessionStore);
        }
    }
}
