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
 * Opens the exact output target granted by the native host, never a request-selected path.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record LoggingFileOpenParams(
    /** Explicit verbosity. If absent, the runtime applies its debug environment and default level. */
    @JsonProperty("level") FileLogLevel level,
    /** Additional environment variable names that enable debug output when set to true or 1. */
    @JsonProperty("debugEnvironmentVariables") List<String> debugEnvironmentVariables,
    /** Additional secrets registered with the shared process filter before any log write. */
    @JsonProperty("secretValues") List<String> secretValues
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public LoggingFileOpenParams {}

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
        private FileLogLevel level;
        private List<String> debugEnvironmentVariables;
        private List<String> secretValues;

        /**
         * Sets the {@code level} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder level(FileLogLevel value) {
            this.level = value;
            return this;
        }

        /**
         * Sets the {@code debugEnvironmentVariables} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder debugEnvironmentVariables(List<String> value) {
            this.debugEnvironmentVariables = value;
            return this;
        }

        /**
         * Sets the {@code secretValues} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder secretValues(List<String> value) {
            this.secretValues = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public LoggingFileOpenParams build() {
            return new LoggingFileOpenParams(level, debugEnvironmentVariables, secretValues);
        }
    }
}
