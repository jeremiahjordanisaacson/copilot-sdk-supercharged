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
 * Appends a redacted record to an owned file logger.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record LoggingFileWriteParams(
    /** Opaque connection-owned logger identifier. */
    @JsonProperty("logId") String logId,
    /** Operation whose level and category the runtime applies. */
    @JsonProperty("operation") FileLogOperation operation,
    /** Text to redact and append. Use an empty string for endGroup. */
    @JsonProperty("message") String message,
    /** Optional verbosity, with the runtime's existing group-level semantics. */
    @JsonProperty("groupLevel") FileLogLevel groupLevel
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public LoggingFileWriteParams {}

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
        private String logId;
        private FileLogOperation operation;
        private String message;
        private FileLogLevel groupLevel;

        /**
         * Sets the {@code logId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder logId(String value) {
            this.logId = value;
            return this;
        }

        /**
         * Sets the {@code operation} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder operation(FileLogOperation value) {
            this.operation = value;
            return this;
        }

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Sets the {@code groupLevel} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder groupLevel(FileLogLevel value) {
            this.groupLevel = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public LoggingFileWriteParams build() {
            if (logId == null) throw new IllegalStateException("Missing required property: logId");
            if (operation == null) throw new IllegalStateException("Missing required property: operation");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            return new LoggingFileWriteParams(logId, operation, message, groupLevel);
        }
    }
}
