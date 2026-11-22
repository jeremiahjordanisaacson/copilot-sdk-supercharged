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
 * A timestamped service message.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIServiceLogEntry(
    /** Time in milliseconds since the Unix epoch. */
    @JsonProperty("timestamp") Double timestamp,
    /** Display message. */
    @JsonProperty("message") String message
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIServiceLogEntry {}

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
        private Double timestamp;
        private String message;

        /**
         * Sets the {@code timestamp} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder timestamp(Double value) {
            this.timestamp = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIServiceLogEntry build() {
            if (timestamp == null) throw new IllegalStateException("Missing required property: timestamp");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            return new UIServiceLogEntry(timestamp, message);
        }
    }
}
