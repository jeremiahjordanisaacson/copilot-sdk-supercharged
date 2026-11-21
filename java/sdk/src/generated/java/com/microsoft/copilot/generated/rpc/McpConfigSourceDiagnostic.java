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
 * A source file or server entry that the MCP composition omitted.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpConfigSourceDiagnostic(
    /** Source file that contains the problem. */
    @JsonProperty("path") String path,
    /** Diagnostic from the existing MCP parser. */
    @JsonProperty("message") String message,
    /** Either malformed, unreadable, or invalid-server. */
    @JsonProperty("reason") String reason
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpConfigSourceDiagnostic {}

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
        private String path;
        private String message;
        private String reason;

        /**
         * Sets the {@code path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder path(String value) {
            this.path = value;
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
         * Sets the {@code reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reason(String value) {
            this.reason = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpConfigSourceDiagnostic build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            if (reason == null) throw new IllegalStateException("Missing required property: reason");
            return new McpConfigSourceDiagnostic(path, message, reason);
        }
    }
}
