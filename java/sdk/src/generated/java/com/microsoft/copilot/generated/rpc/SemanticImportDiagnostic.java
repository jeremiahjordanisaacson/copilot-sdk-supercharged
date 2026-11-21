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
 * One actionable semantic import diagnostic.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SemanticImportDiagnostic(
    /** One-based input line. */
    @JsonProperty("line") Long line,
    /** Stable diagnostic code. */
    @JsonProperty("code") String code,
    /** Explanation of the invalid input or failed operation. */
    @JsonProperty("message") String message,
    /** External record identifier, when present. */
    @JsonProperty("externalId") String externalId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SemanticImportDiagnostic {}

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
        private Long line;
        private String code;
        private String message;
        private String externalId;

        /**
         * Sets the {@code line} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder line(Long value) {
            this.line = value;
            return this;
        }

        /**
         * Sets the {@code code} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder code(String value) {
            this.code = value;
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
         * Sets the {@code externalId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder externalId(String value) {
            this.externalId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SemanticImportDiagnostic build() {
            if (line == null) throw new IllegalStateException("Missing required property: line");
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            return new SemanticImportDiagnostic(line, code, message, externalId);
        }
    }
}
