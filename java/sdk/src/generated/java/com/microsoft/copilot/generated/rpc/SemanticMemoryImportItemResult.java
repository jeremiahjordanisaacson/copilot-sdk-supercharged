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
 * One semantic memory record outcome.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SemanticMemoryImportItemResult(
    /** One-based input line. */
    @JsonProperty("line") Long line,
    /** External record identifier, when present. */
    @JsonProperty("externalId") String externalId,
    /** Outcome: imported, skipped, failed, or validated. */
    @JsonProperty("status") SemanticMemoryImportItemStatus status,
    /** Stable operation code. */
    @JsonProperty("code") String code,
    /** Human-readable operation result. */
    @JsonProperty("message") String message
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SemanticMemoryImportItemResult {}

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
        private String externalId;
        private SemanticMemoryImportItemStatus status;
        private String code;
        private String message;

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
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(SemanticMemoryImportItemStatus value) {
            this.status = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SemanticMemoryImportItemResult build() {
            if (line == null) throw new IllegalStateException("Missing required property: line");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            return new SemanticMemoryImportItemResult(line, externalId, status, code, message);
        }
    }
}
