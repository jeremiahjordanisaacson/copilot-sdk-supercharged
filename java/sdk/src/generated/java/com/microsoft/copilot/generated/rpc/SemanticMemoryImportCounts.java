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
 * Number of submitted memories and their outcomes.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SemanticMemoryImportCounts(
    /** Total validated input records. */
    @JsonProperty("total") Long total,
    /** Successfully stored records. */
    @JsonProperty("imported") Long imported,
    /** Duplicate or conflicting records skipped by policy. */
    @JsonProperty("skipped") Long skipped,
    /** Failed records. */
    @JsonProperty("failed") Long failed
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SemanticMemoryImportCounts {}

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
        private Long total;
        private Long imported;
        private Long skipped;
        private Long failed;

        /**
         * Sets the {@code total} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder total(Long value) {
            this.total = value;
            return this;
        }

        /**
         * Sets the {@code imported} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder imported(Long value) {
            this.imported = value;
            return this;
        }

        /**
         * Sets the {@code skipped} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skipped(Long value) {
            this.skipped = value;
            return this;
        }

        /**
         * Sets the {@code failed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder failed(Long value) {
            this.failed = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SemanticMemoryImportCounts build() {
            if (total == null) throw new IllegalStateException("Missing required property: total");
            if (imported == null) throw new IllegalStateException("Missing required property: imported");
            if (skipped == null) throw new IllegalStateException("Missing required property: skipped");
            if (failed == null) throw new IllegalStateException("Missing required property: failed");
            return new SemanticMemoryImportCounts(total, imported, skipped, failed);
        }
    }
}
