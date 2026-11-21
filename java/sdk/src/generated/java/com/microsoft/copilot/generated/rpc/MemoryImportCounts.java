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
 * Counts of memory batch outcomes.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record MemoryImportCounts(
    /** Number of validated input records. */
    @JsonProperty("total") Long total,
    /** Number of records stored. */
    @JsonProperty("imported") Long imported,
    /** Number of conflicts skipped. */
    @JsonProperty("skipped") Long skipped,
    /** Number of failed records. */
    @JsonProperty("failed") Long failed
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public MemoryImportCounts {}

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
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
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
        public MemoryImportCounts build() {
            if (total == null) throw new IllegalStateException("Missing required property: total");
            if (imported == null) throw new IllegalStateException("Missing required property: imported");
            if (skipped == null) throw new IllegalStateException("Missing required property: skipped");
            if (failed == null) throw new IllegalStateException("Missing required property: failed");
            return new MemoryImportCounts(total, imported, skipped, failed);
        }
    }
}
