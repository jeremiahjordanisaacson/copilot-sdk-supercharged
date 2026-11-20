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
 * Counts of index, checkout, and untracked changes.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitChangeCountsResult(
    /** Number of index changes. */
    @JsonProperty("staged") Long staged,
    /** Number of tracked checkout changes. */
    @JsonProperty("unstaged") Long unstaged,
    /** Number of untracked entries. */
    @JsonProperty("untracked") Long untracked,
    /** Whether any status entries exist. */
    @JsonProperty("hasChanges") Boolean hasChanges
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitChangeCountsResult {}

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
        private Long staged;
        private Long unstaged;
        private Long untracked;
        private Boolean hasChanges;

        /**
         * Sets the {@code staged} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder staged(Long value) {
            this.staged = value;
            return this;
        }

        /**
         * Sets the {@code unstaged} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder unstaged(Long value) {
            this.unstaged = value;
            return this;
        }

        /**
         * Sets the {@code untracked} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder untracked(Long value) {
            this.untracked = value;
            return this;
        }

        /**
         * Sets the {@code hasChanges} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasChanges(Boolean value) {
            this.hasChanges = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitChangeCountsResult build() {
            if (staged == null) throw new IllegalStateException("Missing required property: staged");
            if (unstaged == null) throw new IllegalStateException("Missing required property: unstaged");
            if (untracked == null) throw new IllegalStateException("Missing required property: untracked");
            if (hasChanges == null) throw new IllegalStateException("Missing required property: hasChanges");
            return new GitChangeCountsResult(staged, unstaged, untracked, hasChanges);
        }
    }
}
