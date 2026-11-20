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
 * Existing persisted counts for a proposal.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ForgeProposalSummary(
    /** Total changed files. */
    @JsonProperty("file_count") Long fileCount,
    /** Added files. */
    @JsonProperty("added_count") Long addedCount,
    /** Modified files. */
    @JsonProperty("modified_count") Long modifiedCount,
    /** Deleted files. */
    @JsonProperty("deleted_count") Long deletedCount,
    /** Renamed files. */
    @JsonProperty("renamed_count") Long renamedCount
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ForgeProposalSummary {}

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
        private Long fileCount;
        private Long addedCount;
        private Long modifiedCount;
        private Long deletedCount;
        private Long renamedCount;

        /**
         * Sets the {@code file_count} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fileCount(Long value) {
            this.fileCount = value;
            return this;
        }

        /**
         * Sets the {@code added_count} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder addedCount(Long value) {
            this.addedCount = value;
            return this;
        }

        /**
         * Sets the {@code modified_count} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder modifiedCount(Long value) {
            this.modifiedCount = value;
            return this;
        }

        /**
         * Sets the {@code deleted_count} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder deletedCount(Long value) {
            this.deletedCount = value;
            return this;
        }

        /**
         * Sets the {@code renamed_count} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder renamedCount(Long value) {
            this.renamedCount = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ForgeProposalSummary build() {
            if (fileCount == null) throw new IllegalStateException("Missing required property: file_count");
            if (addedCount == null) throw new IllegalStateException("Missing required property: added_count");
            if (modifiedCount == null) throw new IllegalStateException("Missing required property: modified_count");
            if (deletedCount == null) throw new IllegalStateException("Missing required property: deleted_count");
            if (renamedCount == null) throw new IllegalStateException("Missing required property: renamed_count");
            return new ForgeProposalSummary(fileCount, addedCount, modifiedCount, deletedCount, renamedCount);
        }
    }
}
