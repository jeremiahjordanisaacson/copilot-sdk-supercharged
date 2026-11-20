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
 * Tracked and untracked changes in a checkout.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitWorkingTreeStatus(
    /** Whether tracked files have unstaged changes. */
    @JsonProperty("hasUnstagedChanges") Boolean hasUnstagedChanges,
    /** Whether the index has staged changes. */
    @JsonProperty("hasStagedChanges") Boolean hasStagedChanges,
    /** Whether untracked files exist. */
    @JsonProperty("hasUntrackedFiles") Boolean hasUntrackedFiles
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitWorkingTreeStatus {}

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
        private Boolean hasUnstagedChanges;
        private Boolean hasStagedChanges;
        private Boolean hasUntrackedFiles;

        /**
         * Sets the {@code hasUnstagedChanges} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasUnstagedChanges(Boolean value) {
            this.hasUnstagedChanges = value;
            return this;
        }

        /**
         * Sets the {@code hasStagedChanges} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasStagedChanges(Boolean value) {
            this.hasStagedChanges = value;
            return this;
        }

        /**
         * Sets the {@code hasUntrackedFiles} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasUntrackedFiles(Boolean value) {
            this.hasUntrackedFiles = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitWorkingTreeStatus build() {
            if (hasUnstagedChanges == null) throw new IllegalStateException("Missing required property: hasUnstagedChanges");
            if (hasStagedChanges == null) throw new IllegalStateException("Missing required property: hasStagedChanges");
            if (hasUntrackedFiles == null) throw new IllegalStateException("Missing required property: hasUntrackedFiles");
            return new GitWorkingTreeStatus(hasUnstagedChanges, hasStagedChanges, hasUntrackedFiles);
        }
    }
}
