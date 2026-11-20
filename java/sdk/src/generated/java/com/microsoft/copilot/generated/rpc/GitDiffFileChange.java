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
 * One changed file and its bounded diff.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitDiffFileChange(
    /** Repository-relative file path. */
    @JsonProperty("path") String path,
    /** Unified diff. */
    @JsonProperty("diff") String diff,
    /** Change kind: added, modified, deleted, or renamed. */
    @JsonProperty("changeType") GitDiffChangeType changeType,
    /** Previous path for a rename. */
    @JsonProperty("oldPath") String oldPath,
    /** Whether the diff exceeds its size budget. */
    @JsonProperty("isTruncated") Boolean isTruncated
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitDiffFileChange {}

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
        private String diff;
        private GitDiffChangeType changeType;
        private String oldPath;
        private Boolean isTruncated;

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
         * Sets the {@code diff} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder diff(String value) {
            this.diff = value;
            return this;
        }

        /**
         * Sets the {@code changeType} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder changeType(GitDiffChangeType value) {
            this.changeType = value;
            return this;
        }

        /**
         * Sets the {@code oldPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder oldPath(String value) {
            this.oldPath = value;
            return this;
        }

        /**
         * Sets the {@code isTruncated} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder isTruncated(Boolean value) {
            this.isTruncated = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitDiffFileChange build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (diff == null) throw new IllegalStateException("Missing required property: diff");
            if (changeType == null) throw new IllegalStateException("Missing required property: changeType");
            return new GitDiffFileChange(path, diff, changeType, oldPath, isTruncated);
        }
    }
}
