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
 * A created or reused worktree.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitPreparedWorktree(
    /** Worktree directory. */
    @JsonProperty("path") String path,
    /** Actual branch name. */
    @JsonProperty("branch") String branch,
    /** Whether an existing worktree was reused. */
    @JsonProperty("reused") Boolean reused
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitPreparedWorktree {}

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
        private String branch;
        private Boolean reused;

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
         * Sets the {@code branch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder branch(String value) {
            this.branch = value;
            return this;
        }

        /**
         * Sets the {@code reused} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reused(Boolean value) {
            this.reused = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitPreparedWorktree build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (branch == null) throw new IllegalStateException("Missing required property: branch");
            if (reused == null) throw new IllegalStateException("Missing required property: reused");
            return new GitPreparedWorktree(path, branch, reused);
        }
    }
}
