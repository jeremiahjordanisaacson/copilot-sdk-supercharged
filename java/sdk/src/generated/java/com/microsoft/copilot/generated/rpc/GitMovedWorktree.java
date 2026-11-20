/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * A worktree with restored local changes.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitMovedWorktree(
    /** Destination directory. */
    @JsonProperty("path") String path,
    /** Actual branch name. */
    @JsonProperty("branch") String branch,
    /** Whether changes moved. */
    @JsonProperty("movedChanges") Boolean movedChanges,
    /** Submodules whose local changes remain in the source checkout. */
    @JsonProperty("leftBehindSubmodules") List<String> leftBehindSubmodules,
    /** Untracked nested repositories that remain in the source checkout. */
    @JsonProperty("leftBehindRepositories") List<String> leftBehindRepositories
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitMovedWorktree {}

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
        private Boolean movedChanges;
        private List<String> leftBehindSubmodules;
        private List<String> leftBehindRepositories;

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
         * Sets the {@code movedChanges} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder movedChanges(Boolean value) {
            this.movedChanges = value;
            return this;
        }

        /**
         * Sets the {@code leftBehindSubmodules} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder leftBehindSubmodules(List<String> value) {
            this.leftBehindSubmodules = value;
            return this;
        }

        /**
         * Sets the {@code leftBehindRepositories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder leftBehindRepositories(List<String> value) {
            this.leftBehindRepositories = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitMovedWorktree build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (branch == null) throw new IllegalStateException("Missing required property: branch");
            if (movedChanges == null) throw new IllegalStateException("Missing required property: movedChanges");
            if (leftBehindSubmodules == null) throw new IllegalStateException("Missing required property: leftBehindSubmodules");
            if (leftBehindRepositories == null) throw new IllegalStateException("Missing required property: leftBehindRepositories");
            return new GitMovedWorktree(path, branch, movedChanges, leftBehindSubmodules, leftBehindRepositories);
        }
    }
}
