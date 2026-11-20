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
 * Repository and branch that own a canonical Forge proposal.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ForgeProposalScope(
    /** Repository owner, absent when the checkout has no known remote. */
    @JsonProperty("repo_owner") String repoOwner,
    /** Repository name, absent when the checkout has no known remote. */
    @JsonProperty("repo_name") String repoName,
    /** Working tree root, in the existing persisted path format. */
    @JsonProperty("git_root_path") String gitRootPath,
    /** Branch name, or the existing unknown sentinel. */
    @JsonProperty("branch_name") String branchName
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ForgeProposalScope {}

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
        private String repoOwner;
        private String repoName;
        private String gitRootPath;
        private String branchName;

        /**
         * Sets the {@code repo_owner} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder repoOwner(String value) {
            this.repoOwner = value;
            return this;
        }

        /**
         * Sets the {@code repo_name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder repoName(String value) {
            this.repoName = value;
            return this;
        }

        /**
         * Sets the {@code git_root_path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitRootPath(String value) {
            this.gitRootPath = value;
            return this;
        }

        /**
         * Sets the {@code branch_name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder branchName(String value) {
            this.branchName = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ForgeProposalScope build() {
            if (gitRootPath == null) throw new IllegalStateException("Missing required property: git_root_path");
            if (branchName == null) throw new IllegalStateException("Missing required property: branch_name");
            return new ForgeProposalScope(repoOwner, repoName, gitRootPath, branchName);
        }
    }
}
