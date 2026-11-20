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
 * Checkout changes to summarize.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitWorktreeChangeSummaryParams(
    /** Absolute repository root. */
    @JsonProperty("gitRoot") String gitRoot,
    /** Maximum number of files to include. */
    @JsonProperty("maxFiles") Long maxFiles
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitWorktreeChangeSummaryParams {}

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
        private String gitRoot;
        private Long maxFiles;

        /**
         * Sets the {@code gitRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitRoot(String value) {
            this.gitRoot = value;
            return this;
        }

        /**
         * Sets the {@code maxFiles} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder maxFiles(Long value) {
            this.maxFiles = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitWorktreeChangeSummaryParams build() {
            if (gitRoot == null) throw new IllegalStateException("Missing required property: gitRoot");
            if (maxFiles == null) throw new IllegalStateException("Missing required property: maxFiles");
            return new GitWorktreeChangeSummaryParams(gitRoot, maxFiles);
        }
    }
}
