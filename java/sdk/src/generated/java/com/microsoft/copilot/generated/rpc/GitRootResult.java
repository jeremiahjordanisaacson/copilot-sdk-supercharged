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
 * Repository discovery, including the explicit not-found result.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitRootResult(
    /** Root path, or the resolver's empty path when no repository exists. */
    @JsonProperty("gitRoot") String gitRoot,
    /** Whether the directory belongs to a repository. */
    @JsonProperty("found") Boolean found
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitRootResult {}

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
        private Boolean found;

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
         * Sets the {@code found} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder found(Boolean value) {
            this.found = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitRootResult build() {
            if (gitRoot == null) throw new IllegalStateException("Missing required property: gitRoot");
            if (found == null) throw new IllegalStateException("Missing required property: found");
            return new GitRootResult(gitRoot, found);
        }
    }
}
