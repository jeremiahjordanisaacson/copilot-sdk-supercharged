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
 * The remote names configured in a checkout.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitRemotesResult(
    /** Remote names in Git's order. */
    @JsonProperty("remotes") List<String> remotes
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitRemotesResult {}

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
        private List<String> remotes;

        /**
         * Sets the {@code remotes} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder remotes(List<String> value) {
            this.remotes = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitRemotesResult build() {
            if (remotes == null) throw new IllegalStateException("Missing required property: remotes");
            return new GitRemotesResult(remotes);
        }
    }
}
