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
 * Parameters for an unstaged repository diff without a live model session.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitDiffParams(
    /** Absolute working directory. Changes cover its entire Git working tree. */
    @JsonProperty("cwd") String cwd,
    /** Ignore whitespace-only changes. Defaults to false. */
    @JsonProperty("ignoreWhitespace") Boolean ignoreWhitespace
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitDiffParams {}

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
        private String cwd;
        private Boolean ignoreWhitespace;

        /**
         * Sets the {@code cwd} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder cwd(String value) {
            this.cwd = value;
            return this;
        }

        /**
         * Sets the {@code ignoreWhitespace} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ignoreWhitespace(Boolean value) {
            this.ignoreWhitespace = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitDiffParams build() {
            if (cwd == null) throw new IllegalStateException("Missing required property: cwd");
            return new GitDiffParams(cwd, ignoreWhitespace);
        }
    }
}
