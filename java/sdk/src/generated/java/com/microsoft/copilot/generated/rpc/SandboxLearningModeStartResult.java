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
 * How shell commands start under the managed learning policy.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SandboxLearningModeStartResult(
    /** Whether the host can allow and record denials. */
    @JsonProperty("relaxed") Boolean relaxed,
    /** Explanation when the host cannot apply the managed learning policy. */
    @JsonProperty("notice") String notice
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxLearningModeStartResult {}

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
        private Boolean relaxed;
        private String notice;

        /**
         * Sets the {@code relaxed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder relaxed(Boolean value) {
            this.relaxed = value;
            return this;
        }

        /**
         * Sets the {@code notice} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder notice(String value) {
            this.notice = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxLearningModeStartResult build() {
            if (relaxed == null) throw new IllegalStateException("Missing required property: relaxed");
            return new SandboxLearningModeStartResult(relaxed, notice);
        }
    }
}
