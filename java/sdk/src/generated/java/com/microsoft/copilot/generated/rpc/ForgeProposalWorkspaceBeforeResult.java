/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Stored before-generation capture, or null when no capture exists.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ForgeProposalWorkspaceBeforeResult(
    /** Original path-to-content mapping; an empty map remains distinct from no capture. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("contents") Map<String, String> contents
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ForgeProposalWorkspaceBeforeResult {}

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
        private Map<String, String> contents;
        private boolean $presence0;

        /**
         * Sets the {@code contents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder contents(Map<String, String> value) {
            this.contents = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ForgeProposalWorkspaceBeforeResult build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: contents");
            return new ForgeProposalWorkspaceBeforeResult(contents);
        }
    }
}
