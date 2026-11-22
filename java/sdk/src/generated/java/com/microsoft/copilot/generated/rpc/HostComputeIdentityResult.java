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
 * Stable compute identity stored atomically in the canonical settings document.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record HostComputeIdentityResult(
    /** Existing or newly allocated UUID; malformed existing identities are never replaced. */
    @JsonProperty("computeId") String computeId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    HostComputeIdentityResult {}

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
        private String computeId;

        /**
         * Sets the {@code computeId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder computeId(String value) {
            this.computeId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HostComputeIdentityResult build() {
            if (computeId == null) throw new IllegalStateException("Missing required property: computeId");
            return new HostComputeIdentityResult(computeId);
        }
    }
}
