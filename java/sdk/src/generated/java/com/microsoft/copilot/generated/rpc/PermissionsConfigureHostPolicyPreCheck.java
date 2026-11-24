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
 * Owner-authenticated declaration for the non-prompting host-policy pre-check. The runtime accepts a decision only for the declared revision, which every SDK represents exactly.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record PermissionsConfigureHostPolicyPreCheck(
    /** Whether this connection declares the host-policy pre-check. */
    @JsonProperty("enabled") Boolean enabled,
    /** Host policy revision from 0 through 2^53 - 1. */
    @JsonProperty("policyRevision") Long policyRevision
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PermissionsConfigureHostPolicyPreCheck {}

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
        private Boolean enabled;
        private Long policyRevision;

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Sets the {@code policyRevision} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder policyRevision(Long value) {
            this.policyRevision = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PermissionsConfigureHostPolicyPreCheck build() {
            if (enabled == null) throw new IllegalStateException("Missing required property: enabled");
            if (policyRevision == null) throw new IllegalStateException("Missing required property: policyRevision");
            return new PermissionsConfigureHostPolicyPreCheck(enabled, policyRevision);
        }
    }
}
