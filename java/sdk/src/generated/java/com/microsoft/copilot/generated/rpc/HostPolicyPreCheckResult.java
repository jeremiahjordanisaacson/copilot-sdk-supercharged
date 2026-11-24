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
 * Host policy decision and the exact policy revision used to make it.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record HostPolicyPreCheckResult(
    /** Whether host policy approves or defers this request. */
    @JsonProperty("decision") HostPolicyPreCheckDecision decision,
    /** Policy revision used for the decision. */
    @JsonProperty("policyRevision") Long policyRevision
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    HostPolicyPreCheckResult {}

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
        private HostPolicyPreCheckDecision decision;
        private Long policyRevision;

        /**
         * Sets the {@code decision} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder decision(HostPolicyPreCheckDecision value) {
            this.decision = value;
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
        public HostPolicyPreCheckResult build() {
            if (decision == null) throw new IllegalStateException("Missing required property: decision");
            if (policyRevision == null) throw new IllegalStateException("Missing required property: policyRevision");
            return new HostPolicyPreCheckResult(decision, policyRevision);
        }
    }
}
