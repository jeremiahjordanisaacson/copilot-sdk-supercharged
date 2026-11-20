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
 * Existing ready proposal or completed generation; null for an ineligible automatic generation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ForgeProposalReconcileResult(
    /** Canonical proposal selected or completed by reconciliation. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("proposal") ForgeProposalRecord proposal
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ForgeProposalReconcileResult {}

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
        private ForgeProposalRecord proposal;
        private boolean $presence0;

        /**
         * Sets the {@code proposal} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder proposal(ForgeProposalRecord value) {
            this.proposal = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ForgeProposalReconcileResult build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: proposal");
            return new ForgeProposalReconcileResult(proposal);
        }
    }
}
