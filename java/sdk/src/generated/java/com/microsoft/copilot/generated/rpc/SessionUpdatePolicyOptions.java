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
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Host-injected session policy.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionUpdatePolicyOptions(
    /** Additional content-exclusion policies to merge into the session's policy set. */
    @CopilotExperimental
    @JsonProperty("additionalContentExclusionPolicies") List<OptionsUpdateAdditionalContentExclusionPolicy> additionalContentExclusionPolicies
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdatePolicyOptions {}

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
        private List<OptionsUpdateAdditionalContentExclusionPolicy> additionalContentExclusionPolicies;

        /**
         * Sets the {@code additionalContentExclusionPolicies} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder additionalContentExclusionPolicies(List<OptionsUpdateAdditionalContentExclusionPolicy> value) {
            this.additionalContentExclusionPolicies = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdatePolicyOptions build() {
            return new SessionUpdatePolicyOptions(additionalContentExclusionPolicies);
        }
    }
}
