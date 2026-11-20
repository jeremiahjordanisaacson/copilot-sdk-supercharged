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
 * The result of atomically consuming the user's first-enable sandbox marker.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SandboxConsumeOnboardingResult(
    /** True only when this call first records that the user saw sandbox onboarding. False when a previous call or sandbox enable command already recorded the marker. */
    @JsonProperty("firstEnable") Boolean firstEnable
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxConsumeOnboardingResult {}

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
        private Boolean firstEnable;

        /**
         * Sets the {@code firstEnable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder firstEnable(Boolean value) {
            this.firstEnable = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxConsumeOnboardingResult build() {
            if (firstEnable == null) throw new IllegalStateException("Missing required property: firstEnable");
            return new SandboxConsumeOnboardingResult(firstEnable);
        }
    }
}
