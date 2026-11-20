/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Why the provider could not determine a sensitivity label.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SensitivityLabelFailure(
    /** Provider-defined failure category. */
    @JsonProperty("category") String category,
    /** Status code the provider reported for the failure, when present. */
    @JsonProperty("status") Long status,
    /** Whether the provider reported that retrying the request may determine a label. */
    @JsonProperty("retryable") Boolean retryable
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SensitivityLabelFailure {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String category;
        private Long status;
        private Boolean retryable;

        /**
         * Sets the {@code category} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder category(String value) {
            this.category = value;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder status(Long value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code retryable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder retryable(Boolean value) {
            this.retryable = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SensitivityLabelFailure build() {
            if (category == null) throw new IllegalStateException("Missing required property: category");
            if (retryable == null) throw new IllegalStateException("Missing required property: retryable");
            return new SensitivityLabelFailure(category, status, retryable);
        }
    }
}
