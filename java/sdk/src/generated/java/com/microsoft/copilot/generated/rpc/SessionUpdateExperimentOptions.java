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
 * Feature experimentation and trusted assignments.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionUpdateExperimentOptions(
    /** Map of feature-flag IDs to their boolean enabled state. */
    @JsonProperty("flags") Map<String, Boolean> flags,
    /** Whether experimental capabilities are enabled. */
    @JsonProperty("enableExperimentalMode") Boolean enableExperimentalMode
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateExperimentOptions {}

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
        private Map<String, Boolean> flags;
        private Boolean enableExperimentalMode;

        /**
         * Sets the {@code flags} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flags(Map<String, Boolean> value) {
            this.flags = value;
            return this;
        }

        /**
         * Sets the {@code enableExperimentalMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableExperimentalMode(Boolean value) {
            this.enableExperimentalMode = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateExperimentOptions build() {
            return new SessionUpdateExperimentOptions(flags, enableExperimentalMode);
        }
    }
}
