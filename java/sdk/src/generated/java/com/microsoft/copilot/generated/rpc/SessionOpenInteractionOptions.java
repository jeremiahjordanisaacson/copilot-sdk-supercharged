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
 * Human input and mode-switch callback capabilities.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenInteractionOptions(
    /** Whether ask_user is explicitly disabled. */
    @JsonProperty("askUserDisabled") Boolean askUserDisabled,
    /** Whether the host is an interactive UI. */
    @JsonProperty("runningInInteractiveMode") Boolean runningInInteractiveMode
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenInteractionOptions {}

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
        private Boolean askUserDisabled;
        private Boolean runningInInteractiveMode;

        /**
         * Sets the {@code askUserDisabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder askUserDisabled(Boolean value) {
            this.askUserDisabled = value;
            return this;
        }

        /**
         * Sets the {@code runningInInteractiveMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder runningInInteractiveMode(Boolean value) {
            this.runningInInteractiveMode = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenInteractionOptions build() {
            return new SessionOpenInteractionOptions(askUserDisabled, runningInInteractiveMode);
        }
    }
}
