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
 * Managed sandbox enforcement state for a session.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SandboxEnforcementStatus(
    /** Whether the effective managed policy requires an available sandbox backend. */
    @JsonProperty("required") Boolean required,
    /** Whether an enforcement failure has permanently blocked the session. */
    @JsonProperty("blocked") Boolean blocked,
    /** The first sandbox enforcement failure that blocked the session. */
    @JsonProperty("reason") String reason
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SandboxEnforcementStatus {}

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
        private Boolean required;
        private Boolean blocked;
        private String reason;

        /**
         * Sets the {@code required} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder required(Boolean value) {
            this.required = value;
            return this;
        }

        /**
         * Sets the {@code blocked} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder blocked(Boolean value) {
            this.blocked = value;
            return this;
        }

        /**
         * Sets the {@code reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reason(String value) {
            this.reason = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxEnforcementStatus build() {
            if (required == null) throw new IllegalStateException("Missing required property: required");
            if (blocked == null) throw new IllegalStateException("Missing required property: blocked");
            return new SandboxEnforcementStatus(required, blocked, reason);
        }
    }
}
