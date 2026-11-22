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
 * One host-owned step in the ACP telemetry transaction.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AcpTelemetryRequest(
    /** The transaction step. */
    @JsonProperty("action") AcpTelemetryAction action,
    /** The reservation produced by the prepare step. */
    @JsonProperty("preparationId") String preparationId,
    /** The session admitted by this transaction. */
    @JsonProperty("sessionId") String sessionId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AcpTelemetryRequest {}

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
        private AcpTelemetryAction action;
        private String preparationId;
        private String sessionId;

        /**
         * Sets the {@code action} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder action(AcpTelemetryAction value) {
            this.action = value;
            return this;
        }

        /**
         * Sets the {@code preparationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder preparationId(String value) {
            this.preparationId = value;
            return this;
        }

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AcpTelemetryRequest build() {
            if (action == null) throw new IllegalStateException("Missing required property: action");
            return new AcpTelemetryRequest(action, preparationId, sessionId);
        }
    }
}
