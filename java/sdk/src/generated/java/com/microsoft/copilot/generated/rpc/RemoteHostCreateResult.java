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
 * An allocated, inert outgoing connection.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record RemoteHostCreateResult(
    /** Application-selected connection UUID. */
    @JsonProperty("hostId") String hostId,
    /** Correlation identifier for existing host event consumers. */
    @JsonProperty("relayId") Long relayId,
    /** Canonical initial engine state. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("state") Object state,
    /** Initial effective client identity. */
    @JsonProperty("clientId") String clientId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteHostCreateResult {}

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
        private String hostId;
        private Long relayId;
        private Object state;
        private boolean $presence2;
        private String clientId;

        /**
         * Sets the {@code hostId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hostId(String value) {
            this.hostId = value;
            return this;
        }

        /**
         * Sets the {@code relayId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder relayId(Long value) {
            this.relayId = value;
            return this;
        }

        /**
         * Sets the {@code state} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder state(Object value) {
            this.state = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code clientId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder clientId(String value) {
            this.clientId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCreateResult build() {
            if (hostId == null) throw new IllegalStateException("Missing required property: hostId");
            if (relayId == null) throw new IllegalStateException("Missing required property: relayId");
            if (!$presence2) throw new IllegalStateException("Missing required property: state");
            if (clientId == null) throw new IllegalStateException("Missing required property: clientId");
            return new RemoteHostCreateResult(hostId, relayId, state, clientId);
        }
    }
}
