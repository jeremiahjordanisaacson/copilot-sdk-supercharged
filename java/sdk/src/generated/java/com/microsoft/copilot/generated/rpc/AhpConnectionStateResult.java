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
 * The connection's current protocol snapshot and negotiated client identity.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpConnectionStateResult(
    /** Opaque connection identifier scoped to the originating SDK connection. */
    @JsonProperty("connectionId") String connectionId,
    /** Serialized AHP relay state, including its monotonic revision. */
    @JsonProperty("stateJson") String stateJson,
    /** Effective client identity, including any Mission Control assignment. */
    @JsonProperty("clientId") String clientId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionStateResult {}

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
        private String connectionId;
        private String stateJson;
        private String clientId;

        /**
         * Sets the {@code connectionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder connectionId(String value) {
            this.connectionId = value;
            return this;
        }

        /**
         * Sets the {@code stateJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder stateJson(String value) {
            this.stateJson = value;
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
        public AhpConnectionStateResult build() {
            if (connectionId == null) throw new IllegalStateException("Missing required property: connectionId");
            if (stateJson == null) throw new IllegalStateException("Missing required property: stateJson");
            if (clientId == null) throw new IllegalStateException("Missing required property: clientId");
            return new AhpConnectionStateResult(connectionId, stateJson, clientId);
        }
    }
}
