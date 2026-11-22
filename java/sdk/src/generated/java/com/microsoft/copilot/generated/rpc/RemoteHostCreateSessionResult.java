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
 * The created session and the refresh already performed by its owning engine.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record RemoteHostCreateSessionResult(
    /** Newly created AHP session URI. */
    @JsonProperty("uri") String uri,
    /** Canonical engine state after creation. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("state") Object state,
    /** Effective client identity after the connection's negotiation. */
    @JsonProperty("clientId") String clientId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteHostCreateSessionResult {}

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
        private String uri;
        private Object state;
        private boolean $presence1;
        private String clientId;

        /**
         * Sets the {@code uri} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder uri(String value) {
            this.uri = value;
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
            this.$presence1 = true;
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
        public RemoteHostCreateSessionResult build() {
            if (uri == null) throw new IllegalStateException("Missing required property: uri");
            if (!$presence1) throw new IllegalStateException("Missing required property: state");
            if (clientId == null) throw new IllegalStateException("Missing required property: clientId");
            return new RemoteHostCreateSessionResult(uri, state, clientId);
        }
    }
}
