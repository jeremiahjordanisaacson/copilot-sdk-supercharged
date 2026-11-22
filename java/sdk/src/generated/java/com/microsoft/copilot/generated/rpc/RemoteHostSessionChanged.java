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
 * A session action projected by the existing relay engine.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record RemoteHostSessionChanged(
    /** Owned outgoing connection UUID. */
    @JsonProperty("hostId") String hostId,
    /** Subscribed AHP session URI. */
    @JsonProperty("sessionUri") String sessionUri,
    /** Existing engine projection of the remote action. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("dispatch") Object dispatch
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteHostSessionChanged {}

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
        private String sessionUri;
        private Object dispatch;
        private boolean $presence2;

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
         * Sets the {@code sessionUri} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionUri(String value) {
            this.sessionUri = value;
            return this;
        }

        /**
         * Sets the {@code dispatch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder dispatch(Object value) {
            this.dispatch = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostSessionChanged build() {
            if (hostId == null) throw new IllegalStateException("Missing required property: hostId");
            if (sessionUri == null) throw new IllegalStateException("Missing required property: sessionUri");
            if (!$presence2) throw new IllegalStateException("Missing required property: dispatch");
            return new RemoteHostSessionChanged(hostId, sessionUri, dispatch);
        }
    }
}
