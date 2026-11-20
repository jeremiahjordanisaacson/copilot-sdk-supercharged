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
 * A registered runtime session and its independent attachment lease.
Read session.remote.readProjection before using host state.
The first read waits for subscription readiness and reports initialization errors.
Release the lease if initialization fails.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpSessionAttachResult(
    /** Runtime identity, distinct from the host's AHP URI. */
    @JsonProperty("sessionId") String sessionId,
    /** Lease for independent facade teardown on this SDK connection. */
    @JsonProperty("attachmentId") String attachmentId,
    /** Host metadata used for the SDK attachment. */
    @JsonProperty("metadata") AhpSessionMetadata metadata
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpSessionAttachResult {}

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
        private String sessionId;
        private String attachmentId;
        private AhpSessionMetadata metadata;

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
         * Sets the {@code attachmentId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder attachmentId(String value) {
            this.attachmentId = value;
            return this;
        }

        /**
         * Sets the {@code metadata} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder metadata(AhpSessionMetadata value) {
            this.metadata = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpSessionAttachResult build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (attachmentId == null) throw new IllegalStateException("Missing required property: attachmentId");
            if (metadata == null) throw new IllegalStateException("Missing required property: metadata");
            return new AhpSessionAttachResult(sessionId, attachmentId, metadata);
        }
    }
}
