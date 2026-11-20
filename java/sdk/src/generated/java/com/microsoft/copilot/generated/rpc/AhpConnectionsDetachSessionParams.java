/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Releases this caller's runtime facade without disposing the host's session.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpConnectionsDetachSessionParams(
    /** Runtime session identity returned by ahp.connections.attachSession. */
    @JsonProperty("sessionId") String sessionId,
    /** Lease returned to this facade by attachSession. */
    @JsonProperty("attachmentId") String attachmentId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionsDetachSessionParams {}

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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionsDetachSessionParams build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (attachmentId == null) throw new IllegalStateException("Missing required property: attachmentId");
            return new AhpConnectionsDetachSessionParams(sessionId, attachmentId);
        }
    }
}
