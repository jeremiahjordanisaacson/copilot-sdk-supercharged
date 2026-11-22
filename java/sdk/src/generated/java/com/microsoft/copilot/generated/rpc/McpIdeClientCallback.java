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
 * One callback routed only to the engine's original observer.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpIdeClientCallback(
    /** Unique live-client identity, independent of server name and graph token. */
    @JsonProperty("callbackId") String callbackId,
    /** Canonical session that owns this client. */
    @JsonProperty("sessionId") String sessionId,
    /** Tool, protocol, cancellation, or release operation as JSON. */
    @JsonProperty("requestJson") String requestJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpIdeClientCallback {}

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
        private String callbackId;
        private String sessionId;
        private String requestJson;

        /**
         * Sets the {@code callbackId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder callbackId(String value) {
            this.callbackId = value;
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
         * Sets the {@code requestJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder requestJson(String value) {
            this.requestJson = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpIdeClientCallback build() {
            if (callbackId == null) throw new IllegalStateException("Missing required property: callbackId");
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (requestJson == null) throw new IllegalStateException("Missing required property: requestJson");
            return new McpIdeClientCallback(callbackId, sessionId, requestJson);
        }
    }
}
