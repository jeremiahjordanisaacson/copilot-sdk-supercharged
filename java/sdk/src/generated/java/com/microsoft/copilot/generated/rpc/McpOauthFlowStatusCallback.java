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
 * Progress from an OAuth attempt to its original SDK requester.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpOauthFlowStatusCallback(
    /** Session that owns the authentication attempt. */
    @JsonProperty("sessionId") String sessionId,
    /** Identity reserved by the original requester. */
    @JsonProperty("flowId") String flowId,
    /** Current OAuth flow status. */
    @JsonProperty("status") String status,
    /** Status detail for the host UI, or null when absent. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("message") String message,
    /** Serialized device authorization details, or null when absent. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("payloadJson") String payloadJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpOauthFlowStatusCallback {}

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
        private String flowId;
        private String status;
        private String message;
        private boolean $presence3;
        private String payloadJson;
        private boolean $presence4;

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
         * Sets the {@code flowId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flowId(String value) {
            this.flowId = value;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(String value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder message(String value) {
            this.message = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code payloadJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder payloadJson(String value) {
            this.payloadJson = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpOauthFlowStatusCallback build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (flowId == null) throw new IllegalStateException("Missing required property: flowId");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (!$presence3) throw new IllegalStateException("Missing required property: message");
            if (!$presence4) throw new IllegalStateException("Missing required property: payloadJson");
            return new McpOauthFlowStatusCallback(sessionId, flowId, status, message, payloadJson);
        }
    }
}
