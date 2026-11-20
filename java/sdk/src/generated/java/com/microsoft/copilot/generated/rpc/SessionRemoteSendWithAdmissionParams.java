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
 * One trusted CLI host's message and its invocation-local admission identity.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionRemoteSendWithAdmissionParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Opaque identity registered by the originating client before this request. */
    @JsonProperty("admissionId") String admissionId,
    /** Serialized canonical SendRequest, including clientTurnId and all supplied message options.
The runtime validates these options and owns their preparation and routing. */
    @JsonProperty("requestJson") String requestJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionRemoteSendWithAdmissionParams {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String sessionId;
        private String admissionId;
        private String requestJson;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code admissionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder admissionId(String value) {
            this.admissionId = value;
            return this;
        }

        /**
         * Sets the {@code requestJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        public SessionRemoteSendWithAdmissionParams build() {
            if (admissionId == null) throw new IllegalStateException("Missing required property: admissionId");
            if (requestJson == null) throw new IllegalStateException("Missing required property: requestJson");
            return new SessionRemoteSendWithAdmissionParams(sessionId, admissionId, requestJson);
        }
    }
}
