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
 * One request-owned admission decision, sent only to its originating SDK connection.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record RemoteCheckSendAdmissionParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Invocation identity supplied by the same client that owns the send. */
    @JsonProperty("admissionId") String admissionId,
    /** Recheck a deferred guard, or acknowledge the final irreversible admission. */
    @JsonProperty("phase") AhpSessionAdmissionPhase phase
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteCheckSendAdmissionParams {}

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
        private AhpSessionAdmissionPhase phase;

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
         * Sets the {@code phase} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder phase(AhpSessionAdmissionPhase value) {
            this.phase = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteCheckSendAdmissionParams build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (admissionId == null) throw new IllegalStateException("Missing required property: admissionId");
            if (phase == null) throw new IllegalStateException("Missing required property: phase");
            return new RemoteCheckSendAdmissionParams(sessionId, admissionId, phase);
        }
    }
}
