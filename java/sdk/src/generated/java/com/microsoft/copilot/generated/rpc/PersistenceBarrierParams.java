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
 * A typed request to the original host's retained persistence resources.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record PersistenceBarrierParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Resource barrier to await. */
    @JsonProperty("phase") SessionPersistenceBarrierPhase phase,
    /** Runtime-assigned identity for this flush, independent of concurrent flushes. */
    @JsonProperty("operationId") String operationId,
    /** Whether the original writer must persist an otherwise empty session. */
    @JsonProperty("force") Boolean force,
    /** Native flush failure, combined with retained callback failures at completion. */
    @JsonProperty("nativeFailure") String nativeFailure
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PersistenceBarrierParams {}

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
        private SessionPersistenceBarrierPhase phase;
        private String operationId;
        private Boolean force;
        private String nativeFailure;

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
         * Sets the {@code phase} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder phase(SessionPersistenceBarrierPhase value) {
            this.phase = value;
            return this;
        }

        /**
         * Sets the {@code operationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder operationId(String value) {
            this.operationId = value;
            return this;
        }

        /**
         * Sets the {@code force} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder force(Boolean value) {
            this.force = value;
            return this;
        }

        /**
         * Sets the {@code nativeFailure} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder nativeFailure(String value) {
            this.nativeFailure = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PersistenceBarrierParams build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (phase == null) throw new IllegalStateException("Missing required property: phase");
            if (operationId == null) throw new IllegalStateException("Missing required property: operationId");
            if (force == null) throw new IllegalStateException("Missing required property: force");
            return new PersistenceBarrierParams(sessionId, phase, operationId, force, nativeFailure);
        }
    }
}
