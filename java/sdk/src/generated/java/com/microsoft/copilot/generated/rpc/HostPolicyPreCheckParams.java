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
 * One non-prompting host-policy decision requested before the runtime invokes the Assisted Permissions judge.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record HostPolicyPreCheckParams(
    /** Session whose owning connection declared the host-policy pre-check. */
    @JsonProperty("sessionId") String sessionId,
    /** Runtime-generated correlation identifier for this permission request. */
    @JsonProperty("requestId") String requestId,
    /** Exact permission request being evaluated, with the same shape as `permission.requested.data.permissionRequest`. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("permissionRequest") Object permissionRequest,
    /** Exact user-facing gate the host may settle without prompting, with the same shape as `permission.requested.data.promptRequest`. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("promptRequest") Object promptRequest,
    /** Agent mode captured for the request, when available. */
    @JsonProperty("agentMode") SessionMode agentMode,
    /** Live host policy revision whose decision the runtime will accept. */
    @JsonProperty("policyRevision") Long policyRevision
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    HostPolicyPreCheckParams {}

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
        private String requestId;
        private Object permissionRequest;
        private boolean $presence2;
        private Object promptRequest;
        private boolean $presence3;
        private SessionMode agentMode;
        private Long policyRevision;

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
         * Sets the {@code requestId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder requestId(String value) {
            this.requestId = value;
            return this;
        }

        /**
         * Sets the {@code permissionRequest} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder permissionRequest(Object value) {
            this.permissionRequest = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code promptRequest} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder promptRequest(Object value) {
            this.promptRequest = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code agentMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder agentMode(SessionMode value) {
            this.agentMode = value;
            return this;
        }

        /**
         * Sets the {@code policyRevision} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder policyRevision(Long value) {
            this.policyRevision = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HostPolicyPreCheckParams build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (!$presence2) throw new IllegalStateException("Missing required property: permissionRequest");
            if (!$presence3) throw new IllegalStateException("Missing required property: promptRequest");
            if (policyRevision == null) throw new IllegalStateException("Missing required property: policyRevision");
            return new HostPolicyPreCheckParams(sessionId, requestId, permissionRequest, promptRequest, agentMode, policyRevision);
        }
    }
}
