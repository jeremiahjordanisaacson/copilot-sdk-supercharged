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
 * Pending elicitation request ID and the user's response (accept/decline/cancel + form values).
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionUiHandleHumanAskUserParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** The unique request ID from the elicitation.requested event */
    @JsonProperty("requestId") String requestId,
    /** The elicitation response (accept with form values, decline, or cancel) */
    @JsonProperty("result") UIElicitationResponse result,
    /** Who answered and how. When it says a person answered interactively (`source` `human_response` with `responseCapability` `interactive`), the runtime records the answer as the person's own authority for Assisted Permissions, but only from the session's authenticated owning connection or the in-process host; an answer from any other caller settles without it. The runtime decides which answers carry authority. Omit for automated answers. */
    @JsonProperty("decisionContext") PermissionDecisionContext decisionContext
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionUiHandleHumanAskUserParams {}

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
        private UIElicitationResponse result;
        private PermissionDecisionContext decisionContext;

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
         * Sets the {@code result} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder result(UIElicitationResponse value) {
            this.result = value;
            return this;
        }

        /**
         * Sets the {@code decisionContext} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder decisionContext(PermissionDecisionContext value) {
            this.decisionContext = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUiHandleHumanAskUserParams build() {
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (result == null) throw new IllegalStateException("Missing required property: result");
            return new SessionUiHandleHumanAskUserParams(sessionId, requestId, result, decisionContext);
        }
    }
}
