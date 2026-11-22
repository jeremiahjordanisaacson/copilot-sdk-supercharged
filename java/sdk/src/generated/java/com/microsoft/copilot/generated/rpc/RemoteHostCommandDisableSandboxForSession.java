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
 * Disable the sandbox through its pending owner request.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandDisableSandboxForSession extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "disableSandboxForSession";

    @Override
    public String getType() { return type; }

    /** Pending request identifier. */
    @JsonProperty("requestId")
    private String requestId;

    /** Permission decision context. */
    @JsonProperty("decisionContext")
    private Object decisionContext;

    public String getRequestId() { return requestId; }
    public void setRequestId(String requestId) { this.requestId = requestId; }

    public Object getDecisionContext() { return decisionContext; }
    public void setDecisionContext(Object decisionContext) { this.decisionContext = decisionContext; }


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
        private String requestId;
        private Object decisionContext;

        /**
         * Sets the {@code requestId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder requestId(String value) {
            this.requestId = value;
            return this;
        }

        /**
         * Sets the {@code decisionContext} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder decisionContext(Object value) {
            this.decisionContext = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandDisableSandboxForSession build() {
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            RemoteHostCommandDisableSandboxForSession instance = new RemoteHostCommandDisableSandboxForSession();
            instance.setRequestId(requestId);
            instance.setDecisionContext(decisionContext);
            return instance;
        }
    }
}
