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
 * Answer approval for a tool result.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandConfirmToolResult extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "confirmToolResult";

    @Override
    public String getType() { return type; }

    /** Turn identifier. */
    @JsonProperty("turnId")
    private String turnId;

    /** Tool call identifier. */
    @JsonProperty("toolCallId")
    private String toolCallId;

    /** Approval decision. */
    @JsonProperty("approved")
    private Boolean approved;

    /** Permission context. */
    @JsonProperty("decisionContext")
    private Object decisionContext;

    public String getTurnId() { return turnId; }
    public void setTurnId(String turnId) { this.turnId = turnId; }

    public String getToolCallId() { return toolCallId; }
    public void setToolCallId(String toolCallId) { this.toolCallId = toolCallId; }

    public Boolean getApproved() { return approved; }
    public void setApproved(Boolean approved) { this.approved = approved; }

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
        private String turnId;
        private String toolCallId;
        private Boolean approved;
        private Object decisionContext;

        /**
         * Sets the {@code turnId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder turnId(String value) {
            this.turnId = value;
            return this;
        }

        /**
         * Sets the {@code toolCallId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder toolCallId(String value) {
            this.toolCallId = value;
            return this;
        }

        /**
         * Sets the {@code approved} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder approved(Boolean value) {
            this.approved = value;
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
        public RemoteHostCommandConfirmToolResult build() {
            if (turnId == null) throw new IllegalStateException("Missing required property: turnId");
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            if (approved == null) throw new IllegalStateException("Missing required property: approved");
            RemoteHostCommandConfirmToolResult instance = new RemoteHostCommandConfirmToolResult();
            instance.setTurnId(turnId);
            instance.setToolCallId(toolCallId);
            instance.setApproved(approved);
            instance.setDecisionContext(decisionContext);
            return instance;
        }
    }
}
