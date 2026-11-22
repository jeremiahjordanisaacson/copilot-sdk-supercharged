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
 * Answer a pending tool approval.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandConfirmToolCall extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "confirmToolCall";

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

    /** Permission context forwarded to the session owner. */
    @JsonProperty("decisionContext")
    private Object decisionContext;

    /** Host-advertised option identifier. */
    @JsonProperty("selectedOptionId")
    private String selectedOptionId;

    /** User-visible reason. */
    @JsonProperty("reasonMessage")
    private String reasonMessage;

    /** Machine-readable decision reason. */
    @JsonProperty("reason")
    private String reason;

    public String getTurnId() { return turnId; }
    public void setTurnId(String turnId) { this.turnId = turnId; }

    public String getToolCallId() { return toolCallId; }
    public void setToolCallId(String toolCallId) { this.toolCallId = toolCallId; }

    public Boolean getApproved() { return approved; }
    public void setApproved(Boolean approved) { this.approved = approved; }

    public Object getDecisionContext() { return decisionContext; }
    public void setDecisionContext(Object decisionContext) { this.decisionContext = decisionContext; }

    public String getSelectedOptionId() { return selectedOptionId; }
    public void setSelectedOptionId(String selectedOptionId) { this.selectedOptionId = selectedOptionId; }

    public String getReasonMessage() { return reasonMessage; }
    public void setReasonMessage(String reasonMessage) { this.reasonMessage = reasonMessage; }

    public String getReason() { return reason; }
    public void setReason(String reason) { this.reason = reason; }


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
        private String selectedOptionId;
        private String reasonMessage;
        private String reason;

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
         * Sets the {@code selectedOptionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder selectedOptionId(String value) {
            this.selectedOptionId = value;
            return this;
        }

        /**
         * Sets the {@code reasonMessage} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reasonMessage(String value) {
            this.reasonMessage = value;
            return this;
        }

        /**
         * Sets the {@code reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reason(String value) {
            this.reason = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandConfirmToolCall build() {
            if (turnId == null) throw new IllegalStateException("Missing required property: turnId");
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            if (approved == null) throw new IllegalStateException("Missing required property: approved");
            RemoteHostCommandConfirmToolCall instance = new RemoteHostCommandConfirmToolCall();
            instance.setTurnId(turnId);
            instance.setToolCallId(toolCallId);
            instance.setApproved(approved);
            instance.setDecisionContext(decisionContext);
            instance.setSelectedOptionId(selectedOptionId);
            instance.setReasonMessage(reasonMessage);
            instance.setReason(reason);
            return instance;
        }
    }
}
