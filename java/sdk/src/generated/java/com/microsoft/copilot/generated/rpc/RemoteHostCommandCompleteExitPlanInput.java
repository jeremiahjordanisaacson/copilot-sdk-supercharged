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
 * Answer a pending plan approval.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandCompleteExitPlanInput extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "completeExitPlanInput";

    @Override
    public String getType() { return type; }

    /** Pending request identifier. */
    @JsonProperty("requestId")
    private String requestId;

    /** Approval decision. */
    @JsonProperty("approved")
    private Boolean approved;

    /** Selected host action. */
    @JsonProperty("selectedAction")
    private String selectedAction;

    /** Optional user feedback. */
    @JsonProperty("feedback")
    private String feedback;

    public String getRequestId() { return requestId; }
    public void setRequestId(String requestId) { this.requestId = requestId; }

    public Boolean getApproved() { return approved; }
    public void setApproved(Boolean approved) { this.approved = approved; }

    public String getSelectedAction() { return selectedAction; }
    public void setSelectedAction(String selectedAction) { this.selectedAction = selectedAction; }

    public String getFeedback() { return feedback; }
    public void setFeedback(String feedback) { this.feedback = feedback; }


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
        private Boolean approved;
        private String selectedAction;
        private String feedback;

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
         * Sets the {@code selectedAction} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder selectedAction(String value) {
            this.selectedAction = value;
            return this;
        }

        /**
         * Sets the {@code feedback} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder feedback(String value) {
            this.feedback = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandCompleteExitPlanInput build() {
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (approved == null) throw new IllegalStateException("Missing required property: approved");
            RemoteHostCommandCompleteExitPlanInput instance = new RemoteHostCommandCompleteExitPlanInput();
            instance.setRequestId(requestId);
            instance.setApproved(approved);
            instance.setSelectedAction(selectedAction);
            instance.setFeedback(feedback);
            return instance;
        }
    }
}
