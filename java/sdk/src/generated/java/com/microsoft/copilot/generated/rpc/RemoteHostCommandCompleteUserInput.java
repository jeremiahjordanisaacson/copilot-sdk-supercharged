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
 * Answer an interactive user question.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandCompleteUserInput extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "completeUserInput";

    @Override
    public String getType() { return type; }

    /** Pending request identifier. */
    @JsonProperty("requestId")
    private String requestId;

    /** Whether the answer is text. */
    @JsonProperty("isText")
    private Boolean isText;

    /** User answer. */
    @JsonProperty("answer")
    private String answer;

    /** Whether the answer came from a freeform entry. */
    @JsonProperty("wasFreeform")
    private Boolean wasFreeform;

    /** Whether the user dismissed the question. */
    @JsonProperty("dismissed")
    private Boolean dismissed;

    /** Optional question identifier. */
    @JsonProperty("questionId")
    private String questionId;

    public String getRequestId() { return requestId; }
    public void setRequestId(String requestId) { this.requestId = requestId; }

    public Boolean getIsText() { return isText; }
    public void setIsText(Boolean isText) { this.isText = isText; }

    public String getAnswer() { return answer; }
    public void setAnswer(String answer) { this.answer = answer; }

    public Boolean getWasFreeform() { return wasFreeform; }
    public void setWasFreeform(Boolean wasFreeform) { this.wasFreeform = wasFreeform; }

    public Boolean getDismissed() { return dismissed; }
    public void setDismissed(Boolean dismissed) { this.dismissed = dismissed; }

    public String getQuestionId() { return questionId; }
    public void setQuestionId(String questionId) { this.questionId = questionId; }


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
        private Boolean isText;
        private String answer;
        private Boolean wasFreeform;
        private Boolean dismissed;
        private String questionId;

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
         * Sets the {@code isText} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder isText(Boolean value) {
            this.isText = value;
            return this;
        }

        /**
         * Sets the {@code answer} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder answer(String value) {
            this.answer = value;
            return this;
        }

        /**
         * Sets the {@code wasFreeform} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder wasFreeform(Boolean value) {
            this.wasFreeform = value;
            return this;
        }

        /**
         * Sets the {@code dismissed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder dismissed(Boolean value) {
            this.dismissed = value;
            return this;
        }

        /**
         * Sets the {@code questionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder questionId(String value) {
            this.questionId = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandCompleteUserInput build() {
            if (requestId == null) throw new IllegalStateException("Missing required property: requestId");
            if (isText == null) throw new IllegalStateException("Missing required property: isText");
            if (answer == null) throw new IllegalStateException("Missing required property: answer");
            if (wasFreeform == null) throw new IllegalStateException("Missing required property: wasFreeform");
            if (dismissed == null) throw new IllegalStateException("Missing required property: dismissed");
            RemoteHostCommandCompleteUserInput instance = new RemoteHostCommandCompleteUserInput();
            instance.setRequestId(requestId);
            instance.setIsText(isText);
            instance.setAnswer(answer);
            instance.setWasFreeform(wasFreeform);
            instance.setDismissed(dismissed);
            instance.setQuestionId(questionId);
            return instance;
        }
    }
}
