/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Start a remote turn exactly once.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandStartTurn extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "startTurn";

    @Override
    public String getType() { return type; }

    /** Display prompt. */
    @JsonProperty("text")
    private String text;

    /** Optional distinct model prompt. */
    @JsonProperty("modelText")
    private String modelText;

    /** Optional caller-selected turn identifier. */
    @JsonProperty("turnId")
    private String turnId;

    /** Prepared AHP attachments. */
    @JsonProperty("attachments")
    private List<Object> attachments;

    public String getText() { return text; }
    public void setText(String text) { this.text = text; }

    public String getModelText() { return modelText; }
    public void setModelText(String modelText) { this.modelText = modelText; }

    public String getTurnId() { return turnId; }
    public void setTurnId(String turnId) { this.turnId = turnId; }

    public List<Object> getAttachments() { return attachments; }
    public void setAttachments(List<Object> attachments) { this.attachments = attachments; }


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
        private String text;
        private String modelText;
        private String turnId;
        private List<Object> attachments;

        /**
         * Sets the {@code text} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder text(String value) {
            this.text = value;
            return this;
        }

        /**
         * Sets the {@code modelText} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder modelText(String value) {
            this.modelText = value;
            return this;
        }

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
         * Sets the {@code attachments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder attachments(List<Object> value) {
            this.attachments = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandStartTurn build() {
            if (text == null) throw new IllegalStateException("Missing required property: text");
            RemoteHostCommandStartTurn instance = new RemoteHostCommandStartTurn();
            instance.setText(text);
            instance.setModelText(modelText);
            instance.setTurnId(turnId);
            instance.setAttachments(attachments);
            return instance;
        }
    }
}
