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
 * Insert or replace a queued or steering message.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandSetPendingMessage extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "setPendingMessage";

    @Override
    public String getType() { return type; }

    /** AHP pending message kind. */
    @JsonProperty("kind")
    private RemoteHostPendingMessageKind kind;

    /** Message identifier. */
    @JsonProperty("id")
    private String id;

    /** Display prompt. */
    @JsonProperty("text")
    private String text;

    /** Optional distinct model prompt. */
    @JsonProperty("modelText")
    private String modelText;

    /** Prepared AHP attachments. */
    @JsonProperty("attachments")
    private List<Object> attachments;

    public RemoteHostPendingMessageKind getKind() { return kind; }
    public void setKind(RemoteHostPendingMessageKind kind) { this.kind = kind; }

    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    public String getText() { return text; }
    public void setText(String text) { this.text = text; }

    public String getModelText() { return modelText; }
    public void setModelText(String modelText) { this.modelText = modelText; }

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
        private RemoteHostPendingMessageKind kind;
        private String id;
        private String text;
        private String modelText;
        private List<Object> attachments;

        /**
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder kind(RemoteHostPendingMessageKind value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder id(String value) {
            this.id = value;
            return this;
        }

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
        public RemoteHostCommandSetPendingMessage build() {
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            if (id == null) throw new IllegalStateException("Missing required property: id");
            if (text == null) throw new IllegalStateException("Missing required property: text");
            RemoteHostCommandSetPendingMessage instance = new RemoteHostCommandSetPendingMessage();
            instance.setKind(kind);
            instance.setId(id);
            instance.setText(text);
            instance.setModelText(modelText);
            instance.setAttachments(attachments);
            return instance;
        }
    }
}
