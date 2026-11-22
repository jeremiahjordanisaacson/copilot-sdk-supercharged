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
 * An assistant message from an external conversation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record HistoryImportAssistantMessage(
    /** Identifier of this message. */
    @JsonProperty("messageId") String messageId,
    /** Original assistant message text. */
    @JsonProperty("content") String content,
    /** Model that produced the original message. */
    @JsonProperty("model") String model,
    /** Identifier shared with records from the original turn. */
    @JsonProperty("turnId") String turnId,
    /** Original tool requests, which the runtime does not execute. */
    @JsonProperty("toolRequests") List<HistoryImportToolRequest> toolRequests
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public HistoryImportAssistantMessage {}

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
        private String messageId;
        private String content;
        private String model;
        private String turnId;
        private List<HistoryImportToolRequest> toolRequests;

        /**
         * Sets the {@code messageId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder messageId(String value) {
            this.messageId = value;
            return this;
        }

        /**
         * Sets the {@code content} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder content(String value) {
            this.content = value;
            return this;
        }

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder model(String value) {
            this.model = value;
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
         * Sets the {@code toolRequests} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder toolRequests(List<HistoryImportToolRequest> value) {
            this.toolRequests = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HistoryImportAssistantMessage build() {
            if (messageId == null) throw new IllegalStateException("Missing required property: messageId");
            if (content == null) throw new IllegalStateException("Missing required property: content");
            return new HistoryImportAssistantMessage(messageId, content, model, turnId, toolRequests);
        }
    }
}
