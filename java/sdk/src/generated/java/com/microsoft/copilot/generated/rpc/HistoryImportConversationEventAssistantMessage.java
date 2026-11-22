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
 * An imported assistant message and its tool requests.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class HistoryImportConversationEventAssistantMessage extends HistoryImportConversationEvent {

    @JsonProperty("type")
    private final String type = "assistant.message";

    @Override
    public String getType() { return type; }

    /** Original conversation record payload. */
    @JsonProperty("data")
    private HistoryImportAssistantMessage data;

    public HistoryImportAssistantMessage getData() { return data; }
    public void setData(HistoryImportAssistantMessage data) { this.data = data; }


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
        private HistoryImportAssistantMessage data;

        /**
         * Sets the {@code data} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder data(HistoryImportAssistantMessage value) {
            this.data = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HistoryImportConversationEventAssistantMessage build() {
            if (data == null) throw new IllegalStateException("Missing required property: data");
            HistoryImportConversationEventAssistantMessage instance = new HistoryImportConversationEventAssistantMessage();
            instance.setData(data);
            return instance;
        }
    }
}
