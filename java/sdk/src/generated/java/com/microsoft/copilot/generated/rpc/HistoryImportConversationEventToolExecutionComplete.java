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
 * An imported tool result.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class HistoryImportConversationEventToolExecutionComplete extends HistoryImportConversationEvent {

    @JsonProperty("type")
    private final String type = "tool.execution_complete";

    @Override
    public String getType() { return type; }

    /** Original conversation record payload. */
    @JsonProperty("data")
    private HistoryImportToolCompletion data;

    public HistoryImportToolCompletion getData() { return data; }
    public void setData(HistoryImportToolCompletion data) { this.data = data; }


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
        private HistoryImportToolCompletion data;

        /**
         * Sets the {@code data} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder data(HistoryImportToolCompletion value) {
            this.data = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HistoryImportConversationEventToolExecutionComplete build() {
            if (data == null) throw new IllegalStateException("Missing required property: data");
            HistoryImportConversationEventToolExecutionComplete instance = new HistoryImportConversationEventToolExecutionComplete();
            instance.setData(data);
            return instance;
        }
    }
}
