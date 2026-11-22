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
 * Complete a tool call on its owning chat.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandCompleteToolCall extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "completeToolCall";

    @Override
    public String getType() { return type; }

    /** Turn identifier. */
    @JsonProperty("turnId")
    private String turnId;

    /** Tool call identifier. */
    @JsonProperty("toolCallId")
    private String toolCallId;

    /** AHP tool result. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("result")
    private Object result;

    public String getTurnId() { return turnId; }
    public void setTurnId(String turnId) { this.turnId = turnId; }

    public String getToolCallId() { return toolCallId; }
    public void setToolCallId(String toolCallId) { this.toolCallId = toolCallId; }

    public Object getResult() { return result; }
    public void setResult(Object result) { this.result = result; }


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
        private Object result;
        private boolean $presence2;

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
         * Sets the {@code result} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder result(Object value) {
            this.result = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandCompleteToolCall build() {
            if (turnId == null) throw new IllegalStateException("Missing required property: turnId");
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            if (!$presence2) throw new IllegalStateException("Missing required property: result");
            RemoteHostCommandCompleteToolCall instance = new RemoteHostCommandCompleteToolCall();
            instance.setTurnId(turnId);
            instance.setToolCallId(toolCallId);
            instance.setResult(result);
            return instance;
        }
    }
}
