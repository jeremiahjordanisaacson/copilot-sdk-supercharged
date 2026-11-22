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
 * An imported tool completion with a text result or an error.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record HistoryImportToolCompletion(
    /** Identifier shared with the imported tool invocation. */
    @JsonProperty("toolCallId") String toolCallId,
    /** Whether the original tool invocation succeeded. */
    @JsonProperty("success") Boolean success,
    /** Identifier shared with other records from the original turn. */
    @JsonProperty("turnId") String turnId,
    /** Original text output for a successful invocation. */
    @JsonProperty("result") HistoryImportToolOutput result,
    /** Original error for a failed invocation. */
    @JsonProperty("error") HistoryImportToolError error
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public HistoryImportToolCompletion {}

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
        private String toolCallId;
        private Boolean success;
        private String turnId;
        private HistoryImportToolOutput result;
        private HistoryImportToolError error;

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
         * Sets the {@code success} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder success(Boolean value) {
            this.success = value;
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
         * Sets the {@code result} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder result(HistoryImportToolOutput value) {
            this.result = value;
            return this;
        }

        /**
         * Sets the {@code error} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder error(HistoryImportToolError value) {
            this.error = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HistoryImportToolCompletion build() {
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            if (success == null) throw new IllegalStateException("Missing required property: success");
            return new HistoryImportToolCompletion(toolCallId, success, turnId, result, error);
        }
    }
}
