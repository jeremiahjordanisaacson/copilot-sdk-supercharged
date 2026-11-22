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
 * A question shown to the user.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIUserInputPrompt(
    /** Question text. */
    @JsonProperty("question") String question,
    /** Suggested answers, when present. */
    @JsonProperty("choices") List<String> choices,
    /** Whether a custom answer is allowed. */
    @JsonProperty("allowFreeform") Boolean allowFreeform,
    /** Tool call that requested the answer. */
    @JsonProperty("toolCallId") String toolCallId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIUserInputPrompt {}

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
        private String question;
        private List<String> choices;
        private Boolean allowFreeform;
        private String toolCallId;

        /**
         * Sets the {@code question} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder question(String value) {
            this.question = value;
            return this;
        }

        /**
         * Sets the {@code choices} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder choices(List<String> value) {
            this.choices = value;
            return this;
        }

        /**
         * Sets the {@code allowFreeform} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowFreeform(Boolean value) {
            this.allowFreeform = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIUserInputPrompt build() {
            if (question == null) throw new IllegalStateException("Missing required property: question");
            return new UIUserInputPrompt(question, choices, allowFreeform, toolCallId);
        }
    }
}
