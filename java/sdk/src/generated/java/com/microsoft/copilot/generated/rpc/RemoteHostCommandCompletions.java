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
 * Request remote composer completions.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandCompletions extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "completions";

    @Override
    public String getType() { return type; }

    /** Composer text. */
    @JsonProperty("text")
    private String text;

    /** Cursor offset in UTF-16 code units. */
    @JsonProperty("offset")
    private Long offset;

    public String getText() { return text; }
    public void setText(String text) { this.text = text; }

    public Long getOffset() { return offset; }
    public void setOffset(Long offset) { this.offset = offset; }


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
        private Long offset;

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
         * Sets the {@code offset} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder offset(Long value) {
            this.offset = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandCompletions build() {
            if (text == null) throw new IllegalStateException("Missing required property: text");
            if (offset == null) throw new IllegalStateException("Missing required property: offset");
            RemoteHostCommandCompletions instance = new RemoteHostCommandCompletions();
            instance.setText(text);
            instance.setOffset(offset);
            return instance;
        }
    }
}
