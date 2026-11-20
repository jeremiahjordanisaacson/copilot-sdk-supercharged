/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Content to redact with the runtime's process filter.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SecretsFilterParams(
    /** Content to redact. The runtime never returns its registered secret values. */
    @JsonProperty("text") String text,
    /** Content representation. Defaults to plain text. */
    @JsonProperty("format") SecretFilterFormat format
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SecretsFilterParams {}

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
        private SecretFilterFormat format;

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
         * Sets the {@code format} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder format(SecretFilterFormat value) {
            this.format = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SecretsFilterParams build() {
            if (text == null) throw new IllegalStateException("Missing required property: text");
            return new SecretsFilterParams(text, format);
        }
    }
}
