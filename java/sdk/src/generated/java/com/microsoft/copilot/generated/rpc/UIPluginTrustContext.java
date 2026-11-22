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
 * Trust inputs retained by the runtime's plugin policy.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPluginTrustContext(
    /** Persisted or authoritative trust mode. */
    @JsonProperty("mode") String mode,
    /** Authoritative trust decision, when supplied. */
    @JsonProperty("trusted") Boolean trusted,
    /** Host that supplied the authoritative decision. */
    @JsonProperty("source") String source
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPluginTrustContext {}

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
        private String mode;
        private Boolean trusted;
        private String source;

        /**
         * Sets the {@code mode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder mode(String value) {
            this.mode = value;
            return this;
        }

        /**
         * Sets the {@code trusted} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trusted(Boolean value) {
            this.trusted = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(String value) {
            this.source = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPluginTrustContext build() {
            if (mode == null) throw new IllegalStateException("Missing required property: mode");
            return new UIPluginTrustContext(mode, trusted, source);
        }
    }
}
