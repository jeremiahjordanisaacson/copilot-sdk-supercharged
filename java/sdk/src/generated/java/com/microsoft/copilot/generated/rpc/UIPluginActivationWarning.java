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
 * A plugin resolution warning ready for display.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPluginActivationWarning(
    /** Warning classification. */
    @JsonProperty("kind") String kind,
    /** Configuration layer. */
    @JsonProperty("layer") String layer,
    /** Marketplace name. */
    @JsonProperty("marketplace") String marketplace,
    /** Marketplace directory. */
    @JsonProperty("marketplaceDir") String marketplaceDir,
    /** Plugin name for a plugin-specific warning. */
    @JsonProperty("plugin") String plugin,
    /** Expected plugin directory, when present. */
    @JsonProperty("expectedDir") String expectedDir,
    /** Human-readable warning. */
    @JsonProperty("message") String message
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPluginActivationWarning {}

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
        private String kind;
        private String layer;
        private String marketplace;
        private String marketplaceDir;
        private String plugin;
        private String expectedDir;
        private String message;

        /**
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder kind(String value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code layer} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder layer(String value) {
            this.layer = value;
            return this;
        }

        /**
         * Sets the {@code marketplace} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder marketplace(String value) {
            this.marketplace = value;
            return this;
        }

        /**
         * Sets the {@code marketplaceDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder marketplaceDir(String value) {
            this.marketplaceDir = value;
            return this;
        }

        /**
         * Sets the {@code plugin} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugin(String value) {
            this.plugin = value;
            return this;
        }

        /**
         * Sets the {@code expectedDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder expectedDir(String value) {
            this.expectedDir = value;
            return this;
        }

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPluginActivationWarning build() {
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            if (layer == null) throw new IllegalStateException("Missing required property: layer");
            if (marketplace == null) throw new IllegalStateException("Missing required property: marketplace");
            if (marketplaceDir == null) throw new IllegalStateException("Missing required property: marketplaceDir");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            return new UIPluginActivationWarning(kind, layer, marketplace, marketplaceDir, plugin, expectedDir, message);
        }
    }
}
