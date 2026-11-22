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
 * Source and reason for a plugin activation decision.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPluginActivationDecision(
    /** Installed plugin descriptor. */
    @JsonProperty("plugin") InstalledPlugin plugin,
    /** Whether the plugin is active. */
    @JsonProperty("active") Boolean active,
    /** Configuration layer that selected the plugin. */
    @JsonProperty("source") String source,
    /** Runtime explanation of the decision. */
    @JsonProperty("reason") String reason,
    /** Whether the inventory entry comes from persistent configuration. */
    @JsonProperty("persisted") Boolean persisted
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPluginActivationDecision {}

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
        private InstalledPlugin plugin;
        private Boolean active;
        private String source;
        private String reason;
        private Boolean persisted;

        /**
         * Sets the {@code plugin} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugin(InstalledPlugin value) {
            this.plugin = value;
            return this;
        }

        /**
         * Sets the {@code active} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder active(Boolean value) {
            this.active = value;
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
         * Sets the {@code reason} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reason(String value) {
            this.reason = value;
            return this;
        }

        /**
         * Sets the {@code persisted} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder persisted(Boolean value) {
            this.persisted = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPluginActivationDecision build() {
            if (plugin == null) throw new IllegalStateException("Missing required property: plugin");
            if (active == null) throw new IllegalStateException("Missing required property: active");
            if (reason == null) throw new IllegalStateException("Missing required property: reason");
            if (persisted == null) throw new IllegalStateException("Missing required property: persisted");
            return new UIPluginActivationDecision(plugin, active, source, reason, persisted);
        }
    }
}
