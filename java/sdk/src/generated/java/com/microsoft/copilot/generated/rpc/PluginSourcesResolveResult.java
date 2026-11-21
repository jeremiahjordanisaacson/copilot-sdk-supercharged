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
 * Active plugins and the separate persisted inventory, without persisting live directory records.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record PluginSourcesResolveResult(
    /** Active records, including trusted live marketplaces and explicit directories. */
    @JsonProperty("plugins") List<Object> plugins,
    /** Persisted ambient records, including disabled or shadowed installations. */
    @JsonProperty("persistedPlugins") List<Object> persistedPlugins,
    /** Live directory records, including inactive records that still own their names. */
    @JsonProperty("livePlugins") List<Object> livePlugins,
    /** Whether this pass confirmed the live tier instead of using persisted fallback. */
    @JsonProperty("liveTierKnown") Boolean liveTierKnown,
    /** Scanner and live-marketplace warnings for this exact source pass. */
    @JsonProperty("warnings") List<String> warnings
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginSourcesResolveResult {}

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
        private List<Object> plugins;
        private List<Object> persistedPlugins;
        private List<Object> livePlugins;
        private Boolean liveTierKnown;
        private List<String> warnings;

        /**
         * Sets the {@code plugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugins(List<Object> value) {
            this.plugins = value;
            return this;
        }

        /**
         * Sets the {@code persistedPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder persistedPlugins(List<Object> value) {
            this.persistedPlugins = value;
            return this;
        }

        /**
         * Sets the {@code livePlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder livePlugins(List<Object> value) {
            this.livePlugins = value;
            return this;
        }

        /**
         * Sets the {@code liveTierKnown} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder liveTierKnown(Boolean value) {
            this.liveTierKnown = value;
            return this;
        }

        /**
         * Sets the {@code warnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder warnings(List<String> value) {
            this.warnings = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginSourcesResolveResult build() {
            if (plugins == null) throw new IllegalStateException("Missing required property: plugins");
            if (persistedPlugins == null) throw new IllegalStateException("Missing required property: persistedPlugins");
            if (warnings == null) throw new IllegalStateException("Missing required property: warnings");
            return new PluginSourcesResolveResult(plugins, persistedPlugins, livePlugins, liveTierKnown, warnings);
        }
    }
}
