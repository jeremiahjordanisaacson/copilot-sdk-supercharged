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
 * One plugin in a host invocation's complete merged inventory.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record PluginInvocationInventoryEntry(
    /** Plugin metadata and effective activation. */
    @JsonProperty("plugin") InstalledPluginInfo plugin,
    /** Whether an explicit host directory supplies this plugin. */
    @JsonProperty("external") Boolean external
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginInvocationInventoryEntry {}

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
        private InstalledPluginInfo plugin;
        private Boolean external;

        /**
         * Sets the {@code plugin} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugin(InstalledPluginInfo value) {
            this.plugin = value;
            return this;
        }

        /**
         * Sets the {@code external} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder external(Boolean value) {
            this.external = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginInvocationInventoryEntry build() {
            if (plugin == null) throw new IllegalStateException("Missing required property: plugin");
            if (external == null) throw new IllegalStateException("Missing required property: external");
            return new PluginInvocationInventoryEntry(plugin, external);
        }
    }
}
