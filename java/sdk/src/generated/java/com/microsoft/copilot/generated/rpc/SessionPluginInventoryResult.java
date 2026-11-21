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
 * Available plugin inventory under the selected session's settings, policy, source roots, and trusted bundle registration.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionPluginInventoryResult(
    /** Installed, live, and explicit plugins, including disabled entries. */
    @JsonProperty("plugins") List<SessionPluginInventoryEntry> plugins,
    /** Registered bundles before shadowing, for availability and trust checks. */
    @JsonProperty("builtinPlugins") List<SessionPluginInventoryEntry> builtinPlugins
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionPluginInventoryResult {}

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
        private List<SessionPluginInventoryEntry> plugins;
        private List<SessionPluginInventoryEntry> builtinPlugins;

        /**
         * Sets the {@code plugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugins(List<SessionPluginInventoryEntry> value) {
            this.plugins = value;
            return this;
        }

        /**
         * Sets the {@code builtinPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder builtinPlugins(List<SessionPluginInventoryEntry> value) {
            this.builtinPlugins = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPluginInventoryResult build() {
            if (plugins == null) throw new IllegalStateException("Missing required property: plugins");
            if (builtinPlugins == null) throw new IllegalStateException("Missing required property: builtinPlugins");
            return new SessionPluginInventoryResult(plugins, builtinPlugins);
        }
    }
}
