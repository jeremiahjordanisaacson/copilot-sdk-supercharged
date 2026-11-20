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
 * Runtime activation and source for a directory-backed marketplace plugin.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record PluginMarketplaceLiveState(
    /** Whether this plugin is active in the session's current policy context. */
    @JsonProperty("enabled") Boolean enabled,
    /** The configured directory marketplace that supplies this plugin. */
    @JsonProperty("installedFrom") String installedFrom
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PluginMarketplaceLiveState {}

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
        private Boolean enabled;
        private String installedFrom;

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Sets the {@code installedFrom} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder installedFrom(String value) {
            this.installedFrom = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginMarketplaceLiveState build() {
            if (enabled == null) throw new IllegalStateException("Missing required property: enabled");
            if (installedFrom == null) throw new IllegalStateException("Missing required property: installedFrom");
            return new PluginMarketplaceLiveState(enabled, installedFrom);
        }
    }
}
