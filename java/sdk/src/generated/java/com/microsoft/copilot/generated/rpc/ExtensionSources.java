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
 * Session-owned extension source overrides. These do not enable extensions or activate plugins.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record ExtensionSources(
    /** Discover personal extensions when true; exclude them when false. Omission preserves existing discovery. */
    @JsonProperty("user") Boolean user,
    /** Discover project extensions when true; exclude them when false. Omission preserves existing discovery and trust checks. */
    @JsonProperty("project") Boolean project,
    /** Permit extensions from otherwise-active plugins, including bundles. Defaults to true; other plugin contributions are unaffected. */
    @JsonProperty("plugins") Boolean plugins
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public ExtensionSources {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private Boolean user;
        private Boolean project;
        private Boolean plugins;

        /**
         * Sets the {@code user} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder user(Boolean value) {
            this.user = value;
            return this;
        }

        /**
         * Sets the {@code project} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder project(Boolean value) {
            this.project = value;
            return this;
        }

        /**
         * Sets the {@code plugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder plugins(Boolean value) {
            this.plugins = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ExtensionSources build() {
            return new ExtensionSources(user, project, plugins);
        }
    }
}
