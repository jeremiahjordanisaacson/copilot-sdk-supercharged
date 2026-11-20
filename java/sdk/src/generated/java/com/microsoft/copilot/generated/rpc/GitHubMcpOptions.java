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
 * Options for the built-in GitHub MCP server.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record GitHubMcpOptions(
    /** Expose all supported tools. */
    @JsonProperty("enableAllTools") Boolean enableAllTools,
    /** Additional toolsets to expose. */
    @JsonProperty("additionalToolsets") List<String> additionalToolsets,
    /** Additional tools to expose. */
    @JsonProperty("additionalTools") List<String> additionalTools,
    /** Exclude tools that the host's gh CLI can replace. */
    @JsonProperty("excludeGhReplaceableTools") Boolean excludeGhReplaceableTools,
    /** Enable insiders mode. */
    @JsonProperty("enableInsidersMode") Boolean enableInsidersMode,
    /** Enable FIDES tools. */
    @JsonProperty("enableFidesIfc") Boolean enableFidesIfc,
    /** Enable MCP app responses. */
    @JsonProperty("enableMcpApps") Boolean enableMcpApps,
    /** Disable deferred form loading. */
    @JsonProperty("disableFormDeferral") Boolean disableFormDeferral,
    /** Integration attribution for requests. */
    @JsonProperty("copilotIntegrationId") String copilotIntegrationId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitHubMcpOptions {}

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
        private Boolean enableAllTools;
        private List<String> additionalToolsets;
        private List<String> additionalTools;
        private Boolean excludeGhReplaceableTools;
        private Boolean enableInsidersMode;
        private Boolean enableFidesIfc;
        private Boolean enableMcpApps;
        private Boolean disableFormDeferral;
        private String copilotIntegrationId;

        /**
         * Sets the {@code enableAllTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableAllTools(Boolean value) {
            this.enableAllTools = value;
            return this;
        }

        /**
         * Sets the {@code additionalToolsets} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder additionalToolsets(List<String> value) {
            this.additionalToolsets = value;
            return this;
        }

        /**
         * Sets the {@code additionalTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder additionalTools(List<String> value) {
            this.additionalTools = value;
            return this;
        }

        /**
         * Sets the {@code excludeGhReplaceableTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludeGhReplaceableTools(Boolean value) {
            this.excludeGhReplaceableTools = value;
            return this;
        }

        /**
         * Sets the {@code enableInsidersMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableInsidersMode(Boolean value) {
            this.enableInsidersMode = value;
            return this;
        }

        /**
         * Sets the {@code enableFidesIfc} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableFidesIfc(Boolean value) {
            this.enableFidesIfc = value;
            return this;
        }

        /**
         * Sets the {@code enableMcpApps} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableMcpApps(Boolean value) {
            this.enableMcpApps = value;
            return this;
        }

        /**
         * Sets the {@code disableFormDeferral} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disableFormDeferral(Boolean value) {
            this.disableFormDeferral = value;
            return this;
        }

        /**
         * Sets the {@code copilotIntegrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder copilotIntegrationId(String value) {
            this.copilotIntegrationId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitHubMcpOptions build() {
            return new GitHubMcpOptions(enableAllTools, additionalToolsets, additionalTools, excludeGhReplaceableTools, enableInsidersMode, enableFidesIfc, enableMcpApps, disableFormDeferral, copilotIntegrationId);
        }
    }
}
