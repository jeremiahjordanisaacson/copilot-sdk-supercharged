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
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * One invocation's GitHub MCP tool overrides.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpGitHubInvocationToolOptions(
    /** Expose all supported tools. */
    @JsonProperty("enableAllTools") Boolean enableAllTools,
    /** Additional toolsets. */
    @JsonProperty("additionalToolsets") List<String> additionalToolsets,
    /** Additional tools. */
    @JsonProperty("additionalTools") List<String> additionalTools,
    /** Select the insiders endpoint. */
    @JsonProperty("enableInsidersMode") Boolean enableInsidersMode,
    /** Override form deferral when the shared tool resolver supports it. */
    @JsonProperty("disableFormDeferral") Boolean disableFormDeferral
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpGitHubInvocationToolOptions {}

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
        private Boolean enableAllTools;
        private List<String> additionalToolsets;
        private List<String> additionalTools;
        private Boolean enableInsidersMode;
        private Boolean disableFormDeferral;

        /**
         * Sets the {@code enableAllTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
        public Builder additionalTools(List<String> value) {
            this.additionalTools = value;
            return this;
        }

        /**
         * Sets the {@code enableInsidersMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder enableInsidersMode(Boolean value) {
            this.enableInsidersMode = value;
            return this;
        }

        /**
         * Sets the {@code disableFormDeferral} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder disableFormDeferral(Boolean value) {
            this.disableFormDeferral = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpGitHubInvocationToolOptions build() {
            return new McpGitHubInvocationToolOptions(enableAllTools, additionalToolsets, additionalTools, enableInsidersMode, disableFormDeferral);
        }
    }
}
