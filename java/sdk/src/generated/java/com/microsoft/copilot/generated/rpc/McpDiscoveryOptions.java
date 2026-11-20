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
 * Read-only invocation options for effective MCP discovery. User and repository settings remain unchanged.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record McpDiscoveryOptions(
    /** Additional MCP configuration, as inline JSON or @file references, merged in order after the persisted and plugin sources. Relative file references resolve against workingDirectory. */
    @JsonProperty("additionalConfig") List<String> additionalConfig,
    /** Server names explicitly disabled for this invocation. These disables take precedence over enabledServers. */
    @JsonProperty("disabledServers") List<String> disabledServers,
    /** Server names explicitly enabled for this invocation, overriding persisted disabled names but not explicit invocation disables or runtime policy. */
    @JsonProperty("enabledServers") List<String> enabledServers,
    /** Disable the runtime's built-in MCP server names for this invocation. */
    @JsonProperty("disableBuiltinServers") Boolean disableBuiltinServers,
    /** Experimental feature opt-in for this invocation. Defaults to the effective user and trusted repository setting. */
    @JsonProperty("experimental") Boolean experimental,
    /** Enable the full GitHub MCP tool surface for this invocation. */
    @JsonProperty("enableAllGithubTools") Boolean enableAllGitHubTools,
    /** Additional GitHub MCP toolsets selected for this invocation. */
    @JsonProperty("githubToolsets") List<String> gitHubToolsets,
    /** Additional GitHub MCP tools selected for this invocation. */
    @JsonProperty("githubTools") List<String> gitHubTools,
    /** Invocation tool allowlist. Together with excludedTools, it controls whether shell-replaceable GitHub MCP tools remain necessary. */
    @JsonProperty("availableTools") List<String> availableTools,
    /** Invocation tool exclusions. These exclusions take precedence over availableTools. */
    @JsonProperty("excludedTools") List<String> excludedTools
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public McpDiscoveryOptions {}

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
        private List<String> additionalConfig;
        private List<String> disabledServers;
        private List<String> enabledServers;
        private Boolean disableBuiltinServers;
        private Boolean experimental;
        private Boolean enableAllGitHubTools;
        private List<String> gitHubToolsets;
        private List<String> gitHubTools;
        private List<String> availableTools;
        private List<String> excludedTools;

        /**
         * Sets the {@code additionalConfig} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder additionalConfig(List<String> value) {
            this.additionalConfig = value;
            return this;
        }

        /**
         * Sets the {@code disabledServers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disabledServers(List<String> value) {
            this.disabledServers = value;
            return this;
        }

        /**
         * Sets the {@code enabledServers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabledServers(List<String> value) {
            this.enabledServers = value;
            return this;
        }

        /**
         * Sets the {@code disableBuiltinServers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disableBuiltinServers(Boolean value) {
            this.disableBuiltinServers = value;
            return this;
        }

        /**
         * Sets the {@code experimental} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder experimental(Boolean value) {
            this.experimental = value;
            return this;
        }

        /**
         * Sets the {@code enableAllGithubTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableAllGitHubTools(Boolean value) {
            this.enableAllGitHubTools = value;
            return this;
        }

        /**
         * Sets the {@code githubToolsets} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitHubToolsets(List<String> value) {
            this.gitHubToolsets = value;
            return this;
        }

        /**
         * Sets the {@code githubTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitHubTools(List<String> value) {
            this.gitHubTools = value;
            return this;
        }

        /**
         * Sets the {@code availableTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder availableTools(List<String> value) {
            this.availableTools = value;
            return this;
        }

        /**
         * Sets the {@code excludedTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludedTools(List<String> value) {
            this.excludedTools = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpDiscoveryOptions build() {
            return new McpDiscoveryOptions(additionalConfig, disabledServers, enabledServers, disableBuiltinServers, experimental, enableAllGitHubTools, gitHubToolsets, gitHubTools, availableTools, excludedTools);
        }
    }

    @Override
    public String toString() {
        return "McpDiscoveryOptions[additionalConfig=" + (additionalConfig == null ? null : "<redacted>") + ", disabledServers=" + disabledServers + ", enabledServers=" + enabledServers + ", disableBuiltinServers=" + disableBuiltinServers + ", experimental=" + experimental + ", enableAllGitHubTools=" + enableAllGitHubTools + ", gitHubToolsets=" + gitHubToolsets + ", gitHubTools=" + gitHubTools + ", availableTools=" + availableTools + ", excludedTools=" + excludedTools + "]";
    }
}
