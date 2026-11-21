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
 * Resolve the authentication-derived built-in without a session or MCP connection.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpGithubResolveForInvocationParams(
    /** Repository context for host detection and shell availability. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Effective user and trusted repository settings. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("userSettings") Object userSettings,
    /** Invocation tool overrides, above settings. */
    @JsonProperty("flags") McpGitHubInvocationToolOptions flags,
    /** Tool filters used by the session this invocation describes. */
    @JsonProperty("toolFilters") McpShouldExcludeGitHubToolsRequest toolFilters,
    /** Effective disabled servers, after enable overrides. */
    @JsonProperty("disabledServers") List<String> disabledServers,
    /** Client integration identifier for the secret-free descriptor. */
    @JsonProperty("integrationId") String integrationId,
    /** Existing launch populations; account identity comes only from the connection. */
    @JsonProperty("audience") McpInvocationAudience audience
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpGithubResolveForInvocationParams {}

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
        private String workingDirectory;
        private Object userSettings;
        private boolean $presence1;
        private McpGitHubInvocationToolOptions flags;
        private McpShouldExcludeGitHubToolsRequest toolFilters;
        private List<String> disabledServers;
        private String integrationId;
        private McpInvocationAudience audience;

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code userSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder userSettings(Object value) {
            this.userSettings = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code flags} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder flags(McpGitHubInvocationToolOptions value) {
            this.flags = value;
            return this;
        }

        /**
         * Sets the {@code toolFilters} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder toolFilters(McpShouldExcludeGitHubToolsRequest value) {
            this.toolFilters = value;
            return this;
        }

        /**
         * Sets the {@code disabledServers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder disabledServers(List<String> value) {
            this.disabledServers = value;
            return this;
        }

        /**
         * Sets the {@code integrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder integrationId(String value) {
            this.integrationId = value;
            return this;
        }

        /**
         * Sets the {@code audience} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder audience(McpInvocationAudience value) {
            this.audience = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpGithubResolveForInvocationParams build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (!$presence1) throw new IllegalStateException("Missing required property: userSettings");
            if (disabledServers == null) throw new IllegalStateException("Missing required property: disabledServers");
            if (integrationId == null) throw new IllegalStateException("Missing required property: integrationId");
            if (audience == null) throw new IllegalStateException("Missing required property: audience");
            return new McpGithubResolveForInvocationParams(workingDirectory, userSettings, flags, toolFilters, disabledServers, integrationId, audience);
        }
    }
}
