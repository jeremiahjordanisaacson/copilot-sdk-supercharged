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
 * Repository and server state for GitHub MCP exposure.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpGitHubWorkspace(
    /** The repository has an Azure DevOps remote. */
    @JsonProperty("hasAzureDevOps") Boolean hasAzureDevOps,
    /** The repository has a GitHub remote. */
    @JsonProperty("hasGithub") Boolean hasGitHub,
    /** A user configured the GitHub MCP server. */
    @JsonProperty("userConfiguredGithubMcp") Boolean userConfiguredGitHubMcp,
    /** Server names already disabled by the host. */
    @JsonProperty("alreadyDisabled") List<String> alreadyDisabled
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpGitHubWorkspace {}

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
        private Boolean hasAzureDevOps;
        private Boolean hasGitHub;
        private Boolean userConfiguredGitHubMcp;
        private List<String> alreadyDisabled;

        /**
         * Sets the {@code hasAzureDevOps} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasAzureDevOps(Boolean value) {
            this.hasAzureDevOps = value;
            return this;
        }

        /**
         * Sets the {@code hasGithub} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hasGitHub(Boolean value) {
            this.hasGitHub = value;
            return this;
        }

        /**
         * Sets the {@code userConfiguredGithubMcp} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder userConfiguredGitHubMcp(Boolean value) {
            this.userConfiguredGitHubMcp = value;
            return this;
        }

        /**
         * Sets the {@code alreadyDisabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder alreadyDisabled(List<String> value) {
            this.alreadyDisabled = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpGitHubWorkspace build() {
            if (hasAzureDevOps == null) throw new IllegalStateException("Missing required property: hasAzureDevOps");
            if (hasGitHub == null) throw new IllegalStateException("Missing required property: hasGithub");
            if (userConfiguredGitHubMcp == null) throw new IllegalStateException("Missing required property: userConfiguredGithubMcp");
            if (alreadyDisabled == null) throw new IllegalStateException("Missing required property: alreadyDisabled");
            return new McpGitHubWorkspace(hasAzureDevOps, hasGitHub, userConfiguredGitHubMcp, alreadyDisabled);
        }
    }
}
