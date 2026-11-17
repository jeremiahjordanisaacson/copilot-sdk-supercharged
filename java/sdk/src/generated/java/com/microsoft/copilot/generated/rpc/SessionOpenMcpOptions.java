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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * MCP servers, OAuth, and renderer capability.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenMcpOptions(
    /** OAuth Client ID Metadata Document URL used by this host for MCP authorization. */
    @JsonProperty("authClientIdMetadataUrl") String authClientIdMetadataUrl,
    /** How MCP server environment values are interpreted. */
    @JsonProperty("envValueMode") SessionOpenOptionsEnvValueMode envValueMode,
    /** MCP server names disabled for this session. Disabled servers are not started or authenticated on create or cold resume. */
    @JsonProperty("disabledServers") List<String> disabledServers,
    /** Non-secret host-managed HTTP MCP servers keyed by stable managed identity. Managed provenance is runtime-established from this separate field and credentials are supplied through dynamic-header refresh. */
    @CopilotExperimental
    @JsonProperty("managedServers") Map<String, ManagedMcpServerConfig> managedServers,
    /** Whether to include instructions from every MCP server in the system prompt instead of only allowlisted servers. */
    @JsonProperty("allowAllServerInstructions") Boolean allowAllServerInstructions
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenMcpOptions {}

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
        private String authClientIdMetadataUrl;
        private SessionOpenOptionsEnvValueMode envValueMode;
        private List<String> disabledServers;
        private Map<String, ManagedMcpServerConfig> managedServers;
        private Boolean allowAllServerInstructions;

        /**
         * Sets the {@code authClientIdMetadataUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authClientIdMetadataUrl(String value) {
            this.authClientIdMetadataUrl = value;
            return this;
        }

        /**
         * Sets the {@code envValueMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder envValueMode(SessionOpenOptionsEnvValueMode value) {
            this.envValueMode = value;
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
         * Sets the {@code managedServers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder managedServers(Map<String, ManagedMcpServerConfig> value) {
            this.managedServers = value;
            return this;
        }

        /**
         * Sets the {@code allowAllServerInstructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowAllServerInstructions(Boolean value) {
            this.allowAllServerInstructions = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenMcpOptions build() {
            return new SessionOpenMcpOptions(authClientIdMetadataUrl, envValueMode, disabledServers, managedServers, allowAllServerInstructions);
        }
    }
}
