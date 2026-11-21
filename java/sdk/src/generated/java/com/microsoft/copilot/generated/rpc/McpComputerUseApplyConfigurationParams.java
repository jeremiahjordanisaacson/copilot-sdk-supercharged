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
 * Apply the host's Computer Use platform overlay without starting a server.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpComputerUseApplyConfigurationParams(
    /** Complete server map, including provenance and fields unknown to older clients. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("servers") Object servers,
    /** Disabled servers after invocation precedence. */
    @JsonProperty("disabledServers") List<String> disabledServers,
    /** Explicitly enabled servers. */
    @JsonProperty("enabledServers") List<String> enabledServers
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpComputerUseApplyConfigurationParams {}

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
        private Object servers;
        private boolean $presence0;
        private List<String> disabledServers;
        private List<String> enabledServers;

        /**
         * Sets the {@code servers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder servers(Object value) {
            this.servers = value;
            this.$presence0 = true;
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
         * Sets the {@code enabledServers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder enabledServers(List<String> value) {
            this.enabledServers = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpComputerUseApplyConfigurationParams build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: servers");
            if (disabledServers == null) throw new IllegalStateException("Missing required property: disabledServers");
            if (enabledServers == null) throw new IllegalStateException("Missing required property: enabledServers");
            return new McpComputerUseApplyConfigurationParams(servers, disabledServers, enabledServers);
        }
    }
}
