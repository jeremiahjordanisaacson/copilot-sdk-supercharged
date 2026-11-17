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
 * Server name and optional transport selector for an individual MCP server stop. Omit transport to stop the server by name.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionMcpStopServerParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Exact owned receipt identity. Stop also forgets this session's durable activation. */
    @JsonProperty("expectedInstallationId") String expectedInstallationId,
    /** Name of the MCP server to stop */
    @JsonProperty("serverName") String serverName,
    /** Optional transport selector. The only supported value, local-http, requires serverName ide and cannot accompany expectedInstallationId. It cancels pending IDE initialization and removes local IDE state without stopping an ordinary server with the same name. Omit this field for a generic stop by name. */
    @JsonProperty("transport") McpServerConfigLocalType transport
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionMcpStopServerParams {}

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
        private String sessionId;
        private String expectedInstallationId;
        private String serverName;
        private McpServerConfigLocalType transport;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code expectedInstallationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder expectedInstallationId(String value) {
            this.expectedInstallationId = value;
            return this;
        }

        /**
         * Sets the {@code serverName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder serverName(String value) {
            this.serverName = value;
            return this;
        }

        /**
         * Sets the {@code transport} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder transport(McpServerConfigLocalType value) {
            this.transport = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionMcpStopServerParams build() {
            if (serverName == null) throw new IllegalStateException("Missing required property: serverName");
            return new SessionMcpStopServerParams(sessionId, expectedInstallationId, serverName, transport);
        }
    }
}
