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
 * A legacy MCP configuration credential to persist in the host's secret store.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record McpSecretsSaveParams(
    /** Secret reference used by an MCP server configuration. */
    @JsonProperty("secretId") String secretId,
    /** Credential value to persist. */
    @JsonProperty("value") String value,
    /** Expected mcp-secrets directory. Omit to use the runtime configuration root. */
    @JsonProperty("configDir") String configDir
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public McpSecretsSaveParams {}

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
        private String secretId;
        private String value;
        private String configDir;

        /**
         * Sets the {@code secretId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder secretId(String value) {
            this.secretId = value;
            return this;
        }

        /**
         * Sets the {@code value} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder value(String value) {
            this.value = value;
            return this;
        }

        /**
         * Sets the {@code configDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configDir(String value) {
            this.configDir = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpSecretsSaveParams build() {
            if (secretId == null) throw new IllegalStateException("Missing required property: secretId");
            if (value == null) throw new IllegalStateException("Missing required property: value");
            return new McpSecretsSaveParams(secretId, value, configDir);
        }
    }

    @Override
    public String toString() {
        return "McpSecretsSaveParams[secretId=" + secretId + ", value=" + (value == null ? null : "<redacted>") + ", configDir=" + configDir + "]";
    }
}
