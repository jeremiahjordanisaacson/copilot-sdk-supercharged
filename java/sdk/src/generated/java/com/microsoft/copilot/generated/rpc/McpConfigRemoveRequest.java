/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.Objects;
import javax.annotation.processing.Generated;

/**
 * MCP server name to remove from user configuration.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class McpConfigRemoveRequest {

    /** Name of the MCP server to remove */
    @JsonProperty("name")
    private final String name;

    /** OAuth Client ID Metadata Document URL whose persisted credentials should also be removed. */
    @JsonProperty("authClientIdMetadataUrl")
    private String authClientIdMetadataUrl;

    /** Configuration root for the user MCP document and credentials. Omission retains the connection's root; overrides require a trusted host or direct SDK caller. */
    @JsonProperty("configDir")
    private String configDir;

    /**
     * Creates a request with its required inputs.
     *
     * @param name Name of the MCP server to remove
     */
    public McpConfigRemoveRequest(String name) {
        this.name = Objects.requireNonNull(name, "name");
    }

    /**
     * Returns the {@code name} property.
     *
     * @return Name of the MCP server to remove
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the {@code authClientIdMetadataUrl} property.
     *
     * @return OAuth Client ID Metadata Document URL whose persisted credentials should also be removed.
     */
    public String getAuthClientIdMetadataUrl() {
        return authClientIdMetadataUrl;
    }

    /**
     * Returns the {@code configDir} property.
     *
     * @return Configuration root for the user MCP document and credentials. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
     */
    public String getConfigDir() {
        return configDir;
    }

    /**
     * Sets the {@code authClientIdMetadataUrl} property.
     *
     * @param value OAuth Client ID Metadata Document URL whose persisted credentials should also be removed.
     * @return this request
     */
    public McpConfigRemoveRequest setAuthClientIdMetadataUrl(String value) {
        this.authClientIdMetadataUrl = value;
        return this;
    }

    /**
     * Sets the {@code configDir} property.
     *
     * @param value Configuration root for the user MCP document and credentials. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
     * @return this request
     */
    public McpConfigRemoveRequest setConfigDir(String value) {
        this.configDir = value;
        return this;
    }
}
