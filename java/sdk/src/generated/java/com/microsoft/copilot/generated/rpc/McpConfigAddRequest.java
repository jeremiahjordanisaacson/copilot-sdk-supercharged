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
 * MCP server name and configuration to add to user configuration.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class McpConfigAddRequest {

    /** Unique name for the MCP server */
    @JsonProperty("name")
    private final String name;

    /** MCP server configuration (stdio process or remote HTTP/SSE) */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("config")
    private final Object config;

    /** Configuration root for the user MCP document. Omission retains the connection's root; overrides require a trusted host or direct SDK caller. */
    @JsonProperty("configDir")
    private String configDir;

    /**
     * Creates a request with its required inputs.
     *
     * @param name Unique name for the MCP server
     * @param config MCP server configuration (stdio process or remote HTTP/SSE)
     */
    public McpConfigAddRequest(String name, Object config) {
        this.name = Objects.requireNonNull(name, "name");
        this.config = config;
    }

    /**
     * Returns the {@code name} property.
     *
     * @return Unique name for the MCP server
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the {@code config} property.
     *
     * @return MCP server configuration (stdio process or remote HTTP/SSE)
     */
    public Object getConfig() {
        return config;
    }

    /**
     * Returns the {@code configDir} property.
     *
     * @return Configuration root for the user MCP document. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
     */
    public String getConfigDir() {
        return configDir;
    }

    /**
     * Sets the {@code configDir} property.
     *
     * @param value Configuration root for the user MCP document. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
     * @return this request
     */
    public McpConfigAddRequest setConfigDir(String value) {
        this.configDir = value;
        return this;
    }
}
