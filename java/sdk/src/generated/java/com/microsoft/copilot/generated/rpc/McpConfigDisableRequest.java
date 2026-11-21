/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import java.util.Objects;
import javax.annotation.processing.Generated;

/**
 * MCP server names to disable for new sessions.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class McpConfigDisableRequest {
    @com.fasterxml.jackson.annotation.JsonIgnore
    private final java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();

    /** Names of MCP servers to disable. Each server is added to the persisted disabled list so new sessions skip it. Already-disabled names are ignored. Active sessions keep their current connections until they end. */
    @JsonProperty("names")
    private final List<String> names;

    /** Optional configuration root for user settings. Omission retains the connection's root; overrides require a trusted host or direct SDK caller. */
    @JsonProperty("configDir")
    private String configDir;

    /**
     * Creates a request with its required inputs.
     *
     * @param names Names of MCP servers to disable. Each server is added to the persisted disabled list so new sessions skip it. Already-disabled names are ignored. Active sessions keep their current connections until they end.
     */
    public McpConfigDisableRequest(List<String> names) {
        this.names = Objects.requireNonNull(names, "names");
    }

    /**
     * Returns the {@code names} property.
     *
     * @return Names of MCP servers to disable. Each server is added to the persisted disabled list so new sessions skip it. Already-disabled names are ignored. Active sessions keep their current connections until they end.
     */
    public List<String> getNames() {
        return names;
    }

    /**
     * Returns the {@code configDir} property.
     *
     * @return Optional configuration root for user settings. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
     */
    public String getConfigDir() {
        return configDir;
    }

    /**
     * Sets the {@code configDir} property.
     *
     * @param value Optional configuration root for user settings. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
     * @return this request
     */
    public McpConfigDisableRequest setConfigDir(String value) {
        this.configDir = value;
        if (value == null) $explicitNulls.add("configDir");
        else $explicitNulls.remove("configDir");
        return this;
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("configDir") && configDir == null) values.put("configDir", null);
        return values;
    }
}
