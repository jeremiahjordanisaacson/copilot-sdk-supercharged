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
 * Optional working directory used as context for MCP server discovery.
 * <p>
 * Required inputs are constructor arguments. Optional inputs have fluent setters.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class McpDiscoverRequest {

    /** Working directory used as context for discovery (e.g., plugin resolution) */
    @JsonProperty("workingDirectory")
    private String workingDirectory;

    /** Whether to include canonical effectiveSource metadata for each discovered server. Callers must opt in so protocol-3 clients retain the legacy closed response shape. */
    @JsonProperty("includeEffectiveSource")
    private Boolean includeEffectiveSource;

    /** External plugin directories mounted for this discovery only. Relative paths resolve against workingDirectory. The runtime resolves trust and policy; these directories do not become trusted built-ins or change settings. */
    @JsonProperty("pluginDirectories")
    private List<String> pluginDirectories;

    /** Read-only invocation options layered over runtime-owned configuration. These options never assert account, workspace trust, or managed policy. */
    @JsonProperty("options")
    private McpDiscoveryOptions options;

    /** Include each server's effective serializable configuration and structured diagnostics. Omitted or false preserves the existing response shape exactly. */
    @JsonProperty("includeConfiguration")
    private Boolean includeConfiguration;

    /**
     * Creates a request with its required inputs.
     */
    public McpDiscoverRequest() {
    }

    /**
     * Returns the {@code workingDirectory} property.
     *
     * @return Working directory used as context for discovery (e.g., plugin resolution)
     */
    public String getWorkingDirectory() {
        return workingDirectory;
    }

    /**
     * Returns the {@code includeEffectiveSource} property.
     *
     * @return Whether to include canonical effectiveSource metadata for each discovered server. Callers must opt in so protocol-3 clients retain the legacy closed response shape.
     */
    public Boolean getIncludeEffectiveSource() {
        return includeEffectiveSource;
    }

    /**
     * Returns the {@code pluginDirectories} property.
     *
     * @return External plugin directories mounted for this discovery only. Relative paths resolve against workingDirectory. The runtime resolves trust and policy; these directories do not become trusted built-ins or change settings.
     */
    public List<String> getPluginDirectories() {
        return pluginDirectories;
    }

    /**
     * Returns the {@code options} property.
     *
     * @return Read-only invocation options layered over runtime-owned configuration. These options never assert account, workspace trust, or managed policy.
     */
    public McpDiscoveryOptions getOptions() {
        return options;
    }

    /**
     * Returns the {@code includeConfiguration} property.
     *
     * @return Include each server's effective serializable configuration and structured diagnostics. Omitted or false preserves the existing response shape exactly.
     */
    public Boolean getIncludeConfiguration() {
        return includeConfiguration;
    }

    /**
     * Sets the {@code workingDirectory} property.
     *
     * @param value Working directory used as context for discovery (e.g., plugin resolution)
     * @return this request
     */
    public McpDiscoverRequest setWorkingDirectory(String value) {
        this.workingDirectory = value;
        return this;
    }

    /**
     * Sets the {@code includeEffectiveSource} property.
     *
     * @param value Whether to include canonical effectiveSource metadata for each discovered server. Callers must opt in so protocol-3 clients retain the legacy closed response shape.
     * @return this request
     */
    public McpDiscoverRequest setIncludeEffectiveSource(Boolean value) {
        this.includeEffectiveSource = value;
        return this;
    }

    /**
     * Sets the {@code pluginDirectories} property.
     *
     * @param value External plugin directories mounted for this discovery only. Relative paths resolve against workingDirectory. The runtime resolves trust and policy; these directories do not become trusted built-ins or change settings.
     * @return this request
     */
    public McpDiscoverRequest setPluginDirectories(List<String> value) {
        this.pluginDirectories = value;
        return this;
    }

    /**
     * Sets the {@code options} property.
     *
     * @param value Read-only invocation options layered over runtime-owned configuration. These options never assert account, workspace trust, or managed policy.
     * @return this request
     */
    public McpDiscoverRequest setOptions(McpDiscoveryOptions value) {
        this.options = value;
        return this;
    }

    /**
     * Sets the {@code includeConfiguration} property.
     *
     * @param value Include each server's effective serializable configuration and structured diagnostics. Omitted or false preserves the existing response shape exactly.
     * @return this request
     */
    public McpDiscoverRequest setIncludeConfiguration(Boolean value) {
        this.includeConfiguration = value;
        return this;
    }
}
