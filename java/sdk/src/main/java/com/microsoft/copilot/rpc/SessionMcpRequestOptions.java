/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.List;
import java.util.Map;
/** Grouped SessionMcpRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionMcpRequestOptions {

    @JsonProperty("servers")
    private Map<String, McpServerConfig> servers;

    @JsonProperty("oauthTokenStorage")
    private String oauthTokenStorage;

    @JsonProperty("authClientIdMetadataUrl")
    private String authClientIdMetadataUrl;

    @JsonProperty("envValueMode")
    private String envValueMode;

    @JsonProperty("disabledServers")
    private List<String> disabledServers;

    @JsonProperty("enableApps")
    private Boolean enableApps;

    /** Gets MCP servers. @return the servers map */
    public Map<String, McpServerConfig> getServers() {
        return servers == null ? null : Collections.unmodifiableMap(servers);
    }

    /** Sets MCP servers. @param servers the servers map */
    public SessionMcpRequestOptions setServers(Map<String, McpServerConfig> servers) {
        this.servers = servers;
        return this;
    }

    /** Gets MCP OAuth token storage mode. @return the storage mode */
    public String getOauthTokenStorage() {
        return oauthTokenStorage;
    }

    /**
     * Sets MCP OAuth token storage mode. @param oauthTokenStorage the storage mode
     */
    public SessionMcpRequestOptions setOauthTokenStorage(String oauthTokenStorage) {
        this.oauthTokenStorage = oauthTokenStorage;
        return this;
    }

    /** Gets the host OAuth client metadata URL. @return the metadata URL */
    public String getAuthClientIdMetadataUrl() {
        return authClientIdMetadataUrl;
    }

    /** Sets the host OAuth client metadata URL. @param url the metadata URL */
    public SessionMcpRequestOptions setAuthClientIdMetadataUrl(String url) {
        this.authClientIdMetadataUrl = url;
        return this;
    }

    /** Gets MCP environment variable value mode. @return the mode */
    public String getEnvValueMode() {
        return envValueMode;
    }

    /** Sets MCP environment variable value mode. @param envValueMode the mode */
    public SessionMcpRequestOptions setEnvValueMode(String envValueMode) {
        this.envValueMode = envValueMode;
        return this;
    }

    /** Gets disabled MCP server names. @return the server names */
    public List<String> getDisabledServers() {
        return disabledServers == null ? null : Collections.unmodifiableList(disabledServers);
    }

    /**
     * Sets disabled MCP server names. @param disabledServers the server names
     */
    public SessionMcpRequestOptions setDisabledServers(List<String> disabledServers) {
        this.disabledServers = disabledServers;
        return this;
    }

    /** Gets the enableApps flag. @return the flag */
    public Boolean getEnableApps() {
        return enableApps;
    }

    /** Sets the enableApps flag. @param enableApps the flag */
    public SessionMcpRequestOptions setEnableApps(boolean enableApps) {
        this.enableApps = enableApps;
        return this;
    }

    /** Clears the enableApps setting, reverting to the default behavior. */
    public void clearEnableApps() {
        this.enableApps = null;
    }
}
