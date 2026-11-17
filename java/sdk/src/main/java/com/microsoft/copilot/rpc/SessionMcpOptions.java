/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
/** Grouped SessionMcp configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionMcpOptions {

    private McpAuthHandler onAuthRequest;
    private Map<String, McpServerConfig> servers;
    private String oauthTokenStorage;
    private String authClientIdMetadataUrl;
    private List<String> disabledServers;
    private boolean enableApps;

    /**
     * Gets the MCP OAuth request handler.
     *
     * @return the handler, or {@code null} if not set
     */
    @JsonIgnore
    public McpAuthHandler getOnAuthRequest() {
        return onAuthRequest;
    }

    /**
     * Sets the MCP OAuth request handler.
     *
     * @param onAuthRequest
     *            the handler
     * @return this config instance for method chaining
     */
    public SessionMcpOptions setOnAuthRequest(McpAuthHandler onAuthRequest) {
        this.onAuthRequest = onAuthRequest;
        return this;
    }

    /**
     * Gets the MCP server configurations.
     *
     * @return the MCP servers map
     */
    public Map<String, McpServerConfig> getServers() {
        return servers == null ? null : Collections.unmodifiableMap(servers);
    }

    /**
     * Sets MCP (Model Context Protocol) server configurations.
     *
     * @param servers
     *            the MCP servers configuration map
     * @return this config for method chaining
     */
    public SessionMcpOptions setServers(Map<String, McpServerConfig> servers) {
        this.servers = servers;
        return this;
    }

    /**
     * Gets the MCP OAuth token storage mode.
     *
     * @return the storage mode, or {@code null} if not set
     */
    public String getOauthTokenStorage() {
        return oauthTokenStorage;
    }

    /**
     * Sets the MCP OAuth token storage mode.
     * <p>
     * Controls how MCP OAuth tokens are stored for this session:
     * <ul>
     * <li>{@code "persistent"} — tokens are stored in the OS keychain (shared
     * across sessions)</li>
     * <li>{@code "in-memory"} — tokens are stored in memory and discarded when the
     * session ends</li>
     * </ul>
     * If not set and the client is in {@link CopilotClientMode#EMPTY EMPTY} mode,
     * the SDK defaults to {@code "in-memory"} for safe multitenant behavior. In
     * other modes this field is left unset.
     *
     * @param oauthTokenStorage
     *            the storage mode
     * @return this config for method chaining
     */
    public SessionMcpOptions setOauthTokenStorage(String oauthTokenStorage) {
        this.oauthTokenStorage = oauthTokenStorage;
        return this;
    }

    /**
     * Gets the OAuth Client ID Metadata Document URL identifying the host.
     *
     * @return the metadata URL, or {@code null} if not set
     */
    public String getAuthClientIdMetadataUrl() {
        return authClientIdMetadataUrl;
    }

    /**
     * Sets the OAuth Client ID Metadata Document URL identifying the host for MCP
     * authorization. Re-supply the same host identity used when the session was
     * created.
     *
     * @param authClientIdMetadataUrl
     *            the metadata URL
     * @return this config for method chaining
     */
    public SessionMcpOptions setAuthClientIdMetadataUrl(String authClientIdMetadataUrl) {
        this.authClientIdMetadataUrl = authClientIdMetadataUrl;
        return this;
    }

    /**
     * Gets exact MCP server names disabled for this session.
     *
     * @return the disabled MCP server names, or {@code null} when none are disabled
     */
    public List<String> getDisabledServers() {
        return disabledServers == null ? null : Collections.unmodifiableList(disabledServers);
    }

    /**
     * Sets exact MCP server names to disable for this session. Disabled servers are
     * not started or authenticated on create or cold resume; a resident resume
     * cannot stop servers already running.
     *
     * @param disabledServers
     *            the server names to disable
     * @return this config for method chaining
     */
    public SessionMcpOptions setDisabledServers(List<String> disabledServers) {
        this.disabledServers = disabledServers;
        return this;
    }

    /**
     * Returns whether MCP Apps (SEP-1865) UI passthrough is enabled on resume.
     *
     * @return {@code true} if the consumer has opted into MCP Apps, otherwise
     *         {@code false}
     * @see #setEnableApps(boolean)
     */
    public boolean isEnableApps() {
        return enableApps;
    }

    /**
     * Enables MCP Apps (SEP-1865) UI passthrough on the resumed session. See
     * {@link CreateSessionOptions#setEnableApps(boolean)} for full semantics
     * (runtime gate, capability inspection, renderer requirement).
     *
     * @param enableApps
     *            {@code true} to opt into MCP Apps support on resume
     * @return this config for method chaining
     */
    public SessionMcpOptions setEnableApps(boolean enableApps) {
        this.enableApps = enableApps;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionMcpOptions clone() {
        SessionMcpOptions copy = new SessionMcpOptions();
        copy.servers = this.servers != null ? new java.util.HashMap<>(this.servers) : null;
        copy.oauthTokenStorage = this.oauthTokenStorage;
        copy.authClientIdMetadataUrl = this.authClientIdMetadataUrl;
        copy.disabledServers = this.disabledServers != null ? new ArrayList<>(this.disabledServers) : null;
        copy.onAuthRequest = this.onAuthRequest;
        copy.enableApps = this.enableApps;
        return copy;
    }

}
