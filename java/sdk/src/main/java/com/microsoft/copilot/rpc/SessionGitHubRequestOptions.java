/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
/** Grouped SessionGitHubRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionGitHubRequestOptions {

    @JsonProperty("capi")
    private CapiSessionOptions capi;

    @JsonProperty("telemetryEnabled")
    private Boolean telemetryEnabled;

    @JsonProperty("telemetryForwarding")
    private Boolean telemetryForwarding;

    @JsonProperty("mcp")
    private GitHubMcpToolConfig mcp;

    @JsonProperty("token")
    private String token;

    @JsonProperty("tokenProviderRegistrationId")
    private String tokenProviderRegistrationId;

    @JsonProperty("fetchManagedSettings")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean fetchManagedSettings;

    @JsonProperty("interactiveUser")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean interactiveUser;

    /** Gets the CAPI session options. @return the CAPI session options */
    public CapiSessionOptions getCapi() {
        return capi;
    }

    /** Sets the CAPI session options. @param capi the CAPI session options */
    public SessionGitHubRequestOptions setCapi(CapiSessionOptions capi) {
        this.capi = capi;
        return this;
    }

    /** Gets enable session telemetry flag. @return the flag */
    public Boolean getTelemetryEnabled() {
        return telemetryEnabled;
    }

    /**
     * Sets enable session telemetry flag. @param telemetryEnabled the flag
     */
    public SessionGitHubRequestOptions setTelemetryEnabled(boolean telemetryEnabled) {
        this.telemetryEnabled = telemetryEnabled;
        return this;
    }

    /** Gets the GitHub telemetry forwarding flag. @return the flag */
    public Boolean getTelemetryForwarding() {
        return telemetryForwarding;
    }

    /**
     * Sets the GitHub telemetry forwarding flag. @param telemetryForwarding the
     * flag
     */
    public SessionGitHubRequestOptions setTelemetryForwarding(boolean telemetryForwarding) {
        this.telemetryForwarding = telemetryForwarding;
        return this;
    }

    /** Gets the GitHub MCP tool configuration. @return the configuration */
    public GitHubMcpToolConfig getMcp() {
        return mcp;
    }

    /** Sets the GitHub MCP tool configuration. @param config the value */
    public SessionGitHubRequestOptions setMcp(GitHubMcpToolConfig config) {
        this.mcp = config;
        return this;
    }

    /** Gets the GitHub token for per-session authentication. @return the token */
    public String getToken() {
        return token;
    }

    /**
     * Sets the GitHub token for per-session authentication. @param token the token
     */
    public SessionGitHubRequestOptions setToken(String token) {
        this.token = token;
        return this;
    }

    /**
     * Gets the token-provider registration ID. @return the opaque registration ID
     */
    public String getTokenProviderRegistrationId() {
        return tokenProviderRegistrationId;
    }

    /**
     * Sets the token-provider registration ID. @param registrationId the opaque
     * registration ID
     */
    public SessionGitHubRequestOptions setTokenProviderRegistrationId(String registrationId) {
        this.tokenProviderRegistrationId = registrationId;
        return this;
    }

    /**
     * Gets the self-fetch managed settings flag. @return the flag, or {@code null}
     * if not set
     */
    public Boolean getFetchManagedSettings() {
        return fetchManagedSettings;
    }

    /**
     * Sets the self-fetch managed settings flag. @param fetchManagedSettings the
     * flag
     */
    public SessionGitHubRequestOptions setFetchManagedSettings(boolean fetchManagedSettings) {
        this.fetchManagedSettings = fetchManagedSettings;
        return this;
    }

    /** @return whether this session is human-driven, or {@code null} when unset */
    public Boolean getInteractiveUser() {
        return interactiveUser;
    }

    /**
     * @param interactiveUser
     *            whether a human drives this session
     */
    public SessionGitHubRequestOptions setInteractiveUser(boolean interactiveUser) {
        this.interactiveUser = interactiveUser;
        return this;
    }

    /**
     * Clears the telemetryEnabled setting, reverting to the default behavior.
     */
    public void clearTelemetryEnabled() {
        this.telemetryEnabled = null;
    }

    /**
     * Clears the telemetryForwarding setting, reverting to the default behavior.
     */
    public void clearTelemetryForwarding() {
        this.telemetryForwarding = null;
    }

    /**
     * Clears the fetchManagedSettings setting, reverting to the default behavior.
     */
    public void clearFetchManagedSettings() {
        this.fetchManagedSettings = null;
    }

    /** Clears the interactiveUser setting, reverting to the default behavior. */
    public void clearInteractiveUser() {
        this.interactiveUser = null;
    }
}
