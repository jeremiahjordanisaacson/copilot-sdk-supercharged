/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.Optional;
/** Grouped SessionGitHub configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionGitHubOptions {

    private CapiSessionOptions capi;
    private Boolean telemetryEnabled;
    private GitHubMcpToolConfig mcp;
    private String token;
    @JsonIgnore
    private GitHubTokenProvider tokenProvider;
    private Boolean fetchManagedSettings;
    private Boolean interactiveUser;

    /**
     * Gets the CAPI provider-scoped session options.
     *
     * @return the CAPI session options
     */
    public CapiSessionOptions getCapi() {
        return capi;
    }

    /**
     * Sets CAPI provider-scoped session options.
     * <p>
     * Use {@link CapiSessionOptions#setEnableWebSocketResponses(Boolean)} with
     * {@code false} to force the HTTP Responses transport instead of the default
     * CAPI Responses API WebSocket transport.
     *
     * @param capi
     *            the CAPI session options
     * @return this config for method chaining
     * @see CapiSessionOptions
     */
    public SessionGitHubOptions setCapi(CapiSessionOptions capi) {
        this.capi = capi;
        return this;
    }

    /**
     * Enables or disables internal session telemetry for this session. When
     * {@code false}, disables session telemetry. {@code true} enables it for
     * eligible GitHub-authenticated sessions, including BYOK. When unset, preserves
     * a resident session's choice; otherwise defaults to enabled. Host and account
     * policies still control uploads. This is independent of
     * {@link ClientObservabilityOptions#getTelemetry()
     * CopilotClientOptions.TelemetryConfig}, which configures OpenTelemetry export
     * for observability.
     *
     * @return an {@link java.util.Optional} containing whether session telemetry is
     *         enabled, or {@link java.util.Optional#empty()} for the default
     */
    @JsonIgnore
    public Optional<Boolean> getTelemetryEnabled() {
        return Optional.ofNullable(telemetryEnabled);
    }

    /**
     * Enables or disables internal session telemetry for this session. When
     * {@code false}, disables session telemetry. {@code true} enables it for
     * eligible GitHub-authenticated sessions, including BYOK. When unset, preserves
     * a resident session's choice; otherwise defaults to enabled. Host and account
     * policies still control uploads.
     *
     * @param telemetryEnabled
     *            whether to enable session telemetry
     * @return this config for method chaining
     */
    public SessionGitHubOptions setTelemetryEnabled(boolean telemetryEnabled) {
        this.telemetryEnabled = telemetryEnabled;
        return this;
    }

    /**
     * Gets the configuration for the built-in GitHub MCP server.
     *
     * @return the GitHub MCP configuration, or {@code null}
     */
    public GitHubMcpToolConfig getMcp() {
        return mcp;
    }

    /**
     * Sets the configuration for the built-in GitHub MCP server.
     *
     * @param mcp
     *            the GitHub MCP configuration
     * @return this config instance for method chaining
     */
    public SessionGitHubOptions setMcp(GitHubMcpToolConfig mcp) {
        this.mcp = mcp;
        return this;
    }

    /**
     * Gets the GitHub token for per-session authentication.
     *
     * @return the GitHub token, or {@code null} if not set
     * @since 1.3.0
     */
    public String getToken() {
        return token;
    }

    /**
     * Sets the GitHub token for per-session authentication.
     * <p>
     * When provided, the runtime resolves this token into a full GitHub identity
     * and stores it on the session for content exclusion, model routing, and quota
     * checks.
     *
     * @param token
     *            the GitHub token for per-session authentication
     * @return this config for method chaining
     * @since 1.3.0
     */
    public SessionGitHubOptions setToken(String token) {
        this.token = token;
        return this;
    }

    /**
     * Gets the rotating GitHub token provider for the resumed session.
     *
     * @return the provider, or {@code null} when a static token is used
     */
    public GitHubTokenProvider getTokenProvider() {
        return tokenProvider;
    }

    /**
     * Sets the rotating GitHub token provider for the resumed session.
     * <p>
     * The provider receives only the effective host, optional assigned session ID,
     * and acquisition reason. It must return the remaining lifetime in seconds when
     * its callback completes. More than 3600 seconds must still remain after token
     * validation; validation time counts against the lifetime. Production GitHub
     * tokens typically last eight hours. This option is mutually exclusive with
     * {@link #setToken(String)}.
     *
     * @param tokenProvider
     *            provider used for initial acquisition and refresh
     * @return this config instance for method chaining
     */
    public SessionGitHubOptions setTokenProvider(GitHubTokenProvider tokenProvider) {
        this.tokenProvider = tokenProvider;
        return this;
    }

    /**
     * Gets whether the runtime self-fetches enterprise managed settings at session
     * bootstrap on resume.
     *
     * @return an {@link java.util.Optional} containing {@code true} to opt into
     *         self-fetching managed settings, or {@link java.util.Optional#empty()}
     *         to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getFetchManagedSettings() {
        return Optional.ofNullable(fetchManagedSettings);
    }

    /**
     * Opts the runtime into self-fetching enterprise managed settings on resume.
     * <p>
     * See {@link CreateSessionOptions#setFetchManagedSettings(boolean)} for
     * details. Re-supply on resume so the runtime re-applies the managed-settings
     * self-fetch after a CLI process restart. Serialized on the wire as
     * {@code fetchManagedSettings}.
     *
     * @param fetchManagedSettings
     *            {@code true} to opt into self-fetching managed settings
     * @return this config for method chaining
     */
    public SessionGitHubOptions setFetchManagedSettings(boolean fetchManagedSettings) {
        this.fetchManagedSettings = fetchManagedSettings;
        return this;
    }

    /** @return optional declaration that this session is human-driven */
    @JsonIgnore
    public Optional<Boolean> getInteractiveUser() {
        return Optional.ofNullable(interactiveUser);
    }

    /**
     * Re-supplies that a human drives this session so a session loaded from storage
     * keeps the declaration. It is not persisted and is ignored if the session is
     * already active in the runtime process.
     *
     * @param interactiveUser
     *            {@code true} for interactive hosts such as editors or IDE chat
     *            panels
     * @return this config for method chaining
     */
    public SessionGitHubOptions setInteractiveUser(boolean interactiveUser) {
        this.interactiveUser = interactiveUser;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionGitHubOptions clone() {
        SessionGitHubOptions copy = new SessionGitHubOptions();
        copy.capi = this.capi;
        copy.telemetryEnabled = this.telemetryEnabled;
        copy.mcp = this.mcp;
        copy.token = this.token;
        copy.tokenProvider = this.tokenProvider;
        copy.fetchManagedSettings = this.fetchManagedSettings;
        copy.interactiveUser = this.interactiveUser;
        return copy;
    }

    /**
     * Clears the telemetryEnabled setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public SessionGitHubOptions clearTelemetryEnabled() {
        this.telemetryEnabled = null;
        return this;
    }
}
