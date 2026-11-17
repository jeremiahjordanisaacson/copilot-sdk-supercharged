/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.GitHubTelemetryNotification;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
/** Grouped ClientGitHub configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ClientGitHubOptions {

    private String token;
    private Function<GitHubTelemetryNotification, CompletableFuture<Void>> onTelemetry;
    private Boolean useLoggedInUser;

    /**
     * Gets the GitHub token for authentication.
     *
     * @return the GitHub token, or {@code null} to use other authentication methods
     */
    public String getToken() {
        return token;
    }

    /**
     * Sets the GitHub token to use for authentication.
     * <p>
     * When provided, the token is passed to the CLI server via environment
     * variable. This takes priority over other authentication methods.
     *
     * @param token
     *            the GitHub token (must not be {@code null} or empty)
     * @return this options instance for method chaining
     * @throws IllegalArgumentException
     *             if {@code token} is {@code null} or empty
     */
    public ClientGitHubOptions setToken(String token) {
        this.token = Objects.requireNonNull(token, "token must not be null");
        return this;
    }

    /**
     * Gets the connection-level GitHub telemetry forwarding handler.
     *
     * <p>
     * Experimental: this option may change or be removed without notice.
     *
     * @return the async telemetry handler, or {@code null} if not set
     */
    @JsonIgnore
    @CopilotExperimental
    public Function<GitHubTelemetryNotification, CompletableFuture<Void>> getOnTelemetry() {
        return onTelemetry;
    }

    /**
     * Sets a connection-level handler for GitHub telemetry forwarding
     * (experimental).
     *
     * <p>
     * When provided, the client opts every session it creates or resumes into
     * telemetry forwarding, and the runtime forwards each per-session telemetry
     * event to this handler via the {@code gitHubTelemetry.event} notification. The
     * handler returns a {@link CompletableFuture} that completes when asynchronous
     * processing is finished.
     *
     * @param onTelemetry
     *            the async telemetry handler (must not be {@code null})
     * @return this options instance for method chaining
     * @throws IllegalArgumentException
     *             if {@code onTelemetry} is {@code null}
     */
    @CopilotExperimental
    public ClientGitHubOptions setOnTelemetry(
            Function<GitHubTelemetryNotification, CompletableFuture<Void>> onTelemetry) {
        this.onTelemetry = Objects.requireNonNull(onTelemetry, "onTelemetry must not be null");
        return this;
    }

    /**
     * Returns whether to use the logged-in user for authentication.
     *
     * @return an {@link Optional} containing the boolean value, or empty if not set
     */
    @JsonIgnore
    public Optional<Boolean> getUseLoggedInUser() {
        return Optional.ofNullable(useLoggedInUser);
    }

    /**
     * Sets whether to use the logged-in user for authentication.
     * <p>
     * When true, the CLI server will attempt to use stored OAuth tokens or gh CLI
     * auth. When false, only explicit tokens such as gitHubToken are used;
     * environment variable tokens (COPILOT_GITHUB_TOKEN, GH_TOKEN, GITHUB_TOKEN)
     * are ignored. Default: true (but defaults to false when gitHubToken is
     * provided).
     * <p>
     *
     * @param useLoggedInUser
     *            {@code true} to use logged-in user auth, {@code false} otherwise
     * @return this options instance for method chaining
     */
    public ClientGitHubOptions setUseLoggedInUser(boolean useLoggedInUser) {
        this.useLoggedInUser = useLoggedInUser;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public ClientGitHubOptions clone() {
        ClientGitHubOptions copy = new ClientGitHubOptions();
        copy.token = this.token;
        copy.onTelemetry = this.onTelemetry;
        copy.useLoggedInUser = this.useLoggedInUser;
        return copy;
    }

    /**
     * Clears the useLoggedInUser setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public ClientGitHubOptions clearUseLoggedInUser() {
        this.useLoggedInUser = null;
        return this;
    }
}
