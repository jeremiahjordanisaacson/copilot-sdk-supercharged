/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.Optional;
/** Grouped SessionGit configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionGitOptions {

    private Boolean coauthorEnabled;
    private Boolean enableHostGitOperations;

    /**
     * Gets whether the runtime may append a Co-authored-by trailer.
     *
     * @return the coauthor enabled flag, or empty if not explicitly set
     * @since 1.3.0
     */
    @JsonIgnore
    public Optional<Boolean> getCoauthorEnabled() {
        return Optional.ofNullable(coauthorEnabled);
    }

    /**
     * Sets whether the runtime is allowed to append a {@code Co-authored-by}
     * trailer. Subagents inherit this setting.
     * <p>
     * This option is sent to the server via a {@code session.options.update}
     * JSON-RPC call immediately after session resume. In
     * {@link CopilotClientMode#EMPTY EMPTY} mode the default is {@code false}
     * (disabled); in {@link CopilotClientMode#COPILOT_CLI COPILOT_CLI} mode the
     * value is forwarded only when explicitly set.
     *
     * @param coauthorEnabled
     *            whether coauthor is enabled
     * @return this config instance for method chaining
     * @since 1.3.0
     */
    public SessionGitOptions setCoauthorEnabled(boolean coauthorEnabled) {
        this.coauthorEnabled = coauthorEnabled;
        return this;
    }

    /**
     * Gets whether host git operations are enabled.
     *
     * @return an {@link java.util.Optional} containing {@code true} to enable host
     *         git operations or {@code false} to disable them, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getEnableHostGitOperations() {
        return Optional.ofNullable(enableHostGitOperations);
    }

    /**
     * Sets whether git operations on the host filesystem are enabled.
     *
     * @param enableHostGitOperations
     *            {@code true} to enable host git operations, {@code false} to
     *            disable them
     * @return this config for method chaining
     */
    public SessionGitOptions setEnableHostGitOperations(boolean enableHostGitOperations) {
        this.enableHostGitOperations = enableHostGitOperations;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionGitOptions clone() {
        SessionGitOptions copy = new SessionGitOptions();
        copy.enableHostGitOperations = this.enableHostGitOperations;
        copy.coauthorEnabled = this.coauthorEnabled;
        return copy;
    }

    /**
     * Clears the coauthorEnabled setting.
     *
     * @return this instance for method chaining
     * @since 1.3.0
     */
    public SessionGitOptions clearCoauthorEnabled() {
        this.coauthorEnabled = null;
        return this;
    }

    /**
     * Clears the enableHostGitOperations setting, reverting to the default
     * behavior.
     *
     * @return this instance for method chaining
     */
    public SessionGitOptions clearEnableHostGitOperations() {
        this.enableHostGitOperations = null;
        return this;
    }
}
