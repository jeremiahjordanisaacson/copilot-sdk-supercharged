/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
/** Grouped SessionHook configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionHookOptions {

    private SessionHooks handlers;
    private List<String> directories;
    private Boolean enableRepositoryHooks;

    /**
     * Gets the hook handlers configuration.
     *
     * @return the session handlers
     */
    @JsonIgnore
    public SessionHooks getHandlers() {
        return handlers;
    }

    /**
     * Sets hook handlers for session lifecycle events.
     *
     * @param handlers
     *            the handlers configuration
     * @return this config for method chaining
     * @see SessionHooks
     */
    public SessionHookOptions setHandlers(SessionHooks handlers) {
        this.handlers = handlers;
        return this;
    }

    /**
     * Gets explicit discovery-only file-hook roots, separate from callback hooks.
     * No general filesystem grant. Available without configuration discovery or
     * file-hook enablement (repository-origin hooks only), subject to hook/managed
     * disables. Relative paths resolve once when supplied against the request's
     * effective working directory; retained roots do not move. Null and omission
     * are equivalent; an empty list clears only the SDK contribution.
     *
     * @return the file-hook directory paths
     */
    public List<String> getDirectories() {
        return directories == null ? null : Collections.unmodifiableList(directories);
    }

    /**
     * Sets explicit discovery-only file-hook roots. See {@link #getDirectories()}.
     *
     * @param directories
     *            the file-hook directory paths
     * @return this config for method chaining
     */
    public SessionHookOptions setDirectories(List<String> directories) {
        this.directories = directories == null ? null : new ArrayList<>(directories);
        return this;
    }

    /**
     * Gets whether repository-origin file hooks from {@code .github/hooks/} and
     * trusted repository-configured roots are enabled. Does not control explicit
     * {@link #getDirectories()} or callback hooks.
     *
     * @return an {@link java.util.Optional} containing {@code true} to enable file
     *         hooks or {@code false} to disable them, or
     *         {@link java.util.Optional#empty()} to use the default behavior
     */
    @JsonIgnore
    public Optional<Boolean> getEnableRepositoryHooks() {
        return Optional.ofNullable(enableRepositoryHooks);
    }

    /**
     * Controls repository-origin hooks, including accepted startup additional
     * roots. This gate supports live updates. Additional-root eligibility and
     * {@code DISABLE_ADDITIONAL_DIRECTORY_HOOKS} remain startup-scoped. Explicit
     * hook directories and callbacks are independent.
     *
     * @param enableRepositoryHooks
     *            {@code true} to enable file hooks, {@code false} to disable them
     * @return this config for method chaining
     */
    public SessionHookOptions setEnableRepositoryHooks(boolean enableRepositoryHooks) {
        this.enableRepositoryHooks = enableRepositoryHooks;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionHookOptions clone() {
        SessionHookOptions copy = new SessionHookOptions();
        copy.handlers = this.handlers;
        copy.enableRepositoryHooks = this.enableRepositoryHooks;
        copy.directories = this.directories != null ? new ArrayList<>(this.directories) : null;
        return copy;
    }

    /**
     * Clears the enableRepositoryHooks setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    public SessionHookOptions clearEnableRepositoryHooks() {
        this.enableRepositoryHooks = null;
        return this;
    }
}
