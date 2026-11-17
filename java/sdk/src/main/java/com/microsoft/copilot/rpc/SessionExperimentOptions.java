/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.Map;
import java.util.Optional;
/** Grouped SessionExperiment configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionExperimentOptions {

    private Boolean enableExperimentalMode;
    private CopilotExpAssignmentResponse assignments;
    private Map<String, Boolean> flags;

    /**
     * Controls whether the session enables experimental features.
     *
     * @return {@code true} when experimental features are enabled, {@code false}
     *         when they are disabled, or empty to use the mode-specific default
     */
    @JsonIgnore
    public Optional<Boolean> getEnableExperimentalMode() {
        return Optional.ofNullable(enableExperimentalMode);
    }

    /**
     * Controls whether the session enables experimental features.
     *
     * @param enableExperimentalMode
     *            {@code true} to enable experimental features; {@code false} to
     *            disable them
     * @return this config for method chaining
     */
    public SessionExperimentOptions setEnableExperimentalMode(boolean enableExperimentalMode) {
        this.enableExperimentalMode = enableExperimentalMode;
        return this;
    }

    /**
     * Gets the ExP assignment ("flight") data injected by a trusted integrator.
     *
     * @return the ExP assignment data, or {@code null} if not set
     */
    public CopilotExpAssignmentResponse getAssignments() {
        return assignments;
    }

    /**
     * Sets ExP assignment ("flight") data injected by a trusted integrator.
     * <p>
     * See {@link CreateSessionOptions#setAssignments(CopilotExpAssignmentResponse)}
     * for details. The runtime supports injecting ExP assignments on resume as well
     * as create.
     *
     * @param assignments
     *            the ExP assignment data
     * @return this config for method chaining
     */
    public SessionExperimentOptions setAssignments(CopilotExpAssignmentResponse assignments) {
        this.assignments = assignments;
        return this;
    }

    /** Gets host-resolved feature-flag values. @return the feature flags */
    public Map<String, Boolean> getFlags() {
        return flags;
    }

    /**
     * Sets feature-flag values resolved by the host to apply on resume.
     *
     * @param flags
     *            the feature flags
     * @return this config for method chaining
     */
    public SessionExperimentOptions setFlags(Map<String, Boolean> flags) {
        this.flags = flags;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionExperimentOptions clone() {
        SessionExperimentOptions copy = new SessionExperimentOptions();
        copy.enableExperimentalMode = this.enableExperimentalMode;
        copy.flags = this.flags != null ? new java.util.HashMap<>(this.flags) : null;
        copy.assignments = this.assignments;
        return copy;
    }

    /**
     * Clears the enableExperimentalMode setting. In {@link CopilotClientMode#EMPTY
     * EMPTY} mode this defaults to {@code false}; otherwise the runtime decides.
     *
     * @return this instance for method chaining
     */
    public SessionExperimentOptions clearEnableExperimentalMode() {
        this.enableExperimentalMode = null;
        return this;
    }
}
