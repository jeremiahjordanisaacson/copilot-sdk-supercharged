/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.AutopilotOptions;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
/** Grouped SessionExecution configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionExecutionOptions {

    private SessionLimitsConfig limits;
    private AutopilotOptions autopilot;

    /**
     * Gets the limits for this session's current accounting window.
     *
     * @return the session limits, or {@code null} if not set
     */
    @CopilotExperimental
    public SessionLimitsConfig getLimits() {
        return limits;
    }

    /**
     * Sets limits for this session's current accounting window.
     *
     * @param limits
     *            the session limits
     * @return this config instance for method chaining
     */
    @CopilotExperimental
    public SessionExecutionOptions setLimits(SessionLimitsConfig limits) {
        this.limits = limits;
        return this;
    }

    /**
     * Gets the Autopilot policy patch, independently of the session mode.
     *
     * @return the policy, or {@code null} to preserve resident values
     */
    @CopilotExperimental
    public AutopilotOptions getAutopilot() {
        return autopilot;
    }

    /**
     * Patches resident policy on warm resume. Re-supply on cold resume.
     *
     * @param autopilot
     *            the policy; omitted fields preserve resident values
     * @return this instance for method chaining
     */
    @CopilotExperimental
    public SessionExecutionOptions setAutopilot(AutopilotOptions autopilot) {
        this.autopilot = autopilot;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionExecutionOptions clone() {
        SessionExecutionOptions copy = new SessionExecutionOptions();
        copy.limits = this.limits;
        copy.autopilot = this.autopilot;
        return copy;
    }

    /**
     * Clears the limits setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    @CopilotExperimental
    public SessionExecutionOptions clearLimits() {
        this.limits = null;
        return this;
    }

    /**
     * Omits the Autopilot policy, using runtime defaults.
     *
     * @return this instance for method chaining
     */
    @CopilotExperimental
    public SessionExecutionOptions clearAutopilot() {
        this.autopilot = null;
        return this;
    }
}
