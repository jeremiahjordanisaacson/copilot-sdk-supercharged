/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.AutopilotOptions;
import com.microsoft.copilot.generated.rpc.SessionLimitsConfig;
/** Grouped SessionExecutionRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionExecutionRequestOptions {

    @JsonProperty("limits")
    private SessionLimitsConfig limits;

    @JsonProperty("autopilot")
    private AutopilotOptions autopilot;

    /** Gets the session limits. @return the session limits */
    public SessionLimitsConfig getLimits() {
        return limits;
    }

    /** Sets the session limits. @param limits the session limits */
    public SessionExecutionRequestOptions setLimits(SessionLimitsConfig limits) {
        this.limits = limits;
        return this;
    }

    /** Gets the Autopilot policy patch. @return the policy, or null */
    @CopilotExperimental
    public AutopilotOptions getAutopilot() {
        return autopilot;
    }

    /** Sets the Autopilot policy patch. @param autopilot the policy */
    @CopilotExperimental
    public SessionExecutionRequestOptions setAutopilot(AutopilotOptions autopilot) {
        this.autopilot = autopilot;
        return this;
    }
}
