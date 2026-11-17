/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
/** Grouped SessionExperimentRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionExperimentRequestOptions {

    @JsonProperty("enableExperimentalMode")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean enableExperimentalMode;

    @JsonProperty("assignments")
    private CopilotExpAssignmentResponse assignments;

    @JsonProperty("flags")
    private Map<String, Boolean> flags;

    /**
     * Gets the enableExperimentalMode flag.
     *
     * @return the flag
     */
    public Boolean getEnableExperimentalMode() {
        return enableExperimentalMode;
    }

    /**
     * Sets the enableExperimentalMode flag.
     *
     * @param enableExperimentalMode
     *            the flag
     */
    public SessionExperimentRequestOptions setEnableExperimentalMode(boolean enableExperimentalMode) {
        this.enableExperimentalMode = enableExperimentalMode;
        return this;
    }

    /** Gets the ExP assignment data. @return the ExP assignment data */
    public CopilotExpAssignmentResponse getAssignments() {
        return assignments;
    }

    /**
     * Sets the ExP assignment data. @param assignments the ExP assignment data
     */
    public SessionExperimentRequestOptions setAssignments(CopilotExpAssignmentResponse assignments) {
        this.assignments = assignments;
        return this;
    }

    /** Gets host-resolved feature flags. @return the feature flags */
    public Map<String, Boolean> getFlags() {
        return flags;
    }

    /** Sets host-resolved feature flags. @param flags the feature flags */
    public SessionExperimentRequestOptions setFlags(Map<String, Boolean> flags) {
        this.flags = flags;
        return this;
    }

    /**
     * Clears the enableExperimentalMode setting, reverting to the default behavior.
     */
    public void clearEnableExperimentalMode() {
        this.enableExperimentalMode = null;
    }
}
