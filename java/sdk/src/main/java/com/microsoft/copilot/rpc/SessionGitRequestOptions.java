/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
/** Grouped SessionGitRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionGitRequestOptions {

    @JsonProperty("enableHostGitOperations")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean enableHostGitOperations;

    /** Gets enable host git operations flag. @return the flag */
    public Boolean getEnableHostGitOperations() {
        return enableHostGitOperations;
    }

    /**
     * Sets enable host git operations flag. @param enableHostGitOperations the flag
     */
    public SessionGitRequestOptions setEnableHostGitOperations(boolean enableHostGitOperations) {
        this.enableHostGitOperations = enableHostGitOperations;
        return this;
    }

    /**
     * Clears the enableHostGitOperations setting, reverting to the default
     * behavior.
     */
    public void clearEnableHostGitOperations() {
        this.enableHostGitOperations = null;
    }
}
