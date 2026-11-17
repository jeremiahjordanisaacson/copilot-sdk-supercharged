/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.PermissionMode;
import com.microsoft.copilot.generated.rpc.SandboxConfig;
/** Grouped SessionPermissionRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionPermissionRequestOptions {

    @JsonProperty("request")
    private Boolean request;

    @JsonProperty("sandbox")
    private SandboxConfig sandbox;

    @JsonProperty("mode")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private PermissionMode mode;

    /** Gets request permission flag. @return the flag */
    public Boolean getRequest() {
        return request;
    }

    /** Sets request permission flag. @param request the flag */
    public SessionPermissionRequestOptions setRequest(boolean request) {
        this.request = request;
        return this;
    }

    /** Gets sandbox configuration. @return the sandbox configuration */
    @CopilotExperimental
    public SandboxConfig getSandbox() {
        return sandbox;
    }

    /**
     * Sets sandbox configuration. @param sandbox the sandbox configuration
     */
    @CopilotExperimental
    public SessionPermissionRequestOptions setSandbox(SandboxConfig sandbox) {
        this.sandbox = sandbox;
        return this;
    }

    /** @return the initial permission mode, or {@code null} when unset */
    @CopilotExperimental
    public PermissionMode getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the permission mode to apply before resumed pending work continues
     */
    @CopilotExperimental
    public SessionPermissionRequestOptions setMode(PermissionMode mode) {
        this.mode = mode;
        return this;
    }

    /**
     * Clears the request setting, reverting to the default behavior.
     */
    public void clearRequest() {
        this.request = null;
    }
}
