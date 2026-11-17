/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.PermissionMode;
import com.microsoft.copilot.generated.rpc.SandboxConfig;
import java.util.Optional;
/** Grouped SessionPermission configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionPermissionOptions {

    private PermissionHandler onRequest;
    private SandboxConfig sandbox;
    private PermissionMode mode;

    /**
     * Gets the permission request handler.
     *
     * @return the permission handler
     */
    @JsonIgnore
    public PermissionHandler getOnRequest() {
        return onRequest;
    }

    /**
     * Sets a handler for permission requests from the assistant.
     *
     * @param onRequest
     *            the permission handler
     * @return this config for method chaining
     * @see PermissionHandler
     */
    public SessionPermissionOptions setOnRequest(PermissionHandler onRequest) {
        this.onRequest = onRequest;
        return this;
    }

    /**
     * Gets the sandbox configuration applied during session construction.
     *
     * @return the sandbox configuration, or {@code null} when omitted
     */
    @CopilotExperimental
    public SandboxConfig getSandbox() {
        return sandbox;
    }

    /**
     * SandboxConfig configuration applied when the session is constructed, before
     * MCP servers start. Same shape as the sandbox option of session options
     * update; an identical later update is a no-op.
     *
     * @param sandbox
     *            the sandbox configuration
     * @return this config for method chaining
     */
    @CopilotExperimental
    public SessionPermissionOptions setSandbox(SandboxConfig sandbox) {
        this.sandbox = sandbox;
        return this;
    }

    /**
     * Gets the initial permission mode.
     *
     * @return an optional containing the mode, or empty when unset
     */
    @JsonIgnore
    @CopilotExperimental
    public Optional<PermissionMode> getMode() {
        return Optional.ofNullable(mode);
    }

    /**
     * Permission mode applied before resumed pending work continues and before the
     * resume returns. Uses the same checks as {@code session.permissions.setMode};
     * if it is refused (for example by managed policy), continued work runs under
     * the mode {@code CopilotSession.getPermissionModeResult()} reports. Leave
     * unset to keep the restored mode.
     *
     * @param mode
     *            the permission mode to apply
     * @return this config for method chaining
     */
    @CopilotExperimental
    public SessionPermissionOptions setMode(PermissionMode mode) {
        this.mode = mode;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionPermissionOptions clone() {
        SessionPermissionOptions copy = new SessionPermissionOptions();
        copy.onRequest = this.onRequest;
        copy.sandbox = this.sandbox;
        copy.mode = this.mode;
        return copy;
    }

}
