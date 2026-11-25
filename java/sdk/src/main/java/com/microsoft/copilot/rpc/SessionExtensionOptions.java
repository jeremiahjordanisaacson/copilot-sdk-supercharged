/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;

/**
 * Existing extension activation and SDK injection for a session. Source
 * selection does not enable extensions or grant trust.
 */
@CopilotExperimental
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionExtensionOptions {
    private Boolean enabled;
    private String sdkPath;

    /**
     * Gets extension enablement.
     *
     * @return the explicit choice, or {@code null} for existing runtime defaults
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets extension enablement independently of general configuration discovery.
     *
     * @param enabled
     *            the explicit choice, or {@code null} for runtime defaults
     * @return this options instance
     */
    public SessionExtensionOptions setEnabled(Boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    /**
     * Gets the extension SDK override.
     *
     * @return the SDK directory, or {@code null} for the existing runtime choice
     */
    public String getSdkPath() {
        return sdkPath;
    }

    /**
     * Selects the SDK directory injected into this session's extension children.
     * The directory contains the SDK's {@code index.js} and {@code extension.js}.
     *
     * @param sdkPath
     *            the SDK directory, or {@code null} for the runtime choice
     * @return this options instance
     */
    public SessionExtensionOptions setSdkPath(String sdkPath) {
        this.sdkPath = sdkPath;
        return this;
    }

    /**
     * Copies the session extension choices.
     *
     * @return an independent options copy
     */
    @Override
    public SessionExtensionOptions clone() {
        return new SessionExtensionOptions().setEnabled(enabled).setSdkPath(sdkPath);
    }
}
