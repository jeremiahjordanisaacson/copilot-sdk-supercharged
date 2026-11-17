/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import java.util.Objects;
/** Grouped ClientInstallation configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ClientInstallationOptions {

    private InstallationConfirmationHandler confirmationHandler;

    /**
     * Gets the connection-level installation confirmation handler.
     *
     * @return the handler, or {@code null} if not set
     */
    @JsonIgnore
    @CopilotExperimental
    public InstallationConfirmationHandler getConfirmationHandler() {
        return confirmationHandler;
    }

    /**
     * Sets the connection-level installation confirmation handler.
     * <p>
     * When provided, the client installs the {@code installations.confirm}
     * reverse-RPC handler. This does not enable installation capabilities or call a
     * runtime registration RPC.
     *
     * @param confirmationHandler
     *            the handler (must not be {@code null})
     * @return this options instance for method chaining
     */
    @CopilotExperimental
    public ClientInstallationOptions setConfirmationHandler(InstallationConfirmationHandler confirmationHandler) {
        this.confirmationHandler = Objects.requireNonNull(confirmationHandler, "confirmationHandler must not be null");
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public ClientInstallationOptions clone() {
        ClientInstallationOptions copy = new ClientInstallationOptions();
        copy.confirmationHandler = this.confirmationHandler;
        return copy;
    }

}
