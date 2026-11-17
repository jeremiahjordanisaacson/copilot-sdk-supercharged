/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.microsoft.copilot.CopilotExperimental;
import java.util.Objects;
/** Grouped ClientExtension configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ClientExtensionOptions {

    private ExtensionLaunchProvider launchProvider;

    /**
     * Gets the connection-level extension launch profile provider.
     *
     * @return the provider, or {@code null} if not set
     */
    @JsonIgnore
    @CopilotExperimental
    public ExtensionLaunchProvider getLaunchProvider() {
        return launchProvider;
    }

    /**
     * Sets the connection-level extension launch profile provider.
     * <p>
     * When provided, the client registers the provider during startup before any
     * session can be created.
     *
     * @param launchProvider
     *            the provider (must not be {@code null})
     * @return this options instance for method chaining
     */
    @CopilotExperimental
    public ClientExtensionOptions setLaunchProvider(ExtensionLaunchProvider launchProvider) {
        this.launchProvider = Objects.requireNonNull(launchProvider, "launchProvider must not be null");
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public ClientExtensionOptions clone() {
        ClientExtensionOptions copy = new ClientExtensionOptions();
        copy.launchProvider = this.launchProvider;
        return copy;
    }

}
