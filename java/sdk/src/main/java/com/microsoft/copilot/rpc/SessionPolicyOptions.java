/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
/** Grouped SessionPolicy configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionPolicyOptions {

    private List<String> excludedManagedSettings;
    private Boolean enforceManagedModelDefaults;
    private ManagedSettings managedSettings;

    /**
     * Gets the setting paths excluded on resume.
     *
     * @return excluded paths, or {@code null} for full enforcement
     */
    public List<String> getExcludedManagedSettings() {
        return excludedManagedSettings;
    }

    /**
     * Excludes exact dotted paths as on
     * {@link SessionPolicyOptions#setExcludedManagedSettings(List)}. Omitting
     * restores full enforcement on warm and cold resume.
     *
     * @param paths
     *            managed-setting paths
     * @return this configuration
     */
    public SessionPolicyOptions setExcludedManagedSettings(List<String> paths) {
        this.excludedManagedSettings = paths;
        return this;
    }

    /**
     * Gets whether locked managed model controls are enforced on resume.
     *
     * @return the explicit choice, or empty to use the runtime default
     */
    @JsonIgnore
    public Optional<Boolean> getEnforceManagedModelDefaults() {
        return Optional.ofNullable(enforceManagedModelDefaults);
    }

    /**
     * Sets whether locked managed model controls are enforced after resume.
     *
     * @param enforceManagedModelDefaults
     *            whether to enforce locked managed model controls
     * @return this config for method chaining
     */
    public SessionPolicyOptions setEnforceManagedModelDefaults(boolean enforceManagedModelDefaults) {
        this.enforceManagedModelDefaults = enforceManagedModelDefaults;
        return this;
    }

    /** @return host-injected managed settings, or {@code null} when unset */
    public ManagedSettings getManagedSettings() {
        return managedSettings;
    }

    /**
     * Supplies permissions-only managed settings for this resume. The value
     * replaces the prior injected layer and is not persisted.
     *
     * @param managedSettings
     *            the host-injected managed settings
     * @return this config for method chaining
     */
    public SessionPolicyOptions setManagedSettings(ManagedSettings managedSettings) {
        this.managedSettings = managedSettings;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionPolicyOptions clone() {
        SessionPolicyOptions copy = new SessionPolicyOptions();
        copy.excludedManagedSettings = this.excludedManagedSettings == null
                ? null
                : new ArrayList<>(this.excludedManagedSettings);
        copy.enforceManagedModelDefaults = this.enforceManagedModelDefaults;
        copy.managedSettings = this.managedSettings;
        return copy;
    }

}
