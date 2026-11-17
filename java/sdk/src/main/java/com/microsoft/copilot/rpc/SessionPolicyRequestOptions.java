/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
/** Grouped SessionPolicyRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionPolicyRequestOptions {

    @JsonProperty("excludedManagedSettings")
    private List<String> excludedManagedSettings;

    @JsonProperty("enforceManagedModelDefaults")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean enforceManagedModelDefaults;

    @JsonProperty("managedSettings")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private ManagedSettings managedSettings;

    /** Gets excluded managed-setting paths. @return paths, or {@code null} */
    public List<String> getExcludedManagedSettings() {
        return excludedManagedSettings;
    }

    /** Sets excluded managed-setting paths. @param paths setting paths */
    public SessionPolicyRequestOptions setExcludedManagedSettings(List<String> paths) {
        this.excludedManagedSettings = paths;
        return this;
    }

    /** Gets managed model lock enforcement. @return the flag, or {@code null} */
    public Boolean getEnforceManagedModelDefaults() {
        return enforceManagedModelDefaults;
    }

    /** Sets managed model lock enforcement. @param enforce whether to enforce */
    public SessionPolicyRequestOptions setEnforceManagedModelDefaults(boolean enforce) {
        this.enforceManagedModelDefaults = enforce;
        return this;
    }

    /** @return host-injected managed settings, or {@code null} when unset */
    public ManagedSettings getManagedSettings() {
        return managedSettings;
    }

    /**
     * @param managedSettings
     *            host-injected managed settings
     */
    public SessionPolicyRequestOptions setManagedSettings(ManagedSettings managedSettings) {
        this.managedSettings = managedSettings;
        return this;
    }

    /** Clears managed model lock enforcement. */
    public void clearEnforceManagedModelDefaults() {
        this.enforceManagedModelDefaults = null;
    }
}
