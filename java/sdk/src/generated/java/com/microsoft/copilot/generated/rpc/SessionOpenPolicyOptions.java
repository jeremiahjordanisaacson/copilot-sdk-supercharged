/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Host-injected session policy.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenPolicyOptions(
    /** Managed-setting paths this trusted SDK host enforces externally or does not support, excluded from fetched, device, and policy-helper policy for this session. Exact dotted paths exclude a setting and its descendants (for example sandbox or permissions.disableBypassPermissionsMode); wildcards and array indices are not supported. Host-injected managedSettings and policy availability checks are not excluded. Not persisted; re-supply on resume, where omission restores full enforcement. Requires enableManagedSettings to self-fetch policy. */
    @JsonProperty("excludedManagedSettings") List<String> excludedManagedSettings,
    /** Opt in to enforcing non-overridable managed model controls on session model, Auto-tier, reasoning-effort, and context-tier changes. Managed defaults still apply when omitted; this option only turns conflicting changes into errors. */
    @JsonProperty("enforceManagedModelDefaults") Boolean enforceManagedModelDefaults,
    /** Permissions-only enterprise policy injected by the SDK host at session create or resume. Composes restrictively with self-fetched and device policy and is not persisted. */
    @JsonProperty("managedSettings") SessionManagedSettings managedSettings,
    /** Additional content-exclusion policies to merge into the session policy set. */
    @CopilotExperimental
    @JsonProperty("additionalContentExclusionPolicies") List<SessionOpenOptionsAdditionalContentExclusionPolicy> additionalContentExclusionPolicies
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenPolicyOptions {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    public static final class Builder {
        private Builder() {}
        private List<String> excludedManagedSettings;
        private Boolean enforceManagedModelDefaults;
        private SessionManagedSettings managedSettings;
        private List<SessionOpenOptionsAdditionalContentExclusionPolicy> additionalContentExclusionPolicies;

        /**
         * Sets the {@code excludedManagedSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludedManagedSettings(List<String> value) {
            this.excludedManagedSettings = value;
            return this;
        }

        /**
         * Sets the {@code enforceManagedModelDefaults} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enforceManagedModelDefaults(Boolean value) {
            this.enforceManagedModelDefaults = value;
            return this;
        }

        /**
         * Sets the {@code managedSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder managedSettings(SessionManagedSettings value) {
            this.managedSettings = value;
            return this;
        }

        /**
         * Sets the {@code additionalContentExclusionPolicies} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder additionalContentExclusionPolicies(List<SessionOpenOptionsAdditionalContentExclusionPolicy> value) {
            this.additionalContentExclusionPolicies = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenPolicyOptions build() {
            return new SessionOpenPolicyOptions(excludedManagedSettings, enforceManagedModelDefaults, managedSettings, additionalContentExclusionPolicies);
        }
    }
}
