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
import javax.annotation.processing.Generated;

/**
 * Policy values a host needs to describe the effective sandbox.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SandboxGetPolicyPresentationParams(
    /** Effective sandbox configuration, after managed composition. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("config") Object config,
    /** Explicit sandbox flag, before feature gating. */
    @JsonProperty("sandboxFlag") Boolean sandboxFlag,
    /** Saved user preference before the managed floor applies. */
    @JsonProperty("userSandboxEnabled") Boolean userSandboxEnabled,
    /** Whether managed policy requires the sandbox. */
    @JsonProperty("managedRequiresSandbox") Boolean managedRequiresSandbox,
    /** Whether unresolved policy enabled the fail-closed sandbox floor. */
    @JsonProperty("enabledByUndeterminedPolicy") Boolean enabledByUndeterminedPolicy,
    /** Whether the managed policy remains unresolved. */
    @JsonProperty("policyUndetermined") Boolean policyUndetermined,
    /** Whether managed policy allows an individual command bypass. */
    @JsonProperty("allowBypass") Boolean allowBypass
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxGetPolicyPresentationParams {}

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
        private Object config;
        private boolean $presence0;
        private Boolean sandboxFlag;
        private Boolean userSandboxEnabled;
        private Boolean managedRequiresSandbox;
        private Boolean enabledByUndeterminedPolicy;
        private Boolean policyUndetermined;
        private Boolean allowBypass;

        /**
         * Sets the {@code config} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder config(Object value) {
            this.config = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code sandboxFlag} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sandboxFlag(Boolean value) {
            this.sandboxFlag = value;
            return this;
        }

        /**
         * Sets the {@code userSandboxEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder userSandboxEnabled(Boolean value) {
            this.userSandboxEnabled = value;
            return this;
        }

        /**
         * Sets the {@code managedRequiresSandbox} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder managedRequiresSandbox(Boolean value) {
            this.managedRequiresSandbox = value;
            return this;
        }

        /**
         * Sets the {@code enabledByUndeterminedPolicy} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabledByUndeterminedPolicy(Boolean value) {
            this.enabledByUndeterminedPolicy = value;
            return this;
        }

        /**
         * Sets the {@code policyUndetermined} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder policyUndetermined(Boolean value) {
            this.policyUndetermined = value;
            return this;
        }

        /**
         * Sets the {@code allowBypass} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowBypass(Boolean value) {
            this.allowBypass = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxGetPolicyPresentationParams build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: config");
            return new SandboxGetPolicyPresentationParams(config, sandboxFlag, userSandboxEnabled, managedRequiresSandbox, enabledByUndeterminedPolicy, policyUndetermined, allowBypass);
        }
    }
}
