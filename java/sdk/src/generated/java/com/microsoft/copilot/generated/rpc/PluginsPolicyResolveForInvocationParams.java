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
 * Resolves plugin policy through the requesting connection's original credential owner.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record PluginsPolicyResolveForInvocationParams(
    /** Working directory used by a directory-sensitive policy helper. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Whether a configured policy helper may run. */
    @JsonProperty("policyHelperEnabled") Boolean policyHelperEnabled,
    /** Reads account and server policy from local caches without network access. */
    @JsonProperty("cacheOnly") Boolean cacheOnly,
    /** Retains authoritative source layers for subsequent guarded mutations. */
    @JsonProperty("retainPolicy") Boolean retainPolicy,
    /** Stops on ingestion exceptions instead of reloading the device layer. */
    @JsonProperty("strict") Boolean strict,
    /** Host client identity supplied to the policy helper. */
    @JsonProperty("clientName") String clientName,
    /** Host user agent supplied to policy retrieval. */
    @JsonProperty("userAgent") String userAgent,
    /** Host platform in Node platform spelling. */
    @JsonProperty("platform") String platform,
    /** Host home directory for policy and cache locations. */
    @JsonProperty("homeDirectory") String homeDirectory,
    /** Optional active account identity; credentials still resolve through the original owner. */
    @JsonProperty("authInfo") AuthIdentity authInfo
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginsPolicyResolveForInvocationParams {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String workingDirectory;
        private Boolean policyHelperEnabled;
        private Boolean cacheOnly;
        private Boolean retainPolicy;
        private Boolean strict;
        private String clientName;
        private String userAgent;
        private String platform;
        private String homeDirectory;
        private AuthIdentity authInfo;

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code policyHelperEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder policyHelperEnabled(Boolean value) {
            this.policyHelperEnabled = value;
            return this;
        }

        /**
         * Sets the {@code cacheOnly} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder cacheOnly(Boolean value) {
            this.cacheOnly = value;
            return this;
        }

        /**
         * Sets the {@code retainPolicy} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder retainPolicy(Boolean value) {
            this.retainPolicy = value;
            return this;
        }

        /**
         * Sets the {@code strict} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder strict(Boolean value) {
            this.strict = value;
            return this;
        }

        /**
         * Sets the {@code clientName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder clientName(String value) {
            this.clientName = value;
            return this;
        }

        /**
         * Sets the {@code userAgent} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder userAgent(String value) {
            this.userAgent = value;
            return this;
        }

        /**
         * Sets the {@code platform} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder platform(String value) {
            this.platform = value;
            return this;
        }

        /**
         * Sets the {@code homeDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder homeDirectory(String value) {
            this.homeDirectory = value;
            return this;
        }

        /**
         * Sets the {@code authInfo} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authInfo(AuthIdentity value) {
            this.authInfo = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginsPolicyResolveForInvocationParams build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (policyHelperEnabled == null) throw new IllegalStateException("Missing required property: policyHelperEnabled");
            if (cacheOnly == null) throw new IllegalStateException("Missing required property: cacheOnly");
            if (strict == null) throw new IllegalStateException("Missing required property: strict");
            if (clientName == null) throw new IllegalStateException("Missing required property: clientName");
            if (userAgent == null) throw new IllegalStateException("Missing required property: userAgent");
            if (platform == null) throw new IllegalStateException("Missing required property: platform");
            if (homeDirectory == null) throw new IllegalStateException("Missing required property: homeDirectory");
            return new PluginsPolicyResolveForInvocationParams(workingDirectory, policyHelperEnabled, cacheOnly, retainPolicy, strict, clientName, userAgent, platform, homeDirectory, authInfo);
        }
    }
}
