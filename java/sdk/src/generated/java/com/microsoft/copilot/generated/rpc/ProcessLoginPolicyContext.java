/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * The host facts used for the post-login organization policy evaluation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record ProcessLoginPolicyContext(
    /** The current host environment, including policy provenance restrictions. */
    @JsonProperty("environment") Map<String, String> environment,
    /** The original host platform, such as win32, darwin, or linux. */
    @JsonProperty("platform") String platform,
    /** The original user's home directory. */
    @JsonProperty("homeDirectory") String homeDirectory,
    /** The original host's Node.js version for the existing policy user agent. */
    @JsonProperty("processVersion") String processVersion
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    ProcessLoginPolicyContext {}

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
        private Map<String, String> environment;
        private String platform;
        private String homeDirectory;
        private String processVersion;

        /**
         * Sets the {@code environment} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder environment(Map<String, String> value) {
            this.environment = value;
            return this;
        }

        /**
         * Sets the {@code platform} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
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
        public Builder homeDirectory(String value) {
            this.homeDirectory = value;
            return this;
        }

        /**
         * Sets the {@code processVersion} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder processVersion(String value) {
            this.processVersion = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public ProcessLoginPolicyContext build() {
            if (environment == null) throw new IllegalStateException("Missing required property: environment");
            if (platform == null) throw new IllegalStateException("Missing required property: platform");
            if (homeDirectory == null) throw new IllegalStateException("Missing required property: homeDirectory");
            if (processVersion == null) throw new IllegalStateException("Missing required property: processVersion");
            return new ProcessLoginPolicyContext(environment, platform, homeDirectory, processVersion);
        }
    }
}
