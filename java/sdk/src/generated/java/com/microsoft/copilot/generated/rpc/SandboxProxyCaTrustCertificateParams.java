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
 * A certificate file and the credential hosts permitted by the caller for OS trust.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SandboxProxyCaTrustCertificateParams(
    /** Public certificate file to validate and trust. Omit it to trust the certificate stored in the selected Copilot home. */
    @JsonProperty("certificateFile") String certificateFile,
    /** Additional permitted credential hosts. The runtime always includes the selected profile's saved credential hosts and validates the certificate constraints before it changes OS trust. */
    @JsonProperty("allowHosts") List<String> allowHosts,
    /** Copilot configuration directory whose saved credential hosts authorize trust. Omit it to use the default Copilot home. */
    @JsonProperty("configDir") String configDir
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SandboxProxyCaTrustCertificateParams {}

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
        private String certificateFile;
        private List<String> allowHosts;
        private String configDir;

        /**
         * Sets the {@code certificateFile} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder certificateFile(String value) {
            this.certificateFile = value;
            return this;
        }

        /**
         * Sets the {@code allowHosts} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder allowHosts(List<String> value) {
            this.allowHosts = value;
            return this;
        }

        /**
         * Sets the {@code configDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder configDir(String value) {
            this.configDir = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxProxyCaTrustCertificateParams build() {
            if (allowHosts == null) throw new IllegalStateException("Missing required property: allowHosts");
            return new SandboxProxyCaTrustCertificateParams(certificateFile, allowHosts, configDir);
        }
    }
}
