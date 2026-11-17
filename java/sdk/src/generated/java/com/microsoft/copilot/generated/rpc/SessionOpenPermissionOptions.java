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
 * Permission decisions and sandboxing.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenPermissionOptions(
    /** Resolved sandbox configuration. */
    @JsonProperty("sandbox") SandboxConfig sandbox,
    /** Origin of the sandbox choice. Settings-derived origins (never_configured, user_enabled, user_disabled, repository_policy) let managed policy floor a host preference; explicit below-floor changes remain policy conflicts unless a session opt-out is authorized. Also used for telemetry provenance. */
    @CopilotExperimental
    @JsonProperty("sandboxSource") SandboxConfigSource sandboxSource
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenPermissionOptions {}

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
        private SandboxConfig sandbox;
        private SandboxConfigSource sandboxSource;

        /**
         * Sets the {@code sandbox} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sandbox(SandboxConfig value) {
            this.sandbox = value;
            return this;
        }

        /**
         * Sets the {@code sandboxSource} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sandboxSource(SandboxConfigSource value) {
            this.sandboxSource = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenPermissionOptions build() {
            return new SessionOpenPermissionOptions(sandbox, sandboxSource);
        }
    }
}
