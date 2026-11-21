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
 * Resolve managed Browser activation through the connection's original credential owner.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record PluginsBuiltinResolveActivationParams(
    /** Embedding client identity for the policy helper. */
    @JsonProperty("clientName") String clientName,
    /** Working directory for the policy helper. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Effective user and trusted repository settings. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("userSettings") Object userSettings,
    /** Existing launch populations for the policy-helper gate. */
    @JsonProperty("audience") McpInvocationAudience audience
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginsBuiltinResolveActivationParams {}

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
        private String clientName;
        private String workingDirectory;
        private Object userSettings;
        private boolean $presence2;
        private McpInvocationAudience audience;

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
         * Sets the {@code userSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder userSettings(Object value) {
            this.userSettings = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code audience} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder audience(McpInvocationAudience value) {
            this.audience = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginsBuiltinResolveActivationParams build() {
            if (clientName == null) throw new IllegalStateException("Missing required property: clientName");
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (!$presence2) throw new IllegalStateException("Missing required property: userSettings");
            if (audience == null) throw new IllegalStateException("Missing required property: audience");
            return new PluginsBuiltinResolveActivationParams(clientName, workingDirectory, userSettings, audience);
        }
    }
}
