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
 * Runtime-owned extension discovery and launch controls for a local host.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionsConfigureSessionExtensionsParams(
    /** Session to attach the extension controller delegate to. */
    @JsonProperty("sessionId") String sessionId,
    /** In-process ExtensionController delegate (CLI-only optimization). Marked internal: this field is excluded from the public SDK surface. The post-SDK extension surface exposes list/enable/disable/reload via dedicated RPCs served by the runtime. */
    @JsonProperty("controller") Object controller,
    /** Whether discovered extension processes may run. */
    @JsonProperty("launchProcesses") Boolean launchProcesses,
    /** Keep compatible extension processes instead of restarting them. */
    @JsonProperty("retainProcessesIfUnchanged") Boolean retainProcessesIfUnchanged,
    /** Whether extension management operations are enabled. */
    @JsonProperty("managementToolsEnabled") Boolean managementToolsEnabled,
    /** Whether the model receives extension management tools. */
    @JsonProperty("exposeManagementTools") Boolean exposeManagementTools,
    /** Discovery sources allowed by the host's trust decision. */
    @JsonProperty("includeSources") List<String> includeSources
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionsConfigureSessionExtensionsParams {}

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
        private String sessionId;
        private Object controller;
        private Boolean launchProcesses;
        private Boolean retainProcessesIfUnchanged;
        private Boolean managementToolsEnabled;
        private Boolean exposeManagementTools;
        private List<String> includeSources;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code controller} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder controller(Object value) {
            this.controller = value;
            return this;
        }

        /**
         * Sets the {@code launchProcesses} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder launchProcesses(Boolean value) {
            this.launchProcesses = value;
            return this;
        }

        /**
         * Sets the {@code retainProcessesIfUnchanged} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder retainProcessesIfUnchanged(Boolean value) {
            this.retainProcessesIfUnchanged = value;
            return this;
        }

        /**
         * Sets the {@code managementToolsEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder managementToolsEnabled(Boolean value) {
            this.managementToolsEnabled = value;
            return this;
        }

        /**
         * Sets the {@code exposeManagementTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder exposeManagementTools(Boolean value) {
            this.exposeManagementTools = value;
            return this;
        }

        /**
         * Sets the {@code includeSources} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeSources(List<String> value) {
            this.includeSources = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsConfigureSessionExtensionsParams build() {
            if (sessionId == null) throw new IllegalStateException("Missing required property: sessionId");
            return new SessionsConfigureSessionExtensionsParams(sessionId, controller, launchProcesses, retainProcessesIfUnchanged, managementToolsEnabled, exposeManagementTools, includeSources);
        }
    }
}
