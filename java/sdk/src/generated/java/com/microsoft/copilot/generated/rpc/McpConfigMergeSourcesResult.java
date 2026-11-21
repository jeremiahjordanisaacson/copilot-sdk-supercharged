/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * The composed MCP configuration and all omitted-source diagnostics.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpConfigMergeSourcesResult(
    /** The complete merged configuration, including source provenance. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("config") Object config,
    /** Workspace files or entries that did not load. */
    @JsonProperty("workspaceErrors") List<McpConfigSourceDiagnostic> workspaceErrors,
    /** Invalid user entries omitted while valid siblings remained. */
    @JsonProperty("userConfigErrors") List<McpConfigSourceDiagnostic> userConfigErrors,
    /** Server names discovered through the on-device registry. */
    @JsonProperty("odrServerNames") List<String> odrServerNames,
    /** Registry servers disabled because their MSIX container is unavailable. */
    @JsonProperty("odrMsixDisabledServerNames") List<String> odrMsixDisabledServerNames
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpConfigMergeSourcesResult {}

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
        private List<McpConfigSourceDiagnostic> workspaceErrors;
        private List<McpConfigSourceDiagnostic> userConfigErrors;
        private List<String> odrServerNames;
        private List<String> odrMsixDisabledServerNames;

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
         * Sets the {@code workspaceErrors} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workspaceErrors(List<McpConfigSourceDiagnostic> value) {
            this.workspaceErrors = value;
            return this;
        }

        /**
         * Sets the {@code userConfigErrors} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder userConfigErrors(List<McpConfigSourceDiagnostic> value) {
            this.userConfigErrors = value;
            return this;
        }

        /**
         * Sets the {@code odrServerNames} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder odrServerNames(List<String> value) {
            this.odrServerNames = value;
            return this;
        }

        /**
         * Sets the {@code odrMsixDisabledServerNames} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder odrMsixDisabledServerNames(List<String> value) {
            this.odrMsixDisabledServerNames = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpConfigMergeSourcesResult build() {
            if (!$presence0) throw new IllegalStateException("Missing required property: config");
            if (workspaceErrors == null) throw new IllegalStateException("Missing required property: workspaceErrors");
            if (userConfigErrors == null) throw new IllegalStateException("Missing required property: userConfigErrors");
            if (odrServerNames == null) throw new IllegalStateException("Missing required property: odrServerNames");
            if (odrMsixDisabledServerNames == null) throw new IllegalStateException("Missing required property: odrMsixDisabledServerNames");
            return new McpConfigMergeSourcesResult(config, workspaceErrors, userConfigErrors, odrServerNames, odrMsixDisabledServerNames);
        }
    }
}
