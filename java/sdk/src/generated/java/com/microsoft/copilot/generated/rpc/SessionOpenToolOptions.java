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
 * Tool declarations, exposure, and behavior.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenToolOptions(
    /** Allowlist of available tool names. */
    @JsonProperty("availableTools") List<String> availableTools,
    /** Denylist of tool names. */
    @JsonProperty("excludedTools") List<String> excludedTools,
    /** Per-session settings for built-in shell tools. */
    @JsonProperty("shell") ShellOptions shell,
    /** PowerShell process flags applied to built-in and user-requested shell commands. */
    @JsonProperty("shellProcessFlags") List<String> shellProcessFlags,
    /** Whether interactive shell sessions are logged. */
    @JsonProperty("logInteractiveShells") Boolean logInteractiveShells,
    /** Maximum decoded byte size of a single inline model-facing binary tool result persisted in session events (default 10 MB). */
    @JsonProperty("maxInlineBinaryBytes") Long maxInlineBinaryBytes
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenToolOptions {}

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
        private List<String> availableTools;
        private List<String> excludedTools;
        private ShellOptions shell;
        private List<String> shellProcessFlags;
        private Boolean logInteractiveShells;
        private Long maxInlineBinaryBytes;

        /**
         * Sets the {@code availableTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder availableTools(List<String> value) {
            this.availableTools = value;
            return this;
        }

        /**
         * Sets the {@code excludedTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludedTools(List<String> value) {
            this.excludedTools = value;
            return this;
        }

        /**
         * Sets the {@code shell} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder shell(ShellOptions value) {
            this.shell = value;
            return this;
        }

        /**
         * Sets the {@code shellProcessFlags} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder shellProcessFlags(List<String> value) {
            this.shellProcessFlags = value;
            return this;
        }

        /**
         * Sets the {@code logInteractiveShells} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder logInteractiveShells(Boolean value) {
            this.logInteractiveShells = value;
            return this;
        }

        /**
         * Sets the {@code maxInlineBinaryBytes} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder maxInlineBinaryBytes(Long value) {
            this.maxInlineBinaryBytes = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenToolOptions build() {
            return new SessionOpenToolOptions(availableTools, excludedTools, shell, shellProcessFlags, logInteractiveShells, maxInlineBinaryBytes);
        }
    }
}
