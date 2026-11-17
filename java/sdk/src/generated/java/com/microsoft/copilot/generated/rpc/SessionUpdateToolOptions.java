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
public record SessionUpdateToolOptions(
    /** Enable or disable the native image tool for this session without changing the conversation model.
Omitted values preserve the current setting. Re-supply consent on cold resume. */
    @JsonProperty("imageGeneration") ProtocolImageGenerationOptions imageGeneration,
    /** Allowlist of tool names available to this session. */
    @JsonProperty("availableTools") List<String> availableTools,
    /** Denylist of tool names for this session. */
    @JsonProperty("excludedTools") List<String> excludedTools,
    /** Controls how availableTools (allowlist) and excludedTools (denylist) combine when both are set. Omitted updates preserve the current setting; new sessions default to excluded. */
    @JsonProperty("filterPrecedence") OptionsUpdateToolFilterPrecedence filterPrecedence,
    /** Per-session settings for built-in shell tools. */
    @JsonProperty("shell") ShellOptions shell,
    /** PowerShell process flags applied to built-in and user-requested shell commands. */
    @JsonProperty("shellProcessFlags") List<String> shellProcessFlags,
    /** Whether interactive shell sessions are logged. */
    @JsonProperty("logInteractiveShells") Boolean logInteractiveShells,
    /** Maximum decoded byte size of a single model-facing binary tool result (e.g. an image) persisted inline in session events and re-presented to the model on later turns / resume. Larger results are persisted as a metadata-only marker and shown to the model as a short text note. Defaults to 10 MB. */
    @JsonProperty("maxInlineBinaryBytes") Long maxInlineBinaryBytes,
    /** Whether to expose the `manage_schedule` tool to the agent. The runtime always owns the per-session schedule registry; this flag only controls tool exposure (typically gated to staff users). */
    @JsonProperty("manageScheduleEnabled") Boolean manageScheduleEnabled
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateToolOptions {}

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
        private ProtocolImageGenerationOptions imageGeneration;
        private List<String> availableTools;
        private List<String> excludedTools;
        private OptionsUpdateToolFilterPrecedence filterPrecedence;
        private ShellOptions shell;
        private List<String> shellProcessFlags;
        private Boolean logInteractiveShells;
        private Long maxInlineBinaryBytes;
        private Boolean manageScheduleEnabled;

        /**
         * Sets the {@code imageGeneration} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder imageGeneration(ProtocolImageGenerationOptions value) {
            this.imageGeneration = value;
            return this;
        }

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
         * Sets the {@code filterPrecedence} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder filterPrecedence(OptionsUpdateToolFilterPrecedence value) {
            this.filterPrecedence = value;
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
         * Sets the {@code manageScheduleEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder manageScheduleEnabled(Boolean value) {
            this.manageScheduleEnabled = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateToolOptions build() {
            return new SessionUpdateToolOptions(imageGeneration, availableTools, excludedTools, filterPrecedence, shell, shellProcessFlags, logInteractiveShells, maxInlineBinaryBytes, manageScheduleEnabled);
        }
    }
}
