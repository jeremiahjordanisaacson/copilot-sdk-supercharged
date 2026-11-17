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
 * Instruction sources and discovery.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenInstructionOptions(
    /** Explicit discovery-only instruction roots using existing SDK instruction formats, not filesystem grants. Available without enableConfigDiscovery, subject to instruction/managed disables. Relative SDK paths resolve once when supplied against the request's effective workingDirectory; retained roots do not move on cwd changes. Null and omission are equivalent; [] clears only the explicit SDK contribution. */
    @JsonProperty("directories") List<String> directories,
    /** Whether to skip custom instruction sources. */
    @JsonProperty("skipCustomInstructions") Boolean skipCustomInstructions,
    /** Whether to invalidate cached custom-instruction discovery before constructing the session. Use when instruction files may have changed earlier in the same runtime process. */
    @JsonProperty("refreshCustomInstructions") Boolean refreshCustomInstructions,
    /** Instruction source IDs disabled for this session. */
    @JsonProperty("disabledSources") List<String> disabledSources,
    /** Whether on-demand custom instruction discovery is enabled. */
    @JsonProperty("onDemandDiscovery") Boolean onDemandDiscovery
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenInstructionOptions {}

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
        private List<String> directories;
        private Boolean skipCustomInstructions;
        private Boolean refreshCustomInstructions;
        private List<String> disabledSources;
        private Boolean onDemandDiscovery;

        /**
         * Sets the {@code directories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder directories(List<String> value) {
            this.directories = value;
            return this;
        }

        /**
         * Sets the {@code skipCustomInstructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skipCustomInstructions(Boolean value) {
            this.skipCustomInstructions = value;
            return this;
        }

        /**
         * Sets the {@code refreshCustomInstructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder refreshCustomInstructions(Boolean value) {
            this.refreshCustomInstructions = value;
            return this;
        }

        /**
         * Sets the {@code disabledSources} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disabledSources(List<String> value) {
            this.disabledSources = value;
            return this;
        }

        /**
         * Sets the {@code onDemandDiscovery} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder onDemandDiscovery(Boolean value) {
            this.onDemandDiscovery = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenInstructionOptions build() {
            return new SessionOpenInstructionOptions(directories, skipCustomInstructions, refreshCustomInstructions, disabledSources, onDemandDiscovery);
        }
    }
}
