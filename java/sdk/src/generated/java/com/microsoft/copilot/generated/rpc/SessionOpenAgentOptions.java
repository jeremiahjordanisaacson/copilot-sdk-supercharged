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
 * Agent definitions, selection, and source filters.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenAgentOptions(
    /** Built-in subagent names to include in this session. When specified, only these built-ins are available, subject to runtime availability and exclusions. Custom agents with the same name remain available. */
    @JsonProperty("includedBuiltinAgents") List<String> includedBuiltinAgents,
    /** Built-in subagent names to exclude from this session. Excluded built-ins are hidden from agent discovery and cannot be dispatched unless a custom agent with the same name is available. */
    @JsonProperty("excludedBuiltinAgents") List<String> excludedBuiltinAgents,
    /** Explicit discovery-only agent roots, separate from customAgents and filesystem grants. Available without enableConfigDiscovery, subject to managed/source disables. Relative SDK paths resolve once when supplied against the request's effective workingDirectory; retained roots do not move on cwd changes. Null and omission are equivalent; [] clears only the explicit SDK contribution. */
    @JsonProperty("directories") List<String> directories,
    /** Whether custom agents default to local-only execution. */
    @JsonProperty("localDiscoveryOnly") Boolean localDiscoveryOnly,
    /** Runtime context discriminator for agent filtering. */
    @JsonProperty("context") String context
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenAgentOptions {}

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
        private List<String> includedBuiltinAgents;
        private List<String> excludedBuiltinAgents;
        private List<String> directories;
        private Boolean localDiscoveryOnly;
        private String context;

        /**
         * Sets the {@code includedBuiltinAgents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includedBuiltinAgents(List<String> value) {
            this.includedBuiltinAgents = value;
            return this;
        }

        /**
         * Sets the {@code excludedBuiltinAgents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludedBuiltinAgents(List<String> value) {
            this.excludedBuiltinAgents = value;
            return this;
        }

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
         * Sets the {@code localDiscoveryOnly} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder localDiscoveryOnly(Boolean value) {
            this.localDiscoveryOnly = value;
            return this;
        }

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(String value) {
            this.context = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenAgentOptions build() {
            return new SessionOpenAgentOptions(includedBuiltinAgents, excludedBuiltinAgents, directories, localDiscoveryOnly, context);
        }
    }
}
