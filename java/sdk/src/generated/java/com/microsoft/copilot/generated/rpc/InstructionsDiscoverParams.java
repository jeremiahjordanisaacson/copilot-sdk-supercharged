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
 * Optional project paths to include in instruction discovery.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record InstructionsDiscoverParams(
    /** Explicit discovery-only instruction roots using existing SDK instruction formats, not filesystem grants. Available without ambient discovery, subject to instruction/managed disables. Relative paths resolve against the request's effective working directory when supplied. Null and omission are equivalent; an empty array contributes no explicit roots. */
    @JsonProperty("instructionDirectories") List<String> instructionDirectories,
    /** Optional list of project directory paths to scan for repository/working-directory instruction sources. When omitted or empty, only user-level and plugin instruction sources are returned (no project scan). */
    @JsonProperty("projectPaths") List<String> projectPaths,
    /** When true, omit the host's instruction sources (user/home-level files and plugin rules), leaving only repository and working-directory sources. For multitenant deployments. */
    @JsonProperty("excludeHostInstructions") Boolean excludeHostInstructions
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public InstructionsDiscoverParams {}

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
        private List<String> instructionDirectories;
        private List<String> projectPaths;
        private Boolean excludeHostInstructions;

        /**
         * Sets the {@code instructionDirectories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder instructionDirectories(List<String> value) {
            this.instructionDirectories = value;
            return this;
        }

        /**
         * Sets the {@code projectPaths} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder projectPaths(List<String> value) {
            this.projectPaths = value;
            return this;
        }

        /**
         * Sets the {@code excludeHostInstructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludeHostInstructions(Boolean value) {
            this.excludeHostInstructions = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public InstructionsDiscoverParams build() {
            return new InstructionsDiscoverParams(instructionDirectories, projectPaths, excludeHostInstructions);
        }
    }
}
