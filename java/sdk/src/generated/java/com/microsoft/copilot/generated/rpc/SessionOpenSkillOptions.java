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
 * Skill loading, sources, and provider capability.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionOpenSkillOptions(
    /** Explicit discovery-only skill roots, not filesystem grants. Available without enableConfigDiscovery, subject to skill/managed disables. Relative SDK paths resolve once when supplied against the request's effective workingDirectory; retained roots do not move on cwd changes. Null and omission are equivalent; [] clears only the explicit SDK contribution. */
    @JsonProperty("directories") List<String> directories,
    /** Skill scan directories and descendants excluded from discovery. Supports `~`-relative paths. */
    @JsonProperty("ignoredLocations") List<String> ignoredLocations,
    /** Whether skill loading is enabled. When omitted, explicit SDK skill roots or an SDK skill provider enable skills independently of enableConfigDiscovery. */
    @JsonProperty("enabled") Boolean enabled,
    /** Whether the requesting SDK session has a skill provider. The provider remains ephemeral and is never persisted in session options or history. When enableSkills is false, it remains bound but dormant and receives no callbacks. Cloud, relay, handoff, and raw sessions.open flows reject it because they cannot safely pre-register the callback handler. */
    @CopilotExperimental
    @JsonProperty("hasProvider") Boolean hasProvider,
    /** Built-in skill names to include in this session. When specified, only these runtime-bundled skills are available. Skills from other sources with the same name remain available. */
    @JsonProperty("includedBuiltinSkills") List<String> includedBuiltinSkills,
    /** Skill IDs disabled for this session. */
    @JsonProperty("disabledSkills") List<String> disabledSkills
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenSkillOptions {}

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
        private List<String> ignoredLocations;
        private Boolean enabled;
        private Boolean hasProvider;
        private List<String> includedBuiltinSkills;
        private List<String> disabledSkills;

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
         * Sets the {@code ignoredLocations} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ignoredLocations(List<String> value) {
            this.ignoredLocations = value;
            return this;
        }

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Sets the {@code hasProvider} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder hasProvider(Boolean value) {
            this.hasProvider = value;
            return this;
        }

        /**
         * Sets the {@code includedBuiltinSkills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includedBuiltinSkills(List<String> value) {
            this.includedBuiltinSkills = value;
            return this;
        }

        /**
         * Sets the {@code disabledSkills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disabledSkills(List<String> value) {
            this.disabledSkills = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenSkillOptions build() {
            return new SessionOpenSkillOptions(directories, ignoredLocations, enabled, hasProvider, includedBuiltinSkills, disabledSkills);
        }
    }
}
