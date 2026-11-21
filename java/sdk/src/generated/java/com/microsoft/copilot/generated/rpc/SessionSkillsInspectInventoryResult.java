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
 * Full inventory under the selected session's discovery and activation policy.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionSkillsInspectInventoryResult(
    /** Full local and provider rows visible to this session. */
    @JsonProperty("skills") List<SessionSkillInventoryEntry> skills,
    /** Effective disabled identities, including host and MCP aliases. */
    @JsonProperty("disabledSkills") List<String> disabledSkills,
    /** Nonfatal loader warnings. */
    @JsonProperty("warnings") List<String> warnings,
    /** Source failures that make this inventory incomplete. */
    @JsonProperty("errors") List<String> errors
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionSkillsInspectInventoryResult {}

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
        private List<SessionSkillInventoryEntry> skills;
        private List<String> disabledSkills;
        private List<String> warnings;
        private List<String> errors;

        /**
         * Sets the {@code skills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder skills(List<SessionSkillInventoryEntry> value) {
            this.skills = value;
            return this;
        }

        /**
         * Sets the {@code disabledSkills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder disabledSkills(List<String> value) {
            this.disabledSkills = value;
            return this;
        }

        /**
         * Sets the {@code warnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder warnings(List<String> value) {
            this.warnings = value;
            return this;
        }

        /**
         * Sets the {@code errors} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder errors(List<String> value) {
            this.errors = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionSkillsInspectInventoryResult build() {
            if (skills == null) throw new IllegalStateException("Missing required property: skills");
            if (disabledSkills == null) throw new IllegalStateException("Missing required property: disabledSkills");
            if (warnings == null) throw new IllegalStateException("Missing required property: warnings");
            if (errors == null) throw new IllegalStateException("Missing required property: errors");
            return new SessionSkillsInspectInventoryResult(skills, disabledSkills, warnings, errors);
        }
    }
}
