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
 * Full local skill inventory and report-and-ignore source diagnostics.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SkillResourceDiscoverResult(
    /** Local skill records with invocation identity, content paths, and command metadata. */
    @JsonProperty("skills") List<Object> skills,
    /** Nonfatal loader and manifest warnings. */
    @JsonProperty("warnings") List<String> warnings,
    /** Explicit plugin scan warnings, separate from skill loader diagnostics. */
    @JsonProperty("sourceWarnings") List<String> sourceWarnings,
    /** Sources that failed to load; the remaining inventory is incomplete. */
    @JsonProperty("errors") List<String> errors
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SkillResourceDiscoverResult {}

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
        private List<Object> skills;
        private List<String> warnings;
        private List<String> sourceWarnings;
        private List<String> errors;

        /**
         * Sets the {@code skills} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skills(List<Object> value) {
            this.skills = value;
            return this;
        }

        /**
         * Sets the {@code warnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder warnings(List<String> value) {
            this.warnings = value;
            return this;
        }

        /**
         * Sets the {@code sourceWarnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sourceWarnings(List<String> value) {
            this.sourceWarnings = value;
            return this;
        }

        /**
         * Sets the {@code errors} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
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
        public SkillResourceDiscoverResult build() {
            if (skills == null) throw new IllegalStateException("Missing required property: skills");
            if (warnings == null) throw new IllegalStateException("Missing required property: warnings");
            if (sourceWarnings == null) throw new IllegalStateException("Missing required property: sourceWarnings");
            if (errors == null) throw new IllegalStateException("Missing required property: errors");
            return new SkillResourceDiscoverResult(skills, warnings, sourceWarnings, errors);
        }
    }
}
