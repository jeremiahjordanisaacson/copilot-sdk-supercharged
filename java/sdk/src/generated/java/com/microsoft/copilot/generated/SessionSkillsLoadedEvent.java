/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Session event "session.skills_loaded". Payload of `session.skills_loaded` listing resolved skill metadata.
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class SessionSkillsLoadedEvent extends SessionEvent {

    @Override
    public String getType() { return "session.skills_loaded"; }

    @JsonProperty("data")
    private SessionSkillsLoadedEventData data;

    public SessionSkillsLoadedEventData getData() { return data; }
    public void setData(SessionSkillsLoadedEventData data) { this.data = data; }

    /** Data payload for {@link SessionSkillsLoadedEvent}. */
    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record SessionSkillsLoadedEventData(
        /** Array of resolved skill metadata */
        @JsonProperty("skills") List<SkillsLoadedSkill> skills,
        /** Warnings from the published skill catalog. Older hosts can omit this snapshot. */
        @JsonProperty("warnings") List<String> warnings,
        /** Errors from the published skill catalog. Older hosts can omit this snapshot. */
        @JsonProperty("errors") List<String> errors
    ) {

        /**
         * Creates a record positionally.
         * Use {@link #builder()} for named construction.
         */
        public SessionSkillsLoadedEventData {}

        /**
         * Creates a record without optional runtime metadata.
         * @param skills the {@code skills} property
         */
        public SessionSkillsLoadedEventData(List<SkillsLoadedSkill> skills) {
            this(skills, null, null);
        }

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
            private List<SkillsLoadedSkill> skills;
            private List<String> warnings;
            private List<String> errors;

            /**
             * Sets the {@code skills} property.
             * @param value the property value
             * @return this builder
             * @since 2.0.0
             */
            public Builder skills(List<SkillsLoadedSkill> value) {
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
            public SessionSkillsLoadedEventData build() {
                if (skills == null) throw new IllegalStateException("Missing required property: skills");
                return new SessionSkillsLoadedEventData(skills, warnings, errors);
            }
        }
    }
}
