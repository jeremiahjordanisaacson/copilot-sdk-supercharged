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
 * An agent file to create without replacing an existing file.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record AgentsCreateFileParams(
    /** The display name used to derive the filename. */
    @JsonProperty("name") String name,
    /** The agent description. */
    @JsonProperty("description") String description,
    /** The agent instructions. */
    @JsonProperty("instructions") String instructions,
    /** Enabled groups. Omit for all tools; an empty list enables no tools. */
    @JsonProperty("enabledCategories") List<AgentToolCategoryId> enabledCategories,
    /** The directory owner. */
    @JsonProperty("location") AgentFileLocation location,
    /** The project root. */
    @JsonProperty("projectPath") String projectPath,
    /** The user configuration root. */
    @JsonProperty("userConfigDir") String userConfigDir
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public AgentsCreateFileParams {}

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
        private String name;
        private String description;
        private String instructions;
        private List<AgentToolCategoryId> enabledCategories;
        private AgentFileLocation location;
        private String projectPath;
        private String userConfigDir;

        /**
         * Sets the {@code name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder name(String value) {
            this.name = value;
            return this;
        }

        /**
         * Sets the {@code description} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Sets the {@code instructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder instructions(String value) {
            this.instructions = value;
            return this;
        }

        /**
         * Sets the {@code enabledCategories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabledCategories(List<AgentToolCategoryId> value) {
            this.enabledCategories = value;
            return this;
        }

        /**
         * Sets the {@code location} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder location(AgentFileLocation value) {
            this.location = value;
            return this;
        }

        /**
         * Sets the {@code projectPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder projectPath(String value) {
            this.projectPath = value;
            return this;
        }

        /**
         * Sets the {@code userConfigDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder userConfigDir(String value) {
            this.userConfigDir = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AgentsCreateFileParams build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (instructions == null) throw new IllegalStateException("Missing required property: instructions");
            if (location == null) throw new IllegalStateException("Missing required property: location");
            if (projectPath == null) throw new IllegalStateException("Missing required property: projectPath");
            if (userConfigDir == null) throw new IllegalStateException("Missing required property: userConfigDir");
            return new AgentsCreateFileParams(name, description, instructions, enabledCategories, location, projectPath, userConfigDir);
        }
    }
}
