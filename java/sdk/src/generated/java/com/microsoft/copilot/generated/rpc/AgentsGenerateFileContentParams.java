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
 * Agent content to render without file access.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record AgentsGenerateFileContentParams(
    /** The display name. */
    @JsonProperty("name") String name,
    /** The agent description. */
    @JsonProperty("description") String description,
    /** Tool names to emit. Omit for all tools; an empty list enables no tools. */
    @JsonProperty("tools") List<String> tools,
    /** The agent instructions. */
    @JsonProperty("instructions") String instructions
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public AgentsGenerateFileContentParams {}

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
        private List<String> tools;
        private String instructions;

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
         * Sets the {@code tools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder tools(List<String> value) {
            this.tools = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AgentsGenerateFileContentParams build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (instructions == null) throw new IllegalStateException("Missing required property: instructions");
            return new AgentsGenerateFileContentParams(name, description, tools, instructions);
        }
    }
}
