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
 * Slash-command metadata with name, aliases, description, kind, input hint, execution allowance, and schedulability.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SlashCommandInfo(
    /** Canonical command name without a leading slash */
    @JsonProperty("name") String name,
    /** Display name, including the leading slash. */
    @JsonProperty("displayName") String displayName,
    /** Command source, such as runtime, skill, or protocol-client. */
    @JsonProperty("source") String source,
    /** Whether invocation stops processing the queued commands. */
    @JsonProperty("stopsQueueProcessing") Boolean stopsQueueProcessing,
    /** Whether the command can be the first message in a session. */
    @JsonProperty("availableAtSessionStart") Boolean availableAtSessionStart,
    /** Nested terminal argument grammar, serialized as ArgsSpec JSON. */
    @JsonProperty("cliArgsJson") String cliArgsJson,
    /** Skill that provides this command. */
    @JsonProperty("skillName") String skillName,
    /** Plugin that provides this command. */
    @JsonProperty("pluginName") String pluginName,
    /** Canonical aliases without leading slashes */
    @JsonProperty("aliases") List<String> aliases,
    /** Human-readable command description */
    @JsonProperty("description") String description,
    /** Coarse command category for grouping and behavior: runtime built-in, skill-backed command, or SDK/client-owned command */
    @JsonProperty("kind") SlashCommandKind kind,
    /** Optional unstructured input hint */
    @JsonProperty("input") SlashCommandInput input,
    /** Whether the command may run while an agent turn is active */
    @JsonProperty("allowDuringAgentExecution") Boolean allowDuringAgentExecution,
    /** Whether the command is experimental */
    @JsonProperty("experimental") Boolean experimental,
    /** Whether the command may be the target of `/every` / `/after` schedules. Resolution happens at every tick, so only set this when the command is safe to re-invoke and produces an agent prompt. */
    @JsonProperty("schedulable") Boolean schedulable
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SlashCommandInfo {}

    /**
     * Creates a record without optional runtime metadata.
     * @param name the {@code name} property
     * @param aliases the {@code aliases} property
     * @param description the {@code description} property
     * @param kind the {@code kind} property
     * @param input the {@code input} property
     * @param allowDuringAgentExecution the {@code allowDuringAgentExecution} property
     * @param experimental the {@code experimental} property
     * @param schedulable the {@code schedulable} property
     */
    public SlashCommandInfo(String name, List<String> aliases, String description, SlashCommandKind kind, SlashCommandInput input, Boolean allowDuringAgentExecution, Boolean experimental, Boolean schedulable) {
        this(name, null, null, null, null, null, null, null, aliases, description, kind, input, allowDuringAgentExecution, experimental, schedulable);
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
        private String name;
        private String displayName;
        private String source;
        private Boolean stopsQueueProcessing;
        private Boolean availableAtSessionStart;
        private String cliArgsJson;
        private String skillName;
        private String pluginName;
        private List<String> aliases;
        private String description;
        private SlashCommandKind kind;
        private SlashCommandInput input;
        private Boolean allowDuringAgentExecution;
        private Boolean experimental;
        private Boolean schedulable;

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
         * Sets the {@code displayName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder displayName(String value) {
            this.displayName = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(String value) {
            this.source = value;
            return this;
        }

        /**
         * Sets the {@code stopsQueueProcessing} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder stopsQueueProcessing(Boolean value) {
            this.stopsQueueProcessing = value;
            return this;
        }

        /**
         * Sets the {@code availableAtSessionStart} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder availableAtSessionStart(Boolean value) {
            this.availableAtSessionStart = value;
            return this;
        }

        /**
         * Sets the {@code cliArgsJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder cliArgsJson(String value) {
            this.cliArgsJson = value;
            return this;
        }

        /**
         * Sets the {@code skillName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skillName(String value) {
            this.skillName = value;
            return this;
        }

        /**
         * Sets the {@code pluginName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder pluginName(String value) {
            this.pluginName = value;
            return this;
        }

        /**
         * Sets the {@code aliases} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder aliases(List<String> value) {
            this.aliases = value;
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
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder kind(SlashCommandKind value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code input} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder input(SlashCommandInput value) {
            this.input = value;
            return this;
        }

        /**
         * Sets the {@code allowDuringAgentExecution} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowDuringAgentExecution(Boolean value) {
            this.allowDuringAgentExecution = value;
            return this;
        }

        /**
         * Sets the {@code experimental} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder experimental(Boolean value) {
            this.experimental = value;
            return this;
        }

        /**
         * Sets the {@code schedulable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder schedulable(Boolean value) {
            this.schedulable = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SlashCommandInfo build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            if (allowDuringAgentExecution == null) throw new IllegalStateException("Missing required property: allowDuringAgentExecution");
            return new SlashCommandInfo(name, displayName, source, stopsQueueProcessing, availableAtSessionStart, cliArgsJson, skillName, pluginName, aliases, description, kind, input, allowDuringAgentExecution, experimental, schedulable);
        }
    }
}
