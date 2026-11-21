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
 * A complete skill row for session-owned inspection and catalog publication.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionSkillInventoryEntry(
    /** Original skill name. */
    @JsonProperty("name") String name,
    /** Human-readable skill purpose. */
    @JsonProperty("description") String description,
    /** Runtime-owned source classification. */
    @JsonProperty("source") SkillSource source,
    /** Whether the user can invoke this skill. */
    @JsonProperty("userInvocable") Boolean userInvocable,
    /** Whether model invocation is disabled. */
    @JsonProperty("disableModelInvocation") Boolean disableModelInvocation,
    /** Whether the session enables this skill. */
    @JsonProperty("enabled") Boolean enabled,
    /** Tool names allowed by the skill definition. */
    @JsonProperty("allowedTools") List<String> allowedTools,
    /** Whether this row comes from a plugin command definition. */
    @JsonProperty("isCommand") Boolean isCommand,
    /** Absolute local definition path, when the skill has one. */
    @JsonProperty("filePath") String filePath,
    /** Local resource directory, when the skill has one. */
    @JsonProperty("baseDir") String baseDir,
    /** Loaded definition body, when available. */
    @JsonProperty("content") String content,
    /** Owning plugin's opaque identity. */
    @JsonProperty("pluginId") String pluginId,
    /** Owning plugin's display name. */
    @JsonProperty("pluginName") String pluginName,
    /** Owning plugin's version. */
    @JsonProperty("pluginVersion") String pluginVersion,
    /** Invocation identity supplied by the source. */
    @JsonProperty("invocationName") String invocationName,
    /** Canonical slash-command identity. */
    @JsonProperty("commandName") String commandName,
    /** Freeform argument hint from the definition. */
    @JsonProperty("argumentHint") String argumentHint,
    /** MCP server and resource identity without private activation metadata. */
    @JsonProperty("mcpOrigin") McpSkillOrigin mcpOrigin,
    /** Provider's original name before qualification. */
    @JsonProperty("originalName") String originalName,
    /** Provider-relative definition path. */
    @JsonProperty("relativePath") String relativePath,
    /** Provider-relative resource directory. */
    @JsonProperty("relativeDir") String relativeDir,
    /** Repository boundary for local resource reads. */
    @JsonProperty("repositoryBoundary") String repositoryBoundary
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionSkillInventoryEntry {}

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
        private String name;
        private String description;
        private SkillSource source;
        private Boolean userInvocable;
        private Boolean disableModelInvocation;
        private Boolean enabled;
        private List<String> allowedTools;
        private Boolean isCommand;
        private String filePath;
        private String baseDir;
        private String content;
        private String pluginId;
        private String pluginName;
        private String pluginVersion;
        private String invocationName;
        private String commandName;
        private String argumentHint;
        private McpSkillOrigin mcpOrigin;
        private String originalName;
        private String relativePath;
        private String relativeDir;
        private String repositoryBoundary;

        /**
         * Sets the {@code name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
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
        @com.microsoft.copilot.CopilotExperimental
        public Builder description(String value) {
            this.description = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder source(SkillSource value) {
            this.source = value;
            return this;
        }

        /**
         * Sets the {@code userInvocable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder userInvocable(Boolean value) {
            this.userInvocable = value;
            return this;
        }

        /**
         * Sets the {@code disableModelInvocation} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder disableModelInvocation(Boolean value) {
            this.disableModelInvocation = value;
            return this;
        }

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Sets the {@code allowedTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder allowedTools(List<String> value) {
            this.allowedTools = value;
            return this;
        }

        /**
         * Sets the {@code isCommand} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isCommand(Boolean value) {
            this.isCommand = value;
            return this;
        }

        /**
         * Sets the {@code filePath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder filePath(String value) {
            this.filePath = value;
            return this;
        }

        /**
         * Sets the {@code baseDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder baseDir(String value) {
            this.baseDir = value;
            return this;
        }

        /**
         * Sets the {@code content} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder content(String value) {
            this.content = value;
            return this;
        }

        /**
         * Sets the {@code pluginId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder pluginId(String value) {
            this.pluginId = value;
            return this;
        }

        /**
         * Sets the {@code pluginName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder pluginName(String value) {
            this.pluginName = value;
            return this;
        }

        /**
         * Sets the {@code pluginVersion} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder pluginVersion(String value) {
            this.pluginVersion = value;
            return this;
        }

        /**
         * Sets the {@code invocationName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder invocationName(String value) {
            this.invocationName = value;
            return this;
        }

        /**
         * Sets the {@code commandName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder commandName(String value) {
            this.commandName = value;
            return this;
        }

        /**
         * Sets the {@code argumentHint} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder argumentHint(String value) {
            this.argumentHint = value;
            return this;
        }

        /**
         * Sets the {@code mcpOrigin} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder mcpOrigin(McpSkillOrigin value) {
            this.mcpOrigin = value;
            return this;
        }

        /**
         * Sets the {@code originalName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder originalName(String value) {
            this.originalName = value;
            return this;
        }

        /**
         * Sets the {@code relativePath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder relativePath(String value) {
            this.relativePath = value;
            return this;
        }

        /**
         * Sets the {@code relativeDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder relativeDir(String value) {
            this.relativeDir = value;
            return this;
        }

        /**
         * Sets the {@code repositoryBoundary} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder repositoryBoundary(String value) {
            this.repositoryBoundary = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionSkillInventoryEntry build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (description == null) throw new IllegalStateException("Missing required property: description");
            if (source == null) throw new IllegalStateException("Missing required property: source");
            if (userInvocable == null) throw new IllegalStateException("Missing required property: userInvocable");
            if (disableModelInvocation == null) throw new IllegalStateException("Missing required property: disableModelInvocation");
            if (enabled == null) throw new IllegalStateException("Missing required property: enabled");
            return new SessionSkillInventoryEntry(name, description, source, userInvocable, disableModelInvocation, enabled, allowedTools, isCommand, filePath, baseDir, content, pluginId, pluginName, pluginVersion, invocationName, commandName, argumentHint, mcpOrigin, originalName, relativePath, relativeDir, repositoryBoundary);
        }
    }
}
