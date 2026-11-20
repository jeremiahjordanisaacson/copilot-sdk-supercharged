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
import javax.annotation.processing.Generated;

/**
 * Resolve GitHub MCP exposure and optionally build a secretless server descriptor.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = McpResolveGitHubConfigParams.$NullableDeserializer.class)
record McpResolveGitHubConfigParams(
    /** Invocation options, with precedence over settings. */
    @JsonProperty("flags") GitHubMcpOptions flags,
    /** Persisted user settings in their original shape. */
    @JsonProperty("settings") Object settings,
    /** Session override to validate and merge after invocation options. */
    @JsonProperty("session") Object session,
    /** Enable the FIDES capability. */
    @JsonProperty("fidesIfcEnabled") Boolean fidesIfcEnabled,
    /** The runtime's shell and gh availability decision. */
    @JsonProperty("excludeGhReplaceableTools") Boolean excludeGhReplaceableTools,
    /** Workspace state for automatic ADO restriction. */
    @JsonProperty("workspace") McpGitHubWorkspace workspace,
    /** Build a descriptor using the caller's current account. */
    @JsonProperty("buildConfig") Boolean buildConfig,
    /** Explicit descriptor options, when the host already resolved exposure. */
    @JsonProperty("options") GitHubMcpOptions options,
    /** Pinned identity metadata for a host-owned registration. */
    @JsonProperty("authInfo") Object authInfo,
    /** Existing host auth context for an explicit session credential. */
    @JsonProperty("authContextId") String authContextId,
    /** Opaque selector for an existing host credential owner, admitted on this connection. */
    @JsonProperty("authOwnerId") String authOwnerId,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpResolveGitHubConfigParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private GitHubMcpOptions flags;
        private boolean $presence0;
        private Object settings;
        private boolean $presence1;
        private Object session;
        private boolean $presence2;
        private Boolean fidesIfcEnabled;
        private Boolean excludeGhReplaceableTools;
        private McpGitHubWorkspace workspace;
        private boolean $presence5;
        private Boolean buildConfig;
        private GitHubMcpOptions options;
        private boolean $presence7;
        private Object authInfo;
        private boolean $presence8;
        private String authContextId;
        private String authOwnerId;

        /**
         * Sets the {@code flags} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flags(GitHubMcpOptions value) {
            this.flags = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code settings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder settings(Object value) {
            this.settings = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code session} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder session(Object value) {
            this.session = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code fidesIfcEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fidesIfcEnabled(Boolean value) {
            this.fidesIfcEnabled = value;
            return this;
        }

        /**
         * Sets the {@code excludeGhReplaceableTools} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludeGhReplaceableTools(Boolean value) {
            this.excludeGhReplaceableTools = value;
            return this;
        }

        /**
         * Sets the {@code workspace} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workspace(McpGitHubWorkspace value) {
            this.workspace = value;
            this.$presence5 = true;
            return this;
        }

        /**
         * Sets the {@code buildConfig} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder buildConfig(Boolean value) {
            this.buildConfig = value;
            return this;
        }

        /**
         * Sets the {@code options} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder options(GitHubMcpOptions value) {
            this.options = value;
            this.$presence7 = true;
            return this;
        }

        /**
         * Sets the {@code authInfo} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authInfo(Object value) {
            this.authInfo = value;
            this.$presence8 = true;
            return this;
        }

        /**
         * Sets the {@code authContextId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authContextId(String value) {
            this.authContextId = value;
            return this;
        }

        /**
         * Sets the {@code authOwnerId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authOwnerId(String value) {
            this.authOwnerId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpResolveGitHubConfigParams build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && flags == null) $explicitNulls.add("flags");
            if ($presence1 && settings == null) $explicitNulls.add("settings");
            if ($presence2 && session == null) $explicitNulls.add("session");
            if ($presence5 && workspace == null) $explicitNulls.add("workspace");
            if ($presence7 && options == null) $explicitNulls.add("options");
            if ($presence8 && authInfo == null) $explicitNulls.add("authInfo");
            return new McpResolveGitHubConfigParams(flags, settings, session, fidesIfcEnabled, excludeGhReplaceableTools, workspace, buildConfig, options, authInfo, authContextId, authOwnerId, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("flags") && flags == null) values.put("flags", null);
        if ($explicitNulls.contains("settings") && settings == null) values.put("settings", null);
        if ($explicitNulls.contains("session") && session == null) values.put("session", null);
        if ($explicitNulls.contains("workspace") && workspace == null) values.put("workspace", null);
        if ($explicitNulls.contains("options") && options == null) values.put("options", null);
        if ($explicitNulls.contains("authInfo") && authInfo == null) values.put("authInfo", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<McpResolveGitHubConfigParams> {
        @Override
        public McpResolveGitHubConfigParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(McpResolveGitHubConfigParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("flags") && tree.get("flags").isNull()) $explicitNulls.add("flags");
            if (tree.has("settings") && tree.get("settings").isNull()) $explicitNulls.add("settings");
            if (tree.has("session") && tree.get("session").isNull()) $explicitNulls.add("session");
            if (tree.has("workspace") && tree.get("workspace").isNull()) $explicitNulls.add("workspace");
            if (tree.has("options") && tree.get("options").isNull()) $explicitNulls.add("options");
            if (tree.has("authInfo") && tree.get("authInfo").isNull()) $explicitNulls.add("authInfo");
            return new McpResolveGitHubConfigParams(readProperty(context, tree, "flags", new com.fasterxml.jackson.core.type.TypeReference<GitHubMcpOptions>() {}), readProperty(context, tree, "settings", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "session", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "fidesIfcEnabled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "excludeGhReplaceableTools", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "workspace", new com.fasterxml.jackson.core.type.TypeReference<McpGitHubWorkspace>() {}), readProperty(context, tree, "buildConfig", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "options", new com.fasterxml.jackson.core.type.TypeReference<GitHubMcpOptions>() {}), readProperty(context, tree, "authInfo", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "authContextId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "authOwnerId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
