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
 * Read-only MCP configuration composition from explicit source inventories. The result grants no execution authority.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = McpConfigMergeSourcesParams.$NullableDeserializer.class)
record McpConfigMergeSourcesParams(
    /** Working directory for workspace discovery and relative source references. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Configuration root for the user MCP document. */
    @JsonProperty("configDir") String configDir,
    /** Repository boundary for upward workspace discovery. */
    @JsonProperty("repoRoot") String repoRoot,
    /** Resolved installed, live, and explicit plugin records in precedence order. */
    @JsonProperty("installedPlugins") List<?> installedPlugins,
    /** Host-bundled plugin records, below user and workspace configuration. */
    @JsonProperty("builtinPlugins") List<?> builtinPlugins,
    /** Additional MCP configuration, merged after the other sources. */
    @JsonProperty("additionalConfig") Object additionalConfig,
    /** Include workspace files only where the runtime confirms folder trust. */
    @JsonProperty("includeWorkspaceSources") Boolean includeWorkspaceSources,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpConfigMergeSourcesParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String workingDirectory;
        private String configDir;
        private String repoRoot;
        private List<?> installedPlugins;
        private List<?> builtinPlugins;
        private Object additionalConfig;
        private boolean $presence5;
        private Boolean includeWorkspaceSources;

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code configDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configDir(String value) {
            this.configDir = value;
            return this;
        }

        /**
         * Sets the {@code repoRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder repoRoot(String value) {
            this.repoRoot = value;
            return this;
        }

        /**
         * Sets the {@code installedPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder installedPlugins(List<?> value) {
            this.installedPlugins = value;
            return this;
        }

        /**
         * Sets the {@code builtinPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder builtinPlugins(List<?> value) {
            this.builtinPlugins = value;
            return this;
        }

        /**
         * Sets the {@code additionalConfig} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder additionalConfig(Object value) {
            this.additionalConfig = value;
            this.$presence5 = true;
            return this;
        }

        /**
         * Sets the {@code includeWorkspaceSources} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeWorkspaceSources(Boolean value) {
            this.includeWorkspaceSources = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpConfigMergeSourcesParams build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (configDir == null) throw new IllegalStateException("Missing required property: configDir");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence5 && additionalConfig == null) $explicitNulls.add("additionalConfig");
            return new McpConfigMergeSourcesParams(workingDirectory, configDir, repoRoot, installedPlugins, builtinPlugins, additionalConfig, includeWorkspaceSources, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("additionalConfig") && additionalConfig == null) values.put("additionalConfig", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<McpConfigMergeSourcesParams> {
        @Override
        public McpConfigMergeSourcesParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(McpConfigMergeSourcesParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("additionalConfig") && tree.get("additionalConfig").isNull()) $explicitNulls.add("additionalConfig");
            return new McpConfigMergeSourcesParams(readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "configDir", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "repoRoot", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "installedPlugins", new com.fasterxml.jackson.core.type.TypeReference<List<?>>() {}), readProperty(context, tree, "builtinPlugins", new com.fasterxml.jackson.core.type.TypeReference<List<?>>() {}), readProperty(context, tree, "additionalConfig", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "includeWorkspaceSources", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
