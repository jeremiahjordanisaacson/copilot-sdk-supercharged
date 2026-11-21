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
 * Inspects persisted and live plugins under an explicit effective configuration without changing the existing user-only plugins.list API.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = PluginsInspectInventoryParams.$NullableDeserializer.class)
record PluginsInspectInventoryParams(
    /** Working directory for repository overlay and live marketplace resolution. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Trusted caller-composed read configuration, not persisted by inspection. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("configOverride") Object configOverride,
    /** Seeds pre-migration enablement from installed records below explicit overrides. */
    @JsonProperty("seedEnabledPluginsFromInstalled") Boolean seedEnabledPluginsFromInstalled,
    /** Effective policy for read-only inspection; mutation guards retain their own authority. */
    @JsonProperty("managedSettings") Object managedSettings,
    /** Additional plugin directories in host precedence order. */
    @JsonProperty("pluginDirectories") List<String> pluginDirectories,
    /** Includes pending managed plugins by default. False excludes uninstalled rows before source merging. */
    @JsonProperty("includeUninstalled") Boolean includeUninstalled,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginsInspectInventoryParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String workingDirectory;
        private Object configOverride;
        private boolean $presence1;
        private Boolean seedEnabledPluginsFromInstalled;
        private Object managedSettings;
        private boolean $presence3;
        private List<String> pluginDirectories;
        private Boolean includeUninstalled;

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code configOverride} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder configOverride(Object value) {
            this.configOverride = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code seedEnabledPluginsFromInstalled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder seedEnabledPluginsFromInstalled(Boolean value) {
            this.seedEnabledPluginsFromInstalled = value;
            return this;
        }

        /**
         * Sets the {@code managedSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder managedSettings(Object value) {
            this.managedSettings = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code pluginDirectories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder pluginDirectories(List<String> value) {
            this.pluginDirectories = value;
            return this;
        }

        /**
         * Sets the {@code includeUninstalled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder includeUninstalled(Boolean value) {
            this.includeUninstalled = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginsInspectInventoryParams build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (!$presence1) throw new IllegalStateException("Missing required property: configOverride");
            if (seedEnabledPluginsFromInstalled == null) throw new IllegalStateException("Missing required property: seedEnabledPluginsFromInstalled");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && managedSettings == null) $explicitNulls.add("managedSettings");
            return new PluginsInspectInventoryParams(workingDirectory, configOverride, seedEnabledPluginsFromInstalled, managedSettings, pluginDirectories, includeUninstalled, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("managedSettings") && managedSettings == null) values.put("managedSettings", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<PluginsInspectInventoryParams> {
        @Override
        public PluginsInspectInventoryParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(PluginsInspectInventoryParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("managedSettings") && tree.get("managedSettings").isNull()) $explicitNulls.add("managedSettings");
            return new PluginsInspectInventoryParams(readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "configOverride", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "seedEnabledPluginsFromInstalled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "managedSettings", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "pluginDirectories", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "includeUninstalled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
