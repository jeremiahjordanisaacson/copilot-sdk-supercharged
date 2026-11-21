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
 * Optional workspace settings for a plugin catalog or bulk update through the original caller's runtime. Omission retains the server's configuration.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = PluginsMarketplacesListParams.$NullableDeserializer.class)
public record PluginsMarketplacesListParams(
    /** Working directory whose repository settings and plugin sources apply to this invocation. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Effective user and repository settings for this invocation. Runtime-owned managed restrictions still apply. */
    @JsonProperty("configOverride") Object configOverride,
    /** Managed settings supplied by the host for canonical plugin policy evaluation. This value does not select a credential owner. */
    @JsonProperty("managedSettings") Object managedSettings,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PluginsMarketplacesListParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object managedSettings;
        private boolean $presence2;

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
         * Sets the {@code managedSettings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder managedSettings(Object value) {
            this.managedSettings = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginsMarketplacesListParams build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && configOverride == null) $explicitNulls.add("configOverride");
            if ($presence2 && managedSettings == null) $explicitNulls.add("managedSettings");
            return new PluginsMarketplacesListParams(workingDirectory, configOverride, managedSettings, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("configOverride") && configOverride == null) values.put("configOverride", null);
        if ($explicitNulls.contains("managedSettings") && managedSettings == null) values.put("managedSettings", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<PluginsMarketplacesListParams> {
        @Override
        public PluginsMarketplacesListParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(PluginsMarketplacesListParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("configOverride") && tree.get("configOverride").isNull()) $explicitNulls.add("configOverride");
            if (tree.has("managedSettings") && tree.get("managedSettings").isNull()) $explicitNulls.add("managedSettings");
            return new PluginsMarketplacesListParams(readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "configOverride", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "managedSettings", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
