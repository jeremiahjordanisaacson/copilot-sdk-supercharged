/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * MCP server discovered by `mcp.discover`, with config source, optional plugin source, transport type, and enabled state.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = DiscoveredMcpServer.$NullableDeserializer.class)
public record DiscoveredMcpServer(
    /** Server name (config key) */
    @JsonProperty("name") String name,
    /** Server transport type: stdio, http, sse (deprecated), or memory */
    @JsonProperty("type") DiscoveredMcpServerType type,
    /** Configuration source: user, workspace, plugin, or builtin */
    @JsonProperty("source") McpServerSource source,
    /** Plugin name that provided this server, when source is plugin. */
    @JsonProperty("sourcePlugin") String sourcePlugin,
    /** Plugin version that provided this server, when source is plugin. */
    @JsonProperty("sourcePluginVersion") String sourcePluginVersion,
    /** Canonical identity and location of the effective server declaration. */
    @JsonProperty("effectiveSource") McpSourceRef effectiveSource,
    /** Effective serializable configuration, present only when includeConfiguration is true. Authentication remains owned by the runtime; callers must not log secret configuration values. */
    @JsonProperty("configuration") Object configuration,
    /** Whether the server is enabled (not in the disabled list) */
    @JsonProperty("enabled") Boolean enabled,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public DiscoveredMcpServer { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private DiscoveredMcpServerType type;
        private McpServerSource source;
        private String sourcePlugin;
        private String sourcePluginVersion;
        private McpSourceRef effectiveSource;
        private Object configuration;
        private boolean $presence6;
        private Boolean enabled;

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
         * Sets the {@code type} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder type(DiscoveredMcpServerType value) {
            this.type = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(McpServerSource value) {
            this.source = value;
            return this;
        }

        /**
         * Sets the {@code sourcePlugin} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sourcePlugin(String value) {
            this.sourcePlugin = value;
            return this;
        }

        /**
         * Sets the {@code sourcePluginVersion} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sourcePluginVersion(String value) {
            this.sourcePluginVersion = value;
            return this;
        }

        /**
         * Sets the {@code effectiveSource} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder effectiveSource(McpSourceRef value) {
            this.effectiveSource = value;
            return this;
        }

        /**
         * Sets the {@code configuration} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configuration(Object value) {
            this.configuration = value;
            this.$presence6 = true;
            return this;
        }

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public DiscoveredMcpServer build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (source == null) throw new IllegalStateException("Missing required property: source");
            if (enabled == null) throw new IllegalStateException("Missing required property: enabled");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence6 && configuration == null) $explicitNulls.add("configuration");
            return new DiscoveredMcpServer(name, type, source, sourcePlugin, sourcePluginVersion, effectiveSource, configuration, enabled, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("configuration") && configuration == null) values.put("configuration", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<DiscoveredMcpServer> {
        @Override
        public DiscoveredMcpServer deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(DiscoveredMcpServer.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("configuration") && tree.get("configuration").isNull()) $explicitNulls.add("configuration");
            return new DiscoveredMcpServer(readProperty(context, tree, "name", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "type", new com.fasterxml.jackson.core.type.TypeReference<DiscoveredMcpServerType>() {}), readProperty(context, tree, "source", new com.fasterxml.jackson.core.type.TypeReference<McpServerSource>() {}), readProperty(context, tree, "sourcePlugin", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sourcePluginVersion", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "effectiveSource", new com.fasterxml.jackson.core.type.TypeReference<McpSourceRef>() {}), readProperty(context, tree, "configuration", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "enabled", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }

    @Override
    public String toString() {
        return "DiscoveredMcpServer[name=" + name + ", type=" + type + ", source=" + source + ", sourcePlugin=" + sourcePlugin + ", sourcePluginVersion=" + sourcePluginVersion + ", effectiveSource=" + effectiveSource + ", configuration=" + (configuration == null ? null : "<redacted>") + ", enabled=" + enabled + ", $explicitNulls=" + $explicitNulls + "]";
    }
}
