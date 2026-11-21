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
 * Inspects MCP source configuration without starting servers. The bound session owns workspace trust.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionMcpInspectConfigurationParams.$NullableDeserializer.class)
record SessionMcpInspectConfigurationParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Directory whose workspace sources to inspect. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Include the matching session activation snapshot and additional configuration. */
    @JsonProperty("includeSessionSources") Boolean includeSessionSources,
    /** Explicit host-mounted plugin records, without claiming folder trust or managed policy. */
    @JsonProperty("explicitPlugins") List<?> explicitPlugins,
    /** Additional host configuration, used only when includeSessionSources is true. */
    @JsonProperty("additionalConfig") Object additionalConfig,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionMcpInspectConfigurationParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String sessionId;
        private String workingDirectory;
        private Boolean includeSessionSources;
        private List<?> explicitPlugins;
        private Object additionalConfig;
        private boolean $presence4;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

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
         * Sets the {@code includeSessionSources} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeSessionSources(Boolean value) {
            this.includeSessionSources = value;
            return this;
        }

        /**
         * Sets the {@code explicitPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder explicitPlugins(List<?> value) {
            this.explicitPlugins = value;
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
            this.$presence4 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionMcpInspectConfigurationParams build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence4 && additionalConfig == null) $explicitNulls.add("additionalConfig");
            return new SessionMcpInspectConfigurationParams(sessionId, workingDirectory, includeSessionSources, explicitPlugins, additionalConfig, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("additionalConfig") && additionalConfig == null) values.put("additionalConfig", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionMcpInspectConfigurationParams> {
        @Override
        public SessionMcpInspectConfigurationParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionMcpInspectConfigurationParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("additionalConfig") && tree.get("additionalConfig").isNull()) $explicitNulls.add("additionalConfig");
            return new SessionMcpInspectConfigurationParams(readProperty(context, tree, "sessionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "includeSessionSources", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "explicitPlugins", new com.fasterxml.jackson.core.type.TypeReference<List<?>>() {}), readProperty(context, tree, "additionalConfig", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
