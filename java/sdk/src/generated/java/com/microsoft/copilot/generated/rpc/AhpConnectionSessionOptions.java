/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Options understood by the remote AHP session source.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = AhpConnectionSessionOptions.$NullableDeserializer.class)
record AhpConnectionSessionOptions(
    /** Host-advertised agent provider. */
    @JsonProperty("provider") String provider,
    /** Initial model selection. */
    @JsonProperty("model") AhpConnectionModelSelection model,
    /** Agent-specific initial configuration. */
    @JsonInclude(value = JsonInclude.Include.NON_NULL, content = JsonInclude.Include.ALWAYS)
    @JsonProperty("config") Map<String, Object> config,
    /** Directory on the remote host. Omission uses its default. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Optional project to provision before session creation. */
    @JsonProperty("project") AhpConnectionProjectOptions project,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionSessionOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String provider;
        private boolean $presence0;
        private AhpConnectionModelSelection model;
        private boolean $presence1;
        private Map<String, Object> config;
        private String workingDirectory;
        private boolean $presence3;
        private AhpConnectionProjectOptions project;
        private boolean $presence4;

        /**
         * Sets the {@code provider} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder provider(String value) {
            this.provider = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code model} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder model(AhpConnectionModelSelection value) {
            this.model = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code config} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder config(Map<String, Object> value) {
            this.config = value;
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
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code project} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder project(AhpConnectionProjectOptions value) {
            this.project = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionSessionOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && provider == null) $explicitNulls.add("provider");
            if ($presence1 && model == null) $explicitNulls.add("model");
            if ($presence3 && workingDirectory == null) $explicitNulls.add("workingDirectory");
            if ($presence4 && project == null) $explicitNulls.add("project");
            return new AhpConnectionSessionOptions(provider, model, config, workingDirectory, project, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("provider") && provider == null) values.put("provider", null);
        if ($explicitNulls.contains("model") && model == null) values.put("model", null);
        if ($explicitNulls.contains("workingDirectory") && workingDirectory == null) values.put("workingDirectory", null);
        if ($explicitNulls.contains("project") && project == null) values.put("project", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<AhpConnectionSessionOptions> {
        @Override
        public AhpConnectionSessionOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(AhpConnectionSessionOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("provider") && tree.get("provider").isNull()) $explicitNulls.add("provider");
            if (tree.has("model") && tree.get("model").isNull()) $explicitNulls.add("model");
            if (tree.has("workingDirectory") && tree.get("workingDirectory").isNull()) $explicitNulls.add("workingDirectory");
            if (tree.has("project") && tree.get("project").isNull()) $explicitNulls.add("project");
            return new AhpConnectionSessionOptions(readProperty(context, tree, "provider", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "model", new com.fasterxml.jackson.core.type.TypeReference<AhpConnectionModelSelection>() {}), readProperty(context, tree, "config", new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {}), readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "project", new com.fasterxml.jackson.core.type.TypeReference<AhpConnectionProjectOptions>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
