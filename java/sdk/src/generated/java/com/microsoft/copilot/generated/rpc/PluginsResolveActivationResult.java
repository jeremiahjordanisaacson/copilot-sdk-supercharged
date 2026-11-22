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
 * Canonical activation after workspace consent and plugin policy reconciliation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = PluginsResolveActivationResult.$NullableDeserializer.class)
record PluginsResolveActivationResult(
    /** Current working directory of the session. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Authoritative workspace root, when known. */
    @JsonProperty("workspaceRoot") String workspaceRoot,
    /** Whether the runtime trusts the workspace. */
    @JsonProperty("trusted") Boolean trusted,
    /** Current activation, or null before activation resolves. */
    @JsonProperty("snapshot") UIPluginActivationSnapshot snapshot,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PluginsResolveActivationResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String workspaceRoot;
        private boolean $presence1;
        private Boolean trusted;
        private UIPluginActivationSnapshot snapshot;
        private boolean $presence3;

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
         * Sets the {@code workspaceRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workspaceRoot(String value) {
            this.workspaceRoot = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code trusted} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trusted(Boolean value) {
            this.trusted = value;
            return this;
        }

        /**
         * Sets the {@code snapshot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder snapshot(UIPluginActivationSnapshot value) {
            this.snapshot = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PluginsResolveActivationResult build() {
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (trusted == null) throw new IllegalStateException("Missing required property: trusted");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && workspaceRoot == null) $explicitNulls.add("workspaceRoot");
            if ($presence3 && snapshot == null) $explicitNulls.add("snapshot");
            return new PluginsResolveActivationResult(workingDirectory, workspaceRoot, trusted, snapshot, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("workspaceRoot") && workspaceRoot == null) values.put("workspaceRoot", null);
        if ($explicitNulls.contains("snapshot") && snapshot == null) values.put("snapshot", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<PluginsResolveActivationResult> {
        @Override
        public PluginsResolveActivationResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(PluginsResolveActivationResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("workspaceRoot") && tree.get("workspaceRoot").isNull()) $explicitNulls.add("workspaceRoot");
            if (tree.has("snapshot") && tree.get("snapshot").isNull()) $explicitNulls.add("snapshot");
            return new PluginsResolveActivationResult(readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "workspaceRoot", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "trusted", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "snapshot", new com.fasterxml.jackson.core.type.TypeReference<UIPluginActivationSnapshot>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
