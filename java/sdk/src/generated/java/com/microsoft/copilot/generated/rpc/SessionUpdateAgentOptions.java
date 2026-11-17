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
 * Agent definitions, selection, and source filters.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionUpdateAgentOptions.$NullableDeserializer.class)
public record SessionUpdateAgentOptions(
    /** Built-in subagent names to include in this session. When specified, only these built-ins are available, subject to runtime availability and exclusions. Custom agents with the same name remain available. Set to null to remove the allowlist restriction. */
    @JsonProperty("includedBuiltinAgents") List<String> includedBuiltinAgents,
    /** Built-in subagent names to exclude from this session. Excluded built-ins are hidden from agent discovery and cannot be dispatched unless a custom agent with the same name is available. */
    @JsonProperty("excludedBuiltinAgents") List<String> excludedBuiltinAgents,
    /** Explicit discovery-only agent roots, separate from customAgents and filesystem grants. Available without enableConfigDiscovery, subject to managed/source disables. Relative SDK paths resolve once when supplied against the request's effective workingDirectory; retained roots do not move on cwd changes. Null and omission are equivalent; [] clears only the explicit SDK contribution. */
    @JsonProperty("directories") List<String> directories,
    /** Whether to default custom agents to local-only execution. */
    @JsonProperty("localDiscoveryOnly") Boolean localDiscoveryOnly,
    /** When true, the selected custom agent's prompt is not injected into the user message (skill context is still injected). Used by automation triggers where the agent prompt is already in the problem statement. */
    @JsonProperty("suppressCustomAgentPrompt") Boolean suppressCustomAgentPrompt,
    /** Runtime context discriminator (e.g., `cli`, `actions`). */
    @JsonProperty("context") String context,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateAgentOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private List<String> includedBuiltinAgents;
        private boolean $presence0;
        private List<String> excludedBuiltinAgents;
        private List<String> directories;
        private Boolean localDiscoveryOnly;
        private Boolean suppressCustomAgentPrompt;
        private String context;

        /**
         * Sets the {@code includedBuiltinAgents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includedBuiltinAgents(List<String> value) {
            this.includedBuiltinAgents = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code excludedBuiltinAgents} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder excludedBuiltinAgents(List<String> value) {
            this.excludedBuiltinAgents = value;
            return this;
        }

        /**
         * Sets the {@code directories} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder directories(List<String> value) {
            this.directories = value;
            return this;
        }

        /**
         * Sets the {@code localDiscoveryOnly} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder localDiscoveryOnly(Boolean value) {
            this.localDiscoveryOnly = value;
            return this;
        }

        /**
         * Sets the {@code suppressCustomAgentPrompt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder suppressCustomAgentPrompt(Boolean value) {
            this.suppressCustomAgentPrompt = value;
            return this;
        }

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(String value) {
            this.context = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateAgentOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && includedBuiltinAgents == null) $explicitNulls.add("includedBuiltinAgents");
            return new SessionUpdateAgentOptions(includedBuiltinAgents, excludedBuiltinAgents, directories, localDiscoveryOnly, suppressCustomAgentPrompt, context, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("includedBuiltinAgents") && includedBuiltinAgents == null) values.put("includedBuiltinAgents", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionUpdateAgentOptions> {
        @Override
        public SessionUpdateAgentOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionUpdateAgentOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("includedBuiltinAgents") && tree.get("includedBuiltinAgents").isNull()) $explicitNulls.add("includedBuiltinAgents");
            return new SessionUpdateAgentOptions(readProperty(context, tree, "includedBuiltinAgents", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "excludedBuiltinAgents", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "directories", new com.fasterxml.jackson.core.type.TypeReference<List<String>>() {}), readProperty(context, tree, "localDiscoveryOnly", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "suppressCustomAgentPrompt", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "context", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
