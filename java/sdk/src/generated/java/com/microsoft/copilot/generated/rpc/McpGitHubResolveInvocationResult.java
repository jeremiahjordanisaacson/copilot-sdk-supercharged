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
 * A secret-free GitHub descriptor, an ordinary signed-out result, or a bounded degraded result.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = McpGitHubResolveInvocationResult.$NullableDeserializer.class)
record McpGitHubResolveInvocationResult(
    /** Built-in descriptor without authentication headers; absent when unavailable. */
    @JsonProperty("server") Object server,
    /** Whether the existing two-second resolution budget expired. */
    @JsonProperty("timedOut") Boolean timedOut,
    /** Resolution failure detail, distinct from an ordinary signed-out result. */
    @JsonProperty("warning") String warning,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpGitHubResolveInvocationResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object server;
        private boolean $presence0;
        private Boolean timedOut;
        private String warning;

        /**
         * Sets the {@code server} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder server(Object value) {
            this.server = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code timedOut} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder timedOut(Boolean value) {
            this.timedOut = value;
            return this;
        }

        /**
         * Sets the {@code warning} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder warning(String value) {
            this.warning = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpGitHubResolveInvocationResult build() {
            if (timedOut == null) throw new IllegalStateException("Missing required property: timedOut");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && server == null) $explicitNulls.add("server");
            return new McpGitHubResolveInvocationResult(server, timedOut, warning, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("server") && server == null) values.put("server", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<McpGitHubResolveInvocationResult> {
        @Override
        public McpGitHubResolveInvocationResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(McpGitHubResolveInvocationResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("server") && tree.get("server").isNull()) $explicitNulls.add("server");
            return new McpGitHubResolveInvocationResult(readProperty(context, tree, "server", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "timedOut", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "warning", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
