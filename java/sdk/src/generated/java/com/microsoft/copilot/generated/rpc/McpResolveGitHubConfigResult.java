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
 * Effective GitHub MCP configuration and override attribution.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = McpResolveGitHubConfigResult.$NullableDeserializer.class)
record McpResolveGitHubConfigResult(
    /** Effective options after source precedence and capability policy. */
    @JsonProperty("options") GitHubMcpOptions options,
    /** True when a source explicitly widens exposure. */
    @JsonProperty("userOverrode") Boolean userOverrode,
    /** Override attribution: none, flag, settings, or both. */
    @JsonProperty("source") String source,
    /** True when the workspace requires web_search-only exposure. */
    @JsonProperty("restrictToAdoWebSearch") Boolean restrictToAdoWebSearch,
    /** Validated session options, if present. */
    @JsonProperty("session") Object session,
    /** Secretless descriptor. Omitted when not requested or not authenticated. */
    @JsonProperty("configJson") String configJson,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpResolveGitHubConfigResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private GitHubMcpOptions options;
        private Boolean userOverrode;
        private String source;
        private Boolean restrictToAdoWebSearch;
        private Object session;
        private boolean $presence4;
        private String configJson;

        /**
         * Sets the {@code options} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder options(GitHubMcpOptions value) {
            this.options = value;
            return this;
        }

        /**
         * Sets the {@code userOverrode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder userOverrode(Boolean value) {
            this.userOverrode = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(String value) {
            this.source = value;
            return this;
        }

        /**
         * Sets the {@code restrictToAdoWebSearch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder restrictToAdoWebSearch(Boolean value) {
            this.restrictToAdoWebSearch = value;
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
            this.$presence4 = true;
            return this;
        }

        /**
         * Sets the {@code configJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder configJson(String value) {
            this.configJson = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpResolveGitHubConfigResult build() {
            if (options == null) throw new IllegalStateException("Missing required property: options");
            if (userOverrode == null) throw new IllegalStateException("Missing required property: userOverrode");
            if (source == null) throw new IllegalStateException("Missing required property: source");
            if (restrictToAdoWebSearch == null) throw new IllegalStateException("Missing required property: restrictToAdoWebSearch");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence4 && session == null) $explicitNulls.add("session");
            return new McpResolveGitHubConfigResult(options, userOverrode, source, restrictToAdoWebSearch, session, configJson, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("session") && session == null) values.put("session", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<McpResolveGitHubConfigResult> {
        @Override
        public McpResolveGitHubConfigResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(McpResolveGitHubConfigResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("session") && tree.get("session").isNull()) $explicitNulls.add("session");
            return new McpResolveGitHubConfigResult(readProperty(context, tree, "options", new com.fasterxml.jackson.core.type.TypeReference<GitHubMcpOptions>() {}), readProperty(context, tree, "userOverrode", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "source", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "restrictToAdoWebSearch", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "session", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "configJson", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
