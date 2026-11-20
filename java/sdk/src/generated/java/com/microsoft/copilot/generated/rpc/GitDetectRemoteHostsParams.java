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
 * Directory or already-discovered root whose remotes to classify.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = GitDetectRemoteHostsParams.$NullableDeserializer.class)
record GitDetectRemoteHostsParams(
    /** Absolute directory, or the discovered root when fromRoot is true. */
    @JsonProperty("cwd") String cwd,
    /** Skip root discovery. Defaults to false. */
    @JsonProperty("fromRoot") Boolean fromRoot,
    /** Whether prior discovery found a root. False returns no known hosts. */
    @JsonProperty("found") Boolean found,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    GitDetectRemoteHostsParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String cwd;
        private Boolean fromRoot;
        private boolean $presence1;
        private Boolean found;
        private boolean $presence2;

        /**
         * Sets the {@code cwd} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder cwd(String value) {
            this.cwd = value;
            return this;
        }

        /**
         * Sets the {@code fromRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fromRoot(Boolean value) {
            this.fromRoot = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code found} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder found(Boolean value) {
            this.found = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public GitDetectRemoteHostsParams build() {
            if (cwd == null) throw new IllegalStateException("Missing required property: cwd");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && fromRoot == null) $explicitNulls.add("fromRoot");
            if ($presence2 && found == null) $explicitNulls.add("found");
            return new GitDetectRemoteHostsParams(cwd, fromRoot, found, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("fromRoot") && fromRoot == null) values.put("fromRoot", null);
        if ($explicitNulls.contains("found") && found == null) values.put("found", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<GitDetectRemoteHostsParams> {
        @Override
        public GitDetectRemoteHostsParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(GitDetectRemoteHostsParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("fromRoot") && tree.get("fromRoot").isNull()) $explicitNulls.add("fromRoot");
            if (tree.has("found") && tree.get("found").isNull()) $explicitNulls.add("found");
            return new GitDetectRemoteHostsParams(readProperty(context, tree, "cwd", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "fromRoot", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "found", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
