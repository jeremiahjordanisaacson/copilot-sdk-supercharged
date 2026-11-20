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
 * Seeds project provisioning on a newly created AHP session.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = AhpConnectionProjectOptions.$NullableDeserializer.class)
record AhpConnectionProjectOptions(
    /** Credential-free HTTPS clone URL. */
    @JsonProperty("cloneUrl") String cloneUrl,
    /** Optional branch for the session worktree. */
    @JsonProperty("baseBranch") String baseBranch,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionProjectOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String cloneUrl;
        private String baseBranch;
        private boolean $presence1;

        /**
         * Sets the {@code cloneUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder cloneUrl(String value) {
            this.cloneUrl = value;
            return this;
        }

        /**
         * Sets the {@code baseBranch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder baseBranch(String value) {
            this.baseBranch = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionProjectOptions build() {
            if (cloneUrl == null) throw new IllegalStateException("Missing required property: cloneUrl");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && baseBranch == null) $explicitNulls.add("baseBranch");
            return new AhpConnectionProjectOptions(cloneUrl, baseBranch, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("baseBranch") && baseBranch == null) values.put("baseBranch", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<AhpConnectionProjectOptions> {
        @Override
        public AhpConnectionProjectOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(AhpConnectionProjectOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("baseBranch") && tree.get("baseBranch").isNull()) $explicitNulls.add("baseBranch");
            return new AhpConnectionProjectOptions(readProperty(context, tree, "cloneUrl", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "baseBranch", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
