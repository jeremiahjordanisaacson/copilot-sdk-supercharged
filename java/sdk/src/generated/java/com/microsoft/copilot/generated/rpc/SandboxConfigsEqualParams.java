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
 * Sandbox configurations to compare under runtime identity rules.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SandboxConfigsEqualParams.$NullableDeserializer.class)
record SandboxConfigsEqualParams(
    /** Previous configuration. Absent means no sandbox. */
    @JsonProperty("a") Object a,
    /** New configuration. Absent means no sandbox. */
    @JsonProperty("b") Object b,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxConfigsEqualParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object a;
        private boolean $presence0;
        private Object b;
        private boolean $presence1;

        /**
         * Sets the {@code a} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder a(Object value) {
            this.a = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code b} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder b(Object value) {
            this.b = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxConfigsEqualParams build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && a == null) $explicitNulls.add("a");
            if ($presence1 && b == null) $explicitNulls.add("b");
            return new SandboxConfigsEqualParams(a, b, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("a") && a == null) values.put("a", null);
        if ($explicitNulls.contains("b") && b == null) values.put("b", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SandboxConfigsEqualParams> {
        @Override
        public SandboxConfigsEqualParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SandboxConfigsEqualParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("a") && tree.get("a").isNull()) $explicitNulls.add("a");
            if (tree.has("b") && tree.get("b").isNull()) $explicitNulls.add("b");
            return new SandboxConfigsEqualParams(readProperty(context, tree, "a", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "b", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
