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
 * A tool request from an external conversation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = HistoryImportToolRequest.$NullableDeserializer.class)
public record HistoryImportToolRequest(
    /** Original tool name. */
    @JsonProperty("name") String name,
    /** Identifier shared with the invocation and result. */
    @JsonProperty("toolCallId") String toolCallId,
    /** Original tool arguments. */
    @JsonProperty("arguments") Object arguments,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public HistoryImportToolRequest { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String toolCallId;
        private Object arguments;
        private boolean $presence2;

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
         * Sets the {@code toolCallId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder toolCallId(String value) {
            this.toolCallId = value;
            return this;
        }

        /**
         * Sets the {@code arguments} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder arguments(Object value) {
            this.arguments = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HistoryImportToolRequest build() {
            if (name == null) throw new IllegalStateException("Missing required property: name");
            if (toolCallId == null) throw new IllegalStateException("Missing required property: toolCallId");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence2 && arguments == null) $explicitNulls.add("arguments");
            return new HistoryImportToolRequest(name, toolCallId, arguments, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("arguments") && arguments == null) values.put("arguments", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<HistoryImportToolRequest> {
        @Override
        public HistoryImportToolRequest deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(HistoryImportToolRequest.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("arguments") && tree.get("arguments").isNull()) $explicitNulls.add("arguments");
            return new HistoryImportToolRequest(readProperty(context, tree, "name", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "toolCallId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "arguments", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
