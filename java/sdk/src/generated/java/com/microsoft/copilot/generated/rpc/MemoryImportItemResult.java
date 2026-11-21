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
 * Outcome for one memory record.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = MemoryImportItemResult.$NullableDeserializer.class)
public record MemoryImportItemResult(
    /** Physical input line number. */
    @JsonProperty("line") Long line,
    /** External memory identifier. */
    @JsonProperty("externalId") String externalId,
    /** Record outcome: imported, skipped, failed, or validated. */
    @JsonProperty("status") String status,
    /** Stable record result code. */
    @JsonProperty("code") String code,
    /** Description of the record outcome. */
    @JsonProperty("message") String message,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public MemoryImportItemResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Long line;
        private String externalId;
        private boolean $presence1;
        private String status;
        private String code;
        private String message;

        /**
         * Sets the {@code line} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder line(Long value) {
            this.line = value;
            return this;
        }

        /**
         * Sets the {@code externalId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder externalId(String value) {
            this.externalId = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder status(String value) {
            this.status = value;
            return this;
        }

        /**
         * Sets the {@code code} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder code(String value) {
            this.code = value;
            return this;
        }

        /**
         * Sets the {@code message} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder message(String value) {
            this.message = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public MemoryImportItemResult build() {
            if (line == null) throw new IllegalStateException("Missing required property: line");
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && externalId == null) $explicitNulls.add("externalId");
            return new MemoryImportItemResult(line, externalId, status, code, message, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("externalId") && externalId == null) values.put("externalId", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<MemoryImportItemResult> {
        @Override
        public MemoryImportItemResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(MemoryImportItemResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("externalId") && tree.get("externalId").isNull()) $explicitNulls.add("externalId");
            return new MemoryImportItemResult(readProperty(context, tree, "line", new com.fasterxml.jackson.core.type.TypeReference<Long>() {}), readProperty(context, tree, "externalId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "status", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "code", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "message", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
