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
 * Input for a semantic memory import.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = MemoriesImportParams.$NullableDeserializer.class)
public record MemoriesImportParams(
    /** Complete semantic JSONL input. */
    @JsonProperty("jsonl") String jsonl,
    /** Directory used by the memory service. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Validate without submitting records. */
    @JsonProperty("dryRun") Boolean dryRun,
    /** Conflict policy, either skip or error. */
    @JsonProperty("onConflict") String onConflict,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public MemoriesImportParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String jsonl;
        private String workingDirectory;
        private Boolean dryRun;
        private boolean $presence2;
        private String onConflict;
        private boolean $presence3;

        /**
         * Sets the {@code jsonl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder jsonl(String value) {
            this.jsonl = value;
            return this;
        }

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code dryRun} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder dryRun(Boolean value) {
            this.dryRun = value;
            this.$presence2 = true;
            return this;
        }

        /**
         * Sets the {@code onConflict} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder onConflict(String value) {
            this.onConflict = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public MemoriesImportParams build() {
            if (jsonl == null) throw new IllegalStateException("Missing required property: jsonl");
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence2 && dryRun == null) $explicitNulls.add("dryRun");
            if ($presence3 && onConflict == null) $explicitNulls.add("onConflict");
            return new MemoriesImportParams(jsonl, workingDirectory, dryRun, onConflict, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("dryRun") && dryRun == null) values.put("dryRun", null);
        if ($explicitNulls.contains("onConflict") && onConflict == null) values.put("onConflict", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<MemoriesImportParams> {
        @Override
        public MemoriesImportParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(MemoriesImportParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("dryRun") && tree.get("dryRun").isNull()) $explicitNulls.add("dryRun");
            if (tree.has("onConflict") && tree.get("onConflict").isNull()) $explicitNulls.add("onConflict");
            return new MemoriesImportParams(readProperty(context, tree, "jsonl", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "dryRun", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "onConflict", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
