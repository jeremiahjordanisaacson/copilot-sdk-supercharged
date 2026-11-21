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
 * Input for a semantic session import.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionsImportParams.$NullableDeserializer.class)
public record SessionsImportParams(
    /** Complete semantic JSONL input. */
    @JsonProperty("jsonl") String jsonl,
    /** Input file path used to resolve relative attachments. */
    @JsonProperty("inputPath") String inputPath,
    /** Absolute fallback directory supplied by the caller. */
    @JsonProperty("defaultWorkingDirectory") String defaultWorkingDirectory,
    /** Explicit directory override. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Explicit session name override. */
    @JsonProperty("name") String name,
    /** Validate without creating a session. */
    @JsonProperty("dryRun") Boolean dryRun,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionsImportParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String inputPath;
        private String defaultWorkingDirectory;
        private String workingDirectory;
        private boolean $presence3;
        private String name;
        private boolean $presence4;
        private Boolean dryRun;
        private boolean $presence5;

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
         * Sets the {@code inputPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder inputPath(String value) {
            this.inputPath = value;
            return this;
        }

        /**
         * Sets the {@code defaultWorkingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder defaultWorkingDirectory(String value) {
            this.defaultWorkingDirectory = value;
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
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code name} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder name(String value) {
            this.name = value;
            this.$presence4 = true;
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
            this.$presence5 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsImportParams build() {
            if (jsonl == null) throw new IllegalStateException("Missing required property: jsonl");
            if (inputPath == null) throw new IllegalStateException("Missing required property: inputPath");
            if (defaultWorkingDirectory == null) throw new IllegalStateException("Missing required property: defaultWorkingDirectory");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && workingDirectory == null) $explicitNulls.add("workingDirectory");
            if ($presence4 && name == null) $explicitNulls.add("name");
            if ($presence5 && dryRun == null) $explicitNulls.add("dryRun");
            return new SessionsImportParams(jsonl, inputPath, defaultWorkingDirectory, workingDirectory, name, dryRun, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("workingDirectory") && workingDirectory == null) values.put("workingDirectory", null);
        if ($explicitNulls.contains("name") && name == null) values.put("name", null);
        if ($explicitNulls.contains("dryRun") && dryRun == null) values.put("dryRun", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionsImportParams> {
        @Override
        public SessionsImportParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionsImportParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("workingDirectory") && tree.get("workingDirectory").isNull()) $explicitNulls.add("workingDirectory");
            if (tree.has("name") && tree.get("name").isNull()) $explicitNulls.add("name");
            if (tree.has("dryRun") && tree.get("dryRun").isNull()) $explicitNulls.add("dryRun");
            return new SessionsImportParams(readProperty(context, tree, "jsonl", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "inputPath", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "defaultWorkingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "name", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "dryRun", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
