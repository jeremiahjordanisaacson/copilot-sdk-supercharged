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
 * Semantic conversation input for local import or validation.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionsImportSemanticParams.$NullableDeserializer.class)
record SessionsImportSemanticParams(
    /** Runtime settings overrides, with the original caller's configuration root. */
    @JsonProperty("settings") Object settings,
    /** Semantic session JSONL, not native session events. */
    @JsonProperty("jsonl") String jsonl,
    /** Absolute input path used to resolve attachment references. */
    @JsonProperty("inputPath") String inputPath,
    /** Default working directory when the document supplies none. */
    @JsonProperty("defaultWorkingDirectory") String defaultWorkingDirectory,
    /** Validate without authentication, network access, or writes. */
    @JsonProperty("dryRun") Boolean dryRun,
    /** Explicit working directory override. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Explicit session name override. */
    @JsonProperty("name") String name,
    /** Name of the embedding client. */
    @JsonProperty("clientName") String clientName,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionsImportSemanticParams { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Object settings;
        private boolean $presence0;
        private String jsonl;
        private String inputPath;
        private String defaultWorkingDirectory;
        private Boolean dryRun;
        private String workingDirectory;
        private String name;
        private String clientName;

        /**
         * Sets the {@code settings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder settings(Object value) {
            this.settings = value;
            this.$presence0 = true;
            return this;
        }

        /**
         * Sets the {@code jsonl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
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
        public Builder defaultWorkingDirectory(String value) {
            this.defaultWorkingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code dryRun} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder dryRun(Boolean value) {
            this.dryRun = value;
            return this;
        }

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

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
         * Sets the {@code clientName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder clientName(String value) {
            this.clientName = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsImportSemanticParams build() {
            if (jsonl == null) throw new IllegalStateException("Missing required property: jsonl");
            if (inputPath == null) throw new IllegalStateException("Missing required property: inputPath");
            if (defaultWorkingDirectory == null) throw new IllegalStateException("Missing required property: defaultWorkingDirectory");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence0 && settings == null) $explicitNulls.add("settings");
            return new SessionsImportSemanticParams(settings, jsonl, inputPath, defaultWorkingDirectory, dryRun, workingDirectory, name, clientName, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("settings") && settings == null) values.put("settings", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionsImportSemanticParams> {
        @Override
        public SessionsImportSemanticParams deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionsImportSemanticParams.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("settings") && tree.get("settings").isNull()) $explicitNulls.add("settings");
            return new SessionsImportSemanticParams(readProperty(context, tree, "settings", new com.fasterxml.jackson.core.type.TypeReference<Object>() {}), readProperty(context, tree, "jsonl", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "inputPath", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "defaultWorkingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "dryRun", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "workingDirectory", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "name", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "clientName", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
