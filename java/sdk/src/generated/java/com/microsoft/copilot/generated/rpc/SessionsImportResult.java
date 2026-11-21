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
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Result of a semantic session import.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionsImportResult.$NullableDeserializer.class)
public record SessionsImportResult(
    /** Whether the operation succeeded. */
    @JsonProperty("ok") Boolean ok,
    /** Stable operation result code. */
    @JsonProperty("code") String code,
    /** Description of the operation result. */
    @JsonProperty("message") String message,
    /** External session identifier. */
    @JsonProperty("externalId") String externalId,
    /** Created native session identifier. */
    @JsonProperty("sessionId") String sessionId,
    /** Whether validation ran without persistence. */
    @JsonProperty("dryRun") Boolean dryRun,
    /** Prepared or imported record counts. */
    @JsonProperty("imported") SessionImportCounts imported,
    /** Input validation diagnostics. */
    @JsonProperty("diagnostics") List<ImportDiagnostic> diagnostics,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionsImportResult { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Boolean ok;
        private String code;
        private String message;
        private String externalId;
        private boolean $presence3;
        private String sessionId;
        private boolean $presence4;
        private Boolean dryRun;
        private SessionImportCounts imported;
        private List<ImportDiagnostic> diagnostics;

        /**
         * Sets the {@code ok} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder ok(Boolean value) {
            this.ok = value;
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
         * Sets the {@code externalId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder externalId(String value) {
            this.externalId = value;
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
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
            return this;
        }

        /**
         * Sets the {@code imported} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder imported(SessionImportCounts value) {
            this.imported = value;
            return this;
        }

        /**
         * Sets the {@code diagnostics} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder diagnostics(List<ImportDiagnostic> value) {
            this.diagnostics = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsImportResult build() {
            if (ok == null) throw new IllegalStateException("Missing required property: ok");
            if (code == null) throw new IllegalStateException("Missing required property: code");
            if (message == null) throw new IllegalStateException("Missing required property: message");
            if (dryRun == null) throw new IllegalStateException("Missing required property: dryRun");
            if (imported == null) throw new IllegalStateException("Missing required property: imported");
            if (diagnostics == null) throw new IllegalStateException("Missing required property: diagnostics");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && externalId == null) $explicitNulls.add("externalId");
            if ($presence4 && sessionId == null) $explicitNulls.add("sessionId");
            return new SessionsImportResult(ok, code, message, externalId, sessionId, dryRun, imported, diagnostics, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("externalId") && externalId == null) values.put("externalId", null);
        if ($explicitNulls.contains("sessionId") && sessionId == null) values.put("sessionId", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionsImportResult> {
        @Override
        public SessionsImportResult deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionsImportResult.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("externalId") && tree.get("externalId").isNull()) $explicitNulls.add("externalId");
            if (tree.has("sessionId") && tree.get("sessionId").isNull()) $explicitNulls.add("sessionId");
            return new SessionsImportResult(readProperty(context, tree, "ok", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "code", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "message", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "externalId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "sessionId", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "dryRun", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "imported", new com.fasterxml.jackson.core.type.TypeReference<SessionImportCounts>() {}), readProperty(context, tree, "diagnostics", new com.fasterxml.jackson.core.type.TypeReference<List<ImportDiagnostic>>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
