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
 * Execution accounting and autonomous continuation.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionUpdateExecutionOptions.$NullableDeserializer.class)
public record SessionUpdateExecutionOptions(
    /** Whether to allow auto-mode continuation across turns. */
    @JsonProperty("continueOnAutoMode") Boolean continueOnAutoMode,
    /** Optional session limits. Pass null to clear the session limits. */
    @JsonProperty("limits") SessionLimitsConfig limits,
    /** Patch the Autopilot policy. Omitted fields retain their current values; an empty object makes no changes. Does not change the session mode. */
    @JsonProperty("autopilot") AutopilotOptions autopilot,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateExecutionOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private Boolean continueOnAutoMode;
        private SessionLimitsConfig limits;
        private boolean $presence1;
        private AutopilotOptions autopilot;

        /**
         * Sets the {@code continueOnAutoMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder continueOnAutoMode(Boolean value) {
            this.continueOnAutoMode = value;
            return this;
        }

        /**
         * Sets the {@code limits} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder limits(SessionLimitsConfig value) {
            this.limits = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Sets the {@code autopilot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder autopilot(AutopilotOptions value) {
            this.autopilot = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateExecutionOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence1 && limits == null) $explicitNulls.add("limits");
            return new SessionUpdateExecutionOptions(continueOnAutoMode, limits, autopilot, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("limits") && limits == null) values.put("limits", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionUpdateExecutionOptions> {
        @Override
        public SessionUpdateExecutionOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionUpdateExecutionOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("limits") && tree.get("limits").isNull()) $explicitNulls.add("limits");
            return new SessionUpdateExecutionOptions(readProperty(context, tree, "continueOnAutoMode", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "limits", new com.fasterxml.jackson.core.type.TypeReference<SessionLimitsConfig>() {}), readProperty(context, tree, "autopilot", new com.fasterxml.jackson.core.type.TypeReference<AutopilotOptions>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
