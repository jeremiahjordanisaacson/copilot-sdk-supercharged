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
 * Model tuning and provider catalog.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionUpdateInferenceOptions.$NullableDeserializer.class)
public record SessionUpdateInferenceOptions(
    /** Per-property model capability overrides for the selected model. */
    @JsonProperty("capabilities") ModelCapabilitiesOverride capabilities,
    /** Reasoning effort for the selected model. CAPI values are model-defined and validated against the selected model; BYOK providers may define additional values. When omitted, no effort override is applied. */
    @JsonProperty("reasoningEffort") String reasoningEffort,
    /** Reasoning summary mode for supported model clients. */
    @JsonProperty("reasoningSummary") OptionsUpdateReasoningSummary reasoningSummary,
    /** Output verbosity level for supported models. */
    @JsonProperty("verbosity") Verbosity verbosity,
    /** Connection used for unqualified selections. Omission preserves routing;
null clears it. Requires effective CAPI disablement. */
    @JsonProperty("defaultProvider") String defaultProvider,
    /** Whether to surface reasoning-summary events from the model. */
    @JsonProperty("enableReasoningSummaries") Boolean enableReasoningSummaries,
    /** Context tier for models with tiered pricing. The session uses this to derive effective `modelCapabilitiesOverrides` so compaction, truncation, token display, and request limits honor the selected tier. */
    @JsonProperty("contextTier") OptionsUpdateContextTier contextTier,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionUpdateInferenceOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private ModelCapabilitiesOverride capabilities;
        private String reasoningEffort;
        private OptionsUpdateReasoningSummary reasoningSummary;
        private Verbosity verbosity;
        private String defaultProvider;
        private boolean $presence4;
        private Boolean enableReasoningSummaries;
        private OptionsUpdateContextTier contextTier;

        /**
         * Sets the {@code capabilities} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder capabilities(ModelCapabilitiesOverride value) {
            this.capabilities = value;
            return this;
        }

        /**
         * Sets the {@code reasoningEffort} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reasoningEffort(String value) {
            this.reasoningEffort = value;
            return this;
        }

        /**
         * Sets the {@code reasoningSummary} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder reasoningSummary(OptionsUpdateReasoningSummary value) {
            this.reasoningSummary = value;
            return this;
        }

        /**
         * Sets the {@code verbosity} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder verbosity(Verbosity value) {
            this.verbosity = value;
            return this;
        }

        /**
         * Sets the {@code defaultProvider} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder defaultProvider(String value) {
            this.defaultProvider = value;
            this.$presence4 = true;
            return this;
        }

        /**
         * Sets the {@code enableReasoningSummaries} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enableReasoningSummaries(Boolean value) {
            this.enableReasoningSummaries = value;
            return this;
        }

        /**
         * Sets the {@code contextTier} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder contextTier(OptionsUpdateContextTier value) {
            this.contextTier = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionUpdateInferenceOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence4 && defaultProvider == null) $explicitNulls.add("defaultProvider");
            return new SessionUpdateInferenceOptions(capabilities, reasoningEffort, reasoningSummary, verbosity, defaultProvider, enableReasoningSummaries, contextTier, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("defaultProvider") && defaultProvider == null) values.put("defaultProvider", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionUpdateInferenceOptions> {
        @Override
        public SessionUpdateInferenceOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionUpdateInferenceOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("defaultProvider") && tree.get("defaultProvider").isNull()) $explicitNulls.add("defaultProvider");
            return new SessionUpdateInferenceOptions(readProperty(context, tree, "capabilities", new com.fasterxml.jackson.core.type.TypeReference<ModelCapabilitiesOverride>() {}), readProperty(context, tree, "reasoningEffort", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "reasoningSummary", new com.fasterxml.jackson.core.type.TypeReference<OptionsUpdateReasoningSummary>() {}), readProperty(context, tree, "verbosity", new com.fasterxml.jackson.core.type.TypeReference<Verbosity>() {}), readProperty(context, tree, "defaultProvider", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "enableReasoningSummaries", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "contextTier", new com.fasterxml.jackson.core.type.TypeReference<OptionsUpdateContextTier>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
