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
 * Model tuning and provider catalog.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = SessionOpenInferenceOptions.$NullableDeserializer.class)
public record SessionOpenInferenceOptions(
    /** Initial reasoning effort level. CAPI values are model-defined and validated against the selected model; BYOK providers may define additional values. When omitted, no effort override is applied. */
    @JsonProperty("reasoningEffort") String reasoningEffort,
    /** Initial reasoning summary mode for supported model clients. */
    @JsonProperty("reasoningSummary") SessionOpenOptionsReasoningSummary reasoningSummary,
    /** Initial output verbosity level for supported models. */
    @JsonProperty("verbosity") Verbosity verbosity,
    /** Connection used for unqualified BYOK selections. Requires effective
`github.capi.enabled: false`. Omission on resume preserves the policy;
explicit null clears it. Does not select an initial model. */
    @JsonProperty("defaultProvider") String defaultProvider,
    /** Named BYOK connections. Built-in Copilot availability is independently controlled by github.capi.enabled. */
    @CopilotExperimental
    @JsonProperty("providers") List<NamedProviderConfig> providers,
    /** BYOK model definitions added to the selectable model list, each referencing a provider name. */
    @CopilotExperimental
    @JsonProperty("models") List<ProviderModelConfig> models,
    /** Experimental: enable native model citations for supported Anthropic and OpenAI models, normalized onto the `assistant.message` event. Off by default; may change or be removed while the citations surface is experimental. */
    @CopilotExperimental
    @JsonProperty("enableCitations") Boolean enableCitations,
    /** Whether the model may request multiple tool calls per response. Defaults to true. False sends a provider hint, not an execution guarantee: all returned calls are still handled and handlers may run concurrently. Re-supply on cold resume. */
    @JsonProperty("allowMultipleToolCalls") Boolean allowMultipleToolCalls,
    /** Initial model capability overrides. */
    @JsonProperty("capabilities") ModelCapabilitiesOverride capabilities,
    @com.fasterxml.jackson.annotation.JsonIgnore java.util.Set<String> $explicitNulls
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionOpenInferenceOptions { $explicitNulls = $explicitNulls == null ? java.util.Set.of() : java.util.Set.copyOf($explicitNulls); }

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
        private String reasoningEffort;
        private SessionOpenOptionsReasoningSummary reasoningSummary;
        private Verbosity verbosity;
        private String defaultProvider;
        private boolean $presence3;
        private List<NamedProviderConfig> providers;
        private List<ProviderModelConfig> models;
        private Boolean enableCitations;
        private Boolean allowMultipleToolCalls;
        private ModelCapabilitiesOverride capabilities;

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
        public Builder reasoningSummary(SessionOpenOptionsReasoningSummary value) {
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
            this.$presence3 = true;
            return this;
        }

        /**
         * Sets the {@code providers} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder providers(List<NamedProviderConfig> value) {
            this.providers = value;
            return this;
        }

        /**
         * Sets the {@code models} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder models(List<ProviderModelConfig> value) {
            this.models = value;
            return this;
        }

        /**
         * Sets the {@code enableCitations} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder enableCitations(Boolean value) {
            this.enableCitations = value;
            return this;
        }

        /**
         * Sets the {@code allowMultipleToolCalls} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowMultipleToolCalls(Boolean value) {
            this.allowMultipleToolCalls = value;
            return this;
        }

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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionOpenInferenceOptions build() {
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if ($presence3 && defaultProvider == null) $explicitNulls.add("defaultProvider");
            return new SessionOpenInferenceOptions(reasoningEffort, reasoningSummary, verbosity, defaultProvider, providers, models, enableCitations, allowMultipleToolCalls, capabilities, $explicitNulls);
        }
    }

    @com.fasterxml.jackson.annotation.JsonAnyGetter
    @JsonInclude(value = JsonInclude.Include.ALWAYS, content = JsonInclude.Include.ALWAYS)
    public java.util.Map<String, Object> $nullableProperties() {
        java.util.Map<String, Object> values = new java.util.LinkedHashMap<>();
        if ($explicitNulls.contains("defaultProvider") && defaultProvider == null) values.put("defaultProvider", null);
        return values;
    }

    public static final class $NullableDeserializer extends com.fasterxml.jackson.databind.JsonDeserializer<SessionOpenInferenceOptions> {
        @Override
        public SessionOpenInferenceOptions deserialize(com.fasterxml.jackson.core.JsonParser parser,
                com.fasterxml.jackson.databind.DeserializationContext context) throws java.io.IOException {
            com.fasterxml.jackson.databind.JsonNode tree = context.readTree(parser);
            if (!tree.isObject()) return context.reportInputMismatch(SessionOpenInferenceOptions.class, "Expected a JSON object");
            java.util.Set<String> $explicitNulls = new java.util.LinkedHashSet<>();
            if (tree.has("defaultProvider") && tree.get("defaultProvider").isNull()) $explicitNulls.add("defaultProvider");
            return new SessionOpenInferenceOptions(readProperty(context, tree, "reasoningEffort", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "reasoningSummary", new com.fasterxml.jackson.core.type.TypeReference<SessionOpenOptionsReasoningSummary>() {}), readProperty(context, tree, "verbosity", new com.fasterxml.jackson.core.type.TypeReference<Verbosity>() {}), readProperty(context, tree, "defaultProvider", new com.fasterxml.jackson.core.type.TypeReference<String>() {}), readProperty(context, tree, "providers", new com.fasterxml.jackson.core.type.TypeReference<List<NamedProviderConfig>>() {}), readProperty(context, tree, "models", new com.fasterxml.jackson.core.type.TypeReference<List<ProviderModelConfig>>() {}), readProperty(context, tree, "enableCitations", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "allowMultipleToolCalls", new com.fasterxml.jackson.core.type.TypeReference<Boolean>() {}), readProperty(context, tree, "capabilities", new com.fasterxml.jackson.core.type.TypeReference<ModelCapabilitiesOverride>() {}), $explicitNulls);
        }

        private static <T> T readProperty(com.fasterxml.jackson.databind.DeserializationContext context,
                com.fasterxml.jackson.databind.JsonNode tree, String name,
                com.fasterxml.jackson.core.type.TypeReference<T> type) throws java.io.IOException {
            if (!tree.has(name)) return null;
            return context.readTreeAsValue(tree.get(name), context.getTypeFactory().constructType(type));
        }
    }
}
