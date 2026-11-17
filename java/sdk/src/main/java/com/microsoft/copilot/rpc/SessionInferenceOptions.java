/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.microsoft.copilot.CopilotExperimental;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
/** Grouped SessionInference configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionInferenceOptions {

    private String reasoningEffort;
    private String reasoningSummary;
    private Boolean allowMultipleToolCalls;
    private String contextTier;
    @JsonProperty("defaultProvider")
    private JsonNode defaultProvider;
    private List<NamedProviderConfig> providers;
    private List<ProviderModelConfig> models;
    private Boolean enableCitations;
    private ModelCapabilitiesOverride capabilities;

    /**
     * Gets the reasoning effort level.
     *
     * @return the reasoning effort level ("low", "medium", "high", "xhigh", or
     *         "max")
     */
    public String getReasoningEffort() {
        return reasoningEffort;
    }

    /**
     * Sets the reasoning effort level for models that support it.
     * <p>
     * Valid values: "low", "medium", "high", "xhigh", "max".
     *
     * @param reasoningEffort
     *            the reasoning effort level
     * @return this config for method chaining
     */
    public SessionInferenceOptions setReasoningEffort(String reasoningEffort) {
        this.reasoningEffort = reasoningEffort;
        return this;
    }

    /**
     * Gets the reasoning summary mode.
     *
     * @return the reasoning summary mode ("none", "concise", or "detailed")
     */
    public String getReasoningSummary() {
        return reasoningSummary;
    }

    /**
     * Sets the reasoning summary mode for models that support configurable
     * reasoning summaries. Use {@code "none"} to suppress summary output regardless
     * of whether reasoning is enabled.
     *
     * @param reasoningSummary
     *            the reasoning summary mode
     * @return this config for method chaining
     */
    public SessionInferenceOptions setReasoningSummary(String reasoningSummary) {
        this.reasoningSummary = reasoningSummary;
        return this;
    }

    /**
     * Gets the model multiple-tool-call preference.
     *
     * @return the preference, or an empty optional to preserve a resident value
     */
    @JsonIgnore
    public Optional<Boolean> getAllowMultipleToolCalls() {
        return Optional.ofNullable(allowMultipleToolCalls);
    }

    /**
     * Sets whether the model may request multiple tool calls per response. False is
     * a provider hint, not an execution guarantee: all returned calls are handled
     * and handlers may run concurrently. Re-supply on cold resume.
     *
     * @param allowMultipleToolCalls
     *            the preference
     * @return this config for method chaining
     */
    public SessionInferenceOptions setAllowMultipleToolCalls(boolean allowMultipleToolCalls) {
        this.allowMultipleToolCalls = allowMultipleToolCalls;
        return this;
    }

    /**
     * Gets the context window tier.
     *
     * @return the context window tier ("default" or "long_context")
     */
    public String getContextTier() {
        return contextTier;
    }

    /**
     * Sets the context window tier to apply on resume for models that support it.
     * Use {@code "long_context"} to pin the session to the long-context tier; omit
     * or use {@code "default"} otherwise.
     *
     * @param contextTier
     *            the context window tier
     * @return this config for method chaining
     */
    public SessionInferenceOptions setContextTier(String contextTier) {
        this.contextTier = contextTier;
        return this;
    }

    /**
     * Gets the default provider name.
     *
     * @return the name, or null if omitted or explicitly cleared
     */
    @JsonIgnore
    public String getDefaultProvider() {
        return defaultProvider == null || defaultProvider.isNull() ? null : defaultProvider.asText();
    }

    /**
     * Sets the connection for unqualified BYOK selections. Requires
     * {@code github.capi.enabled=false}. Omission creates no default and preserves
     * it on resume; null clears it. Does not select the initial model. Pass-through
     * uses inferred metadata and session processing, without provider-wide
     * defaults.
     *
     * @param defaultProvider
     *            the configured name, or null to clear the default
     * @return this config for method chaining
     */
    public SessionInferenceOptions setDefaultProvider(String defaultProvider) {
        this.defaultProvider = defaultProvider == null ? NullNode.instance : TextNode.valueOf(defaultProvider);
        return this;
    }

    /**
     * Reports whether routing was supplied, including explicit null.
     *
     * @return true when the setter has been called
     */
    @JsonIgnore
    public boolean hasDefaultProvider() {
        return defaultProvider != null;
    }

    /**
     * Gets the named BYOK provider connections.
     *
     * @return the named provider connections, or {@code null} if not set
     */
    @CopilotExperimental
    public List<NamedProviderConfig> getProviders() {
        return providers;
    }

    /**
     * Re-supplies the named BYOK provider connections on resume (additive
     * multi-provider registry).
     * <p>
     * Attach models referencing these connections with {@link #setModels(List)}.
     *
     * @param providers
     *            the named provider connections
     * @return this config instance for method chaining
     * @see NamedProviderConfig
     */
    @CopilotExperimental
    public SessionInferenceOptions setProviders(List<NamedProviderConfig> providers) {
        this.providers = providers;
        return this;
    }

    /**
     * Gets the BYOK model definitions.
     *
     * @return the model definitions, or {@code null} if not set
     */
    @CopilotExperimental
    public List<ProviderModelConfig> getModels() {
        return models;
    }

    /**
     * Re-supplies the BYOK model definitions on resume, each referencing a named
     * provider supplied via {@link #setProviders(List)}.
     *
     * @param models
     *            the model definitions
     * @return this config instance for method chaining
     * @see ProviderModelConfig
     */
    @CopilotExperimental
    public SessionInferenceOptions setModels(List<ProviderModelConfig> models) {
        this.models = models;
        return this;
    }

    /**
     * Gets whether native model citations are enabled.
     *
     * @return an {@link java.util.Optional} containing whether citations are
     *         enabled, or {@link java.util.Optional#empty()} for the default
     */
    @CopilotExperimental
    @JsonIgnore
    public Optional<Boolean> getEnableCitations() {
        return Optional.ofNullable(enableCitations);
    }

    /**
     * Enables or disables native model citations for supported providers.
     *
     * @param enableCitations
     *            whether to enable citations
     * @return this config instance for method chaining
     */
    @CopilotExperimental
    public SessionInferenceOptions setEnableCitations(boolean enableCitations) {
        this.enableCitations = enableCitations;
        return this;
    }

    /**
     * Gets the model capabilities override.
     *
     * @return the model capabilities override, or {@code null} if not set
     */
    public ModelCapabilitiesOverride getCapabilities() {
        return capabilities;
    }

    /**
     * Sets per-property overrides for model capabilities, deep-merged over runtime
     * defaults.
     *
     * @param capabilities
     *            the model capabilities override
     * @return this config for method chaining
     * @see ModelCapabilitiesOverride
     */
    public SessionInferenceOptions setCapabilities(ModelCapabilitiesOverride capabilities) {
        this.capabilities = capabilities;
        return this;
    }
    /**
     * Copies the group, retaining callback identity and copying mutable
     * collections.
     *
     * @return the independent group copy
     */
    @Override
    public SessionInferenceOptions clone() {
        SessionInferenceOptions copy = new SessionInferenceOptions();
        copy.defaultProvider = this.defaultProvider;
        copy.providers = this.providers != null ? new ArrayList<>(this.providers) : null;
        copy.models = this.models != null ? new ArrayList<>(this.models) : null;
        copy.enableCitations = this.enableCitations;
        copy.reasoningEffort = this.reasoningEffort;
        copy.reasoningSummary = this.reasoningSummary;
        copy.allowMultipleToolCalls = this.allowMultipleToolCalls;
        copy.contextTier = this.contextTier;
        copy.capabilities = this.capabilities;
        return copy;
    }

    /**
     * Clears the model multiple-tool-call preference, restoring the default.
     *
     * @return this config for method chaining
     */
    public SessionInferenceOptions clearAllowMultipleToolCalls() {
        this.allowMultipleToolCalls = null;
        return this;
    }

    /**
     * Clears the enableCitations setting, reverting to the default behavior.
     *
     * @return this instance for method chaining
     */
    @CopilotExperimental
    public SessionInferenceOptions clearEnableCitations() {
        this.enableCitations = null;
        return this;
    }
}
