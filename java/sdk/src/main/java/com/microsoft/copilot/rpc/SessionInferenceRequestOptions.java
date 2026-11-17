/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
/** Grouped SessionInferenceRequest configuration. */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SessionInferenceRequestOptions {

    @JsonProperty("reasoningEffort")
    private String reasoningEffort;

    @JsonProperty("reasoningSummary")
    private String reasoningSummary;

    @JsonProperty("allowMultipleToolCalls")
    private Boolean allowMultipleToolCalls;

    @JsonProperty("contextTier")
    private String contextTier;

    @JsonProperty("defaultProvider")
    private JsonNode defaultProvider;
    @JsonProperty("providers")
    private List<NamedProviderConfig> providers;

    @JsonProperty("models")
    private List<ProviderModelConfig> models;

    @JsonProperty("enableCitations")
    private Boolean enableCitations;

    @JsonProperty("capabilities")
    private ModelCapabilitiesOverride capabilities;

    /** Gets the reasoning effort. @return the reasoning effort level */
    public String getReasoningEffort() {
        return reasoningEffort;
    }

    /**
     * Sets the reasoning effort. @param reasoningEffort the reasoning effort level
     */
    public SessionInferenceRequestOptions setReasoningEffort(String reasoningEffort) {
        this.reasoningEffort = reasoningEffort;
        return this;
    }

    /** Gets the reasoning summary mode. @return the reasoning summary mode */
    public String getReasoningSummary() {
        return reasoningSummary;
    }

    /**
     * Sets the reasoning summary mode. @param reasoningSummary the reasoning
     * summary mode
     */
    public SessionInferenceRequestOptions setReasoningSummary(String reasoningSummary) {
        this.reasoningSummary = reasoningSummary;
        return this;
    }

    /** Gets the model multiple-tool-call preference. @return the preference */
    public Boolean getAllowMultipleToolCalls() {
        return allowMultipleToolCalls;
    }

    /**
     * Sets the model multiple-tool-call preference. @param allowMultipleToolCalls
     * the preference
     */
    public SessionInferenceRequestOptions setAllowMultipleToolCalls(boolean allowMultipleToolCalls) {
        this.allowMultipleToolCalls = allowMultipleToolCalls;
        return this;
    }

    /** Gets the context window tier. @return the context window tier */
    public String getContextTier() {
        return contextTier;
    }

    /** Sets the context window tier. @param contextTier the context window tier */
    public SessionInferenceRequestOptions setContextTier(String contextTier) {
        this.contextTier = contextTier;
        return this;
    }

    /**
     * Gets the default provider wire value. @return null if omitted, or the JSON
     * value
     */
    public JsonNode getDefaultProvider() {
        return defaultProvider;
    }

    /**
     * Sets the default connection. @param defaultProvider the name, or null to
     * clear
     */
    public SessionInferenceRequestOptions setDefaultProvider(String defaultProvider) {
        this.defaultProvider = defaultProvider == null ? NullNode.instance : TextNode.valueOf(defaultProvider);
        return this;
    }

    /** Gets the named provider connections. @return the named providers */
    @CopilotExperimental
    public List<NamedProviderConfig> getProviders() {
        return providers;
    }

    /** Sets the named provider connections. @param providers the named providers */
    @CopilotExperimental
    public SessionInferenceRequestOptions setProviders(List<NamedProviderConfig> providers) {
        this.providers = providers;
        return this;
    }

    /** Gets the BYOK model definitions. @return the models */
    @CopilotExperimental
    public List<ProviderModelConfig> getModels() {
        return models;
    }

    /** Sets the BYOK model definitions. @param models the models */
    @CopilotExperimental
    public SessionInferenceRequestOptions setModels(List<ProviderModelConfig> models) {
        this.models = models;
        return this;
    }

    /** Gets enable citations flag. @return the flag */
    public Boolean getEnableCitations() {
        return enableCitations;
    }

    /** Sets enable citations flag. @param enableCitations the flag */
    public SessionInferenceRequestOptions setEnableCitations(boolean enableCitations) {
        this.enableCitations = enableCitations;
        return this;
    }

    /** Gets the model capabilities override. @return the override */
    public ModelCapabilitiesOverride getCapabilities() {
        return capabilities;
    }

    /**
     * Sets the model capabilities override. @param capabilities the override
     */
    public SessionInferenceRequestOptions setCapabilities(ModelCapabilitiesOverride capabilities) {
        this.capabilities = capabilities;
        return this;
    }
}
