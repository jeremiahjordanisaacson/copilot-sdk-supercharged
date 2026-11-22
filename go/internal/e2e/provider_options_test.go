package e2e

import copilot "github.com/microsoft/copilot-sdk/go/v2"

func byokInference(provider *copilot.ProviderConfig, options copilot.SessionInferenceOptions) copilot.SessionInferenceOptions {
	name := "byok"
	defaultProvider := &name
	options.DefaultProvider = &defaultProvider
	options.Providers = []copilot.NamedProviderConfig{{
		Name: name, Type: provider.Type, WireAPI: provider.WireAPI, Transport: provider.Transport,
		ModelProvider: provider.ModelProvider, BaseURL: provider.BaseURL, APIKey: provider.APIKey,
		APIKeyCommand: provider.APIKeyCommand, BearerToken: provider.BearerToken,
		BearerTokenProvider: provider.BearerTokenProvider, Azure: provider.Azure, Headers: provider.Headers,
	}}
	model := provider.ModelID
	if model == "" {
		model = "claude-sonnet-5"
	}
	options.Models = []copilot.ProviderModelConfig{{
		ID: model, Provider: name, ModelID: provider.ModelID, WireModel: provider.WireModel,
		MaxPromptTokens: provider.MaxPromptTokens, MaxOutputTokens: provider.MaxOutputTokens,
	}}
	return options
}
