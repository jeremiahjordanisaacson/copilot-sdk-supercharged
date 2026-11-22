namespace Microsoft.Copilot.Test;

internal static class ByokTestOptions
{
    internal static SessionInferenceOptions Inference(ProviderConfig provider, SessionInferenceOptions? options = null, string? model = null)
    {
        options ??= new();
        options.DefaultProvider = "byok";
        options.Providers = [new NamedProviderConfig
        {
            Name = "byok", Type = provider.Type, WireApi = provider.WireApi, Transport = provider.Transport,
            ModelProvider = provider.ModelProvider, BaseUrl = provider.BaseUrl, ApiKey = provider.ApiKey,
            ApiKeyCommand = provider.ApiKeyCommand, BearerToken = provider.BearerToken,
            BearerTokenProvider = provider.BearerTokenProvider, Azure = provider.Azure, Headers = provider.Headers,
        }];
        options.Models = [new ProviderModelConfig
        {
            Id = model ?? provider.ModelId ?? "claude-sonnet-5", Provider = "byok", ModelId = provider.ModelId,
            WireModel = provider.WireModel, MaxPromptTokens = provider.MaxPromptTokens,
            MaxOutputTokens = provider.MaxOutputTokens,
        }];
        return options;
    }
}
