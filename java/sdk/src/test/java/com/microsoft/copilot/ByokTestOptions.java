package com.microsoft.copilot;

import java.util.List;
import com.microsoft.copilot.rpc.*;

final class ByokTestOptions {
    private ByokTestOptions() {
    }

    static SessionInferenceOptions inference(ProviderConfig provider) {
        return inference(new SessionInferenceOptions(), provider);
    }

    static SessionInferenceOptions inference(SessionInferenceOptions options, ProviderConfig provider) {
        var connection = new NamedProviderConfig().setName("byok").setType(provider.getType())
                .setBaseUrl(provider.getBaseUrl()).setWireApi(provider.getWireApi())
                .setTransport(provider.getTransport()).setModelProvider(provider.getModelProvider())
                .setApiKey(provider.getApiKey()).setApiKeyCommand(provider.getApiKeyCommand())
                .setBearerToken(provider.getBearerToken()).setBearerTokenProvider(provider.getBearerTokenProvider())
                .setAzure(provider.getAzure()).setHeaders(provider.getHeaders());
        var id = provider.getModelId() == null ? "claude-sonnet-5" : provider.getModelId();
        var model = new ProviderModelConfig().setId(id).setProvider("byok").setModelId(provider.getModelId())
                .setWireModel(provider.getWireModel());
        provider.getMaxPromptTokens().ifPresent(model::setMaxPromptTokens);
        provider.getMaxOutputTokens().ifPresent(model::setMaxOutputTokens);
        return options.setDefaultProvider("byok").setProviders(List.of(connection)).setModels(List.of(model));
    }
}
