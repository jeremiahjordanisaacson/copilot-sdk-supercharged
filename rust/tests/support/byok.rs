use copilot_sdk::{
    NamedProviderConfig, ProviderConfig, ProviderModelConfig, SessionInferenceOptions,
};

pub(crate) fn inference(
    mut options: SessionInferenceOptions,
    provider: ProviderConfig,
) -> SessionInferenceOptions {
    let mut connection = NamedProviderConfig::new("byok", provider.base_url);
    connection.provider_type = provider.provider_type;
    connection.wire_api = provider.wire_api;
    connection.transport = provider.transport;
    connection.model_provider = provider.model_provider;
    connection.api_key = provider.api_key;
    connection.api_key_command = provider.api_key_command;
    connection.bearer_token = provider.bearer_token;
    connection.bearer_token_provider = provider.bearer_token_provider;
    connection.azure = provider.azure;
    connection.headers = provider.headers;
    let id = provider
        .model_id
        .clone()
        .unwrap_or_else(|| "claude-sonnet-5".into());
    let mut model = ProviderModelConfig::new(id, "byok");
    model.model_id = provider.model_id;
    model.wire_model = provider.wire_model;
    model.max_prompt_tokens = provider.max_prompt_tokens;
    model.max_output_tokens = provider.max_output_tokens;
    options.default_provider = Some(Some("byok".into()));
    options.providers = Some(vec![connection]);
    options.models = Some(vec![model]);
    options
}
