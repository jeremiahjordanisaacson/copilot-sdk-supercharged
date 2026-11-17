from typing import Any


def byok_inference(provider: dict[str, Any], model: str | None = None) -> dict[str, Any]:
    model_fields = {
        "model_id",
        "wire_model",
        "max_prompt_tokens",
        "max_context_window_tokens",
        "max_output_tokens",
        "capabilities",
    }
    connection = {key: value for key, value in provider.items() if key not in model_fields}
    row = {key: value for key, value in provider.items() if key in model_fields}
    return {
        "default_provider": "byok",
        "providers": [{"name": "byok", **connection}],
        "models": [
            {
                "id": model or provider.get("model_id") or "claude-sonnet-5",
                "provider": "byok",
                **row,
            }
        ],
    }
