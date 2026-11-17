"""Model capability overrides must use the runtime schema's wire names."""

from unittest.mock import AsyncMock, Mock

import pytest

from copilot import (
    CopilotClient,
    Model,
    ModelCapabilitiesLimits,
    ModelCapabilitiesOverride,
    ModelCapabilitiesOverrideLimits,
    RuntimeConnection,
)
from copilot.session import CopilotSession, PermissionHandler


def test_listed_model_limits_round_trip_max_output_tokens():
    wire_limits = {
        "max_prompt_tokens": 128_000,
        "max_output_tokens": 16_384,
        "max_context_window_tokens": 144_384,
    }
    model = Model.from_dict(
        {"id": "m", "name": "M", "capabilities": {"supports": {}, "limits": wire_limits}}
    )

    assert model.capabilities.limits.max_output_tokens == 16_384
    assert model.capabilities.limits.to_dict() == wire_limits
    assert ModelCapabilitiesLimits.from_dict({"max_context_window_tokens": 1}).to_dict() == {
        "max_context_window_tokens": 1
    }


@pytest.mark.parametrize("operation", ["create", "resume", "set_model"])
@pytest.mark.asyncio
async def test_session_operations_forward_capability_limits(operation):
    async def request(method, params, **kwargs):
        response = (
            {"sessionId": params["sessionId"]}
            if method in ("session.create", "session.resume")
            else {}
        )
        if callback := kwargs.get("on_response_inline"):
            callback(response)
        return response

    rpc = Mock(request=AsyncMock(side_effect=request))
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    client._client = rpc
    client._state = "connected"
    capabilities = ModelCapabilitiesOverride(
        limits=ModelCapabilitiesOverrideLimits(max_context_window_tokens=1_048_576)
    )

    if operation == "set_model":
        session = CopilotSession("capability-session", rpc)
        await session.set_model("custom-model", model_capabilities=capabilities)
        method = "session.model.switchTo"
    else:
        config = {
            "inference": {"capabilities": capabilities},
            "permissions": {"on_request": PermissionHandler.approve_all},
        }
        if operation == "create":
            await client.create_session(session_id="capability-session", **config)
        else:
            await client.resume_session("capability-session", **config)
        method = f"session.{operation}"

    payload = next(call.args[1] for call in rpc.request.await_args_list if call.args[0] == method)
    override = (
        payload["modelCapabilities"]
        if operation == "set_model"
        else payload["inference"]["capabilities"]
    )
    assert override == {"limits": {"max_context_window_tokens": 1_048_576}}


@pytest.mark.parametrize("operation", ["create", "resume"])
@pytest.mark.parametrize("context_limit", [None, 0, 1_048_576])
@pytest.mark.asyncio
async def test_session_operations_forward_provider_context_limit(operation, context_limit):
    async def request(method, params, **kwargs):
        response = {"sessionId": params["sessionId"]}
        if callback := kwargs.get("on_response_inline"):
            callback(response)
        return response

    rpc = Mock(request=AsyncMock(side_effect=request))
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    client._client = rpc
    client._state = "connected"
    model = {"id": "custom", "provider": "example", "max_prompt_tokens": 900_000}
    if context_limit is not None:
        model["max_context_window_tokens"] = context_limit
    config = {
        "model": "example/custom",
        "github": {"capi": {"enabled": False}},
        "inference": {
            "providers": [{"name": "example", "base_url": "https://provider.invalid/v1"}],
            "models": [model],
        },
        "permissions": {"on_request": PermissionHandler.approve_all},
    }

    if operation == "create":
        await client.create_session(session_id="provider-session", **config)
    else:
        await client.resume_session("provider-session", **config)

    payload = next(
        call.args[1]
        for call in rpc.request.await_args_list
        if call.args[0] == f"session.{operation}"
    )
    expected = {"id": "custom", "provider": "example", "maxPromptTokens": 900_000}
    if context_limit is not None:
        expected["maxContextWindowTokens"] = context_limit
    assert payload["inference"]["models"][0] == expected


def test_named_model_capabilities_use_the_same_wire_contract():
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    capabilities = ModelCapabilitiesOverride(
        limits=ModelCapabilitiesOverrideLimits(max_context_window_tokens=1_048_576)
    )
    wire = client._convert_model_to_wire_format(
        {"id": "custom", "provider": "example", "capabilities": capabilities}
    )

    assert wire == {
        "id": "custom",
        "provider": "example",
        "capabilities": {"limits": {"max_context_window_tokens": 1_048_576}},
    }


@pytest.mark.parametrize("operation", ["create", "resume"])
@pytest.mark.asyncio
async def test_session_operations_preserve_local_model_metadata(operation):
    async def request(method, params, **kwargs):
        response = {"sessionId": params["sessionId"]}
        if callback := kwargs.get("on_response_inline"):
            callback(response)
        return response

    rpc = Mock(request=AsyncMock(side_effect=request))
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    client._client = rpc
    client._state = "connected"
    annotation = {"source": "azureml", "future": [None, True, 9007199254740993]}
    metadata = {"autoLocal": annotation, "future": {"enabled": False}}
    config = {
        "inference": {
            "models": [
                {
                    "id": "model",
                    "provider": "local",
                    "auto_local": annotation,
                    "metadata": metadata,
                }
            ]
        },
        "permissions": {"on_request": PermissionHandler.approve_all},
    }

    if operation == "create":
        await client.create_session(session_id="local-model-session", **config)
    else:
        await client.resume_session("local-model-session", **config)

    payload = next(
        call.args[1]
        for call in rpc.request.await_args_list
        if call.args[0] == f"session.{operation}"
    )
    assert payload["inference"]["models"] == [
        {"id": "model", "provider": "local", "autoLocal": annotation, "metadata": metadata}
    ]
