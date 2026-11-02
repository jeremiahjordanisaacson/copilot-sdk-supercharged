# Copyright (c) Microsoft Corporation. All rights reserved.

"""Image generation request serialization without starting a runtime."""

import json
from unittest.mock import AsyncMock, Mock

import pytest

from copilot import CopilotClient, ImageGenerationConfig, PermissionHandler, RuntimeConnection
from copilot.rpc import ProtocolImageGenerationOptions


@pytest.mark.parametrize("enabled", [None, False, True])
@pytest.mark.parametrize("watermark", [None, False, True])
def test_protocol_image_generation_preserves_positional_and_keyword_arguments(
    enabled: bool | None, watermark: bool | None
):
    positional = ProtocolImageGenerationOptions(enabled, watermark)
    keyword = ProtocolImageGenerationOptions(enabled=enabled, watermark=watermark)
    expected = {
        key: value
        for key, value in {"enabled": enabled, "watermark": watermark}.items()
        if value is not None
    }
    assert positional == keyword
    assert positional.to_dict() == expected
    assert ProtocolImageGenerationOptions.from_dict(expected) == positional


@pytest.mark.parametrize("enabled", [None, False, True])
def test_protocol_image_generation_preserves_one_argument_constructor(enabled: bool | None):
    options = ProtocolImageGenerationOptions(enabled)
    assert options.enabled is enabled
    assert options.watermark is None
    assert options.save_to_disk is None
    assert options.to_dict() == ({} if enabled is None else {"enabled": enabled})


@pytest.mark.parametrize("enabled", [None, False, True])
@pytest.mark.parametrize("watermark", [None, False, True])
@pytest.mark.parametrize("save_to_disk", [None, False, True])
def test_protocol_image_generation_appends_save_to_disk(
    enabled: bool | None, watermark: bool | None, save_to_disk: bool | None
):
    positional = ProtocolImageGenerationOptions(enabled, watermark, save_to_disk)
    keyword = ProtocolImageGenerationOptions(
        enabled=enabled, watermark=watermark, save_to_disk=save_to_disk
    )
    expected = {
        key: value
        for key, value in {
            "enabled": enabled,
            "watermark": watermark,
            "saveToDisk": save_to_disk,
        }.items()
        if value is not None
    }
    assert positional == keyword
    assert positional.to_dict() == expected
    assert ProtocolImageGenerationOptions.from_dict(expected) == positional


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "option",
    [
        None,
        {},
        {"enabled": True},
        {"enabled": False},
        {"watermark": True},
        {"watermark": False},
        {"enabled": False, "watermark": True},
        {"enabled": True, "watermark": False},
        {"save_to_disk": True},
        {"save_to_disk": False},
        {"enabled": True, "watermark": False, "save_to_disk": True},
    ],
)
async def test_image_generation_create_and_resume_wire(option: ImageGenerationConfig | None):
    client = CopilotClient(connection=RuntimeConnection.for_stdio(path="unused-cli"))
    client._state = "connected"
    transport = Mock()
    captured = {}

    async def request(method, params, **kwargs):
        captured[method] = json.loads(json.dumps(params))
        response = {"sessionId": params["sessionId"]}
        if callback := kwargs.get("on_response_inline"):
            callback(response)
        return response

    transport.request = AsyncMock(side_effect=request)
    transport.stop = AsyncMock()
    client._client = transport
    try:
        session = await client.create_session(
            on_permission_request=PermissionHandler.approve_all,
            image_generation=option,
        )
        await client.resume_session(
            session.session_id,
            on_permission_request=PermissionHandler.approve_all,
            image_generation=option,
        )
        for method in ("session.create", "session.resume"):
            if option is None:
                assert "imageGeneration" not in captured[method], captured[method]
            else:
                expected = {
                    "saveToDisk" if key == "save_to_disk" else key: value
                    for key, value in option.items()
                }
                assert captured[method]["imageGeneration"] == expected
    finally:
        await client.force_stop()
