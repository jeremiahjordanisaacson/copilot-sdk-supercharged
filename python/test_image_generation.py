# Copyright (c) Microsoft Corporation. All rights reserved.

"""Image generation request serialization without starting a runtime."""

import json
from unittest.mock import AsyncMock, Mock

import pytest

from copilot import CopilotClient, ImageGenerationConfig, PermissionHandler, RuntimeConnection


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
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path="unused-cli")
    )
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
            permissions={"on_request": PermissionHandler.approve_all},
            tools={"image_generation": option},
        )
        await client.resume_session(
            session.session_id,
            permissions={"on_request": PermissionHandler.approve_all},
            tools={"image_generation": option},
        )
        for method in ("session.create", "session.resume"):
            if option is None:
                assert "imageGeneration" not in captured[method].get("tools", {}), captured[method]
            else:
                expected = {
                    "saveToDisk" if key == "save_to_disk" else key: value
                    for key, value in option.items()
                }
                assert captured[method]["tools"]["imageGeneration"] == expected
    finally:
        await client.force_stop()
