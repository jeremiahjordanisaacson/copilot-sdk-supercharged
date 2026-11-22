# Copyright (c) Microsoft Corporation. All rights reserved.

"""Representative generated RPC round trips over the real SDK transport."""

import json

import pytest

from copilot import CopilotClient, RuntimeConnection
from copilot._generated.rpc import _InternalServerRpc, _UserSettingsUpdateRequest
from copilot.rpc import PingRequest, UserSettingsNormalizeRequest, UserSettingsSetRequest
from copilot.session import PermissionHandler
from copilot.session_events import SessionMode

from .testharness import CLI_PATH

pytestmark = pytest.mark.asyncio(loop_scope="module")


@pytest.mark.parametrize("method", ["set", "update"])
async def test_settings_write_rejects_oversized_integer_without_a_write(
    tmp_path, monkeypatch, method
):
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("COPILOT_HOME", str(home))
    settings_file = home / "settings.json"
    original = '{"statusLine":{"command":"echo before","padding":3}}'
    settings_file.write_text(original)
    value = {"command": "echo after", "padding": 2**64 + 1}
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path=CLI_PATH)
    )

    async def write():
        if method == "set":
            await client.rpc.user.settings.set(
                UserSettingsSetRequest(settings={"statusLine": value})
            )
        else:
            await _InternalServerRpc(client._client).user.settings._update(
                _UserSettingsUpdateRequest(key="statusLine", value=value, config_dir=str(home))
            )

    try:
        await client.start()
        with pytest.raises(ValueError, match="integer.*range"):
            await write()
        assert settings_file.read_text() == original
        assert value == {"command": "echo after", "padding": 2**64 + 1}

        value["padding"] = 2**64 - 1
        await write()
        assert json.loads(settings_file.read_text())["statusLine"] == value
    finally:
        await client.force_stop()


async def test_settings_update_rejects_oversized_integer_without_a_write(tmp_path, monkeypatch):
    monkeypatch.setenv("COPILOT_HOME", str(tmp_path / "home"))
    config_dir = tmp_path / "target"
    config_dir.mkdir()
    settings_file = config_dir / "settings.json"
    original = '{"statusLine":{"command":"echo before","padding":-9223372036854775808}}'
    settings_file.write_text(original)
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path=CLI_PATH)
    )
    try:
        await client.start()
        settings = _InternalServerRpc(client._client).user.settings
        with pytest.raises(ValueError, match="integer.*range"):
            await settings._update(
                _UserSettingsUpdateRequest(
                    key="statusLine",
                    value={"command": "echo after"},
                    config_dir=str(config_dir),
                    if_fields_match={"padding": -(2**63) - 1},
                )
            )
        assert settings_file.read_text() == original

        result = await settings._update(
            _UserSettingsUpdateRequest(
                key="statusLine",
                value={"command": "echo after"},
                config_dir=str(config_dir),
                if_fields_match={"padding": -(2**63)},
            )
        )
        assert result.applied is True
        assert json.loads(settings_file.read_text())["statusLine"] == {
            "command": "echo after",
            "padding": -(2**63),
        }
    finally:
        await client.force_stop()


async def test_settings_normalize_rejects_oversized_integer_before_conditional_write(
    tmp_path, monkeypatch
):
    monkeypatch.setenv("COPILOT_HOME", str(tmp_path / "home"))
    config_dir = tmp_path / "target"
    config_dir.mkdir()
    settings_file = config_dir / "settings.json"
    original = '{"statusLine":{"command":"echo before","padding":-9223372036854775808}}'
    settings_file.write_text(original)
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path=CLI_PATH)
    )
    value = {"status_line": {"padding": -(2**63) - 1}}
    try:
        await client.start()
        settings = _InternalServerRpc(client._client).user.settings
        with pytest.raises(ValueError, match="integer.*range"):
            await settings._normalize(UserSettingsNormalizeRequest(settings=value))
        assert value == {"status_line": {"padding": -(2**63) - 1}}
        assert settings_file.read_text() == original

        supported = {
            "status_line": {"padding": -(2**63)},
            "custom": [2**64 - 1, 1e20, "-9223372036854775809", None, True],
        }
        normalized = await settings._normalize(UserSettingsNormalizeRequest(settings=supported))
        assert normalized.settings == {
            "statusLine": {"padding": -(2**63)},
            "custom": supported["custom"],
        }
        assert supported["status_line"] == {"padding": -(2**63)}
        result = await settings._update(
            _UserSettingsUpdateRequest(
                key="statusLine",
                value={"command": "echo after"},
                config_dir=str(config_dir),
                if_fields_match=normalized.settings["statusLine"],
            )
        )
        assert result.applied is True
        assert json.loads(settings_file.read_text())["statusLine"] == {
            "command": "echo after",
            "padding": -(2**63),
        }
    finally:
        await client.force_stop()


async def test_generated_rpc_round_trip():
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path=CLI_PATH)
    )
    try:
        await client.start()

        pong = await client.rpc.ping(PingRequest(message="typed rpc test"))
        assert pong.message == "pong: typed rpc test"
        assert pong.timestamp is not None

        session = await client.create_session(
            permissions={"on_request": PermissionHandler.approve_all}
        )
        try:
            assert await session.rpc.mode.get() == SessionMode.INTERACTIVE
        finally:
            await session.disconnect()
    finally:
        await client.force_stop()
