import pytest

from copilot import CopilotClient, PermissionHandler, RuntimeConnection
from copilot.client import InProcessRuntimeConnection, SpawnedStdioRuntimeConnection


class GroupedRuntime:
    def __init__(self):
        self.requests = []

    async def request(self, method, params, **kwargs):
        self.requests.append((method, params))
        result = (
            {"sessionId": params.get("sessionId", "grouped")}
            if method in ("session.create", "session.resume")
            else {"success": True}
        )
        if callback := kwargs.get("on_response_inline"):
            callback(result)
        return result


@pytest.mark.asyncio
@pytest.mark.parametrize("resume", [False, True])
async def test_grouped_options_preserve_wire_values_and_callback_ownership(resume):
    runtime = GroupedRuntime()
    client = CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1))
    client._client = runtime
    options = {
        "tools": {"definitions": [], "available_tools": []},
        "events": {"streaming": False},
        "filesystem": {"additional_directories": [], "track_file_changes": False},
        "mcp": {"servers": {}, "disabled_servers": []},
        "experiments": {"flags": {}, "enable_experimental_mode": False},
        "permissions": {"on_request": PermissionHandler.approve_all},
        "interaction": {"on_shell_authentication_request": lambda *_: {"action": "cancel"}},
        "github": {"token": ""},
        "inference": {"allow_multiple_tool_calls": False},
    }
    session = (
        await client.resume_session("grouped", suppress_resume_event=True, **options)
        if resume
        else await client.create_session(session_id="grouped", **options)
    )
    try:
        method, payload = runtime.requests[0]
        assert method == ("session.resume" if resume else "session.create")
        assert payload["tools"] == {"definitions": [], "availableTools": []}
        assert payload["events"]["streaming"] is False
        assert payload["filesystem"] == {"additionalDirectories": [], "trackFileChanges": False}
        assert payload["mcp"] == {"servers": {}, "disabledServers": [], "envValueMode": "direct"}
        assert payload["experiments"] == {"flags": {}, "enableExperimentalMode": False}
        assert payload["permissions"] == {"request": True}
        assert payload["interaction"]["requestShellAuthentication"] is True
        assert payload["github"] == {"token": ""}
        assert payload["inference"] == {"allowMultipleToolCalls": False}
        assert "requestPermission" not in payload
        assert "streaming" not in payload
        assert "gitHubToken" not in payload
        assert "disableResume" not in payload
        if resume:
            assert payload["suppressResumeEvent"] is True
        assert options["mcp"]["servers"] == {}
    finally:
        await session.disconnect()


def test_default_runtime_resolves_at_invocation_and_only_accepts_owned_settings(monkeypatch):
    monkeypatch.setenv("COPILOT_SDK_DEFAULT_CONNECTION", "StDiO")
    first = RuntimeConnection.for_default(base_directory="first", session_idle_timeout_seconds=0)
    monkeypatch.setenv("COPILOT_SDK_DEFAULT_CONNECTION", "INPROCESS")
    second = RuntimeConnection.for_default(base_directory="second", session_idle_timeout_seconds=12)
    assert isinstance(first, SpawnedStdioRuntimeConnection)
    assert isinstance(second, InProcessRuntimeConnection)
    assert (first.base_directory, first.session_idle_timeout_seconds) == ("first", 0)
    assert (second.base_directory, second.session_idle_timeout_seconds) == ("second", 12)
    monkeypatch.setenv("COPILOT_SDK_DEFAULT_CONNECTION", "uri")
    with pytest.raises(ValueError, match="COPILOT_SDK_DEFAULT_CONNECTION"):
        RuntimeConnection.for_default()
    with pytest.raises(TypeError, match="env"):
        RuntimeConnection.for_inprocess(env={})


@pytest.mark.asyncio
async def test_grouped_surface_rejects_flat_options():
    with pytest.raises(TypeError, match="log_level"):
        CopilotClient(mode="coding-agent", log_level="debug")
    client = CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1))
    with pytest.raises(TypeError, match="on_permission_request"):
        await client.create_session(on_permission_request=PermissionHandler.approve_all)
    with pytest.raises(TypeError, match="Unknown hooks"):
        await client.create_session(hooks={"on_session_start": lambda *_: None})


@pytest.mark.asyncio
async def test_protocol_three_is_rejected_before_grouped_session_requests():
    runtime = GroupedRuntime()

    async def request(method, params, **kwargs):
        runtime.requests.append((method, params))
        assert method == "connect"
        return {"ok": True, "protocolVersion": 3, "version": "old"}

    runtime.request = request
    client = CopilotClient(mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1))
    client._client = runtime
    with pytest.raises(RuntimeError, match=r"protocol version mismatch.*4-4.*3"):
        await client._verify_protocol_version()
    assert [method for method, _ in runtime.requests] == ["connect"]
