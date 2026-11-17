# Copyright (c) Microsoft Corporation. All rights reserved.

from unittest.mock import AsyncMock

import pytest

from copilot import CopilotClient, RuntimeConnection
from copilot._jsonrpc import JsonRpcClient


@pytest.mark.asyncio
@pytest.mark.parametrize("included_builtin_agents", [None, [], ["explore"]])
async def test_create_and_resume_preserve_builtin_agent_allowlist(
    monkeypatch, included_builtin_agents
):
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    rpc = AsyncMock(spec=JsonRpcClient)
    captured = {}

    async def request(method, params, **kwargs):
        captured[method] = params
        result = {"sessionId": params["sessionId"]}
        if callback := kwargs.get("on_response_inline"):
            callback(result)
        return result

    rpc.request.side_effect = request
    monkeypatch.setattr(client, "_client", rpc)
    try:
        session = await client.create_session(
            agents={"included_builtin_agents": included_builtin_agents}
        )
        await client.resume_session(
            session.session_id, agents={"included_builtin_agents": included_builtin_agents}
        )
        for method in ("session.create", "session.resume"):
            if included_builtin_agents is None:
                assert "includedBuiltinAgents" not in captured[method].get("agents", {})
            else:
                assert (
                    captured[method]["agents"]["includedBuiltinAgents"] == included_builtin_agents
                )
    finally:
        await client.force_stop()


@pytest.mark.asyncio
@pytest.mark.parametrize("roots", [None, [], ["relative-root", "other-root"]])
async def test_customization_roots_preserved_on_create_and_resume(monkeypatch, roots):
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    rpc = AsyncMock(spec=JsonRpcClient)
    requests = {}

    async def request(method, params, **kwargs):
        requests[method] = params
        if method in ("session.create", "session.resume"):
            response = {"sessionId": params["sessionId"]}
            if callback := kwargs.get("on_response_inline"):
                callback(response)
            return response
        return {}

    rpc.request.side_effect = request
    monkeypatch.setattr(client, "_client", rpc)
    skills = None if roots is None else [f"{root}-skills" for root in roots]
    agents = None if roots is None else [f"{root}-agents" for root in roots]
    instructions = None if roots is None else [f"{root}-instructions" for root in roots]
    hooks = None if roots is None else [f"{root}-hooks" for root in roots]
    try:
        session = await client.create_session(
            working_directory="request-cwd",
            enable_config_discovery=False,
            hooks={"enable_repository_hooks": False, "directories": hooks},
            skills={"directories": skills},
            agents={"directories": agents},
            instructions={"directories": instructions},
        )
        await client.resume_session(
            session.session_id,
            working_directory="resume-cwd",
            enable_config_discovery=False,
            hooks={"enable_repository_hooks": False, "directories": hooks},
            skills={"directories": skills},
            agents={"directories": agents},
            instructions={"directories": instructions},
        )
        for method in ("session.create", "session.resume"):
            payload = requests[method]
            for group, expected in (
                ("skills", skills),
                ("agents", agents),
                ("instructions", instructions),
                ("hooks", hooks),
            ):
                if expected is None:
                    assert "directories" not in payload.get(group, {})
                else:
                    assert payload[group]["directories"] == expected
            assert payload["enableConfigDiscovery"] is False
            assert payload["hooks"]["enableRepositoryHooks"] is False
            assert "additionalDirectories" not in payload.get("filesystem", {})
            assert "definitions" not in payload.get("agents", {})
            assert "hasHandlers" not in payload.get("hooks", {})
    finally:
        await client.force_stop()
