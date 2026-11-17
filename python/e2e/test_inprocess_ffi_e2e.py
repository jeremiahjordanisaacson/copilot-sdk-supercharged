"""E2E smoke test for the in-process (FFI) transport.

Starts a client over the in-process FFI transport, performs a ``ping``
round-trip through the native runtime library, and stops cleanly. Covers both
the default with no environment override and explicit connection selection.

Mirrors nodejs/test/e2e/inprocess_ffi.e2e.test.ts.
"""

from __future__ import annotations

import asyncio
import os

import pytest

from copilot.client import CopilotClient, RuntimeConnection, _InternalTestOptions

from .testharness import E2ETestContext

pytestmark = pytest.mark.asyncio(loop_scope="module")


class TestInProcessFfi:
    async def test_independent_hosts_overlap_and_survive_sibling_shutdown(self, ctx, tmp_path):
        original_environment = os.environ.copy()
        original_cwd = os.getcwd()
        for side in ("first", "second"):
            await ctx.set_copilot_user_by_token(
                f"ffi-{side}-token",
                {
                    "login": side,
                    "copilot_plan": "individual_pro",
                    "endpoints": {"api": ctx.proxy_url, "telemetry": ctx.proxy_url},
                },
            )
        clients = [
            ctx.create_inprocess_client(
                mode="coding-agent",
                _test_options=_InternalTestOptions(
                    copilot_api_url=ctx.proxy_url, github_api_url=ctx.proxy_url
                ),
                runtime=RuntimeConnection.for_inprocess(base_directory=str(tmp_path / side)),
                github={"token": f"ffi-{side}-token", "use_logged_in_user": False},
            )
            for side in ("first", "second")
        ]
        try:
            await asyncio.gather(*(client.start() for client in clients))
            statuses = await asyncio.gather(*(client.get_auth_status() for client in clients))
            assert all(status.is_authenticated for status in statuses)
            sessions = await asyncio.gather(
                *(client.create_session(working_directory=ctx.work_dir) for client in clients)
            )
            identities = await asyncio.gather(
                *(session.rpc.git_hub_auth.get_status() for session in sessions)
            )
            assert [identity.login for identity in identities] == ["first", "second"]
            for side, session in zip(("first", "second"), sessions, strict=True):
                assert session.workspace_path == (
                    tmp_path / side / "session-state" / session.session_id
                )
            replies = await asyncio.gather(
                *(client.ping(str(index)) for index, client in enumerate(clients))
            )
            assert [reply.message for reply in replies] == ["pong: 0", "pong: 1"]
            await clients[0].stop()
            assert (await clients[1].ping("survivor")).message == "pong: survivor"
            assert (await sessions[1].rpc.git_hub_auth.get_status()).login == "second"
            assert os.environ == original_environment
            assert os.getcwd() == original_cwd
        finally:
            await asyncio.gather(*(client.force_stop() for client in clients))

    @pytest.mark.parametrize("explicit", [False, True], ids=["default", "explicit"])
    @pytest.mark.parametrize("shutdown_method", ["stop", "force_stop"])
    async def test_should_start_and_connect_over_in_process_ffi(
        self, ctx: E2ETestContext, shutdown_method: str, explicit: bool, monkeypatch
    ):
        # In-process hosting loads runtime.node directly. ``ping`` is a purely local
        # RPC round-trip, so no auth or replay proxy is involved.
        with monkeypatch.context() as environment:
            environment.delenv("COPILOT_SDK_DEFAULT_CONNECTION", raising=False)
            client = CopilotClient(
                mode="coding-agent",
                runtime=RuntimeConnection.for_inprocess() if explicit else None,
            )
        await client.start()

        try:
            assert client._ffi_host is not None
            pong = await client.ping("ffi message")
            assert pong.message == "pong: ffi message"
            assert pong.timestamp is not None
        finally:
            await getattr(client, shutdown_method)()
