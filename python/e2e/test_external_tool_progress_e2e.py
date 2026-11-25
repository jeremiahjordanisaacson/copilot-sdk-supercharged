"""E2E tests for tool progress reported from a native Python tool handler."""

from __future__ import annotations

import pickle

import pytest

from copilot._generated.rpc import ToolsExecuteRequest
from copilot._generated.session_events import ToolExecutionProgressData
from copilot.session import PermissionHandler
from copilot.tools import Tool, ToolInvocation, ToolResult

from .testharness import E2ETestContext, wait_for_condition

pytestmark = pytest.mark.asyncio(loop_scope="module")


class TestExternalToolProgress:
    async def test_reports_progress_only_from_the_original_of_a_pickled_invocation(
        self, ctx: E2ETestContext
    ):
        originals: list[ToolInvocation] = []
        restored: list[ToolInvocation] = []

        async def handler(invocation: ToolInvocation) -> ToolResult:
            copy = pickle.loads(pickle.dumps(invocation))
            originals.append(invocation)
            restored.append(copy)
            await copy.report_progress("from the restored copy")
            await invocation.report_progress("from the original")
            return ToolResult(text_result_for_llm="PICKLE_PROBE_DONE")

        session = await ctx.client.create_session(
            permissions={"on_request": PermissionHandler.approve_all},
            tools={"definitions": [Tool("pickle_probe", "Pickle the invocation", handler=handler)]},
        )
        progress: list[ToolExecutionProgressData] = []
        unsubscribe = session.on(
            lambda event: (
                progress.append(event.data)
                if isinstance(event.data, ToolExecutionProgressData)
                else None
            )
        )
        try:
            await session.rpc.tools.initialize_and_validate()
            result = await session.rpc.tools.execute(
                ToolsExecuteRequest(
                    name="pickle_probe", arguments={"path": "src/a.py"}, tool_call_id="pickle-call"
                )
            )

            assert isinstance(result, dict)
            assert result["resultType"] == "success"
            assert result["textResultForLlm"] == "PICKLE_PROBE_DONE"
            assert len(restored) == 1
            # Dataclass equality covers every field, so a pickle that drops the
            # arguments or the pending request id fails here.
            assert restored[0] == originals[0]
            assert restored[0].arguments == {"path": "src/a.py"}
            assert restored[0].request_id
            assert restored[0].tool_call_id == "pickle-call"
            assert restored[0].tool_name == "pickle_probe"
            assert restored[0].session_id == session.session_id
            await wait_for_condition(
                lambda: len(progress) > 0,
                timeout=30.0,
                timeout_message="Timed out waiting for tool.execution_progress.",
            )
            assert [(item.tool_call_id, item.progress_message) for item in progress] == [
                ("pickle-call", "from the original")
            ]
        finally:
            unsubscribe()
            await session.disconnect()
