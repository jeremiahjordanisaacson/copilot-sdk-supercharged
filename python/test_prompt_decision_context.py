"""A labeled answer to a user-input, plan-review or elicitation prompt carries its
decision context on the wire; an unlabeled answer carries no ``decisionContext``."""

import asyncio
import json
from unittest.mock import AsyncMock, Mock

import pytest

from copilot import (
    CopilotClient,
    RuntimeConnection,
    UserInputImageAttachment,
    create_attributed_result,
)
from copilot.rpc import (
    PermissionDecisionContext,
    PermissionDecisionOutcome,
    PermissionDecisionSource,
    PermissionDecisionSurface,
    PermissionResponseCapability,
)
from copilot.session import (
    CopilotSession,
    ElicitationContext,
    ElicitationHandlerResult,
    ExitPlanModeRequest,
    ExitPlanModeResult,
    UserInputRequest,
    UserInputResponse,
)
from copilot.session_events import (
    ElicitationCompletedAction,
    ElicitationCompletedData,
    ElicitationRequestedData,
    ElicitationRequestedMode,
    session_event_from_dict,
)

PERSON_ANSWERED = PermissionDecisionContext(
    source=PermissionDecisionSource.HUMAN_RESPONSE,
    response_capability=PermissionResponseCapability.INTERACTIVE,
    surface=PermissionDecisionSurface.SDK,
    outcome=PermissionDecisionOutcome.PROMPTED_USER,
)

PERSON_ANSWERED_ON_WIRE = {
    "source": "human_response",
    "responseCapability": "interactive",
    "surface": "sdk",
    "outcome": "prompted_user",
}

LABELED = pytest.mark.parametrize("labeled", [True, False], ids=["person", "unlabeled"])


def _client_with_session() -> tuple[CopilotClient, CopilotSession]:
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    session = CopilotSession("session-1", Mock())
    client._sessions[session.session_id] = session
    return client, session


def _on_wire(reply: dict) -> dict:
    # The JSON-RPC layer serializes the reply as-is, so it must already be plain JSON.
    return json.loads(json.dumps(reply))


@LABELED
async def test_user_input_reply_carries_decision_context_at_top_level(labeled: bool) -> None:
    client, session = _client_with_session()

    def handler(request: UserInputRequest, invocation: dict[str, str]) -> UserInputResponse:
        answer: UserInputResponse = {"answer": "Only src/app.ts", "wasFreeform": True}
        return create_attributed_result(answer, PERSON_ANSWERED) if labeled else answer

    session._register_user_input_handler(handler)

    reply = await client._handle_user_input_request(
        {"sessionId": session.session_id, "question": "Which files?", "requestId": "input-1"}
    )

    expected: dict = {"answer": "Only src/app.ts", "wasFreeform": True}
    if labeled:
        expected["decisionContext"] = PERSON_ANSWERED_ON_WIRE
    assert _on_wire(reply) == expected


async def test_user_input_image_answer_is_json_serializable() -> None:
    client, session = _client_with_session()

    def handler(request: UserInputRequest, invocation: dict[str, str]) -> UserInputResponse:
        constraints = request["imageAttachmentConstraints"]
        assert constraints.allowed_mime_types == ["image/png"]
        assert constraints.max_count == 1
        assert constraints.max_bytes_per_image == 1024
        return {
            "answer": "",
            "wasFreeform": True,
            "attachments": [
                UserInputImageAttachment(
                    type="image",
                    data="aW1hZ2U=",
                    mime_type="image/png",
                    display_name="Screenshot",
                    description="Requested image",
                )
            ],
        }

    session._register_user_input_handler(handler)
    reply = await client._handle_user_input_request(
        {
            "sessionId": session.session_id,
            "question": "Share a screenshot",
            "imageAttachmentConstraints": {
                "allowedMimeTypes": ["image/png"],
                "maxCount": 1,
                "maxBytesPerImage": 1024,
            },
        }
    )
    assert _on_wire(reply) == {
        "answer": "",
        "wasFreeform": True,
        "attachments": [
            {
                "type": "image",
                "data": "aW1hZ2U=",
                "mimeType": "image/png",
                "displayName": "Screenshot",
                "description": "Requested image",
            }
        ],
    }


@LABELED
async def test_exit_plan_mode_reply_carries_decision_context_at_top_level(labeled: bool) -> None:
    client, session = _client_with_session()

    def handler(request: ExitPlanModeRequest, invocation: dict[str, str]) -> ExitPlanModeResult:
        review: ExitPlanModeResult = {"approved": True, "selectedAction": "interactive"}
        return create_attributed_result(review, PERSON_ANSWERED) if labeled else review

    session._register_exit_plan_mode_handler(handler)

    reply = await client._handle_exit_plan_mode_request(
        {
            "sessionId": session.session_id,
            "summary": "Review the plan",
            "actions": ["interactive", "autopilot"],
            "recommendedAction": "autopilot",
            "requestId": "plan-1",
        }
    )

    expected: dict = {"approved": True, "selectedAction": "interactive"}
    if labeled:
        expected["decisionContext"] = PERSON_ANSWERED_ON_WIRE
    assert _on_wire(reply) == expected


@LABELED
async def test_elicitation_answer_sends_decision_context_beside_result(labeled: bool) -> None:
    rpc_client = Mock(request=AsyncMock(return_value={"success": True}))
    session = CopilotSession("session-1", rpc_client)

    def handler(context: ElicitationContext) -> ElicitationHandlerResult:
        answer: ElicitationHandlerResult = {"action": "accept", "content": {"color": "blue"}}
        return create_attributed_result(answer, PERSON_ANSWERED) if labeled else answer

    session._register_elicitation_handler(handler)

    await session._handle_elicitation_request(
        {"session_id": session.session_id, "message": "Pick a color"}, "elicitation-1"
    )

    expected: dict = {
        "sessionId": "session-1",
        "requestId": "elicitation-1",
        "result": {"action": "accept", "content": {"color": "blue"}},
    }
    if labeled:
        expected["decisionContext"] = PERSON_ANSWERED_ON_WIRE
    rpc_client.request.assert_awaited_once_with("session.ui.handlePendingElicitation", expected)


@LABELED
async def test_elicitation_image_metadata_survives_event_handler_and_rpc(labeled: bool) -> None:
    key = "github.com/copilot/ask-user-images"
    requested_meta = {key: {"version": 1, "fieldNames": ["screenshot"]}}
    answered_meta = {
        key: {
            "version": 1,
            "attachments": {
                "screenshot": [{"type": "image", "data": "aW1hZ2U=", "mimeType": "image/png"}]
            },
        }
    }
    completed = asyncio.get_running_loop().create_future()

    async def request(method: str, params: dict) -> dict:
        completed.set_result((method, params))
        return {"success": True}

    session = CopilotSession("session-1", Mock(request=AsyncMock(side_effect=request)))
    observed_metadata: list[dict] = []

    def redact_event(event) -> None:
        assert isinstance(event.data, ElicitationRequestedData)
        assert event.data._meta is not None
        event.data._meta[key]["fieldNames"][0] = "redacted"
        observed_metadata.append(event.data._meta)

    session.on(redact_event)

    def handler(context: ElicitationContext) -> ElicitationHandlerResult:
        assert context["requestId"] == "image-question"
        assert context["_meta"] == {key: {"version": 1, "fieldNames": ["screenshot"]}}
        context["_meta"][key]["fieldNames"][0] = "callback"
        answer: ElicitationHandlerResult = {
            "action": "accept",
            "content": {"screenshot": ""},
            "_meta": answered_meta,
        }
        return create_attributed_result(answer, PERSON_ANSWERED) if labeled else answer

    session._register_elicitation_handler(handler)
    session._dispatch_event(
        session_event_from_dict(
            {
                "id": "8b28e27d-fca3-4f90-bd7f-0c3f91ec4af1",
                "timestamp": "2026-11-19T00:00:00Z",
                "parentId": None,
                "type": "elicitation.requested",
                "ephemeral": True,
                "data": {
                    "requestId": "image-question",
                    "message": "Share a screenshot",
                    "_meta": requested_meta,
                },
            }
        )
    )
    method, params = await asyncio.wait_for(completed, timeout=2)
    expected = {
        "sessionId": "session-1",
        "requestId": "image-question",
        "result": {
            "action": "accept",
            "content": {"screenshot": ""},
            "_meta": answered_meta,
        },
    }
    if labeled:
        expected["decisionContext"] = PERSON_ANSWERED_ON_WIRE
    assert method == "session.ui.handlePendingElicitation"
    assert params == expected
    assert observed_metadata == [{key: {"version": 1, "fieldNames": ["redacted"]}}]


@pytest.mark.asyncio
@pytest.mark.parametrize("metadata", [None, {}, {"example.com/form": {"receipt": ["saved"]}}])
async def test_public_elicitation_preserves_application_result_metadata(metadata) -> None:
    wire_result = {"action": "accept", "content": {"name": "Ada"}}
    if metadata is not None:
        wire_result["_meta"] = metadata
    rpc = Mock(request=AsyncMock(return_value=wire_result))
    session = CopilotSession("session-1", rpc)
    session._set_capabilities({"ui": {"elicitation": True}})

    result = await session.ui.elicitation(
        {"message": "Name?", "requestedSchema": {"type": "object", "properties": {}}}
    )

    assert result == wire_result
    assert rpc.request.await_args.args[0] == "session.ui.elicitation"


def test_text_elicitation_positional_event_constructors_keep_their_wire_shape() -> None:
    assert ElicitationRequestedData(
        "Question",
        "request",
        "source",
        "Source",
        ElicitationRequestedMode.FORM,
        None,
        "tool",
        "https://example.com",
    ).to_dict() == {
        "message": "Question",
        "requestId": "request",
        "elicitationSource": "source",
        "elicitationSourceDisplayName": "Source",
        "mode": "form",
        "toolCallId": "tool",
        "url": "https://example.com",
    }
    assert ElicitationCompletedData(
        "request", ElicitationCompletedAction.ACCEPT, {"text": "answer"}
    ).to_dict() == {
        "requestId": "request",
        "action": "accept",
        "content": {"text": "answer"},
    }
