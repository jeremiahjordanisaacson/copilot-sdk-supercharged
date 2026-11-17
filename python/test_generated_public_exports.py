"""Verify the public shims expose the private generated protocol types."""

import pytest

import copilot
from copilot import rpc, session_events
from copilot._generated import rpc as generated_rpc
from copilot._generated import session_events as generated_session_events


@pytest.mark.parametrize(
    ("public", "generated"),
    [(rpc, generated_rpc), (session_events, generated_session_events)],
    ids=["rpc", "session-events"],
)
def test_generated_exports_are_reexported(public, generated):
    for name in generated.__all__:
        assert getattr(public, name) is getattr(generated, name), name


def test_public_types_are_defined_in_private_package():
    assert rpc.PingRequest.__module__ == "copilot._generated.rpc"
    assert session_events.AssistantMessageData.__module__ == "copilot._generated.session_events"
    assert copilot.SessionEvent is session_events.SessionEvent
    assert not hasattr(rpc, "_InternalServerRpc")
    assert not hasattr(session_events, "_load_PermissionRequest")
