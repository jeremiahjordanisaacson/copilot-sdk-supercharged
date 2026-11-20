from datetime import datetime
from pathlib import Path

import pytest

import copilot._generated.session_events as generated_events
from copilot.session_events import SessionLimitsConfig, SessionResumeData, Verbosity


def test_complete_generated_event_module_compiles():
    source = Path(generated_events.__file__)
    compile(source.read_text(encoding="utf-8"), str(source), "exec")


@pytest.mark.parametrize("kept,overridden", [(None, None), (False, None), (True, True)])
def test_resume_reasoning_outcomes_preserve_other_wire_fields(kept, overridden):
    resumed_at = datetime(2026, 1, 1)
    limits = SessionLimitsConfig(max_ai_credits=12.5)
    data = SessionResumeData(
        event_count=7,
        resume_time=resumed_at,
        selected_model="selected-model",
        session_limits=limits,
        session_was_active=True,
        verbosity=Verbosity.LOW,
    )

    assert data.selected_model == "selected-model"
    assert data.session_limits is limits
    assert data.session_was_active is True
    assert data.verbosity is Verbosity.LOW
    assert data.responses_reasoning_kept is None
    assert data.responses_reasoning_overridden is None
    data.responses_reasoning_kept = kept
    data.responses_reasoning_overridden = overridden
    expected = {
        "eventCount": 7,
        "resumeTime": resumed_at.isoformat(),
        "selectedModel": "selected-model",
        "sessionLimits": {"maxAiCredits": 12.5},
        "sessionWasActive": True,
        "verbosity": "low",
    }
    if kept is not None:
        expected["responsesReasoningKept"] = kept
    if overridden is not None:
        expected["responsesReasoningOverridden"] = overridden
    assert data.to_dict() == expected
    assert SessionResumeData.from_dict(expected) == data
