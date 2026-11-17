import re
import types

from copilot import rpc, session_events
from copilot._generated.rpc import (
    _load_AuthEnumerateValue,
    _load_PermissionDecisionApproveForLocationApproval,
    _load_PermissionDecisionApproveForSessionApproval,
)
from copilot._generated.session_events import _load_AssistedApprovalAssessment


def test_permission_approval_exports_are_union_aliases():
    approval_exports = [
        name
        for name in rpc.__all__
        if re.fullmatch(r"PermissionDecisionApproveFor.*Approval", name)
    ]
    assert approval_exports

    for name in approval_exports:
        exported = getattr(rpc, name)
        assert isinstance(exported, types.UnionType), (
            f"{name} must be a union alias, not a synthetic dataclass"
        )


def test_permission_approval_union_loaders_deserialize_expected_variants():
    session = _load_PermissionDecisionApproveForSessionApproval(
        {"kind": "commands", "commandIdentifiers": ["git status"]}
    )
    location = _load_PermissionDecisionApproveForLocationApproval({"kind": "read"})

    assert isinstance(session, rpc.PermissionDecisionApproveForSessionApprovalCommands)
    assert isinstance(location, rpc.PermissionDecisionApproveForLocationApprovalRead)


def test_assisted_approval_assessment_is_a_discriminated_union():
    assert isinstance(session_events.AssistedApprovalAssessment, types.UnionType)

    assessment = _load_AssistedApprovalAssessment(
        {
            "status": "completed",
            "recommendation": "requireApproval",
            "rationale": "The command modifies protected files.",
        }
    )

    assert isinstance(
        assessment,
        session_events.AssistedApprovalAssessmentCompleted,
    )
    assert (
        assessment.recommendation
        is session_events.AssistedApprovalCompletedRecommendation.REQUIRE_APPROVAL
    )
    assert assessment.rationale == "The command modifies protected files."
    assert assessment.to_dict() == {
        "status": "completed",
        "recommendation": "requireApproval",
        "rationale": "The command modifies protected files.",
    }

    not_run = _load_AssistedApprovalAssessment({"status": "not-run", "reason": "inactive"})
    assert isinstance(not_run, session_events.AssistedApprovalAssessmentNotRun)
    assert not_run.reason == "inactive"
    assert not_run.to_dict() == {"status": "not-run", "reason": "inactive"}

    pending = rpc.PendingPermissionRequest.from_dict(
        {
            "request": {
                "kind": "commands",
                "canOfferSessionApproval": False,
                "commandIdentifiers": ["git status"],
                "fullCommandText": "git status",
                "intention": "Inspect the repository.",
            },
            "requestId": "permission-1",
            "assistedApprovalAssessment": assessment.to_dict(),
        }
    )
    assert isinstance(
        pending.assisted_approval_assessment,
        session_events.AssistedApprovalAssessmentCompleted,
    )
    assert pending.to_dict()["assistedApprovalAssessment"] == assessment.to_dict()


def test_login_account_enumeration_deserializes_concrete_identities():
    value = _load_AuthEnumerateValue(
        {
            "kind": "loginAccounts",
            "items": [
                {
                    "accountId": "entra:opaque-account",
                    "active": True,
                    "label": "person@contoso.example",
                }
            ],
        }
    )

    assert isinstance(value, rpc.AuthEnumerateValueLoginAccounts)
    assert isinstance(value.items[0], rpc.AuthLoginIdentity)
    assert value.kind == rpc.AuthEnumerateQueryKind.LOGIN_ACCOUNTS
    assert value.items[0].account_id == "entra:opaque-account"


def test_catalog_candidate_provenance_serialises_every_catalog_media_type():
    assert not hasattr(rpc, "MediaType")
    for media_type in rpc.CatalogMediaType:
        provenance = rpc.CatalogCandidateProvenance(
            authority="agentfinder.github.com",
            media_type=media_type,
            observed_at="2026-10-13T00:00:00.000Z",
        )
        serialised = provenance.to_dict()
        assert serialised["mediaType"] == media_type.value
        assert rpc.CatalogCandidateProvenance.from_dict(serialised) == provenance


def test_image_availability_preserves_public_session_limit_discriminator():
    assert "SessionLimitPredictionResultKind" in rpc.__all__
    assert rpc.SessionLimitPredictionResultKind is not rpc.ImageToolModelAvailability

    payload = {"kind": "unavailable", "reason": "no_model"}
    result = rpc.SessionLimitPredictionResult.from_dict(payload)
    assert result.kind is rpc.SessionLimitPredictionResultKind.UNAVAILABLE
    assert result.to_dict() == payload

    for value in ("available", "unavailable"):
        kind = rpc.SessionLimitPredictionResultKind(value)
        availability = rpc.ImageToolModelAvailability(value)
        assert kind is not availability
        assert kind.value == availability.value == value
        image_payload = {
            "availability": value,
            "id": "test-image",
            "isDefault": True,
            "name": "Test image",
            "supportedOperations": [],
        }
        image = rpc.ImageToolModel.from_dict(image_payload)
        assert image.availability is availability
        assert image.to_dict() == image_payload
