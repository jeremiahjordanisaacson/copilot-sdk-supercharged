/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use serde_json::json;

use super::{
    acquire_session_initialization, build_mode_post_create_patch, has_managed_settings,
    is_autopilot_continuation_idle, merge_cli_session_options, permission_request_data,
    permission_response_params,
};
use crate::handler::PermissionResult;
use crate::types::{
    PermissionDecisionContext, PermissionDecisionOutcome, PermissionDecisionSource,
    PermissionDecisionSurface, RequestId, SessionEvent, SessionId,
};
use crate::{Client, ClientMode, ErrorKind, SessionErrorKind};

fn initialization_client() -> (Client, tokio::io::DuplexStream) {
    let (transport, peer) = tokio::io::duplex(4096);
    let (reader, writer) = tokio::io::split(transport);
    let client = Client::from_streams(
        ClientMode::CodingAgent,
        reader,
        writer,
        std::path::PathBuf::from("."),
    )
    .unwrap();
    (client, peer)
}

#[tokio::test]
async fn initialization_churn_reclaims_successful_and_rejected_attempts() {
    let (client, _peer) = initialization_client();
    for index in 0..32 {
        let id = SessionId::from(format!("initialization-{index}"));
        for recovery in [false, true] {
            let initialization = acquire_session_initialization(&client, &id, recovery)
                .await
                .unwrap();
            if recovery {
                let callback = client.inner.router.track_rpc_callback(&id);
                let error = acquire_session_initialization(&client, &id, false)
                    .await
                    .err()
                    .expect("same-ID callback reentry must be rejected");
                assert_eq!(*error.kind(), ErrorKind::InvalidConfig);
                drop(callback);
            }
            drop(initialization);
            assert_eq!(client.inner.router.initialization_count(), 0);
        }
    }
    client.force_stop();
}

#[tokio::test]
async fn initialization_drop_and_reacquire_preserve_same_id_exclusion() {
    let (client, _peer) = initialization_client();
    let id = SessionId::from("contended-initialization");
    let ordinary = acquire_session_initialization(&client, &id, false)
        .await
        .unwrap();
    let mut recovery = Box::pin(acquire_session_initialization(&client, &id, true));
    assert!(futures_util::poll!(&mut recovery).is_pending());

    // The queued writer already owns the old lock when its last reader drops.
    drop(ordinary);
    client.inner.router.clear();
    let mut next = Box::pin(acquire_session_initialization(&client, &id, false));
    assert!(futures_util::poll!(&mut next).is_pending());
    let recovery = recovery.await.unwrap();
    assert!(futures_util::poll!(&mut next).is_pending());
    assert_eq!(client.inner.router.initialization_count(), 1);
    drop(recovery);
    drop(next.await.unwrap());
    assert_eq!(client.inner.router.initialization_count(), 0);

    for _ in 0..32 {
        drop(
            acquire_session_initialization(&client, &id, false)
                .await
                .unwrap(),
        );
        drop(
            acquire_session_initialization(&client, &id, true)
                .await
                .unwrap(),
        );
    }
    assert_eq!(client.inner.router.initialization_count(), 0);
    client.force_stop();
}

#[tokio::test]
async fn initialization_cancelled_waiters_reclaim_after_either_drop_order() {
    let (client, _peer) = initialization_client();
    let id = SessionId::from("cancelled-initialization");
    for recovery in [false, true] {
        for cancel_first in [false, true] {
            let held = acquire_session_initialization(&client, &id, !recovery)
                .await
                .unwrap();
            let mut waiting = Box::pin(acquire_session_initialization(&client, &id, recovery));
            assert!(futures_util::poll!(&mut waiting).is_pending());
            if cancel_first {
                drop(waiting);
                drop(held);
            } else {
                drop(held);
                drop(waiting);
            }
            assert_eq!(client.inner.router.initialization_count(), 0);
        }
    }
    client.force_stop();
}

#[tokio::test]
async fn initialization_cleanup_fence_survives_release_and_clear() {
    let (client, _peer) = initialization_client();
    let id = SessionId::from("fenced-initialization");
    let mut initialization = acquire_session_initialization(&client, &id, true)
        .await
        .unwrap();
    initialization.set_cleanup_fenced(true);
    drop(initialization);
    client.inner.router.clear();

    for recovery in [false, true] {
        let error = acquire_session_initialization(&client, &id, recovery)
            .await
            .err()
            .expect("failed cleanup must fence later attempts on this connection");
        assert_eq!(
            *error.kind(),
            ErrorKind::Session(SessionErrorKind::AutoModeRecoveryNotSupported)
        );
        assert_eq!(client.inner.router.initialization_count(), 1);
    }
    client.force_stop();
    assert_eq!(client.inner.router.initialization_count(), 1);
}

#[test]
fn cli_merge_accepts_unset_registrations_without_replacing_sdk_ownership() {
    let registrations = json!({
        "sessionId": "sdk-generated-id",
        "tools": [{"name": "sdk-tool"}],
        "requestUserInput": true,
    });
    let mut params = registrations.clone();
    merge_cli_session_options(
        &mut params,
        json!({
            "sessionId": null,
            "tools": null,
            "hooks": null,
            "requestUserInput": null,
            "githubTokenProviderRegistrationId": null,
            "managedSandboxEnabled": null,
        }),
    )
    .expect("null declarations must leave SDK registrations intact");
    let mut expected = registrations;
    expected["managedSandboxEnabled"] = serde_json::Value::Null;
    assert_eq!(params, expected);
}

#[test]
fn cli_merge_rejects_replacement_identity_and_callback_registrations() {
    for options in [
        json!({"sessionId": "replacement-id"}),
        json!({"requestUserInput": false}),
    ] {
        let mut params = json!({
            "sessionId": "sdk-generated-id",
            "requestUserInput": true,
        });
        let error = merge_cli_session_options(&mut params, options)
            .expect_err("serialized options cannot replace SDK registrations");
        assert!(
            error
                .to_string()
                .contains("must use SDK callback configuration"),
            "{error}"
        );
    }
}

#[test]
fn cli_merge_preserves_grouped_registrations_and_applies_host_policy() {
    let registrations = json!({
        "sessionId": "sdk-generated-id",
        "tools": {"definitions": [{"name": "sdk-tool"}]},
        "hooks": {"hasHandlers": true},
        "interaction": {"requestUserInput": true},
        "permissions": {"request": true},
        "github": {"tokenProviderRegistrationId": "sdk-provider"},
    });
    let mut params = registrations.clone();
    merge_cli_session_options(
        &mut params,
        json!({
            "tools": {"definitions": null, "availableTools": ["sdk-tool"]},
            "hooks": {"hasHandlers": null, "enableRepositoryHooks": false},
            "interaction": {"requestUserInput": true, "runningInInteractiveMode": true},
            "permissions": null,
            "github": {"tokenProviderRegistrationId": null, "fetchManagedSettings": true},
            "events": {"streaming": true},
        }),
    )
    .unwrap();
    let mut expected = registrations;
    expected["tools"]["availableTools"] = json!(["sdk-tool"]);
    expected["hooks"]["enableRepositoryHooks"] = json!(false);
    expected["interaction"]["runningInInteractiveMode"] = json!(true);
    expected["github"]["fetchManagedSettings"] = json!(true);
    expected["events"] = json!({"streaming": true});
    assert_eq!(params, expected);
}

#[cfg(feature = "local-runtime")]
#[tokio::test]
async fn cli_preparation_preserves_runtime_owned_auth_identity_on_create_and_resume() {
    let (client, _peer) = initialization_client();
    let options = json!({
        "sessionId": "cli-owned-session",
        "github": {
            "authInfo": {
                "type": "env",
                "host": "https://github.com",
                "envVar": "GITHUB_TOKEN",
            },
            "fetchManagedSettings": true,
        },
    });
    for prepared in [
        client.prepare_cli_session(
            options.clone(),
            crate::CreateSessionOptions::default(),
            true,
        ),
        client.prepare_cli_resume_session(
            options.clone(),
            crate::ResumeSessionOptions::new(SessionId::from("cli-owned-session")),
            true,
            false,
        ),
    ] {
        let prepared = prepared.expect("CLI preparation must not decode identity as credentials");
        let (fetch_managed_settings, request) = match &prepared.kind {
            super::PreparedKind::CliCreate { config, options } => {
                (config.github.fetch_managed_settings, options)
            }
            super::PreparedKind::CliResume { config, options } => {
                (config.github.fetch_managed_settings, options)
            }
            _ => panic!("expected CLI session preparation"),
        };
        assert_eq!(fetch_managed_settings, Some(true));
        assert_eq!(request["options"], options);
    }
    client.force_stop();
}

#[test]
fn cli_merge_rejects_grouped_callback_replacements() {
    for options in [
        json!({"tools": {"definitions": [{"name": "unregistered-tool"}]}}),
        json!({"canvases": {"provider": {"id": "unregistered-renderer"}}}),
        json!({"hooks": {"hasHandlers": false}}),
        json!({"interaction": {"requestUserInput": false}}),
        json!({"interaction": {"commands": [{"name": "unregistered-command"}]}}),
        json!({"permissions": {"request": false}}),
        json!({"skills": {"hasProvider": true}}),
        json!({"state": {"hasMemoryStore": true}}),
        json!({"github": {"tokenProviderRegistrationId": "unregistered-provider"}}),
    ] {
        let mut params = json!({
            "tools": {"definitions": [{"name": "sdk-tool"}]},
            "hooks": {"hasHandlers": true},
            "interaction": {"requestUserInput": true},
            "permissions": {"request": true},
        });
        let error = merge_cli_session_options(&mut params, options)
            .expect_err("serialized groups cannot replace SDK callback registrations");
        assert!(
            error
                .to_string()
                .contains("must use SDK callback configuration"),
            "{error}"
        );
    }
}

#[test]
fn identifies_only_autopilot_continuation_idles() {
    let mut event = SessionEvent {
        id: "event-1".to_string(),
        timestamp: "2026-01-01T00:00:00Z".to_string(),
        parent_id: None,
        ephemeral: None,
        agent_id: None,
        debug_cli_received_at_ms: None,
        debug_ws_forwarded_at_ms: None,
        event_type: "session.idle".to_string(),
        data: json!({ "mode": "autopilot" }),
    };

    assert!(is_autopilot_continuation_idle(&event));

    event.data = json!({ "mode": "interactive" });
    assert!(!is_autopilot_continuation_idle(&event));

    event.data = json!({});
    assert!(!is_autopilot_continuation_idle(&event));
}

#[test]
fn empty_mode_post_patch_sets_empty_included_builtin_skills() {
    let patch =
        build_mode_post_create_patch(crate::ClientMode::Empty, None, None, None, None, None)
            .expect("empty mode always sends a patch");
    // Serializes as an explicit empty array (not omitted).
    let value = serde_json::to_value(&patch).expect("serialize patch");
    assert_eq!(
        value["skills"]["includedBuiltinSkills"],
        serde_json::json!([])
    );
    assert_eq!(value["plugins"]["installed"], serde_json::json!([]));
}

#[test]
fn empty_mode_post_patch_preserves_explicit_builtin_skill_allowlist() {
    let patch = build_mode_post_create_patch(
        crate::ClientMode::Empty,
        Some(false),
        Some(false),
        Some(true),
        Some(true),
        Some(vec!["code-review".to_string()]),
    )
    .expect("empty mode always sends a patch");
    assert_eq!(
        patch.skills.unwrap().included_builtin_skills,
        Some(vec!["code-review".to_string()])
    );
}

#[test]
fn copilot_cli_mode_does_not_inject_included_builtin_skills() {
    // No fields set -> no patch at all.
    assert!(
        build_mode_post_create_patch(crate::ClientMode::CodingAgent, None, None, None, None, None)
            .is_none()
    );
    // A field set -> patch sent, but skills field stays absent.
    let patch = build_mode_post_create_patch(
        crate::ClientMode::CodingAgent,
        Some(true),
        None,
        None,
        None,
        None,
    )
    .expect("a set field triggers a patch");
    assert!(patch.skills.is_none());
    assert!(patch.plugins.is_none());
    let value = serde_json::to_value(&patch).expect("serialize patch");
    assert!(value.get("skills").is_none());

    let patch = build_mode_post_create_patch(
        crate::ClientMode::CodingAgent,
        None,
        None,
        None,
        None,
        Some(vec!["code-review".to_string()]),
    )
    .expect("an explicit allowlist triggers a patch");
    assert_eq!(
        patch.skills.unwrap().included_builtin_skills,
        Some(vec!["code-review".to_string()])
    );
}

#[test]
fn direct_injection_enables_managed_safeguards() {
    let settings = crate::types::ManagedSettings::default();
    assert!(has_managed_settings(None, Some(&settings)));
    assert!(!has_managed_settings(None, None));
}

fn attribution_context() -> PermissionDecisionContext {
    PermissionDecisionContext {
        outcome: PermissionDecisionOutcome::AutoApproved,
        response_capability: None,
        source: PermissionDecisionSource::AssistedApproval,
        surface: PermissionDecisionSurface::CopilotApp,
        user_feedback: None,
    }
}

#[test]
fn response_params_omit_decision_context_without_attribution() {
    for (result, expected) in [
        (
            PermissionResult::approve_once(),
            json!({ "kind": "approve-once" }),
        ),
        (PermissionResult::reject(None), json!({ "kind": "reject" })),
        (
            PermissionResult::reject(Some("bad".to_string())),
            json!({ "kind": "reject", "feedback": "bad" }),
        ),
        (
            PermissionResult::user_not_available(),
            json!({ "kind": "user-not-available" }),
        ),
    ] {
        let params = permission_response_params(
            &SessionId::from("session-1"),
            &RequestId::from("permission-1"),
            &result,
        )
        .unwrap();
        assert_eq!(
            params,
            json!({
                "sessionId": "session-1",
                "requestId": "permission-1",
                "result": expected,
            })
        );
    }
}

#[test]
fn response_params_forward_decision_context_alongside_result() {
    let mut context = attribution_context();
    context.user_feedback = Some("This approval applies to the displayed request.".to_string());
    let params = permission_response_params(
        &SessionId::from("session-1"),
        &RequestId::from("permission-1"),
        &PermissionResult::approve_once().with_context(context),
    )
    .unwrap();
    assert_eq!(
        params,
        json!({
            "sessionId": "session-1",
            "requestId": "permission-1",
            "result": { "kind": "approve-once" },
            "decisionContext": {
                "outcome": "auto_approved",
                "source": "assisted_approval",
                "surface": "copilot_app",
                "userFeedback": "This approval applies to the displayed request.",
            },
        })
    );
}

#[test]
fn response_params_suppressed_for_no_result() {
    assert!(
        permission_response_params(
            &SessionId::from("session-1"),
            &RequestId::from("permission-1"),
            &PermissionResult::NoResult,
        )
        .is_none()
    );
}

#[test]
fn with_context_is_a_no_op_on_no_result() {
    let result = PermissionResult::no_result().with_context(attribution_context());
    assert!(matches!(result, PermissionResult::NoResult));
}

#[test]
fn permission_request_data_preserves_managed_flag_when_other_fields_are_malformed() {
    let data = permission_request_data(
        &json!({
            "requestId": "permission-1",
            "permissionRequest": {
                "kind": "read",
                "managedApprovalRequired": true,
                "toolCallId": 42
            }
        }),
        false,
    );

    assert_eq!(data.managed_approval_required, Some(true));
    assert_eq!(data.extra["requestId"], "permission-1");
}

#[test]
fn permission_request_data_fails_closed_for_malformed_managed_flag() {
    let data = permission_request_data(
        &json!({
            "requestId": "permission-1",
            "permissionRequest": {
                "kind": "read",
                "managedApprovalRequired": "yes",
                "path": "/workspace/file.txt"
            }
        }),
        false,
    );

    assert_eq!(data.managed_approval_required, Some(true));
}

#[test]
fn permission_request_data_preserves_valid_false_managed_flag() {
    let data = permission_request_data(
        &json!({
            "requestId": "permission-1",
            "permissionRequest": {
                "kind": "read",
                "managedApprovalRequired": false,
                "path": "/workspace/file.txt"
            }
        }),
        false,
    );

    assert_eq!(data.managed_approval_required, Some(false));
}

#[test]
fn permission_request_data_exposes_typed_derived_prompt() {
    let data = permission_request_data(
        &json!({
            "requestId": "permission-1",
            "permissionRequest": {
                "kind": "shell",
                "managedApprovalRequired": true,
                "fullCommandText": "ls"
            },
            "promptRequest": {
                "kind": "commands",
                "fullCommandText": "ls",
                "intention": "List files",
                "commandIdentifiers": ["ls"],
                "canOfferSessionApproval": false,
                "assistedApproval": {
                    "recommendation": "approve"
                }
            }
        }),
        true,
    );

    let prompt = data
        .prompt_request()
        .expect("derived prompt should deserialize")
        .expect("derived prompt should be present");
    let crate::session_events::PermissionPromptRequest::Commands(prompt) = prompt else {
        panic!("expected commands prompt");
    };
    assert_eq!(prompt.managed_approval_required, None);
    assert_eq!(
        prompt
            .assisted_approval
            .expect("assisted approval should be present")
            .recommendation,
        crate::session_events::AssistedApprovalRecommendation::Approve
    );
}
