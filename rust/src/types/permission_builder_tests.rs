/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use std::sync::Arc;

use crate::handler::{ApproveAllHandler, PermissionHandler, PermissionResult};
use crate::permission;
use crate::types::{
    CreateSessionOptions, PermissionDecision, PermissionRequestData, RequestId, SessionId,
};

fn data() -> PermissionRequestData {
    PermissionRequestData {
        extra: serde_json::json!({"tool": "shell"}),
        ..Default::default()
    }
}

/// Apply the same policy-resolution logic that `Client::create_session`
/// uses, so tests exercise the effective handler.
fn resolve_create(mut cfg: CreateSessionOptions) -> Option<Arc<dyn PermissionHandler>> {
    permission::resolve_handler(
        cfg.permissions.on_request.take(),
        cfg.permissions.policy.take(),
    )
}

async fn dispatch(handler: &Arc<dyn PermissionHandler>) -> PermissionResult {
    handler
        .handle(SessionId::from("s1"), RequestId::new("1"), data())
        .await
}

#[tokio::test]
async fn approve_all_standalone_produces_handler() {
    let cfg =
        CreateSessionOptions::default().with_permissions(|group| group.approve_all_permissions());
    let h = resolve_create(cfg).expect("policy alone yields handler");
    assert!(matches!(
        dispatch(&h).await,
        PermissionResult::Decision {
            decision: PermissionDecision::ApproveOnce(_),
            ..
        }
    ));
}

#[tokio::test]
async fn deny_all_is_order_independent() {
    let a = CreateSessionOptions::default()
        .with_permissions(|group| group.with_on_request(Arc::new(ApproveAllHandler)))
        .with_permissions(|group| group.deny_all_permissions());
    let b = CreateSessionOptions::default()
        .with_permissions(|group| group.deny_all_permissions())
        .with_permissions(|group| group.with_on_request(Arc::new(ApproveAllHandler)));
    let ha = resolve_create(a).unwrap();
    let hb = resolve_create(b).unwrap();
    assert!(matches!(
        dispatch(&ha).await,
        PermissionResult::Decision {
            decision: PermissionDecision::Reject(_),
            ..
        }
    ));
    assert!(matches!(
        dispatch(&hb).await,
        PermissionResult::Decision {
            decision: PermissionDecision::Reject(_),
            ..
        }
    ));
}

#[tokio::test]
async fn approve_permissions_if_is_order_independent() {
    let predicate =
        |d: &PermissionRequestData| d.extra.get("tool").and_then(|v| v.as_str()) != Some("shell");
    let a = CreateSessionOptions::default()
        .with_permissions(|group| group.with_on_request(Arc::new(ApproveAllHandler)))
        .with_permissions(|group| group.approve_permissions_if(predicate));
    let b = CreateSessionOptions::default()
        .with_permissions(|group| group.approve_permissions_if(predicate))
        .with_permissions(|group| group.with_on_request(Arc::new(ApproveAllHandler)));
    let ha = resolve_create(a).unwrap();
    let hb = resolve_create(b).unwrap();
    assert!(matches!(
        dispatch(&ha).await,
        PermissionResult::Decision {
            decision: PermissionDecision::Reject(_),
            ..
        }
    ));
    assert!(matches!(
        dispatch(&hb).await,
        PermissionResult::Decision {
            decision: PermissionDecision::Reject(_),
            ..
        }
    ));
}
