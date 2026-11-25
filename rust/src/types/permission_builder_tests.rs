/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use std::sync::Arc;

use crate::handler::{ApproveAllHandler, PermissionHandler, PermissionResult};
use crate::permission;
use crate::types::{
    CreateSessionOptions, PermissionDecision, PermissionRequestData, RequestId, SessionId,
    SessionPermissionOptions,
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
async fn permission_policies_override_handlers_in_either_order() {
    let policies: [fn(SessionPermissionOptions) -> SessionPermissionOptions; 2] =
        [SessionPermissionOptions::deny_all_permissions, |group| {
            group.approve_permissions_if(|d| {
                d.extra.get("tool").and_then(|v| v.as_str()) != Some("shell")
            })
        }];
    let handler =
        |group: SessionPermissionOptions| group.with_on_request(Arc::new(ApproveAllHandler));
    for policy in policies {
        for options in [
            CreateSessionOptions::default()
                .with_permissions(handler)
                .with_permissions(policy),
            CreateSessionOptions::default()
                .with_permissions(policy)
                .with_permissions(handler),
        ] {
            assert!(matches!(
                dispatch(&resolve_create(options).unwrap()).await,
                PermissionResult::Decision {
                    decision: PermissionDecision::Reject(_),
                    ..
                }
            ));
        }
    }
}
