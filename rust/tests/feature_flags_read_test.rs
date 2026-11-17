#![cfg(test)]
#![allow(clippy::unwrap_used)]

use copilot_sdk::Error;
use copilot_sdk::rpc::{
    FeatureFlagServiceReadParams, FeatureFlagServiceSnapshot, SessionFeatureFlagsReadParams,
    SessionFeatureFlagsReadResult,
};
use copilot_sdk::session::Session;
use serde_json::json;

async fn _read_signature(
    session: &Session,
    params: SessionFeatureFlagsReadParams,
) -> Result<SessionFeatureFlagsReadResult, Error> {
    session.rpc().feature_flags().read(params).await
}

#[test]
fn flag_read_request_includes_display_options() {
    let params = FeatureFlagServiceReadParams {
        display_only: Some(true),
        service_id: Some("display-service".to_string()),
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(params).unwrap(),
        json!({ "displayOnly": true, "serviceId": "display-service" }),
    );
}

#[test]
fn flag_read_result_retains_display_state() {
    let result: SessionFeatureFlagsReadResult = serde_json::from_value(json!({
        "flags": { "enabled": true },
        "overrides": [false, null],
        "snapshot": FeatureFlagServiceSnapshot::default(),
        "displayState": { "available": true },
    }))
    .unwrap();
    assert_eq!(result.display_state, Some(json!({ "available": true })));
    assert!(result.flags["enabled"]);
    assert_eq!(result.overrides, vec![Some(false), None]);
}
