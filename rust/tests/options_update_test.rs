#![cfg(test)]
#![allow(clippy::unwrap_used)]

use copilot_sdk::Error;
use copilot_sdk::rpc::{
    SessionSettingsPatch, SessionUpdateOptionsParams, SessionUpdateOptionsResult,
};
use copilot_sdk::session::Session;
use serde_json::json;

async fn _update_signature(
    session: &Session,
    params: SessionUpdateOptionsParams,
) -> Result<SessionUpdateOptionsResult, Error> {
    session.rpc().options().update(params).await
}

#[test]
fn options_update_forwards_model_and_explicit_session_setting() {
    let params = SessionUpdateOptionsParams {
        model: Some("selected-model".to_string()),
        settings: Some(SessionSettingsPatch {
            memory: Some(false),
            ..Default::default()
        }),
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(params).unwrap(),
        json!({ "model": "selected-model", "settings": { "memory": false } }),
    );
}
