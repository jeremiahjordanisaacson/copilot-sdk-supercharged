#![cfg(test)]

use super::{TIMEOUT, create_session_pair, timeout};
use copilot_sdk::rpc::{SessionSettingsPatch, SessionUpdateOptionsParams};
use serde_json::json;

#[tokio::test]
async fn expanded_update_preserves_discovery_false_true_and_omission_on_wire() {
    let (session, mut server) = create_session_pair().await;
    for discovery in [None, Some(false), Some(true)] {
        let options = SessionUpdateOptionsParams {
            model: Some("legacy-model".into()),
            settings: Some(SessionSettingsPatch {
                memory: Some(false),
                ..Default::default()
            }),
            enable_config_discovery: discovery,
            ..Default::default()
        };
        let original = serde_json::to_value(&options).unwrap();
        let request_options = options.clone();
        let rpc = session.rpc().options();
        let update = rpc.update(request_options);
        let peer = async {
            let request = server.read_request().await;
            assert_eq!(request["method"], "session.options.update");
            let mut expected = json!({
                "sessionId": session.id().as_str(),
                "model": "legacy-model",
                "settings": {"memory": false},
            });
            if let Some(discovery) = discovery {
                expected["enableConfigDiscovery"] = json!(discovery);
            }
            assert_eq!(request["params"], expected);
            server.respond(&request, json!({"success": true})).await;
        };
        let (response, ()) = timeout(TIMEOUT, async { tokio::join!(update, peer) })
            .await
            .unwrap();
        assert!(response.unwrap().success);
        assert_eq!(serde_json::to_value(&options).unwrap(), original);
    }
    timeout(TIMEOUT, session.stop_event_loop()).await.unwrap();
}
