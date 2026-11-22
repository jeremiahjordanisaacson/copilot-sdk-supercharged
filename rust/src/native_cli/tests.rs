#![cfg(test)]

use serde_json::json;
use tokio::io::AsyncWriteExt;

use super::*;
use crate::ffi::initiation_tests::Peer;

fn notification_peer() -> (Client, tokio::io::DuplexStream, tempfile::TempDir) {
    let cwd = tempfile::tempdir().unwrap();
    let (client, server) = tokio::io::duplex(4096);
    let (reader, writer) = tokio::io::split(client);
    let client = Client::from_streams(reader, writer, cwd.path().to_path_buf()).unwrap();
    (client, server, cwd)
}

async fn write_notification(peer: &mut tokio::io::DuplexStream, method: &str, params: Value) {
    let body = serde_json::to_vec(&json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": params,
    }))
    .unwrap();
    peer.write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
        .await
        .unwrap();
    peer.write_all(&body).await.unwrap();
}

#[tokio::test]
async fn notifications_preserve_cross_method_frame_order_for_each_subscriber() {
    let (client, mut peer, _cwd) = notification_peer();
    let methods = ["remoteHosts.stateChanged", "remoteHosts.sessionChanged"];
    let mut first = subscribe_notifications(&client, &methods).unwrap();
    let mut second = subscribe_notifications(&client, &methods).unwrap();
    let expected = [
        (methods[0], json!({"sequence": 0})),
        (methods[1], json!({"sequence": 1})),
        (methods[0], json!({"sequence": 2})),
        (methods[1], json!({"sequence": 3})),
    ];
    for (method, params) in &expected {
        write_notification(&mut peer, method, params.clone()).await;
        write_notification(&mut peer, "unrelated.notification", Value::Null).await;
    }
    peer.shutdown().await.unwrap();

    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        for subscription in [&mut first, &mut second] {
            for (method, params) in &expected {
                let notification = subscription.next().await.unwrap();
                assert_eq!(notification.method.as_str(), *method);
                assert_eq!(&notification.params, params);
            }
            assert!(subscription.next().await.is_none());
        }
    })
    .await
    .expect("ordered notification streams did not drain");
}

#[tokio::test]
async fn notifications_deregister_on_drop_without_retaining_the_client() {
    let (client, mut peer, _cwd) = notification_peer();
    let methods = ["remoteHosts.stateChanged", "remoteHosts.sessionChanged"];
    let dropped = subscribe_notifications(&client, &methods).unwrap();
    let mut surviving = subscribe_notifications(&client, &methods).unwrap();
    let registry = dropped.registry.upgrade().unwrap();
    assert_eq!(registry.state.lock().subscribers.len(), 2);
    drop(dropped);
    assert_eq!(registry.state.lock().subscribers.len(), 1);
    drop(registry);

    write_notification(&mut peer, methods[1], json!({"retained": true})).await;
    let notification = tokio::time::timeout(std::time::Duration::from_secs(5), surviving.next())
        .await
        .expect("the surviving subscription did not receive its frame")
        .unwrap();
    assert_eq!(notification.params, json!({"retained": true}));

    let owner = Arc::downgrade(&client.inner);
    drop(client);
    assert!(
        owner.upgrade().is_none(),
        "the subscription retained the SDK owner"
    );
    assert!(
        tokio::time::timeout(std::time::Duration::from_secs(5), surviving.next())
            .await
            .expect("owner drop did not close the subscription")
            .is_none()
    );
}

#[tokio::test]
async fn notifications_close_at_transport_eof_while_the_client_remains_alive() {
    let (client, mut peer, _cwd) = notification_peer();
    let mut subscription = subscribe_notifications(&client, &["remoteHosts.stateChanged"]).unwrap();
    peer.shutdown().await.unwrap();
    assert!(
        tokio::time::timeout(std::time::Duration::from_secs(5), subscription.next())
            .await
            .expect("transport EOF did not close the subscription")
            .is_none()
    );
    assert!(subscribe_notifications(&client, &["remoteHosts.stateChanged"]).is_err());
}

#[tokio::test]
async fn timing_initialization_failure_keeps_the_existing_connection_usable() {
    let borrowed = Peer::with_ownership(false);
    let client = borrowed.client();
    let error = initialize_session_timing(client, &json!({ "appVersion": "test" })).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("is missing an expected export (copilot_runtime_cli_host_timing_initialize)"),
        "{error}",
    );
    assert_eq!(
        client.call("ping", None).await.unwrap(),
        json!({"method": "ping"}),
    );
    assert_eq!(borrowed.shutdowns(), 0);
}

#[tokio::test]
async fn missing_private_export_closes_only_the_new_non_owning_connection() {
    let original = Peer::with_ownership(true);
    let borrowed = Peer::with_ownership(false);
    let handle = borrowed.client().clone();
    assert_eq!(
        handle.call("ping", None).await.unwrap(),
        json!({"method": "ping"}),
    );

    let error = finish_start(handle.clone(), |_| Ok::<_, Error>(())).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("is missing an expected export (copilot_runtime_cli_host_connection_admit)"),
        "{error}",
    );
    assert_eq!(handle.in_process_connection_id(), None);
    assert_eq!(borrowed.shutdowns(), 0);
    assert_eq!(original.shutdowns(), 0);
    assert_eq!(
        original.client().call("ping", None).await.unwrap(),
        json!({"method": "ping"}),
    );
}

#[tokio::test]
async fn image_validation_rejects_before_private_symbol_access() {
    let borrowed = Peer::with_ownership(false);
    let handle = borrowed.client().clone();
    let error = finish_start(handle.clone(), |_| {
        Err(Error::with_message(
            ErrorKind::InvalidConfig,
            "image identity rejected",
        ))
    })
    .unwrap_err();
    assert_eq!(error.to_string(), "image identity rejected");
    assert_eq!(handle.in_process_connection_id(), None);
    assert_eq!(borrowed.shutdowns(), 0);
}
