//! Rust embedding peers share one real runtime without a competing SDK reader.
#![cfg(feature = "local-runtime")]
#![allow(clippy::unwrap_used)]

use std::path::PathBuf;
use std::time::Duration;

use copilot_sdk::{
    AttachedInProcessRuntimeOptions, Client, ClientMode, ClientOptions,
    InProcessProtocolConnection, InProcessProtocolConnectionOptions, InProcessRuntimeOptions,
    OwnedRuntimeOptions, RuntimeConnection,
};
use serde_json::{Value, json};

const DEADLINE: Duration = Duration::from_secs(180);

fn frame(id: u64, method: &str, params: Value) -> String {
    let body = json!({"jsonrpc":"2.0", "id":id, "method":method, "params":params}).to_string();
    format!(
        "Content-Length: {}\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{body}",
        body.len()
    )
}

fn request(peer: &InProcessProtocolConnection, id: u64, method: &str, params: Value) {
    let frame = frame(id, method, params);
    // A real stream can split either its headers or its body across writes.
    peer.write(&frame.as_bytes()[..7]).unwrap();
    peer.write(&frame.as_bytes()[7..]).unwrap();
}

async fn response_message(
    peer: &InProcessProtocolConnection,
    id: u64,
    pending: &mut Vec<u8>,
) -> Value {
    tokio::time::timeout(DEADLINE, async {
        loop {
            if let Some(end) = pending.windows(4).position(|bytes| bytes == b"\r\n\r\n") {
                let headers = std::str::from_utf8(&pending[..end]).unwrap();
                let length: usize = headers
                    .lines()
                    .find_map(|line| line.strip_prefix("Content-Length:"))
                    .unwrap()
                    .trim()
                    .parse()
                    .unwrap();
                let frame_end = end + 4 + length;
                if pending.len() >= frame_end {
                    let value: Value =
                        serde_json::from_slice(&pending[end + 4..frame_end]).unwrap();
                    pending.drain(..frame_end);
                    if value["id"] == id {
                        return value;
                    }
                    continue;
                }
            }
            pending.extend(
                peer.next_outbound()
                    .await
                    .expect("peer must deliver its response before EOF"),
            );
        }
    })
    .await
    .expect("raw peer response deadline")
}

async fn response(peer: &InProcessProtocolConnection, id: u64, pending: &mut Vec<u8>) -> Value {
    let value = response_message(peer, id, pending).await;
    assert!(value.get("error").is_none(), "{value}");
    value["result"].clone()
}

async fn open(client: &Client, token: &str) -> InProcessProtocolConnection {
    let mut options = InProcessProtocolConnectionOptions::default();
    options.connection_token = Some(token.into());
    let peer = client
        .open_in_process_protocol_connection(options)
        .await
        .unwrap();
    request(
        &peer,
        1,
        "connect",
        json!({
            "token": token,
            "clientInfo": {"extensionName": "rust-raw-protocol-fixture", "extensionVersion": "1.0.0"}
        }),
    );
    response(&peer, 1, &mut Vec::new()).await;
    peer
}

#[tokio::test]
async fn same_server_peers_create_sessions_drain_and_close_independently() {
    let library = PathBuf::from(
        std::env::var_os("COPILOT_RUNTIME_CDYLIB")
            .expect("set COPILOT_RUNTIME_CDYLIB to the same-checkout runtime.node"),
    );
    assert!(library.is_file(), "runtime addon: {}", library.display());
    let directory = tempfile::tempdir().unwrap();
    let options = ClientOptions::new(ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::InProcess(InProcessRuntimeOptions {
            owned: OwnedRuntimeOptions::new().with_base_directory(directory.path()),
            runtime_library_path: Some(library),
            ..Default::default()
        }))
        .with_github(|group| {
            group
                .with_token("ghs_protocol_embedding_fixture")
                .with_use_logged_in_user(false)
        })
        .with_observability(|group| group.with_disable_client_telemetry(true));
    let owner = Client::start(options).await.unwrap();
    let outcome = std::panic::AssertUnwindSafe(async {
        let first = open(&owner, "first-token").await;
        let second = open(&owner, "second-token").await;
        let mut denied_options = InProcessProtocolConnectionOptions::default();
        denied_options.connection_token = Some("expected-token".into());
        let denied = owner
            .open_in_process_protocol_connection(denied_options)
            .await
            .unwrap();
        request(&denied, 1, "connect", json!({"token":"wrong-token"}));
        let rejection = response_message(&denied, 1, &mut Vec::new()).await;
        assert!(rejection.get("error").is_some(), "{rejection}");
        denied.close().await.unwrap();
        assert_ne!(first.connection_id(), second.connection_id());
        let mut first_pending = Vec::new();
        let mut second_pending = Vec::new();
        request(&first, 2, "session.create", json!({}));
        request(&second, 2, "session.create", json!({}));
        let (first_created, second_created) = tokio::join!(
            response(&first, 2, &mut first_pending),
            response(&second, 2, &mut second_pending),
        );
        let first_session = first_created["sessionId"].as_str().unwrap();
        let second_session = second_created["sessionId"].as_str().unwrap();
        assert_ne!(first_session, second_session);
        owner
            .call(
                "session.getMessages",
                Some(json!({"sessionId": first_session})),
            )
            .await
            .expect("the owner sees the raw peer's session on the same engine");
        request(
            &first,
            3,
            "ping",
            json!({"message": "drained-before-close"}),
        );
        first.close().await.unwrap();
        let pong = response(&first, 3, &mut first_pending).await;
        assert_eq!(pong["message"], "pong: drained-before-close");
        assert!(first.next_outbound().await.is_none());
        assert!(first.write(b"closed").is_err());
        request(
            &second,
            3,
            "session.getMessages",
            json!({"sessionId": second_session}),
        );
        response(&second, 3, &mut second_pending).await;
        owner.call("ping", Some(json!({}))).await.unwrap();
        owner.stop().await.unwrap();
        tokio::time::timeout(DEADLINE, async {
            while second.next_outbound().await.is_some() {}
            second.close().await.unwrap();
        })
        .await
        .expect("owner shutdown releases the remaining peer and its reader");
        assert!(second.write(b"closed").is_err());
    });
    use futures_util::FutureExt;
    let result = outcome.catch_unwind().await;
    owner.stop().await.unwrap();
    if let Err(panic) = result {
        std::panic::resume_unwind(panic);
    }
}

#[tokio::test]
async fn borrowed_client_peer_close_does_not_stop_the_engine_owner() {
    let library = PathBuf::from(
        std::env::var_os("COPILOT_RUNTIME_CDYLIB")
            .expect("set COPILOT_RUNTIME_CDYLIB to the same-checkout runtime.node"),
    );
    let directory = tempfile::tempdir().unwrap();
    let options = ClientOptions::new(ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::InProcess(InProcessRuntimeOptions {
            owned: OwnedRuntimeOptions::new().with_base_directory(directory.path()),
            runtime_library_path: Some(library.clone()),
            ..Default::default()
        }))
        .with_github(|group| {
            group
                .with_token("ghs_borrowed_protocol_fixture")
                .with_use_logged_in_user(false)
        })
        .with_observability(|group| group.with_disable_client_telemetry(true));
    let owner = Client::start(options).await.unwrap();
    let borrowed_options = ClientOptions::new(ClientMode::CodingAgent).with_runtime(
        RuntimeConnection::AttachedInProcess(AttachedInProcessRuntimeOptions {
            server_id: owner.in_process_server_id().unwrap(),
            runtime_library_path: library,
            session_binding: None,
            run_on_ui_thread: None,
            file_logging_path: None,
        }),
    );
    let borrowed = Client::start(borrowed_options).await.unwrap();
    let peer = open(&borrowed, "borrowed-token").await;
    borrowed.stop().await.unwrap();
    tokio::time::timeout(DEADLINE, async {
        while peer.next_outbound().await.is_some() {}
        peer.close().await.unwrap();
    })
    .await
    .expect("borrowed client shutdown releases its own peer");
    owner.call("ping", Some(json!({}))).await.unwrap();
    let sibling = open(&owner, "replacement-token").await;
    sibling.close().await.unwrap();
    owner.stop().await.unwrap();
}

#[test]
fn foreground_publications_keep_submission_order_with_reversed_polling() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(1)
        .build()
        .unwrap();
    runtime.block_on(async {
        use futures_util::FutureExt;
        use std::future::{Future, poll_fn};
        use std::task::Poll;

        let library = PathBuf::from(
            std::env::var_os("COPILOT_RUNTIME_CDYLIB")
                .expect("set COPILOT_RUNTIME_CDYLIB to the same-checkout runtime.node"),
        );
        let directory = tempfile::tempdir().unwrap();
        let options = ClientOptions::new(ClientMode::CodingAgent)
            .with_runtime(RuntimeConnection::InProcess(InProcessRuntimeOptions {
                owned: OwnedRuntimeOptions::new().with_base_directory(directory.path()),
                runtime_library_path: Some(library),
                ..Default::default()
            }))
            .with_github(|group| group.with_token("").with_use_logged_in_user(false))
            .with_observability(|group| group.with_disable_client_telemetry(true));
        let owner = Client::start(options).await.unwrap();
        let outcome = std::panic::AssertUnwindSafe(async {
            let created = owner
                .call(
                    "session.create",
                    Some(json!({"hooks": {"enableRepositoryHooks": false}, "enableConfigDiscovery": false})),
                )
                .await
                .unwrap();
            let session_id = created["sessionId"].as_str().unwrap();
            assert!(
                owner
                    .publish_in_process_foreground_session(Some(session_id))
                    .await
                    .unwrap()
            );
            let (started, entered) = tokio::sync::oneshot::channel();
            let (release, blocked) = std::sync::mpsc::channel();
            let occupied = tokio::task::spawn_blocking(move || {
                let _ = started.send(());
                let _ = blocked.recv();
            });
            entered.await.unwrap();

            let mut clear = Box::pin(owner.publish_in_process_foreground_session(None));
            let mut publish =
                Box::pin(owner.publish_in_process_foreground_session(Some(session_id)));
            // One occupied worker fixes execution order to the order of queued work.
            poll_fn(|context| {
                assert!(publish.as_mut().poll(context).is_pending());
                assert!(clear.as_mut().poll(context).is_pending());
                Poll::Ready(())
            })
            .await;
            release.send(()).unwrap();
            occupied.await.unwrap();
            let (cleared, published) = tokio::join!(clear, publish);
            assert!(cleared.unwrap());
            assert!(published.unwrap());
            let foreground = owner.get_foreground_session_id().await.unwrap();
            assert_eq!(
                foreground.as_ref().map(|id| id.to_string()),
                Some(session_id.to_owned()),
                "The earlier clear must not overwrite the later publication",
            );

            let clear = owner.publish_in_process_foreground_session(None);
            let canceled = owner.publish_in_process_foreground_session(None);
            let publish = owner.publish_in_process_foreground_session(Some(session_id));
            drop(canceled);
            let (published, cleared) =
                tokio::time::timeout(DEADLINE, async { tokio::join!(publish, clear) })
                    .await
                    .expect("A canceled reservation must not block later publications");
            assert!(cleared.unwrap());
            assert!(published.unwrap());
            assert_eq!(
                owner
                    .get_foreground_session_id()
                    .await
                    .unwrap()
                    .as_ref()
                    .map(|id| id.to_string()),
                Some(session_id.to_owned()),
                "A canceled reservation must preserve the order of its neighbors",
            );
        })
        .catch_unwind()
        .await;
        owner.stop().await.unwrap();
        if let Err(panic) = outcome {
            std::panic::resume_unwind(panic);
        }
    });
}
