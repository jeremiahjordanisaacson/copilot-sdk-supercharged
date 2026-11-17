#![allow(clippy::unwrap_used)]

use copilot_sdk::Client;
#[cfg(feature = "runtime")]
use copilot_sdk::{RuntimeConnection, TcpRuntimeOptions};
use tokio::io::{AsyncReadExt, AsyncWrite, AsyncWriteExt, duplex};

#[tokio::test]
async fn custom_streams_forward_entra_and_reject_legacy_servers_without_ping() {
    use copilot_sdk::EntraAuthConfig;

    for supported in [true, false] {
        let (client_write, mut server_read) = duplex(8192);
        let (mut server_write, client_read) = duplex(8192);
        let client = Client::from_streams_with_entra(
            copilot_sdk::ClientMode::CodingAgent,
            client_read,
            client_write,
            std::env::temp_dir(),
            Some(EntraAuthConfig {
                client_id: "custom-stream-client".to_owned(),
                mats_app_name: Some("Custom Stream Host".to_owned()),
                one_auth_application_id: None,
            }),
        )
        .unwrap();
        let server = async {
            let request = read_framed(&mut server_read).await;
            assert_eq!(request["method"], "connect");
            assert_eq!(
                request["params"]["entra"]["clientId"],
                "custom-stream-client"
            );
            assert_eq!(
                request["params"]["entra"]["matsAppName"],
                "Custom Stream Host"
            );
            let response = if supported {
                serde_json::json!({"jsonrpc":"2.0","id":request["id"],"result":{"ok":true,"protocolVersion":4,"version":"test"}})
            } else {
                serde_json::json!({"jsonrpc":"2.0","id":request["id"],"error":{"code":-32601,"message":"Method not found"}})
            };
            write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;
        };
        let result = tokio::time::timeout(std::time::Duration::from_secs(2), async {
            let (result, ()) = tokio::join!(client.verify_protocol_version(), server);
            result
        })
        .await
        .expect("handshake must settle without a legacy ping");
        if supported {
            result.unwrap();
            assert_eq!(client.protocol_version(), Some(4));
        } else {
            assert!(
                result
                    .unwrap_err()
                    .to_string()
                    .contains("does not support Entra client configuration")
            );
        }
        drop(client);
        assert_eq!(
            server_read.read(&mut [0]).await.unwrap(),
            0,
            "unexpected ping"
        );
    }
}

#[cfg(feature = "runtime")]
#[tokio::test]
async fn legacy_ping_fallback_requires_no_entra_configuration() {
    use copilot_sdk::{ClientOptions, EntraAuthConfig};

    for entra in [false, true] {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = tokio::spawn(async move {
            let (mut stream, _) = listener.accept().await.unwrap();
            let connect = read_framed(&mut stream).await;
            assert_eq!(connect["method"], "connect");
            write_framed(
                &mut stream,
                &serde_json::to_vec(&serde_json::json!({
                    "jsonrpc": "2.0", "id": connect["id"],
                    "error": {"code": -32601, "message": "Method not found"}
                }))
                .unwrap(),
            )
            .await;
            if !entra {
                let ping = read_framed(&mut stream).await;
                assert_eq!(ping["method"], "ping");
                write_framed(
                    &mut stream,
                    &serde_json::to_vec(&serde_json::json!({
                        "jsonrpc": "2.0", "id": ping["id"],
                        "result": {"protocolVersion": 4}
                    }))
                    .unwrap(),
                )
                .await;
            }
            let mut byte = [0];
            assert_eq!(
                stream.read(&mut byte).await.unwrap(),
                0,
                "unexpected request after handshake"
            );
        });
        let mut options = ClientOptions::new(copilot_sdk::ClientMode::CodingAgent).with_runtime(
            RuntimeConnection::Tcp(TcpRuntimeOptions {
                host: address.ip().to_string(),
                port: address.port(),
                connection_token: None,
            }),
        );
        if entra {
            options = options.with_entra(EntraAuthConfig {
                client_id: "host-entra-client".to_owned(),
                mats_app_name: None,
                one_auth_application_id: None,
            });
        }
        let result = Client::start(options).await;
        if entra {
            let error = result.expect_err("unsupported Entra configuration must fail");
            assert!(
                error
                    .to_string()
                    .contains("does not support Entra client configuration"),
                "{error}"
            );
        } else {
            let client = result.unwrap();
            assert_eq!(client.protocol_version(), Some(4));
            drop(client);
        }
        tokio::time::timeout(std::time::Duration::from_secs(5), server)
            .await
            .unwrap()
            .unwrap();
    }
}

#[tokio::test]
async fn dropping_external_client_closes_its_streams() {
    let (client_write, mut server_read) = duplex(8192);
    let (_server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();
    client
        .register_request_handler("host.shutdown", |_, client| async move {
            client.call("ping", None).await
        })
        .unwrap();
    drop(client);
    let mut byte = [0];
    let count = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        server_read.read(&mut byte),
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(count, 0);
}

#[tokio::test]
async fn connection_handler_can_await_sdk_replies_before_responding() {
    let (client_write, mut server_read) = duplex(8192);
    let (mut server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();
    client
        .register_request_handler("host.shutdown", |_, client| async move {
            client.call("ping", None).await
        })
        .unwrap();
    let request = serde_json::json!({"jsonrpc":"2.0","id":41,"method":"host.shutdown","params":{}});
    write_framed(&mut server_write, &serde_json::to_vec(&request).unwrap()).await;
    let ping = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        read_framed(&mut server_read),
    )
    .await
    .unwrap();
    assert_eq!(ping["method"], "ping");
    let response = serde_json::json!({"jsonrpc":"2.0","id":ping["id"],"result":{"drained":true}});
    write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;
    let shutdown = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        read_framed(&mut server_read),
    )
    .await
    .unwrap();
    assert_eq!(shutdown["id"], 41);
    assert_eq!(shutdown["result"], serde_json::json!({"drained":true}));
    drop(client);
    let mut byte = [0];
    assert_eq!(
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            server_read.read(&mut byte),
        )
        .await
        .unwrap()
        .unwrap(),
        0
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn synchronous_handler_setup_does_not_block_the_reader() {
    let (client_write, mut server_read) = duplex(8192);
    let (mut server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        ".".into(),
    )
    .unwrap();
    let (started_tx, mut started) = tokio::sync::mpsc::unbounded_channel();
    let (unblock, gate) = std::sync::mpsc::channel();
    let gate = std::sync::Mutex::new(gate);
    client
        .register_request_handler("blocking.setup", move |_, _| {
            started_tx.send(()).unwrap();
            gate.lock().unwrap().recv().unwrap();
            async { Ok(serde_json::json!({})) }
        })
        .unwrap();
    let request = serde_json::json!({"jsonrpc":"2.0","id":44,"method":"blocking.setup"});
    write_framed(&mut server_write, &serde_json::to_vec(&request).unwrap()).await;
    tokio::time::timeout(std::time::Duration::from_secs(2), started.recv())
        .await
        .unwrap()
        .unwrap();
    let ping = tokio::spawn({
        let client = client.clone();
        async move { client.call("ping", None).await }
    });
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        let request = read_framed(&mut server_read).await;
        let response = serde_json::json!({"jsonrpc":"2.0","id":request["id"],"result":{}});
        write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;
        ping.await.unwrap().unwrap();
    })
    .await
    .unwrap();
    unblock.send(()).unwrap();
    assert_eq!(read_framed(&mut server_read).await["id"], 44);
    client.force_stop();
}

#[tokio::test]
async fn connection_handlers_reject_duplicate_registration_and_report_errors() {
    let (client_write, mut server_read) = duplex(8192);
    let (mut server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();
    client
        .register_request_handler("host.shutdown", |_, _| async {
            Err(copilot_sdk::Error::with_message(
                copilot_sdk::ErrorKind::InvalidConfig,
                "drain failed",
            ))
        })
        .unwrap();
    assert!(
        client
            .register_request_handler("host.shutdown", |_, _| async { Ok(serde_json::json!({})) })
            .is_err()
    );
    assert!(
        client
            .register_request_handler("", |_, _| async { Ok(serde_json::json!({})) })
            .is_err()
    );
    let request = serde_json::json!({"jsonrpc":"2.0","id":42,"method":"host.shutdown","params":{}});
    write_framed(&mut server_write, &serde_json::to_vec(&request).unwrap()).await;
    let response = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        read_framed(&mut server_read),
    )
    .await
    .unwrap();
    assert_eq!(response["error"]["code"], -32603);
    assert_eq!(response["error"]["message"], "drain failed");
    client.force_stop();
}

#[tokio::test]
async fn connection_eof_cancels_inbound_handlers() {
    use std::sync::Arc;

    use tokio::sync::Notify;

    struct OnDrop(Arc<Notify>);
    impl Drop for OnDrop {
        fn drop(&mut self) {
            self.0.notify_one();
        }
    }
    let (client_write, _server_read) = duplex(8192);
    let (mut server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();
    let started = Arc::new(Notify::new());
    let cancelled = Arc::new(Notify::new());
    let callback_started = started.clone();
    let callback_cancelled = cancelled.clone();
    client
        .register_request_handler("host.shutdown", move |_, client| {
            let started = callback_started.clone();
            let cancelled = callback_cancelled.clone();
            async move {
                let _guard = OnDrop(cancelled);
                let _client = client;
                started.notify_one();
                std::future::pending().await
            }
        })
        .unwrap();
    let request = serde_json::json!({"jsonrpc":"2.0","id":43,"method":"host.shutdown","params":{}});
    write_framed(&mut server_write, &serde_json::to_vec(&request).unwrap()).await;
    tokio::time::timeout(std::time::Duration::from_secs(2), started.notified())
        .await
        .unwrap();
    drop(client);
    server_write.shutdown().await.unwrap();
    tokio::time::timeout(std::time::Duration::from_secs(2), cancelled.notified())
        .await
        .unwrap();
}

#[cfg(not(feature = "runtime"))]
#[tokio::test]
async fn external_stream_build_cannot_launch_or_discover_a_runtime() {
    let error = Client::start(copilot_sdk::ClientOptions::new(
        copilot_sdk::ClientMode::CodingAgent,
    ))
    .await
    .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("requires the `runtime` Cargo feature")
    );
}

async fn write_framed(writer: &mut (impl AsyncWrite + Unpin), body: &[u8]) {
    let header = format!("Content-Length: {}\r\n\r\n", body.len());
    writer.write_all(header.as_bytes()).await.unwrap();
    writer.write_all(body).await.unwrap();
    writer.flush().await.unwrap();
}

async fn read_framed(reader: &mut (impl tokio::io::AsyncRead + Unpin)) -> serde_json::Value {
    let mut header = String::new();
    loop {
        let mut byte = [0u8; 1];
        AsyncReadExt::read_exact(reader, &mut byte).await.unwrap();
        header.push(byte[0] as char);
        if header.ends_with("\r\n\r\n") {
            break;
        }
    }
    let length: usize = header
        .trim()
        .strip_prefix("Content-Length: ")
        .unwrap()
        .parse()
        .unwrap();
    let mut buf = vec![0u8; length];
    AsyncReadExt::read_exact(reader, &mut buf).await.unwrap();
    serde_json::from_slice(&buf).unwrap()
}

/// Verify protocol version against a fake server. Mimics a legacy server
/// that lacks the `connect` JSON-RPC method (-32601 MethodNotFound),
/// forcing the client to fall back to `ping` — the canonical
/// backward-compatibility path documented on `verify_protocol_version`.
async fn verify_with_result(
    result: serde_json::Value,
) -> (Result<(), copilot_sdk::Error>, Option<u32>) {
    let (client_write, server_read) = duplex(8192);
    let (server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();

    let mut server_read = server_read;
    let mut server_write = server_write;

    let verify_handle = tokio::spawn({
        let client = client.clone();
        async move { client.verify_protocol_version().await }
    });

    // 1. Client sends `connect` first; respond with MethodNotFound so the
    //    client falls back to `ping` (the legacy-server compatibility path).
    let connect_req = read_framed(&mut server_read).await;
    assert_eq!(connect_req["method"], "connect");
    let not_found = serde_json::json!({
        "jsonrpc": "2.0",
        "id": connect_req["id"],
        "error": { "code": -32601, "message": "Method not found" },
    });
    write_framed(&mut server_write, &serde_json::to_vec(&not_found).unwrap()).await;

    // 2. Client falls back to `ping`; respond with the requested result.
    let req = read_framed(&mut server_read).await;
    assert_eq!(req["method"], "ping");
    let response = serde_json::json!({
        "jsonrpc": "2.0",
        "id": req["id"],
        "result": result,
    });
    write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;

    let res = tokio::time::timeout(std::time::Duration::from_secs(2), verify_handle)
        .await
        .unwrap()
        .unwrap();
    let version = client.protocol_version();
    (res, version)
}

#[tokio::test]
async fn accepted_when_version_in_range() {
    let (res, version) = verify_with_result(serde_json::json!({ "protocolVersion": 4 })).await;
    assert!(res.is_ok());
    assert_eq!(version, Some(4));
}

#[tokio::test]
async fn rejected_when_version_out_of_range() {
    let (res, version) = verify_with_result(serde_json::json!({ "protocolVersion": 3 })).await;
    let err = res.unwrap_err();
    assert!(matches!(
        err.kind(),
        copilot_sdk::ErrorKind::Protocol(copilot_sdk::ProtocolErrorKind::VersionMismatch {
            server: 3,
            min: 4,
            max: 4
        })
    ));
    assert_eq!(version, None);
}

#[cfg(feature = "runtime")]
#[tokio::test]
async fn incompatible_runtime_is_rejected_before_session_operations() {
    use copilot_sdk::{ClientOptions, ErrorKind, ProtocolErrorKind};

    for version in [3, 5] {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let address = listener.local_addr().unwrap();
        let server = async {
            let (mut stream, _) = listener.accept().await.unwrap();
            let request = read_framed(&mut stream).await;
            assert_eq!(request["method"], "connect");
            let response = serde_json::json!({
                "jsonrpc": "2.0", "id": request["id"],
                "result": {"ok": true, "version": "test", "protocolVersion": version}
            });
            write_framed(&mut stream, &serde_json::to_vec(&response).unwrap()).await;
            assert_eq!(
                stream.read(&mut [0]).await.unwrap(),
                0,
                "incompatible runtime received a request after negotiation"
            );
        };
        let start = Client::start(
            ClientOptions::new(copilot_sdk::ClientMode::CodingAgent).with_runtime(
                RuntimeConnection::for_tcp(address.ip().to_string(), address.port()),
            ),
        );
        let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::join!(start, server)
        })
        .await
        .expect("incompatible startup must close the transport");
        assert_eq!(
            result.unwrap_err().kind(),
            &ErrorKind::Protocol(ProtocolErrorKind::VersionMismatch {
                server: version,
                min: 4,
                max: 4,
            })
        );
    }
}

#[tokio::test]
async fn succeeds_when_version_missing() {
    let (res, version) = verify_with_result(serde_json::json!({ "message": "pong" })).await;
    assert!(res.is_ok());
    assert_eq!(version, None);
}

/// New `connect` handshake path: when the server supports `connect` (modern
/// CLIs do), the client uses it directly without falling back to `ping`.
/// Validates the protocolVersion negotiated through the new RPC.
#[tokio::test]
async fn connect_handshake_supplies_protocol_version() {
    let (client_write, server_read) = duplex(8192);
    let (server_write, client_read) = duplex(8192);
    let client = Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();

    let mut server_read = server_read;
    let mut server_write = server_write;

    let verify_handle = tokio::spawn({
        let client = client.clone();
        async move { client.verify_protocol_version().await }
    });

    let req = read_framed(&mut server_read).await;
    assert_eq!(req["method"], "connect");
    // Token is None for the from_streams entry point (no transport spawn).
    assert!(req["params"].get("token").is_none());
    assert!(req["params"].get("clientInfo").is_none());
    let response = serde_json::json!({
        "jsonrpc": "2.0",
        "id": req["id"],
        "result": { "ok": true, "protocolVersion": 4, "version": "test-1.0.0" },
    });
    write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;

    let res = tokio::time::timeout(std::time::Duration::from_secs(2), verify_handle)
        .await
        .unwrap()
        .unwrap();
    assert!(res.is_ok());
    assert_eq!(client.protocol_version(), Some(4));
}

#[tokio::test]
async fn connect_handshake_forwards_explicit_and_generated_tokens() {
    let token = Client::generate_connection_token_for_test();
    // Sanity-check the generated shape: 32-char lowercase hex (16 bytes,
    // 128 bits of entropy). A regression in the helper would silently
    // weaken loopback authentication.
    assert_eq!(token.len(), 32, "expected 32-char hex, got {token:?}");
    assert!(
        token
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_uppercase()),
        "expected lowercase hex, got {token:?}",
    );

    for token in ["explicit-token-abc".to_string(), token] {
        let (client_write, mut server_read) = duplex(8192);
        let (mut server_write, client_read) = duplex(8192);
        let client = Client::from_streams_with_connection_token(
            client_read,
            client_write,
            std::env::temp_dir(),
            Some(token.clone()),
        )
        .unwrap();

        let verify_handle = tokio::spawn({
            let client = client.clone();
            async move { client.verify_protocol_version().await }
        });

        let req = read_framed(&mut server_read).await;
        assert_eq!(req["method"], "connect");
        assert_eq!(req["params"]["token"], token);

        let response = serde_json::json!({
            "jsonrpc": "2.0",
            "id": req["id"],
            "result": { "ok": true, "protocolVersion": 4, "version": "test-1.0.0" },
        });
        write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;

        tokio::time::timeout(std::time::Duration::from_secs(2), verify_handle)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
    }
}

#[tokio::test]
async fn connect_handshake_serializes_client_info_and_omits_empty_fields() {
    for (info, expected) in [
        (
            copilot_sdk::ClientInfo::new()
                .with_application_name("acme-developer-portal")
                .with_application_version("2.4.0")
                .with_integration_name("copilot-assistant")
                .with_integration_version("1.5.0"),
            Some(serde_json::json!({
                "editorName": "acme-developer-portal",
                "editorVersion": "2.4.0",
                "extensionName": "copilot-assistant",
                "extensionVersion": "1.5.0",
            })),
        ),
        (
            copilot_sdk::ClientInfo::new()
                .with_application_name("example-app")
                .with_application_version(""),
            Some(serde_json::json!({ "editorName": "example-app" })),
        ),
        (
            copilot_sdk::ClientInfo::new()
                .with_application_name("")
                .with_application_version("")
                .with_integration_name("")
                .with_integration_version(""),
            None,
        ),
    ] {
        let (client_write, mut server_read) = duplex(8192);
        let (mut server_write, client_read) = duplex(8192);
        let client = Client::from_streams_with_client_info(
            client_read,
            client_write,
            std::env::temp_dir(),
            Some(info),
        )
        .unwrap();

        let verify_handle = tokio::spawn({
            let client = client.clone();
            async move { client.verify_protocol_version().await }
        });

        let req = read_framed(&mut server_read).await;
        assert_eq!(req["method"], "connect");
        assert_eq!(req["params"].get("clientInfo"), expected.as_ref());

        let response = serde_json::json!({
            "jsonrpc": "2.0",
            "id": req["id"],
            "result": { "ok": true, "protocolVersion": 4, "version": "test-1.0.0" },
        });
        write_framed(&mut server_write, &serde_json::to_vec(&response).unwrap()).await;

        tokio::time::timeout(std::time::Duration::from_secs(2), verify_handle)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
    }
}
