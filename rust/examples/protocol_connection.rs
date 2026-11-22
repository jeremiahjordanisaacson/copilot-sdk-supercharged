// Copyright (c) Microsoft Corporation. All rights reserved.

//! Host independent raw protocol peers beside an SDK client.
//! Run with `cargo run --no-default-features --features local-runtime --example
//! protocol_connection -- /absolute/path/runtime.node ./embedding-profile`.

#[cfg(feature = "local-runtime")]
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use copilot_sdk::{
        AttachedInProcessRuntimeOptions, Client, ClientMode, ClientOptions,
        InProcessPluginHostOperation, InProcessProtocolConnectionOptions, InProcessRuntimeOptions,
        OwnedRuntimeOptions, RuntimeConnection,
    };
    use serde_json::json;

    let mut args = std::env::args_os().skip(1);
    let library = args.next().ok_or("supply the runtime.node path")?;
    let profile = std::path::absolute(args.next().ok_or("supply a private profile directory")?)?;
    let options = ClientOptions::new(ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::InProcess(InProcessRuntimeOptions {
            owned: OwnedRuntimeOptions::new().with_base_directory(&profile),
            runtime_library_path: Some(library.into()),
            ..Default::default()
        }))
        .with_github(|group| group.with_token("").with_use_logged_in_user(false));
    let owner = Client::start(options).await?;
    let engine = owner.in_process_server_id();
    owner
        .configure_in_process_server(
            json!({"processWorkingDirectory": profile})
                .as_object()
                .ok_or("server defaults must be an object")?,
        )
        .await?;
    owner
        .configure_in_process_server(
            json!({"eventsLogIncludesSubagents": true})
                .as_object()
                .ok_or("server defaults must be an object")?,
        )
        .await?;
    assert!(
        owner
            .configure_in_process_server(
                json!({"processWorkingDirectory": "must-not-apply", "zzUnknown": true})
                    .as_object()
                    .ok_or("server defaults must be an object")?,
            )
            .await
            .is_err()
    );
    assert_eq!(owner.in_process_server_id(), engine);
    let host_options = json!({
        "configOverride": {
            "extraKnownMarketplaces": {
                "sdk-host-witness": {"source": {"source": "github", "repo": "org/witness"}}
            }
        },
        "workingDirectory": profile,
    });
    let ordinary = owner
        .call("plugins.marketplaces.list", Some(host_options.clone()))
        .await?;
    assert!(
        !ordinary["marketplaces"]
            .as_array()
            .ok_or("missing ordinary marketplace inventory")?
            .iter()
            .any(|marketplace| marketplace["name"] == "sdk-host-witness"),
        "Ordinary wire calls must not acquire host-only options: {ordinary}"
    );
    assert!(
        owner
            .call_in_process_plugin_host(
                InProcessPluginHostOperation::ListMarketplaces,
                Some(host_options.clone()),
            )
            .await
            .is_err(),
        "The private SDK host must not replace canonical runtime configuration"
    );
    owner
        .call(
            "user.settings.set",
            Some(json!({"settings": host_options["configOverride"]})),
        )
        .await?;
    let hosted = owner
        .call_in_process_plugin_host(
            InProcessPluginHostOperation::ListMarketplaces,
            Some(json!({"workingDirectory": profile})),
        )
        .await?;
    assert!(
        hosted["marketplaces"]
            .as_array()
            .ok_or("missing host marketplace inventory")?
            .iter()
            .any(|marketplace| marketplace["name"] == "sdk-host-witness"),
        "The private SDK host must use canonical runtime configuration: {hosted}"
    );
    println!(
        "The private plugin host uses canonical settings; ordinary wire arity stays unchanged."
    );
    let created = owner
        .call(
            "session.create",
            Some(
                json!({"hooks": {"enableRepositoryHooks": false}, "enableConfigDiscovery": false}),
            ),
        )
        .await?;
    let session_id = created["sessionId"].as_str().ok_or("missing session ID")?;
    let metadata = owner
        .call(
            "session.metadata.snapshot",
            Some(json!({"sessionId": session_id})),
        )
        .await?;
    assert_eq!(metadata["workingDirectory"], json!(profile), "{metadata}");
    let retention = owner
        .call("session.retainView", Some(json!({"sessionId": session_id})))
        .await?;
    assert!(owner.in_process_is_session_retained(session_id).await?);
    assert!(
        owner
            .publish_in_process_foreground_session(Some(session_id))
            .await?
    );
    let clear = owner.publish_in_process_foreground_session(None);
    let publish = owner.publish_in_process_foreground_session(Some(session_id));
    let (published, cleared) = tokio::join!(publish, clear);
    assert!(cleared?);
    assert!(published?);
    assert_eq!(
        owner
            .get_foreground_session_id()
            .await?
            .as_ref()
            .map(|id| id.to_string()),
        Some(session_id.to_owned()),
    );
    println!("Foreground calls preserve submission order with reversed future polling.");
    assert!(owner.publish_in_process_foreground_session(None).await?);
    let owner_connection = owner
        .in_process_connection_id()
        .ok_or("missing SDK owner connection")?;
    assert!(
        !owner
            .in_process_has_session_connection(session_id, &[owner_connection])
            .await?
    );
    owner
        .call(
            "session.releaseView",
            Some(json!({"sessionId": session_id, "retentionId": retention["retentionId"]})),
        )
        .await?;
    assert!(!owner.in_process_is_session_retained(session_id).await?);
    let retention = owner
        .call("session.retainView", Some(json!({"sessionId": session_id})))
        .await?;
    owner
        .call("sessions.close", Some(json!({"sessionId": session_id})))
        .await?;
    owner
        .call(
            "session.releaseView",
            Some(json!({"sessionId": session_id, "retentionId": retention["retentionId"]})),
        )
        .await?;
    assert!(!owner.in_process_is_session_retained(session_id).await?);
    println!("Native defaults, foreground ownership, and view retention use the same SDK engine.");
    let first = owner
        .open_in_process_protocol_connection(InProcessProtocolConnectionOptions::default())
        .await?;
    let second = owner
        .open_in_process_protocol_connection(InProcessProtocolConnectionOptions::default())
        .await?;
    assert_ne!(first.connection_id(), second.connection_id());
    let body = json!({"jsonrpc": "2.0", "id": 1, "method": "ping",
                      "params": {"message": "first independent peer"}})
    .to_string();
    first.write(format!("Content-Length: {}\r\n\r\n{body}", body.len()).as_bytes())?;
    first.close().await?;
    let mut drained = String::new();
    while let Some(bytes) = first.next_outbound().await {
        drained.push_str(std::str::from_utf8(&bytes)?);
    }
    assert!(
        drained.contains("pong: first independent peer"),
        "{drained}"
    );
    println!("{drained}");
    assert!(!first.is_active());
    assert!(second.is_active());

    let body = json!({"jsonrpc": "2.0", "id": 2, "method": "ping",
                      "params": {"message": "second peer survives"}})
    .to_string();
    second.write(format!("Content-Length: {}\r\n\r\n{body}", body.len()).as_bytes())?;
    let bytes = second.next_outbound().await.ok_or("second peer closed")?;
    let reply = String::from_utf8(bytes)?;
    assert!(reply.contains("pong: second peer survives"), "{reply}");
    println!("{reply}");

    let borrowed_options = ClientOptions::new(ClientMode::CodingAgent).with_runtime(
        RuntimeConnection::AttachedInProcess(AttachedInProcessRuntimeOptions {
            server_id: owner.in_process_server_id().ok_or("missing SDK server")?,
            runtime_library_path: owner.in_process_library_path().ok_or("missing SDK image")?,
            session_binding: None,
            run_on_ui_thread: None,
            file_logging_path: None,
        }),
    );
    let borrowed = Client::start(borrowed_options).await?;
    let borrowed_peer = borrowed
        .open_in_process_protocol_connection(InProcessProtocolConnectionOptions::default())
        .await?;
    borrowed.stop().await?;
    borrowed_peer.close().await?;
    assert!(!borrowed_peer.is_active());
    assert!(borrowed_peer.next_outbound().await.is_none());
    assert!(second.is_active());
    second.close().await?;
    while second.next_outbound().await.is_some() {}
    owner.call("ping", Some(json!({}))).await?;
    println!("The SDK owner still answers after both raw peers and a borrowed client close.");
    owner.stop().await?;
    assert!(
        owner
            .call_in_process_plugin_host(InProcessPluginHostOperation::ListMarketplaces, None)
            .await
            .is_err(),
        "A stopped owner must not invoke a host operation"
    );
    Ok(())
}

#[cfg(not(feature = "local-runtime"))]
fn main() {
    eprintln!("Enable the local-runtime feature to run this embedding example.");
}
