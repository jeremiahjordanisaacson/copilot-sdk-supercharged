// Copyright (c) Microsoft Corporation. All rights reserved.

//! Receive process effects from the original SDK-owned engine.
//! Run with `cargo run --no-default-features --features local-runtime --example
//! embedder_lifecycle -- /absolute/path/runtime.node ./embedding-profile`.

#[cfg(feature = "local-runtime")]
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    use std::sync::{Arc, Mutex};
    use std::time::Duration;

    use copilot_sdk::{
        Client, ClientMode, ClientOptions, InProcessRuntimeOptions, OwnedRuntimeOptions,
        RuntimeConnection, rpc,
    };
    use serde_json::json;
    use tokio::sync::Notify;

    let mut args = std::env::args_os().skip(1);
    let library = args.next().ok_or("supply the runtime.node path")?;
    let profile = std::path::absolute(args.next().ok_or("supply a private profile directory")?)?;
    let options = ClientOptions::new(ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::InProcess(InProcessRuntimeOptions {
            owned: OwnedRuntimeOptions::new()
                .with_base_directory(&profile)
                .with_session_idle_timeout_seconds(1),
            runtime_library_path: Some(library.into()),
            ..Default::default()
        }))
        .with_github(|group| group.with_token("").with_use_logged_in_user(false));
    let owner = Client::start(options).await?;
    let phases = Arc::new(Mutex::new(Vec::new()));
    let idle = Arc::new(Notify::new());
    let finalized = Arc::new(Notify::new());
    for (method, phase, completed) in [
        ("embedder.prepareShutdown", "prepare", None),
        (
            "embedder.finalizeShutdown",
            "finalize",
            Some(finalized.clone()),
        ),
        ("embedder.managedIdle", "idle", Some(idle.clone())),
    ] {
        let phases = phases.clone();
        owner.register_request_handler(method, move |_, client| {
            let phases = phases.clone();
            let completed = completed.clone();
            async move {
                if phase == "prepare" {
                    let foreground = client
                        .get_foreground_session_id()
                        .await?
                        .expect("shutdown preparation retains the foreground session");
                    client
                        .call(
                            "session.metadata.snapshot",
                            Some(json!({"sessionId": foreground})),
                        )
                        .await?;
                }
                phases
                    .lock()
                    .expect("the phase recorder is not poisoned")
                    .push(phase);
                if let Some(completed) = completed {
                    completed.notify_one();
                }
                Ok(serde_json::Value::Null)
            }
        })?;
    }
    let displayed = phases.clone();
    owner.register_request_handler("embedder.setForeground", move |params, client| {
        let displayed = displayed.clone();
        async move {
            let request: rpc::EmbedderForegroundRequest = serde_json::from_value(params)?;
            assert!(request.native_session_active);
            assert!(
                client
                    .publish_in_process_foreground_session(Some(request.session_id.as_ref()))
                    .await?
            );
            displayed
                .lock()
                .expect("the phase recorder is not poisoned")
                .push("foreground");
            Ok(json!({"success": true}))
        }
    })?;
    owner.register_in_process_embedder_observer().await?;
    let created = owner
        .call(
            "session.create",
            Some(
                json!({"hooks": {"enableRepositoryHooks": false}, "enableConfigDiscovery": false}),
            ),
        )
        .await?;
    owner
        .call(
            "session.setForeground",
            Some(json!({"sessionId": created["sessionId"]})),
        )
        .await?;
    tokio::time::timeout(Duration::from_secs(10), idle.notified()).await?;
    assert!(
        owner
            .call(
                "session.metadata.snapshot",
                Some(json!({"sessionId": created["sessionId"]}))
            )
            .await
            .is_err(),
        "Native idle cleanup must finish before the embedding notification"
    );
    let created = owner
        .call(
            "session.create",
            Some(
                json!({"hooks": {"enableRepositoryHooks": false}, "enableConfigDiscovery": false}),
            ),
        )
        .await?;
    owner
        .call(
            "session.setForeground",
            Some(json!({"sessionId": created["sessionId"]})),
        )
        .await?;
    owner.call("runtime.shutdown", Some(json!({}))).await?;
    tokio::time::timeout(Duration::from_secs(10), finalized.notified()).await?;
    assert_eq!(
        *phases.lock().expect("the phase recorder is not poisoned"),
        ["foreground", "idle", "foreground", "prepare", "finalize"]
    );
    owner.stop().await?;
    assert_eq!(
        phases
            .lock()
            .expect("the phase recorder is not poisoned")
            .iter()
            .filter(|phase| **phase == "finalize")
            .count(),
        1
    );
    println!(
        "SDK foreground, native idle cleanup, preparation, and post-response finalization passed."
    );
    Ok(())
}

#[cfg(not(feature = "local-runtime"))]
fn main() {
    eprintln!("Enable the local-runtime feature to run this embedding example.");
}
