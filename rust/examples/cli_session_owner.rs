//! Exercise engine-owned CLI creation, callbacks, warm resume, and cold resume.
//!
//! Run with `--no-default-features --features local-runtime` and
//! `COPILOT_RUNTIME_CDYLIB` set to the same-checkout runtime addon.
//! This example does not run inference or upload telemetry.

#[cfg(feature = "local-runtime")]
mod example {
    use std::path::PathBuf;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::Duration;

    use async_trait::async_trait;
    use copilot_sdk::cli_host;
    use copilot_sdk::discovery::{DiscoveryPublisherConfig, DiscoveryRegistryKind};
    use copilot_sdk::handler::ElicitationHandler;
    use copilot_sdk::rpc::{
        HistoryImportConversationEvent, HistoryImportConversationEventUserMessage,
        HistoryImportUserMessage, SessionHistoryImportConversationParams,
        SessionMetadataSnapshotDetails, SessionOpenOptions, SessionOpenParams,
        SessionsCloseRequest, SessionsOpenAttach, UIElicitationPrompt,
    };
    use copilot_sdk::{
        Client, ClientMode, ClientOptions, CreateSessionOptions, ElicitationRequest,
        ElicitationResult, InProcessRuntimeOptions, OwnedRuntimeOptions, RequestId,
        ResumeSessionOptions, RuntimeConnection, SessionId,
    };
    use serde_json::json;
    use tokio::sync::Notify;

    struct UserInterface {
        callbacks: Arc<AtomicUsize>,
        entered: Arc<Notify>,
        release: Arc<Notify>,
    }

    #[async_trait]
    impl ElicitationHandler for UserInterface {
        async fn handle(
            &self,
            _session_id: SessionId,
            _request_id: RequestId,
            request: ElicitationRequest,
        ) -> ElicitationResult {
            assert_eq!(request.message, "Confirm the SDK callback route");
            self.callbacks.fetch_add(1, Ordering::SeqCst);
            println!("The engine invoked the SDK elicitation callback");
            self.entered.notify_one();
            self.release.notified().await;
            ElicitationResult {
                action: "accept".to_owned(),
                content: Some(json!({"confirmed": true})),
            }
        }
    }

    pub async fn run() -> Result<(), Box<dyn std::error::Error>> {
        let directory = tempfile::tempdir_in(env!("CARGO_MANIFEST_DIR"))?;
        let library = PathBuf::from(
            std::env::var_os("COPILOT_RUNTIME_CDYLIB")
                .ok_or("Set COPILOT_RUNTIME_CDYLIB to the same-checkout runtime addon")?,
        );
        let client_options = || {
            ClientOptions::new(ClientMode::CodingAgent)
                .with_runtime(RuntimeConnection::InProcess(InProcessRuntimeOptions {
                    owned: OwnedRuntimeOptions::new().with_base_directory(directory.path()),
                    runtime_library_path: Some(library.clone()),
                    ..Default::default()
                }))
                .with_github(|group| group.with_token("").with_use_logged_in_user(false))
                .with_observability(|group| group.with_disable_client_telemetry(true))
        };
        let client = Client::start(client_options()).await?;
        let result = async {
            let callbacks = Arc::new(AtomicUsize::new(0));
            let entered = Arc::new(Notify::new());
            let release = Arc::new(Notify::new());
            let cli_options: SessionOpenOptions = serde_json::from_value(json!({
                "clientKind": "cli",
                "name": "Engine-owned CLI example",
                "workingDirectory": directory.path(),
                "hooks": {"enableRepositoryHooks": false},
                "remoteSteerable": false,
                "git": {"coauthorEnabled": false},
            }))?;
            let prepared = cli_host::prepare_session(
                &client,
                cli_options,
                CreateSessionOptions::default().with_interaction(|group| {
                    group.with_on_elicitation_request(Arc::new(UserInterface {
                        callbacks: callbacks.clone(),
                        entered: entered.clone(),
                        release: release.clone(),
                    }))
                }),
            )?;
            let mut events = prepared.subscribe();
            let session = prepared.start().await?;
            let id = session.id().clone();
            let start = tokio::time::timeout(Duration::from_secs(30), async {
                loop {
                    let event = events.recv().await?;
                    if event.event_type == "session.start" {
                        return Ok::<_, Box<dyn std::error::Error>>(event);
                    }
                }
            })
            .await??;
            assert_eq!(start.data["sessionId"], id.as_str());
            println!("The SDK observed session.start before attachment");
            let imported = "A conversation imported through a typed SDK variant";
            session
                .rpc()
                .history()
                .import_conversation(SessionHistoryImportConversationParams {
                    events: vec![HistoryImportConversationEvent::UserMessage(
                        HistoryImportConversationEventUserMessage {
                            data: HistoryImportUserMessage {
                                content: imported.to_owned(),
                                attachments: None,
                            },
                            r#type: Default::default(),
                        },
                    )],
                })
                .await?;
            assert!(session.get_events().await?.iter().any(|event| {
                event.event_type == "user.message" && event.data["content"] == imported
            }));
            println!("The SDK imported a typed conversation event into canonical history");
            for (call_id, arguments) in [
                ("typed-null-arguments", Some(serde_json::Value::Null)),
                ("typed-omitted-arguments", None),
            ] {
                let mut request = json!({"toolCallId": call_id, "name": "imported_probe"});
                let mut invocation = json!({"toolCallId": call_id, "toolName": "imported_probe"});
                if let Some(arguments) = &arguments {
                    request["arguments"] = arguments.clone();
                    invocation["arguments"] = arguments.clone();
                }
                let conversation = serde_json::from_value::<SessionHistoryImportConversationParams>(
                    json!({"events": [
                        {
                            "type": "assistant.message",
                            "data": {
                                "messageId": call_id,
                                "content": "An imported tool request",
                                "toolRequests": [request],
                            },
                        },
                        {"type": "tool.execution_start", "data": invocation},
                        {
                            "type": "tool.execution_complete",
                            "data": {
                                "toolCallId": call_id,
                                "success": true,
                                "result": {"content": "done"},
                            },
                        },
                    ]}),
                )?;
                session
                    .rpc()
                    .history()
                    .import_conversation(conversation)
                    .await?;
                let events = session.get_events().await?;
                let message = events
                    .iter()
                    .find(|event| {
                        event.event_type == "assistant.message"
                            && event.data["messageId"] == call_id
                    })
                    .expect("the engine retains the imported assistant message");
                assert_eq!(
                    message.data["toolRequests"][0].get("arguments"),
                    arguments.as_ref()
                );
                let invocation = events
                    .iter()
                    .find(|event| {
                        event.event_type == "tool.execution_start"
                            && event.data["toolCallId"] == call_id
                    })
                    .expect("the engine retains the imported tool invocation");
                assert_eq!(invocation.data.get("arguments"), arguments.as_ref());
            }
            println!("The SDK preserves null and omitted tool arguments in canonical history");
            let (response, ()) = tokio::try_join!(
                async {
                    Ok::<_, Box<dyn std::error::Error>>(
                        session
                            .rpc()
                            .ui()
                            .elicitation(serde_json::from_value(json!({
                                "message": "Confirm the SDK callback route",
                                "requestedSchema": {
                                    "type": "object",
                                    "properties": {"confirmed": {"type": "boolean"}},
                                    "required": ["confirmed"],
                                },
                            }))?)
                            .await?,
                    )
                },
                async {
                    entered.notified().await;
                    let pending = session.rpc().ui().snapshot().await;
                    release.notify_one();
                    let pending = pending?;
                    let [request] = pending.pending_elicitation_requests.as_slice() else {
                        return Err("The engine must expose exactly one pending elicitation".into());
                    };
                    let UIElicitationPrompt::Form(form) = &request.request else {
                        return Err("The engine must expose a typed form prompt".into());
                    };
                    assert_eq!(form.message, "Confirm the SDK callback route");
                    println!("The SDK reads a typed form prompt from the canonical UI snapshot");
                    Ok::<_, Box<dyn std::error::Error>>(())
                },
            )?;
            let response = serde_json::to_value(response)?;
            assert_eq!(response["action"], "accept");
            assert_eq!(response["content"]["confirmed"], true);
            assert_eq!(callbacks.load(Ordering::SeqCst), 1);
            let ui_snapshot = session.rpc().ui().snapshot().await?;
            assert!(ui_snapshot.pending_elicitation_requests.is_empty());
            assert_eq!(ui_snapshot.has_pending_repo_hooks, Some(false));
            println!("The SDK reads completed UI requests from the engine");
            println!("The SDK reads pending repository hooks from the engine: false");
            let snapshot = session.rpc().metadata().snapshot().await?;
            assert_eq!(snapshot.session_id, id);
            assert_eq!(
                snapshot.initial_name.as_deref(),
                Some("Engine-owned CLI example")
            );
            let details: SessionMetadataSnapshotDetails = serde_json::from_value(
                client
                    .call(
                        "session.metadata.snapshot",
                        Some(json!({"sessionId": id.as_str()})),
                    )
                    .await?,
            )?;
            let runtime_instance_id = details
                .runtime_instance_id
                .ok_or("The runtime must identify its live session generation")?;
            println!("The SDK reads extended metadata without changing released Rust records");
            client
                .rpc()
                .sessions()
                .open(SessionOpenParams::Attach(SessionsOpenAttach {
                    session_id: id.clone(),
                    ..Default::default()
                }))
                .await?;
            let attached: SessionMetadataSnapshotDetails = serde_json::from_value(
                client
                    .call(
                        "session.metadata.snapshot",
                        Some(json!({"sessionId": id.as_str()})),
                    )
                    .await?,
            )?;
            assert_eq!(
                attached.runtime_instance_id.as_deref(),
                Some(runtime_instance_id.as_str())
            );
            println!("The SDK preserves runtime identity across live attachment");
            let listener = std::net::TcpListener::bind(("127.0.0.1", 0))?;
            let discovery_home = directory.path().join("discovery");
            let discovery =
                client.create_in_process_discovery_publisher(DiscoveryPublisherConfig {
                    kind: DiscoveryRegistryKind::ManagedServer,
                    host: "127.0.0.1".to_owned(),
                    token: None,
                    pid: u64::from(std::process::id()),
                    copilot_version: env!("CARGO_PKG_VERSION").to_owned(),
                    copilot_home: Some(discovery_home.to_string_lossy().into_owned()),
                    home: directory.path().to_string_lossy().into_owned(),
                })?;
            discovery
                .start(
                    u32::from(listener.local_addr()?.port()),
                    snapshot.start_time,
                )
                .await?;
            discovery
                .set_session_and_status(1, id.as_str(), Some("waiting".to_owned()))
                .await?;
            let entry_path = discovery_home
                .join("servers")
                .join(format!("{}.json", std::process::id()));
            let entry: serde_json::Value = serde_json::from_slice(&std::fs::read(&entry_path)?)?;
            assert_eq!(entry["sessionId"], id.as_str());
            assert_eq!(entry["sessionName"], "Engine-owned CLI example");
            discovery.stop().await?;
            assert!(!entry_path.exists());
            drop(listener);
            println!("The SDK published and removed canonical discovery metadata");
            let first = client
                .call(
                    "session.retainView",
                    Some(json!({"sessionId": id.as_str()})),
                )
                .await?;
            let second = client
                .call(
                    "session.retainView",
                    Some(json!({"sessionId": id.as_str()})),
                )
                .await?;
            assert_ne!(first["retentionId"], second["retentionId"]);
            for view in [first, second] {
                let released = client
                    .call(
                        "session.releaseView",
                        Some(json!({"sessionId": id.as_str(), "retentionId": view["retentionId"]})),
                    )
                    .await?;
                assert_eq!(released["released"], true);
            }
            println!("The engine retained and released two independent terminal views");
            session.disconnect().await?;

            let warm = cli_host::prepare_resume_session(
                &client,
                SessionOpenOptions::default(),
                ResumeSessionOptions::new(id.clone()),
            )?
            .start()
            .await?;
            assert_eq!(warm.id(), &id);
            let details: SessionMetadataSnapshotDetails = serde_json::from_value(
                client
                    .call(
                        "session.metadata.snapshot",
                        Some(json!({"sessionId": id.as_str()})),
                    )
                    .await?,
            )?;
            let resumed_runtime_instance_id = details
                .runtime_instance_id
                .ok_or("The runtime must identify the reconstructed session generation")?;
            assert_ne!(resumed_runtime_instance_id, runtime_instance_id);
            println!("The SDK distinguishes reconstruction during warm resume");
            let live = client.call("sessions.listActive", None).await?;
            assert!(
                live["sessionIds"]
                    .as_array()
                    .ok_or("The live session inventory is unavailable")?
                    .iter()
                    .any(|candidate| candidate.as_str() == Some(id.as_str()))
            );
            println!("The SDK reads live membership from the canonical engine");
            client
                .rpc()
                .sessions()
                .close(SessionsCloseRequest {
                    session_id: id.clone(),
                    detach_session_end_hooks: None,
                })
                .await?;
            let live = client.call("sessions.listActive", None).await?;
            assert!(
                !live["sessionIds"]
                    .as_array()
                    .ok_or("The live session inventory is unavailable")?
                    .iter()
                    .any(|candidate| candidate.as_str() == Some(id.as_str()))
            );
            println!("The engine removed the closed session from live membership");
            drop(warm);
            println!("The original engine completed warm resume and close");
            Ok::<_, Box<dyn std::error::Error>>((id, resumed_runtime_instance_id))
        }
        .await;
        let stopped = client.stop().await;
        let (id, runtime_instance_id) = match result {
            Ok(identity) => {
                stopped?;
                identity
            }
            Err(error) => {
                if let Err(shutdown) = stopped {
                    eprintln!("Runtime shutdown also failed: {shutdown}");
                }
                return Err(error);
            }
        };
        let restarted = Client::start(client_options()).await?;
        let result = async {
            let cold = cli_host::prepare_resume_session(
                &restarted,
                SessionOpenOptions::default(),
                ResumeSessionOptions::new(id.clone()),
            )?
            .start()
            .await?;
            let snapshot = cold.rpc().metadata().snapshot().await?;
            assert_eq!(snapshot.session_id, id);
            let details: SessionMetadataSnapshotDetails = serde_json::from_value(
                restarted
                    .call(
                        "session.metadata.snapshot",
                        Some(json!({"sessionId": id.as_str()})),
                    )
                    .await?,
            )?;
            let cold_runtime_instance_id = details
                .runtime_instance_id
                .ok_or("The runtime must identify the restored session generation")?;
            assert_ne!(cold_runtime_instance_id, runtime_instance_id);
            println!("The SDK distinguishes cold resume from live attachment");
            assert_eq!(
                snapshot
                    .workspace
                    .as_ref()
                    .and_then(|workspace| workspace.name.as_deref()),
                Some("Engine-owned CLI example")
            );
            println!("A new engine restored the named session from native persistence");
            Ok::<_, Box<dyn std::error::Error>>(())
        }
        .await;
        let stopped = restarted.stop().await;
        result?;
        stopped?;
        Ok(())
    }
}

#[cfg(feature = "local-runtime")]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    example::run().await
}

#[cfg(not(feature = "local-runtime"))]
fn main() {
    eprintln!("Use --no-default-features --features local-runtime");
}
