use copilot_sdk::{RuntimeConnection, SpawnedProcessOptions, SpawnedTcpRuntimeOptions};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use copilot_sdk::canvas::CanvasDeclaration;
use copilot_sdk::rpc::{ConnectRemoteSessionParams, OpenCanvasInstance, RemoteSessionMode};
use copilot_sdk::session_events::{
    ReasoningSummary, SessionEventType, SessionLimitsConfig, SessionStartData,
};
use copilot_sdk::{
    AgentMode, Attachment, AttachmentLineRange, AttachmentSelectionPosition,
    AttachmentSelectionRange, CliProgram, Client, ClientMode, ClientOptions, CloudSessionOptions,
    CloudSessionRepository, CopilotExpAssignmentResponse, CreateSessionOptions, DeliveryMode,
    ExtensionInfo, GitHubReferenceType, MessageOptions, MessageSource, ResumeSessionOptions,
    SessionId, TranscriptRecovery,
};
use serde::Deserialize;
use serde_json::{Value, json};
use tempfile::TempDir;

#[path = "../support/node_fixture.rs"]
mod node_fixture;

#[cfg(windows)]
#[path = "client_options/windows_process_tree.rs"]
mod windows_process_tree;

#[tokio::test]
async fn spawned_connections_consume_process_cwd_environment_and_arguments() {
    let host_cwd = std::env::current_dir().expect("host cwd");
    let host_value = std::env::var_os("COPILOT_SDK_PROCESS_TEST");
    for tcp in [false, true] {
        let fake = FakeCli::new();
        let mut options = fake
            .client_options("typed-token")
            .with_observability(|group| group.with_log_level(copilot_sdk::LogLevel::Debug));
        let Some(RuntimeConnection::SpawnedStdio(mut process)) = options.runtime.take() else {
            panic!("fake CLI must configure a spawned process");
        };
        process.args.push("--fixture-argument=hello world ☃".into());
        process.env = Some(
            std::env::vars_os()
                .chain([
                    ("COPILOT_SDK_PROCESS_TEST".into(), "first".into()),
                    ("COPILOT_SDK_PROCESS_TEST".into(), "last".into()),
                    ("COPILOT_SDK_AUTH_TOKEN".into(), "overlaid-token".into()),
                ])
                .collect(),
        );
        process.env_remove.push("COPILOT_SDK_AUTH_TOKEN".into());
        let arguments: Vec<_> = process
            .args
            .iter()
            .skip(1)
            .map(|arg| arg.to_str().expect("fixture argument").to_owned())
            .collect();
        options.runtime = Some(if tcp {
            RuntimeConnection::SpawnedTcp(SpawnedTcpRuntimeOptions {
                process,
                port: 0,
                connection_token: None,
            })
        } else {
            RuntimeConnection::SpawnedStdio(process)
        });
        let client = Client::start(options)
            .await
            .expect("start configured child");
        let session_directory = fake.path("session-workspace");
        let session = client
            .create_session(
                CreateSessionOptions::default().with_working_directory(&session_directory),
            )
            .await
            .expect("create session");
        session.disconnect().await.expect("disconnect session");
        client.stop().await.expect("stop configured child");

        let capture = fake.capture();
        assert_eq!(
            capture.cwd.canonicalize().unwrap(),
            fake.work_dir.canonicalize().unwrap()
        );
        assert_eq!(
            client.cwd().canonicalize().unwrap(),
            fake.work_dir.canonicalize().unwrap()
        );
        assert!(capture.args.starts_with(&arguments), "{:?}", capture.args);
        assert_eq!(capture.args[arguments.len()], "--server");
        assert!(
            capture
                .args
                .windows(2)
                .any(|args| args == ["--log-level", "debug"])
        );
        assert_eq!(
            capture
                .env
                .get("COPILOT_SDK_PROCESS_TEST")
                .map(String::as_str),
            Some("last")
        );
        assert_eq!(
            capture
                .env
                .get("COPILOT_SDK_AUTH_TOKEN")
                .map(String::as_str),
            Some("typed-token"),
        );
        assert_eq!(
            capture.env.get("PATH").map(std::ffi::OsStr::new),
            std::env::var_os("PATH").as_deref()
        );
        let create = fake.captured_request("session.create");
        assert!(
            !create
                .params
                .as_object()
                .unwrap()
                .contains_key("remoteSession")
        );
        assert_eq!(
            PathBuf::from(
                create.params["workingDirectory"]
                    .as_str()
                    .expect("session cwd")
            )
            .canonicalize()
            .unwrap(),
            session_directory.canonicalize().unwrap(),
        );
        assert_eq!(std::env::current_dir().unwrap(), host_cwd);
        assert_eq!(std::env::var_os("COPILOT_SDK_PROCESS_TEST"), host_value);
    }
}

#[tokio::test]
async fn spawned_environment_replacement_controls_inheritance_and_runtime_selection() {
    let node = std::process::Command::new("node")
        .args(["-p", "process.execPath"])
        .output()
        .expect("locate the fake runtime's interpreter");
    assert!(node.status.success(), "{:?}", node);
    let node = PathBuf::from(String::from_utf8(node.stdout).unwrap().trim());
    let inherited_path = std::env::var_os("PATH");
    #[cfg(windows)]
    let program = PathBuf::from(std::env::var_os("ComSpec").expect("Windows command processor"));
    #[cfg(not(windows))]
    let program = node.clone();
    let relative_program = PathBuf::from(if cfg!(windows) { "cmd.exe" } else { "node" });

    for environment in [
        None,
        Some(vec![]),
        Some(vec![(
            "COPILOT_CLI_PATH".into(),
            program.as_os_str().to_owned(),
        )]),
        Some(vec![(
            "COPILOT_CLI_PATH".into(),
            relative_program.as_os_str().to_owned(),
        )]),
    ] {
        let inherits = environment.is_none();
        let resolves_from_environment = environment.as_ref().is_some_and(|env| !env.is_empty());
        let relative_override = environment
            .as_ref()
            .and_then(|env| env.first())
            .is_some_and(|(_, path)| std::path::Path::new(path).is_relative());
        let fake = FakeCli::new();
        if relative_override {
            let copied = std::process::Command::new(&node)
                .args([
                    "-e",
                    "require('node:fs').copyFileSync(process.argv[1], process.argv[2])",
                ])
                .arg(&program)
                .arg(fake.work_dir.join(&relative_program))
                .output()
                .expect("copy the interpreter from an isolated process");
            assert!(copied.status.success(), "{copied:?}");
        }
        let mut options = fake.client_options("typed-token");
        let Some(RuntimeConnection::SpawnedStdio(process)) = &mut options.runtime else {
            panic!("fake CLI must configure a spawned process");
        };
        process.program = if resolves_from_environment {
            CliProgram::Resolve
        } else {
            CliProgram::Path(program.clone())
        };
        #[cfg(windows)]
        {
            // Node 24 needs SystemRoot for CSPRNG initialization. Set it inside
            // the fixture, preserving a genuinely empty SDK environment map.
            process.args.splice(
                0..0,
                [
                    "/d".into(),
                    "/c".into(),
                    "set".into(),
                    format!(
                        "SystemRoot={}&&",
                        std::env::var("SystemRoot").expect("Windows system directory")
                    )
                    .into(),
                    node.as_os_str().to_owned(),
                ],
            );
        }
        process.env = environment;

        let client = Client::start(options).await.unwrap_or_else(|error| {
            let capture = std::fs::read_to_string(&fake.capture_path);
            panic!(
                "start configured child (inherits={inherits}, resolves_from_environment={resolves_from_environment}, relative_override={relative_override}, capture={capture:?}): {error:?}"
            )
        });
        client.stop().await.expect("stop configured child");

        let capture = fake.capture();
        assert_eq!(
            capture.env.get("PATH").map(std::ffi::OsStr::new),
            if inherits {
                inherited_path.as_deref()
            } else {
                None
            },
        );
        assert_eq!(
            capture
                .env
                .get("COPILOT_SDK_AUTH_TOKEN")
                .map(String::as_str),
            Some("typed-token"),
        );
    }
}

#[tokio::test]
async fn spawned_process_empty_cwd_uses_host_directory() {
    let fake = FakeCli::new();
    let mut options = fake.client_options("cwd-fallback-token");
    let Some(RuntimeConnection::SpawnedStdio(process)) = &mut options.runtime else {
        panic!("fake CLI must configure a spawned process");
    };
    process.working_directory.clear();
    let client = Client::start(options)
        .await
        .expect("start child in host cwd");
    client.stop().await.expect("stop child");
    assert_eq!(
        fake.capture().cwd.canonicalize().unwrap(),
        std::env::current_dir().unwrap().canonicalize().unwrap(),
    );
}

#[tokio::test]
#[allow(deprecated)]
async fn should_forward_advanced_session_creation_options_to_the_cli() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options("advanced-create-client-token"))
        .await
        .expect("start fake CLI client");

    let config_dir = fake.path("config");
    let working_dir = fake.path("workspace");
    let extension_sdk_path = fake.path("extension-sdk");
    let session = client
        .create_session(
            CreateSessionOptions::default()
                .with_session_id("advanced-session-id")
                .with_name("advanced session")
                .with_client_name("rust-sdk-e2e-client")
                .with_model("claude-sonnet-5")
                .with_inference(|group| group.with_reasoning_effort("low"))
                .with_inference(|group| group.with_reasoning_summary(ReasoningSummary::None))
                .with_inference(|group| group.with_context_tier("long_context"))
                .with_config_directory(config_dir.clone())
                .with_enable_config_discovery(true)
                .with_state(|group| group.with_skip_embedding_retrieval(true))
                .with_state(|group| group.with_embedding_cache_storage("in-memory"))
                .with_instructions(|group| group.with_organization_text("organization guidance"))
                .with_instructions(|group| group.with_on_demand_discovery(true))
                .with_hooks(|group| group.with_enable_repository_hooks(true))
                .with_git(|group| group.with_enable_host_git_operations(false))
                .with_state(|group| group.with_enable_cross_session_store(false))
                .with_skills(|group| group.with_enabled(false))
                .with_working_directory(working_dir.clone())
                .with_remote_session(RemoteSessionMode::Export)
                .with_events(|group| group.with_streaming(true))
                .with_events(|group| group.with_include_sub_agent_streaming_events(false))
                .with_tools(|group| group.with_available_tools(["read_file"]))
                .with_tools(|group| group.with_excluded_tools(["bash"]))
                .with_agents(|group| group.with_excluded_builtin_agents(["legacy-agent"]))
                .with_github(|group| group.with_telemetry_enabled(false))
                .with_inference(|group| group.with_enable_citations(true))
                .with_execution(|group| {
                    group.with_limits(SessionLimitsConfig {
                        max_ai_credits: Some(42.0),
                    })
                })
                .with_instructions(|group| group.with_skip_custom_instructions(true))
                .with_agents(|group| group.with_local_discovery_only(true))
                .with_git(|group| group.with_coauthor_enabled(false))
                .with_tools(|group| group.with_manage_schedule_enabled(false))
                .with_github(|group| group.with_token("advanced-create-session-token"))
                .with_skills(|group| group.with_directories([PathBuf::from("skills")]))
                .with_plugins(|group| group.with_directories([PathBuf::from("plugins")]))
                .with_instructions(|group| group.with_directories([PathBuf::from("instructions")]))
                .with_skills(|group| group.with_disabled_skills(["disabled-skill"]))
                .with_mcp(|group| group.with_enable_apps(true))
                .with_canvases(|group| {
                    group.with_definitions([CanvasDeclaration::new(
                        "canvas",
                        "Canvas",
                        "Canvas description",
                    )])
                })
                .with_canvases(|group| group.with_request_renderer(true))
                .with_extensions(|group| group.with_enabled(true))
                .with_extensions(|group| group.with_sdk_path(path_string(&extension_sdk_path)))
                .with_extensions(|group| {
                    group.with_identity(ExtensionInfo::new("github-app", "rust-e2e-extension"))
                })
                .with_experiments(|group| {
                    group.with_assignments(CopilotExpAssignmentResponse {
                        flights: HashMap::from([("feature".to_string(), "enabled".to_string())]),
                        assignment_context: "ctx".to_string(),
                        ..Default::default()
                    })
                }),
        )
        .await
        .expect("create session");
    session.disconnect().await.expect("disconnect session");
    client.stop().await.expect("stop client");

    let create = fake.captured_request("session.create");
    let params = create.params.as_object().expect("session.create params");
    assert_json_values(
        params,
        [
            ("sessionId", json!("advanced-session-id")),
            ("name", json!("advanced session")),
            ("clientName", json!("rust-sdk-e2e-client")),
            ("model", json!("claude-sonnet-5")),
            ("inference.reasoningEffort", json!("low")),
            ("inference.reasoningSummary", json!("none")),
            ("inference.contextTier", json!("long_context")),
            ("configDirectory", json!(path_string(&config_dir))),
            ("enableConfigDiscovery", json!(true)),
            ("state.skipEmbeddingRetrieval", json!(true)),
            ("state.embeddingCacheStorage", json!("in-memory")),
            (
                "instructions.organizationText",
                json!("organization guidance"),
            ),
            ("instructions.onDemandDiscovery", json!(true)),
            ("hooks.enableRepositoryHooks", json!(true)),
            ("git.enableHostGitOperations", json!(false)),
            ("state.enableCrossSessionStore", json!(false)),
            ("skills.enabled", json!(false)),
            ("workingDirectory", json!(path_string(&working_dir))),
            ("remoteSession", json!("export")),
            ("events.streaming", json!(true)),
            ("events.includeSubAgentStreamingEvents", json!(false)),
            ("github.telemetryEnabled", json!(false)),
            ("inference.enableCitations", json!(true)),
            ("github.token", json!("advanced-create-session-token")),
            ("mcp.enableApps", json!(true)),
            ("canvases.requestRenderer", json!(true)),
            ("extensions.enabled", json!(true)),
            (
                "extensions.sdkPath",
                json!(path_string(&extension_sdk_path)),
            ),
            ("mcp.envValueMode", json!("direct")),
        ],
    );
    assert_eq!(params["tools"]["availableTools"], json!(["read_file"]));
    assert_eq!(params["tools"]["excludedTools"], json!(["bash"]));
    assert_eq!(
        params["agents"]["excludedBuiltinAgents"],
        json!(["legacy-agent"])
    );
    assert_eq!(params["skills"]["directories"], json!(["skills"]));
    assert_eq!(params["plugins"]["directories"], json!(["plugins"]));
    assert_eq!(
        params["instructions"]["directories"],
        json!(["instructions"])
    );
    assert_eq!(
        params["skills"]["disabledSkills"],
        json!(["disabled-skill"])
    );
    assert_eq!(params["execution"]["limits"]["maxAiCredits"], json!(42));
    assert_eq!(
        params["extensions"]["identity"],
        json!({ "source": "github-app", "name": "rust-e2e-extension" })
    );
    assert_eq!(params["canvases"]["definitions"][0]["id"], json!("canvas"));
    assert_eq!(
        params["canvases"]["definitions"][0]["displayName"],
        json!("Canvas")
    );
    assert_eq!(
        params["canvases"]["definitions"][0]["description"],
        json!("Canvas description")
    );
    assert_eq!(
        params["experiments"]["assignments"]["Flights"]["feature"],
        json!("enabled")
    );

    let update = fake.captured_request("session.options.update");
    let update_params = update.params.as_object().expect("options update params");
    assert_json_values(
        update_params,
        [
            ("sessionId", json!("advanced-session-id")),
            ("instructions.skipCustomInstructions", json!(true)),
            ("agents.localDiscoveryOnly", json!(true)),
            ("git.coauthorEnabled", json!(false)),
            ("tools.manageScheduleEnabled", json!(false)),
        ],
    );
}

#[tokio::test]
async fn resume_transcript_recovery_defaults_overrides_and_projection() {
    for mode in [ClientMode::Empty, ClientMode::CodingAgent] {
        for choice in [None, Some(false), Some(true)] {
            let fake = FakeCli::new();
            let reports_recovery = choice.unwrap_or(true);
            let behavior = if reports_recovery {
                "transcript-recovery"
            } else {
                "normal"
            };
            let mut options = fake
                .client_options_with_behavior("recovery-client-token", behavior)
                .with_mode(mode);
            options
                .runtime
                .as_mut()
                .unwrap()
                .owned_options_mut()
                .unwrap()
                .base_directory = Some(fake.path("state"));
            let client = Client::start(options).await.expect("start fake CLI client");
            let mut config = ResumeSessionOptions::new("recovery-session".into())
                .with_tools(|group| group.with_available_tools(Vec::<String>::new()));
            if let Some(allow) = choice {
                config = config.with_allow_transcript_recovery(allow);
            }
            let session = client.resume_session(config).await.expect("resume session");
            session.disconnect().await.expect("disconnect session");
            client.stop().await.expect("stop client");

            let request = fake.captured_request("session.resume");
            let expected = choice.map(Value::Bool);
            assert!(
                !request
                    .params
                    .as_object()
                    .unwrap()
                    .contains_key("remoteSession")
            );
            assert_eq!(
                request.params.get("allowTranscriptRecovery"),
                expected.as_ref(),
                "mode: {mode:?}, choice: {choice:?}"
            );
            let recovery = reports_recovery.then(|| TranscriptRecovery {
                planned_backup_path: "recovery-backup.jsonl".into(),
                invalid_line_numbers: vec![3, 5],
                session_start_moved: true,
            });
            assert_eq!(session.transcript_recovery(), recovery.as_ref());
        }
    }
}

#[tokio::test]
#[allow(deprecated)]
async fn should_forward_advanced_session_resume_options_to_the_cli() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options("advanced-resume-client-token"))
        .await
        .expect("start fake CLI client");

    let config_dir = fake.path("resume-config");
    let working_dir = fake.path("resume-workspace");
    let extension_sdk_path = fake.path("resume-extension-sdk");
    let session = client
        .resume_session(
            ResumeSessionOptions::new(SessionId::from("resume-session-id"))
                .with_model("gpt-5-mini")
                .with_inference(|group| group.with_reasoning_effort("low"))
                .with_inference(|group| group.with_reasoning_summary(ReasoningSummary::None))
                .with_inference(|group| group.with_context_tier("long_context"))
                .with_working_directory(working_dir.clone())
                .with_remote_session(RemoteSessionMode::Export)
                .with_config_directory(config_dir.clone())
                .with_enable_config_discovery(false)
                .with_hooks(|group| group.with_enable_repository_hooks(true))
                .with_suppress_resume_event(true)
                .with_continue_pending_work(false)
                .with_events(|group| group.with_streaming(true))
                .with_events(|group| group.with_include_sub_agent_streaming_events(false))
                .with_github(|group| group.with_token("advanced-resume-session-token"))
                .with_canvases(|group| {
                    group.with_definitions([CanvasDeclaration::new(
                        "resume-canvas",
                        "Resume Canvas",
                        "Resume canvas description",
                    )])
                })
                .with_canvases(|group| {
                    group.with_open([OpenCanvasInstance {
                        canvas_id: "resume-canvas".to_string(),
                        extension_id: "github-app/rust-e2e-extension".to_string(),
                        extension_name: None,
                        icon: None,
                        input: Some(json!({ "value": "from-resume" })),
                        instance_id: "resume-instance".to_string(),
                        status: None,
                        title: None,
                        url: None,
                    }])
                })
                .with_canvases(|group| group.with_request_renderer(true))
                .with_extensions(|group| group.with_enabled(true))
                .with_extensions(|group| group.with_sdk_path(path_string(&extension_sdk_path)))
                .with_extensions(|group| {
                    group.with_identity(ExtensionInfo::new("github-app", "rust-e2e-extension"))
                })
                .with_instructions(|group| group.with_skip_custom_instructions(true))
                .with_agents(|group| group.with_local_discovery_only(true))
                .with_git(|group| group.with_coauthor_enabled(false))
                .with_tools(|group| group.with_manage_schedule_enabled(false))
                .with_experiments(|group| {
                    group.with_assignments(CopilotExpAssignmentResponse {
                        flights: HashMap::from([(
                            "resumeFeature".to_string(),
                            "enabled".to_string(),
                        )]),
                        assignment_context: "ctx".to_string(),
                        ..Default::default()
                    })
                }),
        )
        .await
        .expect("resume session");
    session.disconnect().await.expect("disconnect session");
    client.stop().await.expect("stop client");

    let resume = fake.captured_request("session.resume");
    let params = resume.params.as_object().expect("session.resume params");
    assert_json_values(
        params,
        [
            ("sessionId", json!("resume-session-id")),
            ("model", json!("gpt-5-mini")),
            ("inference.reasoningEffort", json!("low")),
            ("inference.reasoningSummary", json!("none")),
            ("inference.contextTier", json!("long_context")),
            ("workingDirectory", json!(path_string(&working_dir))),
            ("remoteSession", json!("export")),
            ("configDirectory", json!(path_string(&config_dir))),
            ("enableConfigDiscovery", json!(false)),
            ("hooks.enableRepositoryHooks", json!(true)),
            ("suppressResumeEvent", json!(true)),
            ("continuePendingWork", json!(false)),
            ("events.streaming", json!(true)),
            ("events.includeSubAgentStreamingEvents", json!(false)),
            ("github.token", json!("advanced-resume-session-token")),
            ("canvases.requestRenderer", json!(true)),
            ("extensions.enabled", json!(true)),
            (
                "extensions.sdkPath",
                json!(path_string(&extension_sdk_path)),
            ),
            ("mcp.envValueMode", json!("direct")),
        ],
    );
    assert_eq!(
        params["canvases"]["open"][0]["canvasId"],
        json!("resume-canvas")
    );
    assert_eq!(
        params["canvases"]["open"][0]["extensionId"],
        json!("github-app/rust-e2e-extension")
    );
    assert_eq!(
        params["canvases"]["open"][0]["instanceId"],
        json!("resume-instance")
    );
    assert_eq!(
        params["extensions"]["identity"],
        json!({ "source": "github-app", "name": "rust-e2e-extension" })
    );
    assert_eq!(
        params["experiments"]["assignments"]["Flights"]["resumeFeature"],
        json!("enabled")
    );

    let update = fake.captured_request("session.options.update");
    let update_params = update.params.as_object().expect("options update params");
    assert_json_values(
        update_params,
        [
            ("sessionId", json!("resume-session-id")),
            ("instructions.skipCustomInstructions", json!(true)),
            ("agents.localDiscoveryOnly", json!(true)),
            ("git.coauthorEnabled", json!(false)),
            ("tools.manageScheduleEnabled", json!(false)),
        ],
    );
}

#[tokio::test]
async fn should_send_complete_message_wire_shape() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options("message-wire-client-token"))
        .await
        .expect("start fake CLI client");
    let session = client
        .create_session(CreateSessionOptions::default())
        .await
        .expect("create session");
    let file_path = fake.path("message-file").join("scenario.txt");
    let directory_path = fake.path("message-directory");
    let selection_path = fake.path("selection").join("Program.rs");

    let message_id = session
        .send(
            MessageOptions::new("Use the hidden scenario context.")
                .with_display_prompt("Review selected scenario context")
                .with_mode(DeliveryMode::Enqueue)
                .with_agent_mode(AgentMode::Interactive)
                .with_source(MessageSource::Agent("scenario-client".to_string()))
                .with_traceparent("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
                .with_tracestate("scenario-client=send")
                .with_attachments(vec![
                    Attachment::File {
                        path: file_path.clone(),
                        display_name: Some("scenario.txt".to_string()),
                        line_range: Some(AttachmentLineRange { start: 3, end: 9 }),
                    },
                    Attachment::Directory {
                        path: directory_path.clone(),
                        display_name: Some("message-directory".to_string()),
                    },
                    Attachment::Selection {
                        file_path: selection_path.clone(),
                        text: "SCENARIO_SELECTION".to_string(),
                        display_name: Some("Program.rs".to_string()),
                        selection: AttachmentSelectionRange {
                            start: AttachmentSelectionPosition {
                                line: 17,
                                character: 0,
                            },
                            end: AttachmentSelectionPosition {
                                line: 17,
                                character: 18,
                            },
                        },
                    },
                    Attachment::GitHubReference {
                        number: 610,
                        reference_type: GitHubReferenceType::Pr,
                        state: "open".to_string(),
                        title: "Scenario-shaped E2E coverage".to_string(),
                        url: "https://github.com/github/copilot-sdk/pull/610".to_string(),
                    },
                    Attachment::Blob {
                        data: "QVBQX0JMT0I=".to_string(),
                        mime_type: "text/plain".to_string(),
                        display_name: Some("scenario-wire-blob.txt".to_string()),
                    },
                    Attachment::ExtensionContext {
                        captured_at: "2026-09-17T20:00:00Z".to_string(),
                        extension_id: "scenario-client:code-review".to_string(),
                        canvas_id: Some("diff".to_string()),
                        instance_id: Some("diff-17".to_string()),
                        title: "Selected change".to_string(),
                        payload: Some(json!({ "selection": "SCENARIO_SELECTION", "line": 17 })),
                    },
                ]),
        )
        .await
        .expect("send complete message");
    assert_eq!(message_id, "scenario-client-message");

    session.disconnect().await.expect("disconnect session");
    client.stop().await.expect("stop client");

    let send = fake.captured_request("session.send");
    let params = send.params.as_object().expect("session.send params");
    assert_json_values(
        params,
        [
            ("prompt", json!("Use the hidden scenario context.")),
            ("displayPrompt", json!("Review selected scenario context")),
            ("mode", json!("enqueue")),
            ("agentMode", json!("interactive")),
            ("source", json!("agent-scenario-client")),
            (
                "traceparent",
                json!("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
            ),
            ("tracestate", json!("scenario-client=send")),
        ],
    );
    let attachments = params["attachments"].as_array().expect("attachments");
    assert_eq!(
        attachments
            .iter()
            .map(|attachment| attachment["type"].as_str().expect("attachment type"))
            .collect::<Vec<_>>(),
        [
            "file",
            "directory",
            "selection",
            "github_reference",
            "blob",
            "extension_context"
        ]
    );
    assert_eq!(attachments[0]["path"], json!(path_string(&file_path)));
    assert_eq!(attachments[0]["lineRange"], json!({ "start": 3, "end": 9 }));
    assert_eq!(attachments[1]["path"], json!(path_string(&directory_path)));
    assert_eq!(
        attachments[2]["filePath"],
        json!(path_string(&selection_path))
    );
    assert_eq!(attachments[2]["text"], json!("SCENARIO_SELECTION"));
    assert_eq!(attachments[3]["number"], json!(610));
    assert_eq!(attachments[3]["referenceType"], json!("pr"));
    assert_eq!(attachments[4]["data"], json!("QVBQX0JMT0I="));
    assert_eq!(attachments[4]["mimeType"], json!("text/plain"));
    assert_eq!(
        attachments[5]["extensionId"],
        json!("scenario-client:code-review")
    );
    assert_eq!(
        attachments[5]["payload"]["selection"],
        json!("SCENARIO_SELECTION")
    );
}

#[tokio::test]
async fn transport_loss_never_replays_send_for_any_delivery_mode() {
    for (mode, expected_mode) in [
        (None, None),
        (Some(DeliveryMode::Enqueue), Some("enqueue")),
        (Some(DeliveryMode::Immediate), Some("immediate")),
    ] {
        let fake = FakeCli::new();
        let client = Client::start(
            fake.client_options_with_behavior("ambiguous-send-client-token", "drop-after-send"),
        )
        .await
        .expect("start fake CLI client");
        let session = client
            .create_session(CreateSessionOptions::default())
            .await
            .expect("create session");
        let mut message = MessageOptions::new("AMBIGUOUS_SCENARIO_SEND")
            .with_display_prompt("Ambiguous scenario send")
            .with_source(MessageSource::Agent("scenario-client".to_string()));
        message.mode = mode;

        let error = session
            .send(message)
            .await
            .expect_err("transport loss should fail send");
        assert!(error.is_transport_failure());
        client.force_stop();

        let sends = fake
            .capture()
            .requests
            .into_iter()
            .filter(|request| request.method == "session.send")
            .collect::<Vec<_>>();
        assert_eq!(sends.len(), 1);
        assert_eq!(
            sends[0].params.get("mode").and_then(Value::as_str),
            expected_mode
        );
    }
}

#[tokio::test]
async fn cloud_create_routes_first_event_for_server_assigned_session_id() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options("cloud-create-client-token"))
        .await
        .expect("start fake CLI client");
    let prepared = client
        .prepare_session(CreateSessionOptions::default().with_cloud(
            CloudSessionOptions::with_repository(
                CloudSessionRepository::new("github", "copilot-sdk").with_branch("main"),
            ),
        ))
        .expect("prepare cloud session");
    let mut events = prepared.subscribe();
    let session = prepared.start().await.expect("start cloud session");

    assert_eq!(session.id().as_str(), "server-assigned-cloud-session");
    let event = tokio::time::timeout(Duration::from_secs(5), events.recv())
        .await
        .expect("first cloud event timed out")
        .expect("cloud event stream closed");
    assert_eq!(event.parsed_type(), SessionEventType::SessionStart);
    assert_eq!(
        event
            .typed_data::<SessionStartData>()
            .expect("session.start data")
            .session_id,
        session.id().clone()
    );

    session.disconnect().await.expect("disconnect session");
    client.stop().await.expect("stop client");
    let create = fake.captured_request("session.create");
    assert!(create.params.get("sessionId").is_none());
    assert_eq!(
        create.params["cloud"]["repository"]["owner"],
        json!("github")
    );
}

#[tokio::test]
async fn remote_connect_runtime_id_is_used_for_resume() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options("cloud-connect-client-token"))
        .await
        .expect("start fake CLI client");

    let connection = client
        .rpc()
        .sessions()
        .connect(ConnectRemoteSessionParams {
            session_id: SessionId::from("cloud-control-session"),
        })
        .await
        .expect("connect remote session");
    assert_eq!(connection.session_id.as_str(), "runtime-session-id");
    assert_eq!(connection.metadata.session_id, connection.session_id);
    assert_eq!(
        connection.metadata.resource_id.as_deref(),
        Some("github/copilot-sdk#123")
    );
    let resumed = client
        .resume_session(ResumeSessionOptions::new(connection.session_id.clone()))
        .await
        .expect("resume connected runtime session");
    assert_eq!(resumed.id(), &connection.session_id);

    resumed.disconnect().await.expect("disconnect session");
    client.stop().await.expect("stop client");
    assert_eq!(
        fake.captured_request("sessions.connect").params["sessionId"],
        json!("cloud-control-session")
    );
    assert_eq!(
        fake.captured_request("session.resume").params["sessionId"],
        json!("runtime-session-id")
    );
}

#[cfg(windows)]
#[test]
fn node_fixture_paths_preserve_drive_and_unc_roots() {
    for (input, expected) in [
        (
            r"\\?\D:\sdk fixtures\runtime.cjs",
            r"D:\sdk fixtures\runtime.cjs",
        ),
        (
            r"\\?\UNC\server\share\sdk fixtures\runtime.cjs",
            r"\\server\share\sdk fixtures\runtime.cjs",
        ),
        (
            r"D:\sdk fixtures\runtime.cjs",
            r"D:\sdk fixtures\runtime.cjs",
        ),
        (
            r"\\server\share\sdk fixtures\runtime.cjs",
            r"\\server\share\sdk fixtures\runtime.cjs",
        ),
    ] {
        assert_eq!(
            node_fixture::path_for_node(PathBuf::from(input)),
            PathBuf::from(expected)
        );
    }
}

#[tokio::test]
async fn canonical_node_fixture_launches_over_stdio_and_tcp() {
    for tcp in [false, true] {
        let fake = ShutdownCli::new("stop");
        let mut options = fake.options().with_mode(ClientMode::Empty);
        let Some(RuntimeConnection::SpawnedStdio(mut process)) = options.runtime.take() else {
            panic!("shutdown fixture must configure a spawned process");
        };
        assert!(PathBuf::from(&process.args[0]).is_absolute());
        process =
            process.with_owned(|owned| owned.with_base_directory(fake.dir.path().join("state")));
        if tcp {
            process.args.push("--fixture-tcp".into());
        }
        options.runtime = Some(if tcp {
            RuntimeConnection::SpawnedTcp(SpawnedTcpRuntimeOptions {
                process,
                port: 0,
                connection_token: None,
            })
        } else {
            RuntimeConnection::SpawnedStdio(process)
        });
        let client = Client::start(options).await.expect("fixture handshake");
        let pid = client.pid().expect("owned fixture child");
        assert_eq!(
            std::fs::read_to_string(&fake.pid_file).expect("fixture executed"),
            pid.to_string()
        );
        assert_eq!(
            client
                .ping(Some("fixture-path"))
                .await
                .expect("fixture ping")
                .message,
            "fixture-path"
        );
        if tcp {
            client
                .rpc()
                .runtime()
                .shutdown()
                .await
                .expect("fixture shutdown");
            client.force_stop();
        } else {
            client.stop().await.expect("fixture shutdown and EOF");
        }
        wait_for_process_exit(pid).await;
        assert_eq!(
            std::fs::read_to_string(&fake.marker).expect("fixture cleanup"),
            "{\"type\":\"span\"}\n"
        );
    }
}

#[tokio::test]
async fn stdio_exit_unblocks_a_backpressured_request() {
    let dir = tempfile::tempdir().expect("create exit fixture directory");
    let script = r#"
const fs = require("node:fs");
const { spawn } = require("node:child_process");
const path = require("node:path");
const marker = name => path.join(process.cwd(), name);
const holder = `
    const fs = require("node:fs");
    fs.writeFileSync("holder.pid.tmp", String(process.pid));
    fs.renameSync("holder.pid.tmp", "holder.pid");
    setInterval(() => {
        if (!fs.existsSync("keep")) process.exit(0);
    }, 10);
    setTimeout(() => process.exit(1), 60000);
`;
function reply(id, result) {
    const body = JSON.stringify({ jsonrpc: "2.0", id, result });
    process.stdout.write("Content-Length: " + Buffer.byteLength(body) + "\r\n\r\n" + body);
}
let input = Buffer.alloc(0);
let armed = false;
process.stdin.on("data", chunk => {
    if (armed) {
        process.stdin.pause();
        fs.writeFileSync(marker("blocked"), "ready");
        return;
    }
    input = Buffer.concat([input, chunk]);
    for (;;) {
        const end = input.indexOf("\r\n\r\n");
        if (end < 0) return;
        const length = Number(/Content-Length: *(\d+)/i.exec(input.subarray(0, end).toString())[1]);
        if (input.length < end + 4 + length) return;
        const request = JSON.parse(input.subarray(end + 4, end + 4 + length));
        input = input.subarray(end + 4 + length);
        if (request.method === "connect") {
            reply(request.id, { ok: true, protocolVersion: 4, version: "test" });
        } else if (request.params?.message === "arm") {
            spawn(process.execPath, ["-e", holder], {
                stdio: "inherit",
                detached: process.platform === "win32",
                windowsHide: true,
            });
            armed = true;
            reply(request.id, { message: "armed", timestamp: "0" });
        }
    }
});
setInterval(() => {
    if (fs.existsSync(marker("exit"))) process.exit(0);
}, 10);
"#;
    std::fs::write(dir.path().join("keep"), "").expect("keep descendant alive");
    let client = Client::start(
        ClientOptions::new(copilot_sdk::ClientMode::CodingAgent).with_runtime(
            RuntimeConnection::for_spawned_stdio(
                SpawnedProcessOptions::new()
                    .with_program(CliProgram::Path("node".into()))
                    .with_args(["-e", script, "--"])
                    .with_working_directory(dir.path()),
            ),
        ),
    )
    .await
    .expect("start exit fixture");
    let pid = client.pid().expect("owned runtime PID");
    assert_eq!(client.ping(Some("arm")).await.unwrap().message, "armed");
    let holder_pid = dir.path().join("holder.pid");
    wait_for_fixture_file(&holder_pid).await;
    let descendant = std::fs::read_to_string(&holder_pid)
        .unwrap()
        .parse()
        .unwrap();

    // The peer pauses after the first chunk, far short of this frame's length.
    let message = "x".repeat(8 * 1024 * 1024);
    let pending = client.ping(Some(&message));
    tokio::pin!(pending);
    let blocked = dir.path().join("blocked");
    tokio::select! {
        result = &mut pending => panic!("large request completed before runtime exit: {result:?}"),
        _ = wait_for_fixture_file(&blocked) => {}
    }
    let started = std::time::Instant::now();
    std::fs::write(dir.path().join("exit"), "").expect("release runtime exit");
    let result = tokio::time::timeout(Duration::from_millis(2500), &mut pending).await;
    let elapsed = started.elapsed();
    let later = tokio::time::timeout(Duration::from_millis(250), client.ping(Some("later"))).await;
    let holder_alive = process_is_alive(descendant).await;
    let stopped = tokio::time::timeout(Duration::from_millis(500), client.stop()).await;
    // Stop contains the runtime's whole process tree on every platform.
    wait_for_process_exit(descendant).await;
    let holder_alive_after_stop = process_is_alive(descendant).await;
    let runtime_alive = process_is_alive(pid).await;
    client.force_stop();
    std::fs::remove_file(dir.path().join("keep")).expect("release descendant");
    wait_for_process_exit(descendant).await;

    assert!(
        matches!(&result, Ok(Err(error))
            if *error.kind() == copilot_sdk::ErrorKind::Protocol(
                copilot_sdk::ProtocolErrorKind::RequestCancelled)
            && error.to_string().contains("the CLI process exited")),
        "backpressured request: {result:?} after {elapsed:?}"
    );
    assert!(elapsed >= Duration::from_millis(900), "{elapsed:?}");
    assert!(
        matches!(&later, Ok(Err(error)) if error.is_transport_failure()),
        "subsequent request: {later:?}"
    );
    assert!(stopped.is_ok(), "shutdown: {stopped:?}");
    assert!(
        holder_alive,
        "descendant must not release inherited handles"
    );
    assert!(
        !holder_alive_after_stop,
        "stop must terminate the descendant with the runtime's process tree"
    );
    assert!(!runtime_alive, "runtime must be reaped");
}

#[tokio::test]
async fn stdio_exit_watcher_and_graceful_stop_handoffs() {
    for stop_before_exit in [true, false] {
        let fake = ExitHandoffCli::new();
        let client = Client::start(fake.options())
            .await
            .expect("start exit fixture");
        let pid = client.pid().expect("owned runtime PID");
        let descendant = fake.descendant().await;
        let pending = client
            .begin_call("ping", Some(json!({ "message": "pending" })))
            .expect("submit pending request");
        wait_for_fixture_file(&fake.path("pending")).await;
        let stopping = client.stop();
        tokio::pin!(stopping);

        if stop_before_exit {
            let eof = fake.path("eof");
            tokio::select! {
                result = &mut stopping => panic!("stop completed before gated exit: {result:?}"),
                _ = wait_for_fixture_file(&eof) => {}
            }
            // EOF proves shutdown was acknowledged and stdin was closed.
            // Poll once more while exit is gated to register the child wait.
            assert!(futures_util::poll!(&mut stopping).is_pending());
        }
        std::fs::write(fake.path("exit"), "exit").expect("release runtime exit");
        if !stop_before_exit {
            wait_for_runtime_exit(&client).await;
        }

        let stopped = tokio::time::timeout(Duration::from_millis(2500), &mut stopping).await;
        let result = tokio::time::timeout(Duration::from_millis(250), pending).await;
        let later = tokio::time::timeout(Duration::from_millis(250), client.ping(None)).await;
        let runtime_alive = process_is_alive(pid).await;
        client.force_stop();
        fake.release_descendant(descendant).await;

        assert!(
            stopped.is_ok(),
            "stop_before_exit={stop_before_exit}: {stopped:?}"
        );
        if stop_before_exit {
            assert!(matches!(stopped, Ok(Ok(()))), "{stopped:?}");
        }
        assert!(
            matches!(&result, Ok(Err(error)) if error.is_transport_failure()),
            "stop_before_exit={stop_before_exit}, pending request: {result:?}"
        );
        assert!(
            matches!(&later, Ok(Err(error)) if error.is_transport_failure()),
            "stop_before_exit={stop_before_exit}, later request: {later:?}"
        );
        assert!(!runtime_alive, "runtime must be reaped");
        assert!(client.pid().is_none());
    }
}

#[tokio::test]
async fn stdio_force_stop_and_last_client_drop_interrupt_exit_drain() {
    for force in [true, false] {
        let fake = ExitHandoffCli::new();
        let client = Client::start(fake.options())
            .await
            .expect("start exit fixture");
        let pid = client.pid().expect("owned runtime PID");
        let descendant = fake.descendant().await;
        let pending = client
            .begin_call("ping", Some(json!({ "message": "pending" })))
            .expect("submit pending request");
        tokio::pin!(pending);
        wait_for_fixture_file(&fake.path("pending")).await;
        // The probe starts a process, so it cannot sit in the drain window.
        let holder_alive = process_is_alive(descendant).await;
        // Anchor before exit: observing it later can miss most of the drain.
        let started = tokio::time::Instant::now();
        std::fs::write(fake.path("exit"), "exit").expect("release runtime exit");
        wait_for_runtime_exit(&client).await;
        let still_pending = futures_util::poll!(&mut pending).is_pending();
        let in_drain_window = started.elapsed() < Duration::from_millis(750);

        // Keep the client through a forced stop: dropping it would close the
        // connection too and hide a force_stop that does nothing.
        let result = if force {
            client.force_stop();
            let result = tokio::time::timeout(Duration::from_millis(250), &mut pending).await;
            drop(client);
            result
        } else {
            drop(client);
            tokio::time::timeout(Duration::from_millis(250), &mut pending).await
        };
        let runtime_alive = process_is_alive(pid).await;
        fake.release_descendant(descendant).await;

        assert!(
            in_drain_window,
            "must shut down before the one-second drain expires"
        );
        assert!(
            still_pending,
            "request must be pending when shutdown begins"
        );
        assert!(
            holder_alive,
            "descendant must retain stdout during shutdown"
        );
        // Natural drain expiry cancels with the process-exit message instead.
        assert!(
            matches!(&result, Ok(Err(error))
                if *error.kind() == copilot_sdk::ErrorKind::Protocol(
                    copilot_sdk::ProtocolErrorKind::RequestCancelled)
                && error.message().is_none()),
            "force={force}, pending request: {result:?}"
        );
        assert!(!runtime_alive, "runtime must be reaped");
    }
}

#[tokio::test]
async fn stdio_exit_keeps_final_replies_and_bounds_unanswered_requests() {
    let fake = ExitHandoffCli::new();
    let client = Client::start(fake.options())
        .await
        .expect("start exit fixture");
    let pid = client.pid().expect("owned runtime PID");
    let descendant = fake.descendant().await;
    let unanswered = client
        .begin_call("ping", Some(json!({ "message": "pending" })))
        .expect("submit unanswered request");
    tokio::pin!(unanswered);
    wait_for_fixture_file(&fake.path("pending")).await;

    // The runtime writes this reply, which is larger than a pipe buffer, and
    // exits while the descendant keeps its stdout open.
    // The drain starts when the runtime exits, which is after this request
    // is sent, so the clock starts first and cannot miss part of the drain.
    let sent = std::time::Instant::now();
    let reply = client.ping(Some("final-and-exit")).await;
    let result = tokio::time::timeout(Duration::from_millis(2500), &mut unanswered).await;
    let elapsed = sent.elapsed();
    let later = tokio::time::timeout(Duration::from_millis(250), client.ping(None)).await;
    let holder_alive = process_is_alive(descendant).await;
    // The watcher observes the exit without reaping, so the process keeps its
    // identity until stop or force stop contains it.
    let pid_retained = client.pid() == Some(pid);
    client.force_stop();
    fake.release_descendant(descendant).await;

    assert_eq!(
        reply
            .expect("a reply written before the exit is kept")
            .message
            .len(),
        512 * 1024
    );
    assert!(
        matches!(&result, Ok(Err(error))
            if *error.kind() == copilot_sdk::ErrorKind::Protocol(
                copilot_sdk::ProtocolErrorKind::RequestCancelled)
            && error.to_string().contains("the CLI process exited")),
        "unanswered request: {result:?} after {elapsed:?}"
    );
    // The bound is one second after the exit, with room for scheduling.
    assert!(
        (Duration::from_millis(800)..Duration::from_millis(2000)).contains(&elapsed),
        "{elapsed:?}"
    );
    assert!(
        matches!(&later, Ok(Err(error)) if error.is_transport_failure()),
        "later request: {later:?}"
    );
    assert!(holder_alive, "descendant must retain stdout");
    // Windows has no zombie to pin, so only Unix keeps the exited ID.
    assert!(
        pid_retained || cfg!(windows),
        "pid() must keep the exited runtime until stop"
    );
}

#[cfg(target_os = "macos")]
#[tokio::test]
async fn stdio_requests_survive_a_stopped_and_resumed_child() {
    let fake = ExitHandoffCli::new();
    let client = Client::start(fake.options())
        .await
        .expect("start exit fixture");
    let pid = client.pid().expect("owned runtime PID");
    let signal_pid = i32::try_from(pid).unwrap();
    let pending = client
        .begin_call("ping", Some(json!({ "message": "pending-until-resume" })))
        .expect("submit pending request");
    wait_for_fixture_file(&fake.path("pending")).await;

    assert_eq!(unsafe { libc::kill(signal_pid, libc::SIGSTOP) }, 0);
    wait_for_child_status(pid, libc::WSTOPPED, |code| code == libc::CLD_STOPPED).await;
    // The child cannot do I/O while stopped; let the watcher poll and let its
    // one-second drain timer expire if it mistook the stop for an exit.
    tokio::time::pause();
    tokio::time::sleep(Duration::from_secs(2)).await;
    tokio::time::resume();
    std::fs::write(fake.path("release-pending"), "").expect("release pending reply");
    assert_eq!(unsafe { libc::kill(signal_pid, libc::SIGCONT) }, 0);

    let pending = tokio::time::timeout(Duration::from_secs(5), pending).await;
    let next = tokio::time::timeout(Duration::from_secs(5), client.ping(Some("next"))).await;
    client.force_stop();

    assert_eq!(
        pending
            .expect("pending reply timed out")
            .expect("pending request disconnected")["message"],
        "pending-until-resume"
    );
    assert_eq!(
        next.expect("next ping timed out")
            .expect("next ping disconnected")
            .message,
        "next"
    );
}

#[cfg(unix)]
async fn wait_for_child_status(pid: u32, event: i32, matches_status: impl Fn(i32) -> bool) {
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            // Observe without consuming the event or releasing group identity.
            let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
            let result = unsafe {
                libc::waitid(
                    libc::P_PID,
                    pid,
                    &mut info,
                    event | libc::WNOHANG | libc::WNOWAIT,
                )
            };
            if result == 0 {
                if info.si_signo == libc::SIGCHLD && matches_status(info.si_code) {
                    return;
                }
            } else {
                let error = std::io::Error::last_os_error();
                assert_eq!(
                    error.kind(),
                    std::io::ErrorKind::Interrupted,
                    "observe runtime {pid} status: {error}"
                );
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .unwrap_or_else(|_| panic!("runtime {pid} did not report waitid event {event:#x}"));
}

async fn wait_for_runtime_exit(client: &Client) {
    #[cfg(unix)]
    wait_for_child_status(
        client.pid().expect("owned runtime PID"),
        libc::WEXITED,
        |code| matches!(code, libc::CLD_EXITED | libc::CLD_KILLED | libc::CLD_DUMPED),
    )
    .await;

    // On Windows the watcher uses try_wait, which clears the child ID.
    #[cfg(not(unix))]
    tokio::time::timeout(Duration::from_secs(5), async {
        while client.pid().is_some() {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .expect("runtime must exit before testing drain cancellation");

    // On Unix, exit observation does not prove the watcher has seen it yet.
    // Windows already observed the watcher clearing the PID; its blocked stdout
    // reader also prevents paused-clock auto-advance.
    #[cfg(unix)]
    {
        tokio::time::pause();
        tokio::time::sleep(Duration::from_millis(40)).await;
        tokio::time::resume();
    }
}

struct ExitHandoffCli {
    dir: TempDir,
}

impl ExitHandoffCli {
    fn new() -> Self {
        let dir = tempfile::tempdir().expect("create exit handoff fixture directory");
        std::fs::write(dir.path().join("keep"), "keep").expect("keep descendant alive");
        Self { dir }
    }

    fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    fn options(&self) -> ClientOptions {
        let script = r#"
const fs = require("node:fs");
const { spawn } = require("node:child_process");
const holder = spawn(process.execPath, ["-e", `
    const fs = require("node:fs");
    fs.writeFileSync("holder.pid.tmp", String(process.pid));
    fs.renameSync("holder.pid.tmp", "holder.pid");
    setInterval(() => {
        if (!fs.existsSync("keep")) process.exit(0);
    }, 10);
    setTimeout(() => process.exit(1), 60000);
`], {
    stdio: ["ignore", "inherit", "ignore"],
    detached: process.platform === "win32",
    windowsHide: true,
});
holder.on("error", error => { throw error; });
function reply(id, result) {
    const body = JSON.stringify({ jsonrpc: "2.0", id, result });
    process.stdout.write("Content-Length: " + Buffer.byteLength(body) + "\r\n\r\n" + body);
}
let pendingPing;
let input = Buffer.alloc(0);
process.stdin.on("data", chunk => {
    input = Buffer.concat([input, chunk]);
    for (;;) {
        const end = input.indexOf("\r\n\r\n");
        if (end < 0) return;
        const length = Number(/Content-Length: *(\d+)/i.exec(input.subarray(0, end).toString())[1]);
        if (input.length < end + 4 + length) return;
        const request = JSON.parse(input.subarray(end + 4, end + 4 + length));
        input = input.subarray(end + 4 + length);
        if (request.method === "connect") {
            reply(request.id, { ok: true, protocolVersion: 4, version: "test" });
        } else if (request.method === "runtime.shutdown") {
            reply(request.id, {});
        } else if (request.params?.message === "pending") {
            fs.writeFileSync("pending", "accepted");
        } else if (request.params?.message === "pending-until-resume") {
            pendingPing = request;
            fs.writeFileSync("pending", "accepted");
        } else if (request.params?.message === "final-and-exit") {
            // A reply larger than a pipe buffer, written just before the exit.
            const body = JSON.stringify({
                jsonrpc: "2.0",
                id: request.id,
                result: { message: "y".repeat(512 * 1024), timestamp: "0" },
            });
            process.stdout.write(
                "Content-Length: " + Buffer.byteLength(body) + "\r\n\r\n" + body,
                () => process.exit(0),
            );
        } else if (request.method === "ping" && request.params?.message === "next") {
            reply(request.id, { message: request.params.message, timestamp: "0" });
        }
    }
});
process.stdin.on("end", () => fs.writeFileSync("eof", "closed"));
setInterval(() => {
    if (fs.existsSync("exit")) process.exit(0);
    if (pendingPing && fs.existsSync("release-pending")) {
        reply(pendingPing.id, { message: pendingPing.params.message, timestamp: "0" });
        pendingPing = undefined;
    }
}, 10);
setTimeout(() => process.exit(1), 60000);
"#;
        ClientOptions::new(copilot_sdk::ClientMode::CodingAgent).with_runtime(
            RuntimeConnection::for_spawned_stdio(
                SpawnedProcessOptions::new()
                    .with_program(CliProgram::Path("node".into()))
                    .with_args(["-e", script, "--"])
                    .with_working_directory(self.dir.path()),
            ),
        )
    }

    async fn descendant(&self) -> u32 {
        let path = self.path("holder.pid");
        wait_for_fixture_file(&path).await;
        std::fs::read_to_string(path).unwrap().parse().unwrap()
    }

    async fn release_descendant(&self, pid: u32) {
        std::fs::remove_file(self.path("keep")).expect("release descendant");
        wait_for_process_exit(pid).await;
    }
}

async fn wait_for_fixture_file(path: &std::path::Path) {
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            match std::fs::read_to_string(path) {
                Ok(content) if !content.is_empty() => return,
                Ok(_) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => panic!("read fixture marker {path:?}: {error}"),
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .expect("subprocess must publish its checkpoint");
}

#[tokio::test]
async fn stdio_stop_waits_for_eof_cleanup() {
    let fake = ShutdownCli::new("stop");
    let client = Client::start(fake.options()).await.expect("start fake CLI");
    let pid = client.pid().expect("owned child");
    tokio::time::timeout(Duration::from_secs(40), client.stop())
        .await
        .expect("shutdown RPC, graceful exit, and reap must be bounded")
        .expect("stop client");
    assert_eq!(
        std::fs::read_to_string(&fake.marker).expect("EOF cleanup"),
        "{\"type\":\"span\"}\n"
    );
    assert!(!process_is_alive(pid).await);
    client.stop().await.expect("repeated stop");
}

#[tokio::test]
async fn stdio_stop_drains_stdout_during_eof_cleanup() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options_with_behavior("token", "shutdown-output"))
        .await
        .expect("start fake CLI");
    tokio::time::timeout(Duration::from_secs(40), client.stop())
        .await
        .expect("shutdown RPC, graceful exit, and reap must be bounded")
        .expect("stop client");
    assert_eq!(
        std::fs::read_to_string(fake.capture_path.with_extension("cleanup"))
            .expect("cleanup after draining final stdout"),
        "flushed"
    );
}

#[tokio::test]
async fn stdio_stop_preserves_shutdown_error_after_eof_cleanup() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options_with_behavior("token", "shutdown-error"))
        .await
        .expect("start fake CLI");
    let errors = tokio::time::timeout(Duration::from_secs(40), client.stop())
        .await
        .expect("shutdown RPC, graceful exit, and reap must be bounded")
        .expect_err("shutdown error must be returned");
    assert!(errors.to_string().contains("shutdown rejected"), "{errors}");
    assert_eq!(
        std::fs::read_to_string(fake.capture_path.with_extension("cleanup"))
            .expect("EOF cleanup despite RPC failure"),
        "flushed"
    );
}

#[tokio::test]
async fn stdio_stop_bounds_unresponsive_shutdown_and_eof() {
    let fake = FakeCli::new();
    let client = Client::start(fake.client_options_with_behavior("token", "ignore-shutdown"))
        .await
        .expect("start fake CLI");
    let pid = client.pid().expect("owned child");
    let start = std::time::Instant::now();
    let errors = tokio::time::timeout(Duration::from_secs(40), client.stop())
        .await
        .expect("10s RPC + 10s graceful exit + 10s reap must be bounded")
        .expect_err("unanswered shutdown must report its timeout");
    assert!(errors.to_string().contains("timed out"), "{errors}");
    assert!(start.elapsed() >= Duration::from_secs(20));
    assert!(!process_is_alive(pid).await);
    assert!(!fake.capture_path.with_extension("cleanup").exists());
}

#[tokio::test]
async fn stdio_stop_terminates_child_that_does_not_exit_after_eof() {
    let fake = ShutdownCli::new("fallback");
    let client = Client::start(fake.options()).await.expect("start fake CLI");
    let pid = client.pid().expect("owned child");
    let start = std::time::Instant::now();
    tokio::time::timeout(Duration::from_secs(40), client.stop())
        .await
        .expect("shutdown RPC, graceful exit, and reap must be bounded")
        .expect("fallback termination succeeds");
    assert!(start.elapsed() >= Duration::from_secs(10));
    assert!(!process_is_alive(pid).await);
    assert!(
        fake.marker.exists(),
        "fixture must observe shutdown and EOF"
    );
    assert_eq!(
        std::fs::read_to_string(&fake.marker).expect("EOF cleanup"),
        "{\"type\":\"span\"}\n"
    );
}

#[tokio::test]
async fn stdio_force_stop_interrupts_graceful_exit_wait() {
    let fake = ShutdownCli::new("fallback");
    let client = Client::start(fake.options()).await.expect("start fake CLI");
    let pid = client.pid().expect("owned child");
    let stopping = tokio::spawn({
        let client = client.clone();
        async move { client.stop().await }
    });
    wait_for_cleanup_marker(&fake.marker).await;

    client.force_stop();
    tokio::time::timeout(Duration::from_secs(5), async {
        stopping.await.expect("stop task").expect("stop client");
        wait_for_process_exit(pid).await;
    })
    .await
    .expect("force stop must interrupt grace before its 10-second deadline");
}

#[tokio::test]
async fn stdio_cancelled_graceful_exit_wait_terminates_child() {
    let fake = ShutdownCli::new("fallback");
    let client = Client::start(fake.options()).await.expect("start fake CLI");
    let pid = client.pid().expect("owned child");
    let stopping = tokio::spawn({
        let client = client.clone();
        async move { client.stop().await }
    });
    wait_for_cleanup_marker(&fake.marker).await;

    stopping.abort();
    assert!(
        stopping
            .await
            .expect_err("stop task cancelled")
            .is_cancelled()
    );
    wait_for_process_exit(pid).await;
    assert!(client.pid().is_none());
}

async fn wait_for_cleanup_marker(path: &std::path::Path) {
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            match std::fs::read_to_string(path) {
                Ok(content) if content == "{\"type\":\"span\"}\n" => return,
                Ok(_) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => panic!("read cleanup marker: {error}"),
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .expect("fixture must reach EOF cleanup during graceful shutdown");
}

#[tokio::test]
async fn stdio_force_stop_and_drop_remain_immediate() {
    for force in [true, false] {
        let fake = ShutdownCli::new("force");
        let client = Client::start(fake.options()).await.expect("start fake CLI");
        let pid = client.pid().expect("owned child");
        if force {
            client.force_stop();
            assert!(client.pid().is_none());
        }
        drop(client);
        wait_for_process_exit(pid).await;
        assert!(!fake.marker.exists());
    }
}

#[tokio::test]
async fn stdio_startup_failure_terminates_owned_child() {
    let fake = ShutdownCli::new("start-failure");
    let result = tokio::time::timeout(Duration::from_secs(5), Client::start(fake.options()))
        .await
        .expect("startup failure must not await graceful shutdown");
    assert!(result.is_err());
    let pid = std::fs::read_to_string(&fake.pid_file)
        .expect("read child PID")
        .parse()
        .expect("parse child PID");
    wait_for_process_exit(pid).await;
    assert!(!fake.marker.exists());
}

struct ShutdownCli {
    dir: TempDir,
    marker: PathBuf,
    pid_file: PathBuf,
    mode: &'static str,
}

impl ShutdownCli {
    fn new(mode: &'static str) -> Self {
        let dir = tempfile::tempdir().expect("create shutdown fixture directory");
        Self {
            marker: dir.path().join("cleanup.jsonl"),
            pid_file: dir.path().join("child.pid"),
            dir,
            mode,
        }
    }

    fn options(&self) -> ClientOptions {
        let script = node_fixture::shutdown_script();
        ClientOptions::new(copilot_sdk::ClientMode::CodingAgent)
            .with_runtime(RuntimeConnection::for_spawned_stdio(
                SpawnedProcessOptions::new()
                    .with_program(CliProgram::Path("node".into()))
                    .with_args([
                        script.into_os_string(),
                        self.marker.clone().into_os_string(),
                        self.mode.into(),
                        self.pid_file.clone().into_os_string(),
                    ])
                    .with_working_directory(self.dir.path()),
            ))
            .with_github(|group| group.with_use_logged_in_user(false))
    }
}

async fn process_is_alive(pid: u32) -> bool {
    let output = tokio::process::Command::new("node")
        .args([
            "-e",
            "try { process.kill(Number(process.argv[1]), 0); } catch (e) { if (e.code === 'ESRCH') process.exit(3); throw e; }",
            &pid.to_string(),
        ])
        .output()
        .await
        .expect("query child process");
    match output.status.code() {
        Some(0) => true,
        Some(3) => false,
        _ => panic!(
            "query child process: {}",
            String::from_utf8_lossy(&output.stderr)
        ),
    }
}

async fn wait_for_process_exit(pid: u32) {
    tokio::time::timeout(Duration::from_secs(5), async {
        while process_is_alive(pid).await {
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    })
    .await
    .expect("owned child must exit promptly");
}

struct FakeCli {
    _dir: TempDir,
    script_path: PathBuf,
    capture_path: PathBuf,
    work_dir: PathBuf,
}

impl FakeCli {
    fn new() -> Self {
        let dir = tempfile::tempdir().expect("create fake CLI temp dir");
        let script_path = dir.path().join("fake-cli.js");
        let capture_path = dir.path().join("fake-cli-capture.json");
        let work_dir = dir.path().join("cwd");
        std::fs::create_dir(&work_dir).expect("create fake CLI cwd");
        std::fs::write(&script_path, FAKE_STDIO_CLI_SCRIPT).expect("write fake CLI script");
        Self {
            _dir: dir,
            script_path,
            capture_path,
            work_dir,
        }
    }

    fn client_options(&self, token: &str) -> ClientOptions {
        self.client_options_with_behavior(token, "normal")
    }

    fn client_options_with_behavior(&self, token: &str, behavior: &str) -> ClientOptions {
        let process = SpawnedProcessOptions::new()
            .with_program(CliProgram::Path(PathBuf::from("node")))
            .with_args([
                self.script_path.as_os_str().to_owned(),
                "--capture-file".into(),
                self.capture_path.as_os_str().to_owned(),
                "--behavior".into(),
                behavior.into(),
            ])
            .with_working_directory(&self.work_dir);
        ClientOptions::new(copilot_sdk::ClientMode::CodingAgent)
            .with_github(|group| group.with_token(token).with_use_logged_in_user(false))
            .with_runtime(RuntimeConnection::SpawnedStdio(process))
    }

    fn path(&self, name: &str) -> PathBuf {
        let path = self.work_dir.join(name);
        std::fs::create_dir_all(&path).expect("create fake CLI test path");
        path
    }

    fn captured_request(&self, method: &str) -> CapturedRequest {
        let capture = self.capture();
        capture
            .requests
            .iter()
            .find(|request| request.method == method)
            .cloned()
            .unwrap_or_else(|| panic!("expected {method} request in {capture:?}"))
    }

    fn capture(&self) -> CapturedCli {
        let text = std::fs::read_to_string(&self.capture_path).expect("read fake CLI capture file");
        serde_json::from_str(&text).expect("parse fake CLI capture file")
    }
}

#[derive(Debug, Deserialize)]
struct CapturedCli {
    requests: Vec<CapturedRequest>,
    args: Vec<String>,
    cwd: PathBuf,
    env: HashMap<String, String>,
}

#[derive(Debug, Clone, Deserialize)]
struct CapturedRequest {
    method: String,
    #[serde(default)]
    params: Value,
}

#[track_caller]
fn assert_json_values<'a>(
    object: &serde_json::Map<String, Value>,
    expected: impl IntoIterator<Item = (&'a str, Value)>,
) {
    for (key, expected_value) in expected {
        let mut path = key.split('.');
        let mut actual = object.get(path.next().expect("field name"));
        for field in path {
            actual = actual.and_then(|value| value.get(field));
        }
        assert_eq!(
            actual,
            Some(&expected_value),
            "unexpected value for key {key} in {object:?}"
        );
    }
}

fn path_string(path: &std::path::Path) -> String {
    path.to_string_lossy().into_owned()
}

const FAKE_STDIO_CLI_SCRIPT: &str = r#"
const fs = require("fs");

const captureIndex = process.argv.indexOf("--capture-file");
const captureFile = captureIndex >= 0 ? process.argv[captureIndex + 1] : undefined;
const behaviorIndex = process.argv.indexOf("--behavior");
const behavior = behaviorIndex >= 0 ? process.argv[behaviorIndex + 1] : "normal";
const requests = [];

function saveCapture() {
  if (!captureFile) {
    return;
  }
  fs.writeFileSync(captureFile, JSON.stringify({
    requests,
    args: process.argv.slice(2),
    cwd: process.cwd(),
    env: {
      COPILOT_SDK_AUTH_TOKEN: process.env.COPILOT_SDK_AUTH_TOKEN,
      COPILOT_SDK_PROCESS_TEST: process.env.COPILOT_SDK_PROCESS_TEST,
      PATH: process.env.PATH,
    },
  }));
}

saveCapture();

let buffer = Buffer.alloc(0);
let output = process.stdout;
function readChunk(chunk) {
  buffer = Buffer.concat([buffer, chunk]);
  processBuffer();
}
if (process.argv.includes("--stdio")) {
  process.stdin.on("data", readChunk);
  process.stdin.resume();
} else {
  const server = require("net").createServer(socket => {
    output = socket;
    socket.on("data", readChunk);
    socket.on("end", () => { socket.end(); server.close(); });
  });
  server.listen(0, "127.0.0.1", () => {
    console.log(`listening on port ${server.address().port}`);
  });
}
if (["ignore-shutdown", "shutdown-error", "shutdown-output"].includes(behavior)) {
  const keepAlive = setInterval(() => {}, 1000);
  process.stdin.on("end", () => {
    if (behavior === "ignore-shutdown") return;
    if (behavior === "shutdown-output") {
      const body = JSON.stringify({
        jsonrpc: "2.0",
        method: "shutdown.output",
        params: { output: "x".repeat(1024 * 1024) },
      });
      process.stdout.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`, error => {
        if (error) throw error;
        fs.writeFileSync(captureFile.replace(/\.json$/, ".cleanup"), "flushed");
        clearInterval(keepAlive);
      });
      return;
    }
    setTimeout(() => {
      fs.writeFileSync(captureFile.replace(/\.json$/, ".cleanup"), "flushed");
      clearInterval(keepAlive);
    }, 100);
  });
}

function processBuffer() {
  while (true) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd < 0) return;
    const header = buffer.subarray(0, headerEnd).toString("utf8");
    const match = /Content-Length:\s*(\d+)/i.exec(header);
    if (!match) throw new Error("Missing Content-Length header");
    const length = Number(match[1]);
    const bodyStart = headerEnd + 4;
    const bodyEnd = bodyStart + length;
    if (buffer.length < bodyEnd) return;
    const body = buffer.subarray(bodyStart, bodyEnd).toString("utf8");
    buffer = buffer.subarray(bodyEnd);
    handleMessage(JSON.parse(body));
  }
}

function handleMessage(message) {
  if (!Object.prototype.hasOwnProperty.call(message, "id")) {
    return;
  }
  requests.push({ method: message.method, params: message.params });
  saveCapture();
  if (message.method === "connect") {
    writeResponse(message.id, { ok: true, protocolVersion: 4, version: "fake" });
    return;
  }
  if (message.method === "runtime.shutdown" && behavior === "ignore-shutdown") return;
  if (message.method === "runtime.shutdown" && behavior === "shutdown-error") {
    writeMessage({ jsonrpc: "2.0", id: message.id, error: { code: -32000, message: "shutdown rejected" } });
    return;
  }
  if (message.method === "ping") {
    writeResponse(message.id, { message: "pong", protocolVersion: 4, timestamp: Date.now() });
    return;
  }
  if (message.method === "session.create") {
    const isCloud = Boolean(message.params && message.params.cloud);
    const sessionId = (message.params && message.params.sessionId)
      || (isCloud ? "server-assigned-cloud-session" : "fake-session");
    writeResponse(message.id, { sessionId, workspacePath: null, capabilities: null });
    if (isCloud) {
      writeMessage({
        jsonrpc: "2.0",
        method: "session.event",
        params: {
          sessionId,
          event: {
            id: "cloud-start-event",
            timestamp: "2026-09-18T00:00:00Z",
            parentId: null,
            type: "session.start",
            data: {
              sessionId,
              version: 1,
              producer: "fake-cli",
              copilotVersion: "fake",
              startTime: "2026-09-18T00:00:00Z",
            }
          }
        }
      });
    }
    return;
  }
  if (message.method === "sessions.connect") {
    const resourceId = behavior === "resource-mismatch"
      ? "github/docs#456"
      : "github/copilot-sdk#123";
    writeResponse(message.id, {
      sessionId: "runtime-session-id",
      metadata: {
        kind: "coding_agent",
        modifiedTime: "2026-09-18T00:00:00Z",
        repository: { owner: "github", name: "copilot-sdk", branch: "main" },
        resourceId,
        sessionId: "runtime-session-id",
        startTime: "2026-09-18T00:00:00Z"
      }
    });
    return;
  }
  if (message.method === "session.send") {
    if (behavior === "drop-after-send") {
      process.exit(0);
      return;
    }
    writeResponse(message.id, { messageId: "scenario-client-message" });
    return;
  }
  if (message.method === "session.resume") {
    const sessionId = (message.params && message.params.sessionId) || "fake-session";
    const result = { sessionId, workspacePath: null, capabilities: null, openCanvases: [] };
    if (behavior === "transcript-recovery") {
      result.transcriptRecovery = {
        plannedBackupPath: "recovery-backup.jsonl",
        invalidLineNumbers: [3, 5],
        sessionStartMoved: true,
      };
    }
    writeResponse(message.id, result);
    return;
  }
  if (message.method === "session.options.update") {
    writeResponse(message.id, { success: true });
    return;
  }
  if (message.method === "session.detach") {
    writeResponse(message.id, { success: true });
    return;
  }
  writeResponse(message.id, {});
}

function writeResponse(id, result) {
  writeMessage({ jsonrpc: "2.0", id, result });
}

function writeMessage(message) {
  const body = JSON.stringify(message);
  output.write("Content-Length: " + Buffer.byteLength(body, "utf8") + "\r\n\r\n" + body);
}
"#;
