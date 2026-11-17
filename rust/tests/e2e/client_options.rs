use copilot_sdk::{RuntimeConnection, SpawnedProcessOptions, SpawnedTcpRuntimeOptions};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use copilot_sdk::canvas::CanvasDeclaration;
use copilot_sdk::rpc::{ConnectRemoteSessionParams, OpenCanvasInstance};
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
    assert!(!params.contains_key("remoteSession"));
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
        let script = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../test/harness/stdio-shutdown-runtime.cjs")
            .canonicalize()
            .unwrap();
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
