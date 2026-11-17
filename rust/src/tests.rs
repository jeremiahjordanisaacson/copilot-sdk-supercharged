/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use super::*;
use crate::entra::{EntraTokenProviderArgs, EntraTokenProviderContext, EntraTokenProviderResult};

#[cfg(feature = "runtime")]
#[tokio::test]
async fn stderr_stream_logs_unterminated_output_before_eof_in_bounded_chunks() {
    use tokio::io::AsyncWriteExt;

    let (mut writer, reader) = tokio::io::duplex(16 * 1024);
    let (sent, mut received) = mpsc::unbounded_channel();
    let drain = tokio::spawn(async move {
        Client::drain_stderr_stream(reader, |line| sent.send(line.to_owned()).unwrap()).await
    });
    let output = "x".repeat(12 * 1024);
    writer.write_all(output.as_bytes()).await.unwrap();
    let observed = tokio::time::timeout(Duration::from_secs(5), async {
        let mut logged = String::new();
        while logged.len() < output.len() {
            let chunk = received.recv().await.expect("stderr reader stopped");
            assert!(
                chunk.len() <= 4096,
                "oversized stderr chunk: {}",
                chunk.len()
            );
            logged.push_str(&chunk);
        }
        logged
    })
    .await;
    drop(writer);
    drain.await.unwrap().unwrap();
    assert_eq!(
        observed.expect("stderr must be logged before EOF without a newline"),
        output
    );
}

#[cfg(feature = "runtime")]
#[tokio::test]
async fn stderr_stream_preserves_lines_and_split_utf8() {
    let output = format!("first\r\n\n{}\u{1f600}tail", "x".repeat(4095));
    let mut logged = Vec::new();
    Client::drain_stderr_stream(output.as_bytes(), |line| logged.push(line.to_owned()))
        .await
        .unwrap();
    assert_eq!(&logged[..2], &["first", ""]);
    assert_eq!(
        logged[2..].concat(),
        format!("{}\u{1f600}tail", "x".repeat(4095))
    );
}

#[cfg(all(unix, feature = "runtime"))]
#[tokio::test]
async fn tcp_startup_drains_stderr_before_port_announcement() {
    let temp = tempfile::tempdir().unwrap();
    let options = StartupOptions::default();
    let process = SpawnedProcessOptions::new().with_args([
        "-c",
        r#"i=0
while [ "$i" -lt 1024 ]; do
    printf '%01024d\n' 0 >&2
    i=$((i + 1))
done
printf 'listening on port 12345\n'
exec sleep 60"#,
        "--",
    ]);
    let (mut child, _tree, port, _, _) =
        Client::spawn_tcp(Path::new("/bin/sh"), &options, &process, temp.path(), 0)
            .await
            .expect("startup must drain stderr before waiting for the port");
    child.kill().await.unwrap();
    assert_eq!(port, 12345);
}

fn entra_transport(
    options: types::EntraAuthOptions,
) -> (tempfile::TempDir, Client, Arc<jsonrpc::JsonRpcClient>) {
    let temp = tempfile::tempdir().unwrap();
    let (client_stream, server_stream) = tokio::io::duplex(8192);
    let (client_read, client_write) = tokio::io::split(client_stream);
    let (server_read, server_write) = tokio::io::split(server_stream);
    let client = Client::from_streams_with_entra_options(
        crate::ClientMode::CodingAgent,
        client_read,
        client_write,
        temp.path().to_owned(),
        Some(options),
    )
    .unwrap();
    let (notifications, _) = mpsc::unbounded_channel();
    let (requests, _) = tokio::sync::mpsc::unbounded_channel();
    let server = Arc::new(jsonrpc::JsonRpcClient::new(
        server_write,
        server_read,
        notifications,
        requests,
    ));
    (temp, client, server)
}

fn entra_identity_request() -> serde_json::Value {
    serde_json::json!({
        "request": {
            "kind": "identity", "clientId": "host-client", "reason": "signIn",
        }
    })
}

#[tokio::test]
async fn custom_streams_route_entra_callbacks_without_a_session() {
    let (_temp, client, server) = entra_transport(
        types::EntraAuthOptions::new("host-client").with_token_provider(
            |_: EntraTokenProviderArgs, _: EntraTokenProviderContext| async {
                Ok(EntraTokenProviderResult::Identity {
                    account_id: "host-account".to_owned(),
                    username: None,
                })
            },
        ),
    );
    server
        .register_request_handler(
            "connect",
            Arc::new(|params| {
                assert_eq!(params["entraTokenProvider"], true);
                Box::pin(async {
                    Ok(serde_json::json!({
                        "ok": true, "protocolVersion": 4, "version": "test",
                        "entraTokenProvider": true,
                    }))
                })
            }),
        )
        .unwrap();
    let result = tokio::time::timeout(Duration::from_secs(5), async {
        client.verify_protocol_version().await.unwrap();
        server
            .send_request(entra::ACQUIRE_METHOD, Some(entra_identity_request()))
            .await
    })
    .await;
    client.force_stop();
    server.force_close();
    let response = result.unwrap().unwrap();
    assert!(response.error.is_none());
    assert_eq!(
        response.result.unwrap(),
        serde_json::json!({"kind": "identity", "accountId": "host-account"}),
    );
}

#[tokio::test]
async fn pending_entra_acquisition_does_not_keep_the_client_alive() {
    let (entered, mut started) = tokio::sync::mpsc::unbounded_channel();
    let (_temp, client, server) = entra_transport(
        types::EntraAuthOptions::new("host-client").with_token_provider(
            move |_: EntraTokenProviderArgs, context: EntraTokenProviderContext| {
                let entered = entered.clone();
                async move {
                    entered.send(context.cancellation()).unwrap();
                    std::future::pending::<Result<EntraTokenProviderResult>>().await
                }
            },
        ),
    );
    client.inner.router.ensure_started(&client.inner);
    let peer = server.clone();
    let request = tokio::spawn(async move {
        peer.send_request(entra::ACQUIRE_METHOD, Some(entra_identity_request()))
            .await
    });
    let cancellation = tokio::time::timeout(Duration::from_secs(5), started.recv())
        .await
        .unwrap()
        .unwrap();
    let inner = Arc::downgrade(&client.inner);
    drop(client);
    let cancelled = tokio::time::timeout(Duration::from_secs(5), cancellation.cancelled()).await;
    if let Some(retained) = inner.upgrade() {
        Client::from_inner(retained).force_stop();
    }
    server.force_close();
    assert!(request.await.unwrap().is_err());
    assert!(
        cancelled.is_ok(),
        "dropping the client must cancel acquisition"
    );
    assert!(inner.upgrade().is_none());
}

#[tokio::test]
async fn entra_router_observes_an_inference_dispatcher_installed_later() {
    use crate::copilot_request_handler::*;

    struct NoNetwork;
    #[async_trait::async_trait]
    impl CopilotRequestHandler for NoNetwork {
        async fn send_request(
            &self,
            _: CopilotHttpRequest,
            _: &CopilotRequestContext,
        ) -> std::result::Result<CopilotHttpResponse, CopilotRequestError> {
            panic!("this routing test must not issue HTTP requests");
        }

        async fn open_websocket(
            &self,
            _: &CopilotRequestContext,
            _: CopilotWebSocketResponse,
        ) -> std::result::Result<Box<dyn CopilotWebSocketHandler>, CopilotRequestError> {
            panic!("this routing test must not open WebSockets");
        }
    }

    let (_temp, client, server) = entra_transport(
        types::EntraAuthOptions::new("host-client").with_token_provider(
            |_: EntraTokenProviderArgs, _: EntraTokenProviderContext| async {
                Ok(EntraTokenProviderResult::Unavailable)
            },
        ),
    );
    let dispatcher = Arc::new(CopilotRequestDispatcher::new(Arc::new(NoNetwork)));
    dispatcher.set_client(Arc::downgrade(&client.inner));
    assert!(client.inner.llm_inference.set(dispatcher).is_ok());
    client.inner.router.ensure_started(&client.inner);
    let response = tokio::time::timeout(
        Duration::from_secs(5),
        server.send_request("llmInference.unknown", Some(serde_json::json!({}))),
    )
    .await;
    client.force_stop();
    server.force_close();
    assert!(
        response
            .unwrap()
            .unwrap()
            .error
            .unwrap()
            .message
            .contains("unknown llmInference")
    );
}

mod cafe_metrics;

#[tokio::test]
async fn connect_forwards_optional_entra_configuration() {
    for entra in [
        None,
        Some(EntraAuthConfig {
            client_id: "host-entra-client".into(),
            mats_app_name: None,
            one_auth_application_id: None,
        }),
        Some(EntraAuthConfig {
            client_id: "host-entra-client".into(),
            mats_app_name: Some("Host App".into()),
            one_auth_application_id: Some("host-native-descriptor".into()),
        }),
    ] {
        let temp = tempfile::tempdir().unwrap();
        let (client_stream, server_stream) = tokio::io::duplex(8192);
        let (client_read, client_write) = tokio::io::split(client_stream);
        let (server_read, server_write) = tokio::io::split(server_stream);
        let client = Client::from_transport(
            client_read,
            jsonrpc::MessageWriter::stream(client_write),
            None,
            None,
            temp.path().to_owned(),
            None,
            None,
            false,
            false,
            false,
            false,
            None,
            None,
            None,
            ClientMode::CodingAgent,
            None,
            entra.clone().map(Into::into),
            false,
        )
        .unwrap();
        let (notifications, _) = mpsc::unbounded_channel();
        let (requests, _) = tokio::sync::mpsc::unbounded_channel();
        let server =
            jsonrpc::JsonRpcClient::new(server_write, server_read, notifications, requests);
        let (captured, mut received) = tokio::sync::mpsc::unbounded_channel();
        server
            .register_request_handler(
                "connect",
                Arc::new(move |params| {
                    captured.send(params).unwrap();
                    Box::pin(async {
                        Ok(serde_json::json!({
                            "ok": true, "protocolVersion": 4, "version": "test",
                        }))
                    })
                }),
            )
            .unwrap();
        let result =
            tokio::time::timeout(Duration::from_secs(5), client.verify_protocol_version()).await;
        client.force_stop();
        server.force_close();
        result.unwrap().unwrap();
        let params = received.try_recv().unwrap();
        assert_eq!(
            params.get("entra"),
            entra
                .map(|config| {
                    let mut expected = serde_json::json!({ "clientId": config.client_id });
                    if let Some(name) = config.mats_app_name {
                        expected["matsAppName"] = name.into();
                    }
                    if let Some(id) = config.one_auth_application_id {
                        expected["oneAuthApplicationId"] = id.into();
                    }
                    expected
                })
                .as_ref(),
        );
    }
}

#[tokio::test]
async fn settings_apply_top_key_preserves_value_presence_on_wire() {
    use crate::generated::api_types::UserSettingsApplyTopKeyRequest;

    for (name, value, delete) in [
        ("omitted", None, Some(false)),
        ("explicit null", Some(serde_json::Value::Null), None),
        ("value", Some(serde_json::json!("chosen")), None),
        ("delete", None, Some(true)),
    ] {
        let temp = tempfile::tempdir().unwrap();
        let (client_stream, server_stream) = tokio::io::duplex(8192);
        let (client_read, client_write) = tokio::io::split(client_stream);
        let (server_read, server_write) = tokio::io::split(server_stream);
        let client = Client::from_streams(
            crate::ClientMode::CodingAgent,
            client_read,
            client_write,
            temp.path().to_owned(),
        )
        .unwrap();
        let (notifications, _) = mpsc::unbounded_channel();
        let (requests, _) = tokio::sync::mpsc::unbounded_channel();
        let server =
            jsonrpc::JsonRpcClient::new(server_write, server_read, notifications, requests);
        let (captured, mut received) = tokio::sync::mpsc::unbounded_channel();
        server
            .register_request_handler(
                "user.settings.applyTopKey",
                Arc::new(move |params| {
                    captured.send(params).unwrap();
                    Box::pin(async { Ok(serde_json::json!({ "status": "ok" })) })
                }),
            )
            .unwrap();

        let result = tokio::time::timeout(
            Duration::from_secs(5),
            client
                .rpc()
                .user()
                .settings()
                .apply_top_key(UserSettingsApplyTopKeyRequest {
                    canonical_path: vec!["model".to_owned()],
                    top_key: "model".to_owned(),
                    value: value.clone(),
                    delete,
                    complete_staff_update_channel_migration: None,
                }),
        )
        .await;
        client.force_stop();
        server.force_close();

        result.expect(name).expect(name);
        let params = received.try_recv().expect(name);
        assert_eq!(params.get("value"), value.as_ref(), "{name}: {params}");
    }
}

#[test]
fn is_transport_failure_rejects_rpc_error() {
    let err = Error::with_message(ErrorKind::Rpc { code: -1 }, "bad");
    assert!(!err.is_transport_failure());
}

#[test]
fn default_transport_values_resolve_without_process_state() {
    assert!(matches!(
        RuntimeConnection::default_for_value(None, Default::default()).unwrap(),
        RuntimeConnection::InProcess(_)
    ));
    assert!(matches!(
        RuntimeConnection::default_for_value(Some(""), Default::default()).unwrap(),
        RuntimeConnection::InProcess(_)
    ));
    assert!(matches!(
        RuntimeConnection::default_for_value(Some("stdio"), Default::default()).unwrap(),
        RuntimeConnection::SpawnedStdio(_)
    ));
    assert!(matches!(
        RuntimeConnection::default_for_value(Some("INPROCESS"), Default::default()).unwrap(),
        RuntimeConnection::InProcess(_)
    ));
    let error = RuntimeConnection::default_for_value(Some("tcp"), Default::default()).unwrap_err();
    assert!(matches!(error.kind(), ErrorKind::InvalidConfig));
    let message = error.message().expect("error message");
    assert!(
        message.contains(DEFAULT_CONNECTION_ENV_VAR) && message.contains("'tcp'"),
        "{message}"
    );
}

#[cfg(feature = "runtime")]
#[test]
fn default_runtime_factory_uses_platform_keys_and_last_entry() {
    const CHILD_CASE: &str = "COPILOT_SDK_DEFAULT_TRANSPORT_ENV_TEST";
    if let Ok(case) = std::env::var(CHILD_CASE) {
        let runtime = RuntimeConnection::for_default(OwnedRuntimeOptions::new()).unwrap();
        if cfg!(windows) && case == "casing" {
            assert!(
                matches!(runtime, RuntimeConnection::InProcess(_)),
                "{runtime:?}"
            );
        } else {
            assert!(
                matches!(runtime, RuntimeConnection::SpawnedStdio(_)),
                "{runtime:?}"
            );
        }
        return;
    }

    let directory = tempfile::tempdir().unwrap();
    for case in ["casing", "duplicates"] {
        let mut command =
            std::process::Command::new(std::env::current_exe().expect("locate test executable"));
        command
            .args([
                "--exact",
                "tests::default_runtime_factory_uses_platform_keys_and_last_entry",
                "--nocapture",
            ])
            .current_dir(directory.path())
            .env(CHILD_CASE, case)
            .env(DEFAULT_CONNECTION_ENV_VAR, "stdio");
        match case {
            "casing" => {
                command.env("copilot_sdk_default_connection", "inprocess");
            }
            "duplicates" => {
                command
                    .env(DEFAULT_CONNECTION_ENV_VAR, "inprocess")
                    .env(DEFAULT_CONNECTION_ENV_VAR, "stdio");
            }
            _ => unreachable!(),
        }
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            const CREATE_NO_WINDOW: u32 = 0x0800_0000;
            command.creation_flags(CREATE_NO_WINDOW);
        }
        let output = command.output().expect("run isolated transport selection");
        assert!(
            output.status.success(),
            "{case}: {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }
}

#[cfg(feature = "runtime")]
#[test]
fn default_runtime_factory_captures_environment() {
    const CHILD: &str = "COPILOT_SDK_DEFAULT_FACTORY_TEST";
    if std::env::var_os(CHILD).is_some() {
        let runtime = RuntimeConnection::for_default(
            OwnedRuntimeOptions::new()
                .with_base_directory("owned-home")
                .with_session_idle_timeout_seconds(0),
        )
        .unwrap();
        // This subprocess runs only this synchronous test, with no runtime workers.
        unsafe { std::env::set_var(DEFAULT_CONNECTION_ENV_VAR, "invalid-after-factory") };
        let startup = ClientOptions::new(crate::ClientMode::CodingAgent)
            .with_runtime(runtime)
            .into_startup()
            .unwrap();
        assert!(matches!(startup.transport, Transport::InProcess));
        assert_eq!(
            startup.base_directory.as_deref(),
            Some(Path::new("owned-home"))
        );
        assert_eq!(startup.session_idle_timeout_seconds, Some(0));
        assert!(RuntimeConnection::for_default(Default::default()).is_err());
        return;
    }
    let mut command = std::process::Command::new(std::env::current_exe().unwrap());
    command
        .args([
            "--exact",
            "tests::default_runtime_factory_captures_environment",
        ])
        .current_dir(std::env::current_dir().unwrap())
        .env(CHILD, "1")
        .env(DEFAULT_CONNECTION_ENV_VAR, "InPrOcEsS");
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        command.creation_flags(CREATE_NO_WINDOW);
    }
    let output = command.output().unwrap();
    assert!(
        output.status.success(),
        "factory subprocess failed: {}\n{}\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn inprocess_captures_the_client_telemetry_option_without_changing_other_settings() {
    for disabled in [false, true] {
        let options = ClientOptions::new(crate::ClientMode::CodingAgent)
            .with_runtime(
                RuntimeConnection::for_default(
                    OwnedRuntimeOptions::new().with_base_directory("isolated-state"),
                )
                .expect("valid runtime default"),
            )
            .with_github(|group| group.with_token("fixture-token"))
            .with_observability(|group| group.with_disable_client_telemetry(disabled))
            .into_startup()
            .unwrap();
        let environment = inprocess_environment(&options).unwrap();
        assert!(environment.contains(&("COPILOT_HOME".to_owned(), "isolated-state".to_owned())));
        assert!(environment.contains(&(
            "COPILOT_SDK_AUTH_TOKEN".to_owned(),
            "fixture-token".to_owned()
        )));
        assert_eq!(
            environment
                .iter()
                .any(|(key, value)| key == "COPILOT_TELEMETRY_DISABLED" && value == "true"),
            disabled
        );
    }
}

#[test]
fn inprocess_sends_only_typed_overrides() {
    let options = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_runtime(
            RuntimeConnection::for_default(
                OwnedRuntimeOptions::new().with_base_directory("isolated-state"),
            )
            .expect("valid runtime default"),
        )
        .into_startup()
        .unwrap();
    let environment = inprocess_environment(&options).unwrap();
    assert_eq!(
        environment,
        vec![
            ("COPILOT_HOME".to_owned(), "isolated-state".to_owned()),
            (
                "COPILOT_CACHE_HOME".to_owned(),
                Path::new("isolated-state")
                    .join("cache")
                    .to_string_lossy()
                    .into_owned(),
            ),
        ]
    );
    assert!(
        inprocess_environment(
            &ClientOptions::new(crate::ClientMode::CodingAgent)
                .into_startup()
                .unwrap()
        )
        .unwrap()
        .is_empty()
    );
}

#[test]
fn build_command_honors_the_client_telemetry_option_over_an_environment_override() {
    let options = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::for_spawned_stdio(
            SpawnedProcessOptions::new().with_env([("COPILOT_TELEMETRY_DISABLED", "false")]),
        ))
        .with_observability(|group| group.with_disable_client_telemetry(true))
        .into_startup()
        .unwrap();
    let command = build_command(Path::new("copilot"), &options, Path::new("."));
    assert_eq!(
        env_value(&command, "COPILOT_TELEMETRY_DISABLED"),
        Some(std::ffi::OsStr::new("true"))
    );
}

/// The original `io::Error` of a failed spawn, found by walking the source chain.
fn spawn_os_error(error: &Error) -> Option<&std::io::Error> {
    let mut source = std::error::Error::source(error);
    while let Some(current) = source {
        if let Some(os_error) = current.downcast_ref::<std::io::Error>() {
            return Some(os_error);
        }
        source = std::error::Error::source(current);
    }
    None
}

async fn spawn_error(program: PathBuf, working_directory: &Path, tcp: bool) -> Error {
    let process = SpawnedProcessOptions::new()
        .with_program(CliProgram::Path(program))
        .with_working_directory(working_directory);
    let runtime = if tcp {
        RuntimeConnection::SpawnedTcp(SpawnedTcpRuntimeOptions {
            process,
            port: 0,
            connection_token: None,
        })
    } else {
        RuntimeConnection::SpawnedStdio(process)
    };
    let options = ClientOptions::new(crate::ClientMode::CodingAgent).with_runtime(runtime);
    match Client::start(options).await {
        Ok(_) => panic!("start unexpectedly spawned the CLI"),
        Err(error) => error,
    }
}

#[tokio::test]
async fn spawn_failure_names_a_working_directory_that_is_missing_or_not_a_directory() {
    let temp = tempfile::tempdir().unwrap();
    let file = temp.path().join("file");
    std::fs::write(&file, "").unwrap();
    let program = std::env::current_exe().unwrap();
    for tcp in [false, true] {
        for working_directory in [temp.path().join("missing"), file.clone()] {
            let error = spawn_error(program.clone(), &working_directory, tcp).await;
            assert_eq!(error.kind(), &ErrorKind::Io);
            let expected = format!(
                "working directory {} does not exist or is not a directory: ",
                working_directory.display()
            );
            assert!(error.to_string().starts_with(&expected), "{error}");
            let os_error = spawn_os_error(&error).expect("the io::Error stays in the source chain");
            assert!(os_error.raw_os_error().is_some(), "{os_error:?}");
        }
    }
}

#[cfg(unix)]
#[tokio::test]
async fn spawn_failure_does_not_blame_a_working_directory_it_cannot_inspect() {
    use std::os::unix::fs::PermissionsExt;

    let temp = tempfile::tempdir().unwrap();
    let parent = temp.path().join("locked");
    let working_directory = parent.join("cwd");
    std::fs::create_dir_all(&working_directory).unwrap();
    std::fs::set_permissions(&parent, std::fs::Permissions::from_mode(0o000)).unwrap();
    // Root, or a filesystem that ignores modes, can still inspect the directory.
    let inspectable = std::fs::metadata(&working_directory).is_ok();
    let program = temp.path().join("missing-copilot");
    let error = spawn_error(program.clone(), &working_directory, false).await;
    std::fs::set_permissions(&parent, std::fs::Permissions::from_mode(0o755)).unwrap();
    if inspectable {
        return;
    }
    assert_eq!(error.kind(), &ErrorKind::Io);
    let expected = format!(
        "failed to start {} in {}: ",
        program.display(),
        working_directory.display()
    );
    assert!(error.to_string().starts_with(&expected), "{error}");
}

#[tokio::test]
async fn spawn_failure_names_a_missing_program_and_its_working_directory() {
    let temp = tempfile::tempdir().unwrap();
    let program = temp.path().join("missing-copilot");
    for tcp in [false, true] {
        let error = spawn_error(program.clone(), temp.path(), tcp).await;
        assert_eq!(error.kind(), &ErrorKind::Io);
        let expected = format!(
            "failed to start {} in {}: ",
            program.display(),
            temp.path().display()
        );
        assert!(error.to_string().starts_with(&expected), "{error}");
        let os_error = spawn_os_error(&error).expect("the io::Error stays in the source chain");
        assert!(os_error.raw_os_error().is_some(), "{os_error:?}");
    }
}

#[cfg(not(feature = "in-process"))]
#[tokio::test]
async fn inprocess_requires_cargo_feature() {
    let error = Client::start(
        ClientOptions::new(crate::ClientMode::CodingAgent).with_runtime(
            RuntimeConnection::for_in_process(InProcessRuntimeOptions::new()),
        ),
    )
    .await
    .unwrap_err();

    assert!(error.to_string().contains("in-process"));
}

#[test]
fn build_command_typed_token_overrides_environment_removal() {
    let opts = StartupOptions {
        github_token: Some("secret".to_string()),
        transport: Transport::SpawnedStdio(
            SpawnedProcessOptions::new().with_env_remove(["COPILOT_SDK_AUTH_TOKEN"]),
        ),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    let action = cmd
        .as_std()
        .get_envs()
        .find(|(k, _)| *k == std::ffi::OsStr::new("COPILOT_SDK_AUTH_TOKEN"))
        .map(|(_, v)| v);
    assert_eq!(
        action,
        Some(Some(std::ffi::OsStr::new("secret"))),
        "explicit github_token should win over env_remove"
    );
}

#[test]
fn build_command_typed_token_overrides_environment_value() {
    let opts = StartupOptions {
        github_token: Some("from-options".to_string()),
        transport: Transport::SpawnedStdio(
            SpawnedProcessOptions::new().with_env([("COPILOT_SDK_AUTH_TOKEN", "from-env")]),
        ),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    let value = cmd
        .as_std()
        .get_envs()
        .find(|(k, _)| *k == std::ffi::OsStr::new("COPILOT_SDK_AUTH_TOKEN"))
        .and_then(|(_, v)| v);
    assert_eq!(value, Some(std::ffi::OsStr::new("from-options")));
}

#[test]
fn build_command_injects_github_token_by_default() {
    let opts = StartupOptions {
        github_token: Some("just-the-token".to_string()),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    let value = cmd
        .as_std()
        .get_envs()
        .find(|(k, _)| *k == std::ffi::OsStr::new("COPILOT_SDK_AUTH_TOKEN"))
        .and_then(|(_, v)| v);
    assert_eq!(value, Some(std::ffi::OsStr::new("just-the-token")));
}

#[test]
fn internal_test_options_override_owned_runtime_configuration() {
    let options = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::for_spawned_stdio(
            SpawnedProcessOptions::new()
                .with_env([("HTTP_PROXY", "inherited"), ("UNRELATED", "preserved")]),
        ))
        .with_internal_test_options(InternalTestOptions {
            copilot_api_url: Some("http://replay".into()),
            github_api_url: Some("http://github-replay".into()),
            proxy_url: Some("http://interception".into()),
            no_proxy: Some(String::new()),
            ca_certificate_path: Some("/test/ca.pem".into()),
        })
        .into_startup()
        .unwrap();
    let command = build_command(Path::new("/bin/echo"), &options, Path::new("/tmp"));
    let environment = inprocess_environment(&options).unwrap();
    for (key, expected) in [
        ("COPILOT_API_URL", "http://replay"),
        ("COPILOT_DEBUG_GITHUB_API_URL", "http://github-replay"),
        ("HTTP_PROXY", "http://interception"),
        ("HTTPS_PROXY", "http://interception"),
        ("NO_PROXY", ""),
        ("NODE_EXTRA_CA_CERTS", "/test/ca.pem"),
        ("SSL_CERT_FILE", "/test/ca.pem"),
        ("REQUESTS_CA_BUNDLE", "/test/ca.pem"),
        ("CURL_CA_BUNDLE", "/test/ca.pem"),
        ("GIT_SSL_CAINFO", "/test/ca.pem"),
    ] {
        assert_eq!(
            env_value(&command, key),
            Some(std::ffi::OsStr::new(expected)),
            "{key}"
        );
        assert_eq!(
            environment
                .iter()
                .find(|(name, _)| name == key)
                .map(|(_, value)| value.as_str()),
            Some(expected),
            "{key}"
        );
    }
    assert_eq!(
        env_value(&command, "UNRELATED"),
        Some(std::ffi::OsStr::new("preserved"))
    );
    assert_eq!(
        options
            .transport
            .spawned_process()
            .unwrap()
            .env
            .as_ref()
            .unwrap()[0]
            .1,
        std::ffi::OsStr::new("inherited")
    );
}

fn build_command(program: &Path, options: &StartupOptions, cwd: &Path) -> Command {
    let default_process = SpawnedProcessOptions::default();
    Client::build_command(
        program,
        options,
        options
            .transport
            .spawned_process()
            .unwrap_or(&default_process),
        cwd,
    )
}

fn env_value<'a>(cmd: &'a tokio::process::Command, key: &str) -> Option<&'a std::ffi::OsStr> {
    cmd.as_std()
        .get_envs()
        .find(|(k, _)| {
            if cfg!(windows) {
                k.as_encoded_bytes().eq_ignore_ascii_case(key.as_bytes())
            } else {
                *k == std::ffi::OsStr::new(key)
            }
        })
        .and_then(|(_, v)| v)
}

#[test]
fn build_command_sets_otel_env_when_telemetry_enabled() {
    let opts = StartupOptions {
        telemetry: Some(TelemetryConfig {
            otlp_endpoint: Some("http://collector:4318".to_string()),
            otlp_protocol: Some(OtlpHttpProtocol::HttpProtobuf),
            file_path: Some(PathBuf::from("/var/log/copilot.jsonl")),
            exporter_type: Some(OtelExporterType::OtlpHttp),
            source_name: Some("my-app".to_string()),
            capture_content: Some(true),
        }),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert_eq!(
        env_value(&cmd, "COPILOT_OTEL_ENABLED"),
        Some(std::ffi::OsStr::new("true")),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_EXPORTER_OTLP_ENDPOINT"),
        Some(std::ffi::OsStr::new("http://collector:4318")),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_EXPORTER_OTLP_PROTOCOL"),
        Some(std::ffi::OsStr::new("http/protobuf")),
    );
    assert_eq!(
        env_value(&cmd, "COPILOT_OTEL_FILE_EXPORTER_PATH"),
        Some(std::ffi::OsStr::new("/var/log/copilot.jsonl")),
    );
    assert_eq!(
        env_value(&cmd, "COPILOT_OTEL_EXPORTER_TYPE"),
        Some(std::ffi::OsStr::new("otlp-http")),
    );
    assert_eq!(
        env_value(&cmd, "COPILOT_OTEL_SOURCE_NAME"),
        Some(std::ffi::OsStr::new("my-app")),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
        Some(std::ffi::OsStr::new("true")),
    );
    validate_runtime_startup_options(&opts).unwrap();
    let environment = inprocess_environment(&opts).unwrap();
    for (key, value) in cmd.as_std().get_envs().filter(|(key, _)| {
        key.to_string_lossy().starts_with("OTEL_")
            || key.to_string_lossy().starts_with("COPILOT_OTEL_")
    }) {
        assert!(
            environment.contains(&(
                key.to_str().unwrap().to_owned(),
                value.unwrap().to_str().unwrap().to_owned()
            )),
            "missing in-process telemetry setting {key:?}={value:?}"
        );
    }
}

#[test]
fn build_command_omits_otel_env_when_telemetry_none() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .into_startup()
        .unwrap();
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    for key in [
        "COPILOT_OTEL_ENABLED",
        "OTEL_EXPORTER_OTLP_ENDPOINT",
        "OTEL_EXPORTER_OTLP_PROTOCOL",
        "COPILOT_OTEL_FILE_EXPORTER_PATH",
        "COPILOT_OTEL_EXPORTER_TYPE",
        "COPILOT_OTEL_SOURCE_NAME",
        "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT",
        "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT",
    ] {
        assert!(
            env_value(&cmd, key).is_none(),
            "expected {key} to be unset when telemetry is None",
        );
    }
}

#[test]
fn attached_runtime_rejects_runtime_startup_configuration() {
    type ConfigureOptions = fn(StartupOptions) -> StartupOptions;
    let cases: [(&str, ConfigureOptions); 8] = [
        ("github.token", |mut options| {
            options.github_token = Some(String::new());
            options
        }),
        ("github.use_logged_in_user", |mut options| {
            options.use_logged_in_user = Some(false);
            options
        }),
        ("observability.log_level", |mut options| {
            options.log_level = Some(LogLevel::Info);
            options
        }),
        (
            "runtime.owned.session_idle_timeout_seconds",
            |mut options| {
                options.session_idle_timeout_seconds = Some(0);
                options
            },
        ),
        ("observability.telemetry", |mut options| {
            options.telemetry = Some(TelemetryConfig::default());
            options
        }),
        ("runtime.owned.base_directory", |mut options| {
            options.base_directory = Some(PathBuf::new());
            options
        }),
        ("observability.disable_client_telemetry", |mut options| {
            options.disable_client_telemetry = true;
            options
        }),
        ("internal_test_options", |mut options| {
            options.internal_test_options = Some(InternalTestOptions::default());
            options
        }),
    ];
    for attached_tcp in [false, true] {
        for (name, configure) in cases {
            let mut options = StartupOptions::default();
            if attached_tcp {
                options.transport = Transport::Tcp {
                    host: "127.0.0.1".into(),
                    port: 1,
                    connection_token: None,
                };
            } else {
                options.transport = Transport::InProcess;
                options.in_process_server_id = Some(42);
            }
            validate_runtime_startup_options(&options)
                .expect("an attached runtime accepts omitted startup settings");
            let error = validate_runtime_startup_options(&configure(options)).expect_err(&format!(
                "{name} must not silently configure an attached runtime"
            ));
            assert_eq!(error.kind(), &ErrorKind::InvalidConfig, "{name}: {error}");
            assert!(error.to_string().contains(name), "{name}: {error}");
        }
    }
}

#[test]
fn inprocess_telemetry_requires_an_owned_runtime() {
    let mut opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::for_in_process(
            InProcessRuntimeOptions::new(),
        ))
        .with_observability(|group| group.with_telemetry(TelemetryConfig::default()))
        .into_startup()
        .unwrap();
    validate_runtime_startup_options(&opts).unwrap();
    assert_eq!(
        inprocess_environment(&opts).unwrap(),
        vec![("COPILOT_OTEL_ENABLED".to_owned(), "true".to_owned())]
    );
    opts.in_process_server_id = Some(42);
    let error = validate_runtime_startup_options(&opts).unwrap_err();
    assert_eq!(*error.kind(), ErrorKind::InvalidConfig);
    assert!(
        error
            .message()
            .unwrap()
            .contains("requires an owned runtime")
    );
}

#[test]
fn build_command_sets_standard_and_legacy_capture_modes() {
    for (capture, mode) in [(true, "SPAN_ONLY"), (false, "NO_CONTENT")] {
        let opts = StartupOptions {
            telemetry: Some(TelemetryConfig::new().with_capture_content(capture)),
            ..StartupOptions::default()
        };
        let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
        assert_eq!(
            env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
            Some(std::ffi::OsStr::new(mode)),
        );
        assert_eq!(
            env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
            Some(std::ffi::OsStr::new(if capture { "true" } else { "false" })),
        );
        let environment = inprocess_environment(&opts).unwrap();
        assert!(environment.contains(&(
            "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT".to_owned(),
            mode.to_owned()
        )));
        assert!(environment.contains(&(
            "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT".to_owned(),
            capture.to_string()
        )));
    }
}

#[test]
fn build_command_omits_unset_telemetry_fields() {
    let opts = StartupOptions {
        telemetry: Some(TelemetryConfig {
            otlp_endpoint: Some("http://collector:4318".to_string()),
            ..Default::default()
        }),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    // The one set field plus the implicit enabled flag should propagate.
    assert_eq!(
        env_value(&cmd, "COPILOT_OTEL_ENABLED"),
        Some(std::ffi::OsStr::new("true")),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_EXPORTER_OTLP_ENDPOINT"),
        Some(std::ffi::OsStr::new("http://collector:4318")),
    );
    // None of the other fields should leak as env vars.
    for key in [
        "OTEL_EXPORTER_OTLP_PROTOCOL",
        "COPILOT_OTEL_FILE_EXPORTER_PATH",
        "COPILOT_OTEL_EXPORTER_TYPE",
        "COPILOT_OTEL_SOURCE_NAME",
        "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT",
        "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT",
    ] {
        assert!(env_value(&cmd, key).is_none(), "{key} should be unset");
    }
}

#[test]
fn build_command_typed_telemetry_overrides_environment() {
    let opts = StartupOptions {
        telemetry: Some(TelemetryConfig {
            otlp_endpoint: Some("http://from-config:4318".to_string()),
            capture_content: Some(true),
            ..Default::default()
        }),
        transport: Transport::SpawnedStdio(SpawnedProcessOptions {
            env: Some(vec![
                (
                    std::ffi::OsString::from("OTEL_EXPORTER_OTLP_ENDPOINT"),
                    std::ffi::OsString::from("http://from-user-env:4318"),
                ),
                (
                    std::ffi::OsString::from("OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
                    std::ffi::OsString::from("NO_CONTENT"),
                ),
                (
                    std::ffi::OsString::from("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
                    std::ffi::OsString::from("false"),
                ),
            ]),
            ..Default::default()
        }),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert_eq!(
        env_value(&cmd, "OTEL_EXPORTER_OTLP_ENDPOINT"),
        Some(std::ffi::OsStr::new("http://from-config:4318")),
        "explicit telemetry config should override process.env",
    );
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
        Some(std::ffi::OsStr::new("SPAN_ONLY")),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
        Some(std::ffi::OsStr::new("true")),
    );
}

#[test]
fn build_command_preserves_capture_environment_when_typed_setting_is_omitted() {
    for legacy in ["false", "TrUe", "invalid"] {
        let opts = StartupOptions {
            telemetry: Some(TelemetryConfig::new()),
            transport: Transport::SpawnedStdio(SpawnedProcessOptions {
                env: Some(vec![(
                    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT".into(),
                    legacy.into(),
                )]),
                ..Default::default()
            }),
            ..StartupOptions::default()
        };
        let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
        assert_eq!(
            env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
            None,
        );
        assert_eq!(
            env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
            Some(std::ffi::OsStr::new(legacy)),
        );
    }
}

#[test]
fn build_command_typed_capture_overrides_explicit_standard_and_legacy_environment() {
    let opts = StartupOptions {
        telemetry: Some(TelemetryConfig::new().with_capture_content(true)),
        transport: Transport::SpawnedStdio(SpawnedProcessOptions {
            env: Some(vec![
                (
                    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT".into(),
                    "false".into(),
                ),
                (
                    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT".into(),
                    "SPAN_AND_EVENT".into(),
                ),
            ]),
            ..Default::default()
        }),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
        Some(std::ffi::OsStr::new("SPAN_ONLY")),
    );
}

#[test]
fn build_command_typed_capture_overrides_legacy_environment_removal() {
    let opts = StartupOptions {
        telemetry: Some(TelemetryConfig::new().with_capture_content(true)),
        transport: Transport::SpawnedStdio(
            SpawnedProcessOptions::new()
                .with_env_remove(["OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"]),
        ),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
        Some(std::ffi::OsStr::new("SPAN_ONLY")),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
        Some(std::ffi::OsStr::new("true")),
    );
}

#[test]
fn build_command_capture_env_keys_follow_platform_casing() {
    for configured in [false, true] {
        for duplicate in [false, true] {
            let mut env: Vec<(OsString, OsString)> = Vec::new();
            if duplicate {
                env.push((
                    "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT".into(),
                    if configured { "true" } else { "false" }.into(),
                ));
            }
            env.push((
                "otel_instrumentation_genai_capture_message_content".into(),
                if configured { "false" } else { "true" }.into(),
            ));
            let opts = StartupOptions {
                telemetry: Some(TelemetryConfig::new().with_capture_content(configured)),
                transport: Transport::SpawnedStdio(SpawnedProcessOptions::new().with_env(env)),
                ..StartupOptions::default()
            };
            let cmd = build_command(Path::new("copilot"), &opts, Path::new("."));
            let capture = configured;
            assert_eq!(
                env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
                Some(std::ffi::OsStr::new(if capture {
                    "SPAN_ONLY"
                } else {
                    "NO_CONTENT"
                })),
                "configured={configured}, duplicate={duplicate}",
            );
        }
    }
}

#[test]
fn build_command_capture_env_removals_follow_platform_casing() {
    let opts = StartupOptions {
        telemetry: Some(TelemetryConfig::new().with_capture_content(false)),
        transport: Transport::SpawnedStdio(SpawnedProcessOptions {
            env: Some(vec![(
                "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT".into(),
                "true".into(),
            )]),
            env_remove: vec!["otel_instrumentation_genai_capture_message_content".into()],
            ..Default::default()
        }),
        ..StartupOptions::default()
    };
    let cmd = build_command(Path::new("copilot"), &opts, Path::new("."));
    let capture = false;
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT"),
        Some(std::ffi::OsStr::new(if capture {
            "SPAN_ONLY"
        } else {
            "NO_CONTENT"
        })),
    );
    assert_eq!(
        env_value(&cmd, "OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT"),
        Some(std::ffi::OsStr::new("false")),
    );
}

#[test]
fn build_command_sets_copilot_home_env_when_configured() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_runtime(
            RuntimeConnection::for_default(
                OwnedRuntimeOptions::new().with_base_directory(PathBuf::from("/custom/copilot")),
            )
            .expect("valid runtime default"),
        )
        .into_startup()
        .unwrap();
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert_eq!(
        env_value(&cmd, "COPILOT_HOME"),
        Some(std::ffi::OsStr::new("/custom/copilot")),
    );

    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .into_startup()
        .unwrap();
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert!(env_value(&cmd, "COPILOT_HOME").is_none());
}

#[test]
fn build_command_sets_connection_token_env_when_configured() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::for_spawned_tcp(
            SpawnedTcpRuntimeOptions {
                process: SpawnedProcessOptions::new(),
                port: 0,
                connection_token: Some("secret-token".to_string()),
            },
        ))
        .into_startup()
        .unwrap();
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert_eq!(
        env_value(&cmd, "COPILOT_CONNECTION_TOKEN"),
        Some(std::ffi::OsStr::new("secret-token")),
    );

    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .into_startup()
        .unwrap();
    let cmd = build_command(Path::new("/bin/echo"), &opts, Path::new("/tmp"));
    assert!(env_value(&cmd, "COPILOT_CONNECTION_TOKEN").is_none());
}

#[tokio::test]
async fn empty_mode_requires_persistence_for_spawned_transports() {
    let temp = tempfile::tempdir().unwrap();
    let process = SpawnedProcessOptions::new().with_program(temp.path().join("missing-runtime"));
    for runtime in [
        RuntimeConnection::SpawnedStdio(process.clone()),
        RuntimeConnection::SpawnedTcp(SpawnedTcpRuntimeOptions {
            process,
            port: 0,
            connection_token: None,
        }),
    ] {
        let options = ClientOptions::new(ClientMode::Empty).with_runtime(runtime);
        let error = Client::start(options).await.unwrap_err();
        assert_eq!(error.kind(), &ErrorKind::InvalidConfig, "{error}");
        assert!(
            error
                .to_string()
                .contains("no implicit ~/.copilot fallback"),
            "{error}"
        );
    }
}

#[tokio::test]
async fn start_rejects_empty_connection_token() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent).with_runtime(
        RuntimeConnection::for_spawned_tcp(SpawnedTcpRuntimeOptions {
            process: SpawnedProcessOptions::new()
                .with_program(CliProgram::Path(PathBuf::from("/bin/echo"))),
            port: 0,
            connection_token: Some(String::new()),
        }),
    );
    let err = Client::start(opts).await.unwrap_err();
    assert!(
        matches!(err.kind(), ErrorKind::InvalidConfig),
        "got {err:?}"
    );
}

#[tokio::test]
async fn start_rejects_invalid_tcp_endpoint_before_connecting() {
    for (host, port) in [("http://127.0.0.1", 8080), ("127.0.0.1", 0)] {
        let options = ClientOptions::new(crate::ClientMode::CodingAgent)
            .with_runtime(RuntimeConnection::for_tcp(host, port));
        let error = Client::start(options).await.unwrap_err();
        assert!(
            matches!(error.kind(), ErrorKind::InvalidConfig),
            "{host}:{port}: {error:?}"
        );
        assert!(
            error.to_string().contains("TCP connection requires"),
            "{host}:{port}: {error:?}"
        );
    }
}

#[tokio::test]
async fn start_rejects_empty_external_connection_token() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent).with_runtime(
        RuntimeConnection::Tcp(TcpRuntimeOptions {
            host: "127.0.0.1".to_string(),
            port: 1,
            connection_token: Some(String::new()),
        }),
    );
    let err = Client::start(opts).await.unwrap_err();
    assert!(
        matches!(err.kind(), ErrorKind::InvalidConfig),
        "got {err:?}"
    );
}

#[test]
fn session_idle_timeout_args_omitted_for_zero() {
    let opts = StartupOptions {
        session_idle_timeout_seconds: Some(0),
        ..StartupOptions::default()
    };
    assert!(Client::session_idle_timeout_args(&opts).is_empty());
}

#[test]
fn session_idle_timeout_args_emit_flag_for_positive_value() {
    let opts = StartupOptions {
        session_idle_timeout_seconds: Some(300),
        ..StartupOptions::default()
    };
    assert_eq!(
        Client::session_idle_timeout_args(&opts),
        vec!["--session-idle-timeout".to_string(), "300".to_string()]
    );
}

#[test]
fn log_level_args_omitted_when_unset() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .into_startup()
        .unwrap();
    assert!(opts.log_level.is_none());
    assert!(
        Client::log_level_args(&opts).is_empty(),
        "with no caller-supplied log_level the SDK must not pass --log-level"
    );
}

#[test]
fn log_level_args_emit_flag_when_set() {
    let opts = ClientOptions::new(crate::ClientMode::CodingAgent)
        .with_observability(|group| group.with_log_level(LogLevel::Debug))
        .into_startup()
        .unwrap();
    assert_eq!(Client::log_level_args(&opts), vec!["--log-level", "debug"]);
}

#[test]
fn cli_mode_opts_into_process_logging_without_changing_empty_mode_environment() {
    for (mode, expected) in [
        (ClientMode::Empty, None),
        (ClientMode::CodingAgent, Some("1")),
    ] {
        let options = ClientOptions::new(mode)
            .with_runtime(RuntimeConnection::SpawnedStdio(
                SpawnedProcessOptions::new()
                    .with_env([("COPILOT_RUNTIME_PROCESS_FILE_LOGGING", "opposite")]),
            ))
            .into_startup()
            .unwrap();
        let command = build_command(Path::new("copilot-runtime"), &options, Path::new("."));
        let actual = command
            .as_std()
            .get_envs()
            .find(|(key, _)| *key == std::ffi::OsStr::new("COPILOT_RUNTIME_PROCESS_FILE_LOGGING"))
            .and_then(|(_, value)| value);
        assert_eq!(
            actual,
            Some(std::ffi::OsStr::new(expected.unwrap_or("opposite"))),
            "mode: {mode:?}"
        );
    }
}

#[test]
fn client_options_debug_redacts_handler() {
    struct StubHandler;
    #[async_trait]
    impl ListModelsHandler for StubHandler {
        async fn list_models(&self) -> Result<Vec<Model>> {
            Ok(vec![])
        }
    }
    let opts = StartupOptions {
        on_list_models: Some(Arc::new(StubHandler)),
        github_token: Some("secret-token".into()),
        ..StartupOptions::default()
    };
    let debug = format!("{opts:?}");
    assert!(debug.contains("on_list_models: Some(\"<set>\")"));
    assert!(debug.contains("github_token: Some(\"<redacted>\")"));
    assert!(!debug.contains("secret-token"));
}

#[tokio::test(start_paused = true)]
async fn list_models_serializes_concurrent_cache_misses() {
    use std::sync::atomic::{AtomicUsize, Ordering};

    struct SlowCountingHandler {
        calls: Arc<AtomicUsize>,
        models: Vec<Model>,
    }
    #[async_trait]
    impl ListModelsHandler for SlowCountingHandler {
        async fn list_models(&self) -> Result<Vec<Model>> {
            self.calls.fetch_add(1, Ordering::SeqCst);
            tokio::time::sleep(std::time::Duration::from_millis(25)).await;
            Ok(self.models.clone())
        }
    }

    let calls = Arc::new(AtomicUsize::new(0));
    let model = Model {
        id: "single-flight-model".into(),
        name: "Single Flight Model".into(),
        ..Default::default()
    };
    let handler: Arc<dyn ListModelsHandler> = Arc::new(SlowCountingHandler {
        calls: Arc::clone(&calls),
        models: vec![model],
    });
    let client = client_with_list_models_handler(handler);

    let (first, second) = tokio::join!(client.list_models(), client.list_models());
    assert_eq!(first.unwrap()[0].id, "single-flight-model");
    assert_eq!(second.unwrap()[0].id, "single-flight-model");
    assert_eq!(calls.load(Ordering::SeqCst), 1);
}

#[tokio::test]
async fn connection_state_tracks_construction_stop_and_force_stop() {
    for force in [false, true] {
        let (client_write, _server_read) = tokio::io::duplex(8192);
        let (_server_write, client_read) = tokio::io::duplex(8192);
        let client = Client::from_streams(
            crate::ClientMode::CodingAgent,
            client_read,
            client_write,
            std::env::temp_dir(),
        )
        .unwrap();
        assert_eq!(*client.inner.state.lock(), ConnectionState::Connected);

        if force {
            client.force_stop();
        } else {
            client.stop().await.expect("stop a client with no child");
        }
        assert_eq!(
            *client.inner.state.lock(),
            ConnectionState::Disconnected,
            "force={force}"
        );
    }
}

#[tokio::test(start_paused = true)]
async fn cancelled_resume_session_unregisters_pending_session() {
    let (client_write, _server_read) = tokio::io::duplex(8192);
    let (_server_write, client_read) = tokio::io::duplex(8192);
    let client = Client::from_streams(
        crate::ClientMode::CodingAgent,
        client_read,
        client_write,
        std::env::temp_dir(),
    )
    .unwrap();
    assert!(client.startup_timings().is_none());
    let session_id = SessionId::new("resume-cancel-test");
    let handle = tokio::spawn({
        let client = client.clone();
        async move {
            client
                .resume_session(ResumeSessionOptions::new(session_id))
                .await
        }
    });

    wait_for_pending_session_registration(&client).await;
    handle.abort();
    let _ = handle.await;

    assert!(client.inner.router.session_ids().is_empty());
    client.force_stop();
}

#[cfg(any(unix, windows))]
#[tokio::test]
async fn dropping_last_client_kills_spawned_cli() {
    let temp = tempfile::tempdir().unwrap();
    let ready = temp.path().join("ready");
    // `Client::build_command` sets `kill_on_drop(true)`, so dropping the
    // `Child` the client owns would kill the process even if the client's
    // own drop never sent a kill. Turn it off so only that kill can stop it.
    let mut child = test_child_command(temp.path(), &ready)
        .kill_on_drop(false)
        .spawn()
        .unwrap();
    let exited = child.stdout.take().expect("piped child stdout");
    let (client_write, _server_read) = tokio::io::duplex(64);
    let (_server_write, client_read) = tokio::io::duplex(64);
    let client = Client::from_transport(
        client_read,
        jsonrpc::MessageWriter::stream(client_write),
        Some(child),
        None,
        temp.path().to_path_buf(),
        None,
        None,
        false,
        false,
        false,
        false,
        None,
        None,
        None,
        ClientMode::CodingAgent,
        None,
        None,
        false,
    )
    .unwrap();

    wait_for_test_child(&ready).await;
    drop(client);

    assert_test_child_killed(exited).await;
}

#[cfg(any(unix, windows))]
#[tokio::test]
async fn spawned_child_is_killed_when_dropped() {
    let temp = tempfile::tempdir().unwrap();
    let ready = temp.path().join("ready");
    let mut child = test_child_command(temp.path(), &ready).spawn().unwrap();
    let exited = child.stdout.take().expect("piped child stdout");

    wait_for_test_child(&ready).await;
    drop(child);

    assert_test_child_killed(exited).await;
}

/// A child that reports readiness, then idles for a minute without writing to
/// its piped stdout, so EOF on that pipe means the process exited.
#[cfg(any(unix, windows))]
fn test_child_command(temp: &Path, ready: &Path) -> Command {
    let mut command = build_command(
        Path::new("node"),
        &ClientOptions::new(crate::ClientMode::CodingAgent)
            .into_startup()
            .unwrap(),
        temp,
    );
    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        command.creation_flags(CREATE_NO_WINDOW);
    }
    command
        .args([
            "-e",
            r#"
                const fs = require("node:fs");
                fs.writeFileSync(process.env.READY, "ready");
                setTimeout(() => {}, 60_000);
                "#,
        ])
        .env("READY", ready)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit());
    command
}

#[cfg(any(unix, windows))]
async fn wait_for_test_child(ready: &Path) {
    let deadline = tokio::time::Instant::now() + Duration::from_secs(30);
    while !ready.exists() {
        assert!(
            tokio::time::Instant::now() < deadline,
            "child did not report readiness"
        );
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
}

/// Waits for EOF on the child's stdout, which arrives only once the process
/// has exited. A child nobody killed keeps the pipe open for its full minute.
#[cfg(any(unix, windows))]
async fn assert_test_child_killed(mut stdout: tokio::process::ChildStdout) {
    let mut output = Vec::new();
    tokio::time::timeout(
        Duration::from_secs(10),
        tokio::io::AsyncReadExt::read_to_end(&mut stdout, &mut output),
    )
    .await
    .expect("child survived after its owner was dropped")
    .expect("read child stdout");
    assert!(output.is_empty(), "test child wrote to stdout");
}

fn client_with_list_models_handler(handler: Arc<dyn ListModelsHandler>) -> Client {
    Client {
        ahp_host_sessions: None,
        inner: Arc::new(ClientInner {
            ahp_host_callbacks: Arc::new(ahp_host::ExitCallbacks::default()),
            ahp_host_sessions: std::sync::Weak::new(),
            child: parking_lot::Mutex::new(None),
            owns_stdio: false,
            force_stop_requested: tokio_util::sync::CancellationToken::new(),
            process_tree: parking_lot::Mutex::new(None),
            #[cfg(feature = "in-process")]
            ffi_host: parking_lot::Mutex::new(None),
            rpc: {
                let (req_tx, _req_rx) = mpsc::unbounded_channel();
                let (notif_tx, _notif_rx) = mpsc::unbounded_channel();
                let (read_pipe, _write_pipe) = tokio::io::duplex(64);
                let (_unused_read, write_pipe) = tokio::io::duplex(64);
                JsonRpcClient::new(write_pipe, read_pipe, notif_tx, req_tx)
            },
            cwd: PathBuf::from("."),
            request_rx: parking_lot::Mutex::new(None),
            router: router::SessionRouter::new(),
            github_token_registry: Arc::new(github_token::GitHubTokenRegistry::new()),
            negotiated_protocol_version: OnceLock::new(),
            state: parking_lot::Mutex::new(ConnectionState::Connected),
            lifecycle_tx: broadcast::channel(16).0,
            on_list_models: Some(handler),
            models_cache: parking_lot::Mutex::new(Arc::new(tokio::sync::OnceCell::new())),
            session_fs_configured: false,
            session_fs_sqlite_declared: false,
            session_fs_binary_declared: false,
            session_fs_read_file_range_declared: false,
            llm_inference: OnceLock::new(),
            extension_launch_provider: Arc::new(
                extension_launch_provider::ExtensionLaunchProviderDispatcher::new(None),
            ),
            installation_confirmation: Arc::new(
                installation_confirmation::InstallationConfirmationDispatcher::new(),
            ),
            shell_authentication_requests:
                shell_authentication::PendingShellAuthenticationRequests::default(),
            entra_token: Arc::new(entra::EntraTokenDispatcher::new(None)),
            on_github_telemetry: None,
            on_get_trace_context: None,
            effective_connection_token: None,
            mode: ClientMode::CodingAgent,
            client_info: None,
            entra: None,
            startup_timings: OnceLock::new(),
        }),
    }
}

async fn wait_for_pending_session_registration(client: &Client) {
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(1);
    while client.inner.router.session_ids().is_empty() {
        assert!(
            tokio::time::Instant::now() < deadline,
            "session was not registered"
        );
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    }
}
