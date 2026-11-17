//! Language-specific admission callbacks through the built CLI/Node bridge.
#![cfg(feature = "test-support")]
#![allow(clippy::unwrap_used)]

#[path = "support/byok.rs"]
mod byok;
#[allow(dead_code)]
#[path = "e2e/runtime_host_support.rs"]
mod host_support;
#[allow(dead_code)]
#[path = "e2e/support.rs"]
mod support;

use copilot_sdk::{OwnedRuntimeOptions, RuntimeConnection, SpawnedProcessOptions};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use copilot_sdk::handler::ApproveAllHandler;
use copilot_sdk::{
    AhpAuthenticationRequest, AhpAuthenticationResult, AhpHostAuthentication, AhpHostOptions,
    AhpSessionAuthentication, AhpSessionRequest, AhpSessionResumeRequest, CliProgram, Client,
    ClientMode, ClientOptions, ProviderConfig,
};
use futures_util::FutureExt;
use serde_json::{Value, json};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

const RESOURCE: &str = "https://copilot.github.com/hosting/authentication";

fn options(home: &Path) -> ClientOptions {
    let cli = PathBuf::from(std::env::var("COPILOT_CLI_PATH").expect("set built CLI path"));
    let mut process = SpawnedProcessOptions::new()
        .with_working_directory(home.parent().unwrap().join("work"))
        .with_env(std::env::vars_os().chain([
            ("COPILOT_HOME".into(), home.as_os_str().to_owned()),
            ("GH_TOKEN".into(), "".into()),
            ("GITHUB_TOKEN".into(), "".into()),
            ("COPILOT_GITHUB_TOKEN".into(), "".into()),
            ("COPILOT_SDK_AUTH_TOKEN".into(), "".into()),
            ("COPILOT_HMAC_KEY".into(), "".into()),
            ("CAPI_HMAC_KEY".into(), "".into()),
        ]));
    if cli.extension().is_some_and(|ext| ext == "js") {
        process = process
            .with_program(CliProgram::Path("node".into()))
            .with_args([cli]);
    } else {
        process = process.with_program(CliProgram::Path(cli));
    }
    ClientOptions::new(copilot_sdk::ClientMode::CodingAgent)
        .with_runtime(RuntimeConnection::SpawnedStdio(
            process.with_owned(|_| OwnedRuntimeOptions::new().with_base_directory(home)),
        ))
        .with_github(|group| group.with_use_logged_in_user(false))
}

fn fixture_home() -> (tempfile::TempDir, PathBuf) {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path().canonicalize().unwrap();
    let home = root.join("home");
    std::fs::create_dir(&home).unwrap();
    std::fs::create_dir(root.join("work")).unwrap();
    (fixture, home)
}

fn custom(expiry: &'static str, identity: &'static str) -> AhpHostAuthentication {
    AhpHostAuthentication::custom(
        "rust-owner",
        move |request: AhpAuthenticationRequest| async move {
            assert_eq!(request.token, support::DEFAULT_TEST_TOKEN);
            assert!(!request.cancellation_token.is_cancelled());
            Ok(Some(AhpAuthenticationResult {
                identity: identity.into(),
                expires_at: expiry.into(),
            }))
        },
    )
}

async fn create(ahp: &host_support::Ahp, home: &Path) -> Result<String, ahp::ClientError> {
    let uri = format!("ahp-session:/{}", uuid::Uuid::new_v4());
    let _: Value = ahp.client.request("createSession", json!({
        "channel":uri,"provider":"copilot",
        "workingDirectories":[reqwest::Url::from_directory_path(home.parent().unwrap().join("work")).unwrap().as_str()]
    })).await?;
    Ok(uri)
}

#[tokio::test]
async fn real_host_rejects_missing_policy_bad_identity_and_invalid_expiry() {
    let (_fixture, home_path) = fixture_home();
    let owner = Client::start(options(&home_path)).await.unwrap();
    let result = std::panic::AssertUnwindSafe(async {
        for mode in [ClientMode::CodingAgent, ClientMode::Empty] {
            let client = Client::start(options(&home_path).with_mode(mode))
                .await
                .unwrap();
            let rejection = client
                .start_ahp_host(AhpHostOptions::new().with_local_server(Default::default()))
                .await;
            client.stop().await.unwrap();
            assert!(
                rejection
                    .unwrap_err()
                    .to_string()
                    .contains("explicit authentication")
            );
        }
        for (expiry, identity) in [
            ("invalid", "rust-owner"),
            ("2000-01-01T00:00:00Z", "rust-owner"),
            ("2099-01-01T00:00:00Z", "another-owner"),
        ] {
            let host = owner
                .start_ahp_host(
                    AhpHostOptions::new()
                        .with_authentication(custom(expiry, identity))
                        .with_local_server(Default::default()),
                )
                .await
                .unwrap();
            let ahp = host_support::connect(&host).await;
            let rejected = host_support::authenticate_resource(&ahp, RESOURCE).await;
            let gated = create(&ahp, &home_path).await;
            host.dispose().await.unwrap();
            ahp.client.shutdown().await;
            assert!(
                rejected.is_err(),
                "credential {expiry}/{identity} was accepted"
            );
            assert!(gated.is_err(), "rejected credential opened a session");
        }
    })
    .catch_unwind()
    .await;
    owner.stop().await.unwrap();
    if let Err(panic) = result {
        std::panic::resume_unwind(panic);
    }
}

async fn inference_endpoint() -> (
    ProviderConfig,
    mpsc::UnboundedReceiver<String>,
    CancellationToken,
    tokio::task::JoinHandle<()>,
) {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let address = listener.local_addr().unwrap();
    let stop = CancellationToken::new();
    let cancelled = stop.clone();
    let (seen, receiver) = mpsc::unbounded_channel();
    let task = tokio::spawn(async move {
        loop {
            let accepted = tokio::select! {
                biased;
                _ = cancelled.cancelled() => break,
                accepted = listener.accept() => accepted.unwrap(),
            };
            let (mut socket, _) = accepted;
            let request = tokio::time::timeout(Duration::from_secs(10), async {
                let mut bytes = Vec::new();
                let mut buffer = [0u8; 8192];
                loop {
                    let length = socket.read(&mut buffer).await.unwrap();
                    assert!(length > 0, "inference request ended before body");
                    bytes.extend_from_slice(&buffer[..length]);
                    if let Some(end) = bytes.windows(4).position(|part| part == b"\r\n\r\n") {
                        let headers = std::str::from_utf8(&bytes[..end]).unwrap();
                        let length: usize = headers
                            .lines()
                            .find_map(|line| {
                                line.to_ascii_lowercase()
                                    .strip_prefix("content-length: ")
                                    .map(str::to_owned)
                            })
                            .expect("inference Content-Length")
                            .parse()
                            .unwrap();
                        if bytes.len() >= end + 4 + length {
                            break;
                        }
                    }
                }
                String::from_utf8(bytes).unwrap()
            })
            .await
            .unwrap();
            seen.send(request).unwrap();
            let mut body = String::new();
            for choice in [
                json!({"delta":{"role":"assistant","content":"RUST_HOST_BYOK_OK"},"finish_reason":null}),
                json!({"delta":{},"finish_reason":"stop"}),
            ] {
                body.push_str(&format!("data: {}\n\n", json!({
                    "id":"rust-local","object":"chat.completion.chunk","created":1,
                    "model":"claude-sonnet-5","choices":[{"index":0,"delta":choice["delta"],"finish_reason":choice["finish_reason"]}]
                })));
            }
            body.push_str("data: [DONE]\n\n");
            socket.write_all(format!(
                "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}", body.len()
            ).as_bytes()).await.unwrap();
            socket.shutdown().await.unwrap();
        }
    });
    (
        ProviderConfig::new(format!("http://{address}/v1"))
            .with_provider_type("openai")
            .with_wire_api("completions")
            .with_api_key("inference-only-key"),
        receiver,
        stop,
        task,
    )
}

#[tokio::test]
async fn real_host_custom_and_anonymous_byok_create_and_cold_resume() {
    for anonymous in [false, true] {
        let (_fixture, home_path) = fixture_home();
        let (provider, mut requests, stop, endpoint) = inference_endpoint().await;
        let owner = Client::start(options(&home_path)).await.unwrap();
        let replacement = Client::start(options(&home_path)).await.unwrap();
        let expected = if anonymous {
            AhpSessionAuthentication::None
        } else {
            AhpSessionAuthentication::Custom {
                identity: "rust-owner".into(),
            }
        };
        let authentication = if anonymous {
            AhpHostAuthentication::None
        } else {
            custom("2099-01-01T00:00:00Z", "rust-owner")
        };
        let extension = Path::new(env!("CARGO_MANIFEST_DIR"))
            .canonicalize()
            .unwrap()
            .parent()
            .unwrap()
            .join("nodejs/dist")
            .to_str()
            .unwrap()
            .to_owned();
        let (created_tx, mut created) = mpsc::unbounded_channel();
        let (resumed_tx, mut resumed) = mpsc::unbounded_channel();
        let result = std::panic::AssertUnwindSafe(async {
            let create_provider = provider.clone();
            let create_expected = expected.clone();
            let create_extension = extension.clone();
            let host = owner
                .start_ahp_host(
                    AhpHostOptions::new()
                        .with_authentication(authentication.clone())
                        .with_local_server(Default::default())
                        .with_allow_creating_new_sessions(true)
                        .with_create_session(move |request: AhpSessionRequest, client: Client| {
                            assert!(request.authentication == create_expected);
                            assert!(request.config.github.token.is_none());
                            assert!(request.config.github.token_provider.is_none());
                            let config = request
                                .config
                                .clone()
                                .with_inference(|group| {
                                    byok::inference(group, create_provider.clone())
                                })
                                .with_github(|group| {
                                    group.with_capi(
                                        copilot_sdk::CapiSessionOptions::default()
                                            .with_enabled(false),
                                    )
                                })
                                .with_model("claude-sonnet-5")
                                .with_extensions(|group| {
                                    group.with_sdk_path(create_extension.clone())
                                })
                                .with_permissions(|group| {
                                    group.with_on_request(Arc::new(ApproveAllHandler))
                                });
                            let created_tx = created_tx.clone();
                            async move {
                                let session = Arc::new(client.create_session(config).await?);
                                created_tx.send(session.clone()).unwrap();
                                Ok(session)
                            }
                        }),
                )
                .await
                .unwrap();
            let ahp = host_support::connect(&host).await;
            if !anonymous {
                host_support::authenticate_resource(&ahp, RESOURCE)
                    .await
                    .unwrap();
            }
            let uri = create(&ahp, &home_path).await.unwrap();
            let original = tokio::time::timeout(Duration::from_secs(10), created.recv())
                .await
                .unwrap()
                .unwrap();
            let response = original.send_and_wait("Say hello").await.unwrap().unwrap();
            assert_eq!(response.data["content"], "RUST_HOST_BYOK_OK");
            let sent = requests.recv().await.unwrap();
            assert!(
                sent.to_ascii_lowercase()
                    .contains("authorization: bearer inference-only-key"),
                "{sent}"
            );
            assert!(
                !sent.contains(support::DEFAULT_TEST_TOKEN),
                "admission token leaked to inference"
            );
            host.dispose().await.unwrap();
            ahp.client.shutdown().await;
            original.disconnect().await.unwrap();
            owner.stop().await.unwrap();
            let resume_provider = provider.clone();
            let resume_expected = expected.clone();
            let resume_extension = extension.clone();
            let host = replacement
                .start_ahp_host(
                    AhpHostOptions::new()
                        .with_authentication(authentication)
                        .with_local_server(Default::default())
                        .with_resume_session(
                            move |request: AhpSessionResumeRequest, client: Client| {
                                assert!(request.authentication == resume_expected);
                                assert!(request.config.github.token.is_none());
                                assert!(request.config.github.token_provider.is_none());
                                let config = request
                                    .config
                                    .clone()
                                    .with_inference(|group| {
                                        byok::inference(group, resume_provider.clone())
                                    })
                                    .with_github(|group| {
                                        group.with_capi(
                                            copilot_sdk::CapiSessionOptions::default()
                                                .with_enabled(false),
                                        )
                                    })
                                    .with_model("claude-sonnet-5")
                                    .with_extensions(|group| {
                                        group.with_sdk_path(resume_extension.clone())
                                    })
                                    .with_permissions(|group| {
                                        group.with_on_request(Arc::new(ApproveAllHandler))
                                    });
                                let resumed_tx = resumed_tx.clone();
                                async move {
                                    let session = Arc::new(client.resume_session(config).await?);
                                    resumed_tx.send(session.clone()).unwrap();
                                    Ok(session)
                                }
                            },
                        ),
                )
                .await
                .unwrap();
            let ahp = host_support::connect(&host).await;
            if !anonymous {
                host_support::authenticate_resource(&ahp, RESOURCE)
                    .await
                    .unwrap();
            }
            ahp.client.subscribe(uri).await.unwrap();
            let original = tokio::time::timeout(Duration::from_secs(10), resumed.recv())
                .await
                .unwrap()
                .unwrap();
            let response = original
                .send_and_wait("Say hello again")
                .await
                .unwrap()
                .unwrap();
            assert_eq!(response.data["content"], "RUST_HOST_BYOK_OK");
            let sent = requests.recv().await.unwrap();
            assert!(
                !sent.contains(support::DEFAULT_TEST_TOKEN),
                "resumed inference used admission credential"
            );
            host.dispose().await.unwrap();
            ahp.client.shutdown().await;
            original.disconnect().await.unwrap();
        })
        .catch_unwind()
        .await;
        owner.stop().await.unwrap();
        replacement.stop().await.unwrap();
        stop.cancel();
        endpoint.await.unwrap();
        if let Err(panic) = result {
            std::panic::resume_unwind(panic);
        }
    }
}
