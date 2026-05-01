// Copyright (c) Microsoft Corporation. All rights reserved.

//! E2E session tests for the Rust Copilot SDK.
//!
//! These tests use the replaying CAPI proxy to exercise the SDK without
//! hitting real AI endpoints.

use copilot_sdk_supercharged::*;
use std::collections::HashMap;

use super::test_harness::TestHarness;

/// Helper: build a fresh harness, start it, configure for the given snapshot,
/// and return (harness, cli_path, env, work_dir, home_dir).
fn setup_harness(snapshot_name: &str) -> (TestHarness, String, HashMap<String, String>, tempfile::TempDir, tempfile::TempDir) {
    let mut harness = TestHarness::new();
    harness.start();

    let work_dir = tempfile::tempdir().expect("failed to create work dir");
    let home_dir = tempfile::tempdir().expect("failed to create home dir");

    let snapshot_path = harness
        .snapshots_dir()
        .join("session")
        .join(format!("{snapshot_name}.yaml"));

    harness.configure(
        &snapshot_path.to_string_lossy(),
        &work_dir.path().to_string_lossy(),
    );

    let cli_path = harness.cli_path();
    let env = harness.test_env(
        &work_dir.path().to_string_lossy(),
        &home_dir.path().to_string_lossy(),
    );

    (harness, cli_path, env, work_dir, home_dir)
}

/// Verifies that a session can be created and cleanly destroyed.
#[tokio::test]
async fn test_session_create_disconnect() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("session_create_disconnect");

    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some(cli_path),
        cwd: Some(work_dir.path().to_string_lossy().into_owned()),
        env: Some(env),
        ..Default::default()
    });

    client.start().await.expect("client should start");

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("session should be created");

    let session_id = session.session_id().to_string();
    assert!(!session_id.is_empty(), "session ID must not be empty");

    session.destroy().await.expect("session should be destroyed");
    let _ = client.stop().await;
}

/// Verifies that a message can be sent and an assistant response received.
#[tokio::test]
async fn test_send_message() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");

    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some(cli_path),
        cwd: Some(work_dir.path().to_string_lossy().into_owned()),
        env: Some(env),
        ..Default::default()
    });

    client.start().await.expect("client should start");

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("session should be created");

    let response = session
        .send_and_wait(
            MessageOptions {
                prompt: "What is 1+1?".into(),
                attachments: None,
                mode: None,
                response_format: None,
                image_options: None,
                request_headers: None,
            },
            None,
        )
        .await
        .expect("send_and_wait should succeed");

    let event = response.expect("should receive an assistant response");
    assert!(
        event.is_assistant_message(),
        "response should be an assistant.message event"
    );
    let content = event
        .assistant_message_content()
        .expect("response should have content");
    assert!(
        content.contains('2'),
        "response should contain '2', got: {content}"
    );

    session.destroy().await.expect("session should be destroyed");
    let _ = client.stop().await;
}

/// Verifies that a client can be created with sessionFs configuration.
#[tokio::test]
async fn test_session_fs_config() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("session_fs_config");

    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some(cli_path),
        cwd: Some(work_dir.path().to_string_lossy().into_owned()),
        env: Some(env),
        session_fs: Some(SessionFsConfig {
            initial_cwd: "/".into(),
            session_state_path: "/session-state".into(),
            conventions: "posix".into(),
        }),
        ..Default::default()
    });

    client.start().await.expect("client should start");

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("session with sessionFs config should be created");

    let session_id = session.session_id().to_string();
    assert!(!session_id.is_empty(), "session ID must not be empty");

    session.destroy().await.expect("session should be destroyed");
    let _ = client.stop().await;
}
