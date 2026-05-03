// Copyright (c) Microsoft Corporation. All rights reserved.

//! E2E session tests for the Rust Copilot SDK.
//!
//! These tests use the replaying CAPI proxy to exercise the SDK without
//! hitting real AI endpoints.

use copilot_sdk_supercharged::*;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

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

// ---------------------------------------------------------------------------
// Helper: create a started client from harness parts
// ---------------------------------------------------------------------------
async fn make_client(
    cli_path: String,
    env: HashMap<String, String>,
    work_dir: &tempfile::TempDir,
) -> CopilotClient {
    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some(cli_path),
        cwd: Some(work_dir.path().to_string_lossy().into_owned()),
        env: Some(env),
        ..Default::default()
    });
    client.start().await.expect("client should start");
    client
}

// ===== Test 4 =====
/// Multi-turn conversation: two sequential send_and_wait calls.
#[tokio::test]
async fn test_multi_turn_conversation() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("session should be created");

    let msg = |prompt: &str| MessageOptions {
        prompt: prompt.into(),
        attachments: None,
        mode: None,
        response_format: None,
        image_options: None,
        request_headers: None,
    };

    let r1 = session
        .send_and_wait(msg("What is 2+2?"), None)
        .await
        .expect("first send should succeed")
        .expect("should get first response");
    assert!(r1.is_assistant_message(), "first response should be assistant message");

    let r2 = session
        .send_and_wait(msg("What about 3+3?"), None)
        .await
        .expect("second send should succeed")
        .expect("should get second response");
    assert!(r2.is_assistant_message(), "second response should be assistant message");

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 5 =====
/// Resume a session after destroying and restarting the client.
#[tokio::test]
async fn test_session_resume() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path.clone(), env.clone(), &work_dir).await;

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("session should be created");
    let sid = session.session_id().to_string();
    session.destroy().await.expect("destroy first session");
    let _ = client.stop().await;

    // Second client, resume session
    let client2 = make_client(cli_path, env, &work_dir).await;
    let resumed = client2
        .resume_session(ResumeSessionConfig {
            session_id: sid.clone(),
            ..Default::default()
        })
        .await
        .expect("resume_session should succeed");
    assert_eq!(resumed.session_id(), sid);

    resumed.destroy().await.expect("destroy resumed session");
    let _ = client2.stop().await;
}

// ===== Test 6 =====
/// List sessions after creating two.
#[tokio::test]
async fn test_session_list() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let s1 = client.create_session(SessionConfig::default()).await.expect("s1");
    let s2 = client.create_session(SessionConfig::default()).await.expect("s2");

    let sessions = client.list_sessions().await.expect("list_sessions should succeed");
    assert!(sessions.len() >= 2, "should list at least 2 sessions, got {}", sessions.len());

    s1.destroy().await.expect("destroy s1");
    s2.destroy().await.expect("destroy s2");
    let _ = client.stop().await;
}

// ===== Test 7 =====
/// Get session metadata for an existing session.
#[tokio::test]
async fn test_session_metadata() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let session = client.create_session(SessionConfig::default()).await.expect("create");
    let sid = session.session_id().to_string();

    let meta = client.get_session_metadata(&sid).await.expect("get_session_metadata should succeed");
    assert!(meta.is_object() || meta.is_null(), "metadata should be a JSON value");

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 8 =====
/// Delete a session, then verify it no longer appears in list.
#[tokio::test]
async fn test_session_delete() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let session = client.create_session(SessionConfig::default()).await.expect("create");
    let sid = session.session_id().to_string();
    session.destroy().await.expect("destroy");

    client.delete_session(&sid).await.expect("delete_session should succeed");

    let sessions = client.list_sessions().await.expect("list_sessions");
    let found = sessions.iter().any(|s| {
        serde_json::to_value(s)
            .ok()
            .and_then(|v| v.get("sessionId").and_then(|id| id.as_str().map(|s| s == sid)))
            .unwrap_or(false)
    });
    assert!(!found, "deleted session should not appear in list");

    let _ = client.stop().await;
}

// ===== Test 9 =====
/// List models.
#[tokio::test]
async fn test_model_list() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let _models = client.list_models().await.expect("list_models should succeed");

    let _ = client.stop().await;
}

// ===== Test 10 =====
/// Ping the server.
#[tokio::test]
async fn test_ping() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let _resp = client.ping(Some("hello")).await.expect("ping should succeed");

    let _ = client.stop().await;
}

// ===== Test 11 =====
/// Get auth status.
#[tokio::test]
async fn test_auth_status() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let _auth = client.get_auth_status().await.expect("get_auth_status should succeed");

    let _ = client.stop().await;
}

// ===== Test 12 =====
/// Client lifecycle: state transitions across start / stop.
#[tokio::test]
async fn test_client_lifecycle() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");

    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some(cli_path),
        cwd: Some(work_dir.path().to_string_lossy().into_owned()),
        env: Some(env),
        ..Default::default()
    });

    let state_before = client.get_state().await;
    assert_eq!(state_before, ConnectionState::Disconnected, "should start disconnected");

    client.start().await.expect("start");
    let state_after_start = client.get_state().await;
    assert_eq!(state_after_start, ConnectionState::Connected, "should be connected after start");

    let _ = client.stop().await;
    let state_after_stop = client.get_state().await;
    assert_eq!(state_after_stop, ConnectionState::Disconnected, "should be disconnected after stop");
}

// ===== Test 13 =====
/// Foreground session: set and get.
#[tokio::test]
async fn test_foreground_session() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let session = client.create_session(SessionConfig::default()).await.expect("create");
    let sid = session.session_id().to_string();

    client
        .set_foreground_session_id(&sid)
        .await
        .expect("set_foreground_session_id should succeed");

    let fg = client
        .get_foreground_session_id()
        .await
        .expect("get_foreground_session_id should succeed");
    assert_eq!(fg, Some(sid), "foreground session ID should match");

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 14 =====
/// Register tools via SessionConfig, send a message, verify response.
#[tokio::test]
async fn test_tools() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let tool_def = ToolDefinition {
        name: "add_numbers".into(),
        description: Some("Adds two numbers".into()),
        parameters: Some(serde_json::json!({
            "type": "object",
            "properties": {
                "a": { "type": "number" },
                "b": { "type": "number" }
            },
            "required": ["a", "b"]
        })),
    };

    let config = SessionConfig {
        tools: Some(vec![tool_def]),
        ..Default::default()
    };

    let session = client.create_session(config).await.expect("create with tools");

    session.register_tool("add_numbers", Arc::new(|_params, _invocation| {
        Box::pin(async move {
            Ok(serde_json::json!({ "result": 42 }))
        }) as std::pin::Pin<Box<dyn std::future::Future<Output = Result<serde_json::Value, CopilotError>> + Send>>
    })).await;

    let response = session
        .send_and_wait(
            MessageOptions {
                prompt: "Use the add_numbers tool to add 20 and 22.".into(),
                attachments: None,
                mode: None,
                response_format: None,
                image_options: None,
                request_headers: None,
            },
            None,
        )
        .await
        .expect("send_and_wait with tools should succeed");

    assert!(response.is_some(), "should get a response");

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 15 =====
/// Streaming: collect events via on() handler.
#[tokio::test]
async fn test_streaming() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let config = SessionConfig {
        streaming: Some(true),
        ..Default::default()
    };
    let session = client.create_session(config).await.expect("create streaming session");

    let events: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let events_clone = events.clone();

    let _sub = session.on(move |event: SessionEvent| {
        let events_inner = events_clone.clone();
        // on() expects a synchronous handler; use blocking_lock
        events_inner.blocking_lock().push(event.event_type.clone());
    }).await;

    let response = session
        .send_and_wait(
            MessageOptions {
                prompt: "Hello, streaming test!".into(),
                attachments: None,
                mode: None,
                response_format: None,
                image_options: None,
                request_headers: None,
            },
            None,
        )
        .await
        .expect("streaming send_and_wait should succeed");

    assert!(response.is_some(), "should get a response in streaming mode");

    tokio::time::sleep(std::time::Duration::from_millis(200)).await;

    let collected = events.lock().await;
    assert!(!collected.is_empty(), "should have collected at least one event");

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 16 =====
/// System message customization via SessionConfig.
#[tokio::test]
async fn test_system_message_customization() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let config = SessionConfig {
        system_message: Some(SystemMessageConfig::Append {
            content: Some("You are a helpful test assistant.".into()),
        }),
        ..Default::default()
    };

    let session = client
        .create_session(config)
        .await
        .expect("create session with system message should succeed");

    assert!(!session.session_id().is_empty());

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 17 =====
/// Session filesystem provider via client options.
#[tokio::test]
async fn test_session_fs_provider() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");

    let client = CopilotClient::new(CopilotClientOptions {
        cli_path: Some(cli_path),
        cwd: Some(work_dir.path().to_string_lossy().into_owned()),
        env: Some(env),
        session_fs: Some(SessionFsConfig {
            initial_cwd: "/test".into(),
            session_state_path: "/test-state".into(),
            conventions: "posix".into(),
        }),
        ..Default::default()
    });

    client.start().await.expect("client with sessionFs should start");

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("session with fs provider should be created");
    assert!(!session.session_id().is_empty());

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 18 =====
/// MCP servers config in SessionConfig.
#[tokio::test]
async fn test_mcp_servers_config() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let mut mcp_servers = HashMap::new();
    mcp_servers.insert(
        "test-server".to_string(),
        McpServerConfig::Local(McpLocalServerConfig {
            tools: vec!["*".into()],
            server_type: None,
            timeout: Some(30),
            command: "echo".into(),
            args: vec!["hello".into()],
            env: None,
            cwd: None,
        }),
    );

    let config = SessionConfig {
        mcp_servers: Some(mcp_servers),
        ..Default::default()
    };

    let session = client
        .create_session(config)
        .await
        .expect("create session with MCP servers should succeed");
    assert!(!session.session_id().is_empty());

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 19 =====
/// Skills config (skill_directories / disabled_skills) in SessionConfig.
#[tokio::test]
async fn test_skills_config() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let config = SessionConfig {
        skill_directories: Some(vec!["/fake/skills/dir".into()]),
        disabled_skills: Some(vec!["some-skill".into()]),
        ..Default::default()
    };

    let session = client
        .create_session(config)
        .await
        .expect("create session with skills config should succeed");
    assert!(!session.session_id().is_empty());

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}

// ===== Test 20 =====
/// Compaction: send multiple messages and verify all get responses.
#[tokio::test]
async fn test_compaction() {
    let (_harness, cli_path, env, work_dir, _home_dir) = setup_harness("send_message");
    let client = make_client(cli_path, env, &work_dir).await;

    let session = client
        .create_session(SessionConfig::default())
        .await
        .expect("create session");

    let prompts = ["First question: What is 1+1?", "Second: What is 2+2?", "Third: What is 3+3?"];
    for prompt in &prompts {
        let response = session
            .send_and_wait(
                MessageOptions {
                    prompt: prompt.to_string(),
                    attachments: None,
                    mode: None,
                    response_format: None,
                    image_options: None,
                    request_headers: None,
                },
                None,
            )
            .await
            .expect(&format!("send_and_wait for '{}' should succeed", prompt));

        let event = response.expect(&format!("should get response for '{}'", prompt));
        assert!(
            event.is_assistant_message(),
            "response for '{}' should be an assistant message",
            prompt
        );
    }

    session.destroy().await.expect("destroy");
    let _ = client.stop().await;
}
