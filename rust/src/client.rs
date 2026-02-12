// Copyright (c) Microsoft Corporation. All rights reserved.

//! CopilotClient - Main entry point for the Copilot SDK.
//!
//! This module provides the [`CopilotClient`] struct, which manages the connection
//! to the Copilot CLI server and provides session management capabilities.

use std::collections::HashMap;
use std::sync::Arc;

use serde_json::Value;
use tokio::net::TcpStream;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

use crate::jsonrpc::{JsonRpcClient, NotificationHandler, RequestHandler};
use crate::sdk_protocol_version::get_sdk_protocol_version;
use crate::session::CopilotSession;
use crate::types::*;
use crate::CopilotError;

// ============================================================================
// CopilotClient
// ============================================================================

/// Main client for interacting with the Copilot CLI.
///
/// The CopilotClient manages the connection to the Copilot CLI server and provides
/// methods to create and manage conversation sessions. It can either spawn a CLI
/// server process or connect to an existing server.
///
/// # Examples
///
/// ```rust,no_run
/// # use copilot_sdk::*;
/// # async fn example() -> Result<(), CopilotError> {
/// // Create a client with default options (spawns CLI server via stdio)
/// let client = CopilotClient::new(CopilotClientOptions::default());
///
/// // Start the connection
/// client.start().await?;
///
/// // Create a session
/// let session = client.create_session(SessionConfig::default()).await?;
///
/// // Send messages and handle responses
/// let sub = session.on(|event| {
///     if let Some(content) = event.assistant_message_content() {
///         println!("Assistant: {}", content);
///     }
/// }).await;
///
/// session.send(MessageOptions {
///     prompt: "Hello!".to_string(),
///     attachments: None,
///     mode: None,
/// }).await?;
///
/// // Clean up
/// session.destroy().await?;
/// client.stop().await?;
/// # Ok(())
/// # }
/// ```
pub struct CopilotClient {
    options: CopilotClientOptions,
    state: Arc<Mutex<ConnectionState>>,
    rpc_client: Arc<Mutex<Option<Arc<JsonRpcClient>>>>,
    cli_process: Arc<Mutex<Option<Child>>>,
    sessions: Arc<Mutex<HashMap<String, Arc<CopilotSession>>>>,
    is_external_server: bool,
    models_cache: Arc<Mutex<Option<Vec<ModelInfo>>>>,
    lifecycle_handlers: Arc<Mutex<Vec<(u64, Arc<dyn Fn(SessionLifecycleEvent) + Send + Sync>)>>>,
    next_lifecycle_handler_id: Arc<Mutex<u64>>,
}

impl CopilotClient {
    /// Creates a new CopilotClient with the given options.
    ///
    /// This does not start the connection. Call `start()` or use `auto_start`
    /// (which triggers on first `create_session()`).
    pub fn new(options: CopilotClientOptions) -> Self {
        let is_external = options.cli_url.is_some();

        Self {
            options,
            state: Arc::new(Mutex::new(ConnectionState::Disconnected)),
            rpc_client: Arc::new(Mutex::new(None)),
            cli_process: Arc::new(Mutex::new(None)),
            sessions: Arc::new(Mutex::new(HashMap::new())),
            is_external_server: is_external,
            models_cache: Arc::new(Mutex::new(None)),
            lifecycle_handlers: Arc::new(Mutex::new(Vec::new())),
            next_lifecycle_handler_id: Arc::new(Mutex::new(0)),
        }
    }

    /// Returns the current connection state.
    pub async fn get_state(&self) -> ConnectionState {
        *self.state.lock().await
    }

    // ========================================================================
    // Start / Stop
    // ========================================================================

    /// Starts the CLI server and establishes a connection.
    ///
    /// If connecting to an external server (via `cli_url`), only establishes the connection.
    /// Otherwise, spawns the CLI server process and then connects.
    ///
    /// This method is called automatically when creating a session if `auto_start` is true.
    pub async fn start(&self) -> Result<(), CopilotError> {
        {
            let state = self.state.lock().await;
            if *state == ConnectionState::Connected {
                return Ok(());
            }
        }

        {
            let mut state = self.state.lock().await;
            *state = ConnectionState::Connecting;
        }

        let result = self.do_start().await;
        match &result {
            Ok(()) => {
                let mut state = self.state.lock().await;
                *state = ConnectionState::Connected;
            }
            Err(_) => {
                let mut state = self.state.lock().await;
                *state = ConnectionState::Error;
            }
        }
        result
    }

    async fn do_start(&self) -> Result<(), CopilotError> {
        if self.is_external_server {
            // Connect to an existing server via TCP
            self.connect_to_external_server().await?;
        } else if self.options.use_stdio {
            // Spawn CLI and connect via stdio
            self.start_cli_stdio().await?;
        } else {
            // Spawn CLI and connect via TCP
            self.start_cli_tcp().await?;
        }

        // Verify protocol version
        self.verify_protocol_version().await?;

        Ok(())
    }

    /// Stops the CLI server and closes all active sessions.
    ///
    /// Returns a list of errors encountered during cleanup.
    pub async fn stop(&self) -> Result<Vec<CopilotError>, CopilotError> {
        let mut errors = Vec::new();

        // Destroy all active sessions
        let session_ids: Vec<String> = {
            let sessions = self.sessions.lock().await;
            sessions.keys().cloned().collect()
        };

        for session_id in session_ids {
            let session = {
                let sessions = self.sessions.lock().await;
                sessions.get(&session_id).cloned()
            };
            if let Some(session) = session {
                for attempt in 1..=3 {
                    match session.destroy().await {
                        Ok(()) => break,
                        Err(e) => {
                            if attempt == 3 {
                                errors.push(CopilotError::SessionError(format!(
                                    "Failed to destroy session {} after 3 attempts: {}",
                                    session_id, e
                                )));
                            } else {
                                let delay = 100 * (1u64 << (attempt - 1));
                                tokio::time::sleep(std::time::Duration::from_millis(delay)).await;
                            }
                        }
                    }
                }
            }
        }

        {
            let mut sessions = self.sessions.lock().await;
            sessions.clear();
        }

        // Stop the JSON-RPC client
        {
            let mut rpc = self.rpc_client.lock().await;
            if let Some(client) = rpc.take() {
                // We need mutable access; Arc doesn't give us that directly.
                // The client will be cleaned up when all references are dropped.
                // For a cleaner shutdown, we'd need Arc<Mutex<JsonRpcClient>>.
                drop(client);
            }
        }

        // Clear models cache
        {
            let mut cache = self.models_cache.lock().await;
            *cache = None;
        }

        // Kill CLI process
        if !self.is_external_server {
            let mut proc = self.cli_process.lock().await;
            if let Some(ref mut child) = *proc {
                let _ = child.kill().await;
            }
            *proc = None;
        }

        {
            let mut state = self.state.lock().await;
            *state = ConnectionState::Disconnected;
        }

        Ok(errors)
    }

    /// Forcefully stops the client without graceful session cleanup.
    pub async fn force_stop(&self) {
        // Clear sessions immediately
        {
            let mut sessions = self.sessions.lock().await;
            sessions.clear();
        }

        // Drop RPC client
        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = None;
        }

        // Clear cache
        {
            let mut cache = self.models_cache.lock().await;
            *cache = None;
        }

        // Kill process
        if !self.is_external_server {
            let mut proc = self.cli_process.lock().await;
            if let Some(ref mut child) = *proc {
                let _ = child.kill().await;
            }
            *proc = None;
        }

        {
            let mut state = self.state.lock().await;
            *state = ConnectionState::Disconnected;
        }
    }

    // ========================================================================
    // Session Management
    // ========================================================================

    /// Creates a new conversation session with the Copilot CLI.
    pub async fn create_session(
        &self,
        config: SessionConfig,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        self.ensure_connected().await?;

        let rpc = self.get_rpc_client().await?;

        let params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;

        let response = rpc.request("session.create", params, None).await?;
        let session_id = response
            .get("sessionId")
            .and_then(|v| v.as_str())
            .ok_or_else(|| CopilotError::Protocol("Missing sessionId in response".to_string()))?
            .to_string();
        let workspace_path = response
            .get("workspacePath")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        let session = Arc::new(CopilotSession::new(
            session_id.clone(),
            rpc.clone(),
            workspace_path,
        ));

        {
            let mut sessions = self.sessions.lock().await;
            sessions.insert(session_id, Arc::clone(&session));
        }

        Ok(session)
    }

    /// Resumes an existing conversation session by its ID.
    pub async fn resume_session(
        &self,
        config: ResumeSessionConfig,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        self.ensure_connected().await?;

        let rpc = self.get_rpc_client().await?;

        let params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;

        let response = rpc.request("session.resume", params, None).await?;
        let session_id = response
            .get("sessionId")
            .and_then(|v| v.as_str())
            .ok_or_else(|| CopilotError::Protocol("Missing sessionId in response".to_string()))?
            .to_string();
        let workspace_path = response
            .get("workspacePath")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        let session = Arc::new(CopilotSession::new(
            session_id.clone(),
            rpc.clone(),
            workspace_path,
        ));

        {
            let mut sessions = self.sessions.lock().await;
            sessions.insert(session_id, Arc::clone(&session));
        }

        Ok(session)
    }

    /// Gets the last session ID.
    pub async fn get_last_session_id(&self) -> Result<Option<String>, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("session.getLastId", serde_json::json!({}), None)
            .await?;
        Ok(response
            .get("sessionId")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
    }

    /// Deletes a session permanently.
    pub async fn delete_session(&self, session_id: &str) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request(
                "session.delete",
                serde_json::json!({ "sessionId": session_id }),
                None,
            )
            .await?;

        let success = response.get("success").and_then(|v| v.as_bool()).unwrap_or(false);
        if !success {
            let error = response
                .get("error")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown error");
            return Err(CopilotError::SessionError(format!(
                "Failed to delete session {}: {}",
                session_id, error
            )));
        }

        {
            let mut sessions = self.sessions.lock().await;
            sessions.remove(session_id);
        }

        Ok(())
    }

    /// Lists all available sessions.
    pub async fn list_sessions(&self) -> Result<Vec<SessionMetadata>, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("session.list", serde_json::json!({}), None)
            .await?;
        let sessions: Vec<SessionMetadata> = serde_json::from_value(
            response
                .get("sessions")
                .cloned()
                .unwrap_or(Value::Array(vec![])),
        )
        .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        Ok(sessions)
    }

    // ========================================================================
    // Ping / Status / Models
    // ========================================================================

    /// Sends a ping request to the server.
    pub async fn ping(&self, message: Option<&str>) -> Result<PingResponse, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::json!({ "message": message });
        let response = rpc.request("ping", params, None).await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Gets CLI status including version and protocol information.
    pub async fn get_status(&self) -> Result<GetStatusResponse, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("status.get", serde_json::json!({}), None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Gets current authentication status.
    pub async fn get_auth_status(&self) -> Result<GetAuthStatusResponse, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("auth.getStatus", serde_json::json!({}), None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Lists available models with their metadata.
    ///
    /// Results are cached after the first successful call.
    pub async fn list_models(&self) -> Result<Vec<ModelInfo>, CopilotError> {
        // Check cache first
        {
            let cache = self.models_cache.lock().await;
            if let Some(ref models) = *cache {
                return Ok(models.clone());
            }
        }

        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("models.list", serde_json::json!({}), None)
            .await?;
        let models_response: HashMap<String, Vec<ModelInfo>> =
            serde_json::from_value(response)
                .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        let models = models_response.get("models").cloned().unwrap_or_default();

        // Update cache
        {
            let mut cache = self.models_cache.lock().await;
            *cache = Some(models.clone());
        }

        Ok(models)
    }

    // ========================================================================
    // Foreground Session (TUI+server mode)
    // ========================================================================

    /// Gets the foreground session ID in TUI+server mode.
    pub async fn get_foreground_session_id(&self) -> Result<Option<String>, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("session.getForeground", serde_json::json!({}), None)
            .await?;
        Ok(response
            .get("sessionId")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
    }

    /// Sets the foreground session in TUI+server mode.
    pub async fn set_foreground_session_id(
        &self,
        session_id: &str,
    ) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request(
                "session.setForeground",
                serde_json::json!({ "sessionId": session_id }),
                None,
            )
            .await?;
        let success = response.get("success").and_then(|v| v.as_bool()).unwrap_or(false);
        if !success {
            let error = response
                .get("error")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown error");
            return Err(CopilotError::SessionError(error.to_string()));
        }
        Ok(())
    }

    // ========================================================================
    // Lifecycle Event Subscription
    // ========================================================================

    /// Subscribes to session lifecycle events.
    ///
    /// Returns a handler ID that can be used to unsubscribe.
    pub async fn on_lifecycle<F>(&self, handler: F) -> u64
    where
        F: Fn(SessionLifecycleEvent) + Send + Sync + 'static,
    {
        let handler_id = {
            let mut id = self.next_lifecycle_handler_id.lock().await;
            *id += 1;
            *id
        };

        let mut handlers = self.lifecycle_handlers.lock().await;
        handlers.push((handler_id, Arc::new(handler)));
        handler_id
    }

    /// Unsubscribes a lifecycle event handler by its ID.
    pub async fn off_lifecycle(&self, handler_id: u64) {
        let mut handlers = self.lifecycle_handlers.lock().await;
        handlers.retain(|(id, _)| *id != handler_id);
    }

    // ========================================================================
    // Internal: Connection Setup
    // ========================================================================

    async fn ensure_connected(&self) -> Result<(), CopilotError> {
        let state = self.state.lock().await;
        if *state == ConnectionState::Connected {
            return Ok(());
        }
        drop(state);

        if self.options.auto_start {
            self.start().await
        } else {
            Err(CopilotError::NotConnected)
        }
    }

    async fn get_rpc_client(&self) -> Result<Arc<JsonRpcClient>, CopilotError> {
        let rpc = self.rpc_client.lock().await;
        rpc.clone().ok_or(CopilotError::NotConnected)
    }

    async fn start_cli_stdio(&self) -> Result<(), CopilotError> {
        let cli_path = self
            .options
            .cli_path
            .as_deref()
            .ok_or_else(|| CopilotError::Configuration("cli_path is required".to_string()))?;

        let mut args = self.options.cli_args.clone();
        args.extend_from_slice(&[
            "--headless".to_string(),
            "--no-auto-update".to_string(),
            "--log-level".to_string(),
            self.options.log_level.clone(),
            "--stdio".to_string(),
        ]);

        // Add auth flags
        if self.options.github_token.is_some() {
            args.push("--auth-token-env".to_string());
            args.push("COPILOT_SDK_AUTH_TOKEN".to_string());
        }
        let use_logged_in = self
            .options
            .use_logged_in_user
            .unwrap_or(self.options.github_token.is_none());
        if !use_logged_in {
            args.push("--no-auto-login".to_string());
        }

        let mut cmd = Command::new(cli_path);
        cmd.args(&args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        if let Some(ref cwd) = self.options.cwd {
            cmd.current_dir(cwd);
        }

        // Set environment
        if let Some(ref env) = self.options.env {
            cmd.envs(env.iter());
        }
        if let Some(ref token) = self.options.github_token {
            cmd.env("COPILOT_SDK_AUTH_TOKEN", token);
        }

        let mut child = cmd.spawn().map_err(|e| {
            CopilotError::ProcessSpawn(format!("Failed to spawn CLI process: {}", e))
        })?;

        let stdin = child.stdin.take().ok_or_else(|| {
            CopilotError::ProcessSpawn("Failed to capture stdin".to_string())
        })?;
        let stdout = child.stdout.take().ok_or_else(|| {
            CopilotError::ProcessSpawn("Failed to capture stdout".to_string())
        })?;

        // Forward stderr in background
        let stderr = child.stderr.take();
        if let Some(stderr) = stderr {
            tokio::spawn(async move {
                use tokio::io::AsyncBufReadExt;
                let reader = tokio::io::BufReader::new(stderr);
                let mut lines = reader.lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    if !line.trim().is_empty() {
                        eprintln!("[CLI subprocess] {}", line);
                    }
                }
            });
        }

        let rpc_client = Arc::new(JsonRpcClient::new(stdout, stdin));

        // Set up handlers
        self.attach_connection_handlers(&rpc_client).await;

        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = Some(rpc_client);
        }
        {
            let mut proc = self.cli_process.lock().await;
            *proc = Some(child);
        }

        Ok(())
    }

    async fn start_cli_tcp(&self) -> Result<(), CopilotError> {
        let cli_path = self
            .options
            .cli_path
            .as_deref()
            .ok_or_else(|| CopilotError::Configuration("cli_path is required".to_string()))?;

        let mut args = self.options.cli_args.clone();
        args.extend_from_slice(&[
            "--headless".to_string(),
            "--no-auto-update".to_string(),
            "--log-level".to_string(),
            self.options.log_level.clone(),
        ]);

        if self.options.port > 0 {
            args.push("--port".to_string());
            args.push(self.options.port.to_string());
        }

        let mut cmd = Command::new(cli_path);
        cmd.args(&args)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        if let Some(ref cwd) = self.options.cwd {
            cmd.current_dir(cwd);
        }
        if let Some(ref env) = self.options.env {
            cmd.envs(env.iter());
        }

        let mut child = cmd.spawn().map_err(|e| {
            CopilotError::ProcessSpawn(format!("Failed to spawn CLI process: {}", e))
        })?;

        // Read stdout to find the port
        let stdout = child.stdout.take().ok_or_else(|| {
            CopilotError::ProcessSpawn("Failed to capture stdout".to_string())
        })?;

        let port = {
            use tokio::io::AsyncBufReadExt;
            let reader = tokio::io::BufReader::new(stdout);
            let mut lines = reader.lines();
            let mut found_port = None;

            let timeout = tokio::time::timeout(std::time::Duration::from_secs(10), async {
                while let Ok(Some(line)) = lines.next_line().await {
                    if let Some(idx) = line.to_lowercase().find("listening on port ") {
                        let port_str = &line[idx + "listening on port ".len()..];
                        if let Ok(p) = port_str.trim().parse::<u16>() {
                            found_port = Some(p);
                            break;
                        }
                    }
                }
                found_port
            })
            .await;

            match timeout {
                Ok(Some(p)) => p,
                _ => {
                    let _ = child.kill().await;
                    return Err(CopilotError::Timeout(10000));
                }
            }
        };

        // Connect TCP
        let stream = TcpStream::connect(format!("localhost:{}", port))
            .await
            .map_err(|e| CopilotError::Connection(format!("Failed to connect via TCP: {}", e)))?;

        let (reader, writer) = stream.into_split();
        let rpc_client = Arc::new(JsonRpcClient::new(reader, writer));

        self.attach_connection_handlers(&rpc_client).await;

        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = Some(rpc_client);
        }
        {
            let mut proc = self.cli_process.lock().await;
            *proc = Some(child);
        }

        Ok(())
    }

    async fn connect_to_external_server(&self) -> Result<(), CopilotError> {
        let url = self
            .options
            .cli_url
            .as_deref()
            .ok_or_else(|| CopilotError::Configuration("cli_url is required".to_string()))?;

        let (host, port) = Self::parse_cli_url(url)?;

        let stream = TcpStream::connect(format!("{}:{}", host, port))
            .await
            .map_err(|e| {
                CopilotError::Connection(format!("Failed to connect to {}: {}", url, e))
            })?;

        let (reader, writer) = stream.into_split();
        let rpc_client = Arc::new(JsonRpcClient::new(reader, writer));

        self.attach_connection_handlers(&rpc_client).await;

        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = Some(rpc_client);
        }

        Ok(())
    }

    fn parse_cli_url(url: &str) -> Result<(String, u16), CopilotError> {
        let clean = url
            .trim_start_matches("http://")
            .trim_start_matches("https://");

        // Just a port number
        if let Ok(port) = clean.parse::<u16>() {
            return Ok(("localhost".to_string(), port));
        }

        // host:port format
        let parts: Vec<&str> = clean.split(':').collect();
        if parts.len() != 2 {
            return Err(CopilotError::Configuration(format!(
                "Invalid cli_url format: {}. Expected host:port, http://host:port, or port",
                url
            )));
        }

        let host = if parts[0].is_empty() {
            "localhost".to_string()
        } else {
            parts[0].to_string()
        };
        let port: u16 = parts[1].parse().map_err(|_| {
            CopilotError::Configuration(format!("Invalid port in cli_url: {}", url))
        })?;

        Ok((host, port))
    }

    async fn verify_protocol_version(&self) -> Result<(), CopilotError> {
        let expected_version = get_sdk_protocol_version();
        let ping_response = self.ping(None).await?;

        match ping_response.protocol_version {
            None => Err(CopilotError::ProtocolMismatch {
                expected: expected_version,
                actual: None,
            }),
            Some(server_version) if server_version != expected_version => {
                Err(CopilotError::ProtocolMismatch {
                    expected: expected_version,
                    actual: Some(server_version),
                })
            }
            _ => Ok(()),
        }
    }

    // ========================================================================
    // Internal: Connection Handlers
    // ========================================================================

    async fn attach_connection_handlers(&self, rpc_client: &Arc<JsonRpcClient>) {
        // Set up notification handler for session.event and session.lifecycle
        let sessions = Arc::clone(&self.sessions);
        let lifecycle_handlers = Arc::clone(&self.lifecycle_handlers);
        let notification_handler: NotificationHandler =
            Arc::new(move |method: String, params: Value| {
                let sessions = Arc::clone(&sessions);
                let lifecycle_handlers = Arc::clone(&lifecycle_handlers);

                match method.as_str() {
                    "session.event" => {
                        let session_id = params
                            .get("sessionId")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string());
                        let event = params.get("event").cloned();

                        if let (Some(session_id), Some(event_value)) = (session_id, event) {
                            if let Ok(event) =
                                serde_json::from_value::<SessionEvent>(event_value)
                            {
                                tokio::spawn(async move {
                                    let sessions = sessions.lock().await;
                                    if let Some(session) = sessions.get(&session_id) {
                                        session.dispatch_event(event).await;
                                    }
                                });
                            }
                        }
                    }
                    "session.lifecycle" => {
                        if let Ok(event) =
                            serde_json::from_value::<SessionLifecycleEvent>(params)
                        {
                            tokio::spawn(async move {
                                let handlers = lifecycle_handlers.lock().await;
                                for (_, handler) in handlers.iter() {
                                    handler(event.clone());
                                }
                            });
                        }
                    }
                    _ => {}
                }
            });

        rpc_client
            .set_notification_handler(notification_handler)
            .await;

        // Set up tool.call request handler
        let sessions_for_tools = Arc::clone(&self.sessions);
        let tool_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_tools);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
                let tool_call_id = params
                    .get("toolCallId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing toolCallId".to_string()))?;
                let tool_name = params
                    .get("toolName")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing toolName".to_string()))?;
                let arguments = params.get("arguments").cloned().unwrap_or(Value::Null);

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Unknown session {}", session_id))
                })?;

                let handler = session.get_tool_handler(tool_name).await;

                let result = if let Some(handler) = handler {
                    let invocation = ToolInvocation {
                        session_id: session_id.to_string(),
                        tool_call_id: tool_call_id.to_string(),
                        tool_name: tool_name.to_string(),
                        arguments: arguments.clone(),
                    };

                    match handler(arguments, invocation).await {
                        Ok(value) => normalize_tool_result(value),
                        Err(e) => ToolResultObject {
                            text_result_for_llm:
                                "Invoking this tool produced an error. Detailed information is not available."
                                    .to_string(),
                            binary_results_for_llm: None,
                            result_type: ToolResultType::Failure,
                            error: Some(e.to_string()),
                            session_log: None,
                            tool_telemetry: Some(HashMap::new()),
                        },
                    }
                } else {
                    ToolResultObject {
                        text_result_for_llm: format!(
                            "Tool '{}' is not supported by this client instance.",
                            tool_name
                        ),
                        binary_results_for_llm: None,
                        result_type: ToolResultType::Failure,
                        error: Some(format!("tool '{}' not supported", tool_name)),
                        session_log: None,
                        tool_telemetry: Some(HashMap::new()),
                    }
                };

                let response =
                    serde_json::to_value(ToolCallResponsePayload { result })
                        .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                Ok(response)
            })
        });
        rpc_client.set_request_handler("tool.call", tool_handler).await;

        // Set up permission.request handler
        let sessions_for_perm = Arc::clone(&self.sessions);
        let permission_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_perm);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
                let perm_request = params
                    .get("permissionRequest")
                    .cloned()
                    .unwrap_or(Value::Null);

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Session not found: {}", session_id))
                })?;

                let result = match session.handle_permission_request(perm_request).await {
                    Ok(result) => result,
                    Err(_) => PermissionRequestResult {
                        kind: PermissionResultKind::DeniedNoApprovalRuleAndCouldNotRequestFromUser,
                        rules: None,
                    },
                };

                let response = serde_json::json!({ "result": result });
                Ok(response)
            })
        });
        rpc_client
            .set_request_handler("permission.request", permission_handler)
            .await;

        // Set up userInput.request handler
        let sessions_for_input = Arc::clone(&self.sessions);
        let user_input_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_input);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Session not found: {}", session_id))
                })?;

                let result = session.handle_user_input_request(params).await?;
                let response = serde_json::to_value(result)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                Ok(response)
            })
        });
        rpc_client
            .set_request_handler("userInput.request", user_input_handler)
            .await;

        // Set up hooks.invoke handler
        let sessions_for_hooks = Arc::clone(&self.sessions);
        let hooks_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_hooks);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
                let hook_type = params
                    .get("hookType")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing hookType".to_string()))?;
                let input = params.get("input").cloned().unwrap_or(Value::Null);

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Session not found: {}", session_id))
                })?;

                let output = session.handle_hooks_invoke(hook_type, input).await?;
                let response = serde_json::json!({ "output": output });
                Ok(response)
            })
        });
        rpc_client
            .set_request_handler("hooks.invoke", hooks_handler)
            .await;
    }
}

// ============================================================================
// Tool Result Normalization
// ============================================================================

/// Normalizes an arbitrary tool handler return value into a ToolResultObject.
fn normalize_tool_result(value: Value) -> ToolResultObject {
    if value.is_null() {
        return ToolResultObject {
            text_result_for_llm: "Tool returned no result".to_string(),
            binary_results_for_llm: None,
            result_type: ToolResultType::Failure,
            error: Some("tool returned no result".to_string()),
            session_log: None,
            tool_telemetry: Some(HashMap::new()),
        };
    }

    // Check if it's already a ToolResultObject (duck-type check)
    if value.get("textResultForLlm").is_some() && value.get("resultType").is_some() {
        if let Ok(result) = serde_json::from_value::<ToolResultObject>(value.clone()) {
            return result;
        }
    }

    // Wrap as success
    let text_result = if let Some(s) = value.as_str() {
        s.to_string()
    } else {
        serde_json::to_string(&value).unwrap_or_else(|_| "".to_string())
    };

    ToolResultObject {
        text_result_for_llm: text_result,
        binary_results_for_llm: None,
        result_type: ToolResultType::Success,
        error: None,
        session_log: None,
        tool_telemetry: Some(HashMap::new()),
    }
}
