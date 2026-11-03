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
use crate::session::{CopilotSession, GitHubTokenRegistration, GitHubTokenRegistry};
use crate::types::*;
use crate::CopilotError;

/// Moves the Auto routing tier and WebSocket transport options into the `capi` wire object.
fn insert_capi_options(
    params: &mut Value,
    auto_tier: Option<AutoTier>,
    enable_web_socket_responses: Option<bool>,
) {
    let mut capi = serde_json::Map::new();
    if let Some(tier) = auto_tier {
        if let Ok(value) = serde_json::to_value(tier) {
            capi.insert("autoTier".to_string(), value);
        }
    }
    if let Some(enabled) = enable_web_socket_responses {
        capi.insert("enableWebSocketResponses".to_string(), Value::Bool(enabled));
    }
    if !capi.is_empty() {
        if let Some(obj) = params.as_object_mut() {
            obj.insert("capi".to_string(), Value::Object(capi));
        }
    }
}

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
///     ..Default::default()
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
    github_token_providers: GitHubTokenRegistry,
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
            github_token_providers: Arc::new(Mutex::new(HashMap::new())),
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

        // Declare the application identity (if any) before the protocol handshake
        self.send_client_info().await?;

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

        // Drop all GitHub token provider registrations
        {
            let mut providers = self.github_token_providers.lock().await;
            providers.clear();
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

        // Drop all GitHub token provider registrations
        {
            let mut providers = self.github_token_providers.lock().await;
            providers.clear();
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
        mut config: SessionConfig,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        if config.github_token.is_some() && config.github_token_provider.is_some() {
            return Err(CopilotError::Configuration(
                "github_token and github_token_provider are mutually exclusive".to_string(),
            ));
        }

        self.ensure_connected().await?;

        let rpc = self.get_rpc_client().await?;

        // Signal to the runtime that an MCP OAuth host-token handler is registered.
        if config.on_mcp_auth_request.is_some() {
            config.mcp_auth_handler = Some(true);
        }

        let mut params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        insert_capi_options(
            &mut params,
            config.auto_tier,
            config.enable_web_socket_responses,
        );

        let registration_id = self
            .register_github_token_provider(
                config.github_token_provider.as_ref(),
                config.session_id.clone(),
                &mut params,
            )
            .await;

        self.open_session(rpc, "session.create", params, registration_id)
            .await
    }

    /// Resumes an existing conversation session by its ID.
    pub async fn resume_session(
        &self,
        config: ResumeSessionConfig,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        if config.github_token.is_some() && config.github_token_provider.is_some() {
            return Err(CopilotError::Configuration(
                "github_token and github_token_provider are mutually exclusive".to_string(),
            ));
        }

        self.ensure_connected().await?;

        let rpc = self.get_rpc_client().await?;

        let mut params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        insert_capi_options(
            &mut params,
            config.auto_tier,
            config.enable_web_socket_responses,
        );

        let registration_id = self
            .register_github_token_provider(
                config.github_token_provider.as_ref(),
                Some(config.session_id.clone()),
                &mut params,
            )
            .await;

        self.open_session(rpc, "session.resume", params, registration_id)
            .await
    }

    /// Sends `session.create` / `session.resume` and tracks the resulting session.
    ///
    /// A GitHub token provider registration is dropped if the request fails, and
    /// otherwise bound to the returned session (which removes it on destroy).
    async fn open_session(
        &self,
        rpc: Arc<JsonRpcClient>,
        method: &str,
        params: Value,
        github_token_registration: Option<String>,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        let opened = rpc.request(method, params, None).await.and_then(|response| {
            let session_id = response
                .get("sessionId")
                .and_then(|v| v.as_str())
                .ok_or_else(|| {
                    CopilotError::Protocol("Missing sessionId in response".to_string())
                })?
                .to_string();
            let workspace_path = response
                .get("workspacePath")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            Ok((session_id, workspace_path))
        });
        let (session_id, workspace_path) = match opened {
            Ok(opened) => opened,
            Err(e) => {
                if let Some(registration_id) = github_token_registration {
                    let mut providers = self.github_token_providers.lock().await;
                    providers.remove(&registration_id);
                }
                return Err(e);
            }
        };

        let session = Arc::new(CopilotSession::new(
            session_id.clone(),
            rpc.clone(),
            workspace_path,
        ));

        if let Some(registration_id) = github_token_registration {
            {
                let mut providers = self.github_token_providers.lock().await;
                if let Some(registration) = providers.get_mut(&registration_id) {
                    registration.session_id = Some(session_id.clone());
                }
            }
            session
                .set_github_token_registration(
                    registration_id,
                    Arc::clone(&self.github_token_providers),
                )
                .await;
        }

        {
            let mut sessions = self.sessions.lock().await;
            sessions.insert(session_id, Arc::clone(&session));
        }

        Ok(session)
    }

    /// Registers a session's GitHub token provider and adds its registration ID to
    /// the request params (`gitHubTokenProviderRegistrationId`).
    async fn register_github_token_provider(
        &self,
        provider: Option<&GitHubTokenProviderFn>,
        session_id: Option<String>,
        params: &mut Value,
    ) -> Option<String> {
        let provider = provider?.clone();
        let registration_id = uuid::Uuid::new_v4().to_string();
        {
            let mut providers = self.github_token_providers.lock().await;
            providers.insert(
                registration_id.clone(),
                GitHubTokenRegistration {
                    provider,
                    session_id,
                },
            );
        }
        if let Some(obj) = params.as_object_mut() {
            obj.insert(
                "gitHubTokenProviderRegistrationId".to_string(),
                Value::String(registration_id.clone()),
            );
        }
        Some(registration_id)
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

    /// Gets metadata for a specific session.
    pub async fn get_session_metadata(
        &self,
        session_id: &str,
    ) -> Result<serde_json::Value, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request(
                "session.getMetadata",
                serde_json::json!({ "sessionId": session_id }),
                None,
            )
            .await?;
        Ok(response)
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

    /// Clears the runtime's cached managed (enterprise) settings (`managedSettings.clearCache`).
    pub async fn clear_managed_settings_cache(&self) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        rpc.request("managedSettings.clearCache", serde_json::json!({}), None)
            .await?;
        Ok(())
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
    // Session Filesystem Provider
    // ========================================================================

    /// Sets the session filesystem provider configuration.
    ///
    /// This configures the session filesystem with optional initial working directory,
    /// session state path, and conventions.
    pub async fn set_session_fs_provider(
        &self,
        config: SessionFsConfig,
    ) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        rpc.request("sessionFs.setProvider", params, None).await?;
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

    /// Sends the `connect` handshake carrying `client_info` (skipped when unset or all-empty).
    ///
    /// Older CLIs without `connect` answer -32601 / "Unhandled method connect"; that is ignored.
    async fn send_client_info(&self) -> Result<(), CopilotError> {
        let client_info = match self
            .options
            .client_info
            .as_ref()
            .and_then(|info| info.to_wire())
        {
            Some(client_info) => client_info,
            None => return Ok(()),
        };
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::json!({ "clientInfo": client_info });
        match rpc.request("connect", params, None).await {
            Ok(_) => Ok(()),
            Err(CopilotError::JsonRpc { code, message, .. })
                if code == -32601 || message.contains("Unhandled method connect") =>
            {
                Ok(())
            }
            Err(e) => Err(e),
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
                        decision_context: None,
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

        // Set up gitHubToken.getToken handler (session GitHub token providers)
        let github_token_providers = Arc::clone(&self.github_token_providers);
        let github_token_handler: RequestHandler = Arc::new(move |params: Value| {
            let providers = Arc::clone(&github_token_providers);
            Box::pin(async move {
                let registration_id = params
                    .get("registrationId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing registrationId".to_string()))?;
                let host = params
                    .get("host")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing host".to_string()))?;
                let reason = params
                    .get("reason")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing reason".to_string()))?;

                let registration = {
                    let providers = providers.lock().await;
                    providers.get(registration_id).cloned()
                };
                let registration = registration.ok_or_else(|| {
                    CopilotError::NoHandler(format!(
                        "No GitHub token provider registered for registration ID \"{}\"",
                        registration_id
                    ))
                })?;

                let args = GitHubTokenProviderArgs {
                    host: host.to_string(),
                    session_id: params
                        .get("sessionId")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string())
                        .or(registration.session_id),
                    reason: reason.to_string(),
                };
                let result = (registration.provider.0)(args).await.map_err(|e| {
                    CopilotError::SessionError(format!("GitHub token provider failed: {}", e))
                })?;
                serde_json::to_value(result)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))
            })
        });
        rpc_client
            .set_request_handler("gitHubToken.getToken", github_token_handler)
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
