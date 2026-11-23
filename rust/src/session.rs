// Copyright (c) Microsoft Corporation. All rights reserved.

//! CopilotSession - represents a single conversation session with the Copilot CLI.
//!
//! Sessions are created via [`CopilotClient::create_session`] or resumed via
//! [`CopilotClient::resume_session`]. They maintain conversation state, handle events,
//! and manage tool execution.

use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use serde_json::Value;
use tokio::sync::{mpsc, oneshot, Mutex};

use crate::jsonrpc::JsonRpcClient;
use crate::types::*;
use crate::CopilotError;

// ============================================================================
// Handler Type Aliases
// ============================================================================

/// A tool handler is an async function that takes arguments and invocation info,
/// and returns a result value.
pub type ToolHandler = Arc<
    dyn Fn(
            Value,
            ToolInvocation,
        ) -> Pin<Box<dyn Future<Output = Result<Value, CopilotError>> + Send>>
        + Send
        + Sync,
>;

/// A permission handler is an async function that takes a permission request
/// and returns a permission result.
pub type PermissionHandlerFn = Arc<
    dyn Fn(
            PermissionRequest,
            String,
        ) -> Pin<Box<dyn Future<Output = Result<PermissionRequestResult, CopilotError>> + Send>>
        + Send
        + Sync,
>;

/// A user input handler is an async function that takes a user input request
/// and returns a response.
pub type UserInputHandlerFn = Arc<
    dyn Fn(
            UserInputRequest,
            String,
        ) -> Pin<Box<dyn Future<Output = Result<UserInputResponse, CopilotError>> + Send>>
        + Send
        + Sync,
>;

/// A hooks handler is an async function that takes hook type, input, and session ID,
/// and returns optional output.
pub type HooksHandlerFn = Arc<
    dyn Fn(
            String,
            Value,
            String,
        ) -> Pin<Box<dyn Future<Output = Result<Option<Value>, CopilotError>> + Send>>
        + Send
        + Sync,
>;

/// A session event handler callback.
pub type SessionEventHandlerFn = Arc<dyn Fn(SessionEvent) + Send + Sync>;

/// A typed session event handler for a specific event type.
pub type TypedSessionEventHandlerFn = Arc<dyn Fn(SessionEvent) + Send + Sync>;

/// A GitHub token provider registered for one session (routes `gitHubToken.getToken`).
#[derive(Clone)]
pub(crate) struct GitHubTokenRegistration {
    pub(crate) provider: GitHubTokenProviderFn,
    pub(crate) session_id: Option<String>,
}

/// Client-level registry of GitHub token providers, keyed by registration ID.
pub(crate) type GitHubTokenRegistry = Arc<Mutex<HashMap<String, GitHubTokenRegistration>>>;

/// Builds the structured-output `responseFormat` wire object for a JSON Schema.
fn structured_response_format(schema: &Value) -> Value {
    serde_json::json!({
        "type": "json_schema",
        "jsonSchema": {
            "name": "response",
            "strict": true,
            "schema": schema,
        },
    })
}

/// Applies `f` to the locked value without blocking a runtime thread.
///
/// Uses `try_lock` first; when contended inside a runtime the update runs on a
/// spawned task (tokio's `blocking_lock` panics there), otherwise it blocks.
fn lock_detached<T, F>(mutex: Arc<Mutex<T>>, f: F)
where
    T: Send + 'static,
    F: FnOnce(&mut T) + Send + 'static,
{
    if let Ok(mut guard) = mutex.try_lock() {
        f(&mut guard);
        return;
    }
    match tokio::runtime::Handle::try_current() {
        Ok(runtime) => {
            runtime.spawn(async move {
                let mut guard = mutex.lock().await;
                f(&mut guard);
            });
        }
        Err(_) => {
            let mut guard = mutex.blocking_lock();
            f(&mut guard);
        }
    }
}

// ============================================================================
// Unsubscribe Guard
// ============================================================================

/// An RAII guard that unsubscribes a handler when dropped.
/// Can also be explicitly unsubscribed via `unsubscribe()`.
pub struct Subscription {
    unsubscribe_fn: Option<Box<dyn FnOnce() + Send>>,
}

impl Subscription {
    fn new(f: impl FnOnce() + Send + 'static) -> Self {
        Self {
            unsubscribe_fn: Some(Box::new(f)),
        }
    }

    /// Explicitly unsubscribes this handler.
    pub fn unsubscribe(mut self) {
        if let Some(f) = self.unsubscribe_fn.take() {
            f();
        }
    }
}

impl Drop for Subscription {
    fn drop(&mut self) {
        if let Some(f) = self.unsubscribe_fn.take() {
            f();
        }
    }
}

// ============================================================================
// CopilotSession
// ============================================================================

/// Represents a single conversation session with the Copilot CLI.
///
/// A session maintains conversation state, handles events, and manages tool execution.
///
/// # Examples
///
/// ```rust,no_run
/// # use copilot_sdk::*;
/// # async fn example() -> Result<(), CopilotError> {
/// let client = CopilotClient::new(CopilotClientOptions::default());
/// let session = client.create_session(SessionConfig::default()).await?;
///
/// // Subscribe to events
/// let sub = session.on(|event| {
///     if event.is_assistant_message() {
///         if let Some(content) = event.assistant_message_content() {
///             println!("Assistant: {}", content);
///         }
///     }
/// }).await;
///
/// // Send a message and wait for completion
/// let response = session.send_and_wait(
///     MessageOptions { prompt: "Hello!".into(), ..Default::default() },
///     None,
/// ).await?;
///
/// // Clean up
/// session.destroy().await?;
/// # Ok(())
/// # }
/// ```
pub struct CopilotSession {
    /// The unique session ID.
    session_id: String,
    /// Path to the session workspace directory (when infinite sessions are enabled).
    workspace_path: Option<String>,
    /// Reference to the JSON-RPC client.
    rpc_client: Arc<JsonRpcClient>,
    /// Registered tool handlers, keyed by tool name.
    tool_handlers: Arc<Mutex<HashMap<String, ToolHandler>>>,
    /// Permission request handler.
    permission_handler: Arc<Mutex<Option<PermissionHandlerFn>>>,
    /// User input request handler.
    user_input_handler: Arc<Mutex<Option<UserInputHandlerFn>>>,
    /// Hooks handler.
    hooks_handler: Arc<Mutex<Option<HooksHandlerFn>>>,
    /// Wildcard event handlers.
    event_handlers: Arc<Mutex<Vec<(u64, SessionEventHandlerFn)>>>,
    /// Typed event handlers, keyed by event type string.
    typed_event_handlers: Arc<Mutex<HashMap<String, Vec<(u64, TypedSessionEventHandlerFn)>>>>,
    /// Counter for handler IDs (for unsubscribe).
    next_handler_id: Arc<Mutex<u64>>,
    /// GitHub token provider registration owned by this session (removed on destroy).
    github_token_registration: Mutex<Option<(String, GitHubTokenRegistry)>>,
    /// Shell authentication handler (`shellAuthentication.request`).
    shell_authentication_handler: Mutex<Option<ShellAuthenticationHandlerFn>>,
    /// Cancellation senders for in-flight shell authentication requests, keyed by request ID.
    pending_shell_authentications: Mutex<HashMap<String, oneshot::Sender<()>>>,
    /// Application memory store (`memoryStore.store`).
    memory_store: Mutex<Option<MemoryStoreHandlerFn>>,
}

impl CopilotSession {
    /// Creates a new CopilotSession. This is called internally by CopilotClient.
    pub(crate) fn new(
        session_id: String,
        rpc_client: Arc<JsonRpcClient>,
        workspace_path: Option<String>,
    ) -> Self {
        Self {
            session_id,
            workspace_path,
            rpc_client,
            tool_handlers: Arc::new(Mutex::new(HashMap::new())),
            permission_handler: Arc::new(Mutex::new(None)),
            user_input_handler: Arc::new(Mutex::new(None)),
            hooks_handler: Arc::new(Mutex::new(None)),
            event_handlers: Arc::new(Mutex::new(Vec::new())),
            typed_event_handlers: Arc::new(Mutex::new(HashMap::new())),
            next_handler_id: Arc::new(Mutex::new(0)),
            github_token_registration: Mutex::new(None),
            shell_authentication_handler: Mutex::new(None),
            pending_shell_authentications: Mutex::new(HashMap::new()),
            memory_store: Mutex::new(None),
        }
    }

    /// Returns the session ID.
    pub fn session_id(&self) -> &str {
        &self.session_id
    }

    /// Returns the workspace path (when infinite sessions are enabled).
    pub fn workspace_path(&self) -> Option<&str> {
        self.workspace_path.as_deref()
    }

    // ========================================================================
    // Message Sending
    // ========================================================================

    /// Sends a message to this session.
    ///
    /// The message is processed asynchronously. Subscribe to events via `on()`
    /// to receive streaming responses and other session events.
    ///
    /// Returns the message ID.
    pub async fn send(&self, options: MessageOptions) -> Result<String, CopilotError> {
        let mut params = serde_json::json!({
            "sessionId": self.session_id,
            "prompt": options.prompt,
            "attachments": options.attachments,
            "mode": options.mode,
            "agentMode": options.agent_mode,
            "displayPrompt": options.display_prompt,
            "requestHeaders": options.request_headers,
            "responseFormat": options.response_format,
            "imageOptions": options.image_options,
        });
        if let Some(source) = options.source.as_ref() {
            params["source"] = Value::String(source.to_string());
        }
        if let Some(client_turn_id) = options.client_turn_id.as_ref() {
            params["clientTurnId"] = Value::String(client_turn_id.clone());
        }
        // Structured output takes precedence over the legacy string response format.
        if let Some(schema) = options.response_schema.as_ref() {
            params["responseFormat"] = structured_response_format(schema);
        }

        let response = self.rpc_client.request("session.send", params, None).await?;
        let message_id = response
            .get("messageId")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        Ok(message_id)
    }

    /// Sends a message and waits until the session becomes idle.
    ///
    /// This combines `send()` with waiting for the `session.idle` event.
    /// Returns the last `assistant.message` event received, or `None`.
    ///
    /// # Arguments
    /// * `options` - The message options
    /// * `timeout` - Optional timeout in milliseconds (defaults to 60000)
    pub async fn send_and_wait(
        &self,
        options: MessageOptions,
        timeout: Option<u64>,
    ) -> Result<Option<SessionEvent>, CopilotError> {
        let effective_timeout = timeout.unwrap_or(60_000);

        // Channel to signal idle or error
        let (idle_tx, mut idle_rx) = mpsc::channel::<Result<(), CopilotError>>(1);
        // A std mutex: the handler runs synchronously on a runtime thread, where
        // tokio's `blocking_lock` would panic.
        let last_assistant_message: Arc<std::sync::Mutex<Option<SessionEvent>>> =
            Arc::new(std::sync::Mutex::new(None));

        let last_msg_clone = Arc::clone(&last_assistant_message);
        let idle_tx_clone = idle_tx.clone();

        // Register event handler BEFORE calling send to avoid race condition
        let sub = self
            .on(move |event: SessionEvent| {
                if event.is_assistant_message() {
                    if let Ok(mut msg) = last_msg_clone.lock() {
                        *msg = Some(event);
                    }
                } else if event.is_session_idle() {
                    let _ = idle_tx_clone.try_send(Ok(()));
                } else if event.is_session_error() {
                    let error_msg = event
                        .error_message()
                        .unwrap_or("Unknown error")
                        .to_string();
                    let _ = idle_tx_clone.try_send(Err(CopilotError::SessionError(error_msg)));
                }
            })
            .await;

        // Send the message
        self.send(options).await?;

        // Wait for idle or timeout
        let result = tokio::time::timeout(
            std::time::Duration::from_millis(effective_timeout),
            idle_rx.recv(),
        )
        .await;

        // Unsubscribe
        sub.unsubscribe();

        match result {
            Ok(Some(Ok(()))) => {
                let msg = last_assistant_message
                    .lock()
                    .map(|msg| msg.clone())
                    .unwrap_or(None);
                Ok(msg)
            }
            Ok(Some(Err(e))) => Err(e),
            Ok(None) => Err(CopilotError::ConnectionClosed),
            Err(_) => Err(CopilotError::Timeout(effective_timeout)),
        }
    }

    /// Sends a message requesting structured output and returns the parsed JSON result.
    ///
    /// Sets `response_schema` on the message, waits via [`send_and_wait`](Self::send_and_wait),
    /// and parses the final assistant message content as JSON. Returns an error when the
    /// run completes without assistant content.
    ///
    /// # Arguments
    /// * `options` - The message options, or just a prompt (`&str` / `String`)
    /// * `schema` - JSON Schema describing the expected response
    /// * `timeout` - Optional timeout in milliseconds (defaults to 60000)
    pub async fn send_and_wait_structured(
        &self,
        options: impl Into<MessageOptions>,
        schema: Value,
        timeout: Option<u64>,
    ) -> Result<Value, CopilotError> {
        let mut options = options.into();
        options.response_schema = Some(schema);
        let event = self.send_and_wait(options, timeout).await?;
        let content = event
            .as_ref()
            .and_then(|e| e.assistant_message_content())
            .map(str::trim)
            .filter(|c| !c.is_empty())
            .ok_or_else(|| {
                CopilotError::SessionError(
                    "The requested run completed without a structured assistant response."
                        .to_string(),
                )
            })?;
        serde_json::from_str(content).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    // ========================================================================
    // Event Subscription
    // ========================================================================

    /// Subscribes to all events from this session.
    ///
    /// Returns a `Subscription` that unsubscribes when dropped or when
    /// `unsubscribe()` is called.
    pub async fn on<F>(&self, handler: F) -> Subscription
    where
        F: Fn(SessionEvent) + Send + Sync + 'static,
    {
        let handler_id = {
            let mut id = self.next_handler_id.lock().await;
            *id += 1;
            *id
        };

        let handler_arc: SessionEventHandlerFn = Arc::new(handler);
        {
            let mut handlers = self.event_handlers.lock().await;
            handlers.push((handler_id, handler_arc));
        }

        let event_handlers = Arc::clone(&self.event_handlers);
        Subscription::new(move || {
            // Unsubscribe may run from Drop in sync or async contexts
            lock_detached(event_handlers, move |handlers| {
                handlers.retain(|(id, _)| *id != handler_id);
            });
        })
    }

    /// Subscribes to a specific event type from this session.
    ///
    /// # Arguments
    /// * `event_type` - The event type string (e.g., "assistant.message", "session.idle")
    /// * `handler` - The callback function
    pub async fn on_event<F>(&self, event_type: &str, handler: F) -> Subscription
    where
        F: Fn(SessionEvent) + Send + Sync + 'static,
    {
        let handler_id = {
            let mut id = self.next_handler_id.lock().await;
            *id += 1;
            *id
        };

        let handler_arc: TypedSessionEventHandlerFn = Arc::new(handler);
        let event_type_str = event_type.to_string();
        {
            let mut handlers = self.typed_event_handlers.lock().await;
            handlers
                .entry(event_type_str.clone())
                .or_default()
                .push((handler_id, handler_arc));
        }

        let typed_handlers = Arc::clone(&self.typed_event_handlers);
        let et = event_type_str;
        Subscription::new(move || {
            lock_detached(typed_handlers, move |handlers| {
                if let Some(list) = handlers.get_mut(&et) {
                    list.retain(|(id, _)| *id != handler_id);
                }
            });
        })
    }

    // ========================================================================
    // Event Dispatch (internal, called by CopilotClient)
    // ========================================================================

    /// Dispatches a session event to all registered handlers.
    pub(crate) async fn dispatch_event(&self, event: SessionEvent) {
        // Dispatch to typed handlers
        {
            let handlers = self.typed_event_handlers.lock().await;
            if let Some(list) = handlers.get(&event.event_type) {
                for (_, handler) in list {
                    handler(event.clone());
                }
            }
        }

        // Dispatch to wildcard handlers
        {
            let handlers = self.event_handlers.lock().await;
            for (_, handler) in handlers.iter() {
                handler(event.clone());
            }
        }
    }

    // ========================================================================
    // Tool Registration
    // ========================================================================

    /// Registers a tool handler.
    pub async fn register_tool(&self, name: &str, handler: ToolHandler) {
        let mut handlers = self.tool_handlers.lock().await;
        handlers.insert(name.to_string(), handler);
    }

    /// Registers multiple tool handlers.
    pub async fn register_tools(&self, tools: Vec<(String, ToolHandler)>) {
        let mut handlers = self.tool_handlers.lock().await;
        handlers.clear();
        for (name, handler) in tools {
            handlers.insert(name, handler);
        }
    }

    /// Gets a registered tool handler by name.
    pub(crate) async fn get_tool_handler(&self, name: &str) -> Option<ToolHandler> {
        let handlers = self.tool_handlers.lock().await;
        handlers.get(name).cloned()
    }

    // ========================================================================
    // Permission Handler
    // ========================================================================

    /// Registers a permission request handler.
    pub async fn register_permission_handler(&self, handler: PermissionHandlerFn) {
        let mut h = self.permission_handler.lock().await;
        *h = Some(handler);
    }

    /// Handles an incoming permission request from the server.
    pub(crate) async fn handle_permission_request(
        &self,
        request: Value,
    ) -> Result<PermissionRequestResult, CopilotError> {
        let handler = self.permission_handler.lock().await;
        if let Some(ref h) = *handler {
            let perm_request: PermissionRequest = serde_json::from_value(request)
                .map_err(|e| CopilotError::Serialization(e.to_string()))?;
            h(perm_request, self.session_id.clone()).await
        } else {
            Ok(PermissionRequestResult {
                kind: PermissionResultKind::DeniedNoApprovalRuleAndCouldNotRequestFromUser,
                rules: None,
                decision_context: None,
            })
        }
    }

    // ========================================================================
    // User Input Handler
    // ========================================================================

    /// Registers a user input request handler.
    pub async fn register_user_input_handler(&self, handler: UserInputHandlerFn) {
        let mut h = self.user_input_handler.lock().await;
        *h = Some(handler);
    }

    /// Handles an incoming user input request from the server.
    pub(crate) async fn handle_user_input_request(
        &self,
        request: Value,
    ) -> Result<UserInputResponse, CopilotError> {
        let handler = self.user_input_handler.lock().await;
        if let Some(ref h) = *handler {
            let input_request: UserInputRequest = serde_json::from_value(request)
                .map_err(|e| CopilotError::Serialization(e.to_string()))?;
            h(input_request, self.session_id.clone()).await
        } else {
            Err(CopilotError::NoHandler(
                "User input requested but no handler registered".to_string(),
            ))
        }
    }

    // ========================================================================
    // Hooks Handler
    // ========================================================================

    /// Registers a hooks handler for all hook types.
    pub async fn register_hooks_handler(&self, handler: HooksHandlerFn) {
        let mut h = self.hooks_handler.lock().await;
        *h = Some(handler);
    }

    /// Handles an incoming hooks invocation from the server.
    pub(crate) async fn handle_hooks_invoke(
        &self,
        hook_type: &str,
        input: Value,
    ) -> Result<Option<Value>, CopilotError> {
        let handler = self.hooks_handler.lock().await;
        if let Some(ref h) = *handler {
            h(hook_type.to_string(), input, self.session_id.clone()).await
        } else {
            Ok(None)
        }
    }

    // ========================================================================
    // Shell Authentication Handler
    // ========================================================================

    /// Installs the shell authentication handler declared on the session config.
    pub(crate) async fn set_shell_authentication_handler(
        &self,
        handler: ShellAuthenticationHandlerFn,
    ) {
        *self.shell_authentication_handler.lock().await = Some(handler);
    }

    /// Answers a `shellAuthentication.request` from the server.
    ///
    /// The handler future is dropped (and `cancel` answered) when the runtime sends
    /// `shellAuthentication.cancel` for the same request ID.
    pub(crate) async fn handle_shell_authentication_request(
        &self,
        params: Value,
    ) -> Result<ShellAuthenticationResponse, CopilotError> {
        let handler = self
            .shell_authentication_handler
            .lock()
            .await
            .clone()
            .ok_or_else(|| {
                CopilotError::NoHandler(
                    "Shell authentication requested but no handler registered".to_string(),
                )
            })?;
        let request: ShellAuthenticationRequest = serde_json::from_value(params)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        let request_id = request.request_id.clone();

        let (cancel_tx, cancel_rx) = oneshot::channel::<()>();
        self.pending_shell_authentications
            .lock()
            .await
            .insert(request_id.clone(), cancel_tx);

        let answer = (handler.0)(request, self.session_id.clone());
        let result = tokio::select! {
            result = answer => result.map_err(|e| {
                CopilotError::SessionError(format!("Shell authentication handler failed: {}", e))
            }),
            _ = cancel_rx => Ok(ShellAuthenticationResponse::Cancel),
        };

        self.pending_shell_authentications
            .lock()
            .await
            .remove(&request_id);
        result
    }

    /// Cancels a pending shell authentication request (`shellAuthentication.cancel`).
    /// Unknown or already-answered request IDs are ignored.
    pub(crate) async fn cancel_shell_authentication(&self, request_id: &str) {
        if let Some(cancel) = self
            .pending_shell_authentications
            .lock()
            .await
            .remove(request_id)
        {
            let _ = cancel.send(());
        }
    }

    // ========================================================================
    // Memory Store
    // ========================================================================

    /// Installs the memory store callback declared on the session config.
    pub(crate) async fn set_memory_store(&self, store: MemoryStoreHandlerFn) {
        *self.memory_store.lock().await = Some(store);
    }

    /// Stores a memory through the application callback (`memoryStore.store`).
    pub(crate) async fn handle_memory_store(&self, params: Value) -> Result<(), CopilotError> {
        let store = self.memory_store.lock().await.clone().ok_or_else(|| {
            CopilotError::NoHandler("Memory store requested but no store registered".to_string())
        })?;
        let memory: StoredMemory = serde_json::from_value(params)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        (store.0)(memory)
            .await
            .map_err(|e| CopilotError::SessionError(format!("Memory store failed: {}", e)))
    }

    // ========================================================================
    // Session Operations
    // ========================================================================

    /// Retrieves all events and messages from this session's history.
    pub async fn get_messages(&self) -> Result<Vec<SessionEvent>, CopilotError> {
        let params = serde_json::json!({ "sessionId": self.session_id });
        let response = self
            .rpc_client
            .request("session.getMessages", params, None)
            .await?;
        let events: Vec<SessionEvent> = serde_json::from_value(
            response
                .get("events")
                .cloned()
                .unwrap_or(Value::Array(vec![])),
        )
        .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        Ok(events)
    }

    /// Retrieves metadata for this session.
    pub async fn get_metadata(&self) -> Result<Value, CopilotError> {
        let params = serde_json::json!({ "sessionId": self.session_id });
        let response = self
            .rpc_client
            .request("session.getMetadata", params, None)
            .await?;
        Ok(response)
    }

    /// Destroys this session and releases all associated resources.
    ///
    /// Sends `session.detach` (falling back to `session.destroy` on runtimes that
    /// don't implement it). Session state on disk is preserved, so the session can
    /// be resumed later. After calling this method, the session can no longer be used.
    pub async fn destroy(&self) -> Result<(), CopilotError> {
        let params = serde_json::json!({ "sessionId": self.session_id });
        match self
            .rpc_client
            .request("session.detach", params.clone(), None)
            .await
        {
            Ok(_) => {}
            Err(CopilotError::JsonRpc { code: -32601, .. }) => {
                self.rpc_client
                    .request("session.destroy", params, None)
                    .await?;
            }
            Err(e) => return Err(e),
        }

        // Drop this session's GitHub token provider registration
        if let Some((registration_id, registry)) = self.github_token_registration.lock().await.take()
        {
            registry.lock().await.remove(&registration_id);
        }

        // Clear all handlers
        {
            let mut handlers = self.event_handlers.lock().await;
            handlers.clear();
        }
        {
            let mut handlers = self.typed_event_handlers.lock().await;
            handlers.clear();
        }
        {
            let mut handlers = self.tool_handlers.lock().await;
            handlers.clear();
        }
        {
            let mut handler = self.permission_handler.lock().await;
            *handler = None;
        }
        {
            let mut handler = self.user_input_handler.lock().await;
            *handler = None;
        }
        *self.shell_authentication_handler.lock().await = None;
        for (_, cancel) in self.pending_shell_authentications.lock().await.drain() {
            let _ = cancel.send(());
        }
        *self.memory_store.lock().await = None;

        Ok(())
    }

    /// Aborts the currently processing message in this session.
    pub async fn abort(&self) -> Result<(), CopilotError> {
        let params = serde_json::json!({ "sessionId": self.session_id });
        self.rpc_client
            .request("session.abort", params, None)
            .await?;
        Ok(())
    }

    /// Changes the Auto routing preference without changing the selected model
    /// (`session.model.switchAutoTier`). Experimental.
    ///
    /// Pass `None` to return to the provider's default Auto routing. Returns the
    /// runtime's acknowledgement (`status`, `effectiveAutoTier`, `pendingAutoTier`,
    /// `activatingAutoTier`, `supersededAutoTier`).
    pub async fn set_auto_tier(&self, auto_tier: Option<AutoTier>) -> Result<Value, CopilotError> {
        let params = serde_json::json!({
            "sessionId": self.session_id,
            "autoTier": auto_tier,
        });
        self.rpc_client
            .request("session.model.switchAutoTier", params, None)
            .await
    }

    /// Gets the managed (enterprise) settings in effect for this session
    /// (`session.managedSettings.get`).
    pub async fn get_managed_settings(&self) -> Result<Value, CopilotError> {
        let params = serde_json::json!({ "sessionId": self.session_id });
        self.rpc_client
            .request("session.managedSettings.get", params, None)
            .await
    }

    /// Re-runs the managed-settings policy helper for this session
    /// (`session.managedSettings.recheckPolicyHelper`).
    pub async fn recheck_policy_helper(&self) -> Result<Value, CopilotError> {
        let params = serde_json::json!({ "sessionId": self.session_id });
        self.rpc_client
            .request("session.managedSettings.recheckPolicyHelper", params, None)
            .await
    }

    /// Changes the session permission mode (`session.permissions.setMode`): `"manual"`,
    /// `"assisted"`, or `"allow-all"`. Experimental.
    ///
    /// A refused change returns `success: false` with the authoritative mode and, when
    /// known, a `reason` such as [`permission_mode_refusal::MANAGED_POLICY`].
    pub async fn set_permission_mode(
        &self,
        mode: &str,
    ) -> Result<PermissionsSetModeResult, CopilotError> {
        let params = serde_json::json!({
            "sessionId": self.session_id,
            "mode": mode,
        });
        let response = self
            .rpc_client
            .request("session.permissions.setMode", params, None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Records the GitHub token provider registration owned by this session.
    pub(crate) async fn set_github_token_registration(
        &self,
        registration_id: String,
        registry: GitHubTokenRegistry,
    ) {
        let mut registration = self.github_token_registration.lock().await;
        *registration = Some((registration_id, registry));
    }
}
