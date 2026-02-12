// Copyright (c) Microsoft Corporation. All rights reserved.

//! JSON-RPC 2.0 client implementation for stdio and TCP transports.
//!
//! Uses Content-Length header framing (LSP-style) for message delimiting.
//! Messages are formatted as:
//! ```text
//! Content-Length: <length>\r\n
//! \r\n
//! <json-body>
//! ```

use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::sync::{mpsc, oneshot, Mutex};

use crate::CopilotError;

// ============================================================================
// JSON-RPC Message Types
// ============================================================================

/// A JSON-RPC 2.0 request message.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub id: String,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

/// A JSON-RPC 2.0 response message.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

/// A JSON-RPC 2.0 notification (no id).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcNotification {
    pub jsonrpc: String,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

/// A JSON-RPC 2.0 error object.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

impl std::fmt::Display for JsonRpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "JSON-RPC Error {}: {}", self.code, self.message)
    }
}

/// A generic JSON-RPC message (could be request, response, or notification).
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RawMessage {
    jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    method: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    params: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

/// Type alias for an async request handler.
/// Takes params and returns a result Value.
pub type RequestHandler = Arc<
    dyn Fn(Value) -> Pin<Box<dyn Future<Output = Result<Value, CopilotError>> + Send>>
        + Send
        + Sync,
>;

/// Type alias for a notification handler.
/// Takes method name and params.
pub type NotificationHandler = Arc<dyn Fn(String, Value) + Send + Sync>;

// ============================================================================
// Internal message for the write channel
// ============================================================================

enum WriteCommand {
    Send(Vec<u8>),
    Shutdown,
}

// ============================================================================
// JSON-RPC Client
// ============================================================================

/// An async JSON-RPC 2.0 client that communicates over a byte stream
/// using Content-Length header framing.
///
/// This client supports:
/// - Sending requests and waiting for responses
/// - Receiving notifications from the server
/// - Handling incoming requests from the server (e.g., tool.call)
pub struct JsonRpcClient {
    /// Pending request futures awaiting responses, keyed by request ID.
    pending_requests: Arc<Mutex<HashMap<String, oneshot::Sender<Result<Value, JsonRpcError>>>>>,
    /// Registered handlers for incoming server requests (e.g., "tool.call").
    request_handlers: Arc<Mutex<HashMap<String, RequestHandler>>>,
    /// Handler for incoming notifications.
    notification_handler: Arc<Mutex<Option<NotificationHandler>>>,
    /// Channel to send outgoing messages to the writer task.
    write_tx: mpsc::Sender<WriteCommand>,
    /// Handle to the reader task.
    reader_handle: Option<tokio::task::JoinHandle<()>>,
    /// Handle to the writer task.
    writer_handle: Option<tokio::task::JoinHandle<()>>,
}

impl JsonRpcClient {
    /// Creates a new JsonRpcClient from async reader and writer streams.
    ///
    /// This spawns two background tasks:
    /// - A reader task that reads and dispatches incoming messages
    /// - A writer task that serializes and sends outgoing messages
    ///
    /// # Arguments
    /// * `reader` - An async reader (e.g., stdout of a child process or TCP stream read half)
    /// * `writer` - An async writer (e.g., stdin of a child process or TCP stream write half)
    pub fn new<R, W>(reader: R, writer: W) -> Self
    where
        R: tokio::io::AsyncRead + Unpin + Send + 'static,
        W: tokio::io::AsyncWrite + Unpin + Send + 'static,
    {
        let pending_requests: Arc<
            Mutex<HashMap<String, oneshot::Sender<Result<Value, JsonRpcError>>>>,
        > = Arc::new(Mutex::new(HashMap::new()));
        let request_handlers: Arc<Mutex<HashMap<String, RequestHandler>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let notification_handler: Arc<Mutex<Option<NotificationHandler>>> =
            Arc::new(Mutex::new(None));

        // Write channel
        let (write_tx, write_rx) = mpsc::channel::<WriteCommand>(256);

        // Spawn writer task
        let writer_handle = tokio::spawn(Self::writer_loop(writer, write_rx));

        // Spawn reader task
        let reader_handle = tokio::spawn(Self::reader_loop(
            reader,
            Arc::clone(&pending_requests),
            Arc::clone(&request_handlers),
            Arc::clone(&notification_handler),
            write_tx.clone(),
        ));

        Self {
            pending_requests,
            request_handlers,
            notification_handler,
            write_tx,
            reader_handle: Some(reader_handle),
            writer_handle: Some(writer_handle),
        }
    }

    /// Sends a JSON-RPC request and waits for the response.
    ///
    /// # Arguments
    /// * `method` - The RPC method name
    /// * `params` - The parameters to send
    /// * `timeout` - Optional timeout duration (defaults to 30 seconds)
    ///
    /// # Returns
    /// The result value from the response.
    ///
    /// # Errors
    /// Returns an error if the server responds with an error, the request times out,
    /// or the connection is closed.
    pub async fn request(
        &self,
        method: &str,
        params: Value,
        timeout: Option<std::time::Duration>,
    ) -> Result<Value, CopilotError> {
        let request_id = uuid::Uuid::new_v4().to_string();
        let timeout = timeout.unwrap_or(std::time::Duration::from_secs(30));

        let (tx, rx) = oneshot::channel();
        {
            let mut pending = self.pending_requests.lock().await;
            pending.insert(request_id.clone(), tx);
        }

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: request_id.clone(),
            method: method.to_string(),
            params,
        };

        let content = serde_json::to_string(&request)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        let content_bytes = content.as_bytes();
        let header = format!("Content-Length: {}\r\n\r\n", content_bytes.len());
        let mut message = header.into_bytes();
        message.extend_from_slice(content_bytes);

        self.write_tx
            .send(WriteCommand::Send(message))
            .await
            .map_err(|_| CopilotError::ConnectionClosed)?;

        // Wait for response with timeout
        let result = tokio::time::timeout(timeout, rx).await;
        match result {
            Ok(Ok(Ok(value))) => Ok(value),
            Ok(Ok(Err(rpc_error))) => Err(CopilotError::JsonRpc {
                code: rpc_error.code,
                message: rpc_error.message,
                data: rpc_error.data,
            }),
            Ok(Err(_)) => {
                // oneshot channel dropped - clean up
                let mut pending = self.pending_requests.lock().await;
                pending.remove(&request_id);
                Err(CopilotError::ConnectionClosed)
            }
            Err(_) => {
                // Timeout - clean up
                let mut pending = self.pending_requests.lock().await;
                pending.remove(&request_id);
                Err(CopilotError::Timeout(timeout.as_millis() as u64))
            }
        }
    }

    /// Sends a JSON-RPC notification (no response expected).
    pub async fn notify(&self, method: &str, params: Value) -> Result<(), CopilotError> {
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params,
        };

        let content = serde_json::to_string(&notification)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        let content_bytes = content.as_bytes();
        let header = format!("Content-Length: {}\r\n\r\n", content_bytes.len());
        let mut message = header.into_bytes();
        message.extend_from_slice(content_bytes);

        self.write_tx
            .send(WriteCommand::Send(message))
            .await
            .map_err(|_| CopilotError::ConnectionClosed)?;

        Ok(())
    }

    /// Registers a handler for a specific incoming request method.
    ///
    /// When the server sends a request (e.g., "tool.call", "permission.request"),
    /// the registered handler is invoked and its return value is sent back.
    pub async fn set_request_handler(&self, method: &str, handler: RequestHandler) {
        let mut handlers = self.request_handlers.lock().await;
        handlers.insert(method.to_string(), handler);
    }

    /// Removes a previously registered request handler.
    pub async fn remove_request_handler(&self, method: &str) {
        let mut handlers = self.request_handlers.lock().await;
        handlers.remove(method);
    }

    /// Sets the notification handler that receives all server notifications.
    pub async fn set_notification_handler(&self, handler: NotificationHandler) {
        let mut h = self.notification_handler.lock().await;
        *h = Some(handler);
    }

    /// Stops the client, shutting down reader and writer tasks.
    pub async fn stop(&mut self) {
        let _ = self.write_tx.send(WriteCommand::Shutdown).await;

        if let Some(handle) = self.writer_handle.take() {
            let _ = handle.await;
        }
        if let Some(handle) = self.reader_handle.take() {
            handle.abort();
            let _ = handle.await;
        }

        // Cancel all pending requests
        let mut pending = self.pending_requests.lock().await;
        for (_, tx) in pending.drain() {
            let _ = tx.send(Err(JsonRpcError {
                code: -32000,
                message: "Client stopped".to_string(),
                data: None,
            }));
        }
    }

    // ========================================================================
    // Internal: Writer loop
    // ========================================================================

    async fn writer_loop<W: tokio::io::AsyncWrite + Unpin>(
        mut writer: W,
        mut rx: mpsc::Receiver<WriteCommand>,
    ) {
        while let Some(cmd) = rx.recv().await {
            match cmd {
                WriteCommand::Send(data) => {
                    if writer.write_all(&data).await.is_err() {
                        break;
                    }
                    if writer.flush().await.is_err() {
                        break;
                    }
                }
                WriteCommand::Shutdown => break,
            }
        }
    }

    // ========================================================================
    // Internal: Reader loop
    // ========================================================================

    async fn reader_loop<R: tokio::io::AsyncRead + Unpin>(
        reader: R,
        pending_requests: Arc<
            Mutex<HashMap<String, oneshot::Sender<Result<Value, JsonRpcError>>>>,
        >,
        request_handlers: Arc<Mutex<HashMap<String, RequestHandler>>>,
        notification_handler: Arc<Mutex<Option<NotificationHandler>>>,
        write_tx: mpsc::Sender<WriteCommand>,
    ) {
        let mut buf_reader = BufReader::new(reader);

        loop {
            // Read the Content-Length header
            let content_length = match Self::read_content_length(&mut buf_reader).await {
                Ok(len) => len,
                Err(_) => break, // Connection closed or error
            };

            // Read the exact content body
            let mut body = vec![0u8; content_length];
            if buf_reader.read_exact(&mut body).await.is_err() {
                break;
            }

            // Parse the JSON message
            let raw: RawMessage = match serde_json::from_slice(&body) {
                Ok(msg) => msg,
                Err(e) => {
                    tracing::warn!("Failed to parse JSON-RPC message: {}", e);
                    continue;
                }
            };

            // Determine message type and dispatch
            let has_id = raw.id.is_some()
                && raw.id.as_ref().map_or(false, |v| !v.is_null());
            let has_method = raw.method.is_some();

            if has_id && !has_method {
                // This is a response to one of our requests
                let id_str = match &raw.id {
                    Some(Value::String(s)) => s.clone(),
                    Some(Value::Number(n)) => n.to_string(),
                    _ => continue,
                };

                let mut pending = pending_requests.lock().await;
                if let Some(tx) = pending.remove(&id_str) {
                    if let Some(error) = raw.error {
                        let _ = tx.send(Err(error));
                    } else if let Some(result) = raw.result {
                        let _ = tx.send(Ok(result));
                    } else {
                        let _ = tx.send(Ok(Value::Null));
                    }
                }
            } else if has_id && has_method {
                // Incoming request from the server (e.g., tool.call)
                let method = raw.method.unwrap();
                let params = raw.params.unwrap_or(Value::Object(Default::default()));
                let id_str = match &raw.id {
                    Some(Value::String(s)) => s.clone(),
                    Some(Value::Number(n)) => n.to_string(),
                    _ => continue,
                };

                let handlers = request_handlers.lock().await;
                let handler = handlers.get(&method).cloned();
                drop(handlers);

                let write_tx = write_tx.clone();
                if let Some(handler) = handler {
                    tokio::spawn(async move {
                        let result = handler(params).await;
                        let response = match result {
                            Ok(value) => serde_json::json!({
                                "jsonrpc": "2.0",
                                "id": id_str,
                                "result": value
                            }),
                            Err(e) => serde_json::json!({
                                "jsonrpc": "2.0",
                                "id": id_str,
                                "error": {
                                    "code": -32603,
                                    "message": e.to_string()
                                }
                            }),
                        };

                        let content = serde_json::to_string(&response).unwrap();
                        let content_bytes = content.as_bytes();
                        let header =
                            format!("Content-Length: {}\r\n\r\n", content_bytes.len());
                        let mut message = header.into_bytes();
                        message.extend_from_slice(content_bytes);
                        let _ = write_tx.send(WriteCommand::Send(message)).await;
                    });
                } else {
                    // No handler registered - send method not found error
                    let response = serde_json::json!({
                        "jsonrpc": "2.0",
                        "id": id_str,
                        "error": {
                            "code": -32601,
                            "message": format!("Method not found: {}", method)
                        }
                    });
                    let content = serde_json::to_string(&response).unwrap();
                    let content_bytes = content.as_bytes();
                    let header =
                        format!("Content-Length: {}\r\n\r\n", content_bytes.len());
                    let mut message = header.into_bytes();
                    message.extend_from_slice(content_bytes);
                    let _ = write_tx.send(WriteCommand::Send(message)).await;
                }
            } else if has_method && !has_id {
                // Notification from the server
                let method = raw.method.unwrap();
                let params = raw.params.unwrap_or(Value::Object(Default::default()));

                let handler = notification_handler.lock().await;
                if let Some(ref h) = *handler {
                    h(method, params);
                }
            }
        }
    }

    /// Reads the Content-Length header and the blank line separator.
    /// Returns the content length value.
    async fn read_content_length<R: tokio::io::AsyncBufRead + Unpin>(
        reader: &mut R,
    ) -> Result<usize, CopilotError> {
        let mut content_length: Option<usize> = None;

        loop {
            let mut line = String::new();
            let bytes_read = reader
                .read_line(&mut line)
                .await
                .map_err(|e| CopilotError::Io(e.to_string()))?;

            if bytes_read == 0 {
                return Err(CopilotError::ConnectionClosed);
            }

            let trimmed = line.trim();
            if trimmed.is_empty() {
                // Empty line separates header from body
                if let Some(len) = content_length {
                    return Ok(len);
                }
                // Keep reading if we haven't found Content-Length yet
                continue;
            }

            if let Some(value) = trimmed.strip_prefix("Content-Length:") {
                let len: usize = value
                    .trim()
                    .parse()
                    .map_err(|_| CopilotError::Protocol("Invalid Content-Length".to_string()))?;
                content_length = Some(len);
            }
            // Ignore other headers (e.g., Content-Type)
        }
    }
}

impl Drop for JsonRpcClient {
    fn drop(&mut self) {
        if let Some(handle) = self.reader_handle.take() {
            handle.abort();
        }
        // Writer will shut down when write_tx is dropped
    }
}
