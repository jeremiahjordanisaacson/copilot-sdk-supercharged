// Copyright (c) Microsoft Corporation. All rights reserved.

//! # Copilot SDK for Rust
//!
//! A Rust client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.
//!
//! This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio
//! or TCP, with Content-Length header framing (LSP protocol style).
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use copilot_sdk::*;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), CopilotError> {
//!     // Create and start a client
//!     let client = CopilotClient::new(CopilotClientOptions {
//!         cli_path: Some("/path/to/copilot-cli".to_string()),
//!         ..Default::default()
//!     });
//!     client.start().await?;
//!
//!     // Create a session
//!     let session = client.create_session(SessionConfig::default()).await?;
//!
//!     // Send a message and wait for completion
//!     let response = session.send_and_wait(
//!         MessageOptions {
//!             prompt: "What is 2 + 2?".to_string(),
//!             attachments: None,
//!             mode: None,
//!         },
//!         None,
//!     ).await?;
//!
//!     if let Some(event) = response {
//!         if let Some(content) = event.assistant_message_content() {
//!             println!("Assistant: {}", content);
//!         }
//!     }
//!
//!     // Clean up
//!     session.destroy().await?;
//!     client.stop().await?;
//!     Ok(())
//! }
//! ```

pub mod client;
pub mod jsonrpc;
pub mod sdk_protocol_version;
pub mod session;
pub mod types;

// Re-export main types at crate root for convenience
pub use client::CopilotClient;
pub use session::{
    CopilotSession, HooksHandlerFn, PermissionHandlerFn, Subscription, ToolHandler,
    UserInputHandlerFn,
};
pub use types::*;

/// Error types for the Copilot SDK.
#[derive(Debug, thiserror::Error)]
pub enum CopilotError {
    /// JSON-RPC error response from the server.
    #[error("JSON-RPC error {code}: {message}")]
    JsonRpc {
        code: i32,
        message: String,
        data: Option<serde_json::Value>,
    },

    /// Serialization/deserialization error.
    #[error("Serialization error: {0}")]
    Serialization(String),

    /// Connection closed unexpectedly.
    #[error("Connection closed")]
    ConnectionClosed,

    /// Request timed out.
    #[error("Request timed out after {0}ms")]
    Timeout(u64),

    /// I/O error.
    #[error("I/O error: {0}")]
    Io(String),

    /// Protocol error (malformed messages, etc.).
    #[error("Protocol error: {0}")]
    Protocol(String),

    /// Client not connected.
    #[error("Client not connected. Call start() first.")]
    NotConnected,

    /// Configuration error.
    #[error("Configuration error: {0}")]
    Configuration(String),

    /// Failed to spawn CLI process.
    #[error("Process spawn error: {0}")]
    ProcessSpawn(String),

    /// Connection failed.
    #[error("Connection error: {0}")]
    Connection(String),

    /// Protocol version mismatch.
    #[error("SDK protocol version mismatch: expected {expected}, server reports {actual:?}")]
    ProtocolMismatch {
        expected: u32,
        actual: Option<u32>,
    },

    /// Session-related error.
    #[error("Session error: {0}")]
    SessionError(String),

    /// No handler registered for a request.
    #[error("No handler: {0}")]
    NoHandler(String),
}
