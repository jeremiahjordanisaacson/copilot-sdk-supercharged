//! Local discovery publication through the original in-process SDK host.

use std::ffi::c_void;
use std::sync::atomic::{AtomicU32, Ordering};

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::ffi::DiscoveryPublisherRequestFn;
use crate::{Client, Error, ErrorKind};

/// The local discovery entry format.
#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiscoveryRegistryKind {
    /// Terminal UI server discovery.
    UiServer,
    /// Managed SDK server discovery.
    ManagedServer,
}

/// Process facts and registry paths for a local discovery publisher.
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoveryPublisherConfig {
    /// The discovery entry format.
    pub kind: DiscoveryRegistryKind,
    /// The bound localhost interface.
    pub host: String,
    /// The local listener's connection credential, when configured.
    pub token: Option<String>,
    /// The process that owns the listener.
    pub pid: u64,
    /// The host application version.
    pub copilot_version: String,
    /// The explicit configuration home, when configured.
    pub copilot_home: Option<String>,
    /// The user's home directory.
    pub home: String,
}

/// A runtime-owned local discovery publisher.
///
/// Explicitly stop this handle before client shutdown to observe removal errors.
/// Dropping the handle also requests removal, without waiting for completion.
pub struct DiscoveryPublisher {
    client: Client,
    server_id: u32,
    publisher_id: AtomicU32,
    request: DiscoveryPublisherRequestFn,
}

impl DiscoveryPublisher {
    pub(crate) fn create(
        client: Client,
        configuration: DiscoveryPublisherConfig,
    ) -> Result<Self, Error> {
        let host = client.in_process_host()?;
        let (server_id, create, request) = host.discovery_publisher_exports()?;
        let configuration = serde_json::to_vec(&configuration)?;
        let publisher_id =
            unsafe { create(server_id, configuration.as_ptr(), configuration.len()) };
        if publisher_id == 0 {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "the runtime refused discovery publisher configuration",
            ));
        }
        Ok(Self {
            client,
            server_id,
            publisher_id: AtomicU32::new(publisher_id),
            request,
        })
    }

    /// Set the bound listener port and process start timestamp.
    pub async fn start(&self, port: u32, started_at: String) -> Result<(), Error> {
        self.invoke(json!({
            "operation": "start",
            "port": port,
            "startedAt": started_at,
        }))
        .await
    }

    async fn snapshot(&self, session_id: &str) -> Result<Value, Error> {
        let metadata: crate::rpc::SessionMetadataSnapshot = self
            .client
            .call_typed(
                "session.metadata.snapshot",
                Some(json!({ "sessionId": session_id })),
            )
            .await?;
        let workspace_string =
            |field: fn(&crate::rpc::SessionMetadataSnapshotWorkspace) -> &Option<String>| {
                metadata
                    .workspace
                    .as_ref()
                    .and_then(|workspace| field(workspace).as_ref())
                    .filter(|value| !value.is_empty())
                    .cloned()
            };
        let session_cwd =
            (!metadata.working_directory.is_empty()).then_some(metadata.working_directory.as_str());
        Ok(json!({
            "sessionId": session_id,
            "sessionName": workspace_string(|workspace| &workspace.name),
            "sessionCwd": session_cwd,
            "workspaceCwd": if session_cwd.is_none() {
                workspace_string(|workspace| &workspace.cwd)
            } else {
                None
            },
            "branch": workspace_string(|workspace| &workspace.branch),
            "model": metadata.selected_model.filter(|model| !model.is_empty()),
        }))
    }

    /// Publish current canonical metadata for a public SDK session.
    pub async fn set_session(&self, generation: u32, session_id: &str) -> Result<(), Error> {
        self.invoke(json!({
            "operation": "setSession",
            "generation": generation,
            "session": self.snapshot(session_id).await?,
        }))
        .await
    }

    /// Publish canonical session metadata and status in one registry write.
    pub async fn set_session_and_status(
        &self,
        generation: u32,
        session_id: &str,
        status: Option<String>,
    ) -> Result<(), Error> {
        self.invoke(json!({
            "operation": "setSessionAndStatus",
            "generation": generation,
            "session": self.snapshot(session_id).await?,
            "status": status,
        }))
        .await
    }

    /// Refresh the existing entry's heartbeat through its runtime owner.
    pub async fn heartbeat(&self) -> Result<(), Error> {
        self.invoke(json!({ "operation": "heartbeat" })).await
    }

    /// Remove the entry and release its runtime publisher.
    pub async fn stop(&self) -> Result<(), Error> {
        let id = self.publisher_id.swap(0, Ordering::AcqRel);
        if id == 0 {
            return Ok(());
        }
        self.request(id, json!({ "operation": "stop" })).await
    }

    async fn invoke(&self, operation: Value) -> Result<(), Error> {
        let id = self.publisher_id.load(Ordering::Acquire);
        if id == 0 {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "discovery publisher is closed",
            ));
        }
        self.request(id, operation).await
    }

    async fn request(&self, id: u32, operation: Value) -> Result<(), Error> {
        unsafe extern "C" fn complete(data: *mut c_void, bytes: *const u8, len: usize) {
            let sender =
                unsafe { Box::from_raw(data.cast::<tokio::sync::oneshot::Sender<Vec<u8>>>()) };
            let payload = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
            let _ = sender.send(payload);
        }
        let operation = serde_json::to_vec(&operation)?;
        let (sender, receiver) = tokio::sync::oneshot::channel::<Vec<u8>>();
        let sender = Box::into_raw(Box::new(sender));
        let accepted = unsafe {
            (self.request)(
                self.server_id,
                id,
                operation.as_ptr(),
                operation.len(),
                complete,
                sender.cast(),
            )
        };
        if !accepted {
            drop(unsafe { Box::from_raw(sender) });
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "the runtime refused the discovery publisher operation",
            ));
        }
        let response: Value =
            serde_json::from_slice(&receiver.await.map_err(|error| {
                Error::with_message(ErrorKind::InvalidConfig, error.to_string())
            })?)?;
        if response["ok"] == true {
            return Ok(());
        }
        let error: crate::jsonrpc::JsonRpcError =
            serde_json::from_value(response["error"].clone())?;
        Err(Error::from_rpc(error.code, error.message, error.data))
    }
}

impl Drop for DiscoveryPublisher {
    fn drop(&mut self) {
        unsafe extern "C" fn complete(_data: *mut c_void, _bytes: *const u8, _len: usize) {}
        let id = self.publisher_id.swap(0, Ordering::AcqRel);
        if id == 0 {
            return;
        }
        let operation = br#"{"operation":"stop"}"#;
        unsafe {
            (self.request)(
                self.server_id,
                id,
                operation.as_ptr(),
                operation.len(),
                complete,
                std::ptr::null_mut(),
            );
        }
    }
}
