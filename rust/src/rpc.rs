//! JSON-RPC request/response types and typed namespace builders.
//!
//! Protocol types are auto-generated from the Copilot CLI protocol schemas.
//! Handwritten helpers extend them without changing existing construction patterns.
//! This module is the stable public access point — the underlying
//! crate-private modules where the types are defined are an
//! implementation detail whose layout may change.
//!
//! Use the [`crate::Client::rpc`] and [`crate::session::Session::rpc`] helper
//! methods to obtain a typed view over the protocol surface.

pub use crate::generated::api_types::*;
pub use crate::generated::rpc::*;
pub use crate::types::{AutopilotObjectiveGetStateDetails, ExtensionListDetails};

impl SessionRpcExtensions<'_> {
    /// List session extensions with executable module paths and process log paths.
    ///
    /// Uses the existing `session.extensions.list` method. Unlike [`Self::list`],
    /// the result retains the optional `modulePath` and `logPath` fields.
    ///
    /// **Experimental.** May change or be removed in future releases.
    ///
    /// # Example
    ///
    /// This example uses a framed JSON-RPC fixture instead of a runtime process.
    ///
    /// ```
    /// # use copilot_sdk::{Client, ClientMode, CreateSessionOptions};
    /// # use serde_json::{Value, json};
    /// # use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader, duplex};
    /// # #[tokio::main(flavor = "current_thread")]
    /// # async fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// # let (transport, peer) = duplex(8192);
    /// # let (reader, writer) = tokio::io::split(transport);
    /// # let client = Client::from_streams(ClientMode::CodingAgent, reader, writer, std::env::current_dir()?)?;
    /// # let fixture = tokio::spawn(async move {
    /// #     let (reader, mut writer) = tokio::io::split(peer);
    /// #     let mut reader = BufReader::new(reader);
    /// #     for method in ["session.create", "session.extensions.list", "session.extensions.reconcile", "session.metadata.snapshot", "session.autopilotObjective.getState", "session.detach"] {
    /// #         let mut header = String::new();
    /// #         reader.read_line(&mut header).await.unwrap();
    /// #         let length: usize = header.trim().strip_prefix("Content-Length: ").unwrap().parse().unwrap();
    /// #         reader.read_line(&mut String::new()).await.unwrap();
    /// #         let mut body = vec![0; length];
    /// #         reader.read_exact(&mut body).await.unwrap();
    /// #         let request: Value = serde_json::from_slice(&body).unwrap();
    /// #         assert_eq!(request["method"], method);
    /// #         let result = match method {
    /// #             "session.create" => json!({"sessionId": request["params"]["sessionId"]}),
    /// #             "session.detach" => json!({"success": true}),
    /// #             "session.metadata.snapshot" => json!({
    /// #                 "sessionId": request["params"]["sessionId"], "runtimeInstanceId": "runtime-generation",
    /// #                 "alreadyInUse": false, "currentMode": "interactive", "isRemote": false,
    /// #                 "modifiedTime": "2026-11-21T00:00:00Z", "startTime": "2026-11-21T00:00:00Z",
    /// #                 "workingDirectory": "/workspace", "sessionLimits": null, "workspace": null, "workspacePath": null
    /// #             }),
    /// #             "session.autopilotObjective.getState" => json!({"state": {
    /// #                 "id": 1, "objective": "Verify details", "status": "active",
    /// #                 "turnCount": 2, "creditCountNanoAiu": "0",
    /// #                 "createdAt": "2026-11-21T00:00:00Z", "resumeCount": 3
    /// #             }}),
    /// #             _ => json!({"configured": true, "extensions": [{
    /// #                 "id": "project:example", "name": "example", "source": "project",
    /// #                 "status": "running", "modulePath": "/workspace/extension.mjs",
    /// #                 "logPath": "/workspace/extension.log"
    /// #             }]}),
    /// #         };
    /// #         let body = json!({"jsonrpc": "2.0", "id": request["id"], "result": result}).to_string();
    /// #         writer.write_all(format!("Content-Length: {}\r\n\r\n{body}", body.len()).as_bytes()).await.unwrap();
    /// #     }
    /// #     (reader, writer)
    /// # });
    /// # let session = client.create_session(CreateSessionOptions::default()).await?;
    /// let inventory = session.rpc().extensions().list_details().await?;
    /// let extension = &inventory.extensions[0];
    /// assert_eq!(extension.module_path.as_deref(), Some("/workspace/extension.mjs"));
    /// assert_eq!(extension.log_path.as_deref(), Some("/workspace/extension.log"));
    /// println!("Module: {:?}; log: {:?}", extension.module_path, extension.log_path);
    ///
    /// let reconciled = session.rpc().extensions().reconcile_details().await?;
    /// assert_eq!(reconciled.extensions[0].module_path, extension.module_path);
    ///
    /// let metadata = session.rpc().metadata().snapshot_details().await?;
    /// assert_eq!(metadata.runtime_instance_id.as_deref(), Some("runtime-generation"));
    /// let objective = session.rpc().autopilot_objective().get_state_details().await?.state.unwrap();
    /// assert_eq!(objective.created_at.as_deref(), Some("2026-11-21T00:00:00Z"));
    /// assert_eq!(objective.resume_count, Some(3));
    /// println!("Runtime: {:?}; objective resumes: {:?}", metadata.runtime_instance_id, objective.resume_count);
    /// # session.disconnect().await?;
    /// # let _peer = fixture.await?;
    /// # client.stop().await?;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn list_details(&self) -> crate::Result<ExtensionListDetails> {
        self.session
            .client()
            .call_typed(
                rpc_methods::SESSION_EXTENSIONS_LIST,
                Some(serde_json::json!({ "sessionId": self.session.id() })),
            )
            .await
    }

    /// Reconcile session extensions and retain executable module paths and process log paths.
    ///
    /// Uses the existing `session.extensions.reconcile` method, with the same
    /// host requirement, lifecycle guarantees, and errors as [`Self::reconcile`].
    /// Check each extension's status; success does not mean every extension runs.
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub async fn reconcile_details(&self) -> crate::Result<ExtensionListDetails> {
        self.session
            .client()
            .call_typed(
                rpc_methods::SESSION_EXTENSIONS_RECONCILE,
                Some(serde_json::json!({ "sessionId": self.session.id() })),
            )
            .await
    }
}

/// Convert a discovery plan's provider to credential-free saved settings.
///
/// Use the result as `provider` and the plan's unchanged model as `model` in the
/// `providers().config().add_model()` request.
/// Unknown provider families return an [`crate::ErrorKind::InvalidConfig`] error.
pub fn to_saved_provider_configuration(
    provider: NamedProviderConfig,
) -> crate::errors::Result<SavedProviderConfigurationProviderInput> {
    let provider_type = match provider.r#type {
        Some(ProviderConfigType::Openai) => Some(SavedProviderConfigurationType::Openai),
        Some(ProviderConfigType::Azure) => Some(SavedProviderConfigurationType::Azure),
        Some(ProviderConfigType::Anthropic) => Some(SavedProviderConfigurationType::Anthropic),
        Some(ProviderConfigType::Google) => Some(SavedProviderConfigurationType::Google),
        Some(ProviderConfigType::Winml) => Some(SavedProviderConfigurationType::Winml),
        Some(ProviderConfigType::Unknown) => {
            return Err(crate::Error::with_message(
                crate::ErrorKind::InvalidConfig,
                "Cannot save an unknown discovered provider type",
            ));
        }
        None => None,
    };
    Ok(SavedProviderConfigurationProviderInput {
        name: provider.name,
        r#type: provider_type,
        wire_api: provider.wire_api,
        transport: provider.transport,
        base_url: provider.base_url,
        local_model: provider.local_model,
        azure: provider.azure,
        model_provider: provider.model_provider,
    })
}

impl SendRequest {
    /// Set the message provenance without changing other request options.
    ///
    /// When this is not called, the source field is omitted by default.
    pub fn with_source(mut self, source: crate::MessageSource) -> Self {
        self.source = Some(source.to_string());
        self
    }

    /// Mark a message an agent wrote that the host delivers in the user role,
    /// such as the kickoff of a session another agent created.
    ///
    /// Only automatic permission decisions read the mark: they treat the text
    /// as another agent's message, never as the user's own words. Passing
    /// `false` omits the field, as when this is not called.
    pub fn with_agent_authored(mut self, agent_authored: bool) -> Self {
        self.agent_authored = agent_authored.then_some(true);
        self
    }
}

#[cfg(test)]
mod tests;
