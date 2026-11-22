//! Typed, same-checkout calls for the CLI host.
//!
//! These accessors grant no authority. Only the native CLI host can provision its connection.
//!
//! Published SDK builds have no privileged startup helper, including with `local-runtime`:
//!
#![cfg_attr(
    not(copilot_cli_host),
    doc = "```compile_fail\nuse copilot_sdk::native_cli::start;\n```"
)]

pub use crate::generated::cli_api_types::*;
use crate::rpc::SessionOpenOptions;
use crate::session::PreparedSession;
use crate::{Client, CreateSessionOptions, Error, ResumeSessionOptions};
use std::collections::HashMap;

use serde_json::Value;
use tokio::sync::oneshot;

#[derive(Default)]
pub(crate) struct PendingSendAdmissions(
    parking_lot::Mutex<HashMap<String, (crate::SessionId, oneshot::Sender<()>)>>,
);

struct SendAdmissionRegistration<'a> {
    pending: &'a PendingSendAdmissions,
    id: String,
}

impl Drop for SendAdmissionRegistration<'_> {
    fn drop(&mut self) {
        self.pending.0.lock().remove(&self.id);
    }
}

pub(crate) fn register_send_admission_handler(client: &Client) -> Result<(), Error> {
    client.register_request_handler("embedder.sendAdmission", |params, client| async move {
        let request: crate::generated::api_types::SessionSendAdmissionRequest =
            serde_json::from_value(params)?;
        let mut pending = client.inner.send_admissions.0.lock();
        if !pending
            .get(&request.admission_id)
            .is_some_and(|(session_id, _)| session_id == request.session_id)
        {
            return Err(Error::with_message(
                crate::ErrorKind::Rpc { code: -32602 },
                "Unknown or mismatched send admission callback",
            ));
        }
        let (_, sender) = pending
            .remove(&request.admission_id)
            .expect("the matching admission remains under the same lock");
        sender.send(()).map_err(|_| {
            Error::with_message(
                crate::ErrorKind::Rpc { code: -32602 },
                "The send admission invocation is no longer active",
            )
        })?;
        Ok(Value::Null)
    })
}

/// Send on the original CLI connection with separate admission and completion.
///
/// The callback runs once after this invocation enters the canonical queue.
/// `wait` retains its existing completion and event-tail semantics.
/// Dropping this future releases its callback registration.
pub async fn send_with_admission<F, Fut>(
    client: &Client,
    session_id: &crate::SessionId,
    mut params: Value,
    on_admission: F,
) -> Result<Value, Error>
where
    F: FnOnce() -> Fut + Send,
    Fut: Future<Output = Result<(), Error>> + Send,
{
    let params_object = params.as_object_mut().ok_or_else(|| {
        Error::with_message(
            crate::ErrorKind::InvalidConfig,
            "Send parameters must be an object",
        )
    })?;
    let id = uuid::Uuid::new_v4().to_string();
    params_object.insert("sessionId".to_owned(), session_id.as_str().into());
    params_object.insert("sendAdmissionId".to_owned(), id.clone().into());
    let (sender, receiver) = oneshot::channel();
    let pending = &client.inner.send_admissions;
    pending
        .0
        .lock()
        .insert(id.clone(), (session_id.clone(), sender));
    let _registration = SendAdmissionRegistration { pending, id };
    wait_for_send_admission(
        receiver,
        client.call("session.send", Some(params)),
        client.inner.force_stop_requested.cancelled(),
        on_admission,
    )
    .await
}

async fn wait_for_send_admission<F, Fut>(
    mut receiver: oneshot::Receiver<()>,
    completion: impl Future<Output = Result<Value, Error>>,
    stopped: impl Future<Output = ()>,
    on_admission: F,
) -> Result<Value, Error>
where
    F: FnOnce() -> Fut + Send,
    Fut: Future<Output = Result<(), Error>> + Send,
{
    tokio::pin!(completion);
    tokio::select! {
        biased;
        admitted = &mut receiver => {
            admitted.map_err(|_| Error::with_message(
                crate::ErrorKind::Protocol(crate::ProtocolErrorKind::RequestCancelled),
                "Send admission closed before acknowledgment",
            ))?;
            on_admission().await?;
            completion.await
        }
        result = &mut completion => {
            // Admission can arrive after its poll but before completion becomes ready.
            if receiver.try_recv().is_ok() {
                on_admission().await?;
            }
            result
        },
        () = stopped => Err(Error::with_message(
            crate::ErrorKind::Protocol(crate::ProtocolErrorKind::RequestCancelled),
            "The SDK owner stopped before send admission",
        )),
    }
}

/// Typed internal calls on the CLI's existing connection. This grants no authority.
pub fn rpc(client: &Client) -> crate::generated::cli_rpc::ClientRpc<'_> {
    crate::generated::cli_rpc::ClientRpc { client }
}

/// Observe transport closure without acquiring control of the SDK connection.
///
/// The returned child token cannot cancel the underlying transport.
pub fn connection_closed_token(client: &Client) -> tokio_util::sync::CancellationToken {
    client.inner.rpc.connection_closed_token().child_token()
}

/// Prepare an engine-owned CLI session with the SDK's callback registration.
///
/// `options` carries shared construction options. `callbacks` supplies
/// live SDK handlers, providers, and event-buffer configuration. CLI options take
/// precedence over overlapping serialized configuration, but cannot replace the
/// SDK's callback declarations or the registered session identity.
///
/// Subscribe to the returned handle before calling [`PreparedSession::start`].
/// This function does not create another engine or register a host facade.
/// The configured engine supplies server defaults; these options do not replace
/// its feature-flag audience, built-in plugins, or process configuration.
pub fn prepare_session(
    client: &Client,
    options: SessionOpenOptions,
    callbacks: CreateSessionOptions,
) -> Result<PreparedSession, Error> {
    client.prepare_cli_session(serde_json::to_value(options)?, callbacks, true)
}

/// Prepare a CLI resume on the original engine with SDK-owned callbacks.
///
/// The returned handle has the same subscription and cancellation semantics as
/// [`Client::prepare_resume_session`]. The runtime owns history replay,
/// persistence, locks, hooks, and teardown.
pub fn prepare_resume_session(
    client: &Client,
    options: SessionOpenOptions,
    callbacks: ResumeSessionOptions,
) -> Result<PreparedSession, Error> {
    client.prepare_cli_resume_session(serde_json::to_value(options)?, callbacks, true, false)
}

/// Prepare a CLI create with its explicit start-event policy.
///
/// Serialized host options retain fields outside the public SDK option subset.
pub fn prepare_session_open(
    client: &Client,
    options: impl serde::Serialize,
    callbacks: CreateSessionOptions,
    emit_start: bool,
) -> Result<PreparedSession, Error> {
    client.prepare_cli_session(serde_json::to_value(options)?, callbacks, emit_start)
}

/// Prepare a CLI resume with its event and workspace-writeback policies.
///
/// Serialized host options retain fields outside the public SDK option subset.
pub fn prepare_resume_open(
    client: &Client,
    options: impl serde::Serialize,
    callbacks: ResumeSessionOptions,
    emit_resume: bool,
    suppress_workspace_metadata_writeback: bool,
) -> Result<PreparedSession, Error> {
    client.prepare_cli_resume_session(
        serde_json::to_value(options)?,
        callbacks,
        emit_resume,
        suppress_workspace_metadata_writeback,
    )
}

#[cfg(test)]
mod tests;
