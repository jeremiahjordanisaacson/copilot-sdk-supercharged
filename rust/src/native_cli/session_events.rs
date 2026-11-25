/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

use crate::router::{RegistrationState, RegistrationToken, RoutedEvent};
use crate::types::{SessionEvent, SessionId};
use crate::{Client, WeakClientInner};
use std::sync::Arc;
use tokio::sync::mpsc;

/// An observe-only registration for a session opened through the generated RPC tree.
///
/// The native CLI's protocol consumers retain their own event handlers. They use
/// `sessions.open` to preserve the host construction options rather than the
/// convenience session builders. This registration uses the SDK's existing
/// notification router, including its startup buffering and generation ownership.
///
/// Drop removes only this registration. It does not close the runtime session.
/// The consumer must drain promptly and provide its own bounded output queue.
pub struct SessionEvents {
    client: WeakClientInner,
    session_id: SessionId,
    token: RegistrationToken,
    state: Arc<RegistrationState>,
    notifications: mpsc::UnboundedReceiver<RoutedEvent>,
}

/// Subscribe before opening a session with its caller-assigned ID.
///
/// This API is repository-private host plumbing, not a second session lifecycle.
/// The caller still owns `sessions.open` and `sessions.close`. It must not
/// register SDK callback handlers for this observe-only registration.
pub fn subscribe_session(client: &Client, session_id: impl Into<SessionId>) -> SessionEvents {
    let session_id = session_id.into();
    let registration = client.register_session(&session_id);
    // An observe-only owner has no JSON-RPC callbacks. The router answers an
    // unexpected callback instead of leaving the runtime waiting for a response.
    drop(registration.channels.requests);
    SessionEvents {
        client: WeakClientInner::downgrade(&client.inner),
        session_id,
        token: registration.token,
        state: registration.state,
        notifications: registration.channels.notifications,
    }
}

impl SessionEvents {
    /// Receive the next routed runtime event, or `None` after disconnection.
    ///
    /// A subscription created after connection shutdown ends immediately.
    /// Cancellation leaves the next event in the SDK queue.
    pub async fn recv(&mut self) -> Option<SessionEvent> {
        self.notifications
            .recv()
            .await
            .map(|routed| routed.notification.event)
    }
}

impl Drop for SessionEvents {
    fn drop(&mut self) {
        self.state.cancel();
        if let Some(inner) = self.client.upgrade() {
            Client::from_inner(inner).unregister_session_owned(&self.session_id, self.token);
        }
    }
}
