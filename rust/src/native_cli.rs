//! Repository-private construction for the native CLI, not a published SDK mode.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Weak};

use parking_lot::Mutex;
use serde_json::Value;
use tokio::sync::mpsc;
use tracing::Instrument;

use crate::{
    Client, ClientOptions, Error, ErrorKind, InProcessProtocolConnection, RuntimeConnection,
};

/// A runtime notification from the CLI's existing SDK connection.
pub struct Notification {
    /// The exact wire method.
    pub method: String,
    /// The wire parameters, or null when the frame omits them.
    pub params: Value,
}

/// An ordered stream that releases its registration when dropped.
pub struct NotificationSubscription {
    receiver: mpsc::UnboundedReceiver<Notification>,
    registry: Weak<NotificationSubscriptions>,
    id: u64,
}

impl NotificationSubscription {
    /// Receive the next matching frame, or None after connection closure.
    ///
    /// Buffered frames drain before closure. Dropping this future does not
    /// consume a frame or remove the subscription.
    pub async fn next(&mut self) -> Option<Notification> {
        self.receiver.recv().await
    }
}

impl Drop for NotificationSubscription {
    fn drop(&mut self) {
        if let Some(registry) = self.registry.upgrade() {
            registry.state.lock().subscribers.remove(&self.id);
        }
    }
}

struct Subscriber {
    methods: HashSet<String>,
    sender: mpsc::UnboundedSender<Notification>,
}

#[derive(Default)]
struct SubscriptionState {
    next_id: u64,
    closed: bool,
    subscribers: HashMap<u64, Subscriber>,
}

#[derive(Default)]
pub(crate) struct NotificationSubscriptions {
    state: Mutex<SubscriptionState>,
}

impl NotificationSubscriptions {
    pub(crate) fn subscribe(
        self: &Arc<Self>,
        methods: &[&str],
    ) -> Result<NotificationSubscription, Error> {
        let mut state = self.state.lock();
        if state.closed {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "Notification subscriptions require an open connection",
            ));
        }
        let id = state.next_id;
        state.next_id += 1;
        let (sender, receiver) = mpsc::unbounded_channel();
        state.subscribers.insert(
            id,
            Subscriber {
                methods: methods.iter().map(|method| (*method).to_owned()).collect(),
                sender,
            },
        );
        Ok(NotificationSubscription {
            receiver,
            registry: Arc::downgrade(self),
            id,
        })
    }

    pub(crate) fn dispatch(&self, notification: &crate::jsonrpc::JsonRpcNotification) {
        for subscriber in self.state.lock().subscribers.values() {
            if subscriber.methods.contains(&notification.method) {
                let _ = subscriber.sender.send(Notification {
                    method: notification.method.clone(),
                    params: notification.params.clone().unwrap_or(Value::Null),
                });
            }
        }
    }

    pub(crate) fn close(&self) {
        let mut state = self.state.lock();
        state.closed = true;
        state.subscribers.clear();
    }
}

/// Subscribe to exact wire methods in frame order without retaining the client.
///
/// Each subscription has its own unbounded queue. The read loop only enqueues
/// frames; it never runs subscriber work. A closed connection rejects registration.
///
/// ```no_run
/// # async fn example(client: &copilot_sdk::Client) -> Result<(), copilot_sdk::Error> {
/// use copilot_sdk::native_cli::subscribe_notifications;
/// let mut notifications = subscribe_notifications(
///     client,
///     &["remoteHosts.stateChanged", "remoteHosts.sessionChanged"],
/// )?;
/// while let Some(notification) = notifications.next().await {
///     println!("{}: {}", notification.method, notification.params);
/// }
/// # Ok(())
/// # }
/// ```
pub fn subscribe_notifications(
    client: &Client,
    methods: &[&str],
) -> Result<NotificationSubscription, Error> {
    client.inner.rpc.subscribe_notifications(methods)
}

mod session_events;
pub use session_events::{SessionEvents, subscribe_session};

/// Install an owner-scoped timing consumer during the native CLI's connection setup.
///
/// The consumer performs only in-memory initialization; release admission and endpoint
/// resolution remain asynchronous. Ordinary SDK builds do not compile this entry point.
pub fn initialize_session_timing(
    client: &Client,
    config: &serde_json::Value,
) -> Result<bool, Error> {
    let _span = tracing::debug_span!("session.timing.cli_timing_initialize").entered();
    let config = serde_json::to_vec(config)
        .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
    let host = client.inner.ffi_host.lock();
    let host = host.as_ref().ok_or_else(|| {
        Error::with_message(
            ErrorKind::InvalidConfig,
            "CLI timing initialization requires an active in-process connection",
        )
    })?;
    host.initialize_session_timing(&config)
}

/// Start the native CLI's connection after normal SDK setup.
///
/// The caller checks image identity before the SDK captures CLI authority.
/// Ordinary SDK builds do not compile this entry point.
pub async fn start<E>(
    options: ClientOptions,
    validate_image: impl FnOnce(&Client) -> Result<(), E>,
) -> Result<Client, E>
where
    E: From<Error>,
{
    if !matches!(
        &options.runtime,
        Some(RuntimeConnection::InProcess(_) | RuntimeConnection::AttachedInProcess(_))
    ) {
        return Err(Error::with_message(
            ErrorKind::InvalidConfig,
            "native CLI construction requires the in-process transport",
        )
        .into());
    }
    let client = Client::start(options).await.map_err(E::from)?;
    finish_start(client, validate_image)
}

/// Open the original CLI stdio stream with its live process principal.
///
/// Attached clients and ordinary SDK peers cannot use this constructor.
/// The returned stream owns only its connection, not its engine or CLI host.
///
/// ```no_run
/// # async fn example(client: &copilot_sdk::Client) -> Result<(), copilot_sdk::Error> {
/// let stdio = copilot_sdk::native_cli::open_stdio_connection(client).await?;
/// stdio.close().await?;
/// # Ok(())
/// # }
/// ```
pub async fn open_stdio_connection(client: &Client) -> Result<InProcessProtocolConnection, Error> {
    let host = client.inner.ffi_host.lock().clone().ok_or_else(|| {
        Error::with_message(
            ErrorKind::InvalidConfig,
            "original stdio requires an active in-process CLI client",
        )
    })?;
    crate::ffi::open_original_stdio_connection(host)
        .instrument(tracing::debug_span!("session.timing.cli_stdio_admission"))
        .await
}

fn finish_start<E>(
    client: Client,
    validate_image: impl FnOnce(&Client) -> Result<(), E>,
) -> Result<Client, E>
where
    E: From<Error>,
{
    let _span = tracing::debug_span!("session.timing.cli_host_admission").entered();
    let admission = validate_image(&client).and_then(|()| {
        let host = client.inner.ffi_host.lock();
        let host = host.as_ref().ok_or_else(|| {
            E::from(Error::with_message(
                ErrorKind::InvalidConfig,
                "native CLI construction requires an active in-process connection",
            ))
        })?;
        host.admit_cli_host().map_err(E::from)
    });
    if let Err(error) = admission {
        client.force_stop();
        return Err(error);
    }
    Ok(client)
}

#[cfg(test)]
mod tests;
