//! In-process FFI transport: hosts the Copilot runtime by loading its native
//! library and speaking JSON-RPC over its C ABI,
//! instead of spawning a CLI child process and communicating over stdio/TCP.
//!
//! The runtime's `host_start` export constructs the Rust server synchronously in
//! this process. JSON-RPC frames are pumped across the ABI: writes go to
//! `connection_write`; inbound frames arrive on a native callback that feeds an
//! async reader. The framing is unchanged — the same LSP `Content-Length:`
//! frames the stdio transport uses.

use std::collections::HashMap;
use std::ffi::c_void;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU32, Ordering};
use std::sync::{Arc, OnceLock, Weak};
use std::task::{Context, Poll, Waker};

use libloading::Library;
use tokio::io::{AsyncRead, ReadBuf};
use tokio::sync::mpsc;
use tracing::{debug, warn};

use crate::{Error, ErrorKind};

type OutboundCallback = unsafe extern "C" fn(*mut c_void, *const u8, usize);
type HostStartFn = unsafe extern "C" fn(*const u8, usize, *const u8, usize) -> u32;
type AuthOwnerCallback = unsafe extern "C" fn(*mut c_void, u32, u32) -> bool;
type HostStartWithAuthOwnerFn =
    unsafe extern "C" fn(*const u8, usize, *const u8, usize, AuthOwnerCallback, *mut c_void) -> u32;
type HostShutdownFn = unsafe extern "C" fn(u32) -> bool;
type ServerSessionControlFn = unsafe extern "C" fn(u32, *const u8, usize) -> i32;
type ServerHasSessionConnectionFn =
    unsafe extern "C" fn(u32, *const u8, usize, *const u8, usize) -> i32;
type ServerSetConnectionDefaultSessionFn = unsafe extern "C" fn(u32, u32, *const u8, usize) -> i32;
pub(crate) type DiscoveryPublisherCreateFn = unsafe extern "C" fn(u32, *const u8, usize) -> u32;
pub(crate) type DiscoveryPublisherRequestFn =
    unsafe extern "C" fn(u32, u32, *const u8, usize, OutboundCallback, *mut c_void) -> bool;
type ServerSetSdkSessionConfigFn = unsafe extern "C" fn(
    u32,
    *const u8,
    usize,
    bool,
    bool,
    *const u8,
    usize,
    *const u8,
    usize,
) -> bool;
#[allow(clippy::type_complexity)]
type ConnectionOpenFn = unsafe extern "C" fn(
    u32,
    OutboundCallback,
    *mut c_void,
    *const u8,
    usize,
    *const u8,
    usize,
    *const u8,
    usize,
) -> u32;
type ConnectionWriteFn = unsafe extern "C" fn(u32, *const u8, usize) -> bool;
type FileLoggingConnectionAdmitFn = unsafe extern "C" fn(u32, *const u8, usize) -> bool;
type ConnectionCloseFn = unsafe extern "C" fn(u32) -> bool;
type ConnectionCloseAndWaitFn = unsafe extern "C" fn(u32, u32) -> bool;
type ProtocolConnectionOpenFn =
    unsafe extern "C" fn(u32, OutboundCallback, *mut c_void, *const u8, usize) -> u32;
#[cfg(copilot_cli_host)]
type OriginalStdioOpenFn = unsafe extern "C" fn(u32, u32, OutboundCallback, *mut c_void) -> u32;
type ConnectionNotifyFn = unsafe extern "C" fn(u32, *const u8, usize, *const u8, usize) -> bool;
type ConnectionNotifyAfterResponseFn =
    unsafe extern "C" fn(u32, u32, *const u8, usize, *const u8, usize) -> bool;
type ConnectionRequestFn = unsafe extern "C" fn(
    u32,
    *const u8,
    usize,
    *const u8,
    usize,
    OutboundCallback,
    *mut c_void,
) -> bool;
type PluginHostRequestFn =
    unsafe extern "C" fn(u32, u32, u32, *const u8, usize, OutboundCallback, *mut c_void) -> bool;
type SessionConnectionOpenFn = unsafe extern "C" fn(
    u32,
    *const u8,
    usize,
    *const u8,
    usize,
    OutboundCallback,
    *mut c_void,
) -> u32;
type ConnectionSetUiThreadHandlerFn = unsafe extern "C" fn(u32, bool) -> bool;
type ConnectionRunUiActionFn = unsafe extern "C" fn(u32, u32) -> bool;

/// State handed to the native side as `user_data` so the outbound callback can
/// route inbound frames back to the reader.
struct CallbackState {
    tx: parking_lot::Mutex<Option<mpsc::UnboundedSender<Vec<u8>>>>,
    closing: AtomicBool,
}

extern "C" fn on_outbound(user_data: *mut c_void, bytes: *const u8, len: usize) {
    if user_data.is_null() {
        return;
    }
    let state = unsafe { &*(user_data as *const CallbackState) };
    if state.closing.load(Ordering::SeqCst) {
        return;
    }
    if bytes.is_null() && len == 0 {
        state.tx.lock().take();
        return;
    }
    if bytes.is_null() || len == 0 {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts(bytes, len) };
    if let Some(tx) = state.tx.lock().as_ref() {
        let _ = tx.send(slice.to_vec());
    }
}

/// Bound exports and connection lifecycle state, shared between the
/// synchronous frame sender and the owning [`Client`]. The cdylib itself is loaded
/// process-globally and never unloaded (see [`load_library`]), so this holds
/// only the bound fn pointers and connection state.
pub(crate) struct FfiShared {
    #[cfg(copilot_cli_host)]
    library: &'static Library,
    auth_owner: parking_lot::Mutex<Option<Arc<dyn crate::InProcessAuthOwner>>>,
    host_shutdown: HostShutdownFn,
    owns_host: bool,
    connection_write: ConnectionWriteFn,
    connection_close: ConnectionCloseFn,
    connection_close_and_wait: Option<ConnectionCloseAndWaitFn>,
    connection_set_ui_thread_handler: ConnectionSetUiThreadHandlerFn,
    connection_run_ui_action: ConnectionRunUiActionFn,
    server_id: AtomicU32,
    connection_id: AtomicU32,
    callback_state: AtomicPtr<CallbackState>,
    closed: AtomicBool,
    operation_lock: parking_lot::Mutex<()>,
    foreground_order: Arc<tokio::sync::Mutex<()>>,
    protocol_peers: parking_lot::Mutex<Option<Vec<Weak<FfiShared>>>>,
    cleanup_done: tokio::sync::watch::Sender<bool>,
    library_path: PathBuf,
}

// The raw fn pointers and the boxed callback state are safe to move across
// threads: the native side copies buffers synchronously and the callback only
// forwards to a thread-safe channel sender.
unsafe impl Send for FfiShared {}
unsafe impl Sync for FfiShared {}

impl FfiShared {
    #[cfg(copilot_cli_host)]
    pub(crate) fn initialize_session_timing(&self, config: &[u8]) -> Result<bool, Error> {
        let _operation = self.operation_lock.lock();
        let connection = self.connection_id().ok_or_else(|| {
            Error::with_message(
                ErrorKind::InvalidConfig,
                "the SDK's in-process connection is no longer active",
            )
        })?;
        let initialize = bind::<unsafe extern "C" fn(u32, *const u8, usize) -> i32>(
            self.library,
            b"copilot_runtime_cli_host_timing_initialize\0",
            &self.library_path,
        )?;
        // SAFETY: this SDK owns the live connection and retains both the image and input buffer.
        match unsafe { initialize(connection, config.as_ptr(), config.len()) } {
            1 => Ok(true),
            0 => Ok(false),
            _ => Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "the runtime rejected CLI timing initialization",
            )),
        }
    }

    #[cfg(copilot_cli_host)]
    pub(crate) fn admit_cli_host(&self) -> Result<(), Error> {
        let _operation = self.operation_lock.lock();
        let connection = self.connection_id().ok_or_else(|| {
            Error::with_message(
                ErrorKind::InvalidConfig,
                "the SDK's in-process connection is no longer active",
            )
        })?;
        let admit = bind::<unsafe extern "C" fn(u32) -> bool>(
            self.library,
            b"copilot_runtime_cli_host_connection_admit\0",
            &self.library_path,
        )?;
        // SAFETY: the SDK retains this exact image and owns the live connection.
        if unsafe { admit(connection) } {
            Ok(())
        } else {
            Err(Error::with_message(
                ErrorKind::InvalidConfig,
                format!(
                    "the runtime rejected private CLI host admission for connection {connection}"
                ),
            ))
        }
    }

    pub(crate) async fn call_plugin_host(
        &self,
        operation: crate::InProcessPluginHostOperation,
        params: Option<serde_json::Value>,
    ) -> Result<serde_json::Value, Error> {
        unsafe extern "C" fn complete(data: *mut c_void, bytes: *const u8, len: usize) {
            let sender =
                unsafe { Box::from_raw(data.cast::<tokio::sync::oneshot::Sender<Vec<u8>>>()) };
            let payload = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
            let _ = sender.send(payload);
        }
        let params = params.map(|value| serde_json::to_vec(&value)).transpose()?;
        let receiver = self.with_server(|server_id, library| {
            let request = unsafe {
                library.get::<PluginHostRequestFn>(b"copilot_runtime_plugin_host_request\0")
            }
            .map_err(|error| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    format!("the runtime does not support bounded plugin host calls: {error}"),
                )
            })?;
            let connection_id = self.connection_id().ok_or_else(|| {
                Error::with_message(ErrorKind::InvalidConfig, "in-process connection is closed")
            })?;
            let (sender, receiver) = tokio::sync::oneshot::channel::<Vec<u8>>();
            let sender = Box::into_raw(Box::new(sender));
            let (ptr, len) = params
                .as_ref()
                .map_or((std::ptr::null(), 0), |value| (value.as_ptr(), value.len()));
            let accepted = unsafe {
                request(
                    server_id,
                    connection_id,
                    operation as u32,
                    ptr,
                    len,
                    complete,
                    sender.cast(),
                )
            };
            if !accepted {
                drop(unsafe { Box::from_raw(sender) });
                return Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "the local plugin host request was refused",
                ));
            }
            Ok(receiver)
        })?;
        let bytes = receiver
            .await
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
        Ok(serde_json::from_slice(&bytes)?)
    }

    pub(crate) fn discovery_publisher_exports(
        &self,
    ) -> Result<(u32, DiscoveryPublisherCreateFn, DiscoveryPublisherRequestFn), Error> {
        self.with_server(|server_id, library| {
            let create = unsafe {
                library.get::<DiscoveryPublisherCreateFn>(
                    b"copilot_runtime_discovery_publisher_create\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            let request = unsafe {
                library.get::<DiscoveryPublisherRequestFn>(
                    b"copilot_runtime_discovery_publisher_request\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            Ok((server_id, *create, *request))
        })
    }

    pub(crate) fn configure_process_telemetry_host(&self) -> Result<(), Error> {
        if !self.owns_host {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "process telemetry requires the original owned SDK engine",
            ));
        }
        self.with_server(|server_id, library| {
            let configure = unsafe {
                library.get::<unsafe extern "C" fn(u32, u32) -> bool>(
                    b"copilot_runtime_server_configure_process_telemetry_host\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            if unsafe {
                configure(
                    server_id,
                    self.connection_id().ok_or_else(|| {
                        Error::with_message(
                            ErrorKind::InvalidConfig,
                            "in-process connection is closed",
                        )
                    })?,
                )
            } {
                Ok(())
            } else {
                Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "the original engine refused process telemetry callbacks",
                ))
            }
        })
    }

    pub(crate) fn register_embedder_observer(&self) -> Result<(), Error> {
        if !self.owns_host {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "embedding callbacks require the original owned SDK engine",
            ));
        }
        self.with_server(|_server_id, library| {
            let admit = unsafe {
                library.get::<unsafe extern "C" fn(u32) -> bool>(
                    b"copilot_runtime_embedder_observer_admit\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            let connection_id = self.connection_id().ok_or_else(|| {
                Error::with_message(ErrorKind::InvalidConfig, "in-process connection is closed")
            })?;
            if unsafe { admit(connection_id) } {
                Ok(())
            } else {
                Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "the original SDK connection cannot register embedding callbacks",
                ))
            }
        })
    }

    fn with_server<T>(
        &self,
        operation: impl FnOnce(u32, &Library) -> Result<T, Error>,
    ) -> Result<T, Error> {
        let _operation = self.operation_lock.lock();
        let server_id = self.server_id().ok_or_else(|| {
            Error::with_message(ErrorKind::InvalidConfig, "in-process server is closed")
        })?;
        let library = load_library(self.library_path())?;
        operation(server_id, library)
    }

    pub(crate) fn publish_foreground_session(
        &self,
        session_id: Option<&str>,
    ) -> Result<bool, Error> {
        self.with_server(|server_id, library| {
            let publish = unsafe {
                library.get::<ServerSessionControlFn>(
                    b"copilot_runtime_server_set_foreground_session\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            let (pointer, length) = session_id
                .map(|id| (id.as_ptr(), id.len()))
                .unwrap_or((std::ptr::null(), 0));
            Self::server_boolean_result(unsafe { publish(server_id, pointer, length) })
        })
    }

    pub(crate) fn reserve_foreground_publication(
        &self,
    ) -> impl std::future::Future<Output = tokio::sync::OwnedMutexGuard<()>> + Send + 'static + use<>
    {
        let mut reservation = Box::pin(Arc::clone(&self.foreground_order).lock_owned());
        // Register FIFO admission now, before independent executor tasks can reorder calls.
        let admitted = reservation
            .as_mut()
            .poll(&mut Context::from_waker(Waker::noop()));
        async move {
            match admitted {
                Poll::Ready(guard) => guard,
                Poll::Pending => reservation.await,
            }
        }
    }

    pub(crate) fn is_session_retained(&self, session_id: &str) -> Result<bool, Error> {
        self.with_server(|server_id, library| {
            let inspect = unsafe {
                library
                    .get::<ServerSessionControlFn>(b"copilot_runtime_server_is_session_retained\0")
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            Self::server_boolean_result(unsafe {
                inspect(server_id, session_id.as_ptr(), session_id.len())
            })
        })
    }

    pub(crate) fn has_session_connection(
        &self,
        session_id: &str,
        ignored: &[u32],
    ) -> Result<bool, Error> {
        let ignored = serde_json::to_vec(ignored)?;
        self.with_server(|server_id, library| {
            let inspect = unsafe {
                library.get::<ServerHasSessionConnectionFn>(
                    b"copilot_runtime_server_has_session_connection\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            Self::server_boolean_result(unsafe {
                inspect(
                    server_id,
                    session_id.as_ptr(),
                    session_id.len(),
                    ignored.as_ptr(),
                    ignored.len(),
                )
            })
        })
    }

    pub(crate) fn set_connection_default_session(
        &self,
        connection_id: u32,
        session_id: &str,
    ) -> Result<bool, Error> {
        self.with_server(|server_id, library| {
            let set = unsafe {
                library.get::<ServerSetConnectionDefaultSessionFn>(
                    b"copilot_runtime_server_set_connection_default_session_id\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            Self::server_boolean_result(unsafe {
                set(
                    server_id,
                    connection_id,
                    session_id.as_ptr(),
                    session_id.len(),
                )
            })
        })
    }

    pub(crate) fn configure_server_defaults(&self, overrides: &[u8]) -> Result<(), Error> {
        self.with_server(|server_id, library| {
            // The object form extends the existing ABI without changing its signature.
            let configure = unsafe {
                library
                    .get::<ServerSetSdkSessionConfigFn>(
                        b"copilot_runtime_server_set_sdk_session_config\0",
                    )
                    .map_err(|error| {
                        Error::with_message(ErrorKind::InvalidConfig, error.to_string())
                    })?
            };
            if unsafe {
                configure(
                    server_id,
                    overrides.as_ptr(),
                    overrides.len(),
                    false,
                    false,
                    std::ptr::null(),
                    0,
                    std::ptr::null(),
                    0,
                )
            } {
                Ok(())
            } else {
                Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "runtime rejected the in-process server configuration",
                ))
            }
        })
    }

    fn server_boolean_result(result: i32) -> Result<bool, Error> {
        match result {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "in-process server control rejected the engine or arguments",
            )),
        }
    }

    pub(crate) async fn close_protocol_peers(&self) -> Result<(), Error> {
        let peers: Vec<_> = {
            let _operation = self.operation_lock.lock();
            self.protocol_peers
                .lock()
                .take()
                .into_iter()
                .flatten()
                .filter_map(|peer| peer.upgrade())
                .collect()
        };
        for peer in &peers {
            peer.close();
        }
        for peer in peers {
            let mut done = peer.cleanup_done.subscribe();
            while !*done.borrow_and_update() {
                done.changed().await.map_err(|error| {
                    Error::with_message(ErrorKind::InvalidConfig, error.to_string())
                })?;
            }
        }
        Ok(())
    }

    pub(crate) fn server_id(&self) -> Option<u32> {
        let server_id = self.server_id.load(Ordering::SeqCst);
        (!self.closed.load(Ordering::SeqCst) && server_id != 0).then_some(server_id)
    }

    pub(crate) fn configure_ui_thread(&self, enabled: bool) -> Result<(), Error> {
        let connection = self.connection_id.load(Ordering::SeqCst);
        if !self.closed.load(Ordering::SeqCst)
            && unsafe { (self.connection_set_ui_thread_handler)(connection, enabled) }
        {
            Ok(())
        } else {
            Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "failed to configure the in-process UI dispatcher",
            ))
        }
    }

    pub(crate) fn run_ui_action(&self, action_id: u32) -> Result<(), Error> {
        let connection = self.connection_id.load(Ordering::SeqCst);
        // No operation lock across native UI: another thread must still be able
        // to close the connection and request cancellation.
        if !self.closed.load(Ordering::SeqCst)
            && unsafe { (self.connection_run_ui_action)(connection, action_id) }
        {
            Ok(())
        } else {
            Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "native UI action is retired, already executed, or not on the process main thread",
            ))
        }
    }

    pub(crate) fn library_path(&self) -> &Path {
        &self.library_path
    }

    pub(crate) fn owns_host(&self) -> bool {
        self.owns_host
    }

    pub(crate) fn connection_id(&self) -> Option<u32> {
        let id = self.connection_id.load(Ordering::SeqCst);
        (id != 0 && !self.closed.load(Ordering::SeqCst)).then_some(id)
    }

    /// Close the connection, shut down an owned host, and free the callback state.
    /// Idempotent; called from [`Client::stop`], drop, and on startup failure.
    pub(crate) fn close(&self) {
        let (conn, server, state) = {
            let _operation = self.operation_lock.lock();
            if self.closed.swap(true, Ordering::SeqCst) {
                return;
            }
            let state = self
                .callback_state
                .swap(std::ptr::null_mut(), Ordering::SeqCst);
            if !state.is_null() && self.connection_close_and_wait.is_none() {
                unsafe { &*state }.closing.store(true, Ordering::SeqCst);
            }
            (
                self.connection_id.swap(0, Ordering::SeqCst),
                self.server_id.swap(0, Ordering::SeqCst),
                state as usize,
            )
        };
        for peer in self
            .protocol_peers
            .lock()
            .take()
            .into_iter()
            .flatten()
            .filter_map(|peer| peer.upgrade())
        {
            peer.close();
        }
        // New sends now fail without waiting for native close or host shutdown.
        // The callback state remains alive until native callbacks are quiescent.
        if conn != 0 {
            let quiesced = match self.connection_close_and_wait {
                Some(close) => unsafe { close(conn, 0) },
                None => unsafe { (self.connection_close)(conn) },
            };
            if !quiesced {
                let connection_close = self.connection_close;
                let connection_close_and_wait = self.connection_close_and_wait;
                let host_shutdown = self.host_shutdown;
                let owns_host = self.owns_host;
                let auth_owner = Arc::new(self.auth_owner.lock().take());
                let cleanup_auth_owner = Arc::clone(&auth_owner);
                let library_path = self.library_path.clone();
                let cleanup_done = self.cleanup_done.clone();
                if let Err(error) = std::thread::Builder::new()
                    .name("copilot-ffi-cleanup".to_owned())
                    .spawn(move || {
                        let _auth_owner = cleanup_auth_owner;
                        loop {
                            let quiesced = match connection_close_and_wait {
                                Some(close) => unsafe { close(conn, 100) },
                                None => unsafe { connection_close(conn) },
                            };
                            if quiesced {
                                break;
                            }
                            if connection_close_and_wait.is_none() {
                                std::thread::sleep(std::time::Duration::from_millis(100));
                            }
                        }
                        release_callback_state(state);
                        if owns_host && server != 0 && !unsafe { host_shutdown(server) } {
                            warn!(
                                library = %library_path.display(),
                                server_id = server,
                                "FFI runtime host shutdown did not recognize server"
                            );
                        }
                        cleanup_done.send_replace(true);
                        debug!(library = %library_path.display(), "FFI runtime connection closed");
                    })
                {
                    // The host remains live if no cleanup worker can start.
                    std::mem::forget(auth_owner);
                    warn!(
                        error = %error,
                        library = %self.library_path.display(),
                        "failed to start deferred FFI cleanup thread; callback state retained"
                    );
                }
                return;
            }
        }
        release_callback_state(state);
        if self.owns_host && server != 0 && !unsafe { (self.host_shutdown)(server) } {
            warn!(
                library = %self.library_path.display(),
                server_id = server,
                "FFI runtime host shutdown did not recognize server"
            );
        }
        self.auth_owner.lock().take();
        self.cleanup_done.send_replace(true);
        debug!(library = %self.library_path.display(), "FFI runtime connection closed");
    }
}

/// Identity and admission for an independent inbound protocol peer.
///
/// The peer supplies complete protocol bytes, including any headers. The runtime
/// retains responsibility for authentication, negotiation, routing, and sessions.
#[derive(Clone, Default, serde::Serialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct InProcessProtocolConnectionOptions {
    /// Extension source, paired with `extension_name` to identify an extension.
    pub extension_source: Option<String>,
    /// Extension name, paired with `extension_source` to identify an extension.
    pub extension_name: Option<String>,
    /// Stable extension identifier, when the host has one.
    pub extension_id: Option<String>,
    /// Original extension module path, without canonicalization.
    pub extension_module_path: Option<String>,
    /// Session that owns an extension subprocess.
    pub owning_session_id: Option<String>,
    /// Mark an extension peer even when no extension identity is available.
    pub is_extension: bool,
    /// Expected connection token for a peer that requires token authentication.
    pub connection_token: Option<String>,
}

/// An independent raw protocol peer on an SDK client's in-process server.
///
/// Unlike another [`crate::Client`], this handle has no JSON-RPC reader or
/// callback dispatcher. The embedding host forwards bytes in both directions.
/// Closing or dropping it closes only this peer. Stopping its parent client
/// closes the peer, even when that client borrows its server.
pub struct InProcessProtocolConnection {
    reader: tokio::sync::Mutex<FfiReader>,
    shared: Arc<FfiShared>,
    connection_id: u32,
    notify: ConnectionNotifyFn,
    notify_after_response: ConnectionNotifyAfterResponseFn,
    request: ConnectionRequestFn,
}

impl InProcessProtocolConnection {
    /// Bind catalogue policy to a session in this peer's original engine.
    ///
    /// The public identity never exposes or transfers the native session owner.
    pub fn borrow_catalogue_session(&self, session_id: &str) -> Result<(), Error> {
        self.shared.with_server(|server_id, library| {
            let borrow = unsafe {
                library.get::<unsafe extern "C" fn(u32, u32, *const u8, usize) -> i32>(
                    b"copilot_runtime_server_borrow_catalogue_session\0",
                )
            }
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
            if unsafe {
                borrow(
                    server_id,
                    self.connection_id,
                    session_id.as_ptr(),
                    session_id.len(),
                )
            } == 1
            {
                Ok(())
            } else {
                Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "the original engine refused the catalogue session borrow",
                ))
            }
        })
    }

    /// The runtime-assigned connection ID, retained after close for attribution.
    pub fn connection_id(&self) -> u32 {
        self.connection_id
    }

    /// Whether this handle still accepts inbound bytes and outbound calls.
    pub fn is_active(&self) -> bool {
        self.shared.connection_id().is_some() && {
            // The operation lock prevents close from freeing callback state.
            let _operation = self.shared.operation_lock.lock();
            let state = self.shared.callback_state.load(Ordering::SeqCst);
            !state.is_null() && unsafe { &*state }.tx.lock().is_some()
        }
    }

    /// Send a server-initiated notification through the runtime's auth gate.
    pub fn notify(&self, method: &str, params_json: Option<&str>) -> bool {
        let (params, len) =
            params_json.map_or((std::ptr::null(), 0), |value| (value.as_ptr(), value.len()));
        unsafe {
            (self.notify)(
                self.connection_id,
                method.as_ptr(),
                method.len(),
                params,
                len,
            )
        }
    }

    /// Queue a notification after the response identified by `dispatch_token`.
    pub fn notify_after_response(
        &self,
        dispatch_token: u32,
        method: &str,
        params_json: Option<&str>,
    ) -> bool {
        let (params, len) =
            params_json.map_or((std::ptr::null(), 0), |value| (value.as_ptr(), value.len()));
        unsafe {
            (self.notify_after_response)(
                self.connection_id,
                dispatch_token,
                method.as_ptr(),
                method.len(),
                params,
                len,
            )
        }
    }

    /// Issue a server-to-client request and return its exact native JSON envelope.
    pub async fn request(&self, method: &str, params_json: Option<&str>) -> Result<String, Error> {
        unsafe extern "C" fn complete(data: *mut c_void, bytes: *const u8, len: usize) {
            let sender =
                unsafe { Box::from_raw(data.cast::<tokio::sync::oneshot::Sender<Vec<u8>>>()) };
            let payload = unsafe { std::slice::from_raw_parts(bytes, len) }.to_vec();
            let _ = sender.send(payload);
        }
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let sender = Box::into_raw(Box::new(sender));
        let (params, len) =
            params_json.map_or((std::ptr::null(), 0), |value| (value.as_ptr(), value.len()));
        let accepted = unsafe {
            (self.request)(
                self.connection_id,
                method.as_ptr(),
                method.len(),
                params,
                len,
                complete,
                sender.cast(),
            )
        };
        if !accepted {
            drop(unsafe { Box::from_raw(sender) });
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "protocol request was refused",
            ));
        }
        let bytes = receiver
            .await
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
        String::from_utf8(bytes)
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))
    }

    /// Accept inbound protocol bytes without changing their framing or headers.
    ///
    /// This operation copies the bytes before it returns.
    pub fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        self.shared.write_frame(bytes).map_err(Error::from)
    }

    /// Receive the next outbound byte run, or `None` after the peer drains.
    ///
    /// Cancellation is safe: a canceled receive does not consume any bytes.
    /// Only one receive runs at a time; close does not acquire the reader lock.
    /// Native EOF preserves queued frames, then resolves the next receive to `None`.
    pub async fn next_outbound(&self) -> Option<Vec<u8>> {
        self.reader.lock().await.rx.recv().await
    }

    /// Begin peer closure without waiting for native callbacks to finish.
    ///
    /// Returns whether this call initiated closure. Buffered outbound frames
    /// remain readable. Use [`Self::close`] when the native join must finish.
    pub fn begin_close(&self) -> bool {
        let open = self.shared.connection_id().is_some();
        self.shared.close();
        open
    }

    /// Close this peer and join its native driver and outbound callbacks.
    ///
    /// Queued responses remain available through [`Self::next_outbound`] after
    /// this returns. This operation never shuts down the borrowed server.
    /// Cancellation does not interrupt native cleanup; a later close can join it.
    pub async fn close(&self) -> Result<(), Error> {
        let mut done = self.shared.cleanup_done.subscribe();
        self.shared.close();
        while !*done.borrow_and_update() {
            done.changed().await.map_err(|error| {
                Error::with_message(ErrorKind::InvalidConfig, error.to_string())
            })?;
        }
        Ok(())
    }
}

impl Drop for InProcessProtocolConnection {
    fn drop(&mut self) {
        self.shared.close();
    }
}

enum ProtocolAdmission {
    External(InProcessProtocolConnectionOptions),
    #[cfg(copilot_cli_host)]
    OriginalStdio(u32),
}

enum ProtocolOpen {
    External(ProtocolConnectionOpenFn, Vec<u8>),
    #[cfg(copilot_cli_host)]
    OriginalStdio(OriginalStdioOpenFn, u32),
}

pub(crate) async fn open_protocol_connection(
    parent: Arc<FfiShared>,
    options: InProcessProtocolConnectionOptions,
) -> Result<InProcessProtocolConnection, Error> {
    open_protocol_peer(parent, ProtocolAdmission::External(options)).await
}

#[cfg(copilot_cli_host)]
pub(crate) async fn open_original_stdio_connection(
    parent: Arc<FfiShared>,
) -> Result<InProcessProtocolConnection, Error> {
    let connection_id = parent.connection_id().ok_or_else(|| {
        Error::with_message(ErrorKind::InvalidConfig, "in-process client is closed")
    })?;
    open_protocol_peer(parent, ProtocolAdmission::OriginalStdio(connection_id)).await
}

async fn open_protocol_peer(
    parent: Arc<FfiShared>,
    admission: ProtocolAdmission,
) -> Result<InProcessProtocolConnection, Error> {
    // Admission and registration share the parent's close lock. A peer can
    // never escape a concurrent parent shutdown.
    tokio::task::spawn_blocking(move || {
        let _operation = parent.operation_lock.lock();
        let server_id = parent.server_id().ok_or_else(|| {
            Error::with_message(ErrorKind::InvalidConfig, "in-process client is closed")
        })?;
        #[cfg(copilot_cli_host)]
        if matches!(admission, ProtocolAdmission::OriginalStdio(_)) && !parent.owns_host {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "original stdio requires the owning CLI client",
            ));
        }
        if parent.protocol_peers.lock().is_none() {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "in-process client is closing",
            ));
        }
        let host = FfiHost::create(
            None,
            None,
            None,
            Vec::new(),
            Vec::new(),
            FfiHostTarget {
                auth_owner: None,
                server_id: Some(server_id),
                library_path: Some(parent.library_path()),
                session_binding: None,
                file_logging_path: None,
            },
        )?;
        let library = load_library(parent.library_path())?;
        let notify = *bind::<ConnectionNotifyFn>(
            library,
            b"copilot_runtime_connection_notify\0",
            parent.library_path(),
        )?;
        let notify_after_response = *bind::<ConnectionNotifyAfterResponseFn>(
            library,
            b"copilot_runtime_connection_notify_after_response\0",
            parent.library_path(),
        )?;
        let request = *bind::<ConnectionRequestFn>(
            library,
            b"copilot_runtime_connection_request\0",
            parent.library_path(),
        )?;
        let (reader, shared) = host.start_with_protocol_options(Some(admission))?;
        let connection_id = shared.connection_id().ok_or_else(|| {
            Error::with_message(ErrorKind::InvalidConfig, "in-process peer is closed")
        })?;
        let mut peers = parent.protocol_peers.lock();
        let peers = peers.as_mut().ok_or_else(|| {
            Error::with_message(ErrorKind::InvalidConfig, "in-process client is closing")
        })?;
        peers.retain(|peer| peer.strong_count() != 0);
        peers.push(Arc::downgrade(&shared));
        Ok(InProcessProtocolConnection {
            reader: tokio::sync::Mutex::new(reader),
            shared,
            connection_id,
            notify,
            notify_after_response,
            request,
        })
    })
    .await
    .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?
}

impl FfiShared {
    pub(crate) fn write_frame(&self, frame: &[u8]) -> std::io::Result<()> {
        let _operation = self.operation_lock.lock();
        let conn = self.connection_id.load(Ordering::SeqCst);
        if !self.closed.load(Ordering::SeqCst)
            && conn != 0
            && unsafe { (self.connection_write)(conn, frame.as_ptr(), frame.len()) }
        {
            Ok(())
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "failed to write a frame to the in-process runtime connection",
            ))
        }
    }
}

pub(crate) struct HostSessionConstruction {
    pub(crate) token: u32,
    connection_id: u32,
    release: unsafe extern "C" fn(u32, u32) -> bool,
}

impl Drop for HostSessionConstruction {
    fn drop(&mut self) {
        // SAFETY: the library cache retains this function for the process lifetime.
        unsafe { (self.release)(self.connection_id, self.token) };
    }
}

pub(crate) fn prepare_host_session_construction(
    library_path: &Path,
    connection_id: u32,
    payload: &str,
) -> Result<HostSessionConstruction, Error> {
    let library = load_library(library_path)?;
    let prepare = *bind::<unsafe extern "C" fn(u32, *const u8, usize) -> u32>(
        library,
        b"copilot_runtime_prepare_host_session_construction\0",
        library_path,
    )?;
    let release = *bind::<unsafe extern "C" fn(u32, u32) -> bool>(
        library,
        b"copilot_runtime_release_host_session_construction\0",
        library_path,
    )?;
    // SAFETY: the runtime copies this initialized UTF-8 slice before the call returns.
    let token = unsafe { prepare(connection_id, payload.as_ptr(), payload.len()) };
    if token == 0 {
        return Err(Error::with_message(
            ErrorKind::InvalidConfig,
            "the in-process connection cannot prepare host session construction",
        ));
    }
    Ok(HostSessionConstruction {
        token,
        connection_id,
        release,
    })
}

fn release_callback_state(state: usize) {
    if state == 0 {
        return;
    }
    let state = state as *mut CallbackState;
    drop(unsafe { Box::from_raw(state) });
}

type HostResultCallback = unsafe extern "C" fn(*mut c_void, *const u8, usize);
type HostPrepareFn =
    unsafe extern "C" fn(u32, *const u8, usize, HostResultCallback, *mut c_void) -> bool;
type HostInitializeFn = unsafe extern "C" fn(u32, bool, HostResultCallback, *mut c_void) -> bool;
type HostReleaseFn = unsafe extern "C" fn(u32) -> bool;
type HostBeginPermissionUpdateFn = unsafe extern "C" fn(u32) -> u32;

/// A pending permission update on one original in-process host registration.
#[doc(hidden)]
pub struct InProcessPermissionUpdate {
    ticket: u32,
    finish: HostReleaseFn,
}

impl Drop for InProcessPermissionUpdate {
    fn drop(&mut self) {
        unsafe { (self.finish)(self.ticket) };
    }
}

/// Callback authority for a native resource that the SDK already owns.
#[doc(hidden)]
pub struct InProcessSessionHost {
    lease: AtomicU32,
    channel: u32,
    token: String,
    generation: String,
    initialize: HostInitializeFn,
    release: HostReleaseFn,
    begin_permission_update: HostBeginPermissionUpdateFn,
    finish_permission_update: HostReleaseFn,
}

fn host_result(bytes: &[u8]) -> Result<serde_json::Value, Error> {
    let envelope: serde_json::Value = serde_json::from_slice(bytes)
        .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))?;
    if envelope["ok"] != true {
        return Err(Error::with_message(
            ErrorKind::InvalidConfig,
            envelope["error"]["message"]
                .as_str()
                .unwrap_or("Invalid SDK host result")
                .to_owned(),
        ));
    }
    Ok(envelope["result"].clone())
}

unsafe extern "C" fn host_prepare_result(data: *mut c_void, bytes: *const u8, len: usize) {
    let result = unsafe { &mut *data.cast::<Option<Result<serde_json::Value, Error>>>() };
    *result = Some(host_result(unsafe {
        std::slice::from_raw_parts(bytes, len)
    }));
}

unsafe extern "C" fn host_initialize_result(data: *mut c_void, bytes: *const u8, len: usize) {
    let sender = unsafe {
        Box::from_raw(data.cast::<tokio::sync::oneshot::Sender<Result<serde_json::Value, Error>>>())
    };
    let _ = sender.send(host_result(unsafe {
        std::slice::from_raw_parts(bytes, len)
    }));
}

pub(crate) fn prepare_session_host(
    path: &Path,
    connection_id: u32,
    payload: &str,
) -> Result<InProcessSessionHost, Error> {
    let library = load_library(path)?;
    let prepare = *bind::<HostPrepareFn>(library, b"copilot_runtime_session_host_prepare\0", path)?;
    let initialize =
        *bind::<HostInitializeFn>(library, b"copilot_runtime_session_host_initialize\0", path)?;
    let release = *bind::<HostReleaseFn>(library, b"copilot_runtime_session_host_release\0", path)?;
    let begin_permission_update = *bind::<HostBeginPermissionUpdateFn>(
        library,
        b"copilot_runtime_session_host_begin_permission_update\0",
        path,
    )?;
    let finish_permission_update = *bind::<HostReleaseFn>(
        library,
        b"copilot_runtime_session_host_finish_permission_update\0",
        path,
    )?;
    let mut result: Option<Result<serde_json::Value, Error>> = None;
    // The prepare callback runs before this C ABI call returns.
    let accepted = unsafe {
        prepare(
            connection_id,
            payload.as_ptr(),
            payload.len(),
            host_prepare_result,
            (&mut result as *mut Option<Result<serde_json::Value, Error>>).cast(),
        )
    };
    if !accepted {
        return Err(Error::with_message(
            ErrorKind::InvalidConfig,
            "The SDK host preparation was rejected",
        ));
    }
    let result = result.ok_or_else(|| {
        Error::with_message(ErrorKind::InvalidConfig, "The SDK host returned no result")
    })??;
    let lease = result["lease"]
        .as_u64()
        .and_then(|value| u32::try_from(value).ok())
        .ok_or_else(|| {
            Error::with_message(ErrorKind::InvalidConfig, "The SDK host returned no lease")
        })?;
    Ok(InProcessSessionHost {
        lease: AtomicU32::new(lease),
        channel: result["hostChannel"]
            .as_u64()
            .and_then(|value| u32::try_from(value).ok())
            .ok_or_else(|| {
                Error::with_message(ErrorKind::InvalidConfig, "The SDK host returned no channel")
            })?,
        token: result["registrationToken"]
            .as_str()
            .ok_or_else(|| {
                Error::with_message(ErrorKind::InvalidConfig, "The SDK host returned no token")
            })?
            .to_owned(),
        generation: result["generation"]
            .as_str()
            .ok_or_else(|| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    "The SDK host returned no generation",
                )
            })?
            .to_owned(),
        initialize,
        release,
        begin_permission_update,
        finish_permission_update,
    })
}

impl InProcessSessionHost {
    /// Register pending setup before scheduling the SDK setter.
    ///
    /// The ticket settles on drop, including cancellation and SDK errors.
    /// This call takes only short in-memory locks, never native session locks.
    pub fn begin_permission_update(&self) -> Result<InProcessPermissionUpdate, Error> {
        let ticket = unsafe { (self.begin_permission_update)(self.lease.load(Ordering::SeqCst)) };
        if ticket == 0 {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "The SDK host permission update was rejected",
            ));
        }
        Ok(InProcessPermissionUpdate {
            ticket,
            finish: self.finish_permission_update,
        })
    }

    /// The original callback channel.
    pub fn host_channel(&self) -> u32 {
        self.channel
    }

    /// The callback registration capability.
    pub fn registration_token(&self) -> &str {
        &self.token
    }

    /// The native resource generation.
    pub fn generation(&self) -> &str {
        &self.generation
    }

    /// Finish the original facade's callback and model setup.
    pub async fn initialize(&self, exit_plan_mode: bool) -> Result<(), Error> {
        let (sender, receiver) =
            tokio::sync::oneshot::channel::<Result<serde_json::Value, Error>>();
        let data = Box::into_raw(Box::new(sender));
        // An accepted request owns the sender until its single terminal callback.
        let accepted = unsafe {
            (self.initialize)(
                self.lease.load(Ordering::SeqCst),
                exit_plan_mode,
                host_initialize_result,
                data.cast(),
            )
        };
        if !accepted {
            drop(unsafe { Box::from_raw(data) });
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "The SDK host initialization was rejected",
            ));
        }
        receiver
            .await
            .map_err(|error| Error::with_message(ErrorKind::InvalidConfig, error.to_string()))??;
        Ok(())
    }

    /// Release only this facade's callbacks, never its native session.
    pub fn release(&self) {
        let lease = self.lease.swap(0, Ordering::SeqCst);
        if lease != 0 {
            unsafe { (self.release)(lease) };
        }
    }
}

impl Drop for InProcessSessionHost {
    fn drop(&mut self) {
        self.release();
    }
}
impl Drop for FfiShared {
    fn drop(&mut self) {
        self.close();
    }
}

/// Read side of the FFI transport, fed by the native outbound callback via an
/// unbounded channel. Implements [`AsyncRead`] for the JSON-RPC read loop.
pub(crate) struct FfiReader {
    rx: mpsc::UnboundedReceiver<Vec<u8>>,
    leftover: Vec<u8>,
    pos: usize,
}

impl AsyncRead for FfiReader {
    fn poll_read(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        if self.pos >= self.leftover.len() {
            match self.rx.poll_recv(cx) {
                Poll::Ready(Some(chunk)) => {
                    self.leftover = chunk;
                    self.pos = 0;
                }
                Poll::Ready(None) => return Poll::Ready(Ok(())),
                Poll::Pending => return Poll::Pending,
            }
        }
        let available = self.leftover.len() - self.pos;
        let n = available.min(buf.remaining());
        let start = self.pos;
        buf.put_slice(&self.leftover[start..start + n]);
        self.pos += n;
        Poll::Ready(Ok(()))
    }
}

/// Prepared FFI host. The cdylib is loaded process-globally and never unloaded
/// (see [`load_library`]).
pub(crate) struct FfiHost {
    #[cfg(copilot_cli_host)]
    library: &'static Library,
    auth_owner: Option<Arc<dyn crate::InProcessAuthOwner>>,
    host_start_with_auth_owner: Option<HostStartWithAuthOwnerFn>,
    session_binding: Option<crate::InProcessSessionBinding>,
    session_connection_open: Option<SessionConnectionOpenFn>,
    file_logging_path: Option<PathBuf>,
    file_logging_connection_admit: Option<FileLoggingConnectionAdmitFn>,
    library_path: PathBuf,
    cli_entrypoint: Option<PathBuf>,
    integration_id: Option<String>,
    environment: Vec<(String, String)>,
    args: Vec<String>,
    attached_server_id: Option<u32>,
    host_start: HostStartFn,
    host_shutdown: HostShutdownFn,
    connection_open: ConnectionOpenFn,
    connection_write: ConnectionWriteFn,
    connection_close: ConnectionCloseFn,
    connection_set_ui_thread_handler: ConnectionSetUiThreadHandlerFn,
    connection_run_ui_action: ConnectionRunUiActionFn,
}

pub(crate) struct FfiHostTarget<'a> {
    pub auth_owner: Option<Arc<dyn crate::InProcessAuthOwner>>,
    pub server_id: Option<u32>,
    pub library_path: Option<&'a Path>,
    pub session_binding: Option<crate::InProcessSessionBinding>,
    pub file_logging_path: Option<&'a Path>,
}

// SAFETY: as for `FfiShared` — the bound exports are plain fn pointers, safe to
// move to the blocking thread that starts the host.
unsafe impl Send for FfiHost {}

impl FfiHost {
    /// Load the selected cdylib, defaulting to a sibling of `runtime_entrypoint`.
    pub(crate) fn create(
        runtime_entrypoint: Option<&Path>,
        cli_entrypoint: Option<&Path>,
        integration_id: Option<&str>,
        environment: Vec<(String, String)>,
        args: Vec<String>,
        target: FfiHostTarget<'_>,
    ) -> Result<Self, Error> {
        let FfiHostTarget {
            auth_owner,
            server_id: attached_server_id,
            library_path: runtime_library_path,
            session_binding,
            file_logging_path,
        } = target;
        if session_binding.is_some() && attached_server_id.is_none() {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "a borrowed session requires an attached in-process server",
            ));
        }
        let cli_entrypoint = cli_entrypoint
            .map(std::fs::canonicalize)
            .transpose()
            .map_err(|e| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    format!("failed to resolve explicit in-process CLI entrypoint: {e}"),
                )
            })?
            .map(path_for_child_process);
        let selected_library = match (attached_server_id, runtime_library_path) {
            (_, Some(path)) => path.to_path_buf(),
            (Some(_), None) => {
                return Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "an attached server requires the runtime image already loaded by its host",
                ));
            }
            (None, None) => {
                let runtime_entrypoint = runtime_entrypoint.ok_or_else(|| {
                    Error::with_message(
                        ErrorKind::InvalidConfig,
                        "in-process transport requires a runtime image or CLI entrypoint",
                    )
                })?;
                let runtime_entrypoint =
                    std::fs::canonicalize(runtime_entrypoint).map_err(|e| {
                        Error::with_message(
                            ErrorKind::InvalidConfig,
                            format!(
                                "failed to resolve in-process runtime entrypoint '{}': {e}",
                                runtime_entrypoint.display()
                            ),
                        )
                    })?;
                resolve_library_path(&runtime_entrypoint)?
            }
        };
        let library_path = std::fs::canonicalize(selected_library).map_err(|e| {
            Error::with_message(
                ErrorKind::InvalidConfig,
                format!("failed to resolve in-process runtime library: {e}"),
            )
        })?;
        let lib = load_library(&library_path)?;
        let host_start_with_auth_owner = bind_optional::<HostStartWithAuthOwnerFn>(
            lib,
            b"copilot_runtime_host_start_with_auth_owner\0",
            &library_path,
            auth_owner.is_some(),
        )?;
        let session_connection_open = bind_optional::<SessionConnectionOpenFn>(
            lib,
            b"copilot_runtime_session_connection_open\0",
            &library_path,
            session_binding.is_some(),
        )?;
        let file_logging_connection_admit = bind_optional::<FileLoggingConnectionAdmitFn>(
            lib,
            b"copilot_runtime_file_logging_connection_admit\0",
            &library_path,
            file_logging_path.is_some(),
        )?;

        let host_start = *bind::<HostStartFn>(lib, b"copilot_runtime_host_start\0", &library_path)?;
        let host_shutdown =
            *bind::<HostShutdownFn>(lib, b"copilot_runtime_host_shutdown\0", &library_path)?;
        let connection_open =
            *bind::<ConnectionOpenFn>(lib, b"copilot_runtime_connection_open\0", &library_path)?;
        let connection_write =
            *bind::<ConnectionWriteFn>(lib, b"copilot_runtime_connection_write\0", &library_path)?;
        let connection_close =
            *bind::<ConnectionCloseFn>(lib, b"copilot_runtime_connection_close\0", &library_path)?;
        let connection_set_ui_thread_handler = *bind::<ConnectionSetUiThreadHandlerFn>(
            lib,
            b"copilot_runtime_connection_set_ui_thread_handler\0",
            &library_path,
        )?;
        let connection_run_ui_action = *bind::<ConnectionRunUiActionFn>(
            lib,
            b"copilot_runtime_connection_run_ui_action\0",
            &library_path,
        )?;

        Ok(Self {
            #[cfg(copilot_cli_host)]
            library: lib,
            auth_owner,
            host_start_with_auth_owner,
            session_binding,
            session_connection_open,
            file_logging_path: file_logging_path.map(Path::to_path_buf),
            file_logging_connection_admit,
            library_path,
            cli_entrypoint,
            integration_id: integration_id.map(str::to_owned),
            environment,
            args,
            attached_server_id,
            host_start,
            host_shutdown,
            connection_open,
            connection_write,
            connection_close,
            connection_set_ui_thread_handler,
            connection_run_ui_action,
        })
    }

    /// Start the native runtime and open the FFI JSON-RPC connection.
    ///
    /// Cancellation leaves the blocking task to finish. Its unclaimed transport
    /// closes on drop, retaining the auth owner through native cleanup.
    pub(crate) async fn start(self) -> Result<(FfiReader, Arc<FfiShared>), Error> {
        tokio::task::spawn_blocking(move || self.start_blocking())
            .await
            .map_err(|e| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    format!("in-process runtime startup task failed: {e}"),
                )
            })?
    }

    fn start_blocking(self) -> Result<(FfiReader, Arc<FfiShared>), Error> {
        self.start_with_protocol_options(None)
    }

    fn start_with_protocol_options(
        self,
        protocol: Option<ProtocolAdmission>,
    ) -> Result<(FfiReader, Arc<FfiShared>), Error> {
        let (protocol_open, connection_close_and_wait) = if protocol.is_some() {
            let library = load_library(&self.library_path)?;
            let open = match protocol {
                Some(ProtocolAdmission::External(options)) => {
                    let admission = serde_json::to_vec(&options).map_err(|error| {
                        Error::with_message(ErrorKind::InvalidConfig, error.to_string())
                    })?;
                    Some(ProtocolOpen::External(
                        *bind::<ProtocolConnectionOpenFn>(
                            library,
                            b"copilot_runtime_protocol_connection_open\0",
                            &self.library_path,
                        )?,
                        admission,
                    ))
                }
                #[cfg(copilot_cli_host)]
                Some(ProtocolAdmission::OriginalStdio(parent)) => {
                    Some(ProtocolOpen::OriginalStdio(
                        *bind::<OriginalStdioOpenFn>(
                            library,
                            b"copilot_runtime_cli_stdio_connection_open\0",
                            &self.library_path,
                        )?,
                        parent,
                    ))
                }
                None => None,
            };
            (
                open,
                Some(*bind::<ConnectionCloseAndWaitFn>(
                    library,
                    b"copilot_runtime_connection_close_and_wait\0",
                    &self.library_path,
                )?),
            )
        } else {
            (None, None)
        };
        let argv = build_argv_json(
            self.cli_entrypoint.as_deref(),
            self.integration_id.as_deref(),
            &self.args,
        );
        let env = build_env_json(&self.environment);

        let (env_ptr, env_len) = match &env {
            Some(bytes) => (bytes.as_ptr(), bytes.len()),
            None => (std::ptr::null(), 0),
        };

        let owns_host = self.attached_server_id.is_none();
        let server_id = match self.attached_server_id {
            Some(server_id) => server_id,
            None => {
                if let (Some(owner), Some(start)) =
                    (self.auth_owner.as_ref(), self.host_start_with_auth_owner)
                {
                    struct Admission<'a> {
                        owner: &'a dyn crate::InProcessAuthOwner,
                        error: parking_lot::Mutex<Option<Error>>,
                    }
                    unsafe extern "C" fn bind(
                        state: *mut c_void,
                        server: u32,
                        connection: u32,
                    ) -> bool {
                        // SAFETY: startup retains this borrowed state until the call returns.
                        let admission = unsafe { &*state.cast::<Admission<'_>>() };
                        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            admission.owner.bind_startup(server, connection)
                        }));
                        match result {
                            Ok(Ok(())) => true,
                            Ok(Err(error)) => {
                                *admission.error.lock() = Some(error);
                                false
                            }
                            Err(_) => {
                                *admission.error.lock() = Some(Error::with_message(
                                    ErrorKind::InvalidConfig,
                                    "SDK startup auth binder panicked",
                                ));
                                false
                            }
                        }
                    }
                    let mut admission = Admission {
                        owner: owner.as_ref(),
                        error: parking_lot::Mutex::new(None),
                    };
                    // SAFETY: byte spans and callback state remain live throughout startup.
                    let server = unsafe {
                        start(
                            argv.as_ptr(),
                            argv.len(),
                            env_ptr,
                            env_len,
                            bind,
                            (&mut admission as *mut Admission<'_>).cast(),
                        )
                    };
                    if let Some(error) = admission.error.into_inner() {
                        return Err(error);
                    }
                    server
                } else {
                    unsafe { (self.host_start)(argv.as_ptr(), argv.len(), env_ptr, env_len) }
                }
            }
        };

        if server_id == 0 {
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                format!(
                    "copilot_runtime_host_start failed (library '{}')",
                    self.library_path.display()
                ),
            ));
        }

        let (tx, rx) = mpsc::unbounded_channel::<Vec<u8>>();
        let state_ptr = Box::into_raw(Box::new(CallbackState {
            tx: parking_lot::Mutex::new(Some(tx)),
            closing: AtomicBool::new(false),
        }));
        let connection_id = if let Some(binding) = self.session_binding.as_ref() {
            let open = self.session_connection_open.ok_or_else(|| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    "session binding export is missing",
                )
            })?;
            unsafe {
                open(
                    binding.parent_connection_id,
                    binding.native_session_id.as_ptr(),
                    binding.native_session_id.len(),
                    binding.registration_token.as_ptr(),
                    binding.registration_token.len(),
                    on_outbound,
                    state_ptr as *mut c_void,
                )
            }
        } else if let Some(open) = protocol_open {
            match open {
                ProtocolOpen::External(open, admission) => unsafe {
                    open(
                        server_id,
                        on_outbound,
                        state_ptr.cast(),
                        admission.as_ptr(),
                        admission.len(),
                    )
                },
                #[cfg(copilot_cli_host)]
                ProtocolOpen::OriginalStdio(open, parent) => unsafe {
                    open(server_id, parent, on_outbound, state_ptr.cast())
                },
            }
        } else {
            unsafe {
                (self.connection_open)(
                    server_id,
                    on_outbound,
                    state_ptr as *mut c_void,
                    std::ptr::null(),
                    0,
                    std::ptr::null(),
                    0,
                    std::ptr::null(),
                    0,
                )
            }
        };
        if connection_id == 0 {
            drop(unsafe { Box::from_raw(state_ptr) });
            if owns_host {
                unsafe { (self.host_shutdown)(server_id) };
            }
            return Err(Error::with_message(
                ErrorKind::InvalidConfig,
                "copilot_runtime_connection_open failed",
            ));
        }

        let shared = Arc::new(FfiShared {
            #[cfg(copilot_cli_host)]
            library: self.library,
            auth_owner: parking_lot::Mutex::new(self.auth_owner),
            host_shutdown: self.host_shutdown,
            owns_host,
            connection_write: self.connection_write,
            connection_close: self.connection_close,
            connection_close_and_wait,
            connection_set_ui_thread_handler: self.connection_set_ui_thread_handler,
            connection_run_ui_action: self.connection_run_ui_action,
            server_id: AtomicU32::new(server_id),
            connection_id: AtomicU32::new(connection_id),
            callback_state: AtomicPtr::new(state_ptr),
            closed: AtomicBool::new(false),
            operation_lock: parking_lot::Mutex::new(()),
            foreground_order: Arc::new(tokio::sync::Mutex::new(())),
            protocol_peers: parking_lot::Mutex::new(Some(Vec::new())),
            cleanup_done: tokio::sync::watch::channel(false).0,
            library_path: self.library_path.clone(),
        });
        if let Some(path) = self.file_logging_path.as_deref() {
            let path = path.to_str().ok_or_else(|| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    "in_process_file_logging_path must contain valid UTF-8",
                )
            })?;
            let admit = self.file_logging_connection_admit.ok_or_else(|| {
                Error::with_message(
                    ErrorKind::InvalidConfig,
                    "file logging admission export is missing",
                )
            })?;
            // SAFETY: the live connection belongs to this image, and the path bytes remain borrowed.
            if !unsafe { admit(connection_id, path.as_ptr(), path.len()) } {
                return Err(Error::with_message(
                    ErrorKind::InvalidConfig,
                    "the runtime rejected the explicit file logging grant",
                ));
            }
        }

        debug!(
            library = %self.library_path.display(),
            server_id, connection_id, "FFI runtime host started"
        );

        let reader = FfiReader {
            rx,
            leftover: Vec::new(),
            pos: 0,
        };
        Ok((reader, shared))
    }
}

fn bind<'lib, T>(
    lib: &'lib Library,
    symbol: &[u8],
    library_path: &Path,
) -> Result<libloading::Symbol<'lib, T>, Error> {
    match unsafe { lib.get::<T>(symbol) } {
        Ok(export) => Ok(export),
        Err(e) => Err(Error::with_message(
            ErrorKind::InvalidConfig,
            format!(
                "in-process runtime library '{}' is missing an expected export ({}): {e}",
                library_path.display(),
                String::from_utf8_lossy(symbol.strip_suffix(b"\0").unwrap_or(symbol))
            ),
        )),
    }
}

fn bind_optional<T: Copy>(
    lib: &Library,
    symbol: &[u8],
    library_path: &Path,
    enabled: bool,
) -> Result<Option<T>, Error> {
    if enabled {
        Ok(Some(*bind::<T>(lib, symbol, library_path)?))
    } else {
        Ok(None)
    }
}

/// Loads the runtime cdylib once per process and never unloads it, returning a
/// `'static` reference. Subsequent loads of the same path reuse the first
/// handle.
///
/// The library stays mapped because native worker threads can outlive an
/// individual connection teardown.
fn load_library(library_path: &Path) -> Result<&'static Library, Error> {
    static LIBRARIES: OnceLock<parking_lot::Mutex<HashMap<PathBuf, &'static Library>>> =
        OnceLock::new();
    let cache = LIBRARIES.get_or_init(|| parking_lot::Mutex::new(HashMap::new()));

    let mut guard = cache.lock();
    if let Some(lib) = guard.get(library_path) {
        return Ok(*lib);
    }

    let lib = unsafe { Library::new(library_path) }.map_err(|e| {
        Error::with_message(
            ErrorKind::InvalidConfig,
            format!(
                "failed to load in-process runtime library '{}': {e}",
                library_path.display()
            ),
        )
    })?;
    // Leak the library so it is never unloaded for the process lifetime.
    let leaked: &'static Library = Box::leak(Box::new(lib));
    guard.insert(library_path.to_path_buf(), leaked);
    Ok(leaked)
}

/// The natural platform shared-library file name for the runtime cdylib — the
/// `.node` file renamed to what the Rust cdylib would be called on this OS.
fn natural_library_name() -> &'static str {
    if cfg!(windows) {
        "copilot_runtime.dll"
    } else if cfg!(target_os = "macos") {
        "libcopilot_runtime.dylib"
    } else {
        "libcopilot_runtime.so"
    }
}

/// The package prebuild folder name for the current host.
pub(crate) fn prebuilds_folder() -> Option<String> {
    let platform = if cfg!(target_os = "windows") {
        "win32"
    } else if cfg!(target_os = "macos") {
        "darwin"
    } else if cfg!(all(target_os = "linux", target_env = "musl")) {
        "linuxmusl"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else {
        return None;
    };
    let arch = if cfg!(target_arch = "x86_64") {
        "x64"
    } else if cfg!(target_arch = "aarch64") {
        "arm64"
    } else {
        return None;
    };
    Some(format!("{platform}-{arch}"))
}

fn resolve_library_path(entrypoint: &Path) -> Result<PathBuf, Error> {
    let dir = entrypoint.parent().ok_or_else(|| {
        Error::with_message(
            ErrorKind::InvalidConfig,
            format!(
                "could not determine directory for CLI entrypoint '{}'",
                entrypoint.display()
            ),
        )
    })?;

    // Bundled/flat layout: natural shared-library name next to the CLI.
    let flat = dir.join(natural_library_name());
    if flat.is_file() {
        return Ok(flat);
    }

    let adjacent = dir.join("runtime.node");
    if adjacent.is_file() {
        return Ok(adjacent);
    }

    // Development package layout.
    let prebuilds =
        prebuilds_folder().map(|folder| dir.join("prebuilds").join(folder).join("runtime.node"));
    if let Some(prebuilds_path) = &prebuilds
        && prebuilds_path.is_file()
    {
        return Ok(prebuilds_path.clone());
    }

    Err(Error::with_message(
        ErrorKind::BinaryNotFound {
            name: natural_library_name().into(),
            hint: Some(format!(
                "native runtime library not found next to '{}'. Enable the \
                 `in-process` feature or set COPILOT_CLI_PATH to a compatible CLI package.",
                entrypoint.display()
            )),
        },
        "native runtime library not found",
    ))
}

#[cfg(windows)]
fn path_for_child_process(path: PathBuf) -> PathBuf {
    use std::ffi::OsString;
    use std::os::windows::ffi::{OsStrExt, OsStringExt};

    const VERBATIM_PREFIX: &[u16] = &[b'\\' as u16, b'\\' as u16, b'?' as u16, b'\\' as u16];
    const UNC_PREFIX: &[u16] = &[b'U' as u16, b'N' as u16, b'C' as u16, b'\\' as u16];

    let encoded: Vec<u16> = path.as_os_str().encode_wide().collect();
    let Some(stripped) = encoded.strip_prefix(VERBATIM_PREFIX) else {
        return path;
    };
    let normalized = if let Some(unc_path) = stripped.strip_prefix(UNC_PREFIX) {
        let mut result = vec![b'\\' as u16, b'\\' as u16];
        result.extend_from_slice(unc_path);
        result
    } else {
        stripped.to_vec()
    };
    PathBuf::from(OsString::from_wide(&normalized))
}

#[cfg(not(windows))]
fn path_for_child_process(path: PathBuf) -> PathBuf {
    path
}

fn build_argv_json(
    entrypoint: Option<&Path>,
    integration_id: Option<&str>,
    extra_args: &[String],
) -> Vec<u8> {
    let mut argv = Vec::new();
    if let Some(entrypoint) = entrypoint {
        let entrypoint_str = entrypoint.to_string_lossy().into_owned();
        if entrypoint
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext.eq_ignore_ascii_case("js"))
        {
            argv.push("node".to_string());
        }
        argv.extend([
            entrypoint_str,
            "--embedded-host".to_string(),
            "--no-auto-update".to_string(),
        ]);
    }
    if let Some(integration_id) = integration_id {
        argv.push(format!("--cli-login-sync-integration-id={integration_id}"));
    }
    argv.extend_from_slice(extra_args);
    serde_json::to_vec(&argv).expect("argv serializes")
}

fn build_env_json(environment: &[(String, String)]) -> Option<Vec<u8>> {
    if environment.is_empty() {
        return None;
    }
    let map: serde_json::Map<String, serde_json::Value> = std::env::vars_os()
        .filter_map(|(key, value)| {
            let key = key.into_string().ok()?;
            let value = match value.into_string() {
                Ok(value) => value,
                Err(_) => {
                    let name = if cfg!(windows) {
                        key.to_uppercase()
                    } else {
                        key.clone()
                    };
                    if !matches!(
                        name.as_str(),
                        "http_proxy"
                            | "HTTP_PROXY"
                            | "https_proxy"
                            | "HTTPS_PROXY"
                            | "COPILOT_PROXY_KERBEROS_SPN"
                            | "TMPDIR"
                    ) {
                        return None;
                    }
                    // Match native startup capture without falling back to another setting.
                    "\u{FFFD}".into()
                }
            };
            Some((key, value))
        })
        .chain(environment.iter().cloned())
        .map(|(k, v)| {
            let key = if cfg!(windows) { k.to_uppercase() } else { k };
            (key, serde_json::Value::String(v))
        })
        .collect();
    Some(serde_json::to_vec(&map).expect("env serializes"))
}

#[cfg(test)]
mod tests;

#[cfg(test)]
pub(crate) mod initiation_tests;

#[cfg(test)]
mod shutdown_tests;

#[cfg(test)]
mod auth_owner_tests;
