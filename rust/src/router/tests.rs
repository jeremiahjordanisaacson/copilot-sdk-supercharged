/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

#![cfg(test)]

use std::pin::Pin;
use std::sync::atomic::AtomicBool;
use std::task::{Context, Poll};
use std::time::Duration;

use futures_util::task::AtomicWaker;
use serde_json::json;
use tokio::io::{
    AsyncBufReadExt, AsyncReadExt, AsyncWrite, AsyncWriteExt, BufReader, DuplexStream,
};
use tokio::time::timeout;
use tokio_util::sync::CancellationToken;

use super::*;

const DEADLINE: Duration = Duration::from_secs(5);

#[test]
fn terminal_connection_rejects_late_registrations() {
    let router = SessionRouter::new();
    let id = SessionId::new("closed-connection");
    router.event_sink().close();
    let mut registration = router.register(&id);
    assert_eq!(
        registration.channels.notifications.try_recv().err(),
        Some(mpsc::error::TryRecvError::Disconnected)
    );
}

impl SessionRouter {
    pub(crate) fn initialization_count(&self) -> usize {
        self.initializations.lock().len()
    }
}

#[test]
fn rpc_callback_tracking_is_counted_and_scoped_to_the_session_and_router() {
    let router = SessionRouter::new();
    let session_id = SessionId::from("callback-session");
    let first = router.track_rpc_callback(&session_id);
    let second = router.track_rpc_callback(&session_id);
    assert!(router.has_active_rpc_callback(&session_id));
    assert!(!router.has_active_rpc_callback(&SessionId::from("other-session")));
    assert!(!SessionRouter::new().has_active_rpc_callback(&session_id));
    drop(first);
    assert!(router.has_active_rpc_callback(&session_id));
    drop(second);
    assert!(!router.has_active_rpc_callback(&session_id));
}

#[derive(Clone, Copy, Debug)]
enum BlockAt {
    Write,
    Flush,
}

#[derive(Default)]
struct WriteGate {
    released: AtomicBool,
    waker: AtomicWaker,
    entered: CancellationToken,
    dropped: CancellationToken,
}

impl WriteGate {
    fn poll(&self, cx: &mut Context<'_>) -> Poll<()> {
        self.waker.register(cx.waker());
        if self.released.load(Ordering::Acquire) {
            Poll::Ready(())
        } else {
            self.entered.cancel();
            Poll::Pending
        }
    }

    fn release(&self) {
        self.released.store(true, Ordering::Release);
        self.waker.wake();
    }
}

struct GatedWriter {
    writer: DuplexStream,
    gate: Arc<WriteGate>,
    block_at: BlockAt,
}

impl AsyncWrite for GatedWriter {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        bytes: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        if matches!(self.block_at, BlockAt::Write) && self.gate.poll(cx).is_pending() {
            return Poll::Pending;
        }
        Pin::new(&mut self.writer).poll_write(cx, bytes)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        if matches!(self.block_at, BlockAt::Flush) && self.gate.poll(cx).is_pending() {
            return Poll::Pending;
        }
        Pin::new(&mut self.writer).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.writer).poll_shutdown(cx)
    }
}

impl Drop for GatedWriter {
    fn drop(&mut self) {
        self.gate.dropped.cancel();
    }
}

#[derive(Clone, Copy, Debug)]
enum Unroutable {
    MissingSessionId,
    UnknownSession,
    ClosedReceiver,
}

struct Peer {
    client: crate::Client,
    input: DuplexStream,
    output: BufReader<DuplexStream>,
    gate: Arc<WriteGate>,
}

impl Peer {
    fn new(block_at: BlockAt) -> Self {
        let (reader, input) = tokio::io::duplex(4096);
        let (writer, output) = tokio::io::duplex(4096);
        let gate = Arc::new(WriteGate::default());
        let client = crate::Client::from_streams(
            crate::ClientMode::CodingAgent,
            reader,
            GatedWriter {
                writer,
                gate: gate.clone(),
                block_at,
            },
            std::path::PathBuf::from("."),
        )
        .unwrap();
        Self {
            client,
            input,
            output: BufReader::new(output),
            gate,
        }
    }

    async fn send(&mut self, message: &impl serde::Serialize) {
        let body = serde_json::to_vec(message).unwrap();
        timeout(DEADLINE, async {
            self.input
                .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
                .await
                .unwrap();
            self.input.write_all(&body).await.unwrap();
        })
        .await
        .expect("peer could not inject a complete request");
    }

    async fn reject(&mut self, id: u64, kind: Unroutable) {
        let params = match kind {
            Unroutable::MissingSessionId => json!({}),
            Unroutable::UnknownSession => json!({"sessionId": "gone"}),
            Unroutable::ClosedReceiver => {
                drop(self.client.register_session(&SessionId::new("closed")));
                json!({"sessionId": "closed"})
            }
        };
        self.send(&JsonRpcRequest::new(id, "skillProvider.list", Some(params)))
            .await;
    }

    async fn read(&mut self) -> JsonRpcResponse {
        timeout(DEADLINE, async {
            let mut header = String::new();
            self.output.read_line(&mut header).await.unwrap();
            let length = header
                .strip_prefix("Content-Length: ")
                .expect("missing response header")
                .trim()
                .parse::<usize>()
                .unwrap();
            header.clear();
            self.output.read_line(&mut header).await.unwrap();
            assert_eq!(header, "\r\n");
            let mut body = vec![0; length];
            self.output.read_exact(&mut body).await.unwrap();
            serde_json::from_slice(&body).unwrap()
        })
        .await
        .expect("error frame was not delivered")
    }

    async fn stop(&self) {
        self.client.force_stop();
        timeout(DEADLINE, self.gate.dropped.cancelled())
            .await
            .expect("writer did not exit");
    }
}

#[track_caller]
fn assert_error(response: JsonRpcResponse, id: u64, kind: Unroutable) {
    assert_eq!(response.id, id);
    assert!(response.result.is_none());
    let error = response.error.expect("unroutable request received success");
    let (code, message) = match kind {
        Unroutable::MissingSessionId => (
            error_codes::INVALID_PARAMS,
            "missing required field: sessionId",
        ),
        Unroutable::UnknownSession => (error_codes::INTERNAL_ERROR, "Session not found: gone"),
        Unroutable::ClosedReceiver => (error_codes::INTERNAL_ERROR, "Session not found: closed"),
    };
    assert_eq!(error.code, i64::from(code));
    assert_eq!(error.message, message);
}

#[tokio::test]
async fn stalled_rejection_preserves_live_routing_and_eventual_error_delivery() {
    for block_at in [BlockAt::Write, BlockAt::Flush] {
        for kind in [
            Unroutable::MissingSessionId,
            Unroutable::UnknownSession,
            Unroutable::ClosedReceiver,
        ] {
            let mut peer = Peer::new(block_at);
            let mut live = peer
                .client
                .register_session(&SessionId::new("live"))
                .channels
                .requests;
            peer.reject(101, kind).await;
            timeout(DEADLINE, peer.gate.entered.cancelled())
                .await
                .unwrap();
            peer.send(&crate::jsonrpc::JsonRpcNotification {
                jsonrpc: "2.0".into(),
                method: "$/cancelRequest".into(),
                params: Some(json!({"id": 101})),
            })
            .await;
            peer.send(&JsonRpcRequest::new(
                102,
                "skillProvider.list",
                Some(json!({"sessionId": "live"})),
            ))
            .await;
            let routed = timeout(DEADLINE, live.recv())
                .await
                .unwrap_or_else(|_| panic!("live callback stalled behind {kind:?} at {block_at:?}"))
                .expect("live channel closed");
            assert_eq!(routed.id, 102);
            assert!(
                peer.client
                    .inner
                    .rpc
                    .cancellable_requests
                    .claim(101)
                    .is_none()
            );
            assert!(
                !peer
                    .client
                    .inner
                    .rpc
                    .connection_closed_token()
                    .is_cancelled()
            );
            peer.gate.release();
            assert_error(peer.read().await, 101, kind);
            peer.stop().await;
        }
    }
}

#[tokio::test]
async fn delayed_writer_preserves_a_large_healthy_burst_without_disconnect() {
    const BURST: u64 = 512;
    for block_at in [BlockAt::Write, BlockAt::Flush] {
        let mut peer = Peer::new(block_at);
        let mut live = peer
            .client
            .register_session(&SessionId::new("live"))
            .channels
            .requests;
        peer.reject(100, Unroutable::UnknownSession).await;
        timeout(DEADLINE, peer.gate.entered.cancelled())
            .await
            .unwrap();
        for id in 101..100 + BURST {
            peer.reject(id, Unroutable::UnknownSession).await;
        }
        peer.send(&JsonRpcRequest::new(
            100 + BURST,
            "skillProvider.list",
            Some(json!({"sessionId": "live"})),
        ))
        .await;
        assert_eq!(
            timeout(DEADLINE, live.recv()).await.unwrap().unwrap().id,
            100 + BURST
        );
        assert!(
            !peer
                .client
                .inner
                .rpc
                .connection_closed_token()
                .is_cancelled()
        );
        for id in 100..100 + BURST {
            assert!(
                peer.client
                    .inner
                    .rpc
                    .cancellable_requests
                    .claim(id)
                    .is_none()
            );
        }
        peer.gate.release();
        for id in 100..100 + BURST {
            assert_error(peer.read().await, id, Unroutable::UnknownSession);
        }
        peer.stop().await;
    }
}

#[tokio::test]
async fn pending_rejection_does_not_retain_the_last_client() {
    for block_at in [BlockAt::Write, BlockAt::Flush] {
        let mut peer = Peer::new(block_at);
        let mut live = peer
            .client
            .register_session(&SessionId::new("live"))
            .channels
            .requests;
        peer.reject(101, Unroutable::UnknownSession).await;
        timeout(DEADLINE, peer.gate.entered.cancelled())
            .await
            .unwrap();
        let weak = Arc::downgrade(&peer.client.inner);
        let gate = peer.gate.clone();
        drop(peer);
        assert!(
            weak.upgrade().is_none(),
            "error delivery retained the client"
        );
        timeout(DEADLINE, gate.dropped.cancelled()).await.unwrap();
        assert!(timeout(DEADLINE, live.recv()).await.unwrap().is_none());
    }
}

#[tokio::test]
async fn force_stop_releases_pending_rejections() {
    for kind in [
        Unroutable::MissingSessionId,
        Unroutable::UnknownSession,
        Unroutable::ClosedReceiver,
    ] {
        let mut peer = Peer::new(BlockAt::Write);
        let mut live = peer
            .client
            .register_session(&SessionId::new("live"))
            .channels
            .requests;
        peer.reject(101, kind).await;
        timeout(DEADLINE, peer.gate.entered.cancelled())
            .await
            .unwrap();
        peer.stop().await;
        assert!(timeout(DEADLINE, live.recv()).await.unwrap().is_none());
        assert!(
            peer.client
                .inner
                .rpc
                .cancellable_requests
                .claim(101)
                .is_none()
        );
    }
}

#[tokio::test]
async fn client_stop_does_not_await_router_rejection_delivery() {
    for block_at in [BlockAt::Write, BlockAt::Flush] {
        let mut peer = Peer::new(block_at);
        peer.client.inner.router.ensure_started(&peer.client.inner);
        peer.reject(101, Unroutable::UnknownSession).await;
        timeout(DEADLINE, peer.gate.entered.cancelled())
            .await
            .unwrap();
        timeout(DEADLINE, peer.client.stop())
            .await
            .expect("stop awaited rejection output")
            .unwrap();
        timeout(DEADLINE, peer.gate.dropped.cancelled())
            .await
            .unwrap();
    }
}

#[tokio::test]
async fn late_rejection_delivery_does_not_retire_a_reused_request_id() {
    let mut peer = Peer::new(BlockAt::Write);
    let mut live = peer
        .client
        .register_session(&SessionId::new("live"))
        .channels
        .requests;
    peer.reject(101, Unroutable::UnknownSession).await;
    timeout(DEADLINE, peer.gate.entered.cancelled())
        .await
        .unwrap();
    peer.send(&JsonRpcRequest::new(
        101,
        "skillProvider.list",
        Some(json!({"sessionId": "live"})),
    ))
    .await;
    assert_eq!(
        timeout(DEADLINE, live.recv()).await.unwrap().unwrap().id,
        101
    );
    let pending = peer
        .client
        .inner
        .rpc
        .cancellable_requests
        .claim(101)
        .expect("new request lost cancellation");
    peer.gate.release();
    assert_error(peer.read().await, 101, Unroutable::UnknownSession);
    peer.send(&crate::jsonrpc::JsonRpcNotification {
        jsonrpc: "2.0".into(),
        method: "$/cancelRequest".into(),
        params: Some(json!({"id": 101})),
    })
    .await;
    timeout(DEADLINE, pending.cancellation().cancelled())
        .await
        .expect("old completion retired new cancellation");
    peer.stop().await;
}

#[tokio::test]
async fn failed_router_rejection_write_closes_connection_and_wakes_pending_work() {
    let mut peer = Peer::new(BlockAt::Write);
    let mut live = peer
        .client
        .register_session(&SessionId::new("live"))
        .channels
        .requests;
    peer.reject(101, Unroutable::UnknownSession).await;
    timeout(DEADLINE, peer.gate.entered.cancelled())
        .await
        .unwrap();
    let pending = peer
        .client
        .inner
        .rpc
        .send_request_with_inline_callback("ping", None, None);
    tokio::pin!(pending);
    assert!(futures_util::poll!(&mut pending).is_pending());
    let Peer {
        client,
        input: _input,
        output,
        gate,
    } = peer;
    drop(output);
    gate.release();
    timeout(
        DEADLINE,
        client.inner.rpc.connection_closed_token().cancelled(),
    )
    .await
    .expect("write failure left connection open");
    let error = timeout(DEADLINE, pending)
        .await
        .expect("pending request did not wake")
        .expect_err("request succeeded on broken output");
    assert_eq!(error.kind(), &crate::ErrorKind::Io);
    assert!(timeout(DEADLINE, live.recv()).await.unwrap().is_none());
    timeout(DEADLINE, gate.dropped.cancelled()).await.unwrap();
    client.force_stop();
}

/// Unlike a failed write above, a write abandoned when the runtime exits
/// reports the exit, even once the closed writer has dropped the frame.
#[tokio::test]
async fn runtime_exit_reports_its_cause_to_a_request_whose_write_it_abandoned() {
    let peer = Peer::new(BlockAt::Write);
    let pending = peer
        .client
        .inner
        .rpc
        .send_request_with_inline_callback("ping", None, None);
    tokio::pin!(pending);
    assert!(futures_util::poll!(&mut pending).is_pending());
    timeout(DEADLINE, peer.gate.entered.cancelled())
        .await
        .expect("request write did not start");
    peer.client
        .inner
        .rpc
        .close_after_exit("the CLI process exited (exit status: 0)");
    timeout(DEADLINE, peer.gate.dropped.cancelled())
        .await
        .expect("exit left the blocked writer running");
    let error = timeout(DEADLINE, pending)
        .await
        .expect("pending request did not wake")
        .expect_err("request succeeded after runtime exit");
    assert_eq!(
        error.kind(),
        &crate::ErrorKind::Protocol(crate::ProtocolErrorKind::RequestCancelled)
    );
    assert!(
        error.to_string().contains("the CLI process exited"),
        "{error}"
    );
    peer.client.force_stop();
}

#[tokio::test]
async fn peer_eof_retires_routing_with_a_committed_rejection_pending() {
    let mut peer = Peer::new(BlockAt::Write);
    let mut live = peer
        .client
        .register_session(&SessionId::new("live"))
        .channels
        .requests;
    peer.reject(101, Unroutable::UnknownSession).await;
    timeout(DEADLINE, peer.gate.entered.cancelled())
        .await
        .unwrap();
    peer.input.shutdown().await.unwrap();
    timeout(
        DEADLINE,
        peer.client.inner.rpc.connection_closed_token().cancelled(),
    )
    .await
    .unwrap();
    assert!(timeout(DEADLINE, live.recv()).await.unwrap().is_none());
    assert!(
        peer.client
            .inner
            .rpc
            .cancellable_requests
            .claim(101)
            .is_none()
    );
    peer.stop().await;
}

fn session_id() -> SessionId {
    SessionId::new("router-ownership")
}

/// Session and lifecycle notifications share the client's one dispatcher.
/// Every session event reaches its session whole and in order, and every
/// lifecycle event its subscriber.
#[tokio::test]
async fn shared_notifications_reach_sessions_and_lifecycle_subscribers_whole() {
    let data = |n: u64| {
        json!({
            "deltaContent": format!("chunk {n}"),
            "nested": {"index": n, "items": [n, {"deep": [true, null, "x"]}]},
        })
    };
    {
        let mut peer = Peer::new(BlockAt::Write);
        let mut lifecycle = peer.client.subscribe_lifecycle();
        let mut events = peer
            .client
            .register_session(&SessionId::new("live"))
            .channels
            .notifications;
        for n in 0..3 {
            peer.send(&crate::jsonrpc::JsonRpcNotification {
                jsonrpc: "2.0".into(),
                method: "session.event".into(),
                params: Some(json!({
                    "sessionId": "live",
                    "event": {
                        "id": format!("e{n}"),
                        "timestamp": "2025-01-01T00:00:00Z",
                        "type": "assistant.message_delta",
                        "data": data(n),
                    },
                })),
            })
            .await;
            peer.send(&crate::jsonrpc::JsonRpcNotification {
                jsonrpc: "2.0".into(),
                method: "session.lifecycle".into(),
                params: Some(json!({"type": "session.updated", "sessionId": format!("s{n}")})),
            })
            .await;
        }
        for n in 0..3 {
            let routed = timeout(DEADLINE, events.recv())
                .await
                .unwrap_or_else(|_| panic!("session event {n} stalled"))
                .expect("session channel closed")
                .notification;
            assert_eq!(routed.session_id, SessionId::new("live"));
            assert_eq!(routed.event.id, format!("e{n}"));
            assert_eq!(routed.event.event_type, "assistant.message_delta");
            assert_eq!(routed.event.data, data(n));
            let changed = timeout(DEADLINE, lifecycle.recv())
                .await
                .unwrap_or_else(|_| panic!("lifecycle event {n} stalled"))
                .expect("lifecycle channel closed");
            assert_eq!(changed.session_id, SessionId::new(format!("s{n}")));
        }
        peer.stop().await;
    }
}

fn typed_event(session_id: &str, id: &str, event_type: &str, request_id: &str) -> Value {
    json!({
        "sessionId": session_id,
        "event": {
            "id": id,
            "timestamp": "2025-01-01T00:00:00Z",
            "type": event_type,
            "data": {"requestId": request_id},
        },
    })
}

fn session_event(session_id: &str, id: &str) -> Value {
    typed_event(session_id, id, "external_tool.requested", id)
}

/// Delivered event IDs, with settled ones marked.
fn drain(receiver: &mut mpsc::UnboundedReceiver<RoutedEvent>) -> Vec<String> {
    std::iter::from_fn(|| receiver.try_recv().ok())
        .map(|routed| {
            let id = routed.notification.event.id;
            if routed.settled {
                format!("{id} (settled)")
            } else {
                id
            }
        })
        .collect()
}

/// A held request whose completion was also held reaches the session for
/// observers only; one still open is dispatched as usual.
#[test]
fn held_requests_answered_before_registration_are_delivered_settled() {
    let router = SessionRouter::new();
    let sink = router.event_sink();
    for (id, event_type, request_id) in [
        ("p", "permission.requested", "r1"),
        ("t", "external_tool.requested", "r2"),
        ("e", "elicitation.requested", "r3"),
        ("p-done", "permission.completed", "r1"),
        ("t-other", "external_tool.completed", "other"),
        ("e-mismatch", "permission.completed", "r3"),
    ] {
        sink.route(typed_event("held", id, event_type, request_id));
    }

    let mut registration = router.register(&SessionId::new("held"));
    assert_eq!(
        drain(&mut registration.channels.notifications),
        ["p (settled)", "t", "e", "p-done", "t-other", "e-mismatch"]
    );
}

/// An event that arrives before its session registers is held, then
/// delivered ahead of the events that follow registration.
#[test]
fn events_for_an_unregistered_session_wait_for_its_registration() {
    let router = SessionRouter::new();
    let sink = router.event_sink();
    sink.route(session_event("early", "e0"));
    sink.route(session_event("early", "e1"));

    let mut registration = router.register(&SessionId::new("early"));
    sink.route(session_event("early", "e2"));
    assert_eq!(
        drain(&mut registration.channels.notifications),
        ["e0", "e1", "e2"]
    );
}

/// Past the bound on held events, the IDs seen first are dropped so a
/// session that registers later still gets its early events.
#[test]
fn held_events_past_the_bound_drop_the_oldest_unregistered_ids() {
    let router = SessionRouter::new();
    let sink = router.event_sink();
    for n in 0..MAX_UNCLAIMED_EVENTS {
        sink.route(session_event("never", &format!("n{n}")));
    }
    sink.route(session_event("late", "l0"));

    let mut never = router.register(&SessionId::new("never"));
    let mut late = router.register(&SessionId::new("late"));
    assert!(drain(&mut never.channels.notifications).is_empty());
    assert_eq!(drain(&mut late.channels.notifications), ["l0"]);
}

fn sized_event(session_id: &str, id: &str, payload_bytes: usize) -> Value {
    json!({
        "sessionId": session_id,
        "event": {
            "id": id,
            "timestamp": "2025-01-01T00:00:00Z",
            "type": "assistant.message",
            "data": {"content": "x".repeat(payload_bytes)},
        },
    })
}

/// What an ID that is not registered yet keeps held, by size. Each row runs
/// its steps in order against a fresh router.
#[test]
fn held_events_stay_within_the_byte_budget() {
    const MIB: usize = 1024 * 1024;
    enum Step {
        /// Route this many events of this many payload bytes to the session.
        Route(&'static str, usize, usize),
        /// Register the session and expect every event routed to it.
        Whole(&'static str, usize),
        /// Register the session and expect only its newest events, within the budget.
        Trimmed(&'static str, usize, usize),
    }
    use Step::{Route, Trimmed, Whole};
    let rows: [(&str, &[Step]); 4] = [
        (
            "under the budget nothing is dropped",
            &[
                Route("held", 15, MIB),
                Route("late", 1, 16),
                Whole("held", 15),
                Whole("late", 1),
            ],
        ),
        (
            "one event over the budget is still held",
            &[Route("held", 1, 20 * MIB), Whole("held", 1)],
        ),
        (
            "a registration releases the budget its events used",
            &[
                Route("first", 15, MIB),
                Whole("first", 15),
                Route("second", 15, MIB),
                Whole("second", 15),
            ],
        ),
        (
            "past the budget the oldest held events are dropped",
            &[
                Route("held", 40, MIB),
                Route("late", 1, 16),
                Whole("late", 1),
                Trimmed("held", 40, MIB),
            ],
        ),
    ];
    for (name, steps) in rows {
        let router = SessionRouter::new();
        let sink = router.event_sink();
        for step in steps {
            match *step {
                Route(session, count, payload) => {
                    for n in 0..count {
                        sink.route(sized_event(session, &format!("{session}{n}"), payload));
                    }
                }
                Whole(session, count) => {
                    let mut registration = router.register(&SessionId::new(session));
                    let expected: Vec<String> =
                        (0..count).map(|n| format!("{session}{n}")).collect();
                    assert_eq!(
                        drain(&mut registration.channels.notifications),
                        expected,
                        "{name}: {session}"
                    );
                }
                Trimmed(session, count, payload) => {
                    let mut registration = router.register(&SessionId::new(session));
                    let delivered = drain(&mut registration.channels.notifications);
                    assert_eq!(
                        delivered.last(),
                        Some(&format!("{session}{}", count - 1)),
                        "{name}: newest event kept"
                    );
                    assert!(
                        delivered.len() * payload <= MAX_UNCLAIMED_BYTES,
                        "{name}: {} of {count} events held",
                        delivered.len()
                    );
                }
            }
        }
    }
}

#[test]
fn only_detaching_retains_events_for_a_later_registration() {
    for (detached, event, expected) in [
        (true, "after-rejoin", &["after-rejoin", "live"][..]),
        (false, "stale", &["live"][..]),
    ] {
        let router = SessionRouter::new();
        let sink = router.event_sink();
        let first = router.register(&session_id());
        assert!(if detached {
            router.unregister_detached(&session_id(), first.token)
        } else {
            router.unregister_owned(&session_id(), first.token)
        });
        sink.route(session_event(session_id().as_str(), event));
        let mut second = router.register(&session_id());
        sink.route(session_event(session_id().as_str(), "live"));
        assert_eq!(drain(&mut second.channels.notifications), expected);
    }
}

/// The dispatcher ends a detached registration where its reply was read.
/// Events read before that reach it until it is unregistered and are dropped
/// after, even when the ID registers again first; the runtime cancelled their
/// requests. Events read after the reply are held for the next registration,
/// however late the detached session unregisters.
#[test]
fn a_detach_divides_events_where_its_reply_was_read() {
    let router = SessionRouter::new();
    let (dispatcher, mut inputs) = mpsc::unbounded_channel();
    router.connect_dispatcher(&dispatcher);
    let sink = router.event_sink();
    let mut next_input = || inputs.try_recv().expect("nothing queued");
    let claim = |input| match input {
        DispatcherInput::Claim(id, token) => (id, token),
        _ => panic!("expected a claim"),
    };

    // The caller unregisters before the dispatcher reaches the reply.
    let mut first = router.register(&session_id());
    let (id, token) = claim(next_input());
    sink.claim(&id, token);
    sink.route(session_event(session_id().as_str(), "before-reply"));
    router.detach_replied(Some(first.token))(&session_id());
    let DispatcherInput::Detached(detached_id, detached_token) = next_input() else {
        panic!("detach reply queued no mark");
    };
    assert!(router.unregister_detached(&session_id(), first.token));
    let mut second = router.register(&session_id());
    let (id, token) = claim(next_input());
    sink.route(typed_event(
        session_id().as_str(),
        "cancelled",
        "external_tool.requested",
        "t1",
    ));
    sink.detached(&detached_id, detached_token);
    sink.route(session_event(session_id().as_str(), "after-reply"));
    sink.claim(&id, token);
    assert_eq!(drain(&mut first.channels.notifications), ["before-reply"]);
    assert_eq!(drain(&mut second.channels.notifications), ["after-reply"]);

    // The caller unregisters after the dispatcher passed the reply, and after
    // a request the rejoined session must answer.
    router.detach_replied(Some(second.token))(&session_id());
    let DispatcherInput::Detached(detached_id, detached_token) = next_input() else {
        panic!("detach reply queued no mark");
    };
    sink.detached(&detached_id, detached_token);
    sink.route(typed_event(
        session_id().as_str(),
        "after-rejoin",
        "external_tool.requested",
        "t2",
    ));
    assert!(!router.unregister_detached(&session_id(), second.token));
    let mut third = router.register(&session_id());
    let (id, token) = claim(next_input());
    sink.claim(&id, token);
    assert!(drain(&mut second.channels.notifications).is_empty());
    assert_eq!(drain(&mut third.channels.notifications), ["after-rejoin"]);
}

/// A detach ends only its own registration. A resume that registered the ID
/// before the detach reply was read keeps its registration and its events,
/// including those read before the reply.
#[test]
fn a_detach_never_ends_a_concurrent_registration() {
    let router = SessionRouter::new();
    let (dispatcher, mut inputs) = mpsc::unbounded_channel();
    router.connect_dispatcher(&dispatcher);
    let sink = router.event_sink();
    let detaching = router.register(&session_id());
    let Ok(DispatcherInput::Claim(id, token)) = inputs.try_recv() else {
        panic!("registration queued no claim");
    };
    sink.claim(&id, token);
    let mut resumed = router.register(&session_id());
    let Ok(DispatcherInput::Claim(id, token)) = inputs.try_recv() else {
        panic!("registration queued no claim");
    };

    router.detach_replied(Some(detaching.token))(&session_id());
    let Ok(DispatcherInput::Detached(detached_id, detached_token)) = inputs.try_recv() else {
        panic!("detach reply queued no mark");
    };
    sink.route(session_event(session_id().as_str(), "before-reply"));
    sink.claim(&id, token);
    sink.detached(&detached_id, detached_token);
    assert!(!router.unregister_detached(&session_id(), detaching.token));
    sink.route(session_event(session_id().as_str(), "live"));

    assert!(router.is_registered_owner(&session_id(), resumed.token));
    assert_eq!(
        drain(&mut resumed.channels.notifications),
        ["before-reply", "live"]
    );
}

/// Events already held for an ID when its detach reply is read were read
/// before it: they are dropped when the dispatcher reaches the mark, and a
/// registration made after the reply never takes them.
#[test]
fn a_detach_drops_events_already_held_before_its_reply() {
    let router = SessionRouter::new();
    let (dispatcher, mut inputs) = mpsc::unbounded_channel();
    router.connect_dispatcher(&dispatcher);
    let sink = router.event_sink();
    sink.route(typed_event(
        session_id().as_str(),
        "held",
        "permission.requested",
        "r1",
    ));

    router.detach_replied(None)(&session_id());
    let Ok(DispatcherInput::Detached(detached_id, detached_token)) = inputs.try_recv() else {
        panic!("detach reply queued no mark");
    };
    let mut later = router.register(&session_id());
    let Ok(DispatcherInput::Claim(id, token)) = inputs.try_recv() else {
        panic!("registration queued no claim");
    };
    sink.detached(&detached_id, detached_token);
    sink.route(session_event(session_id().as_str(), "after-reply"));
    sink.claim(&id, token);
    assert_eq!(drain(&mut later.channels.notifications), ["after-reply"]);
}

#[test]
fn a_deleted_lifecycle_supersedes_a_queued_delete_reply() {
    for register_before_dispatch in [false, true] {
        let router = SessionRouter::new();
        let (dispatcher, mut inputs) = mpsc::unbounded_channel();
        router.connect_dispatcher(&dispatcher);
        let sink = router.event_sink();
        sink.route(session_event(session_id().as_str(), "old-conversation"));

        let deleted = router.delete_replied(&session_id());
        // The reader gets ahead while the dispatcher is held back.
        for (method, params) in [
            (
                "session.lifecycle",
                json!({"type": "session.deleted", "sessionId": session_id()}),
            ),
            (
                "session.event",
                session_event(session_id().as_str(), "replacement-before-reply"),
            ),
        ] {
            assert!(
                dispatcher
                    .send(DispatcherInput::Notification(
                        crate::jsonrpc::JsonRpcNotification {
                            jsonrpc: "2.0".into(),
                            method: method.into(),
                            params: Some(params),
                        },
                    ))
                    .is_ok()
            );
        }
        deleted(&session_id());
        drop(deleted);
        let mut later = register_before_dispatch.then(|| router.register(&session_id()));
        while let Ok(input) = inputs.try_recv() {
            match input {
                DispatcherInput::Notification(notification)
                    if notification.method == "session.lifecycle" =>
                {
                    sink.deleted(&session_id());
                }
                DispatcherInput::Notification(notification) => {
                    assert_eq!(notification.method, "session.event");
                    sink.route(notification.params.unwrap());
                }
                DispatcherInput::Detached(id, token) => sink.detached(&id, token),
                DispatcherInput::Claim(id, token) => sink.claim(&id, token),
            }
        }
        if later.is_none() {
            later = Some(router.register(&session_id()));
            let Ok(DispatcherInput::Claim(id, token)) = inputs.try_recv() else {
                panic!("registration queued no claim");
            };
            sink.claim(&id, token);
        }
        sink.route(session_event(
            session_id().as_str(),
            "replacement-after-reply",
        ));
        assert_eq!(
            drain(&mut later.unwrap().channels.notifications),
            ["replacement-before-reply", "replacement-after-reply"],
            "registered before dispatch: {register_before_dispatch}"
        );
    }
}

#[test]
fn a_detach_releases_the_held_byte_budget() {
    let router = SessionRouter::new();
    let sink = router.event_sink();
    sink.route(sized_event("detaching", "obsolete", 15 * 1024 * 1024));
    sink.route(session_event("other", "kept"));

    router.detach_replied(None)(&SessionId::new("detaching"));
    sink.route(sized_event("rejoined", "new", 2 * 1024 * 1024));

    let mut other = router.register(&SessionId::new("other"));
    let mut rejoined = router.register(&SessionId::new("rejoined"));
    assert_eq!(drain(&mut other.channels.notifications), ["kept"]);
    assert_eq!(drain(&mut rejoined.channels.notifications), ["new"]);
}

/// A claim queued by a registration that has since ended must not release a
/// later registration of the same ID before that registration's own claim.
#[test]
fn a_superseded_claim_does_not_release_a_later_registration() {
    let router = SessionRouter::new();
    let (dispatcher, mut claims) = mpsc::unbounded_channel();
    router.connect_dispatcher(&dispatcher);
    let sink = router.event_sink();
    let mut next_claim = || match claims.try_recv() {
        Ok(DispatcherInput::Claim(session_id, token)) => (session_id, token),
        _ => panic!("registration queued no claim"),
    };

    let first = router.register(&session_id());
    let stale = next_claim();
    assert!(router.unregister_owned(&session_id(), first.token));
    let mut second = router.register(&session_id());
    let current = next_claim();

    sink.claim(&stale.0, stale.1);
    for (id, event_type) in [
        ("p", "permission.requested"),
        ("p-done", "permission.completed"),
    ] {
        sink.route(typed_event(session_id().as_str(), id, event_type, "r1"));
    }
    assert!(drain(&mut second.channels.notifications).is_empty());

    sink.claim(&current.0, current.1);
    assert_eq!(
        drain(&mut second.channels.notifications),
        ["p (settled)", "p-done"]
    );
}

#[test]
fn restore_owned_hands_the_id_back_to_a_running_replaced_registration() {
    let router = SessionRouter::new();
    let resident = router.register(&session_id());
    let (attempt, replaced) = router.replace(&session_id());
    assert!(replaced.is_some());

    assert!(router.restore_owned(&session_id(), attempt.token, replaced));
    assert!(router.is_registered_owner(&session_id(), resident.token));
}

#[test]
fn restore_owned_unregisters_when_the_replaced_session_has_stopped() {
    let router = SessionRouter::new();
    let resident = router.register(&session_id());
    let (attempt, replaced) = router.replace(&session_id());
    drop(resident);

    assert!(router.restore_owned(&session_id(), attempt.token, replaced));
    assert!(router.session_ids().is_empty());
}

#[test]
fn replaced_registration_becomes_resident_only_after_setup_completes() {
    let router = SessionRouter::new();
    let startup = router.register(&session_id());
    let (_attempt, replaced) = router.replace(&session_id());
    let replaced = replaced.expect("startup registration must have been replaced");
    assert!(!replaced.is_resident());

    startup.state.mark_ready();
    assert!(replaced.is_resident());
    startup.state.cancel();
    assert!(!replaced.is_resident());
}

#[test]
fn restore_owned_leaves_a_newer_registration_alone() {
    let router = SessionRouter::new();
    let _resident = router.register(&session_id());
    let (attempt, replaced) = router.replace(&session_id());
    let newer = router.register(&session_id());

    assert!(!router.restore_owned(&session_id(), attempt.token, replaced));
    assert!(router.is_registered_owner(&session_id(), newer.token));
}
