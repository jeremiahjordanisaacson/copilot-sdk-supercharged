/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

use crate::Error;
use serde_json::Value;
use std::future::Future;
use std::pin::Pin;

/// Process facts and memory samples supplied by an original in-process host.
pub trait InProcessHostTelemetry: Send + Sync + 'static {
    /// Resolve process facts without blocking SDK startup.
    fn configuration(&self) -> Pin<Box<dyn Future<Output = Result<Value, Error>> + Send + '_>>;

    /// Capture the embedding process, including memory outside the native runtime.
    fn memory_snapshot(
        &self,
        capture_full_memory: bool,
    ) -> Pin<Box<dyn Future<Output = Result<Option<Value>, Error>> + Send + '_>>;
}

#[cfg(feature = "in-process")]
pub(crate) fn register(
    client: &crate::Client,
    provider: std::sync::Arc<dyn InProcessHostTelemetry>,
) -> Result<(), Error> {
    let configuration = std::sync::Arc::clone(&provider);
    client.register_request_handler("host.processTelemetryConfiguration", move |_, _| {
        let provider = std::sync::Arc::clone(&configuration);
        async move { provider.configuration().await }
    })?;
    client.register_request_handler("host.processMemorySnapshot", move |params, _| {
        let provider = std::sync::Arc::clone(&provider);
        async move {
            let capture = params
                .get("captureFullMemory")
                .and_then(Value::as_bool)
                .unwrap_or(false);
            Ok(match provider.memory_snapshot(capture).await? {
                Some(snapshot) => Value::String(snapshot.to_string()),
                None => Value::Null,
            })
        }
    })
}
