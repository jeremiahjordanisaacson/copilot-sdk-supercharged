// Copyright (c) Microsoft Corporation. All rights reserved.

#![cfg(test)]

use std::future::{pending, poll_fn};
use std::task::Poll;

use futures_util::FutureExt;
use serde_json::json;

use super::*;

#[tokio::test]
async fn completion_cannot_skip_admission_delivered_during_its_poll() {
    let (sender, receiver) = oneshot::channel();
    let mut sender = Some(sender);
    let completion = poll_fn(move |_| {
        sender.take().unwrap().send(()).unwrap();
        Poll::Ready(Ok(json!({"messageId": "admitted-message"})))
    });
    let mut admitted = false;

    let result = wait_for_send_admission(receiver, completion, pending(), || async {
        admitted = true;
        Ok(())
    })
    .await
    .unwrap();

    assert!(
        admitted,
        "completion skipped the delivered admission receipt"
    );
    assert_eq!(result, json!({"messageId": "admitted-message"}));
}

#[tokio::test]
async fn rejection_before_admission_does_not_wait_or_call_callback() {
    let (_sender, receiver) = oneshot::channel();
    let mut admitted = false;
    let result = wait_for_send_admission(
        receiver,
        std::future::ready(Err(Error::with_message(
            crate::ErrorKind::Rpc { code: -32602 },
            "Rejected before admission",
        ))),
        pending(),
        || async {
            admitted = true;
            Ok(())
        },
    )
    .now_or_never()
    .expect("rejection must not wait for an admission receipt")
    .unwrap_err();

    assert!(!admitted);
    assert!(matches!(
        result.kind(),
        crate::ErrorKind::Rpc { code: -32602 }
    ));
}
