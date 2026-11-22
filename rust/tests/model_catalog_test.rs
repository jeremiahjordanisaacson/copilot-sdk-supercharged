// Copyright (c) Microsoft Corporation. All rights reserved.

#![allow(clippy::unwrap_used)]

#[allow(dead_code)]
#[path = "support/fake_runtime.rs"]
mod fake_runtime;

use copilot_sdk::{Client, ClientMode, CreateSessionOptions};
use fake_runtime::{WAIT, read_frame, write_frame};
use serde_json::json;
use tokio::io::duplex;
use tokio::time::timeout;

#[tokio::test]
async fn describe_client_catalog_preserves_raw_capi_model_descriptions() {
    let raw_model = json!({
        "id": "catalog-model",
        "name": "Catalog model",
        "preview": true,
        "billing": {
            "token_prices": {
                "batch_size": 1_000_000,
                "default": {
                    "input_price": 500,
                    "output_price": 2500,
                    "cache_read_price": 50,
                    "max_prompt_tokens": 200_000
                },
                "long_context": {
                    "input_price": 1000,
                    "output_price": 5000,
                    "cache_read_price": 100,
                    "max_prompt_tokens": 936_000
                }
            }
        },
        "capabilities": {
            "supports": {
                "tool_calls": true,
                "parallel_tool_calls": false,
                "reasoning_effort": ["low", "high"]
            },
            "limits": {
                "max_context_window_tokens": 1_000_000,
                "max_prompt_tokens": 936_000,
                "max_output_tokens": 64_000
            }
        }
    });
    let (client_write, mut reader) = duplex(8192);
    let (mut writer, client_read) = duplex(8192);
    let client = Client::from_streams(
        ClientMode::CodingAgent,
        client_read,
        client_write,
        env!("CARGO_MANIFEST_DIR").into(),
    )
    .unwrap();
    let wire_model = raw_model.clone();
    let peer = async move {
        let create = read_frame(&mut reader).await;
        assert_eq!(create["method"], "session.create");
        let session_id = create["params"]["sessionId"].clone();
        write_frame(
            &mut writer,
            json!({
                "jsonrpc": "2.0", "id": create["id"],
                "result": { "sessionId": session_id }
            }),
        )
        .await;

        let describe = read_frame(&mut reader).await;
        assert_eq!(describe["method"], "session.model.describeClientCatalog");
        assert_eq!(describe["params"], json!({ "sessionId": session_id }));
        write_frame(
            &mut writer,
            json!({
                "jsonrpc": "2.0", "id": describe["id"],
                "result": { "models": [wire_model] }
            }),
        )
        .await;

        let detach = read_frame(&mut reader).await;
        assert_eq!(detach["method"], "session.detach");
        assert_eq!(detach["params"], json!({ "sessionId": session_id }));
        write_frame(
            &mut writer,
            json!({
                "jsonrpc": "2.0", "id": detach["id"],
                "result": { "success": true }
            }),
        )
        .await;
        (reader, writer)
    };
    let request = async {
        let session = client
            .create_session(CreateSessionOptions::default())
            .await
            .unwrap();
        let result = client
            .call(
                "session.model.describeClientCatalog",
                Some(json!({"sessionId": session.id().as_str()})),
            )
            .await;
        let stopped = client.stop().await;
        (result, stopped)
    };
    let (_peer, (result, stopped)) = timeout(WAIT, async { tokio::join!(peer, request) })
        .await
        .expect("catalog request and SDK shutdown must finish with the peer connected");
    stopped.unwrap();
    let model = result.unwrap()["models"][0].clone();
    assert_eq!(
        model["billing"]["token_prices"],
        raw_model["billing"]["token_prices"]
    );
    assert_eq!(
        model["capabilities"]["supports"],
        raw_model["capabilities"]["supports"]
    );
    assert_eq!(
        model["capabilities"]["limits"],
        raw_model["capabilities"]["limits"]
    );
    assert_eq!(model, raw_model);
}
