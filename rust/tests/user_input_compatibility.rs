#![cfg(test)]
#![allow(clippy::unwrap_used)]

use copilot_sdk::Error;
use copilot_sdk::rpc::{
    UIHandlePendingResult, UIHandlePendingUserInputRequest,
    UIHandlePendingUserInputWithImagesRequest, UIUserInputResponse, UIUserInputResponseWithImages,
};
use copilot_sdk::session::Session;
use serde_json::json;

async fn _legacy_answer_signature(
    session: &Session,
    params: UIHandlePendingUserInputRequest,
) -> Result<UIHandlePendingResult, Error> {
    session.rpc().ui().handle_pending_user_input(params).await
}

async fn _image_answer_signature(
    session: &Session,
    params: UIHandlePendingUserInputWithImagesRequest,
) -> Result<UIHandlePendingResult, Error> {
    session
        .rpc()
        .ui()
        .handle_pending_user_input_with_images(params)
        .await
}

#[test]
fn legacy_response_literals_keep_the_text_only_wire_contract() {
    let response = UIUserInputResponse {
        answer: "yes".into(),
        was_freeform: true,
    };
    assert_eq!(
        serde_json::to_value(response).unwrap(),
        json!({"answer": "yes", "wasFreeform": true}),
    );
}

#[test]
fn image_only_response_uses_the_same_pending_answer_wire_envelope() {
    let response: UIUserInputResponseWithImages = serde_json::from_value(json!({
        "answer": "",
        "wasFreeform": true,
        "attachments": [{"type": "image", "data": "aW1hZ2U=", "mimeType": "image/png"}],
    }))
    .unwrap();
    let request = UIHandlePendingUserInputWithImagesRequest {
        request_id: "input".into(),
        response,
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(request).unwrap(),
        json!({
            "requestId": "input",
            "response": {
                "answer": "",
                "wasFreeform": true,
                "attachments": [{"type": "image", "data": "aW1hZ2U=", "mimeType": "image/png"}],
            },
        }),
    );
}
