#![cfg(test)]

use crate::rpc::ModelProviderDiscoverRequest;
use serde::Deserialize;
use serde::de::value::MapDeserializer;
use serde_json::Value;

mod derived {
    #[derive(serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub(super) struct ModelProviderDiscoverRequest {
        pub adapter_id: String,
        pub input: Option<serde_json::Value>,
    }
}

#[test]
fn small_record_preserves_serde_field_and_sequence_rules() {
    for input in [
        r#"{"adapterId":"a","input":{"value":3},"extra":[null]}"#,
        r#"{"unused":[[2]],"adapterId":"a"}"#,
        r#"{"adapterId":"a","input":null}"#,
        r#"{"input":null,"input":12,"adapterId":"a"}"#,
        r#"{"adapterId":"a","adapterId":"b"}"#,
        r#"{"input":1}"#,
        r#"["a",null]"#,
        r#"["a"]"#,
        r#"["a",12,false]"#,
        r#"{"adapterId":null}"#,
        "42",
    ] {
        let actual = serde_json::from_str::<ModelProviderDiscoverRequest>(input)
            .map(|v| (v.adapter_id, v.input))
            .map_err(|e| e.to_string());
        let expected = serde_json::from_str::<derived::ModelProviderDiscoverRequest>(input)
            .map(|v| (v.adapter_id, v.input))
            .map_err(|e| e.to_string());
        assert_eq!(actual, expected, "{input}");
    }
}

#[test]
fn small_record_accepts_numeric_keys_and_consumes_unknown_fields() {
    let deserializer = MapDeserializer::<_, serde_json::Error>::new(
        [(u64::MAX, "ignored"), (1, "extra"), (0, "adapter")]
            .into_iter()
            .map(|(key, value)| (key, Value::from(value))),
    );
    let request = ModelProviderDiscoverRequest::deserialize(deserializer).unwrap();
    assert_eq!(request.adapter_id, "adapter");
    assert_eq!(request.input, Some(Value::from("extra")));
}

#[test]
fn small_record_emits_optional_payload_only_when_present() {
    for (input, json) in [
        (None, r#"{"adapterId":"a"}"#),
        (Some(Value::Null), r#"{"adapterId":"a","input":null}"#),
        (
            Some(Value::from(false)),
            r#"{"adapterId":"a","input":false}"#,
        ),
    ] {
        let request = ModelProviderDiscoverRequest {
            adapter_id: "a".into(),
            input,
        };
        assert_eq!(serde_json::to_string(&request).unwrap(), json);
    }
}

#[test]
fn ordered_record_handles_unknown_keys_between_known_keys() {
    let input = r#"{"repositoryHost":"ghe","repositoryBranch":["ignored"],"cwd":"dir","baseCommit":"base"}"#;
    let record: crate::session_events::WorkingDirectoryContext =
        serde_json::from_str(input).unwrap();
    assert_eq!(
        serde_json::to_string(&record).unwrap(),
        r#"{"baseCommit":"base","cwd":"dir","repositoryHost":"ghe"}"#
    );
}
