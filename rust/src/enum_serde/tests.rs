#![cfg(test)]

use serde::Deserialize;
use serde::de::value::{BytesDeserializer, Error, MapAccessDeserializer, MapDeserializer};

mod legacy {
    #[derive(Debug, PartialEq, serde::Deserialize)]
    pub enum AttachmentFileType {
        #[serde(rename = "file")]
        File,
    }

    #[derive(Debug, PartialEq, serde::Deserialize)]
    pub enum SessionMode {
        #[serde(rename = "interactive")]
        Interactive,
        #[serde(rename = "plan")]
        Plan,
        #[serde(rename = "autopilot")]
        Autopilot,
        #[serde(other)]
        Unknown,
    }
}

#[test]
fn generated_enums_preserve_derived_json_representations_and_errors() {
    for input in [
        r#""file""#,
        r#"{"file":null}"#,
        r#""other""#,
        r#"{"other":null}"#,
        r#"{"file":1}"#,
        r#""interactive""#,
        r#""plan""#,
        r#""autopilot""#,
        r#""Unknown""#,
        r#"{"interactive":null}"#,
        r#"{"other":17}"#,
        "null",
        "[]",
    ] {
        let actual = serde_json::from_str::<crate::rpc::AttachmentFileType>(input).map(|_| ());
        let expected = serde_json::from_str::<legacy::AttachmentFileType>(input).map(|_| ());
        assert_eq!(
            actual.map_err(|error| error.to_string()),
            expected.map_err(|error| error.to_string()),
            "constant enum: {input}"
        );
        let actual = serde_json::from_str::<crate::session_events::SessionMode>(input)
            .map(|value| format!("{value:?}"));
        let expected =
            serde_json::from_str::<legacy::SessionMode>(input).map(|value| format!("{value:?}"));
        assert_eq!(
            actual.map_err(|error| error.to_string()),
            expected.map_err(|error| error.to_string()),
            "fallback enum: {input}"
        );
    }
}

#[test]
fn generated_enums_preserve_numeric_variant_identifiers() {
    for index in [0_u64, 1, 2, 3, 4, u64::MAX] {
        let deserializer = || {
            MapAccessDeserializer::new(MapDeserializer::<_, Error>::new([(index, ())].into_iter()))
        };
        let actual = crate::rpc::AttachmentFileType::deserialize(deserializer()).map(|_| ());
        let expected = legacy::AttachmentFileType::deserialize(deserializer()).map(|_| ());
        assert_eq!(
            actual.map_err(|error| error.to_string()),
            expected.map_err(|error| error.to_string()),
            "constant enum, index {index}"
        );
        let actual = crate::session_events::SessionMode::deserialize(deserializer())
            .map(|value| format!("{value:?}"));
        let expected =
            legacy::SessionMode::deserialize(deserializer()).map(|value| format!("{value:?}"));
        assert_eq!(
            actual.map_err(|error| error.to_string()),
            expected.map_err(|error| error.to_string()),
            "fallback enum, index {index}"
        );
    }
}

#[test]
fn generated_enums_preserve_byte_variant_identifiers() {
    for identifier in [
        b"file".as_slice(),
        b"interactive",
        b"plan",
        b"other",
        &[0xff],
    ] {
        let deserializer = || {
            MapAccessDeserializer::new(MapDeserializer::<_, Error>::new(
                [(BytesDeserializer::<Error>::new(identifier), ())].into_iter(),
            ))
        };
        let actual = crate::rpc::AttachmentFileType::deserialize(deserializer()).map(|_| ());
        let expected = legacy::AttachmentFileType::deserialize(deserializer()).map(|_| ());
        assert_eq!(
            actual.map_err(|error| error.to_string()),
            expected.map_err(|error| error.to_string()),
            "constant enum: {identifier:?}"
        );
        let actual = crate::session_events::SessionMode::deserialize(deserializer())
            .map(|value| format!("{value:?}"));
        let expected =
            legacy::SessionMode::deserialize(deserializer()).map(|value| format!("{value:?}"));
        assert_eq!(
            actual.map_err(|error| error.to_string()),
            expected.map_err(|error| error.to_string()),
            "fallback enum: {identifier:?}"
        );
    }
}
