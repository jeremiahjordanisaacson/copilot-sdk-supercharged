// Unit tests for custom API serialization and released Rust source compatibility.

#![allow(clippy::unwrap_used)]

use copilot_sdk::AutoTier;
use copilot_sdk::rpc::{
    AcceptedEnqueueCommandResult, ConnectorConnectResult, EnqueueCommandResult,
    ModelSwitchAutoTierRequest, ProtocolExternalToolDefinition, ProviderModelConfig,
    SavedProviderConfigurationReadResult, SessionsOpenCreate, SessionsOpenCreateKind,
    SessionsOpenResume, SessionsOpenResumeKind, UnsupportedEnqueueCommandResult,
};
use copilot_sdk::session_events::{RecommendedAutoTier, UserMessageData};

#[test]
fn released_session_records_preserve_complete_struct_literals() {
    use copilot_sdk::rpc::{
        AutopilotObjectiveState, AutopilotObjectiveStatus, Extension, ExtensionSource,
        ExtensionStatus,
    };

    let objective = AutopilotObjectiveState {
        completion_summary: None,
        credit_count_nano_aiu: "0".into(),
        credit_limit: None,
        id: 1,
        objective: "Preserve Rust construction".into(),
        pause_reason: None,
        status: AutopilotObjectiveStatus::Paused,
        turn_count: 0,
    };
    assert_eq!(
        serde_json::to_value(objective).unwrap(),
        serde_json::json!({
            "creditCountNanoAiu": "0", "id": 1,
            "objective": "Preserve Rust construction", "status": "paused", "turnCount": 0
        })
    );
    let extension = Extension {
        id: "project:legacy".into(),
        name: "legacy".into(),
        pid: None,
        source: ExtensionSource::Project,
        status: ExtensionStatus::Dormant,
    };
    assert_eq!(
        serde_json::to_value(extension).unwrap(),
        serde_json::json!({
            "id": "project:legacy", "name": "legacy", "source": "project", "status": "dormant"
        })
    );
}

#[test]
fn released_metadata_records_preserve_complete_struct_literals() {
    use copilot_sdk::rpc::{SessionMetadataSnapshot, SessionMetadataSnapshotResult};

    macro_rules! snapshot {
        ($ty:ident) => {
            $ty {
                already_in_use: false,
                client_name: None,
                current_mode: Default::default(),
                indexed_search: None,
                initial_name: None,
                is_remote: false,
                modified_time: "2026-01-01T00:00:00Z".into(),
                remote_metadata: None,
                selected_model: None,
                session_id: "released-session".into(),
                session_limits: None,
                start_time: "2026-01-01T00:00:00Z".into(),
                summary: None,
                working_directory: "/workspace".into(),
                workspace: None,
                workspace_path: None,
            }
        };
    }
    for snapshot in [
        serde_json::to_value(snapshot!(SessionMetadataSnapshot)).unwrap(),
        serde_json::to_value(snapshot!(SessionMetadataSnapshotResult)).unwrap(),
    ] {
        assert_eq!(snapshot["sessionId"], "released-session");
        assert_eq!(snapshot["workingDirectory"], "/workspace");
        assert!(snapshot.get("runtimeInstanceId").is_none());
    }
}

#[test]
fn expanded_session_records_retain_the_current_wire_fields() {
    use copilot_sdk::rpc::{
        AutopilotObjectiveStateDetails, ExtensionDetails, SessionMetadataSnapshotDetails,
        SessionMetadataSnapshotResultDetails,
    };

    let objective: AutopilotObjectiveStateDetails = serde_json::from_value(serde_json::json!({
        "id": 1, "objective": "Read current state", "status": "paused",
        "turnCount": 3, "creditCountNanoAiu": "0",
        "createdAt": "2026-01-01T00:00:00Z", "resumeCount": 2
    }))
    .unwrap();
    assert_eq!(
        objective.created_at.as_deref(),
        Some("2026-01-01T00:00:00Z")
    );
    assert_eq!(objective.resume_count, Some(2));
    let extension: ExtensionDetails = serde_json::from_value(serde_json::json!({
        "id": "project:legacy", "name": "legacy", "source": "project", "status": "dormant",
        "modulePath": "/workspace/extension.mjs", "logPath": "/workspace/extension.log"
    }))
    .unwrap();
    assert_eq!(
        extension.module_path.as_deref(),
        Some("/workspace/extension.mjs")
    );
    assert_eq!(
        extension.log_path.as_deref(),
        Some("/workspace/extension.log")
    );
    let snapshot = serde_json::json!({
        "alreadyInUse": false, "currentMode": "interactive", "isRemote": false,
        "modifiedTime": "2026-01-01T00:00:00Z", "sessionId": "released-session",
        "sessionLimits": null, "startTime": "2026-01-01T00:00:00Z",
        "workingDirectory": "/workspace", "workspace": null, "workspacePath": null,
        "runtimeInstanceId": "runtime-generation"
    });
    let details: SessionMetadataSnapshotDetails = serde_json::from_value(snapshot.clone()).unwrap();
    let result: SessionMetadataSnapshotResultDetails = serde_json::from_value(snapshot).unwrap();
    assert_eq!(
        details.runtime_instance_id.as_deref(),
        Some("runtime-generation")
    );
    assert_eq!(result.runtime_instance_id, details.runtime_instance_id);
}

#[test]
fn elicitation_request_accepts_released_hash_map_input() {
    use std::collections::HashMap;

    use copilot_sdk::rpc::{UIElicitationRequest, UIElicitationSchema};

    let request = UIElicitationRequest {
        message: "Confirm the change".into(),
        requested_schema: UIElicitationSchema {
            properties: HashMap::from([(
                "confirmed".into(),
                serde_json::json!({"type": "boolean"}),
            )]),
            required: Some(vec!["confirmed".into()]),
            ..Default::default()
        },
        ..Default::default()
    };

    assert_eq!(
        serde_json::to_value(request).unwrap(),
        serde_json::json!({
            "message": "Confirm the change",
            "requestedSchema": {
                "type": "object",
                "properties": {"confirmed": {"type": "boolean"}},
                "required": ["confirmed"]
            }
        })
    );
}

#[test]
fn pending_elicitation_preserves_declared_form_field_order() {
    use copilot_sdk::rpc::{UIElicitationPrompt, UIPendingElicitation};

    let wire = r#"{
        "requestId": "ordered-form",
        "request": {
            "mode": "form",
            "message": "Complete the form",
            "requestedSchema": {
                "type": "object",
                "properties": {
                    "greeting": {"type": "string"},
                    "count": {"type": "number"},
                    "enabled": {"type": "boolean"},
                    "color": {"type": "string"},
                    "account": {"type": "string"},
                    "firstName": {"type": "string"},
                    "lastName": {"type": "string"},
                    "email": {"type": "string"},
                    "timezone": {"type": "string"},
                    "notes": {"type": "string"}
                }
            }
        }
    }"#;
    let pending: UIPendingElicitation = serde_json::from_str(wire).unwrap();
    let UIElicitationPrompt::Form(form) = &pending.request else {
        panic!("Expected a pending form");
    };
    let properties = &form.requested_schema.properties;
    let expected = [
        "greeting",
        "count",
        "enabled",
        "color",
        "account",
        "firstName",
        "lastName",
        "email",
        "timezone",
        "notes",
    ];
    assert_eq!(
        properties.keys().map(String::as_str).collect::<Vec<_>>(),
        expected
    );
    let encoded = serde_json::to_value(pending).unwrap();
    assert_eq!(
        encoded,
        serde_json::from_str::<serde_json::Value>(wire).unwrap()
    );
    assert_eq!(
        encoded["request"]["requestedSchema"]["properties"]
            .as_object()
            .unwrap()
            .keys()
            .map(String::as_str)
            .collect::<Vec<_>>(),
        expected
    );
}

#[test]
fn elicitation_prompts_preserve_mcp_extensions() {
    use copilot_sdk::rpc::UIElicitationPrompt;

    let mut cases = Vec::new();
    for (mode, properties) in [
        (
            "form",
            serde_json::json!({"requestedSchema": {"type": "object", "properties": {}}}),
        ),
        (
            "url",
            serde_json::json!({
                "url": "https://example.com/authorize",
                "elicitationId": "browser-request"
            }),
        ),
    ] {
        let mut wire = serde_json::json!({
            "mode": mode,
            "message": "MCP request",
            "x-vendor": {"nested": [{"enabled": true}, null, 42], "label": "extension"},
            "x-null": null,
            "x-count": 3
        });
        wire.as_object_mut()
            .unwrap()
            .extend(properties.as_object().unwrap().clone());
        let prompt: UIElicitationPrompt = serde_json::from_value(wire.clone()).unwrap();
        assert!(matches!(
            (mode, &prompt),
            ("form", UIElicitationPrompt::Form(_)) | ("url", UIElicitationPrompt::Url(_))
        ));
        cases.push((wire, serde_json::to_value(prompt).unwrap()));
    }
    assert_eq!(
        cases.iter().map(|(wire, _)| wire).collect::<Vec<_>>(),
        cases.iter().map(|(_, encoded)| encoded).collect::<Vec<_>>()
    );
}

#[test]
fn mcp_discovery_debug_redacts_configuration_without_changing_wire() {
    use copilot_sdk::rpc::{McpDiscoverOptions, McpDiscoverResult};

    let configuration = serde_json::json!({
        "type": "http",
        "url": "https://example.com/mcp",
        "env": {"TOKEN": "fake-discovery-token"},
        "headers": {"Authorization": "Bearer fake-discovery-authorization"}
    });
    let request_wire = serde_json::json!({
        "includeConfiguration": true,
        "options": {
            "additionalConfig": [serde_json::json!({
                "mcpServers": {"configured": configuration}
            }).to_string()],
            "disabledServers": ["disabled"]
        }
    });
    let result_wire = serde_json::json!({
        "servers": [{
            "name": "configured",
            "source": "user",
            "enabled": true,
            "configuration": configuration
        }],
        "diagnostics": []
    });
    let request = McpDiscoverOptions::new()
        .include_configuration(true)
        .options(serde_json::from_value(request_wire["options"].clone()).unwrap());
    let result: McpDiscoverResult = serde_json::from_value(result_wire.clone()).unwrap();

    for diagnostic in [format!("{request:?}"), format!("{result:?}")] {
        assert!(!diagnostic.contains("fake-discovery-token"), "{diagnostic}");
        assert!(
            !diagnostic.contains("fake-discovery-authorization"),
            "{diagnostic}"
        );
    }
    assert!(format!("{request:?}").contains("disabled"));
    assert!(format!("{result:?}").contains("configured"));
    assert_eq!(serde_json::to_value(request).unwrap(), request_wire);
    assert_eq!(serde_json::to_value(result).unwrap(), result_wire);
}

#[test]
fn git_diff_result_preserves_workspace_fallback_details() {
    use copilot_sdk::rpc::{GitDiffResult, WorkspaceDiffFileChange, WorkspaceDiffMode};

    let result: GitDiffResult = serde_json::from_value(serde_json::json!({
        "changes": [{
            "path": "file.txt",
            "diff": "@@ -1 +1 @@\n-before\n+after\n",
            "changeType": "modified"
        }],
        "isFallback": true,
        "mode": "unstaged",
        "requestedMode": "session",
        "unavailableReason": "session-busy"
    }))
    .unwrap();

    assert!(result.is_fallback);
    assert_eq!(result.mode, WorkspaceDiffMode::Unstaged);
    assert_eq!(result.requested_mode, WorkspaceDiffMode::Session);
    assert!(result.base_branch.is_none());
    assert_eq!(
        serde_json::to_value(result.unavailable_reason).unwrap(),
        serde_json::json!("session-busy")
    );
    let changes: Vec<WorkspaceDiffFileChange> = result.changes;
    assert_eq!(changes[0].path, "file.txt");
}

#[test]
fn forge_manifest_retains_nullable_metadata() {
    use copilot_sdk::rpc::ForgeProposalManifestEntry;

    for metadata in [
        serde_json::json!({}),
        serde_json::json!({"after_hash": null, "before_content": null, "before_hash": null}),
        serde_json::json!({"after_hash": "after", "before_content": "before", "before_hash": "hash"}),
    ] {
        let mut wire = serde_json::json!({
            "path": ".github/skills/demo/SKILL.md",
            "change_type": "added",
        });
        wire.as_object_mut()
            .unwrap()
            .extend(metadata.as_object().unwrap().clone());
        let entry: ForgeProposalManifestEntry = serde_json::from_value(wire.clone()).unwrap();
        assert_eq!(serde_json::to_value(entry).unwrap(), wire);
    }
}

#[test]
fn shell_environment_debug_redacts_values_without_changing_wire() {
    use copilot_sdk::rpc::{
        ShellBuildInteractiveEnvironmentRequest, ShellBuildInteractiveEnvironmentResult,
    };

    let environment = serde_json::json!({
        "GITHUB_TOKEN": "fake-shell-token",
        "CUSTOM_CREDENTIAL": "fake-shell-custom-credential"
    });
    let request_wire = serde_json::json!({
        "hostEnvironment": environment,
        "additionalBlockedNames": [],
        "caseInsensitive": false
    });
    let result_wire = serde_json::json!({ "environment": environment });
    let request: ShellBuildInteractiveEnvironmentRequest =
        serde_json::from_value(request_wire.clone()).unwrap();
    let result: ShellBuildInteractiveEnvironmentResult =
        serde_json::from_value(result_wire.clone()).unwrap();

    for diagnostic in [format!("{request:?}"), format!("{result:?}")] {
        for secret in environment.as_object().unwrap().values() {
            assert!(
                !diagnostic.contains(secret.as_str().unwrap()),
                "{diagnostic}"
            );
        }
    }
    assert_eq!(serde_json::to_value(request).unwrap(), request_wire);
    assert_eq!(serde_json::to_value(result).unwrap(), result_wire);
}

#[test]
fn fetch_response_defaults_preserve_each_variant_mode() {
    use copilot_sdk::rpc::{
        FetchSendResult, FetchSendResultFile, FetchSendResultStream, FetchSendResultText,
    };

    for (mode, result) in [
        (
            "stream",
            FetchSendResult::Stream(FetchSendResultStream::default()),
        ),
        (
            "text",
            FetchSendResult::Text(FetchSendResultText::default()),
        ),
        (
            "file",
            FetchSendResult::File(FetchSendResultFile::default()),
        ),
    ] {
        let wire = serde_json::to_value(result).unwrap();
        assert_eq!(wire["responseMode"], mode);
        serde_json::from_value::<FetchSendResult>(wire).unwrap();
    }
}

#[test]
fn fetch_response_modes_match_request_modes_and_round_trip() {
    use copilot_sdk::rpc::{FetchResponseMode, FetchSendResult};

    for (mode, extra) in [
        (FetchResponseMode::Stream, serde_json::json!({})),
        (
            FetchResponseMode::Text,
            serde_json::json!({"bodyText": "hello", "endedAtUnverifiedEof": false}),
        ),
        (
            FetchResponseMode::File,
            serde_json::json!({"path": "response.txt", "sizeBytes": 5, "endedAtUnverifiedEof": false}),
        ),
    ] {
        let mut wire = serde_json::json!({
            "responseMode": mode, "url": "https://example.com",
            "status": 200, "statusText": "OK", "headers": [],
        });
        wire.as_object_mut()
            .unwrap()
            .extend(extra.as_object().unwrap().clone());
        let result: FetchSendResult = serde_json::from_value(wire.clone()).unwrap();
        match &result {
            FetchSendResult::Stream(value) => assert_eq!(value.response_mode, mode),
            FetchSendResult::Text(value) => assert_eq!(value.response_mode, mode),
            FetchSendResult::File(value) => assert_eq!(value.response_mode, mode),
        }
        let mut invalid = result.clone();
        match &mut invalid {
            FetchSendResult::Stream(value) => value.response_mode = FetchResponseMode::Text,
            FetchSendResult::Text(value) => value.response_mode = FetchResponseMode::File,
            FetchSendResult::File(value) => value.response_mode = FetchResponseMode::Stream,
        }
        assert!(serde_json::to_value(invalid).is_err());
        assert_eq!(serde_json::to_value(result).unwrap(), wire);
    }
}

macro_rules! filesystem_callback_result {
    ($name:ident, $result:ty) => {
        #[test]
        fn $name() {
            for wire in [
                serde_json::Value::Null,
                serde_json::json!({ "code": "ENOENT", "message": "missing file" }),
            ] {
                let result: $result = serde_json::from_value(wire.clone())
                    .expect("filesystem callback results must accept success and failure");
                assert_eq!(serde_json::to_value(result).unwrap(), wire);
            }
        }
    };
}

filesystem_callback_result!(
    filesystem_write_file_result_accepts_null_success,
    copilot_sdk::rpc::SessionFsWriteFileResult
);
filesystem_callback_result!(
    filesystem_write_file_bytes_result_accepts_null_success,
    copilot_sdk::rpc::SessionFsWriteFileBytesResult
);
filesystem_callback_result!(
    filesystem_append_file_result_accepts_null_success,
    copilot_sdk::rpc::SessionFsAppendFileResult
);
filesystem_callback_result!(
    filesystem_mkdir_result_accepts_null_success,
    copilot_sdk::rpc::SessionFsMkdirResult
);
filesystem_callback_result!(
    filesystem_rm_result_accepts_null_success,
    copilot_sdk::rpc::SessionFsRmResult
);
filesystem_callback_result!(
    filesystem_rename_result_accepts_null_success,
    copilot_sdk::rpc::SessionFsRenameResult
);

#[test]
fn saved_provider_configuration_read_accepts_integral_float_token_limits() {
    let mut response: SavedProviderConfigurationReadResult =
        serde_json::from_value(serde_json::json!({
            "models": [{
                "id": "gpt-4o",
                "provider": "openai",
                "maxContextWindowTokens": 128000.0,
                "maxOutputTokens": 16384.0,
                "maxPromptTokens": 111616.0
            }],
            "providers": [{
                "credentialStatus": "absent",
                "provider": {
                    "baseUrl": "https://api.example.com/v1",
                    "name": "openai"
                }
            }]
        }))
        .expect("saved provider configuration accepts JSON-number token limits");

    let output_limit: i64 = 16_384;
    assert_eq!(
        response.models[0].max_context_window_tokens,
        Some(128_000_i64)
    );
    assert_eq!(response.models[0].max_prompt_tokens, Some(111_616_i64));
    response.models[0].max_output_tokens = Some(output_limit);
    let response = serde_json::to_value(response).unwrap();
    assert_eq!(
        response["models"][0]["maxContextWindowTokens"],
        serde_json::json!(128000)
    );
    assert_eq!(
        response["models"][0]["maxOutputTokens"],
        serde_json::json!(16384)
    );
    assert_eq!(
        response["models"][0]["maxPromptTokens"],
        serde_json::json!(111616)
    );
}

#[test]
fn provider_model_token_limits_preserve_integer_bounds_without_rounding() {
    for limit in [serde_json::json!(i64::MIN), serde_json::json!(i64::MAX)] {
        let model: ProviderModelConfig = serde_json::from_value(serde_json::json!({
            "id": "test", "provider": "test", "maxOutputTokens": limit,
        }))
        .expect("existing integer limits must remain readable without precision loss");
        assert_eq!(
            serde_json::to_value(model).unwrap()["maxOutputTokens"],
            limit
        );
    }

    for limit in [
        serde_json::json!(1.5),
        serde_json::json!(i64::MAX as f64),
        serde_json::json!(-9_223_372_036_854_777_856.0),
        serde_json::json!("16384"),
    ] {
        let result = serde_json::from_value::<ProviderModelConfig>(serde_json::json!({
            "id": "test", "provider": "test", "maxOutputTokens": limit,
        }));
        assert!(
            result.is_err(),
            "non-integer or out-of-range limit {limit} must be rejected"
        );
    }

    for limit in [None, Some(serde_json::Value::Null)] {
        let mut wire = serde_json::json!({"id": "test", "provider": "test"});
        if let Some(limit) = limit {
            wire["maxOutputTokens"] = limit;
        }
        let model: ProviderModelConfig = serde_json::from_value(wire).unwrap();
        assert!(
            serde_json::to_value(model)
                .unwrap()
                .get("maxOutputTokens")
                .is_none(),
            "an omitted or null token limit must not become an explicit value"
        );
    }
}

#[test]
fn classify_trajectory_request_accepts_recorded_session_events() {
    let event: copilot_sdk::types::SessionEvent = serde_json::from_value(serde_json::json!({
        "id": "event-1",
        "parentId": null,
        "timestamp": "2026-01-01T00:00:00Z",
        "type": "user.message",
        "data": { "content": "Fix the build" }
    }))
    .unwrap();

    let request =
        copilot_sdk::rpc::TaskAnalyticsClassifyTrajectoryRequest::from_session_events(&[event])
            .unwrap();

    assert_eq!(
        serde_json::to_value(&request).unwrap()["events"],
        serde_json::json!([{
            "id": "event-1",
            "parentId": null,
            "timestamp": "2026-01-01T00:00:00Z",
            "type": "user.message",
            "data": { "content": "Fix the build" }
        }])
    );
}

#[test]
fn user_message_data_preserves_external_struct_literals() {
    let data = UserMessageData {
        content: "Fix the build".into(),
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(data).unwrap(),
        serde_json::json!({"content": "Fix the build"})
    );
}

#[test]
fn session_open_create_preserves_external_struct_literals() {
    let request = SessionsOpenCreate {
        kind: SessionsOpenCreateKind::Create,
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(request).unwrap(),
        serde_json::json!({"kind": "create"})
    );
    let explicit = SessionsOpenCreate {
        emit_start: Some(false),
        kind: SessionsOpenCreateKind::Create,
        options: None,
    };
    assert_eq!(
        serde_json::to_value(explicit).unwrap(),
        serde_json::json!({"kind": "create", "emitStart": false})
    );
    let decoded: SessionsOpenCreate = serde_json::from_value(serde_json::json!({
        "kind": "create", "hostConstructionToken": 123
    }))
    .unwrap();
    assert_eq!(
        serde_json::to_value(decoded).unwrap(),
        serde_json::json!({"kind": "create"})
    );
}

#[test]
fn session_open_resume_preserves_external_struct_literals() {
    let request = SessionsOpenResume {
        kind: SessionsOpenResumeKind::Resume,
        session_id: "resume-session".into(),
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(request).unwrap(),
        serde_json::json!({"kind": "resume", "sessionId": "resume-session"})
    );
    let explicit = SessionsOpenResume {
        kind: SessionsOpenResumeKind::Resume,
        options: None,
        resume: Some(false),
        session_id: "resume-session".into(),
        suppress_resume_workspace_metadata_writeback: Some(true),
    };
    assert_eq!(
        serde_json::to_value(explicit).unwrap(),
        serde_json::json!({
            "kind": "resume", "sessionId": "resume-session", "resume": false,
            "suppressResumeWorkspaceMetadataWriteback": true
        })
    );
    let decoded: SessionsOpenResume = serde_json::from_value(serde_json::json!({
        "kind": "resume", "sessionId": "resume-session", "hostConstructionToken": 123
    }))
    .unwrap();
    assert_eq!(
        serde_json::to_value(decoded).unwrap(),
        serde_json::json!({"kind": "resume", "sessionId": "resume-session"})
    );
}

#[test]
fn external_tool_definitions_preserve_public_struct_literals() {
    let definition = ProtocolExternalToolDefinition {
        defer: None,
        description: "Description".to_owned(),
        idempotent: None,
        is_terminal: Some(true),
        metadata: None,
        name: "lookup".to_owned(),
        namespace: None,
        overrides_built_in_tool: None,
        parameters: None,
        skip_permission: None,
        title: None,
    };
    let defaults = ProtocolExternalToolDefinition {
        name: definition.name.clone(),
        ..Default::default()
    };
    assert_eq!(defaults.name, definition.name);
    assert_eq!(
        serde_json::to_value(&definition).unwrap(),
        serde_json::json!({
            "description": "Description",
            "name": "lookup",
            "isTerminal": true
        })
    );
}

#[test]
fn extensible_tier_unknown_literal_retains_its_provider_identity() {
    let wire = serde_json::json!("Unknown");
    let tier: AutoTier = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(tier, AutoTier::Custom("Unknown".to_owned()));
    assert_eq!(serde_json::to_value(tier).unwrap(), wire);
    let recommendation: RecommendedAutoTier = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(
        recommendation,
        RecommendedAutoTier::Custom("Unknown".to_owned())
    );
    assert_eq!(serde_json::to_value(recommendation).unwrap(), wire);
}

#[test]
fn connector_connect_results_do_not_treat_unknown_outcomes_as_connected() {
    let future_result = serde_json::json!({
        "kind": "future_outcome",
        "continuationId": "continuation-1",
        "consentUrl": "https://example.com/consent",
        "status": {
            "apiVersion": 1,
            "availability": "enabled",
            "runtimeServers": [],
            "pendingConnections": 0
        }
    });

    assert!(serde_json::from_value::<ConnectorConnectResult>(future_result).is_err());
}

#[test]
fn enqueue_command_result_preserves_boolean_discriminator() {
    let accepted: EnqueueCommandResult = serde_json::from_value(serde_json::json!({
        "queued": true,
        "queueId": "queue-1"
    }))
    .unwrap();
    assert!(matches!(
        &accepted,
        EnqueueCommandResult::AcceptedEnqueueCommandResult(_)
    ));
    assert_eq!(
        serde_json::to_value(&accepted).unwrap(),
        serde_json::json!({ "queued": true, "queueId": "queue-1" })
    );

    let unsupported: EnqueueCommandResult =
        serde_json::from_value(serde_json::json!({ "queued": false, "queueId": null })).unwrap();
    assert!(matches!(
        &unsupported,
        EnqueueCommandResult::UnsupportedEnqueueCommandResult(_)
    ));
    assert_eq!(
        serde_json::to_value(&unsupported).unwrap(),
        serde_json::json!({ "queued": false })
    );

    assert!(
        serde_json::to_value(AcceptedEnqueueCommandResult {
            queued: false,
            queue_id: "queue-1".to_string(),
        })
        .is_err()
    );
    assert!(
        serde_json::to_value(UnsupportedEnqueueCommandResult {
            queued: true,
            queue_id: None,
        })
        .is_err()
    );
}

#[test]
fn switch_auto_tier_request_serializes_explicit_null_tier() {
    // `autoTier` is a required field whose null value means "use provider-default
    // routing", so it must survive serialization rather than being skipped.
    let request = ModelSwitchAutoTierRequest {
        auto_tier: None,
        source: None,
    };
    let wire = serde_json::to_value(&request).unwrap();

    assert_eq!(wire.get("autoTier"), Some(&serde_json::Value::Null));
    assert!(wire.get("source").is_none());
}

/// Released structs and enums that are unrelated to the MCP installation payloads keep
/// distinct types, so downstream trait impls, variant imports and literals still compile.
mod released_type_shapes {
    use copilot_sdk::rpc::{
        DiagnosticsReadResultEntriesItem, DiagnosticsReadResultEntriesItemSource,
        MetadataContextAttributionResultContextAttribution, MetadataContextInfoResultContextInfo,
        SendMessagesRequestResponseFormat, SendMessagesRequestResponseFormatType,
        SendRequestResponseFormat, SendRequestResponseFormatType,
        SessionDiagnosticsReadResultEntriesItem, SessionDiagnosticsReadResultEntriesItemSource,
        SessionMetadataContextInfoResultContextInfo,
        SessionMetadataGetContextAttributionResultContextAttribution,
        SessionMetadataSnapshotResultWorkspace, SessionMetadataSnapshotWorkspace,
    };

    trait Released {
        const NAME: &'static str;
    }
    macro_rules! released {
        ($($ty:ty),* $(,)?) => { $(impl Released for $ty { const NAME: &'static str = stringify!($ty); })* };
    }
    // Separate impls on each released name fail to compile if two names alias one type.
    released!(
        DiagnosticsReadResultEntriesItem,
        SessionDiagnosticsReadResultEntriesItem,
        DiagnosticsReadResultEntriesItemSource,
        SessionDiagnosticsReadResultEntriesItemSource,
        MetadataContextAttributionResultContextAttribution,
        SessionMetadataGetContextAttributionResultContextAttribution,
        MetadataContextInfoResultContextInfo,
        SessionMetadataContextInfoResultContextInfo,
        SendRequestResponseFormat,
        SendMessagesRequestResponseFormat,
        SendRequestResponseFormatType,
        SendMessagesRequestResponseFormatType,
        SessionMetadataSnapshotWorkspace,
        SessionMetadataSnapshotResultWorkspace,
    );

    #[test]
    fn released_names_are_distinct_types() {
        assert_ne!(
            <SendRequestResponseFormat as Released>::NAME,
            <SendMessagesRequestResponseFormat as Released>::NAME
        );
    }

    #[test]
    fn released_enum_variants_import_and_match() {
        use SendRequestResponseFormatType::*;
        let format = SendRequestResponseFormat {
            r#type: JsonSchema,
            ..Default::default()
        };
        assert!(matches!(format.r#type, JsonSchema));
        use DiagnosticsReadResultEntriesItemSource::Mcp;
        assert!(matches!(
            DiagnosticsReadResultEntriesItemSource::default(),
            Mcp
        ));
        assert!(matches!(
            SessionDiagnosticsReadResultEntriesItemSource::default(),
            SessionDiagnosticsReadResultEntriesItemSource::Mcp
        ));
        assert!(matches!(
            SendMessagesRequestResponseFormatType::default(),
            SendMessagesRequestResponseFormatType::JsonSchema
        ));
    }
}
