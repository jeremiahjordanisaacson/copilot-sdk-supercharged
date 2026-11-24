#![cfg(test)]
#![allow(clippy::unwrap_used)]

use copilot_sdk::Error;
use copilot_sdk::rpc::{
    CatalogClientContract, CatalogSearchRequest, ClientRpcCatalog, ClientRpcMcp, ClientRpcSessions,
    McpPlanInstallRequest, McpPlanInstallSource, McpPlanInstallSourceCandidate,
    McpPlanInstallSourceCandidateKind, McpSecretSaveRequest, McpSecretSaveResult,
    McpSecretsSaveResult, PermissionsSetRequiredRequest, SessionRpcPermissions,
    SessionUpdateInferenceOptions, SessionUpdateOptionsParams, SessionsCloseRequest,
};
use serde_json::json;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader, duplex};

#[tokio::test]
async fn mcp_save_result_accepts_canonical_and_plural_consumers() {
    fn canonical_consumer(result: &McpSecretSaveResult) -> bool {
        result.used_keychain
    }

    fn plural_consumer(result: &McpSecretsSaveResult) -> bool {
        canonical_consumer(result)
    }

    let workspace = tempfile::tempdir().unwrap();
    let (client_write, server_read) = duplex(4096);
    let (mut server_write, client_read) = duplex(4096);
    let client = copilot_sdk::Client::from_streams(
        copilot_sdk::ClientMode::CodingAgent,
        client_read,
        client_write,
        workspace.path().to_path_buf(),
    )
    .unwrap();
    let server = tokio::spawn(async move {
        let mut reader = BufReader::new(server_read);
        let mut header = String::new();
        reader.read_line(&mut header).await.unwrap();
        let length: usize = header
            .trim()
            .strip_prefix("Content-Length: ")
            .unwrap()
            .parse()
            .unwrap();
        let mut separator = String::new();
        reader.read_line(&mut separator).await.unwrap();
        assert_eq!(separator, "\r\n");
        let mut body = vec![0; length];
        reader.read_exact(&mut body).await.unwrap();
        let request: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(request["method"], "mcp.secrets.save");
        assert_eq!(
            request["params"],
            json!({"secretId": "consumer/key", "value": "wire-value"})
        );
        let response = serde_json::to_vec(&json!({
            "jsonrpc": "2.0",
            "id": request["id"],
            "result": {"usedKeychain": true}
        }))
        .unwrap();
        server_write
            .write_all(format!("Content-Length: {}\r\n\r\n", response.len()).as_bytes())
            .await
            .unwrap();
        server_write.write_all(&response).await.unwrap();
        server_write.flush().await.unwrap();
    });
    let result = client
        .rpc()
        .mcp()
        .secrets()
        .save(McpSecretSaveRequest {
            config_dir: None,
            secret_id: "consumer/key".to_owned(),
            value: "wire-value".to_owned(),
        })
        .await
        .unwrap();
    server.await.unwrap();
    assert!(canonical_consumer(&result));
    assert!(plural_consumer(&result));
}

#[test]
fn grouped_options_update_preserves_default_provider_omission_selection_and_clear() {
    for (default_provider, inference) in [
        (None, json!({})),
        (Some(None), json!({"defaultProvider": null})),
        (
            Some(Some("gateway".to_owned())),
            json!({"defaultProvider": "gateway"}),
        ),
    ] {
        let params = SessionUpdateOptionsParams {
            inference: Some(SessionUpdateInferenceOptions {
                default_provider: default_provider.clone(),
                ..Default::default()
            }),
            ..Default::default()
        };
        let expected = json!({"inference": inference});
        assert_eq!(serde_json::to_value(params).unwrap(), expected);
        let decoded: SessionUpdateOptionsParams = serde_json::from_value(expected).unwrap();
        assert_eq!(
            decoded.inference.unwrap().default_provider,
            default_provider
        );
    }
}

#[test]
fn permission_request_preserves_provider_scope_and_explicit_false() {
    let request = PermissionsSetRequiredRequest {
        required: false,
        provider_id: Some("provider".to_string()),
    };
    assert_eq!(
        serde_json::to_value(request).unwrap(),
        json!({"required": false, "providerId": "provider"})
    );
}

#[test]
fn session_close_preserves_absent_and_explicit_detach_values() {
    for detach in [None, Some(false), Some(true)] {
        let request = SessionsCloseRequest {
            session_id: "session".into(),
            detach_session_end_hooks: detach,
        };
        let wire = serde_json::to_value(request).unwrap();
        assert_eq!(wire["sessionId"], "session");
        assert_eq!(
            wire.get("detachSessionEndHooks"),
            detach.map(|v| json!(v)).as_ref()
        );
    }
}

fn contract() -> CatalogClientContract {
    CatalogClientContract {
        protocol_version: 3,
        required_capabilities: vec!["mcp-install-planning".to_string()],
    }
}

fn candidate() -> McpPlanInstallSource {
    McpPlanInstallSource::Candidate(McpPlanInstallSourceCandidate {
        candidate_handle: "candidate".to_string(),
        kind: McpPlanInstallSourceCandidateKind::Candidate,
        search_id: "search".to_string(),
    })
}

#[test]
fn catalog_requests_include_policy_scope_without_an_extended_options_type() {
    let plan = McpPlanInstallRequest {
        contract: contract(),
        policy_session_id: Some("session".to_string()),
        scope: None,
        source: candidate(),
    };
    let search = CatalogSearchRequest {
        contract: contract(),
        policy_session_id: Some("session".to_string()),
        query: "catalogue query".to_string(),
        limit: Some(4),
        ..Default::default()
    };
    assert_eq!(
        serde_json::to_value(plan).unwrap()["policySessionId"],
        "session"
    );
    let search = serde_json::to_value(search).unwrap();
    assert_eq!(search["policySessionId"], "session");
    assert_eq!(search["limit"], 4);
    assert!(search.get("categories").is_none());
}

#[allow(dead_code)]
async fn request_entry_points_compile(
    permissions: SessionRpcPermissions<'_>,
    sessions: ClientRpcSessions<'_>,
    mcp: ClientRpcMcp<'_>,
    catalog: ClientRpcCatalog<'_>,
) -> Result<(), Error> {
    permissions
        .set_required(PermissionsSetRequiredRequest {
            required: false,
            provider_id: Some("provider".to_string()),
        })
        .await?;
    sessions
        .close(SessionsCloseRequest {
            session_id: "session".into(),
            detach_session_end_hooks: Some(true),
        })
        .await?;
    mcp.plan_install(McpPlanInstallRequest {
        contract: contract(),
        policy_session_id: Some("session".to_string()),
        scope: None,
        source: candidate(),
    })
    .await?;
    catalog
        .search(CatalogSearchRequest {
            contract: contract(),
            query: "catalogue query".to_string(),
            ..Default::default()
        })
        .await?;
    Ok(())
}
