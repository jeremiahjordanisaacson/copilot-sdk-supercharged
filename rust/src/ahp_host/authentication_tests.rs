#![cfg(test)]

use super::*;

async fn authenticate(peer: &mut Peer, host_id: &str, request_id: &str) {
    peer.send(json!({
        "jsonrpc": "2.0", "id": 902, "method": "host.validateAuthentication",
        "params": {"hostId": host_id, "requestId": request_id, "token": "application-token"}
    }))
    .await;
}

async fn cancel(peer: &mut Peer, host_id: &str, request_id: &str) {
    peer.send(json!({
        "jsonrpc": "2.0", "method": "host.authenticationCancelled",
        "params": {"hostId": host_id, "requestId": request_id}
    }))
    .await;
}

#[tokio::test]
async fn requires_authentication_before_host_start_in_every_mode() {
    for mode in [crate::ClientMode::CodingAgent, crate::ClientMode::Empty] {
        let (client_write, read) = duplex(1 << 20);
        let (write, client_read) = duplex(1 << 20);
        let client = Client::from_streams_with_mode_for_test(
            client_read,
            client_write,
            PathBuf::from("."),
            mode,
        )
        .unwrap();
        let mut peer = Peer { read, write };
        let options = AhpHostOptions::new().with_local_server(Default::default());
        let error = client.start_ahp_host(options).await.unwrap_err();
        assert!(
            error.to_string().contains("explicit authentication"),
            "{error}"
        );
        // No host request was enqueued: an ordinary next request is first on the wire.
        let next = tokio::spawn({
            let client = client.clone();
            async move { client.rpc().host().list_sessions(Default::default()).await }
        });
        let request = peer.request().await;
        assert_eq!(request["method"], "host.listSessions", "{mode:?}");
        peer.respond(&request, json!({"sessions": []})).await;
        next.await.unwrap().unwrap();
    }
}

#[tokio::test]
async fn custom_options_reject_empty_identity_and_mission_control() {
    for identity in ["", "   ", "owner"] {
        let (client, _peer) = fixture();
        let mut options = local_options().with_authentication(AhpHostAuthentication::custom(
            identity,
            |_: AhpAuthenticationRequest| async { Ok(None) },
        ));
        if identity == "owner" {
            options = options.with_github_environment(HostGitHubEnvironmentOptions::default());
        }
        let error = client.start_ahp_host(options).await.unwrap_err();
        assert!(matches!(error.kind(), ErrorKind::InvalidConfig));
        assert!(
            client
                .host_sessions()
                .unwrap()
                .0
                .lock()
                .authenticators
                .is_empty()
        );
    }
}

#[tokio::test]
async fn anonymous_selection_is_separate_from_connection_secret() {
    let (client, mut peer) = fixture();
    let pending = start(
        &client,
        local_options().with_authentication(AhpHostAuthentication::None),
    );
    let request = peer.request().await;
    assert_eq!(request["params"]["authentication"], json!({"mode": "none"}));
    peer.started(&request, Some("listener-secret")).await;
    let host = pending.await.unwrap().unwrap();
    assert_eq!(host.token.as_deref(), Some("listener-secret"));
    authenticate(&mut peer, &host.host_id, "unavailable").await;
    let response = peer.request().await;
    assert!(response.get("error").is_some(), "{response}");
}

#[tokio::test]
async fn custom_validator_accepts_rejects_errors_and_panics_at_rpc_boundary() {
    for outcome in ["accept", "reject", "error", "panic"] {
        let (client, mut peer) = fixture();
        let authentication = AhpHostAuthentication::custom(
            "owner",
            move |request: AhpAuthenticationRequest| async move {
                assert_eq!(request.token, "application-token");
                assert!(!request.cancellation_token.is_cancelled());
                match outcome {
                    "accept" => Ok(Some(AhpAuthenticationResult {
                        identity: "owner".into(),
                        expires_at: "2099-01-01T00:00:00Z".into(),
                    })),
                    "reject" => Ok(None),
                    "error" => Err(handoff_error("validator failed")),
                    _ => panic!("validator panic"),
                }
            },
        );
        let pending = start(&client, local_options().with_authentication(authentication));
        let request = peer.request().await;
        assert_eq!(
            request["params"]["authentication"],
            json!({"mode":"custom","requireIdentity":"owner"})
        );
        assert!(!request.to_string().contains("application-token"));
        peer.started(&request, None).await;
        let host = pending.await.unwrap().unwrap();
        authenticate(&mut peer, &host.host_id, "attempt").await;
        let response = peer.request().await;
        match outcome {
            "accept" => assert_eq!(
                response["result"],
                json!({"identity":"owner","expiresAt":"2099-01-01T00:00:00Z"})
            ),
            "reject" => assert!(
                response.get("result").is_some_and(Value::is_null),
                "{response}"
            ),
            _ => assert!(response.get("error").is_some(), "{response}"),
        }
        assert!(
            client
                .host_sessions()
                .unwrap()
                .0
                .lock()
                .authentications
                .is_empty()
        );
    }
}

#[tokio::test]
async fn cancellation_and_host_exit_fence_uncooperative_validator() {
    for ending in ["cancel", "exit", "disconnect"] {
        let (client, mut peer) = fixture();
        let (entered, mut receiver) = mpsc::unbounded_channel();
        let options = local_options().with_authentication(AhpHostAuthentication::custom(
            "owner",
            move |request: AhpAuthenticationRequest| {
                let entered = entered.clone();
                async move {
                    entered.send(request.cancellation_token).unwrap();
                    std::future::pending::<Result<Option<AhpAuthenticationResult>, Error>>().await
                }
            },
        ));
        let pending = start(&client, options);
        let request = peer.request().await;
        peer.started(&request, None).await;
        let host = pending.await.unwrap().unwrap();
        authenticate(&mut peer, &host.host_id, "attempt").await;
        let cancellation = timeout(TIMEOUT, receiver.recv()).await.unwrap().unwrap();
        match ending {
            "cancel" => cancel(&mut peer, &host.host_id, "attempt").await,
            "exit" => peer.exited(&host.host_id).await,
            _ => client.force_stop(),
        }
        timeout(TIMEOUT, cancellation.cancelled()).await.unwrap();
        if ending != "disconnect" {
            let response = peer.request().await;
            assert!(response.get("error").is_some(), "{response}");
        }
    }
}

#[tokio::test]
async fn trusted_identity_is_a_create_and_resume_sibling_not_session_config() {
    let (client, mut peer) = fixture();
    let (seen, mut receiver) = mpsc::unbounded_channel();
    let create_seen = seen.clone();
    let options = local_options()
        .with_create_session(move |request: AhpSessionRequest, _: Client| {
            create_seen.send(request.authentication).unwrap();
            async { Err(handoff_error("observed create identity")) }
        })
        .with_resume_session(move |request: AhpSessionResumeRequest, _: Client| {
            seen.send(request.authentication).unwrap();
            async { Err(handoff_error("observed resume identity")) }
        });
    let pending = start(&client, options);
    let request = peer.request().await;
    peer.started(&request, None).await;
    let host = pending.await.unwrap().unwrap();
    for resume in [false, true] {
        peer.send(json!({
            "jsonrpc": "2.0", "id": 903, "method": "host.materializeSession",
            "params": {"hostId":host.host_id,"handoffId":format!("handoff-{resume}"),
                "resume":resume,"config":{"sessionId":"trusted-session"},
                "authentication":{"mode":"github","identity":"trusted-owner"}}
        }))
        .await;
        let identity = timeout(TIMEOUT, receiver.recv()).await.unwrap().unwrap();
        assert!(
            matches!(identity, AhpSessionAuthentication::Github { identity } if identity == "trusted-owner")
        );
        assert!(peer.request().await.get("error").is_some());
    }
}

#[test]
fn trusted_authentication_must_match_host_policy() {
    use TrustedAuthenticationPolicy as Policy;
    let github = |identity: &str| AhpSessionAuthentication::Github {
        identity: identity.into(),
    };
    let custom = |identity: &str| AhpSessionAuthentication::Custom {
        identity: identity.into(),
    };
    assert!(Policy::Github.accepts(&github("octocat")));
    assert!(!Policy::Github.accepts(&github(" ")));
    assert!(!Policy::Github.accepts(&AhpSessionAuthentication::None));
    assert!(Policy::None.accepts(&AhpSessionAuthentication::None));
    assert!(!Policy::None.accepts(&custom("owner")));
    let owner = Policy::Custom("owner".into());
    assert!(owner.accepts(&custom("owner")));
    assert!(!owner.accepts(&custom("other")));
    assert!(!owner.accepts(&github("owner")));
}
