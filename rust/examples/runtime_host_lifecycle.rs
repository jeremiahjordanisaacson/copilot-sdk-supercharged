//! Run with `--no-default-features --features local-runtime` and a prepared
//! same-checkout `COPILOT_CLI_PATH`. This example disables telemetry uploads.
//! Pass `-- --in-process` to inspect the client's native host identity.
//! In-process mode inherits host settings. Set `COPILOT_GITHUB_TELEMETRY_DISABLED=true`,
//! `COPILOT_TELEMETRY_DISABLED=true`, and `DO_NOT_TRACK=1` before running it.
//! Pass `-- --observe-ahp-exit` to observe a real loopback listener across a restart.
//! A retired subscription cannot cancel the replacement that uses the same callback.

use std::error::Error;
use std::sync::Arc;
use std::time::Duration;

use copilot_sdk::rpc::{HostDisposeRequest, HostLocalServerOptions, HostStartRequest};
use copilot_sdk::{
    AhpHostExitReason, Client, ClientOptions, GetStatusResponse, RuntimeConnection,
    SpawnedProcessOptions,
};

async fn demonstrate(client: &Client, observe_exit: bool) -> Result<(), Box<dyn Error>> {
    let pending = client.begin_call("status.get", None)?;
    println!("Status request queued before awaiting its response");
    let status: GetStatusResponse = serde_json::from_value(pending.await?)?;
    println!("Runtime version: {}", status.version);
    println!("Protocol version: {}", status.protocol_version);
    if observe_exit {
        let host_id = uuid::Uuid::new_v4().to_string();
        let (sender, mut exits) = tokio::sync::mpsc::unbounded_channel();
        let callback = Arc::new(move |exit: copilot_sdk::AhpHostExit| {
            let _ = sender.send(exit);
        });
        let options = HostStartRequest {
            host_id: host_id.clone(),
            authentication: Some(serde_json::json!({ "mode": "none" })),
            allow_creating_new_sessions: Some(false),
            local_server: Some(HostLocalServerOptions {
                hostname: Some("127.0.0.1".to_owned()),
                port: Some(0),
                require_connection_token: Some(false),
                token: None,
            }),
            ..Default::default()
        };
        let mut previous = None;
        for _ in 0..2 {
            let subscription = client.observe_ahp_host_exit(host_id.clone(), callback.clone())?;
            drop(previous.take());
            println!("AHP observer registered before host.start: {host_id}");
            let host = client.rpc().host().start(options.clone()).await?;
            println!("AHP loopback listener started: {:?}", host.url);
            client
                .rpc()
                .host()
                .dispose(HostDisposeRequest {
                    host_id: host_id.clone(),
                })
                .await?;
            let exit = tokio::time::timeout(Duration::from_secs(10), exits.recv())
                .await?
                .ok_or_else(|| {
                    std::io::Error::other("AHP exit observer closed without an event")
                })?;
            println!("host.exited: {}", serde_json::to_string(&exit)?);
            if exit.host_id != host_id || exit.reason != AhpHostExitReason::Disposed {
                return Err(std::io::Error::other("Unexpected AHP exit identity or reason").into());
            }
            previous = Some(subscription);
        }
        drop(previous);
        println!("AHP observer dropped after disposal");
    }
    Ok(())
}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let home = tempfile::tempdir()?;
    let in_process = std::env::args().any(|arg| arg == "--in-process");
    let transport = if in_process {
        RuntimeConnection::for_in_process(Default::default())
    } else {
        RuntimeConnection::for_spawned_stdio(SpawnedProcessOptions::new().with_env(
            std::env::vars_os().chain([
                ("COPILOT_GITHUB_TELEMETRY_DISABLED".into(), "true".into()),
                ("COPILOT_TELEMETRY_DISABLED".into(), "true".into()),
                ("DO_NOT_TRACK".into(), "1".into()),
            ]),
        ))
    };
    let mut runtime = transport;
    runtime
        .owned_options_mut()
        .expect("owned runtime")
        .base_directory = Some(home.path().to_owned());
    let options = ClientOptions::new(copilot_sdk::ClientMode::CodingAgent)
        .with_runtime(runtime)
        .with_github(|group| group.with_token("").with_use_logged_in_user(false))
        .with_observability(|group| group.with_disable_client_telemetry(true));
    let client = Client::start(options).await?;
    println!("Runtime started through the SDK");
    println!("In-process server ID: {:?}", client.in_process_server_id());
    println!(
        "In-process connection ID: {:?}",
        client.in_process_connection_id()
    );
    let observe_exit = std::env::args().any(|arg| arg == "--observe-ahp-exit");
    let result = demonstrate(&client, observe_exit).await;
    let stopped = client.stop().await;
    if let Err(error) = result {
        if let Err(shutdown_error) = stopped {
            eprintln!("Shutdown also failed: {shutdown_error}");
        }
        return Err(error);
    }
    stopped?;
    println!(
        "In-process server after shutdown: {:?}",
        client.in_process_server_id()
    );
    println!("Runtime shutdown complete");
    Ok(())
}
