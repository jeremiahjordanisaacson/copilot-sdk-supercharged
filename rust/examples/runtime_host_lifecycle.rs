//! Run with `--no-default-features --features local-runtime` and a prepared
//! same-checkout `COPILOT_CLI_PATH`. This example disables telemetry uploads.
//! Pass `-- --in-process` to inspect the client's native host identity.
//! In-process mode inherits host settings. Set `COPILOT_GITHUB_TELEMETRY_DISABLED=true`,
//! `COPILOT_TELEMETRY_DISABLED=true`, and `DO_NOT_TRACK=1` before running it.

use copilot_sdk::RuntimeConnection;
use copilot_sdk::{Client, ClientOptions, GetStatusResponse, Result, SpawnedProcessOptions};

async fn demonstrate(client: &Client) -> Result<()> {
    let pending = client.begin_call("status.get", None)?;
    println!("Status request queued before awaiting its response");
    let status: GetStatusResponse = serde_json::from_value(pending.await?)?;
    println!("Runtime version: {}", status.version);
    println!("Protocol version: {}", status.protocol_version);
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
    let result = demonstrate(&client).await;
    let stopped = client.stop().await;
    if let Err(error) = result {
        if let Err(shutdown_error) = stopped {
            eprintln!("Shutdown also failed: {shutdown_error}");
        }
        return Err(error.into());
    }
    stopped?;
    println!(
        "In-process server after shutdown: {:?}",
        client.in_process_server_id()
    );
    println!("Runtime shutdown complete");
    Ok(())
}
