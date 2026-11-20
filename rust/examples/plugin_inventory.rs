//! Lists session plugins and their verified-source metadata.
//! Run with `--no-default-features --features local-runtime --example plugin_inventory`
//! and a prepared same-checkout `COPILOT_CLI_PATH`.

use std::path::Path;
use std::sync::Arc;

use copilot_sdk::handler::ApproveAllHandler;
use copilot_sdk::types::CreateSessionOptions;
use copilot_sdk::{
    Client, ClientMode, ClientOptions, Result, RuntimeConnection, SpawnedProcessOptions,
};

async fn demonstrate(client: &Client, profile: &Path) -> Result<()> {
    let session = client
        .create_session(
            CreateSessionOptions::default()
                .with_permissions(|group| group.with_on_request(Arc::new(ApproveAllHandler)))
                .with_config_directory(profile)
                .with_enable_config_discovery(false),
        )
        .await?;
    let inventory = session.rpc().plugins().list().await?;
    println!("Session plugin count: {}", inventory.plugins.len());
    println!("Session plugin inventory with verified source metadata:");
    println!("{}", serde_json::to_string_pretty(&inventory)?);
    session.disconnect().await?;
    Ok(())
}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let profile = tempfile::tempdir()?;
    let mut runtime = RuntimeConnection::for_spawned_stdio(SpawnedProcessOptions::new().with_env(
        std::env::vars_os().chain([
            ("COPILOT_GITHUB_TELEMETRY_DISABLED".into(), "true".into()),
            ("COPILOT_TELEMETRY_DISABLED".into(), "true".into()),
            ("DO_NOT_TRACK".into(), "1".into()),
        ]),
    ));
    runtime
        .owned_options_mut()
        .expect("owned runtime")
        .base_directory = Some(profile.path().to_owned());
    let options = ClientOptions::new(ClientMode::CodingAgent)
        .with_runtime(runtime)
        .with_github(|group| group.with_token("").with_use_logged_in_user(false))
        .with_observability(|group| group.with_disable_client_telemetry(true));
    let client = Client::start(options).await?;
    let result = demonstrate(&client, profile.path()).await;
    let stopped = client.stop().await;
    if let Err(error) = result {
        if let Err(shutdown_error) = stopped {
            eprintln!("Shutdown also failed: {shutdown_error}");
        }
        return Err(error.into());
    }
    stopped?;
    Ok(())
}
