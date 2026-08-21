// Copyright (c) Microsoft Corporation. All rights reserved.

//! Basic example demonstrating the Copilot SDK for Rust.
//!
//! This example shows how to:
//! - Create and start a client, then connect to the CLI server
//! - Subscribe to session events
//! - Send messages and wait for responses

use copilot_sdk::*;

#[tokio::main]
async fn main() -> Result<(), CopilotError> {
    // ========================================================================
    // 1. Create and start the client
    // ========================================================================
    let client = CopilotClient::new(CopilotClientOptions::default());

    println!("Starting Copilot client...");
    client.start().await?;
    println!("Client connected!");

    // Verify connectivity with a ping
    let ping = client.ping(Some("hello from Rust SDK")).await?;
    println!(
        "Ping response: {} (protocol v{})",
        ping.message,
        ping.protocol_version.unwrap_or(0)
    );

    // ========================================================================
    // 2. List available models (optional)
    // ========================================================================
    match client.list_models().await {
        Ok(models) => {
            println!("\nAvailable models:");
            for model in &models {
                println!("  - {} ({})", model.name, model.id);
            }
        }
        Err(e) => println!("Could not list models: {}", e),
    }

    // ========================================================================
    // 3. Create a session
    // ========================================================================
    let session = client.create_session(SessionConfig::default()).await?;
    println!("\nSession created: {}", session.session_id());

    // ========================================================================
    // 4. Subscribe to events
    // ========================================================================
    let _sub = session
        .on(|event| {
            if let Some(content) = event.assistant_message_content() {
                print!("{}", content);
            }
        })
        .await;

    // ========================================================================
    // 5. Send a message and wait for the response
    // ========================================================================
    println!("\nSending message...");
    let response = session
        .send_and_wait(
            MessageOptions {
                prompt: "What is the weather in San Francisco?".to_string(),
                ..Default::default()
            },
            Some(120_000),
        )
        .await?;

    match response {
        Some(event) => {
            if let Some(content) = event.assistant_message_content() {
                println!("\nsend_and_wait response: {}", content);
            }
        }
        None => println!("\nNo assistant message received."),
    }

    // ========================================================================
    // 6. Send a follow-up message
    // ========================================================================
    println!("\nSending follow-up...");
    let response = session
        .send_and_wait(
            MessageOptions {
                prompt: "Now what about New York?".to_string(),
                ..Default::default()
            },
            None,
        )
        .await?;

    if let Some(event) = response {
        if let Some(content) = event.assistant_message_content() {
            println!("\nFollow-up response: {}", content);
        }
    }

    // ========================================================================
    // 7. Session metadata (optional)
    // ========================================================================
    if let Ok(meta) = client.get_session_metadata(session.session_id()).await {
        println!("\nSession metadata: {:?}", meta);
    }

    // ========================================================================
    // 8. Clean up
    // ========================================================================
    println!("\nCleaning up...");
    session.destroy().await?;
    if let Err(e) = client.stop().await {
        eprintln!("Cleanup errors: {}", e);
    }

    println!("Done!");
    Ok(())
}
