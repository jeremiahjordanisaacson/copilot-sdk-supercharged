// Copyright (c) Microsoft Corporation. All rights reserved.

//! Basic example demonstrating the Copilot SDK for Rust.
//!
//! This example shows how to:
//! - Create a Client and connect to the CLI server
//! - Register custom tools
//! - Subscribe to session events
//! - Send messages and wait for responses

use std::time::Duration;

use github_copilot_sdk::*;

#[tokio::main]
async fn main() -> Result<(), Error> {
    // ========================================================================
    // 1. Create and configure the client
    // ========================================================================
    let options = ClientOptions::default();

    println!("Starting Copilot client...");
    let client = Client::start(options).await?;
    println!("Client connected!");

    // Verify connectivity with a ping
    let ping_response = client.ping(Some("hello from Rust SDK")).await?;
    println!(
        "Ping response: {} (protocol v{})",
        ping_response.message,
        ping_response.protocol_version.unwrap_or(0)
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
        Err(e) => {
            println!("Could not list models: {}", e);
        }
    }

    // ========================================================================
    // 3. Create a session with a custom tool
    // ========================================================================
    let weather_tool = Tool::new("get_weather")
        .with_description("Get the current weather for a location")
        .with_parameters(serde_json::json!({
            "type": "object",
            "properties": {
                "location": {
                    "type": "string",
                    "description": "The city name to get weather for"
                }
            },
            "required": ["location"]
        }));

    let config = SessionConfig::default().with_tools(vec![weather_tool]);

    let session = client.create_session(config).await?;
    println!("\nSession created: {}", session.id());

    // ========================================================================
    // 4. Subscribe to events
    // ========================================================================
    let sub = session.subscribe();

    // Spawn a task to process events
    let event_task = tokio::spawn(async move {
        let mut sub = sub;
        loop {
            match sub.recv().await {
                Ok(event) => match event.event_type.as_str() {
                    "assistant.message_delta" => {
                        if let Some(delta) =
                            event.data.get("deltaContent").and_then(|v| v.as_str())
                        {
                            print!("{}", delta);
                        }
                    }
                    "tool.execution_start" => {
                        let tool_name = event
                            .data
                            .get("toolName")
                            .and_then(|v| v.as_str())
                            .unwrap_or("unknown");
                        println!("\n[Tool executing: {}]", tool_name);
                    }
                    "tool.execution_complete" => {
                        let success = event
                            .data
                            .get("success")
                            .and_then(|v| v.as_bool())
                            .unwrap_or(false);
                        println!("[Tool completed: success={}]", success);
                    }
                    "session.idle" => {
                        println!("\n[Session idle]");
                    }
                    "session.error" => {
                        let msg = event
                            .data
                            .get("message")
                            .and_then(|v| v.as_str())
                            .unwrap_or("unknown error");
                        eprintln!("\n[Session error: {}]", msg);
                    }
                    _ => {}
                },
                Err(RecvError::Closed) => break,
                Err(RecvError::Lagged(_)) => {
                    eprintln!("Skipped some events (lagged)");
                }
                Err(_) => break,
            }
        }
    });

    // ========================================================================
    // 5. Send a message and wait for response
    // ========================================================================
    println!("\nSending message...");
    let response = session
        .send_and_wait(
            MessageOptions::new("What is the weather in San Francisco? Use the get_weather tool.")
                .with_wait_timeout(Duration::from_secs(120)),
        )
        .await?;

    match response {
        Some(event) => {
            if let Some(content) = event.data.get("content").and_then(|v| v.as_str()) {
                println!("\nsend_and_wait response: {}", content);
            }
        }
        None => {
            println!("\nNo assistant message received.");
        }
    }

    // ========================================================================
    // 6. Send a follow-up message
    // ========================================================================
    println!("\nSending follow-up...");
    let response = session
        .send_and_wait(MessageOptions::new("Now what about New York?"))
        .await?;

    if let Some(event) = response {
        if let Some(content) = event.data.get("content").and_then(|v| v.as_str()) {
            println!("\nFollow-up response: {}", content);
        }
    }

    // ========================================================================
    // 7. v2.0 Features
    // ========================================================================

    // Session Metadata
    if let Ok(Some(meta)) = client.get_session_metadata(session.id()).await {
        println!("Session metadata: {:?}", meta);
    }

    // ========================================================================
    // 8. Clean up
    // ========================================================================
    println!("\nCleaning up...");
    event_task.abort();
    session.disconnect().await?;
    if let Err(e) = client.stop().await {
        eprintln!("Cleanup errors: {}", e);
    }

    println!("Done!");
    Ok(())
}
