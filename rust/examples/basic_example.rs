// Copyright (c) Microsoft Corporation. All rights reserved.

//! Basic example demonstrating the Copilot SDK for Rust.
//!
//! This example shows how to:
//! - Create a CopilotClient and connect to the CLI server
//! - Register custom tools
//! - Subscribe to session events
//! - Send messages and wait for responses

use std::sync::Arc;

use copilot_sdk::*;

/// A simple weather tool that returns mock weather data.
fn create_weather_tool() -> (ToolDefinition, ToolHandler) {
    let definition = ToolDefinition {
        name: "get_weather".to_string(),
        description: Some("Get the current weather for a location".to_string()),
        parameters: Some(serde_json::json!({
            "type": "object",
            "properties": {
                "location": {
                    "type": "string",
                    "description": "The city name to get weather for"
                }
            },
            "required": ["location"]
        })),
    };

    let handler: ToolHandler = Arc::new(|args, _invocation| {
        Box::pin(async move {
            let location = args
                .get("location")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown");

            let result = serde_json::json!({
                "location": location,
                "temperature": 72,
                "unit": "fahrenheit",
                "condition": "sunny",
                "humidity": 45
            });

            Ok(result)
        })
    });

    (definition, handler)
}

#[tokio::main]
async fn main() -> Result<(), CopilotError> {
    // ========================================================================
    // 1. Create and configure the client
    // ========================================================================
    let options = CopilotClientOptions {
        cli_path: Some("copilot-cli".to_string()), // Path to your CLI executable
        log_level: "info".to_string(),
        ..Default::default()
    };

    let client = CopilotClient::new(options);

    // Start the connection
    println!("Starting Copilot client...");
    client.start().await?;
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
    let (weather_def, weather_handler) = create_weather_tool();

    let config = SessionConfig {
        tools: Some(vec![weather_def.clone()]),
        request_permission: Some(false),
        request_user_input: Some(false),
        ..Default::default()
    };

    let session = client.create_session(config).await?;
    println!("\nSession created: {}", session.session_id());

    // Register the tool handler on the session
    session
        .register_tool(&weather_def.name, weather_handler)
        .await;

    // ========================================================================
    // 4. Subscribe to events
    // ========================================================================

    // Subscribe to all events
    let _all_events_sub = session
        .on(|event: SessionEvent| {
            match event.event_type.as_str() {
                "assistant.message_delta" => {
                    // Streaming delta - print without newline
                    if let Some(delta) = event.data.get("deltaContent").and_then(|v| v.as_str()) {
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
                _ => {
                    // Ignore other event types in this example
                }
            }
        })
        .await;

    // Subscribe to just assistant.message events
    let _msg_sub = session
        .on_event("assistant.message", |event| {
            if let Some(content) = event.assistant_message_content() {
                println!("\n\n=== Final Assistant Message ===");
                println!("{}", content);
                println!("===============================");
            }
        })
        .await;

    // ========================================================================
    // 5. Send a message and wait for response
    // ========================================================================
    println!("\nSending message...");
    let response = session
        .send_and_wait(
            MessageOptions {
                prompt: "What is the weather in San Francisco? Use the get_weather tool."
                    .to_string(),
                attachments: None,
                mode: None,
            },
            Some(120_000), // 2 minute timeout
        )
        .await?;

    match response {
        Some(event) => {
            if let Some(content) = event.assistant_message_content() {
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
        .send_and_wait(
            MessageOptions {
                prompt: "Now what about New York?".to_string(),
                attachments: None,
                mode: None,
            },
            None, // default 60s timeout
        )
        .await?;

    if let Some(event) = response {
        if let Some(content) = event.assistant_message_content() {
            println!("\nFollow-up response: {}", content);
        }
    }

    // ========================================================================
    // 7. Clean up
    // ========================================================================
    println!("\nCleaning up...");
    session.destroy().await?;
    let errors = client.stop().await?;
    if !errors.is_empty() {
        eprintln!("Cleanup errors:");
        for e in &errors {
            eprintln!("  - {}", e);
        }
    }

    println!("Done!");
    Ok(())
}
