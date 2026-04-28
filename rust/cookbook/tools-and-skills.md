# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in Rust.

## Defining a Simple Tool

Use the `define_tool!` macro to expose a function that the model can call.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use copilot_sdk::tools::define_tool;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    // Define a tool the model can call
    let weather_tool = define_tool!(
        "get_weather",
        "Get the current weather for a city",
        |args| {
            let city = args.get_str("city")?;
            Ok(format!(
                r#"{{"city": "{}", "temp": "72F", "condition": "sunny"}}"#,
                city
            ))
        }
    );

    let config = SessionConfig {
        tools: vec![weather_tool],
        ..SessionConfig::default()
    };

    let session = client.create_session(&config).await?;
    let response = session
        .send_and_wait(&SendOptions::new("What is the weather in Seattle?"))
        .await?;
    println!("{}", response.message());

    client.stop().await?;
    Ok(())
}
```

## Tool with Parameters

Define tools with typed parameters.

```rust
use copilot_sdk::tools::{define_tool, ToolParam};

let search_tool = define_tool!(
    "search_docs",
    "Search the documentation for a topic",
    params: [
        ToolParam::required("query", "string", "The search query"),
        ToolParam::optional("limit", "integer", "Max results to return"),
    ],
    |args| {
        let query = args.get_str("query")?;
        let limit = args.get_int("limit").unwrap_or(10);
        Ok(format!(
            r#"[{{"title": "Result for '{}'", "count": {}}}]"#,
            query, limit
        ))
    }
);
```

## Multiple Tools

Register multiple tools on a single session.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use copilot_sdk::tools::define_tool;
use std::fs;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let read_file = define_tool!("read_file", "Read a file from disk", |args| {
        let path = args.get_str("path")?;
        Ok(fs::read_to_string(path)?)
    });

    let list_files = define_tool!("list_files", "List files in a directory", |args| {
        let dir = args.get_str("directory")?;
        let entries: Vec<String> = fs::read_dir(dir)?
            .filter_map(|e| e.ok())
            .map(|e| e.file_name().to_string_lossy().to_string())
            .collect();
        Ok(entries.join("\n"))
    });

    let config = SessionConfig {
        tools: vec![read_file, list_files],
        ..SessionConfig::default()
    };

    let session = client.create_session(&config).await?;
    let response = session
        .send_and_wait(&SendOptions::new("List files in /src and read main.rs"))
        .await?;
    println!("{}", response.message());

    client.stop().await?;
    Ok(())
}
```

## Skills Configuration

Pass skill definitions in the session config to enable specialized behaviors.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use copilot_sdk::skills::Skill;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let code_review = Skill::new("code-review", "Review code for bugs and improvements");

    let config = SessionConfig {
        skills: vec![code_review],
        ..SessionConfig::default()
    };

    let session = client.create_session(&config).await?;
    let response = session
        .send_and_wait(&SendOptions::new("Review this pull request for security issues"))
        .await?;
    println!("{}", response.message());

    client.stop().await?;
    Ok(())
}
```

## Listening for Sub-Agent Events

When the model spawns sub-agents, listen for their lifecycle events.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use copilot_sdk::tools::define_tool;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let analyzer = define_tool!("analyze_code", "Analyze code quality", |_args| {
        Ok(r#"{"issues": 3, "severity": "medium"}"#.to_string())
    });

    let config = SessionConfig {
        tools: vec![analyzer],
        ..SessionConfig::default()
    };

    let session = client.create_session(&config).await?;

    // Listen for sub-agent events
    session.on_event("assistant.tool_call", |event| {
        println!("Tool called: {}", event.tool_name());
    });
    session.on_event("assistant.tool_result", |_event| {
        println!("Tool result received");
    });

    let response = session
        .send_and_wait(&SendOptions::new("Analyze the code in /src for quality issues"))
        .await?;
    println!("{}", response.message());

    client.stop().await?;
    Ok(())
}
```

## Best Practices

- Give tools clear, descriptive names and descriptions so the model knows when to use them.
- Use the `define_tool!` macro for concise tool definitions.
- Return `Result` from tool handlers to propagate errors cleanly.
- Return structured JSON from tool handlers for easier model interpretation.
- Keep tool handlers fast; the model waits for the result before continuing.
- Use skills to specialize session behavior without adding custom tools.
