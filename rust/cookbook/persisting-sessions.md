# Persisting Sessions

Save and resume sessions across restarts using the GitHub Copilot SDK in Rust.

## Custom Session IDs

Provide a custom session ID to resume a conversation later.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    // Create a session with a custom ID
    let config = SessionConfig {
        session_id: Some("project-alpha-session-001".to_string()),
        ..SessionConfig::default()
    };

    let session = client.create_session(&config).await?;
    let response = session
        .send_and_wait(&SendOptions::new("Let's plan the auth module"))
        .await?;
    println!("{}", response.message());

    client.stop().await?;
    Ok(())
}
```

## Resuming a Session

Use the same session ID to pick up where you left off after a restart.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // First run: start a session
    {
        let client = CopilotClient::new(CopilotClientOptions::default());
        client.start().await?;

        let config = SessionConfig {
            session_id: Some("persistent-session-42".to_string()),
            ..SessionConfig::default()
        };
        let session = client.create_session(&config).await?;
        session
            .send_and_wait(&SendOptions::new("We are building a payment gateway"))
            .await?;
        client.stop().await?;
    }

    // Later run: resume the same session
    {
        let client = CopilotClient::new(CopilotClientOptions::default());
        client.start().await?;

        let config = SessionConfig {
            session_id: Some("persistent-session-42".to_string()),
            ..SessionConfig::default()
        };
        let session = client.create_session(&config).await?;

        // The session remembers previous context
        let response = session
            .send_and_wait(&SendOptions::new("What were we building?"))
            .await?;
        println!("{}", response.message()); // Refers to the payment gateway

        client.stop().await?;
    }

    Ok(())
}
```

## Infinite Sessions with Compaction

Enable infinite sessions for long-running conversations. The SDK automatically compacts
history when it grows too large, emitting compaction events.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let config = SessionConfig {
        session_id: Some("long-running-agent".to_string()),
        infinite_session: Some(true),
        ..SessionConfig::default()
    };

    let session = client.create_session(&config).await?;

    // Listen for compaction events
    session.on_event("session.compaction_start", |_event| {
        println!("Compaction started: conversation history is being summarized...");
    });
    session.on_event("session.compaction_complete", |_event| {
        println!("Compaction complete: history has been summarized.");
    });

    // Long-running loop that may trigger compaction
    let tasks = vec![
        "Analyze the auth module",
        "Review the database schema",
        "Check the API endpoints",
        "Suggest performance improvements",
        "Write unit tests for the service layer",
    ];

    for task in tasks {
        let response = session.send_and_wait(&SendOptions::new(task)).await?;
        println!("Task: {}", task);
        println!("Response: {}\n", response.message());
    }

    client.stop().await?;
    Ok(())
}
```

## Session State Location

By default, session state is persisted to `~/.copilot/session-state/{sessionId}`.
This allows automatic resumption without any extra configuration.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let config = SessionConfig {
        session_id: Some("my-app-session".to_string()),
        ..SessionConfig::default()
    };

    let _session = client.create_session(&config).await?;
    println!("Session ID: my-app-session");
    println!("State stored at: ~/.copilot/session-state/my-app-session/");

    client.stop().await?;
    Ok(())
}
```

## Best Practices

- Use descriptive, deterministic session IDs (e.g., `"user-123-project-alpha"`) for easy resumption.
- Enable infinite sessions (`infinite_session: Some(true)`) for long-running agents.
- Listen for compaction events to provide user feedback during context summarization.
- Session state is stored in `~/.copilot/session-state/{sessionId}` by default.
- Avoid reusing session IDs across unrelated conversations to prevent context pollution.
