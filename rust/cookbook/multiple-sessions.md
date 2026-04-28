# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in Rust.

## Creating Multiple Sessions

Each session maintains its own conversation history, so you can run independent tasks in parallel.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    // Create two independent sessions
    let code_review = client.create_session(&SessionConfig::default()).await?;
    let documentation = client.create_session(&SessionConfig::default()).await?;

    // Each session tracks its own context
    let review = code_review
        .send_and_wait(&SendOptions::new("Review this function for bugs"))
        .await?;
    println!("Review: {}", review.message());

    let docs = documentation
        .send_and_wait(&SendOptions::new("Write API docs for UserService"))
        .await?;
    println!("Docs: {}", docs.message());

    client.stop().await?;
    Ok(())
}
```

## Independent Conversations

Sessions do not share context. A follow-up in one session has no effect on the other.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let session1 = client.create_session(&SessionConfig::default()).await?;
    let session2 = client.create_session(&SessionConfig::default()).await?;

    // Session 1 conversation
    session1.send_and_wait(&SendOptions::new("I'm working on a REST API in Rust")).await?;
    let follow_up1 = session1.send_and_wait(&SendOptions::new("How should I handle auth?")).await?;
    println!("Session 1: {}", follow_up1.message());

    // Session 2 conversation (knows nothing about session1)
    session2.send_and_wait(&SendOptions::new("I'm building a CLI tool in Rust")).await?;
    let follow_up2 = session2.send_and_wait(&SendOptions::new("How should I handle auth?")).await?;
    println!("Session 2: {}", follow_up2.message());

    client.stop().await?;
    Ok(())
}
```

## Parallel Requests with tokio::join

Run multiple sessions concurrently using `tokio::join!`.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let s1 = client.create_session(&SessionConfig::default()).await?;
    let s2 = client.create_session(&SessionConfig::default()).await?;
    let s3 = client.create_session(&SessionConfig::default()).await?;

    // Fire all requests concurrently
    let (r1, r2, r3) = tokio::join!(
        s1.send_and_wait(&SendOptions::new("Explain lifetimes")),
        s2.send_and_wait(&SendOptions::new("Explain traits")),
        s3.send_and_wait(&SendOptions::new("Explain async/await")),
    );

    println!("Lifetimes: {}", r1?.message());
    println!("Traits: {}", r2?.message());
    println!("Async: {}", r3?.message());

    client.stop().await?;
    Ok(())
}
```

## Session Lifecycle

Sessions remain active until the client is stopped. Clean up with `client.stop()`.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    // Short-lived session for a one-off task
    let temp = client.create_session(&SessionConfig::default()).await?;
    let result = temp.send_and_wait(&SendOptions::new("Format this JSON")).await?;
    println!("{}", result.message());

    // Long-lived session for interactive use
    let main_session = client.create_session(&SessionConfig::default()).await?;
    main_session.send_and_wait(&SendOptions::new("Let's build a web server")).await?;
    main_session.send_and_wait(&SendOptions::new("Add a health check endpoint")).await?;
    main_session.send_and_wait(&SendOptions::new("Add graceful shutdown")).await?;

    client.stop().await?; // All sessions are cleaned up
    Ok(())
}
```

## Best Practices

- Use separate sessions for tasks that should not share conversational context.
- Use `tokio::join!` or `tokio::spawn` to run independent sessions concurrently.
- Keep session references accessible so you can send follow-up messages to the right context.
- Call `client.stop()` once when done; this cleans up all active sessions.
