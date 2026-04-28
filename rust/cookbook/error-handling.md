# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Rust.

## Basic Error Handling

Use the `?` operator and `Result` types for clean error propagation.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let session = client.create_session(&SessionConfig::default()).await?;
    let response = session.send_and_wait(&SendOptions::new("Explain ownership")).await?;

    println!("{}", response.message());

    client.stop().await?;
    Ok(())
}
```

## Handling Specific Error Types

Match on error variants to handle different failure modes.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions, SdkError};

#[tokio::main]
async fn main() {
    let client = CopilotClient::new(CopilotClientOptions::default());

    if let Err(e) = client.start().await {
        match e {
            SdkError::ConnectionFailed(msg) => {
                eprintln!("Connection failed. Is the Copilot CLI running? {}", msg);
                return;
            }
            SdkError::Timeout => {
                eprintln!("Connection timed out.");
                return;
            }
            other => {
                eprintln!("Unexpected error: {}", other);
                return;
            }
        }
    }

    let session = match client.create_session(&SessionConfig::default()).await {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to create session: {}", e);
            let _ = client.stop().await;
            return;
        }
    };

    match session.send_and_wait(&SendOptions::new("Hello")).await {
        Ok(response) => println!("{}", response.message()),
        Err(e) => eprintln!("Send failed: {}", e),
    }

    let _ = client.stop().await;
}
```

## Timeout Handling

Use `tokio::time::timeout` to cap how long a request can run.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use std::time::Duration;
use tokio::time::timeout;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let session = client.create_session(&SessionConfig::default()).await?;

    let result = timeout(
        Duration::from_secs(30),
        session.send_and_wait(&SendOptions::new("Summarize this codebase")),
    )
    .await;

    match result {
        Ok(Ok(response)) => println!("{}", response.message()),
        Ok(Err(e)) => eprintln!("SDK error: {}", e),
        Err(_) => eprintln!("Request timed out after 30 seconds"),
    }

    client.stop().await?;
    Ok(())
}
```

## Aborting a Request

Use a `CancellationToken` or drop the future to cancel in-flight requests.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use tokio_util::sync::CancellationToken;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let session = client.create_session(&SessionConfig::default()).await?;
    let token = CancellationToken::new();
    let cancel = token.clone();

    let handle = tokio::spawn(async move {
        tokio::select! {
            result = session.send_and_wait(&SendOptions::new("Write a long essay")) => {
                match result {
                    Ok(r) => println!("{}", r.message()),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            _ = cancel.cancelled() => {
                println!("Request was cancelled.");
            }
        }
    });

    // Cancel after 5 seconds
    tokio::time::sleep(std::time::Duration::from_secs(5)).await;
    token.cancel();
    handle.await?;

    client.stop().await?;
    Ok(())
}
```

## Graceful Shutdown

Use `Drop` or signal handling to guarantee cleanup.

```rust
use copilot_sdk::{CopilotClient, CopilotClientOptions, SessionConfig, SendOptions};
use tokio::signal;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = CopilotClient::new(CopilotClientOptions::default());
    client.start().await?;

    let session = client.create_session(&SessionConfig::default()).await?;

    tokio::select! {
        result = session.send_and_wait(&SendOptions::new("Hello from Rust")) => {
            match result {
                Ok(r) => println!("{}", r.message()),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        _ = signal::ctrl_c() => {
            println!("Received Ctrl+C, shutting down...");
        }
    }

    client.stop().await?;
    Ok(())
}
```

## Best Practices

- Propagate errors with `?` wherever possible instead of `unwrap()`.
- Use `tokio::time::timeout` for all production requests to prevent indefinite hangs.
- Match on specific error variants for meaningful error messages.
- Always call `client.stop().await` before exiting, even on error paths.
- Use `tokio::select!` to race cancellation against long-running requests.
- Prefer `Result<T, E>` returns over `panic!` in library code.
