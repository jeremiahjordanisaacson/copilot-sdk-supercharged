# Persisting Sessions

Save and resume sessions across restarts using the GitHub Copilot SDK in Swift.

## Custom Session IDs

Provide a custom session ID to resume a conversation later.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    // Create a session with a custom ID
    var config = SessionConfig()
    config.sessionId = "project-alpha-session-001"

    let session = try await client.createSession(config: config)
    let response = try await session.sendAndWait(
        options: SendOptions(message: "Let's plan the auth module")
    )
    print(response.message)
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Resuming a Session

Use the same session ID to pick up where you left off after a restart.

```swift
import CopilotSDK

// First run: start a session
do {
    let client1 = CopilotClient()
    try await client1.start()

    var config = SessionConfig()
    config.sessionId = "persistent-session-42"

    let session = try await client1.createSession(config: config)
    _ = try await session.sendAndWait(
        options: SendOptions(message: "We are building a payment gateway")
    )
    try await client1.stop()
}

// Later run: resume the same session
do {
    let client2 = CopilotClient()
    try await client2.start()

    var config = SessionConfig()
    config.sessionId = "persistent-session-42"

    let session = try await client2.createSession(config: config)

    // The session remembers previous context
    let response = try await session.sendAndWait(
        options: SendOptions(message: "What were we building?")
    )
    print(response.message) // Refers to the payment gateway

    try await client2.stop()
} catch {
    print("Error: \(error)")
}
```

## Infinite Sessions with Compaction

Enable infinite sessions for long-running conversations. The SDK automatically compacts
history when it grows too large, emitting compaction events.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    var config = SessionConfig()
    config.sessionId = "long-running-agent"
    config.infiniteSession = true

    let session = try await client.createSession(config: config)

    // Listen for compaction events
    session.onEvent("session.compaction_start") { _ in
        print("Compaction started: conversation history is being summarized...")
    }
    session.onEvent("session.compaction_complete") { _ in
        print("Compaction complete: history has been summarized.")
    }

    // Long-running loop that may trigger compaction
    let tasks = [
        "Analyze the auth module",
        "Review the database schema",
        "Check the API endpoints",
        "Suggest performance improvements",
        "Write unit tests for the service layer",
    ]

    for task in tasks {
        let response = try await session.sendAndWait(
            options: SendOptions(message: task)
        )
        print("Task: \(task)")
        print("Response: \(response.message)\n")
    }
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Session State Location

By default, session state is persisted to `~/.copilot/session-state/{sessionId}`.
This allows automatic resumption without any extra configuration.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    var config = SessionConfig()
    config.sessionId = "my-app-session"

    let _ = try await client.createSession(config: config)
    print("Session ID: my-app-session")
    print("State stored at: ~/.copilot/session-state/my-app-session/")
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Best Practices

- Use descriptive, deterministic session IDs (e.g., `"user-123-project-alpha"`) for easy resumption.
- Enable infinite sessions (`infiniteSession = true`) for long-running agents or interactive tools.
- Listen for compaction events to provide user feedback during context summarization.
- Session state is stored in `~/.copilot/session-state/{sessionId}` by default.
- Avoid reusing session IDs across unrelated conversations to prevent context pollution.
