# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in Swift.

## Creating Multiple Sessions

Each session maintains its own conversation history, so you can run independent tasks in parallel.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    // Create two independent sessions
    let codeReview = try await client.createSession(config: SessionConfig())
    let documentation = try await client.createSession(config: SessionConfig())

    // Each session tracks its own context
    let reviewResponse = try await codeReview.sendAndWait(
        options: SendOptions(message: "Review this function for bugs")
    )
    print("Review: \(reviewResponse.message)")

    let docsResponse = try await documentation.sendAndWait(
        options: SendOptions(message: "Write API docs for UserService")
    )
    print("Docs: \(docsResponse.message)")
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Independent Conversations

Sessions do not share context. A follow-up in one session has no effect on the other.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    let session1 = try await client.createSession(config: SessionConfig())
    let session2 = try await client.createSession(config: SessionConfig())

    // Session 1 conversation
    _ = try await session1.sendAndWait(
        options: SendOptions(message: "I'm working on a SwiftUI app")
    )
    let followUp1 = try await session1.sendAndWait(
        options: SendOptions(message: "How should I handle auth?")
    )
    print("Session 1: \(followUp1.message)")

    // Session 2 conversation (knows nothing about session1)
    _ = try await session2.sendAndWait(
        options: SendOptions(message: "I'm building a server with Vapor")
    )
    let followUp2 = try await session2.sendAndWait(
        options: SendOptions(message: "How should I handle auth?")
    )
    print("Session 2: \(followUp2.message)")
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Parallel Requests with TaskGroup

Run multiple sessions concurrently using structured concurrency.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    let s1 = try await client.createSession(config: SessionConfig())
    let s2 = try await client.createSession(config: SessionConfig())
    let s3 = try await client.createSession(config: SessionConfig())

    let results = try await withThrowingTaskGroup(of: (String, String).self) { group in
        let pairs: [(CopilotSession, String)] = [
            (s1, "Explain optionals"),
            (s2, "Explain protocols"),
            (s3, "Explain async/await"),
        ]
        for (session, prompt) in pairs {
            group.addTask {
                let response = try await session.sendAndWait(
                    options: SendOptions(message: prompt)
                )
                return (prompt, response.message)
            }
        }

        var collected: [(String, String)] = []
        for try await result in group {
            collected.append(result)
        }
        return collected
    }

    for (prompt, response) in results {
        print("\(prompt): \(response)")
    }
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Session Lifecycle

Sessions remain active until the client is stopped. Clean up with `client.stop()`.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()

    // Short-lived session for a one-off task
    let temp = try await client.createSession(config: SessionConfig())
    let result = try await temp.sendAndWait(
        options: SendOptions(message: "Format this JSON")
    )
    print(result.message)

    // Long-lived session for interactive use
    let mainSession = try await client.createSession(config: SessionConfig())
    _ = try await mainSession.sendAndWait(options: SendOptions(message: "Let's build an API"))
    _ = try await mainSession.sendAndWait(options: SendOptions(message: "Add a /users endpoint"))
    _ = try await mainSession.sendAndWait(options: SendOptions(message: "Add pagination"))
} catch {
    print("Error: \(error)")
}

try? await client.stop() // All sessions are cleaned up
```

## Best Practices

- Use separate sessions for tasks that should not share conversational context.
- Use `withThrowingTaskGroup` for concurrent requests with structured concurrency.
- Keep session references accessible so you can send follow-up messages to the right context.
- Call `client.stop()` once when done; this cleans up all active sessions.
