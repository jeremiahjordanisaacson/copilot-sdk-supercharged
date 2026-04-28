# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Swift.

## Basic Error Handling

Use `do/try/catch` with async/await for clean error handling.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()
    let session = try await client.createSession(config: SessionConfig())
    let response = try await session.sendAndWait(options: SendOptions(message: "Explain Swift optionals"))
    print(response.message)
} catch {
    print("SDK error: \(error.localizedDescription)")
}

try? await client.stop()
```

## Handling Specific Error Types

Match on SDK-specific error types for targeted recovery logic.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()
    let session = try await client.createSession(config: SessionConfig())
    let response = try await session.sendAndWait(options: SendOptions(message: "Hello"))
    print(response.message)
} catch let error as CopilotError {
    switch error {
    case .connectionFailed(let msg):
        print("Connection failed. Is the Copilot CLI running? \(msg)")
    case .timeout:
        print("Request timed out.")
    case .requestFailed(let msg):
        print("Request failed: \(msg)")
    }
} catch {
    print("Unexpected error: \(error)")
}

try? await client.stop()
```

## Timeout Handling

Use a `Task` with a timeout to cap how long a request can run.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()
    let session = try await client.createSession(config: SessionConfig())

    let response = try await withThrowingTaskGroup(of: SendResponse.self) { group in
        group.addTask {
            try await session.sendAndWait(
                options: SendOptions(message: "Summarize this codebase")
            )
        }
        group.addTask {
            try await Task.sleep(nanoseconds: 30_000_000_000) // 30 seconds
            throw CopilotError.timeout
        }
        let result = try await group.next()!
        group.cancelAll()
        return result
    }

    print(response.message)
} catch CopilotError.timeout {
    print("Request timed out after 30 seconds")
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Aborting a Request

Use structured concurrency with `Task.cancel()` to abort a request.

```swift
import CopilotSDK

let client = CopilotClient()

do {
    try await client.start()
    let session = try await client.createSession(config: SessionConfig())

    let task = Task {
        let response = try await session.sendAndWait(
            options: SendOptions(message: "Write a long essay")
        )
        print(response.message)
    }

    // Cancel after 5 seconds
    try await Task.sleep(nanoseconds: 5_000_000_000)
    task.cancel()
    print("Request was cancelled.")
} catch {
    print("Error: \(error)")
}

try? await client.stop()
```

## Graceful Shutdown

Use `defer` or signal handling to guarantee the client stops on exit.

```swift
import CopilotSDK
import Foundation

@main
struct App {
    static func main() async {
        let client = CopilotClient()

        // Handle SIGINT for graceful shutdown
        let sigintSource = DispatchSource.makeSignalSource(signal: SIGINT, queue: .main)
        signal(SIGINT, SIG_IGN)
        sigintSource.setEventHandler {
            print("\nReceived interrupt, shutting down...")
            Task {
                try? await client.stop()
                exit(0)
            }
        }
        sigintSource.resume()

        do {
            try await client.start()
            defer {
                Task { try? await client.stop() }
            }

            let session = try await client.createSession(config: SessionConfig())
            let response = try await session.sendAndWait(
                options: SendOptions(message: "Hello from Swift")
            )
            print(response.message)
        } catch {
            print("Error: \(error)")
        }
    }
}
```

## Best Practices

- Always call `client.stop()` using `defer` or in cleanup logic, even on error paths.
- Use `try?` for cleanup calls where you do not need to handle stop errors.
- Catch SDK-specific error types with `catch let error as CopilotError` for targeted recovery.
- Use `withThrowingTaskGroup` or `Task.sleep` for timeout logic in async contexts.
- Use `Task.cancel()` for aborting in-flight requests with structured concurrency.
- Prefer `async/await` over callback-based patterns for cleaner error propagation.
