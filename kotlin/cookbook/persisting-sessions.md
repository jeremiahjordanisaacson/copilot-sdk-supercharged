# Persisting Sessions

Save and resume sessions across restarts using the GitHub Copilot SDK in Kotlin.

## Custom Session IDs

Provide a custom session ID to resume a conversation later.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        // Create a session with a custom ID
        val config = SessionConfig(sessionId = "project-alpha-session-001")
        val session = client.createSession(config)

        val response = session.sendAndWait(SendOptions(message = "Let's plan the auth module"))
        println(response.message)
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Resuming a Session

Use the same session ID to pick up where you left off after a restart.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    // First run: start a session
    val client1 = CopilotClient()
    try {
        client1.start()
        val config = SessionConfig(sessionId = "persistent-session-42")
        val session = client1.createSession(config)
        session.sendAndWait(SendOptions(message = "We are building a payment gateway"))
    } finally {
        client1.stop()
    }

    // Later run: resume the same session
    val client2 = CopilotClient()
    try {
        client2.start()
        val config = SessionConfig(sessionId = "persistent-session-42")
        val session = client2.createSession(config)

        // The session remembers previous context
        val response = session.sendAndWait(SendOptions(message = "What were we building?"))
        println(response.message) // Refers to the payment gateway
    } finally {
        client2.stop()
    }
}
```

## Infinite Sessions with Compaction

Enable infinite sessions for long-running conversations. The SDK automatically compacts
history when it grows too large, emitting compaction events.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val config = SessionConfig(
            sessionId = "long-running-agent",
            infiniteSession = true
        )
        val session = client.createSession(config)

        // Listen for compaction events
        session.onEvent("session.compaction_start") {
            println("Compaction started: conversation history is being summarized...")
        }
        session.onEvent("session.compaction_complete") {
            println("Compaction complete: history has been summarized.")
        }

        // Long-running loop that may trigger compaction
        val tasks = listOf(
            "Analyze the auth module",
            "Review the database schema",
            "Check the API endpoints",
            "Suggest performance improvements",
            "Write unit tests for the service layer"
        )

        for (task in tasks) {
            val response = session.sendAndWait(SendOptions(message = task))
            println("Task: $task")
            println("Response: ${response.message}\n")
        }
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Session State Location

By default, session state is persisted to `~/.copilot/session-state/{sessionId}`.
This allows automatic resumption without any extra configuration.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val config = SessionConfig(sessionId = "my-app-session")
        val session = client.createSession(config)

        println("Session ID: ${config.sessionId}")
        println("State stored at: ~/.copilot/session-state/my-app-session/")
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Best Practices

- Use descriptive, deterministic session IDs (e.g., `"user-123-project-alpha"`) for easy resumption.
- Enable infinite sessions (`infiniteSession = true`) for long-running agents or interactive tools.
- Listen for compaction events to provide user feedback during context summarization.
- Session state is stored in `~/.copilot/session-state/{sessionId}` by default.
- Avoid reusing session IDs across unrelated conversations to prevent context pollution.
