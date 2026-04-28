# Multiple Sessions

Manage multiple independent conversations simultaneously using the GitHub Copilot SDK in Kotlin.

## Creating Multiple Sessions

Each session maintains its own conversation history, so you can run independent tasks in parallel.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        // Create two independent sessions
        val codeReview = client.createSession(SessionConfig())
        val documentation = client.createSession(SessionConfig())

        // Each session tracks its own context
        val reviewResponse = codeReview.sendAndWait(
            SendOptions(message = "Review this function for bugs")
        )
        println("Review: ${reviewResponse.message}")

        val docsResponse = documentation.sendAndWait(
            SendOptions(message = "Write API docs for UserService")
        )
        println("Docs: ${docsResponse.message}")
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Independent Conversations

Sessions do not share context. A follow-up in one session has no effect on the other.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val session1 = client.createSession(SessionConfig())
        val session2 = client.createSession(SessionConfig())

        // Session 1 conversation
        session1.sendAndWait(SendOptions(message = "I'm working on a Ktor server"))
        val followUp1 = session1.sendAndWait(SendOptions(message = "How should I handle auth?"))
        println("Session 1: ${followUp1.message}")

        // Session 2 conversation (knows nothing about session1)
        session2.sendAndWait(SendOptions(message = "I'm building an Android app"))
        val followUp2 = session2.sendAndWait(SendOptions(message = "How should I handle auth?"))
        println("Session 2: ${followUp2.message}")
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Parallel Requests with Coroutines

Run multiple sessions concurrently using `async`/`await`.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import kotlinx.coroutines.*

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val s1 = client.createSession(SessionConfig())
        val s2 = client.createSession(SessionConfig())
        val s3 = client.createSession(SessionConfig())

        // Fire all requests concurrently
        coroutineScope {
            val d1 = async { s1.sendAndWait(SendOptions(message = "Explain coroutines")) }
            val d2 = async { s2.sendAndWait(SendOptions(message = "Explain sealed classes")) }
            val d3 = async { s3.sendAndWait(SendOptions(message = "Explain data classes")) }

            println("Coroutines: ${d1.await().message}")
            println("Sealed: ${d2.await().message}")
            println("Data: ${d3.await().message}")
        }
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Session Lifecycle

Sessions remain active until the client is stopped. Clean up with `client.stop()`.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        // Short-lived session for a one-off task
        val temp = client.createSession(SessionConfig())
        val result = temp.sendAndWait(SendOptions(message = "Format this JSON"))
        println(result.message)

        // Long-lived session for interactive use
        val mainSession = client.createSession(SessionConfig())
        mainSession.sendAndWait(SendOptions(message = "Let's build a REST API"))
        mainSession.sendAndWait(SendOptions(message = "Add a /users endpoint"))
        mainSession.sendAndWait(SendOptions(message = "Add pagination support"))
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop() // All sessions are cleaned up
    }
}
```

## Best Practices

- Use separate sessions for tasks that should not share conversational context.
- Use `async`/`await` from `kotlinx.coroutines` to run independent sessions concurrently.
- Keep session references accessible so you can send follow-up messages to the right context.
- Call `client.stop()` once when done; this cleans up all active sessions.
- Use `coroutineScope` to propagate exceptions from child coroutines properly.
