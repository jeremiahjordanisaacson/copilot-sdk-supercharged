# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Kotlin.

## Basic Error Handling

Use try/catch with coroutines for clean error handling.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()
        val session = client.createSession(SessionConfig())
        val response = session.sendAndWait(SendOptions(message = "Explain Kotlin coroutines"))
        println(response.message)
    } catch (e: Exception) {
        System.err.println("SDK error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Handling Specific Error Types

Catch more specific exceptions before the generic fallback.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import com.github.copilot.sdk.exception.ConnectionException
import com.github.copilot.sdk.exception.TimeoutException

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()
        val session = client.createSession(SessionConfig())
        val response = session.sendAndWait(SendOptions(message = "Hello"))
        println(response.message)
    } catch (e: ConnectionException) {
        System.err.println("Connection failed. Is the Copilot CLI running? ${e.message}")
    } catch (e: TimeoutException) {
        System.err.println("Request timed out: ${e.message}")
    } catch (e: Exception) {
        System.err.println("Unexpected error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Using runCatching

Use Kotlin's `runCatching` for a functional error-handling style.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()
        val session = client.createSession(SessionConfig())

        val result = runCatching {
            session.sendAndWait(SendOptions(message = "Explain sealed classes"))
        }

        result
            .onSuccess { response -> println(response.message) }
            .onFailure { error -> System.err.println("Send failed: ${error.message}") }
    } finally {
        client.stop()
    }
}
```

## Timeout Handling

Use `withTimeout` from Kotlin coroutines to cap request duration.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import kotlinx.coroutines.withTimeout
import kotlinx.coroutines.TimeoutCancellationException

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()
        val session = client.createSession(SessionConfig())

        val response = withTimeout(30_000) { // 30-second timeout
            session.sendAndWait(SendOptions(message = "Summarize this codebase"))
        }
        println(response.message)
    } catch (e: TimeoutCancellationException) {
        System.err.println("Request timed out after 30 seconds")
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Aborting a Request

Use coroutine cancellation to abort in-flight requests.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import kotlinx.coroutines.*

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()
        val session = client.createSession(SessionConfig())

        val job = CoroutineScope(Dispatchers.IO).launch {
            val response = session.sendAndWait(SendOptions(message = "Write a long essay"))
            println(response.message)
        }

        // Cancel after 5 seconds
        delay(5000)
        job.cancelAndJoin()
        println("Request was cancelled.")
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Graceful Shutdown

Add a JVM shutdown hook to guarantee the client stops on exit.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val client = CopilotClient()

    Runtime.getRuntime().addShutdownHook(Thread {
        println("Shutting down...")
        runBlocking { client.stop() }
    })

    try {
        client.start()
        val session = client.createSession(SessionConfig())
        val response = session.sendAndWait(SendOptions(message = "Hello from Kotlin"))
        println(response.message)
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    }
}
```

## Best Practices

- Always call `client.stop()` in a `finally` block or shutdown hook.
- Catch specific exception types before catching the generic `Exception`.
- Use `withTimeout` for all production requests to prevent indefinite blocking.
- Use `runCatching` for functional-style error handling in non-critical paths.
- Use structured concurrency (`coroutineScope`, `supervisorScope`) for proper cancellation.
- Log errors with enough context (session ID, prompt snippet) for easier debugging.
