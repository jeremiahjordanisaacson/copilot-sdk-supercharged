# Error Handling

Patterns for handling errors when using the GitHub Copilot SDK in Kotlin.

## Basic Error Handling

`start()`, `createSession()`, and `sendAndWait()` are `suspend` functions that throw on failure.
`stop()` returns a list of cleanup errors instead of throwing, so it's safe to call in `finally`.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        // Returns the last assistant.message event of the turn, or null if there was none
        val reply = session.sendAndWait(MessageOptions(prompt = "Explain Kotlin coroutines"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } catch (e: Exception) {
        System.err.println("SDK error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Handling Specific Error Types

| Failure | What the SDK throws |
| --- | --- |
| `copilot` isn't on `PATH` | `IOException` from `start()` |
| `COPILOT_CLI_PATH` or `cliPath` is an absolute path that doesn't exist | `RuntimeException` ("Copilot CLI not found at ...") from `start()` |
| The server at `cliUrl` isn't reachable | `java.net.ConnectException` (an `IOException`) |
| The CLI rejects a request | `com.github.copilot.JsonRpcException`, which carries the JSON-RPC error `code` |
| The CLI doesn't answer a request within 30 seconds, or `sendAndWait` times out | `kotlinx.coroutines.TimeoutCancellationException` |
| The session reports `session.error` during `sendAndWait` | `RuntimeException` |

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.JsonRpcException
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import java.io.IOException
import java.net.ConnectException
import kotlinx.coroutines.TimeoutCancellationException
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val reply = session.sendAndWait(MessageOptions(prompt = "Hello"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } catch (e: ConnectException) {
        System.err.println("Could not reach the CLI server: ${e.message}")
    } catch (e: IOException) {
        System.err.println("Could not start the Copilot CLI. Is it on PATH or in COPILOT_CLI_PATH? ${e.message}")
    } catch (e: JsonRpcException) {
        System.err.println("The CLI rejected the request (code ${e.code}): ${e.message}")
    } catch (e: TimeoutCancellationException) {
        System.err.println("Timed out: ${e.message}")
    } catch (e: RuntimeException) {
        System.err.println("The turn failed: ${e.message}")
    } finally {
        client.stop()
    }
}
```

Catch `TimeoutCancellationException` before `RuntimeException`, because it's a subclass.

## Timeout Handling

`sendAndWait` waits 60 seconds for the session to go idle. Pass `timeoutMs` to change that. When the
wait times out, the turn can still be running in the CLI, so abort it.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.TimeoutCancellationException
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        try {
            val reply = session.sendAndWait(MessageOptions(prompt = "Summarize this repository"), timeoutMs = 30_000)
            println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
        } catch (e: TimeoutCancellationException) {
            System.err.println("No reply within 30 seconds")
            session.abort() // stop the turn if it is still running
        }
    } finally {
        client.stop()
    }
}
```

## Aborting a Request

`send()` returns as soon as the message is queued. Call `abort()` to stop the turn; the session then
goes idle and accepts new messages. Cancelling a coroutine that's waiting in `sendAndWait` only stops
the wait, not the turn, so call `abort()` either way.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val idle = CompletableDeferred<Unit>()
        session.on("abort") { event ->
            println("Aborted: ${event.data?.get("reason")?.jsonPrimitive?.contentOrNull}")
        }
        session.on("session.idle") { idle.complete(Unit) }

        session.send(MessageOptions(prompt = "Write a long essay about the history of Kotlin"))

        delay(5_000)
        session.abort()

        withTimeout(30_000) { idle.await() }
    } finally {
        client.stop()
    }
}
```

## Graceful Shutdown

Stop the client from a shutdown hook so the CLI process it started is cleaned up, even when the
program is interrupted.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val client = CopilotClient()

    // Runs on normal exit, System.exit, and Ctrl+C
    Runtime.getRuntime().addShutdownHook(Thread {
        println("Shutting down...")
        runBlocking { client.stop() }
    })

    client.start()
    val session = client.createSession(SessionConfig(
        onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
    ))

    val reply = session.sendAndWait(MessageOptions(prompt = "Hello from Kotlin"))
    println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
}
```

## Best Practices

- Call `client.stop()` in a `finally` block or a shutdown hook. It detaches every session the client created and stops the CLI process.
- Check `sendAndWait` results for `null`; a turn can finish without an assistant message.
- Catch specific exception types before the generic `Exception`, and `TimeoutCancellationException` before `RuntimeException`.
- Pass `timeoutMs` to `sendAndWait` for long tasks, and call `abort()` when you stop waiting.
- Don't wrap SDK calls in `runCatching` inside coroutines: it also catches `CancellationException` and breaks cancellation.
- Log the session ID (`session.sessionId`) with errors so you can find the session later.
