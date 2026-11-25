# Multiple Sessions

Manage multiple independent conversations at the same time with the GitHub Copilot SDK in Kotlin.

## Creating Multiple Sessions

One client can host many sessions. Each session keeps its own conversation history.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.SessionEvent
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

private fun SessionEvent?.text(): String =
    this?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)"

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val config = SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        )

        val codeReview = client.createSession(config)
        val documentation = client.createSession(config)

        val review = codeReview.sendAndWait(
            MessageOptions(prompt = "Review this function for bugs: fun add(a: Int, b: Int) = a - b")
        )
        println("Review: ${review.text()}")

        val docs = documentation.sendAndWait(
            MessageOptions(prompt = "Write a KDoc comment for a function that adds two Ints")
        )
        println("Docs: ${docs.text()}")
    } finally {
        client.stop()
    }
}
```

## Independent Conversations

Sessions don't share context. A follow-up in one session has no effect on the other.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.SessionEvent
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

private fun SessionEvent?.text(): String =
    this?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)"

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val config = SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        )

        val server = client.createSession(config)
        val android = client.createSession(config)

        server.sendAndWait(MessageOptions(prompt = "I'm working on a Ktor server"))
        val serverAnswer = server.sendAndWait(MessageOptions(prompt = "How should I handle auth?"))
        println("Ktor session: ${serverAnswer.text()}")

        // This session knows nothing about the Ktor conversation
        android.sendAndWait(MessageOptions(prompt = "I'm building an Android app"))
        val androidAnswer = android.sendAndWait(MessageOptions(prompt = "How should I handle auth?"))
        println("Android session: ${androidAnswer.text()}")
    } finally {
        client.stop()
    }
}
```

## Parallel Requests with Coroutines

`sendAndWait` suspends instead of blocking, so `async` runs several sessions concurrently.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.SessionEvent
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

private fun SessionEvent?.text(): String =
    this?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)"

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val config = SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        )

        val questions = listOf(
            "Explain coroutines in two sentences",
            "Explain sealed classes in two sentences",
            "Explain data classes in two sentences",
        )
        val sessions = questions.map { client.createSession(config) }

        // coroutineScope waits for every request and fails fast if one of them throws
        val answers = coroutineScope {
            sessions.zip(questions)
                .map { (session, question) ->
                    async { session.sendAndWait(MessageOptions(prompt = question), timeoutMs = 120_000).text() }
                }
                .awaitAll()
        }

        questions.zip(answers).forEach { (question, answer) -> println("$question\n$answer\n") }
    } finally {
        client.stop()
    }
}
```

## Session Lifecycle

`destroy()` releases a session you no longer need and keeps its saved state, so you can resume it
later. `deleteSession()` removes the saved state for good. `client.stop()` destroys every session the
client still holds.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.SessionEvent
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

private fun SessionEvent?.text(): String =
    this?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)"

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val config = SessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        )

        // A short-lived session for a one-off task
        val scratch = client.createSession(config)
        println(scratch.sendAndWait(MessageOptions(prompt = """Format this JSON: {"a":1,"b":[1,2]}""")).text())
        scratch.destroy()
        client.deleteSession(scratch.sessionId)

        // A long-lived session for a multi-step task
        val project = client.createSession(config)
        project.sendAndWait(MessageOptions(prompt = "Let's design a REST API for a todo app"))
        project.sendAndWait(MessageOptions(prompt = "Add a /users endpoint"))
        println(project.sendAndWait(MessageOptions(prompt = "Add pagination to both endpoints")).text())
    } finally {
        client.stop()
    }
}
```

## Best Practices

- Use a separate session for each task that shouldn't share conversational context.
- Reuse one `CopilotClient` for all sessions; each client starts its own CLI process.
- Run independent sessions concurrently with `async` inside `coroutineScope`, one session per task.
- Keep session references (or IDs) so follow-up messages go to the right conversation.
- Call `destroy()` on sessions you're done with, and `client.stop()` once at the end.
