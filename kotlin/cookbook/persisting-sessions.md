# Persisting Sessions

Save and resume sessions across restarts with the GitHub Copilot SDK in Kotlin.

## Custom Session IDs

Give a session your own ID so you can find and resume it later. To continue a saved conversation,
use `resumeSession`: `createSession` with an ID that's already in use starts a new conversation
without the saved history.

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
            sessionId = "project-alpha-session-001",
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))
        println("Session ID: ${session.sessionId}")

        val reply = session.sendAndWait(MessageOptions(prompt = "Let's plan the auth module"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } finally {
        client.stop()
    }
}
```

## Resuming a Session

Sessions are saved as you go. `client.stop()` (or `session.destroy()`) detaches a session and keeps
its state on disk, and `resumeSession` picks it up again, even from another process. Handlers such as
`onPermissionRequest` and tools aren't saved with the session, so pass them again in
`ResumeSessionConfig`.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.ResumeSessionConfig
import com.github.copilot.SessionConfig
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

const val SESSION_ID = "persistent-session-42"

fun main() = runBlocking {
    // First run: start the conversation
    val first = CopilotClient()
    try {
        first.start()
        val session = first.createSession(SessionConfig(
            sessionId = SESSION_ID,
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))
        session.sendAndWait(MessageOptions(prompt = "We are building a payment gateway in Kotlin"))
    } finally {
        first.stop()
    }

    // Later run: resume the conversation by ID
    val later = CopilotClient()
    try {
        later.start()
        val session = later.resumeSession(SESSION_ID, ResumeSessionConfig(
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val reply = session.sendAndWait(MessageOptions(prompt = "What are we building?"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)") // mentions the payment gateway
    } finally {
        later.stop()
    }
}
```

## Listing and Deleting Saved Sessions

```kotlin
import com.github.copilot.CopilotClient
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        for (meta in client.listSessions()) {
            println("${meta.sessionId}  modified ${meta.modifiedTime}  ${meta.summary}")
        }

        // Removes a saved session and its state for good:
        // client.deleteSession("project-alpha-session-001")

        val lastId = client.getLastSessionId()
        println("Most recent session: ${lastId ?: "(none)"}")
    } finally {
        client.stop()
    }
}
```

## Infinite Sessions with Compaction

Infinite sessions are on by default: when the conversation nears the model's context limit, the
runtime compacts older history in the background and emits compaction events. Pass an
`InfiniteSessionConfig` only to change the thresholds or to turn the feature off
(`enabled = false`).

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.InfiniteSessionConfig
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.booleanOrNull
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            sessionId = "long-running-agent",
            infiniteSessions = InfiniteSessionConfig(
                enabled = true,
                backgroundCompactionThreshold = 0.80, // start compacting at 80% of the context window
                bufferExhaustionThreshold = 0.95,     // pause new work at 95% until compaction finishes
            ),
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        session.on("session.compaction_start") { println("Compacting conversation history...") }
        session.on("session.compaction_complete") { event ->
            println("Compaction finished, success=${event.data?.get("success")?.jsonPrimitive?.booleanOrNull}")
        }

        val tasks = listOf(
            "Analyze the auth module",
            "Review the database schema",
            "Check the API endpoints",
            "Suggest performance improvements",
            "Write unit tests for the service layer",
        )
        for (task in tasks) {
            val reply = session.sendAndWait(MessageOptions(prompt = task), timeoutMs = 300_000)
            println("Task: $task")
            println("Response: ${reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)"}")
        }
    } finally {
        client.stop()
    }
}
```

## Session State Location

With infinite sessions on, each session has a workspace directory for its checkpoints, plan, and
files. By default it's `~/.copilot/session-state/{sessionId}`. `workspacePath` holds the actual path,
or `null` when infinite sessions are off.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            sessionId = "my-app-session",
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        println("Session ID: ${session.sessionId}")
        println("Workspace: ${session.workspacePath}")
    } finally {
        client.stop()
    }
}
```

## Best Practices

- Use descriptive, deterministic session IDs (for example `"user-123-project-alpha"`) so you can resume them.
- Resume with `resumeSession`; `createSession` with an existing ID doesn't load the saved history.
- Register handlers and tools again on every resume.
- Listen for compaction events to tell users when history is being summarized.
- Delete sessions you no longer need with `deleteSession`.
