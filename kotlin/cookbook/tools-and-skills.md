# Tools and Skills

Define custom tools, load skills, add custom agents, and watch tool and sub-agent activity with the
GitHub Copilot SDK in Kotlin.

## Defining a Simple Tool

Build a tool with the `defineTool` DSL: give it a description, a JSON Schema for its parameters, and
a handler. The handler receives the arguments as a `JsonElement` (a `JsonObject` for object schemas)
and returns the result for the model: a `String` or a `ToolResultObject`. Other values are sent as
their `toString()`, so build structured results as JSON text.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.defineTool
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive
import kotlinx.serialization.json.put

fun main() = runBlocking {
    val weatherTool = defineTool("get_weather") {
        description = "Get the current weather for a city"
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf(
                "city" to mapOf("type" to "string", "description" to "City name"),
            ),
            "required" to listOf("city"),
        )
        handler = { args, _ ->
            val city = (args as? JsonObject)?.get("city")?.jsonPrimitive?.contentOrNull ?: "unknown"
            // Call a real weather API here
            buildJsonObject {
                put("city", city)
                put("temperatureF", 72)
                put("condition", "sunny")
            }.toString()
        }
    }

    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            tools = listOf(weatherTool),
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val reply = session.sendAndWait(MessageOptions(prompt = "What is the weather in Seattle?"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } finally {
        client.stop()
    }
}
```

## Required and Optional Parameters

List required parameters in the schema's `required` array and give optional ones a default in the
handler.

```kotlin
import com.github.copilot.Tool
import com.github.copilot.defineTool
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.intOrNull
import kotlinx.serialization.json.jsonPrimitive

val searchDocsTool: Tool = defineTool("search_docs") {
    description = "Search the documentation for a topic"
    parameters = mapOf(
        "type" to "object",
        "properties" to mapOf(
            "query" to mapOf("type" to "string", "description" to "The search query"),
            "limit" to mapOf("type" to "integer", "description" to "Maximum number of results (default 10)"),
        ),
        "required" to listOf("query"),
    )
    idempotent = true // repeating a call with the same arguments has no extra effect
    handler = { args, _ ->
        val input = args as? JsonObject
        val query = input?.get("query")?.jsonPrimitive?.contentOrNull ?: ""
        val limit = input?.get("limit")?.jsonPrimitive?.intOrNull ?: 10
        "Top $limit results for '$query': ..."
    }
}
```

## Reporting Tool Failures

If a handler throws, the call fails. The runtime logs the exception message but the model only sees
a generic failure, so when the model should know why (to fix its arguments, for example), return a
`ToolResultObject` with `resultType = "failure"`. This sample uses `createTool`, the non-DSL form.

```kotlin
import com.github.copilot.Tool
import com.github.copilot.ToolResultObject
import com.github.copilot.createTool
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

val issues = mapOf("42" to "Login fails when the password contains a space")

val lookupIssueTool: Tool = createTool(
    name = "lookup_issue",
    description = "Fetch issue details from the tracker",
    parameters = mapOf(
        "type" to "object",
        "properties" to mapOf("id" to mapOf("type" to "string", "description" to "Issue ID")),
        "required" to listOf("id"),
    ),
) { args, _ ->
    val id = (args as? JsonObject)?.get("id")?.jsonPrimitive?.contentOrNull ?: ""
    issues[id] ?: ToolResultObject(
        textResultForLlm = "Issue $id was not found. Check the ID and try again.",
        resultType = "failure",
    )
}
```

## Multiple Tools

Register several tools on one session. Avoid the names of the CLI's built-in tools (such as `view`,
`edit`, or `grep`).

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.defineTool
import java.io.File
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

private fun stringArg(args: Any?, name: String): String =
    (args as? JsonObject)?.get(name)?.jsonPrimitive?.contentOrNull
        ?: throw IllegalArgumentException("Missing argument: $name")

fun main() = runBlocking {
    val readFile = defineTool("read_project_file") {
        description = "Read a UTF-8 text file from the project"
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf("path" to mapOf("type" to "string", "description" to "File path")),
            "required" to listOf("path"),
        )
        handler = { args, _ -> File(stringArg(args, "path")).readText() }
    }

    val listFiles = defineTool("list_project_files") {
        description = "List the entries in a project directory"
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf("directory" to mapOf("type" to "string", "description" to "Directory path")),
            "required" to listOf("directory"),
        )
        handler = { args, _ ->
            File(stringArg(args, "directory")).listFiles()
                ?.map { it.name }
                ?.sorted()
                ?.joinToString("\n")
                ?: "Directory not found"
        }
    }

    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            tools = listOf(readFile, listFiles),
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val reply = session.sendAndWait(
            MessageOptions(prompt = "List the files in ./src, then read the first one and summarize it")
        )
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } finally {
        client.stop()
    }
}
```

## Skills

A skill is a directory with a `SKILL.md` file: YAML front matter (`name`, `description`) followed by
instructions. Point the session at the parent directory, and turn off individual skills by name.

```text
skills/
  code-review/
    SKILL.md
  release-notes/
    SKILL.md
```

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
            skillDirectories = listOf("./skills"),
            disabledSkills = listOf("release-notes"),
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val reply = session.sendAndWait(MessageOptions(prompt = "Use the code-review skill to review src/Main.kt"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } finally {
        client.stop()
    }
}
```

## Custom Agents

Custom agents are sub-agents with their own prompt and tool list. The main agent can delegate work
to them. `tools` limits the agent to the listed tools (`null` allows all of them), and `skills`
preloads skills from the session's skill directories into the agent's context.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.CustomAgentConfig
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

fun main() = runBlocking {
    val reviewer = CustomAgentConfig(
        name = "security-reviewer",
        displayName = "Security Reviewer",
        description = "Reviews code for security problems",
        prompt = "You review code for injection, authentication, and secret-handling problems. " +
            "Report each finding with its file and line.",
        tools = listOf("view", "grep", "glob"), // read-only tools
        skills = listOf("code-review"),
    )

    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            skillDirectories = listOf("./skills"),
            customAgents = listOf(reviewer),
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        val reply = session.sendAndWait(
            MessageOptions(prompt = "Ask the security reviewer to check the src folder"),
            timeoutMs = 300_000,
        )
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } finally {
        client.stop()
    }
}
```

## Watching Tool and Sub-Agent Events

Tool calls emit `tool.execution_start` and `tool.execution_complete`. Sub-agents emit
`subagent.started`, then `subagent.completed` or `subagent.failed`.

```kotlin
import com.github.copilot.CopilotClient
import com.github.copilot.MessageOptions
import com.github.copilot.PermissionRequestResult
import com.github.copilot.SessionConfig
import com.github.copilot.SessionEvent
import com.github.copilot.defineTool
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonPrimitive

private fun SessionEvent.field(name: String): String? = data?.get(name)?.jsonPrimitive?.contentOrNull

fun main() = runBlocking {
    val analyzer = defineTool("analyze_code_quality") {
        description = "Analyze code quality for a directory"
        parameters = mapOf(
            "type" to "object",
            "properties" to mapOf("directory" to mapOf("type" to "string")),
            "required" to listOf("directory"),
        )
        handler = { _, _ -> """{"issues": 3, "severity": "medium"}""" }
    }

    val client = CopilotClient()
    try {
        client.start()
        val session = client.createSession(SessionConfig(
            tools = listOf(analyzer),
            onPermissionRequest = { _, _ -> PermissionRequestResult(kind = "approve-once") }
        ))

        session.on("tool.execution_start") { println("Tool started: ${it.field("toolName")}") }
        session.on("tool.execution_complete") { println("Tool finished, success=${it.field("success")}") }
        session.on("subagent.started") { println("Sub-agent started: ${it.field("agentDisplayName")}") }
        session.on("subagent.completed") { println("Sub-agent completed: ${it.field("agentDisplayName")}") }
        session.on("subagent.failed") { println("Sub-agent failed: ${it.field("agentDisplayName")}: ${it.field("error")}") }

        val reply = session.sendAndWait(MessageOptions(prompt = "Analyze the code quality of ./src and summarize the result"))
        println(reply?.data?.get("content")?.jsonPrimitive?.contentOrNull ?: "(no reply)")
    } finally {
        client.stop()
    }
}
```

## Best Practices

- Give tools clear names and descriptions so the model knows when to use them.
- Describe every parameter in the JSON Schema and list the required ones.
- Keep handlers fast; the model waits for the result before it continues.
- Return a failure `ToolResultObject` when the model needs to know why a call failed.
- Use skills and custom agents to specialize behavior without writing more tools.
- Log `tool.execution_start` and `tool.execution_complete` events to monitor tool use.
