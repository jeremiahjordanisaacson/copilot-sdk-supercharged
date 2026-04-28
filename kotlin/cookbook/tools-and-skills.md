# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in Kotlin.

## Defining a Simple Tool

Use `defineTool` to expose a function that the model can call.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import com.github.copilot.sdk.tools.defineTool

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        // Define a tool the model can call
        val weatherTool = defineTool("get_weather", "Get the current weather for a city") { args ->
            val city = args.getString("city")
            // In production, call a real weather API
            """{"city": "$city", "temp": "72F", "condition": "sunny"}"""
        }

        val config = SessionConfig(tools = listOf(weatherTool))
        val session = client.createSession(config)

        val response = session.sendAndWait(
            SendOptions(message = "What is the weather in Seattle?")
        )
        println(response.message)
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Tool with Parameters

Define tools with typed parameters.

```kotlin
import com.github.copilot.sdk.tools.defineTool
import com.github.copilot.sdk.tools.ToolParameter

val searchTool = defineTool(
    name = "search_docs",
    description = "Search the documentation for a topic",
    parameters = listOf(
        ToolParameter("query", "string", "The search query", required = true),
        ToolParameter("limit", "integer", "Max results to return", required = false),
    )
) { args ->
    val query = args.getString("query")
    val limit = args.getIntOrDefault("limit", 10)
    """[{"title": "Result for '$query'", "count": $limit}]"""
}
```

## Multiple Tools

Register multiple tools on a single session.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import com.github.copilot.sdk.tools.defineTool
import java.io.File

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val readFile = defineTool("read_file", "Read a file from disk") { args ->
            File(args.getString("path")).readText()
        }

        val listFiles = defineTool("list_files", "List files in a directory") { args ->
            File(args.getString("directory"))
                .listFiles()
                ?.joinToString("\n") { it.name }
                ?: "Directory not found"
        }

        val config = SessionConfig(tools = listOf(readFile, listFiles))
        val session = client.createSession(config)

        val response = session.sendAndWait(
            SendOptions(message = "List the files in /src and read Main.kt")
        )
        println(response.message)
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Skills Configuration

Pass skill definitions in the session config to enable specialized behaviors.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import com.github.copilot.sdk.skills.Skill

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val codeReviewSkill = Skill("code-review", "Review code for bugs and improvements")

        val config = SessionConfig(skills = listOf(codeReviewSkill))
        val session = client.createSession(config)

        val response = session.sendAndWait(
            SendOptions(message = "Review this pull request for security issues")
        )
        println(response.message)
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Listening for Sub-Agent Events

When the model spawns sub-agents, listen for their lifecycle events.

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig
import com.github.copilot.sdk.SendOptions
import com.github.copilot.sdk.tools.defineTool

suspend fun main() {
    val client = CopilotClient()

    try {
        client.start()

        val analyzer = defineTool("analyze_code", "Analyze code quality") { _ ->
            """{"issues": 3, "severity": "medium"}"""
        }

        val config = SessionConfig(tools = listOf(analyzer))
        val session = client.createSession(config)

        // Listen for sub-agent events
        session.onEvent("assistant.tool_call") { event ->
            println("Tool called: ${event.toolName}")
        }
        session.onEvent("assistant.tool_result") { _ ->
            println("Tool result received")
        }

        val response = session.sendAndWait(
            SendOptions(message = "Analyze the code in /src for quality issues")
        )
        println(response.message)
    } catch (e: Exception) {
        System.err.println("Error: ${e.message}")
    } finally {
        client.stop()
    }
}
```

## Best Practices

- Give tools clear, descriptive names and descriptions so the model knows when to use them.
- Use Kotlin's trailing lambda syntax for concise tool definitions.
- Mark parameters as required or optional with sensible defaults.
- Return JSON strings from tool handlers for easier model interpretation.
- Keep tool handlers fast; the model waits for the result before continuing.
- Use skills to specialize session behavior without adding custom tools.
- Listen for tool call events to log and monitor tool usage in production.
