# Tools and Skills in Scala

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in Scala.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```scala
import com.github.copilot._
import com.github.copilot.tools._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

val timeTool = DefineTool("get_current_time", "Returns the current date and time") {
  ctx =>
    val now = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    ToolResult(content = now)
}
```

## Defining a Tool with Parameters

```scala
import com.github.copilot.tools._
import scala.io.Source
import scala.util.{Try, Using}

val readFileTool = DefineTool("read_file", "Reads the contents of a file given its path") {
  ctx =>
    val path = ctx.getParam[String]("path").getOrElse("")
    if (path.isEmpty) {
      ToolResult(error = Some("Missing required parameter: path"))
    } else {
      Using(Source.fromFile(path)) { source =>
        ToolResult(content = source.mkString)
      }.getOrElse {
        ToolResult(error = Some(s"Could not read file: $path"))
      }
    }
}
```

## Registering Tools with a Client

```scala
import com.github.copilot._
import scala.concurrent.Await
import scala.concurrent.duration._

object ToolExample extends App {
  val opts = CopilotOptions(
    tools = Seq(timeTool, readFileTool)
  )
  val client = CopilotClient(opts)
  Await.result(client.start(), 10.seconds)

  try {
    val config = SessionConfig(
      systemPrompt = Some("You have access to file reading and time tools.")
    )
    val session = Await.result(client.createSession(config), 10.seconds)

    val response = Await.result(
      session.sendAndWait(SendOptions(message = "What time is it?")),
      30.seconds
    )
    println(response.message)
  } finally {
    Await.result(client.stop(), 5.seconds)
  }
}
```

## Defining Multiple Related Tools (Skill Pattern)

```scala
import com.github.copilot.tools._
import scala.collection.mutable

class KVStoreSkill {
  private val store = mutable.Map.empty[String, String]

  val tools: Seq[ToolDefinition] = Seq(
    DefineTool("kv_get", "Get a value by key") { ctx =>
      val key = ctx.getParam[String]("key").getOrElse("")
      store.get(key) match {
        case Some(value) => ToolResult(content = value)
        case None        => ToolResult(content = s"Key not found: $key")
      }
    },

    DefineTool("kv_set", "Set a key-value pair") { ctx =>
      val key = ctx.getParam[String]("key").getOrElse("")
      val value = ctx.getParam[String]("value").getOrElse("")
      store(key) = value
      ToolResult(content = s"Stored: $key")
    },

    DefineTool("kv_list", "List all keys") { _ =>
      if (store.isEmpty) ToolResult(content = "Store is empty")
      else ToolResult(content = store.keys.mkString("\n"))
    }
  )
}

// Usage
val kvSkill = new KVStoreSkill()

val opts = CopilotOptions(tools = kvSkill.tools)
val client = CopilotClient(opts)
```

## Sub-Agent Orchestration

```scala
import com.github.copilot._
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._

def orchestrate(client: CopilotClient)(implicit ec: ExecutionContext): Unit = {
  // Create specialized sessions as sub-agents
  val plannerF = client.createSession(
    SessionConfig(systemPrompt = Some("You are a planner. Break tasks into steps."))
  )
  val coderF = client.createSession(
    SessionConfig(systemPrompt = Some("You are a Scala developer."))
  )
  val reviewerF = client.createSession(
    SessionConfig(systemPrompt = Some("You review Scala code for bugs."))
  )

  val pipeline = for {
    planner  <- plannerF
    coder    <- coderF
    reviewer <- reviewerF

    // Step 1: Plan
    plan <- planner.sendAndWait(
      SendOptions(message = "Plan a concurrent queue in Scala")
    )
    _ = println(s"Plan: ${plan.message}")

    // Step 2: Code
    code <- coder.sendAndWait(
      SendOptions(message = s"Implement this plan:\n${plan.message}")
    )
    _ = println(s"Code: ${code.message}")

    // Step 3: Review
    review <- reviewer.sendAndWait(
      SendOptions(message = s"Review this code:\n${code.message}")
    )
    _ = println(s"Review: ${review.message}")
  } yield review

  Await.result(pipeline, 120.seconds)
}
```

## Functional Tool Composition

```scala
import com.github.copilot.tools._

// Compose tools by combining their outputs
def composeTool(
  name: String,
  description: String,
  tools: Seq[ToolDefinition]
): ToolDefinition = {
  DefineTool(name, description) { ctx =>
    val results = tools.map { tool =>
      s"[${tool.name}]: ${tool.handler(ctx).content}"
    }
    ToolResult(content = results.mkString("\n"))
  }
}
```

## Best Practices

- **Use curried DefineTool syntax**: `DefineTool("name", "desc")(handler)` is idiomatic Scala.
- **Return `Option` from getParam**: Use `getOrElse` for default values and clean error handling.
- **Group related tools in a class**: Use a class to encapsulate shared mutable state across tools.
- **Use for-comprehensions for pipelines**: Chain Future-based agent calls with for/yield.
- **Use separate sessions for sub-agents**: Each agent gets its own system prompt and conversation context.
- **Leverage pattern matching**: Use `match` on tool results for concise, safe branching.
