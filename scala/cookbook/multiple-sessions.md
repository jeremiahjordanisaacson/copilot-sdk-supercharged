# Multiple Sessions in Scala

Patterns for managing multiple independent conversations with the GitHub Copilot SDK in Scala.

## Scenario

Your application needs to run several conversations at the same time, each with its own context and history, sharing a single client connection.

## Creating Multiple Sessions

```scala
import com.github.copilot._
import scala.concurrent.Await
import scala.concurrent.duration._

object MultipleSessions extends App {
  val client = CopilotClient(CopilotOptions())
  Await.result(client.start(), 10.seconds)

  try {
    // Create separate sessions for different tasks
    val codeConfig = SessionConfig(systemPrompt = Some("You are a Scala expert."))
    val codeSession = Await.result(client.createSession(codeConfig), 10.seconds)

    val docsConfig = SessionConfig(systemPrompt = Some("You are a technical writer."))
    val docsSession = Await.result(client.createSession(docsConfig), 10.seconds)

    // Each session maintains its own conversation history
    val codeReply = Await.result(
      codeSession.sendAndWait(SendOptions(message = "Explain implicits")),
      30.seconds
    )
    println(s"Code: ${codeReply.message}")

    val docsReply = Await.result(
      docsSession.sendAndWait(SendOptions(message = "Write a scaladoc template")),
      30.seconds
    )
    println(s"Docs: ${docsReply.message}")
  } finally {
    Await.result(client.stop(), 5.seconds)
  }
}
```

## Session Manager with Mutable Map

```scala
import com.github.copilot._
import scala.collection.mutable
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._

class SessionManager(client: CopilotClient)(implicit ec: ExecutionContext) {
  private val sessions = mutable.Map.empty[String, CopilotSession]

  def getOrCreate(name: String, systemPrompt: String = ""): Future[CopilotSession] = {
    sessions.get(name) match {
      case Some(session) => Future.successful(session)
      case None =>
        val config = SessionConfig(
          systemPrompt = if (systemPrompt.nonEmpty) Some(systemPrompt) else None
        )
        client.createSession(config).map { session =>
          sessions(name) = session
          session
        }
    }
  }

  def remove(name: String): Unit = sessions.remove(name)

  def count: Int = sessions.size

  def names: Iterable[String] = sessions.keys
}

// Usage
implicit val ec: ExecutionContext = ExecutionContext.global

val client = CopilotClient(CopilotOptions())
Await.result(client.start(), 10.seconds)

val manager = new SessionManager(client)

val frontend = Await.result(
  manager.getOrCreate("frontend", "You are a Scala.js expert."),
  10.seconds
)
val backend = Await.result(
  manager.getOrCreate("backend", "You are an Akka expert."),
  10.seconds
)
```

## Parallel Sessions with Future.sequence

```scala
import com.github.copilot._
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._

implicit val ec: ExecutionContext = ExecutionContext.global

case class Task(name: String, prompt: String, systemPrompt: String)

def parallelSessions(client: CopilotClient, tasks: Seq[Task]): Future[Seq[(String, String)]] = {
  val futures = tasks.map { task =>
    val config = SessionConfig(systemPrompt = Some(task.systemPrompt))
    for {
      session  <- client.createSession(config)
      response <- session.sendAndWait(SendOptions(message = task.prompt))
    } yield (task.name, response.message)
  }

  Future.sequence(futures)
}

// Usage
val tasks = Seq(
  Task("review", "Review this code", "You review Scala code."),
  Task("docs", "Generate API docs", "You write documentation."),
  Task("tests", "Write ScalaTest specs", "You write Scala tests."),
)

val results = Await.result(parallelSessions(client, tasks), 60.seconds)
results.foreach { case (name, result) =>
  println(s"[$name] $result")
}
```

## Best Practices

- **Reuse a single client**: Create one `CopilotClient` and share it across all sessions.
- **Use a manager class**: Encapsulate session creation and lookup with a mutable map.
- **Use `Future.sequence` for parallelism**: Run independent sessions concurrently for better throughput.
- **Use for-comprehensions**: Chain session creation and message sending with flatMap via for/yield.
- **Set distinct system prompts**: Give each session a focused persona for better results.
- **Clean up finished sessions**: Call `remove` on the manager to free sessions that are no longer needed.
