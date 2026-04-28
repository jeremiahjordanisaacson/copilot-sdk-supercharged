# Persisting Sessions in Scala

Patterns for saving and resuming Copilot sessions across application restarts in Scala.

## Scenario

Your application needs to save a conversation so users can close the app and return later to continue exactly where they left off.

## Saving and Loading Session IDs

```scala
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.{Try, Using}
import scala.io.Source

object SessionStore {
  private val sessionFile = "session_state.txt"

  def save(sessionId: String): Try[Unit] = Try {
    Files.writeString(Paths.get(sessionFile), sessionId)
  }

  def load(): Option[String] = {
    val path = Paths.get(sessionFile)
    if (!Files.exists(path)) return None

    Using(Source.fromFile(sessionFile)) { source =>
      val id = source.getLines().mkString.trim
      if (id.nonEmpty) Some(id) else None
    }.toOption.flatten
  }
}
```

## Resuming a Session

```scala
import com.github.copilot._
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

def getOrCreateSession(client: CopilotClient)
                       (implicit ec: ExecutionContext): CopilotSession = {
  SessionStore.load() match {
    case Some(savedId) =>
      Try {
        val config = SessionConfig(sessionId = Some(savedId))
        Await.result(client.createSession(config), 10.seconds)
      } match {
        case Success(session) =>
          println(s"Resumed session: $savedId")
          session
        case Failure(e) =>
          System.err.println(s"Could not resume session: ${e.getMessage}")
          createNewSession(client)
      }
    case None =>
      createNewSession(client)
  }
}

private def createNewSession(client: CopilotClient)
                              (implicit ec: ExecutionContext): CopilotSession = {
  val config = SessionConfig()
  val session = Await.result(client.createSession(config), 10.seconds)
  SessionStore.save(session.id)
  println(s"Created new session: ${session.id}")
  session
}
```

## Full Example: Persistent Chat

```scala
import com.github.copilot._
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.io.StdIn

object PersistentChat extends App {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val client = CopilotClient(CopilotOptions())
  Await.result(client.start(), 10.seconds)

  try {
    val session = getOrCreateSession(client)
    println("Chat (type 'quit' to exit):")

    var running = true
    while (running) {
      print("> ")
      val input = StdIn.readLine()

      if (input == null || input == "quit") {
        running = false
      } else {
        Try {
          val opts = SendOptions(message = input)
          Await.result(session.sendAndWait(opts), 30.seconds)
        } match {
          case Success(response) =>
            println(s"Assistant: ${response.message}")
          case Failure(e) =>
            System.err.println(s"Error: ${e.getMessage}")
        }
      }
    }

    println("Session saved. You can resume later.")
  } finally {
    Await.result(client.stop(), 5.seconds)
  }
}
```

## Storing Multiple Sessions with a Properties-like Format

```scala
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Using
import scala.io.Source

object MultiSessionStore {
  private val sessionsFile = "sessions.properties"

  def loadAll(): Map[String, String] = {
    val path = Paths.get(sessionsFile)
    if (!Files.exists(path)) return Map.empty

    Using(Source.fromFile(sessionsFile)) { source =>
      source.getLines()
        .filter(_.contains("="))
        .map { line =>
          val parts = line.split("=", 2)
          parts(0).trim -> parts(1).trim
        }
        .toMap
    }.getOrElse(Map.empty)
  }

  def saveAll(sessions: Map[String, String]): Unit = {
    val content = sessions.map { case (k, v) => s"$k=$v" }.mkString("\n")
    Files.writeString(Paths.get(sessionsFile), content)
  }

  def saveOne(name: String, sessionId: String): Unit = {
    val all = loadAll() + (name -> sessionId)
    saveAll(all)
  }
}
```

## Best Practices

- **Persist only the session ID**: The SDK and CLI handle conversation state internally. You just need the ID.
- **Try to resume before creating**: Attempt to restore a saved session first, then fall back to a new one.
- **Handle stale sessions with Try**: Use `Try` and pattern matching for clean fallback to a new session.
- **Use `scala.util.Using` for file I/O**: Ensures file handles are closed properly.
- **Store session IDs per name or user**: Use a properties-like format or JSON for multiple sessions.
- **Save on creation, not on exit**: Write the session ID immediately after creating it to avoid data loss on crashes.
