# Error Handling in Scala

Patterns for handling errors when using the GitHub Copilot SDK in Scala.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors using Scala's functional error handling with Try, Future, and pattern matching.

## Basic Try/Catch with Pattern Matching

```scala
import com.github.copilot._
import scala.util.{Try, Success, Failure}

object BasicErrorHandling extends App {
  val opts = CopilotOptions()

  Try {
    val client = CopilotClient(opts)
    try {
      import scala.concurrent.Await
      import scala.concurrent.duration._

      Await.result(client.start(), 10.seconds)

      val config = SessionConfig()
      val session = Await.result(client.createSession(config), 10.seconds)

      val sendOpts = SendOptions(message = "What is pattern matching in Scala?")
      val response = Await.result(session.sendAndWait(sendOpts), 30.seconds)

      println(s"Response: ${response.message}")
    } finally {
      import scala.concurrent.Await
      import scala.concurrent.duration._
      Await.result(client.stop(), 5.seconds)
    }
  } match {
    case Success(_) => println("Completed successfully")
    case Failure(e: ConnectionException) =>
      System.err.println(s"Connection failed: ${e.getMessage}")
    case Failure(e: TimeoutException) =>
      System.err.println(s"Timed out: ${e.getMessage}")
    case Failure(e) =>
      System.err.println(s"Unexpected error: ${e.getMessage}")
  }
}
```

## Functional Error Handling with Future

```scala
import com.github.copilot._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._

implicit val ec: ExecutionContext = ExecutionContext.global

def safeAsk(client: CopilotClient, message: String): Future[String] = {
  val config = SessionConfig()

  client.createSession(config).flatMap { session =>
    val opts = SendOptions(message = message)
    session.sendAndWait(opts).map(_.message)
  }.recover {
    case e: ConnectionException =>
      s"Connection error: ${e.getMessage}"
    case e: TimeoutException =>
      s"Timeout: ${e.getMessage}"
    case e: Exception =>
      s"Unexpected error: ${e.getMessage}"
  }
}
```

## Retry with Exponential Backoff

```scala
import com.github.copilot._
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

def connectWithRetry(opts: CopilotOptions, maxRetries: Int = 3)
                    (implicit ec: ExecutionContext): Future[CopilotClient] = {

  def attempt(n: Int): Future[CopilotClient] = {
    val client = CopilotClient(opts)
    client.start().map(_ => client).recoverWith {
      case e if n < maxRetries =>
        System.err.println(s"Attempt $n failed: ${e.getMessage}")
        val delay = n * 2
        System.err.println(s"Retrying in ${delay}s...")
        Future {
          Thread.sleep(delay * 1000)
        }.flatMap(_ => attempt(n + 1))
      case e =>
        System.err.println(s"All $maxRetries attempts failed")
        Future.failed(e)
    }
  }

  attempt(1)
}
```

## Using Either for Explicit Error Types

```scala
import com.github.copilot._
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

sealed trait CopilotError
case class ConnectionError(message: String) extends CopilotError
case class TimeoutError(message: String) extends CopilotError
case class UnknownError(message: String, cause: Throwable) extends CopilotError

def askSafely(client: CopilotClient, message: String): Either[CopilotError, String] = {
  Try {
    val config = SessionConfig()
    val session = Await.result(client.createSession(config), 10.seconds)
    val opts = SendOptions(message = message)
    val response = Await.result(session.sendAndWait(opts), 30.seconds)
    response.message
  } match {
    case Success(msg) => Right(msg)
    case Failure(e: ConnectionException) => Left(ConnectionError(e.getMessage))
    case Failure(e: TimeoutException) => Left(TimeoutError(e.getMessage))
    case Failure(e) => Left(UnknownError(e.getMessage, e))
  }
}

// Usage
askSafely(client, "What are case classes?") match {
  case Right(answer) => println(s"Answer: $answer")
  case Left(ConnectionError(msg)) => System.err.println(s"Connection: $msg")
  case Left(TimeoutError(msg)) => System.err.println(s"Timeout: $msg")
  case Left(UnknownError(msg, _)) => System.err.println(s"Error: $msg")
}
```

## Resource Management with Using

```scala
import com.github.copilot._
import scala.util.Using
import scala.concurrent.Await
import scala.concurrent.duration._

case class ManagedClient(opts: CopilotOptions) extends AutoCloseable {
  private val client = CopilotClient(opts)
  Await.result(client.start(), 10.seconds)

  def get: CopilotClient = client

  override def close(): Unit = {
    Await.result(client.stop(), 5.seconds)
  }
}

// Usage with Using (Scala 2.13+)
Using(ManagedClient(CopilotOptions())) { managed =>
  val config = SessionConfig()
  val session = Await.result(managed.get.createSession(config), 10.seconds)
  val opts = SendOptions(message = "Tell me about Scala traits")
  val response = Await.result(session.sendAndWait(opts), 30.seconds)
  println(response.message)
}
```

## Best Practices

- **Prefer `Future.recover` over try/catch**: Use functional combinators for async error handling.
- **Use `Either` for typed errors**: Make error types explicit in your API with sealed traits.
- **Implement `AutoCloseable`**: Wrap the client for use with `Using` or try-with-resources patterns.
- **Pattern match on failure types**: Scala's pattern matching gives precise control over error handling.
- **Use exponential backoff with recursion**: Recursive futures are idiomatic for retry logic in Scala.
- **Set explicit timeouts on Await**: Never use `Await.result` without a duration to prevent indefinite blocking.
