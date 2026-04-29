# Error Handling - Nim

Patterns for handling errors gracefully in the Copilot SDK for Nim, including connection failures, timeouts, and resource cleanup.

## Basic Error Handling with try/except

**Scenario:** Catch errors from SDK calls without crashing your application.

```nim
import std/[asyncdispatch, json, os]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()

  try:
    await client.start()
  except IOError as e:
    echo "Failed to start client: ", e.msg
    quit(1)

  var session: CopilotSession
  try:
    session = await client.createSession(newSessionConfig(
      systemPrompt = "You are a helpful assistant.",
    ))
  except CatchableError as e:
    echo "Session creation failed: ", e.msg
    client.stop()
    quit(1)

  let response = await session.sendAndWait(newMessageOptions("Hello!"))
  echo "Response: ", response.message

  client.stop()

waitFor main()
```

## Cleanup with defer

**Scenario:** Ensure the client is always stopped, even if an error occurs mid-session.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "What is error handling?",
  ))
  echo "Response: ", response.message

waitFor main()
```

## Retry Logic

**Scenario:** Transient failures should be retried automatically.

```nim
import std/[asyncdispatch, json, os]
import copilot_sdk

proc retry[T](fn: proc(): Future[T]; maxAttempts: int;
              delayMs: int): Future[T] {.async.} =
  var lastErr: ref CatchableError
  for attempt in 1 .. maxAttempts:
    try:
      return await fn()
    except CatchableError as e:
      lastErr = e
      if attempt < maxAttempts:
        echo "Attempt ", attempt, "/", maxAttempts, " failed: ", e.msg,
             ". Retrying in ", delayMs, "ms..."
        await sleepAsync(delayMs)
  raise lastErr

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  let response = await retry(
    proc(): Future[SendResult] {.async.} =
      result = await session.sendAndWait(newMessageOptions(
        "Summarize the benefits of Nim.",
      )),
    maxAttempts = 3,
    delayMs = 2000,
  )

  echo "Response: ", response.message

waitFor main()
```

## Classifying Error Types

**Scenario:** React differently to connection errors versus timeout errors.

```nim
import std/[asyncdispatch, strutils]
import copilot_sdk

type ErrorKind = enum
  ekTimeout, ekConnection, ekAuth, ekUnknown

proc classifyError(msg: string): ErrorKind =
  let lower = msg.toLowerAscii()
  if "timeout" in lower: ekTimeout
  elif "connection" in lower or "refused" in lower: ekConnection
  elif "auth" in lower or "token" in lower: ekAuth
  else: ekUnknown

proc main() {.async.} =
  let client = newCopilotClient()

  try:
    await client.start()
  except IOError as e:
    case classifyError(e.msg)
    of ekConnection:
      echo "Cannot connect to Copilot CLI. Is it installed and running?"
    of ekAuth:
      echo "Authentication error. Check your credentials."
    of ekTimeout:
      echo "Connection timed out. Try again."
    of ekUnknown:
      echo "Unexpected error: ", e.msg
    quit(1)

  client.stop()

waitFor main()
```

## Resource Guard Template

**Scenario:** Create a reusable template that guarantees cleanup.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

template withClient(config: ClientConfig; body: untyped) =
  let client {.inject.} = newCopilotClient(config)
  await client.start()
  try:
    body
  finally:
    client.stop()

proc main() {.async.} =
  withClient(newClientConfig()):
    let session = await client.createSession(newSessionConfig(
      systemPrompt = "You are a helpful assistant.",
    ))
    let response = await session.sendAndWait(newMessageOptions("Hello!"))
    echo "Response: ", response.message

waitFor main()
```

## Best Practices

1. **Always use try/except** around SDK calls to prevent unhandled crashes.
2. **Use defer** to guarantee `client.stop()` is called on every exit path.
3. **Implement retry with backoff** for transient errors in production systems.
4. **Classify errors** to take targeted recovery actions.
5. **Use templates** for resource management patterns you repeat often.
6. **Log errors to stderr** using `stderr.write()` to keep stdout clean for program output.
