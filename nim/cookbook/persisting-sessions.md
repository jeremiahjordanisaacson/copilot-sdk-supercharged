# Persisting Sessions - Nim

Patterns for saving and resuming Copilot sessions across application restarts in Nim.

## Basic Save and Load with JSON

**Scenario:** Save session state to a JSON file so it can be restored later.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc saveSessionState(session: CopilotSession; filepath: string) =
  let state = session.getState()
  writeFile(filepath, $state)
  echo "Session saved to ", filepath

proc loadSessionState(filepath: string): JsonNode =
  try:
    result = parseJson(readFile(filepath))
  except IOError:
    result = newJNull()

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  discard await session.sendAndWait(newMessageOptions(
    "Remember that my name is Alice.",
  ))
  saveSessionState(session, "session_state.json")

waitFor main()
```

## Resuming a Saved Session

**Scenario:** Restore a previously saved session and continue the conversation.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  var session: CopilotSession

  try:
    let saved = parseJson(readFile("session_state.json"))
    let sessionId = saved["sessionId"].getStr()
    session = await client.resumeSession(newResumeSessionConfig(
      sessionId = sessionId,
      systemPrompt = "You are a helpful assistant.",
    ))
    echo "Resumed existing session"
  except:
    session = await client.createSession(newSessionConfig(
      systemPrompt = "You are a helpful assistant.",
    ))
    echo "Started new session"

  let response = await session.sendAndWait(newMessageOptions("What is my name?"))
  echo "Response: ", response.message

waitFor main()
```

## Session Store with Multiple Named Sessions

**Scenario:** Persist multiple named sessions to disk, each in its own file.

```nim
import std/[asyncdispatch, json, os, strutils]
import copilot_sdk

type
  SessionStore = ref object
    directory: string

proc newSessionStore(directory: string): SessionStore =
  createDir(directory)
  SessionStore(directory: directory)

proc filepath(store: SessionStore; name: string): string =
  store.directory / name & ".json"

proc save(store: SessionStore; name: string; session: CopilotSession) =
  let state = session.getState()
  writeFile(store.filepath(name), $state)

proc load(store: SessionStore; name: string): JsonNode =
  try:
    result = parseJson(readFile(store.filepath(name)))
  except IOError:
    result = newJNull()

proc delete(store: SessionStore; name: string) =
  removeFile(store.filepath(name))

proc list(store: SessionStore): seq[string] =
  for kind, path in walkDir(store.directory):
    if kind == pcFile and path.endsWith(".json"):
      result.add(extractFilename(path).replace(".json", ""))

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let store = newSessionStore("./sessions")

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a project planning assistant.",
  ))

  discard await session.sendAndWait(newMessageOptions(
    "We are building a REST API in Nim.",
  ))
  store.save("project-alpha", session)

  echo "Saved sessions: ", store.list()

waitFor main()
```

## Auto-Save on Every Turn

**Scenario:** Automatically save session state after every message exchange.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc autoSavingSend(session: CopilotSession; opts: MessageOptions;
                    savePath: string): Future[SendResult] {.async.} =
  result = await session.sendAndWait(opts)
  let state = session.getState()
  writeFile(savePath, $state)

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  let savePath = "current_session.json"
  discard await autoSavingSend(session, newMessageOptions("Hello!"), savePath)
  discard await autoSavingSend(session, newMessageOptions("What can you help with?"), savePath)
  echo "Session auto-saved to ", savePath

waitFor main()
```

## Session State with Metadata

**Scenario:** Save extra metadata alongside the session state.

```nim
import std/[asyncdispatch, json, times]
import copilot_sdk

proc saveWithMetadata(session: CopilotSession; filepath: string;
                      metadata: JsonNode) =
  let envelope = %*{
    "metadata": metadata,
    "state": session.getState(),
    "savedAt": $now().utc,
  }
  writeFile(filepath, pretty(envelope))

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  discard await session.sendAndWait(newMessageOptions("Hello!"))

  saveWithMetadata(session, "session_with_meta.json", %*{
    "description": "Test session for cookbook example",
    "createdBy": "cookbook",
    "turnCount": 1,
  })

  let envelope = parseJson(readFile("session_with_meta.json"))
  echo "Saved at: ", envelope["savedAt"].getStr()
  echo "Description: ", envelope["metadata"]["description"].getStr()

waitFor main()
```

## Best Practices

1. **Use the standard `json` module** for serialization since it is part of Nim's stdlib.
2. **Save after every meaningful exchange** to minimize data loss on crashes.
3. **Store metadata** (timestamps, descriptions, turn counts) alongside session state for easier management.
4. **Use a directory-based store** for multi-session applications to keep files organized.
5. **Handle missing files gracefully** by returning `newJNull()` from load functions instead of raising errors.
6. **Clean up old session files** periodically to avoid disk usage growth in long-running applications.
