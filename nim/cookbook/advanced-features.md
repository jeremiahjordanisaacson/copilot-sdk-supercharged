# Advanced v2.0 Features - Nim

Recipes for v2.0 SDK features in Nim: per-session auth, SessionFs, commands, system prompts, skills, config discovery, and more.

## Per-Session Authentication

**Scenario:** Provide a GitHub token on each session for user-scoped auth.

```nim
import std/[asyncdispatch, json, os]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let sessionA = await client.createSession(newSessionConfig(
    githubToken = getEnv("GITHUB_TOKEN_USER_A"),
    systemPrompt = "You are a helpful assistant.",
  ))

  let response = await sessionA.sendAndWait(newMessageOptions(
    "Summarize my recent pull requests.",
  ))
  echo "Response: ", response.message

  # Create a second session with a different user token
  let sessionB = await client.createSession(newSessionConfig(
    githubToken = getEnv("GITHUB_TOKEN_USER_B"),
    systemPrompt = "You are a code reviewer.",
  ))

waitFor main()
```

## Session Idle Timeout

**Scenario:** Automatically expire sessions after a period of inactivity.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    sessionIdleTimeoutSeconds = 300, # 5 minutes
    systemPrompt = "You are a helpful assistant.",
  ))

  let response = await session.sendAndWait(newMessageOptions("Hello!"))
  echo "Response: ", response.message
  # Session automatically expires after 300s of inactivity.

waitFor main()
```

## Commands and UI Elicitation

**Scenario:** Handle elicitation requests from the model.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()

  client.elicitationHandler = proc(req: ElicitationRequest): string =
    echo "Model asks: ", req.message
    for i, option in req.options:
      echo "  ", i + 1, ") ", option
    stdout.write("Your choice: ")
    stdin.readLine()

  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a deployment assistant.",
  ))

  let response = await session.sendAndWait(newMessageOptions("/deploy"))
  echo "Response: ", response.message

waitFor main()
```

## System Prompt Customization

**Scenario:** Use different system prompt modes for different sessions.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  # Simple string prompt
  let session1 = await client.createSession(newSessionConfig(
    systemPrompt = "You are a Nim programming expert. Only answer Nim questions.",
  ))

  # Detailed prompt with context
  let session2 = await client.createSession(newSessionConfig(
    systemPrompt = """You are assisting a senior Nim developer.
Always prefer compile-time features (templates, macros) where appropriate.
Use the standard library over third-party packages when possible.
Cite the Nim manual when relevant.""",
  ))

  let response = await session2.sendAndWait(newMessageOptions(
    "How do I implement iterators in Nim?",
  ))
  echo "Response: ", response.message

waitFor main()
```

## Per-Agent Skills

**Scenario:** Configure skill directories and disable specific skills per agent.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a code assistant with limited skills.",
    skillDirectories = @[
      "/home/user/.copilot/skills",
      "/project/.copilot-skills",
    ],
    disabledSkills = @[
      "web-search",
      "image-generation",
    ],
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "Refactor this function to use tail recursion.",
  ))
  echo "Response: ", response.message

waitFor main()
```

## Per-Agent Tool Visibility

**Scenario:** Hide specific tools from the model for a given session.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a read-only code reviewer.",
    excludedTools = @[
      "file_write",
      "shell_execute",
      "git_push",
    ],
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "Review this code and suggest improvements.",
  ))
  echo "Response: ", response.message

waitFor main()
```

## Runtime Request Headers

**Scenario:** Attach custom headers to individual send requests for tracing or routing.

```nim
import std/[asyncdispatch, json, tables]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  var headers = initTable[string, string]()
  headers["X-Request-Id"] = "req-abc-123"
  headers["X-Trace-Parent"] = "00-traceid-spanid-01"

  let response = await session.sendAndWait(newMessageOptions(
    "Explain templates in Nim.",
    requestHeaders = headers,
  ))
  echo "Response: ", response.message

waitFor main()
```

## Streaming Events

**Scenario:** Subscribe to streaming events for real-time output.

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

  session.on("assistant.message_delta", proc(event: SessionEvent) =
    if event.data.hasKey("delta"):
      stdout.write(event.data["delta"].getStr())
  )

  session.on("session.idle", proc(event: SessionEvent) =
    echo "\n[Session idle]"
  )

  discard await session.send(newMessageOptions(
    "Write a tutorial on Nim macros.",
    streaming = true,
  ))

waitFor main()
```

## Permission Handler

**Scenario:** Control which tool calls are allowed.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()

  # Auto-approve read tools, deny write tools
  client.permissionHandler = proc(req: PermissionRequest): PermissionDecision =
    echo "Permission request for: ", req.toolName
    if req.toolName.contains("read") or req.toolName.contains("list"):
      echo "  -> Allowed"
      pdAllow
    else:
      echo "  -> Denied"
      pdDeny

  await client.start()
  defer: client.stop()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a file assistant.",
  ))

  let response = await session.sendAndWait(newMessageOptions(
    "Read the README file.",
  ))
  echo "Response: ", response.message

waitFor main()
```

## Session Metadata and State

**Scenario:** Retrieve and inspect session state for debugging or persistence.

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

  discard await session.sendAndWait(newMessageOptions("Hello!"))

  let state = session.getState()
  echo "Session ID: ", state["sessionId"].getStr()
  echo "Turn count: ", state["turnCounter"].getInt()
  echo "Messages: ", state["messages"].len

  let messages = session.getMessages()
  for msg in messages:
    echo "  [", msg.role, "] ", msg.content[0 ..< min(80, msg.content.len)]

waitFor main()
```

## Best Practices

1. **Use per-session tokens** for multi-user applications where each user has their own GitHub auth.
2. **Set idle timeouts** to clean up abandoned sessions automatically.
3. **Use permission handlers** to enforce security policies on tool invocations.
4. **Stream responses** for interactive UIs that need real-time output.
5. **Attach request headers** for distributed tracing and observability.
6. **Inspect session state** during development to understand conversation flow.
