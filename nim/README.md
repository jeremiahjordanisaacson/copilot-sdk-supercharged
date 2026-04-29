# GitHub Copilot SDK for Nim

The Nim SDK for the GitHub Copilot CLI. Build AI-powered applications using idiomatic Nim with async/await, ref objects, templates, and the standard library.

## Requirements

- Nim >= 2.0.0
- GitHub Copilot CLI installed (or set `COPILOT_CLI_PATH`)

## Installation

Add to your `.nimble` file:

```nim
requires "copilot_sdk >= 2.0.0"
```

Or clone this repository and use the source directly:

```bash
cd nim
nimble install
```

## Quick Start

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()

  let session = await client.createSession(newSessionConfig(
    systemPrompt = "You are a helpful assistant.",
  ))

  let response = await session.sendAndWait(newMessageOptions("Hello!"))
  echo response.message

  client.stop()

waitFor main()
```

## Architecture

```
Your App --> CopilotClient --> JSON-RPC (stdio) --> Copilot CLI
                 |
                 +-- CopilotSession (conversation)
                 +-- Tools (model-callable functions)
                 +-- Permissions / UI handlers
```

The SDK spawns the Copilot CLI as a child process and communicates over JSON-RPC 2.0 with Content-Length framing (the LSP base protocol).

## Module Structure

| Module | Purpose |
|--------|---------|
| `copilot_sdk` | Main entry point, re-exports all modules |
| `copilot_sdk/client` | `CopilotClient` -- lifecycle, tools, sessions |
| `copilot_sdk/session` | `CopilotSession` -- send, events, state |
| `copilot_sdk/jsonrpc` | JSON-RPC 2.0 framing and message builders |
| `copilot_sdk/types` | All type definitions (objects, enums) |
| `copilot_sdk/tools` | Tool definition helpers and schema generation |

## Creating a Client

```nim
import copilot_sdk

# Default: auto-detect CLI
let client = newCopilotClient()

# Explicit CLI path
let client2 = newCopilotClient(newClientConfig(
  cliPath = "/usr/local/bin/github-copilot",
))

# With extra CLI arguments
let client3 = newCopilotClient(newClientConfig(
  extraArgs = @["--verbose"],
))
```

## Sessions

### Create a Session

```nim
let session = await client.createSession(newSessionConfig(
  systemPrompt = "You are a Nim programming expert.",
))
```

### Send and Wait

```nim
let response = await session.sendAndWait(newMessageOptions("Explain Nim templates"))
echo response.message
```

### Streaming Events

```nim
session.on("assistant.message_delta", proc(event: SessionEvent) =
  if event.data.hasKey("delta"):
    stdout.write(event.data["delta"].getStr())
)

discard await session.send(newMessageOptions("Explain generics", streaming = true))
```

### Resume a Session

```nim
let resumed = await client.resumeSession(newResumeSessionConfig(
  sessionId = "existing-session-id",
  systemPrompt = "You are a helpful assistant.",
))
```

### Session Lifecycle

```nim
await session.disconnect()  # Graceful disconnect
await session.destroy()     # Release server resources
await session.abort()       # Abort current turn
```

## Defining Tools

### Using `defineTool` Template

```nim
let tool = defineTool("get_time", "Returns current time", @[]):
  import std/times
  $now()

await client.registerTool(tool)
```

### With Parameters

```nim
await client.defineTool(
  "calculate",
  "Performs basic arithmetic",
  @[
    newToolParameter("a", "number", "First operand"),
    newToolParameter("b", "number", "Second operand"),
    newToolParameter("op", "string", "Operation: add, sub, mul, div"),
  ],
  proc(params: JsonNode): string =
    let a = params["a"].getFloat()
    let b = params["b"].getFloat()
    case params["op"].getStr()
    of "add": $(%*{"result": a + b})
    of "sub": $(%*{"result": a - b})
    of "mul": $(%*{"result": a * b})
    of "div":
      if b == 0: $(%*{"error": "Division by zero"})
      else: $(%*{"result": a / b})
    else: $(%*{"error": "Unknown operation"})
)
```

### Tool with newTool Constructor

```nim
let tool = newTool(
  "read_file",
  "Read a file from disk",
  @[newToolParameter("path", "string", "File path")],
  proc(params: JsonNode): string =
    let path = params["path"].getStr()
    try:
      $(%*{"content": readFile(path)})
    except IOError as e:
      $(%*{"error": e.msg})
)
await client.registerTool(tool)
```

## Permission and UI Handlers

```nim
# Auto-approve all tool calls
client.permissionHandler = proc(req: PermissionRequest): PermissionDecision =
  echo "Allowing: ", req.toolName
  pdAllow

# Handle user input requests
client.userInputHandler = proc(req: UserInputRequest): string =
  echo req.prompt
  stdout.write("> ")
  stdin.readLine()

# Handle elicitation
client.elicitationHandler = proc(req: ElicitationRequest): string =
  echo req.message
  for i, opt in req.options:
    echo "  ", i + 1, ") ", opt
  stdout.write("Choice: ")
  stdin.readLine()
```

## Per-Session Authentication

```nim
let session = await client.createSession(newSessionConfig(
  systemPrompt = "You are a helpful assistant.",
  githubToken = getEnv("GITHUB_TOKEN"),
))
```

## Session State Persistence

```nim
import std/json

# Save state
let state = session.getState()
writeFile("session.json", $state)

# Later: resume from saved session ID
let saved = parseJson(readFile("session.json"))
let resumed = await client.resumeSession(newResumeSessionConfig(
  sessionId = saved["sessionId"].getStr(),
))
```

## Error Handling

```nim
try:
  await client.start()
except IOError as e:
  echo "Connection failed: ", e.msg

try:
  let response = await session.sendAndWait(newMessageOptions("Hello"))
  echo response.message
except CatchableError as e:
  echo "Request failed: ", e.msg
finally:
  client.stop()
```

## Running Tests

```bash
cd nim
nimble test
# Or directly:
nim c -r tests/test_client.nim
```

## Running Examples

```bash
cd nim
nim c -r examples/basic_example.nim
```

## Cookbook

See the [cookbook/](cookbook/) directory for practical recipes:

- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Tools and Skills](cookbook/tools-and-skills.md)
- [Advanced v2.0 Features](cookbook/advanced-features.md)

## API Summary

### Types

| Type | Description |
|------|-------------|
| `CopilotClient` | Main SDK client, manages CLI process and sessions |
| `CopilotSession` | A single conversation with event subscriptions |
| `ClientConfig` | Client configuration (CLI path, URL, args) |
| `SessionConfig` | Session creation options |
| `ResumeSessionConfig` | Session resume options |
| `MessageOptions` | Message sending options |
| `SendResult` | Response from `sendAndWait` |
| `Tool` | Tool definition with handler |
| `ToolParameter` | Single tool parameter descriptor |
| `SessionEvent` | Server event with kind and data |
| `PermissionRequest` | Permission request from server |
| `PermissionDecision` | Allow or deny permission |
| `ConnectionState` | Client connection state enum |

### Key Procedures

| Procedure | Description |
|-----------|-------------|
| `newCopilotClient` | Create a new client |
| `client.start` | Start CLI and connect |
| `client.stop` | Stop CLI and clean up |
| `client.createSession` | Create a new conversation |
| `client.resumeSession` | Resume an existing conversation |
| `client.registerTools` | Register tools for the model |
| `client.defineTool` | Define and register a tool |
| `session.send` | Send a message (non-blocking) |
| `session.sendAndWait` | Send and wait for idle |
| `session.on` | Subscribe to events |
| `session.disconnect` | Disconnect session |
| `session.getState` | Serialize session state |
| `defineTool` (template) | Inline tool definition |
