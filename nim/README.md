# GitHub Copilot SDK for Nim

The Nim SDK for the GitHub Copilot CLI. Build AI-powered applications using idiomatic Nim with async/await, ref objects, templates, and the standard library.

## Requirements

- Nim >= 2.0.0
- GitHub Copilot CLI 1.0.113 or newer installed (or set `COPILOT_CLI_PATH`)

## Installation

The `copilot_sdk` package is not in the Nimble package directory yet, so install it from a clone of this repository first:

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
cd copilot-sdk-supercharged/nim
nimble install
```

Then add it to your `.nimble` file:

```nim
requires "copilot_sdk >= 3.1.0"
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

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client procedures below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, raised as `CopilotRpcError` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret value is never logged.

```nim
let saved = await client.mcpSecretsSave("my-server-token", getEnv("MY_SERVER_TOKEN"))  # optional configDir
echo(if saved.usedKeychain: "Stored in the OS keychain" else: "Stored in the file backend")

await client.mcpSecretsDelete("my-server-token")
await client.mcpSecretsDeleteServer("my-server")  # every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions. `extensionApprovalsList` and `extensionApprovalsRevoke` take an optional `sessionId` to use an attached session's configuration profile.

```nim
# Persist a pending extension permission request before answering it normally
let grant = await client.extensionApprovalsGrant(session.sessionId, requestId)
if grant.granted and grant.approval.isSome:
  echo "Approved ", grant.approval.get().extensionName

let listed = await client.extensionApprovalsList()
for approval in listed.approvals:
  # extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
  discard await client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey)
```

### Ask-user image attachments

Set `onUserInputRequest` on the session (it sends `requestUserInput`) to answer the agent's ask_user questions with a `UserInputResponse`. When the runtime accepts images in the answer, the `UserInputRequest` carries `imageAttachmentConstraints` (`allowedMimeTypes`, `maxBytesPerImage`, `maxCount`); it is `none` otherwise. The request also exposes `requestId` and `meta` (the opaque `_meta` object) when sent. Answer with `attachments`, a seq of `UserInputImageAttachment` (base64 `data`, `mimeType`, optional `description` and `displayName`; the wire `type` is always `"image"`). Image-only answers use an empty `answer` with `wasFreeform: true`; the runtime enforces the limits.

```nim
import std/base64

let session = await client.createSession(newSessionConfig(
  onUserInputRequest = proc(req: UserInputRequest): UserInputResponse =
    let constraints = req.imageAttachmentConstraints
    if constraints.isSome and "image/png" in constraints.get().allowedMimeTypes:
      UserInputResponse(answer: "", wasFreeform: true, attachments: @[
        UserInputImageAttachment(data: encode(readFile("screenshot.png")),
                                 mimeType: "image/png", displayName: "screenshot.png")])
    else:
      UserInputResponse(answer: "No screenshot available", wasFreeform: true)
))
```

Elicitation requests also pass `requestId` and the opaque `meta` (`_meta`) through to the `ElicitationRequest` your `elicitationHandler` receives.

## What's New in v3.0.0

**SDK protocol 4.** The SDK now speaks SDK protocol 4 (`SdkProtocolVersion == 4`), which requires Copilot CLI 1.0.113 or newer. Older CLIs are rejected at `start` with an `SDK protocol version mismatch` error. Existing options keep working: you pass the same `newSessionConfig` options as before, and the SDK translates the flat payload into the grouped protocol 4 shape (`groupSessionParams`) right before `session.create` and `session.resume`. A legacy single `provider` is translated into a named `"default"` provider automatically.

New client options:

```nim
var config = newClientConfig(
  mode = cmCodingAgent,  # default; cmEmpty requires availableTools on every session
  clientInfo = ClientInfo(editorName: "my-editor", editorVersion: "1.2.0"),
)
# Network callbacks, registered with network.setProvider after connecting.
config.network = NetworkOptions(
  scope: "connection",
  resolveProxy: proc(url: string): Option[string] = some("PROXY proxy.example:8080; DIRECT"),
  authenticateBasic: proc(req: NetworkBasicAuthRequest): Option[NetworkBasicCredentials] =
    some(NetworkBasicCredentials(username: "user", password: getEnv("PROXY_PASSWORD"))),
)
let client = newCopilotClient(config)
await client.start()
await client.clearManagedSettingsCache()  # managedSettings.clearCache
```

In `cmEmpty` mode every `createSession` / `resumeSession` must pass `availableTools`, and conservative defaults (no telemetry, in-memory OAuth and embedding storage, no repository hooks, no host git operations) are applied beneath your own values:

```nim
let client = newCopilotClient(newClientConfig(mode = cmEmpty))
let session = await client.createSession(newSessionConfig(
  availableTools = some(@["view", "grep"]),
))
```

New session options (all optional named arguments of `newSessionConfig`):

```nim
let session = await client.createSession(newSessionConfig(
  model = "gateway/chat",
  askUserVariant = AskUserVariantElicitation,
  featureFlags = {"my_flag": true}.toTable,
  allowedModels = @["gpt-5", "claude-sonnet-4.5"],
  authClientIdMetadataUrl = "https://example.com/oauth/client.json",
  agentDirectories = @["./agents"],
  hookDirectories = @["./hooks"],
  trustWorkingDirectory = TrustWorkingDirectorySession,
  sandbox = some(SandboxConfig(enabled: true, allowBypass: true)),
  modelCapabilities = some(ModelCapabilitiesOverride(maxOutputTokens: 4096)),
  imageGeneration = some(ImageGenerationConfig(enabled: some(true), saveToDisk: some(false))),
  autopilot = some(AutopilotConfig(continuationEnabled: some(true), maxContinues: some(3))),
))
```

Named BYOK providers (select a registered model with `"provider/id"`), with `apiKeyCommand` for fresh keys:

```nim
let session = await client.createSession(newSessionConfig(
  model = "gateway/chat",
  providers = @[NamedProviderConfig(name: "gateway", `type`: "openai",
                                    baseUrl: "https://models.example.com/v1",
                                    apiKeyCommand: "get-model-key")],
  models = @[ProviderModelConfig(provider: "gateway", id: "chat", wireModel: "gpt-5")],
  defaultProvider = "gateway",
  capi = some(CapiConfig(enabled: some(false))),
))

# The legacy single provider still works and gains apiKeyCommand:
let legacy = await client.createSession(newSessionConfig(
  model = "my-deployment",
  provider = some(ProviderConfig(`type`: "openai", baseUrl: "https://models.example.com/v1",
                                 apiKeyCommand: "get-model-key")),
))
```

GitHub token provider (mutually exclusive with `githubToken`; unregistered when the session closes):

```nim
let session = await client.createSession(newSessionConfig(
  gitHubTokenProvider = proc(req: GitHubTokenRequest): GitHubTokenResult =
    GitHubTokenResult(accessToken: fetchToken(req.host), expiresIn: 7200),
))
```

Hooks, including the subagent lifecycle hooks (`HookSubagentStart`, `HookSubagentStop`):

```nim
var hooks = initTable[string, HookHandler]()
hooks[HookSubagentStart] = proc(input: JsonNode): JsonNode =
  echo "subagent started: ", input
  nil
hooks[HookSubagentStop] = proc(input: JsonNode): JsonNode = nil
let session = await client.createSession(newSessionConfig(hooks = hooks))
```

Shell authentication and memory store callbacks:

```nim
let session = await client.createSession(newSessionConfig(
  onShellAuthenticationRequest = proc(req: ShellAuthenticationRequest): ShellAuthenticationResponse =
    if req.program == "sudo": ShellAuthenticationResponse(accept: true, value: getEnv("SUDO_PASSWORD"))
    else: ShellAuthenticationResponse(accept: false),
  onStoreMemory = proc(memory: StoredMemory) =
    echo "remember: ", memory.subject, " = ", memory.fact
))
```

Idempotent tools, declared per session:

```nim
let lookup = newTool("lookup", "Look up a record", @[newToolParameter("id", "string", "Record id")],
  proc(params: JsonNode): string = $(%*{"id": params{"id"}.getStr()}),
  idempotent = true)
let session = await client.createSession(newSessionConfig(tools = @[lookup]))
```

Message options: source, client turn id, and structured output:

```nim
let reply = await session.send(newMessageOptions(
  "List three colors",
  source = MessageSourceSystem,          # or agentMessageSource("reviewer")
  clientTurnId = "turn-42",
  responseSchemaJson = """{"type": "object", "properties": {"colors": {"type": "array"}}}""",
))
```

Session methods:

```nim
discard await session.switchAutoTier(atBalance)        # session.model.switchAutoTier
discard await session.recheckPolicyHelper()            # session.managedSettings.recheckPolicyHelper
let modeResult = await session.setPermissionMode("yolo")
if not modeResult.success and modeResult.reason == PermissionModeRefusalManagedPolicy:
  echo "Blocked by managed policy"
await session.detach()                                 # session.detach
```

Resume options: transcript recovery and resume event suppression:

```nim
let resumed = await client.resumeSession(newResumeSessionConfig(
  "existing-session-id",
  options = newSessionConfig(tools = @[lookup]),
  allowTranscriptRecovery = true,
  disableResume = true,  # sent as suppressResumeEvent
))
```

Shell output events and Policy Helper failures:

```nim
session.on("tool.shell_output", proc(event: SessionEvent) =
  let chunk = parseShellOutputData(event.data)
  stdout.write(chunk.text)
)

try:
  discard await client.createSession(newSessionConfig())
except CopilotRpcError as e:
  let failure = e.policyHelperFailure()
  if failure.isSome:
    echo "Policy Helper: ", failure.get().message
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, the `ahp.*` / `host.*` RPC families) is not implemented: it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection. The SDK keeps spawning the Copilot CLI over stdio (`cliUrl` remains unsupported).
- Server requests are answered synchronously on the reader thread, so `shellAuthentication.cancel` cannot interrupt a handler that is already running; the cancellation is acknowledged.
- The `connect` handshake is only sent when `clientInfo` is set; otherwise the SDK uses `ping` as before.

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
| `copilot_sdk/session_params` | Protocol 4 session payload translation (`groupSessionParams`) |
| `copilot_sdk/sdk_protocol_version` | `SdkProtocolVersion` (4) |

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

## Recent Features (v2.4–v2.5)

Recent upstream syncs added session and client options for parity with `@github/copilot-sdk`. `newSessionConfig` accepts each session option as a named argument; complex JSON-valued options use a `*Json` string field.

**v2.5 wave (`newSessionConfig` args unless noted):**

- **Reasoning effort** — `reasoningEffort` (e.g. `"minimal"`, `"low"`, `"medium"`, `"high"`, `"max"`).
- **Tool search** — `toolSearchJson` (JSON string); discover tools on demand.
- **Session rewind** — `rewindEnabled`; roll the conversation back to an earlier turn.
- **Content exclusion** — `contentExclusion`; honor rules that hide files from the agent.
- **Additional directories** — `additionalDirectories`.
- **Disabled MCP servers** — `disabledMcpServers`.
- **GitHub MCP tool config** — `githubMcpToolConfigJson`.
- **Canvas provider** — `canvasProvider`.
- **Custom agents local-only** — `customAgentsLocalOnly`.
- **Experimental mode** — `experimentalMode`.
- **Permission decision context** — `decisionContextJson` (on the permission reply).
- **Agent factory args** — `argsSchemaJson`.
- **Built-in plugin directories** — `ClientConfig.builtinPluginDirectories`.
- **In-process FFI transport** — `ClientConfig.inProcess`.

**v2.4 wave:**

- **BYOK bearer token provider** — `ClientConfig.bearerTokenProvider` (`BearerTokenProvider`, receives `ProviderTokenArgs`).
- **MCP OAuth token handler** — `onMcpAuthRequest` (`McpAuthHandler`).
- **HTTP request handler** — `ClientConfig.requestHandler` (`CopilotRequestHandler`).
- **Session citations** — `enableCitations`.
- **Excluded built-in agents** — `excludedBuiltinAgents`.
- **Session spending limits** — `sessionLimits` (`SessionLimitsConfig.maxAiCredits`).
- **OTLP telemetry protocol** — `otlpProtocol`.
- **WebSocket transport** — `enableWebSocketResponses`.
- **Session memory** — `memory` (`MemoryConfiguration`).
- **Experiment assignments** — `expAssignmentsJson`.
- **Message agent mode / display prompt** — `MessageOptions.agentMode`, `MessageOptions.displayPrompt`.
- **Tool defer loading** — `ToolDefer` (`tdAuto` / `tdNever`).
- **System-message sections** — `SystemMessageSection` (`smsPreamble` / `smsPreserve`).
- **Hook identifiers** — `HookPostToolUse`, `HookPreMcpToolCall`, `HookUserPromptTransformed`.
- **GitHub attachments** — `GitHubCommit`, `GitHubRepository`.

### Reasoning effort

```nim
let session = await client.createSession(newSessionConfig(
  reasoningEffort = "high",
))
```

### Session rewind & additional directories

```nim
let session = await client.createSession(newSessionConfig(
  rewindEnabled = true,
  additionalDirectories = @["/repo/docs", "/repo/vendor"],
))
```

### Content exclusion

```nim
let session = await client.createSession(newSessionConfig(
  contentExclusion = true,
))
```

### Tool search

```nim
let session = await client.createSession(newSessionConfig(
  toolSearchJson = """{"enabled": true}""",
))
```

### BYOK bearer token provider & in-process transport

```nim
var config = newClientConfig(cliPath = "/usr/local/bin/copilot")
config.bearerTokenProvider = proc(args: ProviderTokenArgs): string =
  fetchToken(args.sessionId)
config.inProcess = true
config.builtinPluginDirectories = @["./plugins"]
let client = newCopilotClient(config)
```

### Spending limits & citations

```nim
let session = await client.createSession(newSessionConfig(
  sessionLimits = SessionLimitsConfig(maxAiCredits: 5.0),
  enableCitations = true,
))
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
| `ClientMode` | `cmCodingAgent` (default) or `cmEmpty` |
| `ClientInfo` | Integrating application identity for `connect` |
| `NetworkOptions` | Proxy resolution and proxy authentication callbacks |
| `NamedProviderConfig` / `ProviderModelConfig` | Named BYOK providers and their models |
| `ProviderConfig` | Legacy single BYOK provider (translated automatically) |
| `SandboxConfig` | Sandbox settings, including `allowBypass` |
| `CopilotRpcError` | JSON-RPC error with `code`, `data`, and `policyHelperFailure` |
| `PolicyHelperFailure` | Structured Policy Helper failure |
| `PermissionModeResult` | Result of `setPermissionMode`, with refusal `reason` |
| `ShellOutputData` | Data of a `tool.shell_output` event |
| `UserInputRequest` / `UserInputResponse` | ask_user question (with `imageAttachmentConstraints`) and answer (with image `attachments`) |
| `UserInputImageAttachment` | Image attached to an ask_user answer |
| `McpSecretsSaveResult` | Result of `mcpSecretsSave` (`usedKeychain`) |
| `PermissionsExtensionApproval` | A user-scoped plugin extension approval |

### Key Procedures

| Procedure | Description |
|-----------|-------------|
| `newCopilotClient` | Create a new client |
| `client.start` | Start CLI and connect (checks SDK protocol 4) |
| `client.stop` | Stop CLI and clean up |
| `client.createSession` | Create a new conversation |
| `client.resumeSession` | Resume an existing conversation |
| `client.registerTools` | Register tools for the model |
| `client.defineTool` | Define and register a tool |
| `client.clearManagedSettingsCache` | Clear cached managed settings |
| `client.mcpSecretsSave` | Save an MCP credential in the runtime secret store |
| `client.mcpSecretsDelete` | Delete an MCP credential |
| `client.mcpSecretsDeleteServer` | Delete the MCP credentials indexed for a server |
| `client.extensionApprovalsGrant` | Persist a pending plugin extension approval |
| `client.extensionApprovalsList` | List plugin extension approvals |
| `client.extensionApprovalsRevoke` | Revoke a plugin extension approval |
| `session.send` | Send a message (non-blocking) |
| `session.sendAndWait` | Send and wait for idle |
| `session.on` | Subscribe to events |
| `session.disconnect` | Disconnect session |
| `session.detach` | Detach from a session without ending it |
| `session.switchAutoTier` | Switch the Auto routing tier |
| `session.recheckPolicyHelper` | Re-run the Policy Helper |
| `session.setPermissionMode` | Set the permission mode |
| `session.getState` | Serialize session state |
| `groupSessionParams` | Translate a flat session payload to protocol 4 groups |
| `defineTool` (template) | Inline tool definition |
