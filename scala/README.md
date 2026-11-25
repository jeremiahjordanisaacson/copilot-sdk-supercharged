# Copilot Supercharged SDK for Scala

Scala 3 SDK for the [GitHub Copilot CLI](https://github.com/github/copilot). Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio or TCP.

## Requirements

- **Scala 3.4+** with **sbt 1.10+**
- **GitHub Copilot CLI** installed and available on your PATH (or specify via `cliPath`)
- **JDK 17+**

## Installation

Add the SDK to your `build.sbt`:

```scala
// Until published to Maven Central, use a local dependency or publishLocal
lazy val copilotSdk = ProjectRef(file("path/to/copilot-sdk-supercharged/scala"), "root")

lazy val myProject = project
  .dependsOn(copilotSdk)
```

Or publish locally and add as a library dependency:

```bash
cd copilot-sdk-supercharged/scala
sbt publishLocal
```

```scala
libraryDependencies += "io.github.jeremiahjordanisaacson" %% "copilot-sdk-supercharged" % "3.1.0"
```

## Quick Start

```scala
import com.github.copilot.*
import io.circe.*
import io.circe.syntax.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

given ExecutionContext = ExecutionContext.global

// Create and start the client
val client = CopilotClient()
Await.result(client.start(), 30.seconds)

// Create a session
val session = Await.result(client.createSession(), 30.seconds)

// Subscribe to events
session.on { event =>
  if event.`type` == SessionEventType.AssistantMessage then
    val content = event.data.hcursor.get[String]("content").getOrElse("")
    println(s"Assistant: $content")
}

// Send a message and wait for the response
val response = Await.result(
  session.sendAndWait(MessageOptions(prompt = "What is Scala?")),
  60.seconds
)

// Clean up
Await.result(session.destroy(), 10.seconds)
client.stop()
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `CopilotClient` methods below call
server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC
method-not-found error, which fails the returned `Future` with a `JsonRpcException` like any other
RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store
(`mcp.secrets.save`, `mcp.secrets.delete`, `mcp.secrets.deleteServer`). The secret value is never
logged. Each method takes an optional `configDir` (None selects the runtime's default).

```scala
client.mcpSecretsSave("my-server-token", sys.env("MY_SERVER_TOKEN")).foreach { saved =>
  println(if saved.usedKeychain then "Stored in the OS keychain" else "Stored in the file backend")
}

client.mcpSecretsDelete("my-server-token")
client.mcpSecretsDeleteServer("my-server")   // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`permissions.extensionApprovals.grant`, `list` and `revoke`). `extensionApprovalsList` and
`extensionApprovalsRevoke` take an optional `sessionId` to use an attached session's configuration
profile.

```scala
// Persist a pending extension permission request (its requestId) before answering it normally
client.extensionApprovalsGrant(session.sessionId, requestId).foreach { grant =>
  if grant.granted then grant.approval.foreach(a => println(s"Approved ${a.extensionName}"))
}

client.extensionApprovalsList().foreach(_.approvals.foreach { approval =>
  // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
  client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey)
    .foreach(result => if result.revoked then println(s"Revoked ${approval.extensionName}"))
})
```

### Ask-user image attachments

When the runtime accepts images in an answer, `UserInputRequest.imageAttachmentConstraints` carries
`allowedMimeTypes`, `maxBytesPerImage` and `maxCount`; it is None otherwise. The request also exposes
`requestId` and `meta` (the opaque `_meta` object) when the runtime sends them. Answer with
`UserInputResponse(..., attachments = Some(List(...)))`, where each `UserInputImageAttachment` holds
base64 `data`, a `mimeType`, and an optional `description` and `displayName` (the wire `type` is
always `"image"`). Attachments are sent only when set; image-only answers use `answer = ""` and
`wasFreeform = true`. The runtime enforces the limits.

```scala
SessionConfig(onUserInputRequest = Some((request, _) =>
  Future.successful {
    if request.imageAttachmentConstraints.exists(_.allowedMimeTypes.contains("image/png")) then
      val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("screenshot.png"))
      val image = UserInputImageAttachment(
        data = java.util.Base64.getEncoder.encodeToString(bytes),
        mimeType = "image/png",
        displayName = Some("screenshot.png")
      )
      UserInputResponse(answer = "", wasFreeform = true, attachments = Some(List(image)))
    else UserInputResponse(answer = "No screenshot available", wasFreeform = true)
  }))
```

## What's New in v3.0.0

This release moves the SDK to **SDK protocol 4** (the protocol of upstream SDK 2.0).

- **Requires Copilot CLI 1.0.113 or newer.** Older CLIs are rejected at `start()` with
  `SDK protocol version mismatch: SDK supports version 4, but server reports version N.`
- **Existing options keep working.** `SessionConfig` and `ResumeSessionConfig` keep their flat
  fields; the SDK translates the payload into the grouped protocol 4 shape (`inference`, `tools`,
  `permissions`, `interaction`, `events`, `agents`, `mcp`, `github`, `state`, ...) right before
  `session.create` and `session.resume`.
- **Legacy BYOK keeps working.** `provider = Some(ProviderConfig(...))` becomes a named `"default"`
  connection that is also the default provider, and Copilot API models are disabled for that
  session, exactly as in v1. Explicit `providers`/`defaultProvider`/`capi` win.
- **Protocol 4 tool and permission flow.** Custom tools and `onPermissionRequest` are served
  through the runtime's `external_tool.requested` and `permission.requested` events, answered
  with `session.tools.handlePendingToolCall` and `session.permissions.handlePendingPermissionRequest`.

### New in v3.0.0

```scala
// Named BYOK providers and model definitions (select a model as "provider/id")
val session = Await.result(client.createSession(SessionConfig(
  model = Some("gateway/chat"),
  providers = Some(List(NamedProviderConfig(
    name = "gateway",
    baseUrl = "https://models.example.com/v1",
    apiKeyCommand = Some("print-model-key"),
    wireApi = Some("responses")
  ))),
  models = Some(List(ProviderModelConfig(provider = "gateway", id = "chat", wireModel = Some("my-deployment")))),
  capi = Some(CapiConfig(enabled = Some(false)))     // BYOK-only catalog
)), 30.seconds)

// Clear a default provider on resume
client.resumeSession(sessionId, ResumeSessionConfig(clearDefaultProvider = true, capi = Some(CapiConfig(enabled = Some(true)))))
```

```scala
// Idempotent tools: the runtime may safely retry them
val lookup = Tool(
  name = "lookup_order",
  description = Some("Look up an order by ID"),
  idempotent = Some(true),
  handler = (_, _) => Future.successful(ToolResultObject("shipped", ToolResultType.Success))
)

// Client turn IDs are echoed on the turn's events
session.send(MessageOptions(prompt = "Summarize the diff", clientTurnId = Some("turn-42")))

// Resume without emitting session.resume
client.resumeSession(sessionId, ResumeSessionConfig(suppressResumeEvent = Some(true)))
```

```scala
// Client mode: "coding-agent" (default) or "empty" (every session must pick its tools,
// and telemetry, repository hooks, cross-session memory, and similar features default off)
val client = CopilotClient(CopilotClientOptions(mode = ClientMode.Empty))
val session = client.createSession(SessionConfig(availableTools = Some(List("view", "grep"))))
```

```scala
// Live shell output events
session.on(SessionEventType.ToolShellOutput, event =>
  ToolShellOutputData.fromEvent(event).foreach(out => print(out.text)))   // out.stream: Some("stderr"), ...

// Shell authentication: answer sudo/ssh prompts from programs that shell tools run
SessionConfig(onShellAuthenticationRequest = Some((request, _) =>
  Future.successful(
    if request.inputKind == "confirm" then ShellAuthenticationResult.Accept("yes")
    else ShellAuthenticationResult.Cancel
  )))

// Memory store: receive the memories the agent saves (requires memory enabled)
SessionConfig(memory = Some(MemoryConfiguration(enabled = true, storeMemory = Some(memory =>
  Future(myStore.save(memory.subject, memory.fact))))))
```

```scala
// Network callbacks: proxy resolution and proxy authentication (never log credentials)
val client = CopilotClient(CopilotClientOptions(network = Some(NetworkConfig(
  scope = Some(NetworkScope.Connection),
  resolveProxy = Some(_ => Future.successful(Some("PROXY proxy.corp.example:8080; DIRECT"))),
  authenticateBasic = Some(_ => Future.successful(Some(NetworkBasicCredentials(Some(proxyUser), Some(proxyPassword)))))
))))
```

```scala
// Policy Helper failures on create/resume, and permission-mode refusal reasons
client.createSession(SessionConfig(permissionMode = Some(PermissionMode.Manual))).failed.foreach { e =>
  PolicyHelperFailure.fromError(e).foreach(f => println(s"${f.category}: ${f.message}"))
}

session.setPermissionMode(PermissionMode.AllowAll).foreach { result =>
  if !result.success && result.reason.contains(PermissionModeRefusalReason.ManagedPolicy) then
    println(s"Blocked by managed policy; mode stays ${result.mode}")
}
```

### Also available (shipped in v2.6.0)

- **Client info handshake**: `CopilotClientOptions(clientInfo = Some(ClientInfo(applicationName = Some("acme-portal"), applicationVersion = Some("2.4.0"))))`.
- **Auto routing tier**: `session.setAutoTier(Some(AutoTier.Balance))` (sends `session.model.switchAutoTier`) or `SessionConfig(autoTier = ...)`.
- **Ask-user variant**: `SessionConfig(askUserVariant = Some(AskUserVariant.Elicitation))`.
- **GitHub token provider**: `SessionConfig(gitHubTokenProvider = Some(args => Future.successful(GitHubTokenProviderResult.Token(fetchToken(args.host), 3600))))`.
- **Sandbox bypass**: `SessionConfig(sandboxConfig = Some(SandboxConfig(enabled = true, allowBypass = Some(true))))`.
- **Feature flag overrides**: `SessionConfig(featureFlags = Some(Map("my_flag" -> true)))`.
- **Session detach**: `session.destroy()` sends `session.detach`, so the session can be resumed later.
- **Message source**: `MessageOptions(prompt = "Run the checks", source = Some(MessageSource.agent("planner")))`.
- **Managed settings cache clear**: `client.clearManagedSettingsCache()`.
- **Policy helper recheck**: `session.recheckPolicyHelper()` (parse the snapshot's `policyHelperFailure` with `PolicyHelperFailure.fromJson`).
- **Model allowlist**: `SessionConfig(allowedModels = Some(List("gpt-5", "claude-sonnet-4.5")))`.
- **OAuth client metadata URL**: `SessionConfig(authClientIdMetadataUrl = Some("https://example.com/oauth/client.json"))`.
- **Max output tokens override**: `SessionConfig(modelCapabilities = Some(Map("limits" -> Json.obj("max_output_tokens" -> 8192.asJson))))`.
- **Structured output**: `session.sendAndWaitStructured(MessageOptions(prompt = "List three colors"), schema)` or `MessageOptions(..., responseSchema = Some(schema))`.
- **Agent/hook directories**: `SessionConfig(agentDirectories = Some(List("./agents")), hookDirectories = Some(List("./hooks")))`.
- **Working directory trust** (create only): `SessionConfig(trustWorkingDirectory = Some(WorkingDirectoryTrust.Session))`.
- **Transcript recovery** (resume only): `ResumeSessionConfig(allowTranscriptRecovery = Some(true))`.
- **Image generation config**: `SessionConfig(imageGeneration = Some(ImageGenerationConfig(enabled = Some(true), watermark = Some(true), saveToDisk = Some(false))))`.
- **Autopilot options**: `SessionConfig(autopilot = Some(AutopilotOptions(continuationEnabled = Some(true), maxContinues = Some(3))))`.
- **Subagent lifecycle hooks**: `SessionHooks(onSubagentStart = Some((input, _) => Future.successful(None)), onSubagentStop = Some((input, _) => Future.successful(None)))`.
- **BYOK API key command**: `ProviderConfig(baseUrl = "https://models.example.com/v1", apiKeyCommand = Some("print-model-key"))` or `NamedProviderConfig(..., apiKeyCommand = ...)`.

### Known limitations

- **AHP host**: the runtime-supervised AHP host (`startAhpHost` and the `ahp.*`/`host.*` RPC
  families) is not implemented. It is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
- **In-process hosting**: this SDK keeps spawning the CLI (or connecting over TCP) instead of
  hosting the runtime in-process by default; `CopilotClientOptions.inProcess` is unchanged.
- **Legacy response format**: `MessageOptions.responseFormat` is no longer sent, because protocol 4
  rejects it. Use `responseSchema` for structured output and `SessionConfig.imageGeneration` for images.

## Architecture

The SDK follows the same architecture as all other SDKs in this repository (40 languages supported):

```
CopilotClient
  |-- spawns CLI process (--headless --no-auto-update --stdio)
  |-- JsonRpcClient (Content-Length header framing)
  |     |-- sendRequest() / sendNotification()
  |     |-- onRequest() / onNotification()
  |-- CopilotSession (one per conversation)
        |-- send() / sendAndWait()
        |-- on() event subscriptions
        |-- tool handlers
        |-- permission/userInput/hooks handlers
```

### JSON-RPC 2.0 Protocol

Messages use **Content-Length header framing** (same as LSP):

```
Content-Length: 73\r\n
\r\n
{"jsonrpc":"2.0","id":"abc-123","method":"ping","params":{"message":"hi"}}
```

### Transport Modes

- **stdio** (default): Communicates via stdin/stdout of the spawned CLI process
- **TCP**: Connects to an existing CLI server over a TCP socket

## API Reference

### CopilotClient

The main entry point. Manages the CLI process and JSON-RPC connection.

```scala
val client = CopilotClient(CopilotClientOptions(
  cliPath = Some("/usr/local/bin/copilot"),
  logLevel = "debug",
  useStdio = true
))
```

| Method | Description |
|--------|-------------|
| `start(): Future[Unit]` | Start the CLI server and connect |
| `stop(): List[Throwable]` | Gracefully stop and return cleanup errors |
| `forceStop(): Unit` | Forcefully stop without cleanup |
| `createSession(config): Future[CopilotSession]` | Create a new session |
| `resumeSession(id, config): Future[CopilotSession]` | Resume an existing session |
| `ping(message): Future[PingResponse]` | Ping the server |
| `getStatus(): Future[GetStatusResponse]` | Get CLI status |
| `getAuthStatus(): Future[GetAuthStatusResponse]` | Get auth status |
| `listModels(): Future[List[ModelInfo]]` | List available models (cached) |
| `deleteSession(id): Future[Unit]` | Delete a session |
| `listSessions(): Future[List[SessionMetadata]]` | List all sessions |
| `mcpSecretsSave(secretId, value, configDir): Future[McpSecretsSaveResult]` | Save an MCP credential |
| `mcpSecretsDelete(secretId, configDir): Future[Unit]` | Delete an MCP credential |
| `mcpSecretsDeleteServer(serverName, configDir): Future[Unit]` | Delete the MCP credentials indexed for a server |
| `extensionApprovalsGrant(sessionId, requestId): Future[PermissionsExtensionApprovalsGrantResult]` | Persist a pending plugin extension approval |
| `extensionApprovalsList(sessionId): Future[PermissionsExtensionApprovalsListResult]` | List plugin extension approvals |
| `extensionApprovalsRevoke(extensionName, installationKey, sessionId): Future[PermissionsExtensionApprovalsRevokeResult]` | Revoke a plugin extension approval |

### CopilotSession

Represents a single conversation. Created via `client.createSession()`.

| Method | Description |
|--------|-------------|
| `send(options): Future[String]` | Send a message, returns message ID |
| `sendAndWait(options, timeout): Future[Option[SessionEvent]]` | Send and wait for idle |
| `on(handler): () => Unit` | Subscribe to all events |
| `on(eventType, handler): () => Unit` | Subscribe to specific event type |
| `getMessages(): Future[List[SessionEvent]]` | Get message history |
| `destroy(): Future[Unit]` | Destroy the session |
| `abort(): Future[Unit]` | Abort current processing |

### DefineTool

Helper for creating tools with a clean builder API:

```scala
val myTool = DefineTool(
  name = "my_tool",
  description = "Does something useful",
  parameters = JsonObject(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "input" -> Json.obj("type" -> "string".asJson)
    ),
    "required" -> Json.arr("input".asJson)
  )
) { (args, invocation) =>
  val input = args.hcursor.get[String]("input").getOrElse("")
  Future.successful(ToolResultObject(
    textResultForLlm = s"Processed: $input",
    resultType = ToolResultType.Success
  ))
}
```

Convenience variants:

- `DefineTool.sync(...)` -- synchronous handler returning `ToolResultObject`
- `DefineTool.simple(...)` -- async handler returning `Future[String]`
- `DefineTool.simpleSync(...)` -- synchronous handler returning `String`

### Session Configuration

```scala
val config = SessionConfig(
  model = Some("gpt-4"),
  reasoningEffort = Some(ReasoningEffort.High),
  tools = List(myTool),
  systemMessage = Some(SystemMessageConfig.append("You are a helpful assistant.")),
  streaming = Some(true),
  workingDirectory = Some("/my/project"),
  onPermissionRequest = Some { (request, inv) =>
    Future.successful(PermissionRequestResult(kind = PermissionRequestResult.Approved))
  },
  onUserInputRequest = Some { (request, inv) =>
    Future.successful(UserInputResponse(answer = "yes", wasFreeform = true))
  },
  hooks = Some(SessionHooks(
    onPreToolUse = Some { (input, inv) =>
      Future.successful(Some(PreToolUseHookOutput(
        permissionDecision = Some("allow")
      )))
    }
  ))
)
```

### Session Events

Events are delivered as `SessionEvent` instances with a `type` discriminator:

```scala
session.on { event =>
  event.`type` match
    case SessionEventType.AssistantMessage =>
      val content = event.data.hcursor.get[String]("content").getOrElse("")
      println(s"Assistant: $content")
    case SessionEventType.SessionIdle =>
      println("Session is idle")
    case SessionEventType.SessionError =>
      val msg = event.data.hcursor.get[String]("message").getOrElse("")
      println(s"Error: $msg")
    case _ => ()
}
```

### Connecting to an Existing Server

```scala
val client = CopilotClient(CopilotClientOptions(
  cliUrl = Some("localhost:3000")
))
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```scala
val client = CopilotClient(CopilotClientOptions(
  sessionIdleTimeoutSeconds = Some(300)
))
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```scala
val client = CopilotClient(CopilotClientOptions(
  sessionFs = Some(SessionFsConfig(
    initialCwd = "/repo",
    sessionStatePath = "/state",
    conventions = "posix"
  ))
))
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```scala
val meta = client.getSessionMetadata("session-123")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```scala
val session = client.createSession(SessionConfig(
  skillDirectories = Seq("./skills"),
  disabledSkills = Seq("test-skill"),
  includeSubAgentStreamingEvents = Some(true)
))
```

- `skillDirectories` - list of directories containing skill definitions to register with the session
- `disabledSkills` - list of skill names to explicitly disable
- `includeSubAgentStreamingEvents` - when true, surfaces streaming events from sub-agents in the event stream

### Custom Providers (BYOK)

```scala
val config = SessionConfig(
  provider = Some(ProviderConfig(
    baseUrl = "https://api.openai.com/v1",
    apiKey = Some("sk-..."),
    `type` = Some("openai")
  ))
)
```

### MCP Servers

```scala
val config = SessionConfig(
  mcpServers = Some(Map(
    "my-server" -> MCPServerConfig.local(
      command = "node",
      args = List("server.js"),
      tools = List("*")
    ),
    "remote-server" -> MCPServerConfig.remote(
      url = "https://mcp.example.com",
      `type` = "http"
    )
  ))
)
```

## Protocol Version

The SDK verifies protocol version compatibility on connect. The current version is **4** (defined in `SdkProtocolVersion.scala`; requires Copilot CLI 1.0.113 or newer). If the server reports a different version, the client throws an error.

## Error Handling

- JSON-RPC errors are raised as `JsonRpcException` containing the error code and message
- Tool handler errors are caught and returned as `failure` results to the LLM
- Permission handler errors default to denying the permission
- Hook handler errors are silently ignored (hooks are best-effort)

## Running the Example

```bash
cd copilot-sdk-supercharged/scala
sbt "runMain BasicExample"
```

## Image Generation

Opt in to image generation for the session with `SessionConfig.imageGeneration`. Per-message
`imageOptions` are still forwarded; the legacy `responseFormat` string is no longer sent because
protocol 4 rejects it (use `responseSchema` for structured output).

```scala
val session = Await.result(client.createSession(SessionConfig(
  imageGeneration = Some(ImageGenerationConfig(enabled = Some(true), saveToDisk = Some(false)))
)), 30.seconds)
val response = session.sendAndWait(MessageOptions(
  prompt = "Generate a sunset over mountains",
  imageOptions = Some(ImageOptions(size = Some("1024x1024"), quality = Some("hd"), style = Some("natural")))
))
```

## Recent Features (v2.4–v2.5)

Parity options tracking the upstream `@github/copilot` SDK, exposed as fields on the
`SessionConfig`, `CopilotClientOptions`, `MessageOptions`, and `SessionHooks` case classes.

### v2.5.0

- **Reasoning effort** — `SessionConfig(reasoningEffort = Some(ReasoningEffort.High))` (`Low`/`Medium`/`High`/`XHigh`).
- **Session rewind** — `SessionConfig(rewindEnabled = Some(true))`; roll back to an earlier snapshot (`session.snapshot_rewind` event).
- **Additional directories** — `SessionConfig(additionalDirectories = ...)`; extra workspace roots the agent may read.
- **Content exclusion** — `SessionConfig(contentExclusion = Some(true))`; honor repository content-exclusion rules.
- **Tool search** — `SessionConfig(toolSearch = ...)`; on-demand tool discovery for large tool sets.
- **Disabled MCP servers** — `SessionConfig(disabledMcpServers = ...)`.
- **GitHub MCP tool config** — `SessionConfig(githubMcpToolConfig = ...)`.
- **Canvas provider** — `SessionConfig(canvasProvider = ...)`.
- **Custom agents local-only** — `SessionConfig(customAgentsLocalOnly = Some(true))`.
- **Experimental mode** — `SessionConfig(experimentalMode = Some(true))`.
- **User-prompt-transformed hook** — `SessionHooks(onUserPromptTransformed = ...)`.
- **Built-in plugin directories** — `CopilotClientOptions(builtinPluginDirectories = ...)`.
- **In-process (FFI) transport** — `CopilotClientOptions(inProcess = Some(true))`.
- **Agent factory args schema** — custom agents accept a typed `argsSchema`.
- **Permission decision context** — permission results carry `decisionContext`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions(bearerTokenProvider = ...)`; a `ProviderTokenArgs => Future[String]`.
- **HTTP request handler** — `CopilotClientOptions(requestHandler = ...)` (`CopilotRequestHandler`).
- **MCP OAuth token handler** — `SessionConfig(onMcpAuthRequest = ...)` (`McpAuthHandler`).
- **Session citations** — `SessionConfig(enableCitations = Some(true))`.
- **Excluded built-in agents** — `SessionConfig(excludedBuiltinAgents = ...)`.
- **Session spending limits** — `SessionConfig(sessionLimits = Some(SessionLimitsConfig(maxAiCredits = ...)))`.
- **Session memory** — `SessionConfig(memory = Some(MemoryConfiguration(...)))`.
- **OTLP protocol** — `SessionConfig(otlpProtocol = Some("grpc"))`.
- **WebSocket responses** — `SessionConfig(enableWebSocketResponses = Some(true))`.
- **Experiment assignments** — `SessionConfig(expAssignments = ...)`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `SessionHooks(onPostToolUse = ..., onPreMcpToolCall = ...)`.
- **Per-message agent mode & display prompt** — `MessageOptions(agentMode = ..., displayPrompt = ...)`.
- **System-message sections** — `SystemMessageSection.Preamble` / `.Preserve`.
- **Tool defer policy** — `ToolDefer.Auto` / `.Never`.
- **GitHub attachment variants** — `GitHubAttachment.GitHubCommit` / `.GitHubRepository`.

### Examples

```scala
// Reasoning effort — trade latency for depth on models that support it
val config = SessionConfig(
  model = Some("gpt-5"),
  reasoningEffort = Some(ReasoningEffort.High)   // Low | Medium | High | XHigh
)
```

```scala
// Roll back to an earlier snapshot, expose extra roots, and honor content exclusion
val config = SessionConfig(
  rewindEnabled = Some(true),                    // emits session.snapshot_rewind events
  additionalDirectories = Some(List("/repo/shared", "/repo/docs")),
  contentExclusion = Some(true)
)
```

```scala
// On-demand tool discovery for large tool sets
val config = SessionConfig(
  toolSearch = Some(Map("enabled" -> true.asJson))
)
```

```scala
// BYOK bearer tokens + in-process FFI transport (client-level)
val client = CopilotClient(CopilotClientOptions(
  bearerTokenProvider = Some(args => Future.successful(fetchToken(args.sessionId))),
  builtinPluginDirectories = Some(List("/opt/copilot/plugins")),
  inProcess = Some(true)
))
```

## License

See [LICENSE](../LICENSE) in the repository root.
