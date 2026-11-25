# GitHub Copilot Supercharged SDK for Kotlin

The Kotlin SDK for the GitHub Copilot CLI provides a coroutine-based interface for communicating with the Copilot CLI server via JSON-RPC 2.0 over stdio or TCP.

## Requirements

- Kotlin 1.9+
- JDK 17+
- The Copilot CLI installed and available on PATH (or set `COPILOT_CLI_PATH` environment variable)
- Active GitHub Copilot authentication

## Installation

The Kotlin SDK is not published to Maven Central yet, so build it from source and publish it to your local Maven repository (`~/.m2`). The `kotlin/` directory has no Gradle wrapper, so run a local Gradle installation:

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
cd copilot-sdk-supercharged/kotlin
gradle publishToMavenLocal
```

### Gradle (Kotlin DSL)

```kotlin
repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    implementation("io.github.jeremiahjordanisaacson:copilot-sdk-supercharged-kotlin:3.1.0")
    // The SDK uses these as implementation dependencies, so declare them to call it from your code
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.2")
}
```

### Building from source

```bash
cd kotlin
gradle build
```

## Quick Start

```kotlin
import com.github.copilot.*
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    // Create a client (auto-spawns the CLI server)
    val client = CopilotClient()

    // Create a session
    val session = client.createSession()

    // Subscribe to events
    session.on { event ->
        println("[${event.type}] ${event.data}")
    }

    // Send a message and wait for the response
    val response = session.sendAndWait(MessageOptions(prompt = "What is 2+2?"))
    println("Response: ${response?.data}")

    // Clean up
    session.destroy()
    client.stop()
}
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `CopilotClient` methods below call
server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC
method-not-found error, which is thrown as a `JsonRpcException` like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store
(`mcp.secrets.save`, `mcp.secrets.delete`, `mcp.secrets.deleteServer`). The secret value is never
logged. Each method takes an optional `configDir` (null selects the runtime's default).

```kotlin
val saved = client.mcpSecretsSave("my-server-token", System.getenv("MY_SERVER_TOKEN"))
println(if (saved.usedKeychain) "Stored in the OS keychain" else "Stored in the file backend")

client.mcpSecretsDelete("my-server-token")
client.mcpSecretsDeleteServer("my-server")   // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`permissions.extensionApprovals.grant`, `list` and `revoke`). `extensionApprovalsList` and
`extensionApprovalsRevoke` take an optional `sessionId` to use an attached session's configuration
profile.

```kotlin
// Persist a pending extension permission request (its requestId) before answering it normally
val grant = client.extensionApprovalsGrant(session.sessionId, requestId)
if (grant.granted) println("Approved ${grant.approval?.extensionName}")

client.extensionApprovalsList().approvals.forEach { approval ->
    // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
    if (client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey).revoked) {
        println("Revoked ${approval.extensionName}")
    }
}
```

### Ask-user image attachments

When the runtime accepts images in an answer, `UserInputRequest.imageAttachmentConstraints` carries
`allowedMimeTypes`, `maxBytesPerImage` and `maxCount`; it is null otherwise. The request also exposes
`requestId` and `meta` (the opaque `_meta` object) when the runtime sends them. Answer with
`UserInputResponse(..., attachments = listOf(...))`, where each `UserInputImageAttachment` holds
base64 `data`, a `mimeType`, and an optional `description` and `displayName` (the wire `type` is
always `"image"`). Attachments are sent only when set; image-only answers use `answer = ""` and
`wasFreeform = true`. The runtime enforces the limits.

```kotlin
SessionConfig(onUserInputRequest = { request, _ ->
    val constraints = request.imageAttachmentConstraints
    if (constraints != null && "image/png" in constraints.allowedMimeTypes) {
        val data = java.util.Base64.getEncoder().encodeToString(java.io.File("screenshot.png").readBytes())
        UserInputResponse(
            answer = "",
            wasFreeform = true,
            attachments = listOf(UserInputImageAttachment(data = data, mimeType = "image/png", displayName = "screenshot.png"))
        )
    } else {
        UserInputResponse(answer = "No screenshot available", wasFreeform = true)
    }
})
```

Elicitation handlers also receive `ElicitationContext.requestId` and `meta` (the opaque `_meta`
object); a `meta` set on the returned `ElicitationResult` is sent back to the runtime unchanged.

## What's New in v3.0.0

This release moves the SDK to **SDK protocol 4** (the protocol of upstream SDK 2.0).

- **Requires Copilot CLI 1.0.113 or newer.** Older CLIs are rejected at `start()` with
  `SDK protocol version mismatch: SDK supports version 4, but server reports version N.`
- **Existing options keep working.** `SessionConfig` and `ResumeSessionConfig` keep their flat
  properties; the SDK translates the payload into the grouped protocol 4 shape (`inference`,
  `tools`, `permissions`, `interaction`, `events`, `agents`, `mcp`, `github`, `state`, ...)
  right before `session.create` and `session.resume`.
- **Legacy BYOK keeps working.** `provider = ProviderConfig(...)` becomes a named `"default"`
  connection that is also the default provider, and Copilot API models are disabled for that
  session, exactly as in v1. Explicit `providers`/`defaultProvider`/`capi` win.
- **Protocol 4 tool and permission flow.** Custom tools and `onPermissionRequest` are served
  through the runtime's `external_tool.requested` and `permission.requested` events, answered
  with `session.tools.handlePendingToolCall` and `session.permissions.handlePendingPermissionRequest`.

### New in v3.0.0

```kotlin
// Named BYOK providers and model definitions (select a model as "provider/id")
val session = client.createSession(SessionConfig(
    model = "gateway/chat",
    providers = listOf(NamedProviderConfig(
        name = "gateway",
        baseUrl = "https://models.example.com/v1",
        apiKeyCommand = "print-model-key",
        wireApi = "responses"
    )),
    models = listOf(ProviderModelConfig(provider = "gateway", id = "chat", wireModel = "my-deployment", maxOutputTokens = 8192)),
    capi = CapiConfig(enabled = false)            // BYOK-only catalog
))

// Clear a default provider on resume
client.resumeSession(sessionId, ResumeSessionConfig(clearDefaultProvider = true, capi = CapiConfig(enabled = true)))
```

```kotlin
// Idempotent tools: the runtime may safely retry them
val lookup = defineTool("lookup_order") {
    description = "Look up an order by ID"
    idempotent = true
    handler = { _, _ -> "shipped" }
}

// Client turn IDs are echoed on the turn's events
session.send(MessageOptions(prompt = "Summarize the diff", clientTurnId = "turn-42"))

// Resume without emitting session.resume
client.resumeSession(sessionId, ResumeSessionConfig(suppressResumeEvent = true))
```

```kotlin
// Client mode: "coding-agent" (default) or "empty" (every session must pick its tools,
// and telemetry, repository hooks, cross-session memory, and similar features default off)
val client = CopilotClient(CopilotClientOptions(mode = ClientMode.EMPTY))
val session = client.createSession(SessionConfig(availableTools = listOf("view", "grep")))
```

```kotlin
// Live shell output events
session.on(ToolShellOutputData.EVENT_TYPE) { event ->
    ToolShellOutputData.fromEvent(event)?.let { print(it.text) }   // it.stream: "stdout", "stderr", or null
}

// Shell authentication: answer sudo/ssh prompts from programs that shell tools run
SessionConfig(onShellAuthenticationRequest = { request, _ ->
    if (request.inputKind == "confirm") ShellAuthenticationResult.Accept("yes") else ShellAuthenticationResult.Cancel
})

// Memory store: receive the memories the agent saves (requires memory enabled)
SessionConfig(memory = MemoryConfiguration(enabled = true, storeMemory = { memory ->
    myStore.save(memory.subject, memory.fact)
}))
```

```kotlin
// Network callbacks: proxy resolution and proxy authentication (never log credentials)
val client = CopilotClient(CopilotClientOptions(
    network = NetworkConfig(
        scope = NetworkScope.CONNECTION,
        resolveProxy = { request -> "PROXY proxy.corp.example:8080; DIRECT" },
        authenticateBasic = { request -> NetworkBasicCredentials(proxyUser, proxyPassword) }
    )
))
```

```kotlin
// Policy Helper failures on create/resume, and permission-mode refusal reasons
try {
    client.createSession(SessionConfig(permissionMode = PermissionMode.MANUAL))
} catch (e: Exception) {
    PolicyHelperFailure.fromError(e)?.let { println("${it.category}: ${it.message}") }
}

val result = session.setPermissionMode(PermissionMode.ALLOW_ALL)
if (!result.success && result.reason == PermissionModeRefusalReason.MANAGED_POLICY) {
    println("Blocked by managed policy; mode stays ${result.mode}")
}
```

### Also available (shipped in v2.6.0)

- **Client info handshake**: `CopilotClientOptions(clientInfo = ClientInfo(applicationName = "acme-portal", applicationVersion = "2.4.0"))`.
- **Auto routing tier**: `session.setAutoTier(AutoTier.BALANCE)` (sends `session.model.switchAutoTier`) or `SessionConfig(autoTier = ...)`.
- **Ask-user variant**: `SessionConfig(askUserVariant = AskUserVariant.ELICITATION)`.
- **GitHub token provider**: `SessionConfig(gitHubTokenProvider = { args -> GitHubTokenProviderResult.Token(fetchToken(args.host), expiresIn = 3600) })`.
- **Sandbox bypass**: `SessionConfig(sandboxConfig = SandboxConfig(enabled = true, allowBypass = true))`.
- **Feature flag overrides**: `SessionConfig(featureFlags = mapOf("my_flag" to true))`.
- **Session detach**: `session.destroy()` sends `session.detach`, so the session can be resumed later.
- **Message source**: `MessageOptions(prompt = "Run the checks", source = MessageSource.agent("planner"))`.
- **Managed settings cache clear**: `client.clearManagedSettingsCache()`.
- **Policy helper recheck**: `val snapshot = session.recheckPolicyHelper()` (parse `snapshot["policyHelperFailure"]` with `PolicyHelperFailure.fromJson`).
- **Model allowlist**: `SessionConfig(allowedModels = listOf("gpt-5", "claude-sonnet-4.5"))`.
- **OAuth client metadata URL**: `SessionConfig(authClientIdMetadataUrl = "https://example.com/oauth/client.json")`.
- **Max output tokens override**: `SessionConfig(modelCapabilities = mapOf("limits" to mapOf("max_output_tokens" to 8192)))`.
- **Structured output**: `val json = session.sendAndWaitStructured("List three colors", schema)` or `MessageOptions(prompt = ..., responseSchema = schema)`.
- **Agent/hook directories**: `SessionConfig(agentDirectories = listOf("./agents"), hookDirectories = listOf("./hooks"))`.
- **Working directory trust** (create only): `SessionConfig(trustWorkingDirectory = WorkingDirectoryTrust.SESSION)`.
- **Transcript recovery** (resume only): `ResumeSessionConfig(allowTranscriptRecovery = true)`.
- **Image generation config**: `SessionConfig(imageGeneration = ImageGenerationConfig(enabled = true, watermark = true, saveToDisk = false))`.
- **Autopilot options**: `SessionConfig(autopilot = AutopilotOptions(continuationEnabled = true, maxContinues = 3))`.
- **Subagent lifecycle hooks**: `SessionHooks(onSubagentStart = { input, _ -> mapOf("additionalContext" to "Follow the style guide") }, onSubagentStop = { _, _ -> null })`.
- **BYOK API key command**: `ProviderConfig(baseUrl = "https://models.example.com/v1", apiKeyCommand = "print-model-key")` or `NamedProviderConfig(..., apiKeyCommand = ...)`.

### Known limitations

- **AHP host**: the runtime-supervised AHP host (`startAhpHost` and the `ahp.*`/`host.*` RPC
  families) is not implemented. It is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
- **In-process hosting**: this SDK keeps spawning the CLI (or connecting over TCP) instead of
  hosting the runtime in-process by default; `CopilotClientOptions.inProcess` is unchanged.
- **Legacy response format**: `MessageOptions.responseFormat` is no longer sent, because protocol 4
  rejects it. Use `responseSchema` for structured output and `SessionConfig.imageGeneration` for images.

## Architecture

The SDK communicates with the Copilot CLI server using the JSON-RPC 2.0 protocol over stdio (default) or TCP. The CLI server is spawned as a subprocess with the flags `--headless --no-auto-update --log-level info --stdio`.

```
Your Application
    |
    v
CopilotClient  (manages CLI process lifecycle)
    |
    v
JsonRpcClient  (JSON-RPC 2.0 with Content-Length header framing)
    |
    v
CLI Process    (stdin/stdout pipes or TCP socket)
```

### Key Components

- **`CopilotClient`** - Main entry point. Spawns the CLI process, connects via stdio/TCP, manages sessions, and handles server requests (tool calls, permissions, user input, hooks).
- **`CopilotSession`** - Represents a conversation. Send messages, subscribe to events, register tools.
- **`JsonRpcClient`** - Low-level JSON-RPC 2.0 transport layer using Content-Length header framing, coroutines for async I/O, and `CompletableDeferred` for pending requests.

## Client Options

```kotlin
val client = CopilotClient(CopilotClientOptions(
    // Path to the CLI executable (default: "copilot" or COPILOT_CLI_PATH env)
    cliPath = "/usr/local/bin/copilot",

    // Extra arguments for the CLI
    cliArgs = listOf("--extra-flag"),

    // Working directory for the CLI process
    cwd = "/path/to/project",

    // Use stdio transport (default: true)
    useStdio = true,

    // Or connect to an existing server via TCP
    // cliUrl = "localhost:3000",

    // Log level: "none", "error", "warning", "info", "debug", "all"
    logLevel = "info",

    // Auto-start on first use (default: true)
    autoStart = true,

    // Auto-restart on crash (default: true)
    autoRestart = true,

    // GitHub token for authentication
    // githubToken = "ghp_...",
))
```

## Sessions

### Creating a Session

```kotlin
val session = client.createSession(SessionConfig(
    model = "gpt-4",
    workingDirectory = "/path/to/project",
    streaming = true,
))
```

### Resuming a Session

```kotlin
val session = client.resumeSession("session-id-123", ResumeSessionConfig(
    model = "gpt-4",
))
```

### Sending Messages

```kotlin
// Fire-and-forget (events come via on() handler)
val messageId = session.send(MessageOptions(prompt = "Hello!"))

// Send and wait for completion
val response = session.sendAndWait(
    MessageOptions(prompt = "Explain this code"),
    timeoutMs = 120_000  // 2 minute timeout
)
```

### Event Handling

```kotlin
// Listen to all events
val unsubscribe = session.on { event ->
    when (event.type) {
        "assistant.message" -> println("Assistant: ${event.data}")
        "assistant.message_delta" -> print(event.data?.get("deltaContent") ?: "")
        "tool.executing" -> println("Executing tool...")
        "session.idle" -> println("Session is idle")
        "session.error" -> println("Error: ${event.data}")
    }
}

// Listen to a specific event type
val unsub2 = session.on("assistant.message") { event ->
    println(event.data)
}

// Unsubscribe when done
unsubscribe()
unsub2()
```

## Custom Tools

### Using the DSL Builder

```kotlin
val weatherTool = defineTool("get_weather") {
    description = "Get weather for a location"
    parameters = mapOf(
        "type" to "object",
        "properties" to mapOf(
            "location" to mapOf("type" to "string", "description" to "City name")
        ),
        "required" to listOf("location")
    )
    handler = { args, invocation ->
        // args is a JsonElement from the server
        val location = (args as? kotlinx.serialization.json.JsonObject)
            ?.get("location")
            ?.let { (it as? kotlinx.serialization.json.JsonPrimitive)?.content }
            ?: "unknown"
        "72F and sunny in $location"
    }
}

val session = client.createSession(SessionConfig(
    tools = listOf(weatherTool)
))
```

### Using createTool

```kotlin
val tool = createTool(
    name = "add_numbers",
    description = "Add two numbers",
    parameters = mapOf(
        "type" to "object",
        "properties" to mapOf(
            "a" to mapOf("type" to "number"),
            "b" to mapOf("type" to "number")
        ),
        "required" to listOf("a", "b")
    )
) { args, _ ->
    val obj = args as? kotlinx.serialization.json.JsonObject
    val a = obj?.get("a")?.jsonPrimitive?.doubleOrNull ?: 0.0
    val b = obj?.get("b")?.jsonPrimitive?.doubleOrNull ?: 0.0
    a + b
}
```

### Structured Tool Results

Return a `ToolResultObject` for full control over the result:

```kotlin
handler = { args, _ ->
    ToolResultObject(
        textResultForLlm = "The operation completed successfully.",
        resultType = "success",
        toolTelemetry = mapOf("duration_ms" to kotlinx.serialization.json.JsonPrimitive(42))
    )
}
```

## Permission Handling

```kotlin
val session = client.createSession(SessionConfig(
    onPermissionRequest = { request, sessionId ->
        println("Permission requested: ${request.kind}")
        // Approve all shell commands for this example
        if (request.kind == "shell") {
            PermissionRequestResult(kind = "approved")
        } else {
            PermissionRequestResult(kind = "denied-interactively-by-user")
        }
    }
))
```

## User Input Handling

```kotlin
val session = client.createSession(SessionConfig(
    onUserInputRequest = { request, sessionId ->
        println("Agent asks: ${request.question}")
        if (request.choices != null) {
            println("Choices: ${request.choices}")
        }
        // Return a response
        UserInputResponse(
            answer = "yes",
            wasFreeform = false
        )
    }
))
```

## Session Hooks

```kotlin
val session = client.createSession(SessionConfig(
    hooks = SessionHooks(
        onPreToolUse = { input, sessionId ->
            println("About to use tool: ${input["toolName"]}")
            // Return null to proceed normally, or return a map with decisions
            mapOf("permissionDecision" to "allow")
        },
        onPostToolUse = { input, sessionId ->
            println("Tool finished: ${input["toolName"]}")
            null
        },
        onSessionStart = { input, sessionId ->
            println("Session started from: ${input["source"]}")
            null
        },
        onSessionEnd = { input, sessionId ->
            println("Session ended: ${input["reason"]}")
            null
        },
        onErrorOccurred = { input, sessionId ->
            println("Error occurred: ${input["error"]}")
            null
        }
    )
))
```

## Client Lifecycle Operations

```kotlin
// Ping the server
val ping = client.ping("health check")
println("Protocol version: ${ping.protocolVersion}")

// Get CLI status
val status = client.getStatus()
println("CLI version: ${status.version}")

// Get auth status
val auth = client.getAuthStatus()
println("Authenticated: ${auth.isAuthenticated}")

// List available models
val models = client.listModels()
models.forEach { println("${it.id}: ${it.name}") }

// List sessions
val sessions = client.listSessions()
sessions.forEach { println("${it.sessionId}: ${it.summary}") }

// Delete a session
client.deleteSession("session-id-123")

// Session lifecycle events
val unsub = client.onLifecycle { event ->
    println("Lifecycle: ${event.type} for session ${event.sessionId}")
}
val unsub2 = client.onLifecycle("session.created") { event ->
    println("New session: ${event.sessionId}")
}
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```kotlin
val client = CopilotClient(CopilotClientOptions(sessionIdleTimeoutSeconds = 300))
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```kotlin
val client = CopilotClient(CopilotClientOptions(
    sessionFs = SessionFsConfig(initialCwd = "/repo", sessionStatePath = "/tmp/state", conventions = "posix")
))
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```kotlin
val meta = client.getSessionMetadata("session-123")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```kotlin
val session = client.createSession(SessionConfig(
    skillDirectories = listOf("./skills"),
    disabledSkills = listOf("test-skill"),
    includeSubAgentStreamingEvents = true
))
```

- `skillDirectories` - Paths to directories containing skill definitions
- `disabledSkills` - Skills to exclude from the session
- `includeSubAgentStreamingEvents` - When `true`, receive streaming events from sub-agents

## Custom Provider (BYOK)

```kotlin
val session = client.createSession(SessionConfig(
    provider = ProviderConfig(
        type = "openai",
        baseUrl = "https://api.openai.com/v1",
        apiKey = "sk-..."
    ),
    model = "gpt-4"
))
```

## MCP Server Integration

```kotlin
val session = client.createSession(SessionConfig(
    mcpServers = mapOf(
        "my-server" to MCPServerConfig(
            tools = listOf("*"),
            command = "node",
            args = listOf("./mcp-server.js"),
        )
    )
))
```

## Protocol Details

- **Transport**: stdio (default) or TCP
- **Framing**: Content-Length header (LSP base protocol)
- **Protocol**: JSON-RPC 2.0
- **Protocol Version**: 4 (verified via ping on connection; requires Copilot CLI 1.0.113 or newer)

### Message Flow

1. Client spawns CLI with `--headless --no-auto-update --log-level info --stdio`
2. Client verifies protocol version via `ping` response
3. Client creates sessions via `session.create` request
4. Client sends messages via `session.send` request
5. Server sends `session.event` notifications with events
6. Server sends `tool.call` requests for custom tool execution
7. Server sends `permission.request` requests for permission decisions
8. Server sends `userInput.request` requests for user input
9. Server sends `hooks.invoke` requests for hook execution
10. Server sends `session.lifecycle` notifications for session state changes

## Image Generation

Opt in to image generation for the session with `SessionConfig.imageGeneration`. Per-message
`imageOptions` are still forwarded; the legacy `responseFormat` enum is no longer sent because
protocol 4 rejects it (use `responseSchema` for structured output).

```kotlin
val session = client.createSession(SessionConfig(
    imageGeneration = ImageGenerationConfig(enabled = true, saveToDisk = false)
))
val response = session.sendAndWait(MessageOptions(
    prompt = "Generate a sunset over mountains",
    imageOptions = ImageOptions(size = "1024x1024", quality = "hd", style = "natural")
))
```

## Recent Features (v2.4–v2.5)

Parity options tracking the upstream `@github/copilot` SDK, exposed as constructor
parameters on the `SessionConfig`, `CopilotClientOptions`, `MessageOptions`, and
`SessionHooks` data classes.

### v2.5.0

- **Reasoning effort** — `SessionConfig(reasoningEffort = ReasoningEffort.HIGH)` (`LOW`/`MEDIUM`/`HIGH`/`XHIGH`).
- **Session rewind** — `SessionConfig(rewindEnabled = true)`; roll back to an earlier snapshot (`session.snapshot_rewind` event).
- **Additional directories** — `SessionConfig(additionalDirectories = ...)`; extra workspace roots the agent may read.
- **Content exclusion** — `SessionConfig(contentExclusion = true)`; honor repository content-exclusion rules.
- **Tool search** — `SessionConfig(toolSearch = ...)`; on-demand tool discovery for large tool sets.
- **Disabled MCP servers** — `SessionConfig(disabledMcpServers = ...)`.
- **GitHub MCP tool config** — `SessionConfig(githubMcpToolConfig = ...)`.
- **Canvas provider** — `SessionConfig(canvasProvider = ...)`.
- **Custom agents local-only** — `SessionConfig(customAgentsLocalOnly = true)`.
- **Experimental mode** — `SessionConfig(experimentalMode = true)`.
- **User-prompt-transformed hook** — `SessionHooks(onUserPromptTransformed = ...)`.
- **Built-in plugin directories** — `CopilotClientOptions(builtinPluginDirectories = ...)`.
- **In-process (FFI) transport** — `CopilotClientOptions(inProcess = true)`.
- **Agent factory args schema** — custom agents accept a typed `argsSchema`.
- **Permission decision context** — permission results carry `decisionContext`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions(bearerTokenProvider = ...)`; a `suspend (ProviderTokenArgs) -> String`.
- **HTTP request handler** — `CopilotClientOptions(requestHandler = ...)`.
- **MCP OAuth token handler** — `SessionConfig(onMcpAuthRequest = ...)`.
- **Session citations** — `SessionConfig(enableCitations = true)`.
- **Excluded built-in agents** — `SessionConfig(excludedBuiltinAgents = ...)`.
- **Session spending limits** — `SessionConfig(sessionLimits = SessionLimitsConfig(maxAiCredits = ...))`.
- **Session memory** — `SessionConfig(memory = MemoryConfiguration(enabled = true))`.
- **OTLP protocol** — `SessionConfig(otlpProtocol = "grpc")`.
- **WebSocket responses** — `SessionConfig(enableWebSocketResponses = true)`.
- **Experiment assignments** — `SessionConfig(expAssignments = ...)`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `SessionHooks(onPostToolUse = ..., onPreMcpToolCall = ...)`.
- **Per-message agent mode & display prompt** — `MessageOptions(agentMode = ..., displayPrompt = ...)`.
- **System-message sections** — `SystemMessageSection.PREAMBLE` / `.PRESERVE`.
- **Tool defer policy** — `ToolDefer.AUTO` / `.NEVER`.
- **GitHub attachment variants** — `GitHubAttachment.GITHUB_COMMIT` / `.GITHUB_REPOSITORY`.

### Examples

```kotlin
// Reasoning effort — trade latency for depth on models that support it
val session = client.createSession(SessionConfig(
    model = "gpt-5",
    reasoningEffort = ReasoningEffort.HIGH   // LOW | MEDIUM | HIGH | XHIGH
))
```

```kotlin
// Roll back to an earlier snapshot, expose extra roots, and honor content exclusion
val session = client.createSession(SessionConfig(
    rewindEnabled = true,                    // emits session.snapshot_rewind events
    additionalDirectories = listOf("/repo/shared", "/repo/docs"),
    contentExclusion = true
))
```

```kotlin
// On-demand tool discovery for large tool sets
val session = client.createSession(SessionConfig(
    toolSearch = mapOf("enabled" to true)
))
```

```kotlin
// BYOK bearer tokens + in-process FFI transport (client-level)
val client = CopilotClient(CopilotClientOptions(
    bearerTokenProvider = { args -> fetchToken(args.sessionId) },
    builtinPluginDirectories = listOf("/opt/copilot/plugins"),
    inProcess = true
))
```

## License

MIT License. See [LICENSE](../LICENSE) for details.
