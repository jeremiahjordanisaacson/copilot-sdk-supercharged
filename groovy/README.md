# GitHub Copilot Supercharged SDK for Groovy

Groovy SDK for programmatic control of GitHub Copilot CLI via JSON-RPC.

[![Groovy 4.0+](https://img.shields.io/badge/Groovy-4.0%2B-blue?logo=apachegroovy&logoColor=white)](https://groovy-lang.org/)
[![Java 17+](https://img.shields.io/badge/Java-17%2B-blue?logo=openjdk&logoColor=white)](https://openjdk.org/)

## Requirements

- Java 17+
- Groovy 4.0+ (included via Gradle)
- Gradle 8+ (for building)

## Installation

The Groovy SDK is not published to Maven Central yet, so build it from source and publish it to your local Maven repository (`~/.m2`):

```bash
git clone https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git
cd copilot-sdk-supercharged/groovy
gradle publishToMavenLocal
```

### Gradle

```groovy
repositories {
    mavenLocal()
    mavenCentral()
}

dependencies {
    implementation 'io.github.jeremiahjordanisaacson:copilot-sdk-groovy:3.1.0'
}
```

### From Source

```bash
cd groovy
gradle build
```

## Quick Start

> **Note:** This SDK is in public preview and may change in breaking ways.

```groovy
import com.github.copilot.*
import com.github.copilot.Types.*

// Create and auto-start client
def client = new CopilotClient()

try {
    def session = client.createSession(model: 'gpt-4o')

    // Handle events with closures
    session.on { event ->
        if (event.type == 'assistant.message') {
            println event.data?.content
        }
    }

    // Send and wait for response
    def result = session.sendAndWait('What is 2+2?')
    println "Answer: ${result?.data?.content}"

    session.destroy()
} finally {
    client.stop()
}
```

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `CopilotClient` methods below call
server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC
method-not-found error that surfaces like any other RPC error
(`JsonRpcClient.isMethodNotFound(e, 'mcp.secrets.save')` returns true).

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store. The secret
value is never logged. Each method takes an optional `configDir` (the runtime's default when omitted).

```groovy
McpSecretsSaveResult saved = client.mcpSecretsSave('my-server-token', System.getenv('MY_SERVER_TOKEN'))
println(saved.usedKeychain ? 'Stored in the OS keychain' : 'Stored in the file backend')

client.mcpSecretsDelete('my-server-token')       // mcp.secrets.delete
client.mcpSecretsDeleteServer('my-server')       // mcp.secrets.deleteServer: every credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`PermissionsExtensionApproval`: `extensionName`, `pluginName`, `capabilities`, `grantedAt`,
`installationKey`, `installationLabel`). `extensionApprovalsList` and `extensionApprovalsRevoke`
take an optional `sessionId` to use an attached session's configuration profile.

```groovy
// Persist a pending extension permission request before answering it normally
def grant = client.extensionApprovalsGrant(session.sessionId, requestId)
if (grant.granted) println "Approved ${grant.approval.extensionName}"

client.extensionApprovalsList().approvals.each { PermissionsExtensionApproval approval ->
    client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey)
}
```

### Ask-user image attachments

When the runtime accepts images in an answer, the `UserInputRequest` passed to `onUserInputRequest`
carries `imageAttachmentConstraints` (`UserInputImageAttachmentConstraints`: `allowedMimeTypes`,
`maxBytesPerImage`, `maxCount`); it is null otherwise. The request also exposes `requestId` and
`meta` (the opaque `_meta` object) when sent. Answer with
`new UserInputResponse(answer, wasFreeform, attachments)`, where `attachments` is a list of
`UserInputImageAttachment` (base64 `data`, `mimeType`, optional `description` and `displayName`;
the wire `type` is always `'image'`) that is sent only when set. Image-only answers use an empty
`answer` with `wasFreeform` true; the runtime enforces the limits.

```groovy
def session = client.createSession(onUserInputRequest: { UserInputRequest request, String sessionId ->
    if (request.imageAttachmentConstraints?.allowedMimeTypes?.contains('image/png')) {
        def image = new UserInputImageAttachment(
            data: new File('screenshot.png').bytes.encodeBase64().toString(),
            mimeType: 'image/png',
            displayName: 'screenshot.png')
        new UserInputResponse('', true, [image])
    } else {
        new UserInputResponse('No screenshot available', true)
    }
})
```

Elicitation handlers (`onElicitationRequest`) now also receive the pending `requestId` and the
request's opaque `meta` (`_meta`) on `ElicitationContext`; set `ElicitationResult.meta` to send
`_meta` back unchanged.

## What's New in v3.0.0

This release moves the SDK to **SDK protocol 4** (the protocol of upstream SDK 2.0).

* **Requires Copilot CLI 1.0.113 or newer.** Older CLIs are rejected at `start()` with
  `SDK protocol version mismatch: SDK supports version 4, but server reports version N.`
* **Existing options keep working.** You still pass the same flat named arguments to
  `createSession`/`resumeSession`; the SDK translates the payload into the grouped protocol 4
  shape (`inference`, `tools`, `permissions`, `interaction`, `events`, `agents`, `mcp`, `github`,
  `state`, ...) right before `session.create` and `session.resume`.
* **Legacy BYOK keeps working.** `provider: new ProviderConfig(...)` becomes a named `'default'`
  connection that is also the default provider, and Copilot API models are disabled for that
  session, exactly as in v1. Explicit `providers`/`defaultProvider`/`capi` win.
* **Protocol 4 tool and permission flow.** Custom tools and `onPermissionRequest` are served
  through the runtime's `external_tool.requested` and `permission.requested` events, answered
  with `session.tools.handlePendingToolCall` and `session.permissions.handlePendingPermissionRequest`.

### New in v3.0.0

```groovy
import com.github.copilot.*
import com.github.copilot.Types.*

// Named BYOK providers and model definitions (select a model as "provider/id")
def session = client.createSession(
    model: 'gateway/chat',
    providers: [new NamedProviderConfig(name: 'gateway', baseUrl: 'https://models.example.com/v1',
                                        apiKeyCommand: 'print-model-key', wireApi: 'responses')],
    models: [new ProviderModelConfig(provider: 'gateway', id: 'chat', wireModel: 'my-deployment', maxOutputTokens: 8192)],
    capi: new CapiConfig(enabled: false)            // BYOK-only catalog
)

// Use a connection for unqualified model names; defaultProvider: null clears it on resume
def resumed = client.resumeSession(session.sessionId, defaultProvider: null, capi: [enabled: false])
```

```groovy
// Idempotent tools: the runtime may safely retry them
def lookup = DefineTool.create('lookup_order')
    .description('Look up an order by ID')
    .parameters([type: 'object', properties: [id: [type: 'string']], required: ['id']])
    .idempotent(true)
    .handler { args, invocation -> "order ${args.id} shipped" }
    .build()

// Client turn IDs are echoed on the turn's events
session.send(prompt: 'Summarize the diff', clientTurnId: 'turn-42')

// Resume without emitting session.resume
client.resumeSession(sessionId, suppressResumeEvent: true, continuePendingWork: false)
```

```groovy
// Client mode: 'coding-agent' (default) or 'empty' (every session must pick its tools, and
// telemetry, repository hooks, cross-session memory, and similar features default off)
def client = new CopilotClient(mode: ClientMode.EMPTY)
def session = client.createSession(availableTools: ['view', 'grep'])
```

```groovy
// Live shell output events
session.on(ToolShellOutputData.EVENT_TYPE) { SessionEvent event ->
    def out = ToolShellOutputData.fromEvent(event)
    print out.text                                  // out.stream is 'stdout', 'stderr', or null
}

// Shell authentication: answer sudo/ssh prompts from programs that shell tools run
client.createSession(onShellAuthenticationRequest: { ShellAuthenticationRequest request, String sessionId ->
    request.inputKind == 'confirm' ? ShellAuthenticationResult.accept('yes') : ShellAuthenticationResult.cancel()
})

// Memory store: receive the memories the agent saves (requires memory enabled)
client.createSession(memory: new MemoryConfiguration(enabled: true,
    storeMemory: { StoredMemory memory -> myStore.save(memory.subject, memory.fact) }))
```

```groovy
// Network callbacks: proxy resolution and proxy authentication (never log credentials)
def client = new CopilotClient(network: [
    scope            : NetworkScope.CONNECTION,
    resolveProxy     : { NetworkResolveProxyRequest request -> 'PROXY proxy.corp.example:8080; DIRECT' },
    authenticateBasic: { NetworkAuthenticateBasicRequest request -> new NetworkBasicCredentials(proxyUser, proxyPassword) }
])
```

```groovy
// Policy Helper failures on create/resume, and permission-mode refusal reasons
try {
    client.createSession(permissionMode: PermissionMode.MANUAL)
} catch (Exception e) {
    def failure = PolicyHelperFailure.fromError(e)
    if (failure) System.err.println("${failure.category}: ${failure.message}")
}

PermissionModeResult result = session.setPermissionMode(PermissionMode.ALLOW_ALL)
if (!result.success && result.reason == PermissionModeRefusalReason.MANAGED_POLICY) {
    println "Blocked by managed policy; mode stays ${result.mode}"
}
```

### Also available (shipped in v2.6.0)

* **Client info handshake**: `new CopilotClient(clientInfo: new ClientInfo(applicationName: 'acme-portal', applicationVersion: '2.4.0', integrationName: 'acme-plugin'))`.
* **Auto routing tier**: `session.setAutoTier(AutoTier.BALANCE)` (sends `session.model.switchAutoTier`) or `createSession(autoTier: AutoTier.BALANCE)`.
* **Ask-user variant**: `createSession(askUserVariant: AskUserVariant.ELICITATION)`.
* **GitHub token provider**: `createSession(gitHubTokenProvider: { GitHubTokenProviderArgs args -> GitHubTokenProviderResult.token(fetchToken(args.host), 28800L) })` (served via `gitHubToken.getToken`; mutually exclusive with `gitHubToken`).
* **Sandbox bypass**: `createSession(sandboxConfig: new SandboxConfig(enabled: true, allowBypass: true))`.
* **Feature flag overrides**: `createSession(featureFlags: [my_flag: true])`.
* **Session detach**: `session.destroy()` sends `session.detach`, so the session can be resumed later.
* **Message source**: `session.send(prompt: 'Run the checks', source: MessageSource.agent('planner'))`.
* **Managed settings cache clear**: `client.clearManagedSettingsCache()`.
* **Policy helper recheck**: `def snapshot = session.recheckPolicyHelper()` (parse `snapshot.policyHelperFailure` with `PolicyHelperFailure.fromMap`).
* **Model allowlist**: `createSession(allowedModels: ['gpt-5', 'claude-sonnet-4.5'])`.
* **OAuth client metadata URL**: `createSession(authClientIdMetadataUrl: 'https://example.com/oauth/client.json')`.
* **Max output tokens override**: `createSession(modelCapabilities: [limits: [max_output_tokens: 8192]])` or `new ModelCapabilities(limits: new ModelCapabilities.ModelLimits(maxOutputTokens: 8192))`.
* **Structured output**: `session.sendAndWait(prompt: 'List three colors', responseSchema: [type: 'object', properties: [colors: [type: 'array', items: [type: 'string']]]])`.
* **Agent/hook directories**: `createSession(agentDirectories: ['./agents'], hookDirectories: ['./hooks'])`.
* **Working directory trust** (create only): `createSession(trustWorkingDirectory: WorkingDirectoryTrust.SESSION)`.
* **Transcript recovery** (resume only): `client.resumeSession(sessionId, allowTranscriptRecovery: true)`.
* **Image generation config**: `createSession(imageGeneration: new ImageGenerationConfig(enabled: true, watermark: true, saveToDisk: false))`.
* **Autopilot options**: `createSession(autopilot: new AutopilotOptions(continuationEnabled: true, maxContinues: 3))`.
* **Subagent lifecycle hooks**: `createSession(hooks: [onSubagentStart: { input, sessionId -> [additionalContext: 'Follow the style guide'] }, onSubagentStop: { input, sessionId -> null }])` (served via `hooks.invoke`).
* **BYOK API key command**: `new ProviderConfig(type: 'openai', baseUrl: url, apiKeyCommand: 'print-model-key')`, or `NamedProviderConfig.apiKeyCommand`.

### Known limitations

* **AHP host**: the runtime-supervised AHP host (`startAhpHost` and the `ahp.*`/`host.*` RPC
  families) is not implemented. It is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
* **In-process hosting**: this SDK keeps spawning the CLI (or connecting over TCP) instead of
  hosting the runtime in-process by default; the `inProcess` option is unchanged.
* **Legacy response format**: the string `responseFormat` send option (`'text'`, `'image'`,
  `'json_object'`) is no longer sent, because protocol 4 rejects the string form. Use
  `responseSchema` for structured output and the `imageGeneration` session option for images.
* **Callback cancellation**: Groovy closures receive no cancellation token. When the runtime
  cancels a shell authentication prompt (the SDK answers `cancel`) or completes an external tool
  call elsewhere (no answer is sent), the SDK interrupts the closure's thread.

## Custom Tools

Define tools using the builder pattern with Groovy closures:

```groovy
def weatherTool = DefineTool.create('get_weather')
    .description('Get weather for a location')
    .parameters([
        type      : 'object',
        properties: [
            location: [type: 'string', description: 'City name']
        ],
        required  : ['location']
    ])
    .handler { args, invocation ->
        def city = (args as Map).location
        "72F in ${city}"
    }
    .build()

def session = client.createSession(tools: [weatherTool])
```

## Connecting to an External Server

```groovy
def client = new CopilotClient(cliUrl: 'localhost:3000')
```

## Streaming

```groovy
def session = client.createSession(streaming: true)

session.on('assistant.message_delta') { event ->
    print event.data?.deltaContent ?: ''
}

session.sendAndWait('Write a haiku about Groovy')
```

## Custom Provider (BYOK)

```groovy
import com.github.copilot.Types.*

def provider = new ProviderConfig().with {
    type = 'openai'
    baseUrl = 'http://localhost:11434/v1'
    it
}

def session = client.createSession(
    model: 'llama3',
    provider: provider
)
```

## Permission Handling

```groovy
def session = client.createSession(
    onPermissionRequest: { req, sessionId ->
        println "Permission requested: ${req.kind}"
        new PermissionRequestResult('approved')
    }
)
```

## Persisting and Resuming Sessions

```groovy
// Create a session
def session = client.createSession()
def sessionId = session.sessionId

// ... later, resume it
def resumed = client.resumeSession(sessionId)
```

## Infinite Sessions

```groovy
def infiniteConfig = new InfiniteSessionConfig().with {
    enabled = true
    backgroundCompactionThreshold = 0.8
    it
}

def session = client.createSession(infiniteSessions: infiniteConfig)
println "Workspace: ${session.workspacePath}"
```

## Image Generation

Opt in to image generation for the session with the `imageGeneration` option. Per-message
`imageOptions` are still forwarded; the legacy `responseFormat` string is no longer sent because
protocol 4 rejects it (use `responseSchema` for structured output).

```groovy
def session = client.createSession(imageGeneration: new ImageGenerationConfig(enabled: true, saveToDisk: false))
def result = session.sendAndWait(
    prompt: 'Generate a sunset over mountains',
    imageOptions: new ImageOptions('1024x1024', 'hd', 'natural')
)
```

## Recent Features (v2.4–v2.5)

Parity options tracking the upstream `@github/copilot` SDK. Session options are passed
as named-argument maps to `createSession`; client options to the `CopilotClient` constructor.

### v2.5.0

- **Reasoning effort** — `createSession(reasoningEffort: 'high')` (`'low'`/`'medium'`/`'high'`/`'xhigh'`; see `ReasoningEffort`).
- **Session rewind** — `createSession(rewindEnabled: true)`; roll back to an earlier snapshot (`session.snapshot_rewind` event).
- **Additional directories** — `createSession(additionalDirectories: [...])`; extra workspace roots the agent may read.
- **Content exclusion** — `createSession(contentExclusion: true)`; honor repository content-exclusion rules.
- **Tool search** — `createSession(toolSearch: [...])`; on-demand tool discovery for large tool sets.
- **Disabled MCP servers** — `createSession(disabledMcpServers: [...])`.
- **GitHub MCP tool config** — `createSession(githubMcpToolConfig: [...])`.
- **Canvas provider** — `createSession(canvasProvider: [...])`.
- **Custom agents local-only** — `createSession(customAgentsLocalOnly: true)`.
- **Experimental mode** — `createSession(experimentalMode: true)`.
- **User-prompt-transformed hook** — `createSession(hooks: [onUserPromptTransformed: { input, sessionId -> ... }])`.
- **Built-in plugin directories** — `new CopilotClient(builtinPluginDirectories: [...])`.
- **In-process (FFI) transport** — `new CopilotClient(inProcess: true)`.
- **Agent factory args schema** — custom agents accept a typed `argsSchema`.
- **Permission decision context** — `PermissionRequestResult.decisionContext`.

### v2.4.0

- **BYOK bearer token provider** — `new CopilotClient(bearerTokenProvider: { args -> ... })`; a closure receiving `ProviderTokenArgs`.
- **HTTP request handler** — `new CopilotClient(requestHandler: { ... })`.
- **MCP OAuth token handler** — `createSession(onMcpAuthRequest: { ... })`.
- **Session citations** — `createSession(enableCitations: true)`.
- **Excluded built-in agents** — `createSession(excludedBuiltinAgents: [...])`.
- **Session spending limits** — `createSession(sessionLimits: new SessionLimitsConfig(maxAiCredits: ...))`.
- **Session memory** — `createSession(memory: new MemoryConfiguration(enabled: true))`.
- **OTLP protocol** — `createSession(otlpProtocol: 'grpc')`.
- **WebSocket responses** — `createSession(enableWebSocketResponses: true)`.
- **Experiment assignments** — `createSession(expAssignments: [...])`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `hooks: [onPostToolUse: {...}, onPreMcpToolCall: {...}]`.
- **Per-message agent mode & display prompt** — `send(agentMode: ..., displayPrompt: ...)`.
- **System-message sections** — `CopilotClient.SystemMessageSection.PREAMBLE` / `.PRESERVE`.
- **Tool defer policy** — `CopilotClient.ToolDefer.AUTO` / `.NEVER` (`Tool.defer`).
- **GitHub attachment variants** — `CopilotClient.GitHubAttachment.GITHUB_COMMIT` / `.GITHUB_REPOSITORY`.

### Examples

```groovy
// Reasoning effort — trade latency for depth on models that support it
def session = client.createSession(
    model: 'gpt-5',
    reasoningEffort: 'high'            // 'low' | 'medium' | 'high' | 'xhigh'
)
```

```groovy
// Roll back to an earlier snapshot, expose extra roots, and honor content exclusion
def session = client.createSession(
    rewindEnabled: true,              // emits session.snapshot_rewind events
    additionalDirectories: ['/repo/shared', '/repo/docs'],
    contentExclusion: true
)
```

```groovy
// On-demand tool discovery for large tool sets
def session = client.createSession(toolSearch: [enabled: true])
```

```groovy
// BYOK bearer tokens + in-process FFI transport (client-level)
def client = new CopilotClient(
    bearerTokenProvider: { args -> fetchToken(args.sessionId) },
    builtinPluginDirectories: ['/opt/copilot/plugins'],
    inProcess: true
)
```

## API Reference

### CopilotClient

| Method | Description |
|--------|-------------|
| `start()` | Start CLI server and connect |
| `stop()` | Graceful shutdown |
| `forceStop()` | Force shutdown |
| `createSession(config)` | Create a new session |
| `resumeSession(id, config)` | Resume an existing session (also `resumeSession(id, name: value)`) |
| `listSessions()` | List all sessions |
| `deleteSession(id)` | Delete a session |
| `ping(message)` | Ping the server |
| `listModels()` | List available models |
| `getState()` | Get connection state |
| `getStatus()` | Get CLI status |
| `getAuthStatus()` | Get auth status |
| `clearManagedSettingsCache()` | Clear cached enterprise managed settings |
| `mcpSecretsSave(secretId, value, configDir)` | Save an MCP credential (`mcp.secrets.save`); returns `McpSecretsSaveResult` |
| `mcpSecretsDelete(secretId, configDir)` | Delete an MCP credential (`mcp.secrets.delete`) |
| `mcpSecretsDeleteServer(serverName, configDir)` | Delete a server's MCP credentials (`mcp.secrets.deleteServer`) |
| `extensionApprovalsGrant(sessionId, requestId)` | Persist a pending plugin extension approval (`permissions.extensionApprovals.grant`) |
| `extensionApprovalsList(sessionId)` | List plugin extension approvals (`permissions.extensionApprovals.list`) |
| `extensionApprovalsRevoke(extensionName, installationKey, sessionId)` | Revoke a plugin extension approval (`permissions.extensionApprovals.revoke`) |
| `on(handler)` | Subscribe to lifecycle events |
| `on(type, handler)` | Subscribe to specific lifecycle event |

### CopilotSession

| Method | Description |
|--------|-------------|
| `send(prompt)` | Send a message |
| `send(options)` | Send with full options map |
| `sendAndWait(prompt)` | Send and wait for idle |
| `sendAndWait(options)` | Send with options and wait |
| `on(handler)` | Subscribe to all events |
| `on(type, handler)` | Subscribe to specific event type |
| `getMessages()` | Get conversation history |
| `getMetadata()` | Get session metadata |
| `abort()` | Abort current processing |
| `setAutoTier(tier)` | Change the Auto routing preference |
| `setPermissionMode(mode)` | Change the permission mode (`PermissionModeResult`) |
| `getManagedSettings()` | Get the managed settings snapshot |
| `recheckPolicyHelper()` | Re-run the Policy Helper and get the snapshot |
| `destroy()` | Detach from the session (state is kept for resume) |

### DefineTool

| Method | Description |
|--------|-------------|
| `create(name)` | Start building a tool |
| `description(desc)` | Set tool description |
| `parameters(schema)` | Set JSON Schema parameters |
| `idempotent(flag)` | Declare that the runtime may safely retry the tool |
| `handler(closure)` | Set handler closure |
| `build()` | Build the Tool instance |

## Groovy Idioms Used

This SDK leverages Groovy-specific features throughout:

- **Closures** for event handlers, tool handlers, and permission callbacks
- **GString** interpolation (`"Hello ${name}"`) in messages and error reporting
- **Map literals** for configuration (`createSession(model: 'gpt-4o', streaming: true)`)
- **`@CompileStatic`** for type-safe, performant core classes
- **`.with{}`** builder pattern for fluent object construction
- **Safe navigation** (`?.`) for null-safe property access in event data
- **Spread operator** (`*.name`) for extracting properties from collections
- **`JsonSlurper` / `JsonOutput`** instead of external JSON libraries
- **`@ToString`** for automatic debug-friendly string representations

## Cookbook

See the [cookbook/](cookbook/) directory for practical recipes:

- [Tools and Skills](cookbook/tools-and-skills.md)
- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Streaming](cookbook/streaming.md)
- [Custom Providers](cookbook/custom-providers.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Advanced Features](cookbook/advanced-features.md)

## Building

```bash
cd groovy
gradle build
gradle test
```

## Contributing

Contributions are welcome! Please see the [Contributing Guide](../CONTRIBUTING.md).

## License

MIT
