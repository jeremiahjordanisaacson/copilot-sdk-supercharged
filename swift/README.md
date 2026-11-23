# Copilot Supercharged SDK for Swift

A Swift SDK for interacting with the GitHub Copilot CLI via JSON-RPC 2.0 over stdio.

## Requirements

- Swift 5.9+
- macOS 13+ / iOS 16+ / tvOS 16+ / watchOS 9+
- GitHub Copilot CLI 1.0.113 or newer (SDK protocol 4) installed and available in PATH (or provide a custom path)

## Installation

### Swift Package Manager

Add the following to your `Package.swift` dependencies:

```swift
.package(url: "https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged", from: "1.0.0")
```

Then add `"CopilotSDK"` as a dependency of your target:

```swift
.target(
    name: "MyApp",
    dependencies: ["CopilotSDK"]
)
```

## Quick Start

```swift
import CopilotSDK

// Create a client (auto-starts the CLI server)
let client = CopilotClient()

// Create a session
let session = try await client.createSession()

// Subscribe to events
await session.on { event in
    if event.type == "assistant.message" {
        print(event.data["content"] as? String ?? "")
    }
}

// Send a message and wait for the response
let response = try await session.sendAndWait(
    MessageOptions(prompt: "Explain what a Swift actor is")
)
print(response?.data["content"] as? String ?? "No response")

// Clean up
try await session.destroy()
await client.stop()
```

## What's New in v3.0.0

SDK protocol 4 support (the upstream SDK 2.0 wire format). **Requires Copilot CLI 1.0.113
or newer**: `start()` rejects older CLIs with
`SDK protocol version mismatch: SDK supports version 4, but server reports version N.`

Existing options keep working. You still pass the flat `SessionConfig` /
`ResumeSessionConfig` options; right before `session.create` / `session.resume` the client
groups them into the protocol 4 payload (`streaming` becomes `events.streaming`,
`onPermissionRequest` becomes `permissions.request`, `skillDirectories` becomes
`skills.directories`, and so on). A legacy `provider` is sent as a `"default"` named
provider with `inference.defaultProvider = "default"` and Copilot API models disabled
(`github.capi.enabled = false`), so v2.x BYOK sessions behave as before. `destroy()` now
sends `session.detach` (falling back to `session.destroy` on older runtimes), and server
requests are answered concurrently so a slow tool or prompt never blocks the connection.

```swift
import CopilotSDK

let client = CopilotClient(options: CopilotClientOptions(
    // Client info: sent as `clientInfo` on the `connect` handshake.
    clientInfo: ClientInfo(
        editorName: "acme-portal",
        editorVersion: "2.4.0",
        extensionName: "acme-copilot",
        extensionVersion: "0.9.0"
    ),
    // Client mode: .codingAgent (default) or .empty; .copilotCli is an alias of .codingAgent.
    mode: .codingAgent,
    // Network callbacks: the client calls `network.setProvider` after connecting and
    // answers `network.resolveProxy` with this callback.
    network: NetworkConfig(
        resolveProxy: { url in url.contains("corp.example") ? "PROXY proxy.corp.example:8080" : nil }
    )
))
try await client.start()

let session = try await client.createSession(SessionConfig(
    // Named BYOK providers: select a registered model with "<provider>/<id>".
    model: "local/qwen3",
    // Idempotent tools: `idempotent` is sent in tools.definitions[].
    tools: [
        defineTool(name: "lookup_ticket", description: "Look up a ticket", idempotent: true) { _ in
            ["status": "open"]
        },
    ],
    // Subagent lifecycle hooks (`subagentStart` / `subagentStop`).
    hooks: SessionHooks(
        onSubagentStart: { input, _ in
            SubagentStartHookOutput(additionalContext: "Focus for \(input.agentName): security review")
        },
        onSubagentStop: { input, _ in
            SubagentStopHookOutput(modifiedResponse: input.response)
        }
    ),
    workingDirectory: "/repo",
    // Max output tokens override (`limits.max_output_tokens`).
    modelCapabilities: ModelCapabilitiesOverride(
        limits: ModelLimitsOverride(maxOutputTokens: 8192)
    ).toDictionary(),
    // Memory store: answers `memoryStore.store` (sends state.hasMemoryStore).
    memory: MemoryConfiguration(storeMemory: { memory in
        print("remember \(memory.subject): \(memory.fact)")
    }),
    name: "ticket-triage",
    askUserVariant: .elicitation,
    sandboxConfig: SandboxConfig(enabled: true, allowBypass: true),
    featureFlags: ["someRuntimeFlag": true],
    allowedModels: ["local/qwen3", "gpt-5"],
    authClientIdMetadataUrl: "https://example.com/oauth/client.json",
    agentDirectories: ["./.github/agents"],
    hookDirectories: ["./.github/hooks"],
    trustWorkingDirectory: WorkingDirectoryTrust.session,
    imageGeneration: ImageGenerationConfig(enabled: true, watermark: true, saveToDisk: true),
    autopilot: AutopilotOptions(continuationEnabled: true, maxContinues: 5),
    providers: [
        NamedProviderConfig(
            name: "local",
            baseUrl: "http://localhost:11434/v1",
            type: "openai",
            apiKeyCommand: "op read op://dev/llm/api-key"
        ),
    ],
    models: [ProviderModelConfig(provider: "local", id: "qwen3", maxOutputTokens: 8192)],
    defaultProvider: "local",
    capi: CapiSessionOptions(enabled: false),
    // Shell authentication: answers `shellAuthentication.request` prompts.
    onShellAuthenticationRequest: { request in
        request.inputKind == "confirm" ? .accept("yes") : .cancel
    }
))

// Shell output events (`tool.shell_output`).
await session.onShellOutput { chunk in
    print(chunk.stream == "stderr" ? "stderr: \(chunk.text)" : chunk.text)
}

// Client turn ID, message source, and structured output.
let reply = try await session.sendAndWait(MessageOptions(
    prompt: "Summarize open tickets",
    source: .agent("triage"),
    responseSchema: [
        "type": "object",
        "properties": ["count": ["type": "integer"]],
        "required": ["count"],
    ],
    clientTurnId: "turn-42"
))
print(reply?.data["content"] as? String ?? "")

// Policy helper recheck (sessions with managed settings) and permission mode refusal reason.
try await session.recheckPolicyHelper()
let modeResult = try await session.setPermissionMode("allow-all")
if !modeResult.success && modeResult.reason == PermissionModeRefusalReason.managedPolicy {
    print("Managed policy keeps the current permission mode")
}

// Detach keeps the on-disk state (`session.detach`).
let sessionId = await session.sessionId
try await session.detach()

// Resume with transcript recovery, without emitting session.resume.
do {
    let resumed = try await client.resumeSession(sessionId, config: ResumeSessionConfig(
        allowTranscriptRecovery: true,
        clearDefaultProvider: true,
        suppressResumeEvent: true
    ))
    try await resumed.detach()
} catch let error as JsonRpcError {
    // Policy helper failure from `error.data.policyHelperFailure`.
    if let failure = error.policyHelperFailure {
        print("\(failure.category): \(failure.message)")
    }
    throw error
}

try await client.clearManagedSettingsCache()  // managedSettings.clearCache
await client.stop()
```

GitHub token provider (answers `gitHubToken.getToken`; mutually exclusive with
`gitHubToken`) and Auto routing tier (the session model must be `auto`):

```swift
let session = try await client.createSession(SessionConfig(
    model: "auto",
    gitHubTokenProvider: { request in
        let token = try await mintToken(host: request.host)
        return .token(accessToken: token, expiresIn: 7200)
    }
))
try await session.switchAutoTier(.efficiency)  // session.model.switchAutoTier
```

Empty client mode: every session must list `availableTools` (`createSession` throws
otherwise), and the empty-mode defaults (no experimental mode, telemetry, persistent MCP
tokens, embeddings, cross-session store, memory, on-demand instructions, repository hooks,
or host git operations) are applied under your own values:

```swift
let minimal = CopilotClient(options: CopilotClientOptions(mode: .empty))
let session = try await minimal.createSession(SessionConfig(availableTools: ["view", "grep"]))
```

New in this release:

- **Protocol 4 grouped payloads**: `createSession` / `resumeSession` send grouped options
  (for example `hooks: {hasHandlers: true}`); `ResumeSessionConfig.suppressResumeEvent`
  (alias `disableResume`) and `allowTranscriptRecovery` stay top level.
- **Named BYOK providers**: `providers`, `models`, `defaultProvider` (or
  `clearDefaultProvider: true` on resume), `capi` (`CapiSessionOptions`, which also
  carries the legacy `enableWebSocketResponses` option).
- **Client mode**, **client info**, **network callbacks** (`NetworkConfig`),
  **shell authentication** (`onShellAuthenticationRequest`, cancelled prompts resolve to
  `.cancel`), **memory store** (`MemoryConfiguration.storeMemory`), **shell output events**
  (`SessionEventTypes.toolShellOutput`, `ToolShellOutputData`, `onShellOutput`),
  **idempotent tools** (`Tool(idempotent:)`, `defineTool(idempotent:)`),
  **client turn ID**, **policy helper failure** (`JsonRpcError.policyHelperFailure`,
  `PolicyHelperFailure`), and **permission mode refusal reason**
  (`PermissionModeSetResult.reason`, `PermissionModeRefusalReason.managedPolicy`).
- **v2.6.0 parity**: `switchAutoTier`, `askUserVariant`, `gitHubTokenProvider`,
  `SandboxConfig.allowBypass`, `featureFlags`, `detach()`, `MessageSource`,
  `clearManagedSettingsCache()`, `recheckPolicyHelper()`, `allowedModels`,
  `authClientIdMetadataUrl`, `ModelLimitsOverride.maxOutputTokens` (`max_output_tokens`),
  `responseSchema`, `agentDirectories` / `hookDirectories`, `trustWorkingDirectory`
  (`"session"` or `"user"`, create only), `allowTranscriptRecovery`,
  `ImageGenerationConfig.saveToDisk`, `AutopilotOptions.maxContinues`,
  `onSubagentStart` / `onSubagentStop` hooks, and `ProviderConfig.apiKeyCommand`.
- **Options now sent**: `includeSubAgentStreamingEvents`, `modelCapabilities`,
  `enableConfigDiscovery`, `gitHubToken` and `instructionDirectories` were previously
  accepted but not sent; `experimentalMode` is now sent as
  `experiments.enableExperimentalMode`.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is an
  experimental upstream surface that depends on the runtime's in-process hosting library;
  it is not available in this SDK.
- In-process native runtime hosting is not available: the client always spawns the CLI.
  The `inProcess` option is accepted but has no effect.
- Only the stdio transport is implemented: `cliUrl` and TCP (`useStdio: false`) are
  accepted but cannot connect yet.
- The legacy per-message `MessageOptions.responseFormat` is no longer sent (protocol 4 uses
  `responseFormat` for structured output): use `responseSchema`, or the session-level
  `imageGeneration` option for images.
- `commands`, `onElicitationRequest` and `onMcpAuthRequest` are accepted on the session
  configs but not yet wired to runtime requests.

## Architecture

The SDK follows the same patterns as all other SDKs in this repository (40 languages supported):

```
CopilotClient
  |-- spawns CLI process with --headless --stdio
  |-- JsonRpcClient (Content-Length framed JSON-RPC 2.0 over stdin/stdout)
  |-- manages CopilotSession instances
```

### Key Components

| File | Description |
|------|-------------|
| `CopilotClient.swift` | Main client: spawns CLI, manages connections and sessions |
| `CopilotSession.swift` | Session: send/receive messages, event subscriptions, tool handling |
| `JsonRpcClient.swift` | JSON-RPC 2.0 client with Content-Length header framing |
| `Types.swift` | All type definitions (Codable structs, enums, typealiases) |
| `SessionParams.swift` | Groups flat session options into the SDK protocol 4 payload |
| `DefineTool.swift` | Helper for defining tools with typed parameters |
| `SdkProtocolVersion.swift` | Protocol version constant (must match CLI server) |

## Core Concepts

### Client

The `CopilotClient` is the entry point. It spawns the Copilot CLI process and manages the JSON-RPC connection.

```swift
// Default options (stdio transport, auto-start)
let client = CopilotClient()

// Custom options
let client = CopilotClient(options: CopilotClientOptions(
    cliPath: "/usr/local/bin/copilot",
    logLevel: "debug"
))

// Connect to an existing server
let client = CopilotClient(options: CopilotClientOptions(
    cliUrl: "localhost:3000"
))

// Explicit start (if autoStart is false)
try await client.start()
```

### Sessions

Sessions maintain conversation state. Create, resume, or list them:

```swift
// Create a new session
let session = try await client.createSession(SessionConfig(
    model: "gpt-4",
    streaming: true
))

// Resume an existing session
let session = try await client.resumeSession("session-id-here")

// List all sessions
let sessions = try await client.listSessions()

// Delete a session
try await client.deleteSession("session-id-here")
```

### Sending Messages

```swift
// Fire and forget (events delivered via handlers)
try await session.send(MessageOptions(prompt: "Hello"))

// Send and wait for completion (blocks until session.idle)
let response = try await session.sendAndWait(
    MessageOptions(prompt: "What is 2+2?"),
    timeout: 30  // seconds
)

// With attachments
try await session.send(MessageOptions(
    prompt: "Explain this file",
    attachments: [.file(path: "/path/to/file.swift")]
))
```

### Event Handling

```swift
// Subscribe to all events
let handlerId = await session.on { event in
    switch event.type {
    case "assistant.message":
        print("Assistant: \(event.data["content"] ?? "")")
    case "assistant.message_delta":
        // Streaming delta
        print(event.data["deltaContent"] ?? "", terminator: "")
    case "tool.execution_start":
        print("Tool: \(event.data["toolName"] ?? "")")
    case "session.error":
        print("Error: \(event.data["message"] ?? "")")
    default:
        break
    }
}

// Subscribe to a specific event type
await session.on("assistant.message") { event in
    print(event.data["content"] as? String ?? "")
}

// Unsubscribe
await session.removeEventHandler(handlerId)
```

### Tools

Define custom tools that the agent can invoke:

```swift
// Using typed parameters (recommended)
struct SearchParams: Codable {
    let query: String
    let maxResults: Int?
}

let searchTool = defineTool(
    name: "search",
    description: "Search for information",
    parametersType: SearchParams.self
) { (params: SearchParams, invocation) -> Any? in
    // Your search logic here
    return "Results for: \(params.query)"
}

// Using explicit JSON schema
let simpleTool = defineTool(
    name: "get_time",
    description: "Get the current time"
) { invocation -> Any? in
    return ISO8601DateFormatter().string(from: Date())
}

// Pass tools to session
let session = try await client.createSession(SessionConfig(
    tools: [searchTool, simpleTool]
))
```

### Permissions

Handle permission requests when the agent needs to perform sensitive operations:

```swift
let session = try await client.createSession(SessionConfig(
    onPermissionRequest: { request, sessionId in
        print("Permission requested: \(request.kind)")
        // Approve all read operations, deny everything else
        if request.kind == "read" {
            return .approved
        }
        return .deniedByUser
    }
))
```

### User Input (ask_user)

Handle questions from the agent to the user:

```swift
let session = try await client.createSession(SessionConfig(
    onUserInputRequest: { request, sessionId in
        print("Agent asks: \(request.question)")
        // In a real app, prompt the user for input
        return UserInputResponse(answer: "Yes", wasFreeform: true)
    }
))
```

### Hooks

Intercept session lifecycle events:

```swift
let session = try await client.createSession(SessionConfig(
    hooks: SessionHooks(
        onPreToolUse: { input, sessionId in
            print("About to use tool: \(input.toolName)")
            return PreToolUseHookOutput(permissionDecision: "allow")
        },
        onPostToolUse: { input, sessionId in
            print("Tool \(input.toolName) completed")
            return nil
        },
        onSessionStart: { input, sessionId in
            print("Session started from: \(input.source)")
            return nil
        }
    )
))
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```swift
let client = CopilotClient(options: CopilotClientOptions(sessionIdleTimeoutSeconds: 300))
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```swift
let client = CopilotClient(options: CopilotClientOptions(
    sessionFs: SessionFsConfig(initialCwd: "/repo", sessionStatePath: "/tmp/state", conventions: .posix)
))
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```swift
let meta = try await client.getSessionMetadata("session-123")
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```swift
let session = try await client.createSession(config: SessionConfig(
    skillDirectories: ["./skills"],
    disabledSkills: ["test-skill"],
    includeSubAgentStreamingEvents: true
))
```

- `skillDirectories` - Paths to directories containing skill definitions
- `disabledSkills` - Skills to exclude from the session
- `includeSubAgentStreamingEvents` - When `true`, receive streaming events from sub-agents

### Custom Providers (BYOK)

Use your own API key with OpenAI, Azure, or Anthropic:

```swift
let session = try await client.createSession(SessionConfig(
    provider: ProviderConfig(
        baseUrl: "https://api.openai.com/v1",
        type: "openai",
        apiKey: "sk-..."
    )
))
```

### MCP Servers

Configure Model Context Protocol servers:

```swift
let session = try await client.createSession(SessionConfig(
    mcpServers: [
        "my-server": .local(
            command: "npx",
            args: ["-y", "@modelcontextprotocol/server-filesystem", "/path"],
            tools: ["*"]
        ),
        "remote-server": .http(
            url: "https://mcp.example.com",
            headers: ["Authorization": "Bearer token"]
        )
    ]
))
```

### Models

List available models:

```swift
let models = try await client.listModels()
for model in models {
    print("\(model.id): \(model.name)")
    if model.capabilities.supports?.reasoningEffort == true {
        print("  Supports reasoning effort: \(model.supportedReasoningEfforts ?? [])")
    }
}
```

### Session Lifecycle Events

Monitor session lifecycle changes (useful in TUI+server mode):

```swift
let handlerId = await client.onLifecycleEvent { event in
    switch event.type {
    case .created:
        print("Session created: \(event.sessionId)")
    case .deleted:
        print("Session deleted: \(event.sessionId)")
    case .foreground:
        print("Session foregrounded: \(event.sessionId)")
    default:
        break
    }
}
```

## Concurrency Model

The SDK uses Swift concurrency throughout:

- `CopilotClient` and `CopilotSession` are **actors**, providing thread-safe access to mutable state.
- All async operations use `async/await`.
- Event handlers and tool handlers are `@Sendable` closures.
- The `JsonRpcClient` is an actor that manages the stdio read/write loop.

## Protocol

The SDK communicates with the Copilot CLI using JSON-RPC 2.0 with Content-Length header framing (the same protocol used by LSP):

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","id":"1","method":"ping","params":{}}
```

### Client-to-Server Methods

| Method | Description |
|--------|-------------|
| `connect` | Handshake declaring `clientInfo` (sent only when configured) |
| `ping` | Health check, returns protocol version (must be 4) |
| `status.get` | Get CLI version and protocol info |
| `auth.getStatus` | Get authentication status |
| `models.list` | List available models |
| `network.setProvider` | Register network callbacks (sent only when configured) |
| `managedSettings.clearCache` | Clear cached managed settings |
| `session.create` | Create a new session (grouped protocol 4 options) |
| `session.resume` | Resume an existing session (grouped protocol 4 options) |
| `session.send` | Send a message to a session |
| `session.detach` | Release a session, keeping its state (falls back to `session.destroy`) |
| `session.abort` | Abort current processing |
| `session.getMessages` | Get session history |
| `session.model.switchAutoTier` | Change the Auto routing tier |
| `session.managedSettings.recheckPolicyHelper` | Re-run the managed-settings Policy Helper |
| `session.permissions.setMode` | Set the permission mode |
| `session.list` | List all sessions |
| `session.delete` | Delete a session from disk |
| `session.getLastId` | Get most recent session ID |
| `session.getForeground` | Get foreground session (TUI mode) |
| `session.setForeground` | Set foreground session (TUI mode) |

### Server-to-Client Notifications

| Method | Description |
|--------|-------------|
| `session.event` | Session event (messages, tool use, etc.) |
| `session.lifecycle` | Session lifecycle changes |

### Server-to-Client Requests

| Method | Description |
|--------|-------------|
| `tool.call` | Invoke a registered tool |
| `permission.request` | Request permission for an operation |
| `userInput.request` | Request user input (ask_user) |
| `hooks.invoke` | Invoke a session hook |
| `exitPlanMode.request` | Ask whether to leave plan mode |
| `gitHubToken.getToken` | Get a token from a session's `gitHubTokenProvider` |
| `shellAuthentication.request` / `shellAuthentication.cancel` | Answer or cancel a shell prompt |
| `memoryStore.store` | Store a memory with `MemoryConfiguration.storeMemory` |
| `network.resolveProxy` / `network.authenticateNegotiate` / `network.authenticateBasic` | Network callbacks |

## Running the Example

```bash
cd swift
swift run BasicExample
```

## Image Generation

Enable image generation for a session with `imageGeneration` (`tools.imageGeneration`):

```swift
let session = try await client.createSession(SessionConfig(
    imageGeneration: ImageGenerationConfig(enabled: true, saveToDisk: true)
))
let response = try await session.sendAndWait(MessageOptions(
    prompt: "Generate a sunset over mountains",
    imageOptions: ImageOptions(size: "1024x1024", quality: "hd", style: "natural")
))
```

The legacy `MessageOptions.responseFormat` (for example `.image`) is still accepted but is
no longer sent, because SDK protocol 4 runtimes use `responseFormat` only for structured
output (`responseSchema`).

## Recent Features (v2.4–v2.5)

The SDK tracks upstream `@github/copilot-sdk`. These options live on `SessionConfig`
(session-scoped), `CopilotClientOptions` (client-scoped), `MessageOptions` (per turn),
or `SessionHooks`.

### v2.5

- **Reasoning effort** — `SessionConfig(reasoningEffort:)` using `ReasoningEffort` (`.low`, `.medium`, `.high`, `.xhigh`).
- **Session rewind** — `SessionConfig(rewindEnabled:)` rolls the conversation back to an earlier turn.
- **Additional directories** — `SessionConfig(additionalDirectories:)` exposes extra workspace roots.
- **Content exclusion** — `SessionConfig(contentExclusion:)` honors rules that hide files from the agent.
- **Tool search** — `SessionConfig(toolSearch:)` discovers tools on demand instead of preloading them.
- **Disabled MCP servers** — `SessionConfig(disabledMcpServers:)`.
- **GitHub MCP tool config** — `SessionConfig(githubMcpToolConfig:)`.
- **Canvas provider** — `SessionConfig(canvasProvider:)`.
- **Custom agents local-only** — `SessionConfig(customAgentsLocalOnly:)`.
- **Experimental mode** — `SessionConfig(experimentalMode:)`.
- **User-prompt-transformed hook** — `SessionHooks(onUserPromptTransformed:)`.
- **Permission decision context** — `PreToolUseHookOutput(decisionContext:)`.
- **Agent-factory args schema** — `argsSchema` on the factory definition.
- **Built-in plugin dirs / in-process FFI** — `CopilotClientOptions(builtinPluginDirectories:)`, `CopilotClientOptions(inProcess:)`.

### v2.4

- **BYOK bearer token provider** — `CopilotClientOptions(bearerTokenProvider:)` (`BearerTokenProvider`).
- **MCP OAuth token handler** — `SessionConfig(onMcpAuthRequest:)` (`McpAuthHandler`).
- **HTTP request handler** — `CopilotClientOptions(requestHandler:)` (`CopilotRequestHandler`).
- **Session citations** — `SessionConfig(enableCitations:)`.
- **Excluded built-in agents** — `SessionConfig(excludedBuiltinAgents:)`.
- **Spending limits** — `SessionConfig(sessionLimits:)` with `SessionLimitsConfig(maxAiCredits:)`.
- **Session memory** — `SessionConfig(memory:)` with `MemoryConfiguration`.
- **OTLP protocol / WebSocket transport** — `SessionConfig(otlpProtocol:)`, `SessionConfig(enableWebSocketResponses:)`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `SessionHooks(onPostToolUse:)`, `SessionHooks(onPreMcpToolCall:)`.
- **Message agent mode / display prompt** — `MessageOptions(agentMode:)`, `MessageOptions(displayPrompt:)`.
- **Tool defer loading** — `ToolDefer.auto` / `ToolDefer.never`.
- **System message sections** — `SystemMessageSection.preamble`, `SystemMessageSection.preserve`.
- **GitHub attachments** — `GitHubAttachment.gitHubCommit`, `GitHubAttachment.gitHubRepository`.
- **Experiment assignments** — `SessionConfig(expAssignments:)`.

```swift
// Reasoning effort + session rewind + extra workspace roots
let session = try await client.createSession(SessionConfig(
    reasoningEffort: .high,
    rewindEnabled: true,
    additionalDirectories: ["/repo/packages/core", "/repo/packages/api"]
))
```

```swift
// Honor content exclusion and discover tools on demand
let session = try await client.createSession(SessionConfig(
    contentExclusion: true,
    toolSearch: ["mode": AnyCodable("auto")]
))
```

```swift
// Bring-your-own-key: supply bearer tokens dynamically
let client = CopilotClient(options: CopilotClientOptions(
    bearerTokenProvider: { _ in await fetchToken() }
))
```

```swift
// Cap AI-credit spend and emit citations
let session = try await client.createSession(SessionConfig(
    enableCitations: true,
    sessionLimits: SessionLimitsConfig(maxAiCredits: 5.0)
))
```

```swift
// Per-message agent mode and display prompt
try await session.send(MessageOptions(
    prompt: "Refactor this module",
    agentMode: "plan",
    displayPrompt: "Refactor (planning)"
))
```

## License

See the [LICENSE](../LICENSE) file in the repository root.
