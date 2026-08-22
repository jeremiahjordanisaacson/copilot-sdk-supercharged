# GitHub Copilot Supercharged SDK for Groovy

Groovy SDK for programmatic control of GitHub Copilot CLI via JSON-RPC.

[![Groovy 4.0+](https://img.shields.io/badge/Groovy-4.0%2B-blue?logo=apachegroovy&logoColor=white)](https://groovy-lang.org/)
[![Java 17+](https://img.shields.io/badge/Java-17%2B-blue?logo=openjdk&logoColor=white)](https://openjdk.org/)

## Requirements

- Java 17+
- Groovy 4.0+ (included via Gradle)
- Gradle 8+ (for building)

## Installation

### Gradle

```groovy
dependencies {
    implementation 'io.github.jeremiahjordanisaacson:copilot-sdk-groovy:2.0.0'
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

```groovy
def result = session.sendAndWait(
    prompt: 'Generate a sunset over mountains',
    responseFormat: 'image',
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
| `resumeSession(id, config)` | Resume an existing session |
| `listSessions()` | List all sessions |
| `deleteSession(id)` | Delete a session |
| `ping(message)` | Ping the server |
| `listModels()` | List available models |
| `getState()` | Get connection state |
| `getStatus()` | Get CLI status |
| `getAuthStatus()` | Get auth status |
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
| `destroy()` | Destroy the session |

### DefineTool

| Method | Description |
|--------|-------------|
| `create(name)` | Start building a tool |
| `description(desc)` | Set tool description |
| `parameters(schema)` | Set JSON Schema parameters |
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
