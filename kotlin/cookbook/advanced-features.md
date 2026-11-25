# Advanced Features

Recipes for advanced session and client options in the GitHub Copilot SDK for Kotlin.

The snippets on this page are fragments. They run inside a coroutine (for example `runBlocking`)
and assume these imports, a started `CopilotClient` named `client`, and the `approveAll` permission
handler below:

```kotlin
import com.github.copilot.*
import kotlinx.serialization.json.*
import java.util.UUID

val approveAll: PermissionHandler = { _, _ -> PermissionRequestResult(kind = "approve-once") }
```

## Per-Session Authentication

Run each session as a different GitHub user, for example one per tenant in a multi-tenant service:

```kotlin
val tenantSession = client.createSession(SessionConfig(
    gitHubToken = System.getenv("TENANT_ABC_GITHUB_TOKEN"),
    onPermissionRequest = approveAll,
))

tenantSession.send(MessageOptions(prompt = "Hello from tenant ABC"))
```

## System Prompt Customization

### Append (the default mode)

Your content is added after the SDK-managed sections of the system prompt:

```kotlin
val config = SessionConfig(
    systemMessage = SystemMessageConfig(content = "Always check for security problems and suggest tests."),
    onPermissionRequest = approveAll,
)
```

### Replace mode

Replaces the whole system prompt, including the SDK's guardrails:

```kotlin
val config = SessionConfig(
    systemMessage = SystemMessageConfig(
        mode = "replace",
        content = "You are a Kotlin code review assistant. Focus on idiomatic code and coroutines.",
    ),
    onPermissionRequest = approveAll,
)
```

### Customize mode

Override individual sections and keep the rest. `SystemPromptSection` has the section IDs and
`SectionOverrideAction` has the actions (`REPLACE`, `REMOVE`, `APPEND`, `PREPEND`). `content` is
appended after all sections.

```kotlin
val config = SessionConfig(
    systemMessage = SystemMessageConfig(
        mode = "customize",
        sections = mapOf(
            SystemPromptSection.TONE to SectionOverride(
                action = SectionOverrideAction.REPLACE,
                content = "Be concise and direct.",
            ),
            SystemPromptSection.GUIDELINES to SectionOverride(
                action = SectionOverrideAction.APPEND,
                content = "\n* Prefer idiomatic Kotlin and structured concurrency.",
            ),
        ),
        content = "Focus on Kotlin backend services.",
    ),
    onPermissionRequest = approveAll,
)
```

## Tool Visibility

Hide built-in tools from a session, or allow only the tools you list:

```kotlin
// Keep the agent from writing files
val readOnly = SessionConfig(
    excludedTools = listOf("create", "edit"),
    onPermissionRequest = approveAll,
)

// Or allow only these tools
val viewOnly = SessionConfig(
    availableTools = listOf("view", "grep", "glob"),
    onPermissionRequest = approveAll,
)
```

To limit the tools of a single custom agent, set `CustomAgentConfig.tools`; see
[Custom Agents](tools-and-skills.md#custom-agents).

## Runtime Request Headers

Add HTTP headers to the outbound model requests of one message, or of every message in a session:

```kotlin
val session = client.createSession(SessionConfig(onPermissionRequest = approveAll))

session.send(MessageOptions(
    prompt = "Analyze this code",
    requestHeaders = mapOf(
        "X-Request-Id" to UUID.randomUUID().toString(),
        "X-Tenant-Id" to "tenant-abc-123",
    ),
))

// Session-wide headers
val config = SessionConfig(
    requestHeaders = mapOf("X-Tenant-Id" to "tenant-abc-123"),
    onPermissionRequest = approveAll,
)
```

## Model Capabilities Override

Deep-merge overrides into the capabilities the runtime reports for the model. `supports` takes
`vision`, `toolCalls`, and `reasoningEffort` flags; `limits` takes `max_prompt_tokens`,
`max_output_tokens`, and `max_context_window_tokens`.

```kotlin
val config = SessionConfig(
    model = "gpt-5",
    modelCapabilities = mapOf(
        "supports" to mapOf("vision" to false),
        "limits" to mapOf("max_output_tokens" to 8192),
    ),
    onPermissionRequest = approveAll,
)
```

## Config Discovery

Let the runtime discover supported configuration, such as ambient user hooks. Settings you pass
explicitly take precedence. It's off by default.

```kotlin
val config = SessionConfig(
    workingDirectory = "/path/to/project",
    enableConfigDiscovery = true,
    onPermissionRequest = approveAll,
)
```

## Streaming and Sub-Agent Streaming Events

With `streaming = true` the session emits `assistant.message_delta` events with a `deltaContent`
field. Deltas from sub-agents are included by default; set `includeSubAgentStreamingEvents = false`
to get only the main agent's deltas.

```kotlin
val session = client.createSession(SessionConfig(
    streaming = true,
    includeSubAgentStreamingEvents = false,
    onPermissionRequest = approveAll,
))

session.on("assistant.message_delta") { event ->
    print(event.data?.get("deltaContent")?.jsonPrimitive?.contentOrNull ?: "")
}
session.sendAndWait(MessageOptions(prompt = "Explain the visitor pattern"))
println()
```

## Session Metadata

`listSessions()` returns typed metadata for saved sessions. `getSessionMetadata()` and
`session.getMetadata()` return the runtime's raw response, with the details under a `session` key:

```kotlin
val session = client.createSession(SessionConfig(onPermissionRequest = approveAll))
session.sendAndWait(MessageOptions(prompt = "Hello"))

client.listSessions()
    .filter { it.sessionId == session.sessionId }
    .forEach { meta ->
        println("Started: ${meta.startTime}, last modified: ${meta.modifiedTime}")
        println("Summary: ${meta.summary}")
    }

val raw = client.getSessionMetadata(session.sessionId)["session"]?.jsonObject
println("Raw metadata: $raw")
```

## MCP Server Configuration

Pass MCP servers as a map from a server name to its configuration. `tools` selects the server's tools
(`"*"` for all); the model sees them as `<server-name>-<tool-name>`.

```kotlin
val config = SessionConfig(
    mcpServers = mapOf(
        "local-tools" to MCPServerConfig(
            tools = listOf("*"),
            type = "stdio",
            command = "node",
            args = listOf("./mcp-server/index.js"),
        ),
        "remote-tools" to MCPServerConfig(
            tools = listOf("*"),
            type = "http",
            url = "https://mcp.example.com/mcp",
            headers = mapOf("Authorization" to "Bearer ${System.getenv("MCP_TOKEN")}"),
        ),
    ),
    onPermissionRequest = approveAll,
)
```

## Structured Output

`sendAndWaitStructured` asks for a reply that matches a JSON Schema and returns it parsed as a
`JsonElement`:

```kotlin
val schema = mapOf(
    "type" to "object",
    "properties" to mapOf(
        "language" to mapOf("type" to "string"),
        "rating" to mapOf("type" to "integer"),
    ),
    "required" to listOf("language", "rating"),
)

val session = client.createSession(SessionConfig(onPermissionRequest = approveAll))
val result = session.sendAndWaitStructured("Name one JVM language and rate it from 1 to 5", schema).jsonObject
println("${result["language"]?.jsonPrimitive?.contentOrNull}: ${result["rating"]?.jsonPrimitive?.intOrNull}")
```

## Features Without a Recipe

These options exist in the API, but the SDK doesn't act on them yet, so a recipe wouldn't work as
written:

- Session idle timeout (`CopilotClientOptions.sessionIdleTimeoutSeconds`, `SessionConfig.idleTimeout`): the SDK accepts the values but doesn't pass them to the CLI.
- Slash commands (`SessionConfig.commands`) and elicitation (`SessionConfig.onElicitationRequest`): the SDK declares them to the runtime, but it doesn't run command handlers, and it calls the elicitation handler only for the legacy `elicitation.request` callback, not for the `elicitation.requested` events that protocol 4 runtimes send.
- Custom session filesystem providers (`SessionFsProvider`): with `CopilotClientOptions.sessionFs` set, the SDK answers the runtime's `sessionFs.*` requests from the local disk itself; there's no way to plug in your own provider.
