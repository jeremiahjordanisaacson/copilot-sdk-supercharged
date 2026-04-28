# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```kotlin
import com.github.copilot.sdk.CopilotClient
import com.github.copilot.sdk.SessionConfig

val client = CopilotClient()

val config = SessionConfig(
    githubToken = "ghu_tenant_abc_token_here"
)

val session = client.createSession(config)
// This session authenticates as a specific tenant user
session.send("Hello from tenant ABC")
```

## Session Idle Timeout

Auto-cleanup inactive sessions with a configurable timeout:

```kotlin
val config = SessionConfig(
    sessionIdleTimeoutSeconds = 600 // 10 minutes
)

val session = client.createSession(config)
// Session automatically closes after 600 seconds of inactivity
```

## SessionFs (Session Filesystem)

Provide a session-scoped filesystem with 10 I/O operations (readFile, writeFile, appendFile, exists, stat, mkdir, readdir, readdirWithTypes, rm, rename):

```kotlin
import com.github.copilot.sdk.SessionFsProvider
import com.github.copilot.sdk.DirEntry
import com.github.copilot.sdk.FileStat
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption

class MyFsProvider(private val basePath: Path) : SessionFsProvider {
    override suspend fun readFile(path: String): ByteArray =
        Files.readAllBytes(basePath.resolve(path))

    override suspend fun writeFile(path: String, content: ByteArray) {
        Files.write(basePath.resolve(path), content)
    }

    override suspend fun appendFile(path: String, content: ByteArray) {
        Files.write(basePath.resolve(path), content, StandardOpenOption.APPEND)
    }

    override suspend fun exists(path: String): Boolean =
        Files.exists(basePath.resolve(path))

    override suspend fun stat(path: String): FileStat {
        val attrs = Files.readAttributes(basePath.resolve(path), java.nio.file.attribute.BasicFileAttributes::class.java)
        return FileStat(size = attrs.size(), isDirectory = attrs.isDirectory)
    }

    override suspend fun mkdir(path: String) {
        Files.createDirectories(basePath.resolve(path))
    }

    override suspend fun readdir(path: String): List<String> =
        Files.list(basePath.resolve(path)).use { stream ->
            stream.map { it.fileName.toString() }.toList()
        }

    override suspend fun readdirWithTypes(path: String): List<DirEntry> =
        Files.list(basePath.resolve(path)).use { stream ->
            stream.map { DirEntry(it.fileName.toString(), Files.isDirectory(it)) }.toList()
        }

    override suspend fun rm(path: String) {
        val full = basePath.resolve(path)
        if (Files.isDirectory(full)) {
            Files.walk(full).sorted(Comparator.reverseOrder()).forEach(Files::delete)
        } else {
            Files.deleteIfExists(full)
        }
    }

    override suspend fun rename(oldPath: String, newPath: String) {
        Files.move(basePath.resolve(oldPath), basePath.resolve(newPath))
    }
}

val config = SessionConfig(
    sessionFs = MyFsProvider(Path.of("/workspace"))
)

val session = client.createSession(config)
```

## Commands and UI Elicitation

Register slash commands and handle interactive prompts/dialogs:

```kotlin
import com.github.copilot.sdk.Command

val commands = listOf(
    Command(name = "deploy", description = "Deploy the current project"),
    Command(name = "test", description = "Run the test suite"),
)

val config = SessionConfig(
    commands = commands,
    onElicitationRequest = { request ->
        // Present the dialog to the user and collect responses
        mapOf(
            "environment" to "staging",
            "confirmed" to true
        )
    }
)

val session = client.createSession(config)
```

## System Prompt Customization

Fine-grained control with append, replace, and customize modes:

### Replace mode

```kotlin
val config = SessionConfig(
    systemPrompt = "You are a Kotlin code review assistant. Focus on idiomatic patterns and coroutines.",
    systemPromptMode = "replace"
)

val session = client.createSession(config)
```

### Customize mode with sections

```kotlin
import com.github.copilot.sdk.SystemPromptSection

val sections = listOf(
    SystemPromptSection(key = "role", content = "You are an expert Kotlin developer."),
    SystemPromptSection(key = "constraints", content = "Always suggest idiomatic Kotlin with coroutines."),
    SystemPromptSection(key = "output", content = "Format responses as Markdown with code blocks."),
)

val config = SessionConfig(
    systemPromptSections = sections,
    systemPromptMode = "customize"
)

val session = client.createSession(config)
```

## Per-Agent Skills

Preload skill content into agent context at startup:

```kotlin
val config = SessionConfig(
    skillDirectories = listOf(
        "/workspace/.copilot/skills",
        "/workspace/custom-skills"
    ),
    disabledSkills = listOf("deprecated-skill")
)

val session = client.createSession(config)
```

## Per-Agent Tool Visibility

Control tool access per agent with excludedTools:

```kotlin
import com.github.copilot.sdk.AgentConfig

val defaultAgent = AgentConfig(
    excludedTools = listOf(
        "shell_exec",
        "file_delete",
        "network_request"
    )
)

val config = SessionConfig(
    defaultAgent = defaultAgent
)

val session = client.createSession(config)
```

## Runtime Request Headers

Send custom HTTP headers per message turn:

```kotlin
import com.github.copilot.sdk.SendOptions
import java.util.UUID

val options = SendOptions(
    requestHeaders = mapOf(
        "X-Request-Id" to UUID.randomUUID().toString(),
        "X-Tenant-Id" to "tenant-abc-123"
    )
)

session.send("Analyze this code", options)
```

## Model Capabilities Override

Deep-merge overrides for model feature flags:

```kotlin
val config = SessionConfig(
    modelCapabilities = mapOf(
        "streaming" to true,
        "functionCalling" to true,
        "vision" to false,
        "maxTokens" to 8192
    )
)

val session = client.createSession(config)
```

## Config Discovery

Auto-detect MCP servers and skill directories from workspace:

```kotlin
val config = SessionConfig(
    enableConfigDiscovery = true
)

val session = client.createSession(config)
// MCP servers and skills from .copilot/ in the workspace are loaded automatically
```

## Sub-Agent Streaming Events

Control streaming event forwarding from sub-agents:

```kotlin
val config = SessionConfig(
    includeSubAgentStreamingEvents = true
)

val session = client.createSession(config)

session.onEvent("assistant.message_delta") { event ->
    // Receives deltas from both the primary agent and sub-agents
    print(event.delta)
}
```

## Session Metadata

Retrieve session metadata via RPC:

```kotlin
val session = client.createSession(config)
session.send("Hello")

val metadata = session.getSessionMetadata()
println("Session ID: ${metadata.sessionId}")
println("Created at: ${metadata.createdAt}")
println("Turn count: ${metadata.turnCount}")
```

## MCP Server Configuration

Configure MCP servers with stdio and HTTP transport types:

```kotlin
import com.github.copilot.sdk.McpServerConfig

val mcpServers = listOf(
    // Stdio transport
    McpServerConfig(
        name = "local-tools",
        transportType = "stdio",
        command = "node",
        args = listOf("./mcp-server/index.js")
    ),
    // HTTP transport
    McpServerConfig(
        name = "remote-tools",
        transportType = "http",
        url = "https://mcp.example.com/api",
        headers = mapOf("Authorization" to "Bearer $token")
    ),
)

val config = SessionConfig(
    mcpServers = mcpServers
)

val session = client.createSession(config)
```

## Image Generation

Configure response format and image options:

```kotlin
val config = SessionConfig(
    responseFormat = "image"
)

val session = client.createSession(config)

val response = session.send("Generate a UML class diagram for a user auth system")
// Response contains image data
response.imageData?.let { data ->
    java.io.File("diagram.png").writeBytes(data)
}
```
