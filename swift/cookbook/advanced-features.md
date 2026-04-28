# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```swift
import CopilotSDK

let client = CopilotClient()

let config = SessionConfig(
    githubToken: "ghu_tenant_abc_token_here"
)

let session = try await client.createSession(config: config)
// This session authenticates as a specific tenant user
try await session.send("Hello from tenant ABC")
```

## Session Idle Timeout

Auto-cleanup inactive sessions with a configurable timeout:

```swift
let config = SessionConfig(
    sessionIdleTimeoutSeconds: 600 // 10 minutes
)

let session = try await client.createSession(config: config)
// Session automatically closes after 600 seconds of inactivity
```

## SessionFs (Session Filesystem)

Provide a session-scoped filesystem with 10 I/O operations (readFile, writeFile, appendFile, exists, stat, mkdir, readdir, readdirWithTypes, rm, rename):

```swift
import Foundation

class MyFsProvider: SessionFsProvider {
    let basePath: URL

    init(basePath: URL) {
        self.basePath = basePath
    }

    func readFile(path: String) async throws -> Data {
        try Data(contentsOf: basePath.appendingPathComponent(path))
    }

    func writeFile(path: String, content: Data) async throws {
        try content.write(to: basePath.appendingPathComponent(path))
    }

    func appendFile(path: String, content: Data) async throws {
        let handle = try FileHandle(forWritingTo: basePath.appendingPathComponent(path))
        defer { handle.closeFile() }
        handle.seekToEndOfFile()
        handle.write(content)
    }

    func exists(path: String) async throws -> Bool {
        FileManager.default.fileExists(atPath: basePath.appendingPathComponent(path).path)
    }

    func stat(path: String) async throws -> FileStat {
        let attrs = try FileManager.default.attributesOfItem(
            atPath: basePath.appendingPathComponent(path).path
        )
        return FileStat(
            size: attrs[.size] as? UInt64 ?? 0,
            isDirectory: (attrs[.type] as? FileAttributeType) == .typeDirectory
        )
    }

    func mkdir(path: String) async throws {
        try FileManager.default.createDirectory(
            at: basePath.appendingPathComponent(path),
            withIntermediateDirectories: true
        )
    }

    func readdir(path: String) async throws -> [String] {
        try FileManager.default.contentsOfDirectory(
            atPath: basePath.appendingPathComponent(path).path
        )
    }

    func readdirWithTypes(path: String) async throws -> [DirEntry] {
        let names = try FileManager.default.contentsOfDirectory(
            atPath: basePath.appendingPathComponent(path).path
        )
        return names.map { name in
            var isDir: ObjCBool = false
            let full = basePath.appendingPathComponent(path).appendingPathComponent(name).path
            FileManager.default.fileExists(atPath: full, isDirectory: &isDir)
            return DirEntry(name: name, isDirectory: isDir.boolValue)
        }
    }

    func rm(path: String) async throws {
        try FileManager.default.removeItem(at: basePath.appendingPathComponent(path))
    }

    func rename(oldPath: String, newPath: String) async throws {
        try FileManager.default.moveItem(
            at: basePath.appendingPathComponent(oldPath),
            to: basePath.appendingPathComponent(newPath)
        )
    }
}

let config = SessionConfig(
    sessionFs: MyFsProvider(basePath: URL(fileURLWithPath: "/workspace"))
)

let session = try await client.createSession(config: config)
```

## Commands and UI Elicitation

Register slash commands and handle interactive prompts/dialogs:

```swift
let commands = [
    Command(name: "deploy", description: "Deploy the current project"),
    Command(name: "test", description: "Run the test suite"),
]

let config = SessionConfig(
    commands: commands,
    onElicitationRequest: { request in
        // Present the dialog to the user and collect responses
        return [
            "environment": "staging",
            "confirmed": true,
        ]
    }
)

let session = try await client.createSession(config: config)
```

## System Prompt Customization

Fine-grained control with append, replace, and customize modes:

### Replace mode

```swift
let config = SessionConfig(
    systemPrompt: "You are a Swift code review assistant. Focus on safety and performance.",
    systemPromptMode: .replace
)

let session = try await client.createSession(config: config)
```

### Customize mode with sections

```swift
let sections = [
    SystemPromptSection(key: "role", content: "You are an expert Swift developer."),
    SystemPromptSection(key: "constraints", content: "Always suggest modern Swift concurrency patterns."),
    SystemPromptSection(key: "output", content: "Format responses as Markdown with code blocks."),
]

let config = SessionConfig(
    systemPromptSections: sections,
    systemPromptMode: .customize
)

let session = try await client.createSession(config: config)
```

## Per-Agent Skills

Preload skill content into agent context at startup:

```swift
let config = SessionConfig(
    skillDirectories: [
        "/workspace/.copilot/skills",
        "/workspace/custom-skills",
    ],
    disabledSkills: ["deprecated-skill"]
)

let session = try await client.createSession(config: config)
```

## Per-Agent Tool Visibility

Control tool access per agent with excludedTools:

```swift
let defaultAgent = AgentConfig(
    excludedTools: [
        "shell_exec",
        "file_delete",
        "network_request",
    ]
)

let config = SessionConfig(
    defaultAgent: defaultAgent
)

let session = try await client.createSession(config: config)
```

## Runtime Request Headers

Send custom HTTP headers per message turn:

```swift
let options = SendOptions(
    requestHeaders: [
        "X-Request-Id": UUID().uuidString,
        "X-Tenant-Id": "tenant-abc-123",
    ]
)

try await session.send("Analyze this code", options: options)
```

## Model Capabilities Override

Deep-merge overrides for model feature flags:

```swift
let config = SessionConfig(
    modelCapabilities: [
        "streaming": true,
        "functionCalling": true,
        "vision": false,
        "maxTokens": 8192,
    ]
)

let session = try await client.createSession(config: config)
```

## Config Discovery

Auto-detect MCP servers and skill directories from workspace:

```swift
let config = SessionConfig(
    enableConfigDiscovery: true
)

let session = try await client.createSession(config: config)
// MCP servers and skills from .copilot/ in the workspace are loaded automatically
```

## Sub-Agent Streaming Events

Control streaming event forwarding from sub-agents:

```swift
let config = SessionConfig(
    includeSubAgentStreamingEvents: true
)

let session = try await client.createSession(config: config)

session.onEvent("assistant.message_delta") { event in
    // Receives deltas from both the primary agent and sub-agents
    print(event.delta, terminator: "")
}
```

## Session Metadata

Retrieve session metadata via RPC:

```swift
let session = try await client.createSession(config: config)
try await session.send("Hello")

let metadata = try await session.getSessionMetadata()
print("Session ID: \(metadata.sessionId)")
print("Created at: \(metadata.createdAt)")
print("Turn count: \(metadata.turnCount)")
```

## MCP Server Configuration

Configure MCP servers with stdio and HTTP transport types:

```swift
let mcpServers = [
    // Stdio transport
    McpServerConfig(
        name: "local-tools",
        transportType: .stdio,
        command: "node",
        args: ["./mcp-server/index.js"]
    ),
    // HTTP transport
    McpServerConfig(
        name: "remote-tools",
        transportType: .http,
        url: "https://mcp.example.com/api",
        headers: ["Authorization": "Bearer \(token)"]
    ),
]

let config = SessionConfig(
    mcpServers: mcpServers
)

let session = try await client.createSession(config: config)
```

## Image Generation

Configure response format and image options:

```swift
let config = SessionConfig(
    responseFormat: "image"
)

let session = try await client.createSession(config: config)

let response = try await session.send("Generate a UML class diagram for a user auth system")
// Response contains image data
if let imageData = response.imageData {
    try imageData.write(to: URL(fileURLWithPath: "diagram.png"))
}
```
