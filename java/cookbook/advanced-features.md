# Advanced v2.0 Features

Recipes for the advanced features introduced in v2.0 of the Copilot SDK.

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.SessionConfig;

CopilotClient client = new CopilotClient();

SessionConfig config = new SessionConfig()
    .setGithubToken("ghu_tenant_abc_token_here");

var session = client.createSession(config);
// This session authenticates as a specific tenant user
session.send("Hello from tenant ABC");
```

## Session Idle Timeout

Auto-cleanup inactive sessions with a configurable timeout:

```java
SessionConfig config = new SessionConfig()
    .setSessionIdleTimeoutSeconds(600); // 10 minutes

var session = client.createSession(config);
// Session automatically closes after 600 seconds of inactivity
```

## SessionFs (Session Filesystem)

Provide a session-scoped filesystem with 10 I/O operations (readFile, writeFile, appendFile, exists, stat, mkdir, readdir, readdirWithTypes, rm, rename):

```java
import com.github.copilot.sdk.SessionFsProvider;

SessionFsProvider fsProvider = new SessionFsProvider() {
    @Override
    public byte[] readFile(String path) {
        return Files.readAllBytes(Path.of(basePath, path));
    }

    @Override
    public void writeFile(String path, byte[] content) {
        Files.write(Path.of(basePath, path), content);
    }

    @Override
    public void appendFile(String path, byte[] content) {
        Files.write(Path.of(basePath, path), content, StandardOpenOption.APPEND);
    }

    @Override
    public boolean exists(String path) {
        return Files.exists(Path.of(basePath, path));
    }

    @Override
    public Map<String, Object> stat(String path) {
        var attrs = Files.readAttributes(Path.of(basePath, path), BasicFileAttributes.class);
        return Map.of("size", attrs.size(), "isDirectory", attrs.isDirectory());
    }

    @Override
    public void mkdir(String path) {
        Files.createDirectories(Path.of(basePath, path));
    }

    @Override
    public List<String> readdir(String path) {
        return Files.list(Path.of(basePath, path))
            .map(p -> p.getFileName().toString())
            .collect(Collectors.toList());
    }

    @Override
    public List<DirEntry> readdirWithTypes(String path) {
        return Files.list(Path.of(basePath, path))
            .map(p -> new DirEntry(p.getFileName().toString(), Files.isDirectory(p)))
            .collect(Collectors.toList());
    }

    @Override
    public void rm(String path) {
        Files.deleteIfExists(Path.of(basePath, path));
    }

    @Override
    public void rename(String oldPath, String newPath) {
        Files.move(Path.of(basePath, oldPath), Path.of(basePath, newPath));
    }
};

SessionConfig config = new SessionConfig()
    .setSessionFs(fsProvider);

var session = client.createSession(config);
```

## Commands and UI Elicitation

Register slash commands and handle interactive prompts/dialogs:

```java
import com.github.copilot.sdk.Command;
import com.github.copilot.sdk.ElicitationHandler;

List<Command> commands = List.of(
    new Command("deploy", "Deploy the current project"),
    new Command("test", "Run the test suite")
);

SessionConfig config = new SessionConfig()
    .setCommands(commands)
    .setOnElicitationRequest((request) -> {
        // Present the dialog to the user and collect responses
        Map<String, Object> response = new HashMap<>();
        response.put("environment", "staging");
        response.put("confirmed", true);
        return response;
    });

var session = client.createSession(config);
```

## System Prompt Customization

Fine-grained control with append, replace, and customize modes:

### Replace mode

```java
SessionConfig config = new SessionConfig()
    .setSystemPrompt("You are a Java code review assistant. Focus on best practices and performance.")
    .setSystemPromptMode("replace");

var session = client.createSession(config);
```

### Customize mode with sections

```java
import com.github.copilot.sdk.SystemPromptSection;

List<SystemPromptSection> sections = List.of(
    new SystemPromptSection("role", "You are an expert Java developer."),
    new SystemPromptSection("constraints", "Always suggest using modern Java 21+ features."),
    new SystemPromptSection("output", "Format responses as Markdown with code blocks.")
);

SessionConfig config = new SessionConfig()
    .setSystemPromptSections(sections)
    .setSystemPromptMode("customize");

var session = client.createSession(config);
```

## Per-Agent Skills

Preload skill content into agent context at startup:

```java
SessionConfig config = new SessionConfig()
    .setSkillDirectories(List.of(
        "/workspace/.copilot/skills",
        "/workspace/custom-skills"
    ))
    .setDisabledSkills(List.of("deprecated-skill"));

var session = client.createSession(config);
```

## Per-Agent Tool Visibility

Control tool access per agent with excludedTools:

```java
import com.github.copilot.sdk.AgentConfig;

AgentConfig defaultAgent = new AgentConfig()
    .setExcludedTools(List.of(
        "shell_exec",
        "file_delete",
        "network_request"
    ));

SessionConfig config = new SessionConfig()
    .setDefaultAgent(defaultAgent);

var session = client.createSession(config);
```

## Runtime Request Headers

Send custom HTTP headers per message turn:

```java
import com.github.copilot.sdk.SendOptions;

Map<String, String> headers = Map.of(
    "X-Request-Id", UUID.randomUUID().toString(),
    "X-Tenant-Id", "tenant-abc-123"
);

SendOptions options = new SendOptions()
    .setRequestHeaders(headers);

session.send("Analyze this code", options);
```

## Model Capabilities Override

Deep-merge overrides for model feature flags:

```java
Map<String, Object> capabilities = Map.of(
    "streaming", true,
    "functionCalling", true,
    "vision", false,
    "maxTokens", 8192
);

SessionConfig config = new SessionConfig()
    .setModelCapabilities(capabilities);

var session = client.createSession(config);
```

## Config Discovery

Auto-detect MCP servers and skill directories from workspace:

```java
SessionConfig config = new SessionConfig()
    .setEnableConfigDiscovery(true);

var session = client.createSession(config);
// MCP servers and skills from .copilot/ in the workspace are loaded automatically
```

## Sub-Agent Streaming Events

Control streaming event forwarding from sub-agents:

```java
SessionConfig config = new SessionConfig()
    .setIncludeSubAgentStreamingEvents(true);

var session = client.createSession(config);

session.onEvent("assistant.message_delta", (event) -> {
    // Receives deltas from both the primary agent and sub-agents
    System.out.print(event.getDelta());
});
```

## Session Metadata

Retrieve session metadata via RPC:

```java
var session = client.createSession(config);
session.send("Hello");

var metadata = session.getSessionMetadata();
System.out.println("Session ID: " + metadata.getSessionId());
System.out.println("Created at: " + metadata.getCreatedAt());
System.out.println("Turn count: " + metadata.getTurnCount());
```

## MCP Server Configuration

Configure MCP servers with stdio and HTTP transport types:

```java
import com.github.copilot.sdk.McpServerConfig;

List<McpServerConfig> mcpServers = List.of(
    // Stdio transport
    new McpServerConfig()
        .setName("local-tools")
        .setTransportType("stdio")
        .setCommand("node")
        .setArgs(List.of("./mcp-server/index.js")),

    // HTTP transport
    new McpServerConfig()
        .setName("remote-tools")
        .setTransportType("http")
        .setUrl("https://mcp.example.com/api")
        .setHeaders(Map.of("Authorization", "Bearer " + token))
);

SessionConfig config = new SessionConfig()
    .setMcpServers(mcpServers);

var session = client.createSession(config);
```

## Image Generation

Configure response format and image options:

```java
SessionConfig config = new SessionConfig()
    .setResponseFormat("image");

var session = client.createSession(config);

var response = session.send("Generate a UML class diagram for a user auth system");
// Response contains image data
byte[] imageData = response.getImageData();
Files.write(Path.of("diagram.png"), imageData);
```
