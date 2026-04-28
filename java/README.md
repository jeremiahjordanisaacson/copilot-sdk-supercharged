# GitHub Copilot Supercharged SDK for Java

Java SDK for programmatic control of GitHub Copilot CLI via JSON-RPC.

[![Build](https://github.com/github/copilot-sdk-java/actions/workflows/build-test.yml/badge.svg)](https://github.com/github/copilot-sdk-java/actions/workflows/build-test.yml)
[![Maven Central](https://img.shields.io/maven-central/v/com.github/copilot-sdk-java)](https://central.sonatype.com/artifact/com.github/copilot-sdk-java)
[![Java 17+](https://img.shields.io/badge/Java-17%2B-blue?logo=openjdk&logoColor=white)](https://openjdk.org/)
[![Documentation](https://img.shields.io/badge/docs-online-brightgreen)](https://github.github.io/copilot-sdk-java/)
[![Javadoc](https://javadoc.io/badge2/com.github/copilot-sdk-java/javadoc.svg)](https://javadoc.io/doc/com.github/copilot-sdk-java/latest/index.html)

## Requirements

- Java 17+
- Maven 3.8+ or Gradle 8+

## Installation

### Maven

```xml
<dependency>
    <groupId>com.github.copilot.supercharged</groupId>
    <artifactId>copilot-sdk-supercharged</artifactId>
    <version>0.1.0</version>
</dependency>
```

### Gradle

```groovy
implementation 'com.github.copilot.supercharged:copilot-sdk-supercharged:0.1.0'
```

## Quick Start

**📦 The upstream Java SDK is maintained in a separate repository: [`github/copilot-sdk-java`](https://github.com/github/copilot-sdk-java)**

> **Note:** This SDK is in public preview and may change in breaking ways.

```java
import com.github.copilot.sdk.CopilotClient;
import com.github.copilot.sdk.events.AssistantMessageEvent;
import com.github.copilot.sdk.events.SessionIdleEvent;
import com.github.copilot.sdk.json.MessageOptions;
import com.github.copilot.sdk.json.PermissionHandler;
import com.github.copilot.sdk.json.SessionConfig;

public class QuickStart {
    public static void main(String[] args) throws Exception {
        // Create and start client
        try (var client = new CopilotClient()) {
            client.start().get();

            // Create a session (onPermissionRequest is required)
            var session = client.createSession(
                new SessionConfig()
                    .setModel("gpt-5")
                    .setOnPermissionRequest(PermissionHandler.APPROVE_ALL)
            ).get();

            var done = new java.util.concurrent.CompletableFuture<Void>();

            // Handle events
            session.on(AssistantMessageEvent.class, msg ->
                System.out.println(msg.getData().content()));
            session.on(SessionIdleEvent.class, idle ->
                done.complete(null));

            // Send a message and wait for completion
            session.send(new MessageOptions().setPrompt("What is 2+2?"));
            done.get();
        }
    }
}
```

## Custom Tools

```java
Tool weatherTool = DefineTool.create("get_weather")
    .description("Get weather for a location")
    .parameters(Map.of(
        "type", "object",
        "properties", Map.of(
            "location", Map.of("type", "string", "description", "City name")
        ),
        "required", List.of("location")
    ))
    .handler((args, invocation) -> {
        Map<String, Object> argMap = (Map<String, Object>) args;
        return "72°F in " + argMap.get("location");
    })
    .build();

CopilotSession session = client.createSession(
    new SessionConfig().tools(List.of(weatherTool))
);
```

### Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```java
CopilotClient client = new CopilotClient(
    new CopilotClientOptions().setSessionIdleTimeoutSeconds(300)
);
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```java
CopilotClientOptions opts = new CopilotClientOptions()
    .setSessionFs(new SessionFsConfig("/repo", "/tmp/state", "posix"));
```

### Session Metadata

Retrieve metadata about a session (model, creation time, status):

```java
SessionMetadata meta = client.getSessionMetadata("session-123");
```

### Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```java
SessionConfig config = new SessionConfig()
    .setSkillDirectories(List.of("./skills"))
    .setDisabledSkills(List.of("test-skill"))
    .setIncludeSubAgentStreamingEvents(true);
```

- `skillDirectories` - Paths to directories containing skill definitions
- `disabledSkills` - Skills to exclude from the session
- `includeSubAgentStreamingEvents` - When `true`, receive streaming events from sub-agents

## Custom Provider (BYOK)

```java
ProviderConfig provider = new ProviderConfig();
provider.type = "openai";
provider.baseUrl = "http://localhost:11434/v1";

CopilotSession session = client.createSession(
    new SessionConfig().model("llama3").provider(provider)
);
```

## Streaming

```java
CopilotSession session = client.createSession(
    new SessionConfig().streaming(true)
);

session.on("assistant.message_delta", event -> {
    System.out.print(event.data.get("deltaContent"));
});
```

## Try it with JBang

Run the SDK without setting up a full project using [JBang](https://www.jbang.dev/):

```bash
jbang https://github.com/github/copilot-sdk-java/blob/main/jbang-example.java
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
| `on(handler)` | Subscribe to lifecycle events |

### CopilotSession

| Method | Description |
|--------|-------------|
| `send(options)` | Send a message |
| `sendAndWait(options)` | Send and wait for idle |
| `on(handler)` | Subscribe to all events |
| `on(type, handler)` | Subscribe to specific event type |
| `getMessages()` | Get conversation history |
| `abort()` | Abort current processing |
| `destroy()` | Destroy the session |

## Image Generation

Request image responses using `responseFormat` and `imageOptions`:

```java
SessionEvent response = session.sendAndWait(
    new MessageOptions("Generate a sunset over mountains")
        .responseFormat("image")
        .imageOptions(new Types.ImageOptions("1024x1024", "hd", "natural"))
);
```

## Documentation & Resources

| Resource                      | Link                                                                                                                                   |
| ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| **Full Documentation**        | [github.github.io/copilot-sdk-java](https://github.github.io/copilot-sdk-java/)                                                        |
| **Getting Started Guide**     | [Documentation](https://github.github.io/copilot-sdk-java/latest/documentation.html)                                                   |
| **API Reference (Javadoc)**   | [javadoc.io](https://javadoc.io/doc/com.github/copilot-sdk-java/latest/index.html)                                                     |
| **MCP Servers Integration**   | [MCP Guide](https://github.github.io/copilot-sdk-java/latest/mcp.html)                                                                 |
| **Cookbook**                  | [Recipes](https://github.com/github/copilot-sdk-java/tree/main/src/site/markdown/cookbook)                                             |
| **Source Code**               | [github/copilot-sdk-java](https://github.com/github/copilot-sdk-java)                                                                  |
| **Issues & Feature Requests** | [GitHub Issues](https://github.com/github/copilot-sdk-java/issues)                                                                     |
| **Releases**                  | [GitHub Releases](https://github.com/github/copilot-sdk-java/releases)                                                                 |
| **Copilot Instructions**      | [copilot-sdk-java.instructions.md](https://github.com/github/copilot-sdk-java/blob/main/instructions/copilot-sdk-java.instructions.md) |

## Contributing

Contributions are welcome! Please see the [Contributing Guide](https://github.com/github/copilot-sdk-java/blob/main/CONTRIBUTING.md) in the GitHub Copilot SDK for Java repository.

## License

MIT — see [LICENSE](https://github.com/github/copilot-sdk-java/blob/main/LICENSE) for details.
