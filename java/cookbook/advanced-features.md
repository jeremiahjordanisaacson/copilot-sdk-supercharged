# Advanced Features

Recipes for advanced session and client options in the GitHub Copilot SDK for Java.

The snippets on this page are fragments. They assume these imports, a started `CopilotClient` named
`client`, and the `approveAll` permission handler below:

```java
import com.github.copilot.*;
import com.github.copilot.Types.*;
import java.util.List;
import java.util.Map;
import java.util.UUID;

PermissionHandler approveAll = (request, sessionId) -> new PermissionRequestResult("approve-once");
```

## Per-Session Authentication

Run each session as a different GitHub user, for example one per tenant in a multi-tenant service:

```java
CopilotSession tenantSession = client.createSession(new SessionConfig()
    .gitHubToken(System.getenv("TENANT_ABC_GITHUB_TOKEN"))
    .onPermissionRequest(approveAll));

tenantSession.send(new MessageOptions("Hello from tenant ABC"));
```

## Session Idle Timeout

The idle timeout is a client option. When the client starts the CLI, the CLI closes sessions that
stay idle for longer than this many seconds:

```java
CopilotClient idleClient = new CopilotClient(
    new CopilotClientOptions().sessionIdleTimeoutSeconds(600)); // 10 minutes
```

## System Prompt Customization

### Append (the default mode)

Your content is added after the SDK-managed sections of the system prompt:

```java
SessionConfig config = new SessionConfig()
    .systemMessage(SystemMessageConfig.append("Always check for security problems and suggest tests."))
    .onPermissionRequest(approveAll);
```

### Replace mode

Replaces the whole system prompt, including the SDK's guardrails:

```java
SessionConfig config = new SessionConfig()
    .systemMessage(SystemMessageConfig.replace(
        "You are a Java code review assistant. Focus on correctness and performance."))
    .onPermissionRequest(approveAll);
```

### Customize mode

Override individual sections and keep the rest. `SystemPromptSection` has the section IDs and
`SectionOverrideAction` has the actions (`REPLACE`, `REMOVE`, `APPEND`, `PREPEND`). The optional
second argument is appended after all sections.

```java
SessionConfig config = new SessionConfig()
    .systemMessage(SystemMessageConfig.customize(
        Map.of(
            SystemPromptSection.TONE,
            new SectionOverride(SectionOverrideAction.REPLACE, "Be concise and direct."),
            SystemPromptSection.GUIDELINES,
            new SectionOverride(SectionOverrideAction.APPEND, "\n* Prefer Java 17 language features.")),
        "Focus on Java backend services."))
    .onPermissionRequest(approveAll);
```

## Tool Visibility

Hide built-in tools from a session, or allow only the tools you list:

```java
// Keep the agent from writing files
SessionConfig readOnly = new SessionConfig()
    .excludedTools(List.of("create", "edit"))
    .onPermissionRequest(approveAll);

// Or allow only these tools
SessionConfig viewOnly = new SessionConfig()
    .availableTools(List.of("view", "grep", "glob"))
    .onPermissionRequest(approveAll);
```

To limit the tools of a single custom agent, set `CustomAgentConfig.tools`; see
[Custom Agents](tools-and-skills.md#custom-agents).

## Runtime Request Headers

Add HTTP headers to the outbound model requests of one message, or of every message in a session:

```java
CopilotSession session = client.createSession(new SessionConfig().onPermissionRequest(approveAll));

session.send(new MessageOptions("Analyze this code")
    .requestHeaders(Map.of(
        "X-Request-Id", UUID.randomUUID().toString(),
        "X-Tenant-Id", "tenant-abc-123")));

// Session-wide headers
SessionConfig config = new SessionConfig()
    .requestHeaders(Map.of("X-Tenant-Id", "tenant-abc-123"))
    .onPermissionRequest(approveAll);
```

## Model Capabilities Override

Deep-merge overrides into the capabilities the runtime reports for the model. `supports` takes
`vision`, `toolCalls`, and `reasoningEffort` flags; `limits` takes `max_prompt_tokens`,
`max_output_tokens`, and `max_context_window_tokens`.

```java
SessionConfig config = new SessionConfig()
    .model("gpt-5")
    .modelCapabilities(Map.of(
        "supports", Map.of("vision", false),
        "limits", Map.of("max_output_tokens", 8192)))
    .onPermissionRequest(approveAll);
```

## Config Discovery

Let the runtime discover supported configuration, such as ambient user hooks. Settings you pass
explicitly take precedence. It's off by default.

```java
SessionConfig config = new SessionConfig()
    .workingDirectory("/path/to/project")
    .enableConfigDiscovery(true)
    .onPermissionRequest(approveAll);
```

## Streaming and Sub-Agent Streaming Events

With `streaming(true)` the session emits `assistant.message_delta` events with a `deltaContent`
field. Deltas from sub-agents are included by default; pass `false` to
`includeSubAgentStreamingEvents` to get only the main agent's deltas.

```java
CopilotSession session = client.createSession(new SessionConfig()
    .streaming(true)
    .includeSubAgentStreamingEvents(false)
    .onPermissionRequest(approveAll));

session.on("assistant.message_delta", event -> System.out.print(event.data.get("deltaContent")));
session.sendAndWait(new MessageOptions("Explain the visitor pattern"));
System.out.println();
```

## Session Metadata

`listSessions()` returns typed metadata for saved sessions, and `session.getMetadata()` returns the
runtime's raw metadata, with the details under a `session` key:

```java
CopilotSession session = client.createSession(new SessionConfig().onPermissionRequest(approveAll));
session.sendAndWait(new MessageOptions("Hello"));

for (SessionMetadata meta : client.listSessions()) {
    if (meta.sessionId.equals(session.getSessionId())) {
        System.out.println("Started: " + meta.startTime + ", last modified: " + meta.modifiedTime);
        System.out.println("Summary: " + meta.summary);
    }
}

Map<String, Object> raw = session.getMetadata();
System.out.println("Raw metadata: " + raw.get("session"));
```

## MCP Server Configuration

Pass MCP servers as a map from a server name to its configuration. `tools` selects the server's tools
(`"*"` for all); the model sees them as `<server-name>-<tool-name>`.

```java
MCPStdioServerConfig localServer = new MCPStdioServerConfig("node", List.of("./mcp-server/index.js"));
localServer.tools = List.of("*");

MCPHTTPServerConfig remoteServer = new MCPHTTPServerConfig("https://mcp.example.com/mcp");
remoteServer.type = "http"; // streamable HTTP; the constructor defaults to "sse"
remoteServer.headers = Map.of("Authorization", "Bearer " + System.getenv("MCP_TOKEN"));
remoteServer.tools = List.of("*");

SessionConfig config = new SessionConfig()
    .mcpServers(Map.of("local-tools", localServer, "remote-tools", remoteServer))
    .onPermissionRequest(approveAll);
```

## Structured Output

`sendAndWaitStructured` asks for a reply that matches a JSON Schema and returns it parsed as a
Jackson `JsonNode`:

```java
Map<String, Object> schema = Map.of(
    "type", "object",
    "properties", Map.of(
        "language", Map.of("type", "string"),
        "rating", Map.of("type", "integer")),
    "required", List.of("language", "rating"));

CopilotSession session = client.createSession(new SessionConfig().onPermissionRequest(approveAll));
com.fasterxml.jackson.databind.JsonNode result =
    session.sendAndWaitStructured("Name one JVM language and rate it from 1 to 5", schema);
System.out.println(result.get("language").asText() + ": " + result.get("rating").asInt());
```

## Features Without a Recipe

These options exist in the API, but the SDK doesn't handle the runtime's callbacks for them yet, so
a recipe wouldn't work as written:

- Slash commands (`SessionConfig.commands`) and elicitation (`SessionConfig.onElicitationRequest`): the SDK declares them to the runtime but doesn't call your command or elicitation handlers.
- Custom session filesystem providers (`Types.SessionFsProvider`): `CopilotClientOptions.sessionFs` registers a session filesystem, but the SDK doesn't answer the runtime's `sessionFs.*` requests.
