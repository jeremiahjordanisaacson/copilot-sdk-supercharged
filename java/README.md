# GitHub Copilot Supercharged SDK for Java

Java SDK for programmatic control of GitHub Copilot CLI via JSON-RPC.

[![Build](https://github.com/github/copilot-sdk-java/actions/workflows/build-test.yml/badge.svg)](https://github.com/github/copilot-sdk-java/actions/workflows/build-test.yml)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.jeremiahjordanisaacson/copilot-sdk-supercharged)](https://central.sonatype.com/artifact/io.github.jeremiahjordanisaacson/copilot-sdk-supercharged)
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
    <groupId>io.github.jeremiahjordanisaacson</groupId>
    <artifactId>copilot-sdk-supercharged</artifactId>
    <version>3.1.0</version>
</dependency>
```

### Gradle

```groovy
implementation 'io.github.jeremiahjordanisaacson:copilot-sdk-supercharged:3.1.0'
```

## Quick Start

**📦 The upstream Java SDK is maintained in a separate repository: [`github/copilot-sdk-java`](https://github.com/github/copilot-sdk-java)**

> **Note:** This SDK is in public preview and may change in breaking ways.

```java
import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types.PermissionRequestResult;

public class QuickStart {
    public static void main(String[] args) throws Exception {
        // Spawns the `copilot` CLI found on PATH (or COPILOT_CLI_PATH).
        // Use new CopilotClientOptions().cliUrl("localhost:3000") to connect to a running server instead.
        CopilotClient client = new CopilotClient();
        try {
            client.start();

            // Approve every permission request (replace this with your own policy)
            CopilotSession session = client.createSession(new SessionConfig()
                .model("gpt-5")
                .onPermissionRequest((request, sessionId) -> new PermissionRequestResult("approve-once")));

            // Print each assistant message as it arrives
            session.on("assistant.message", event -> System.out.println(event.data.get("content")));

            // Send a message and block until the session is idle (60 second default timeout)
            session.sendAndWait(new MessageOptions("What is 2+2?"));
        } finally {
            client.stop(); // also detaches the client's sessions
        }
    }
}
```

The snippets in the rest of this README are fragments. They assume `import com.github.copilot.*;`,
`import com.github.copilot.Types.*;`, the `java.util` and `java.nio.file` imports they use, and an
existing `client` and `session`.

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `CopilotClient` methods below call
server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC
method-not-found error, which is thrown like any other RPC error.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned secret store
(`mcp.secrets.save`, `mcp.secrets.delete`, `mcp.secrets.deleteServer`). The secret value is never
logged. Each method has an overload that takes a `configDir` (null selects the runtime's default).

```java
Types.McpSecretsSaveResult saved = client.mcpSecretsSave("my-server-token", System.getenv("MY_SERVER_TOKEN"));
System.out.println(saved.usedKeychain ? "Stored in the OS keychain" : "Stored in the file backend");

client.mcpSecretsDelete("my-server-token");
client.mcpSecretsDeleteServer("my-server");   // every legacy credential indexed for the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`permissions.extensionApprovals.grant`, `list` and `revoke`). `extensionApprovalsList` and
`extensionApprovalsRevoke` have overloads that take a `sessionId` to use an attached session's
configuration profile.

```java
// Persist a pending extension permission request (its requestId) before answering it normally
Types.PermissionsExtensionApprovalsGrantResult grant = client.extensionApprovalsGrant(session.getSessionId(), requestId);
if (grant.granted) System.out.println("Approved " + grant.approval.extensionName);

for (Types.PermissionsExtensionApproval approval : client.extensionApprovalsList().approvals) {
    // extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel
    if (client.extensionApprovalsRevoke(approval.extensionName, approval.installationKey).revoked) {
        System.out.println("Revoked " + approval.extensionName);
    }
}
```

### Ask-user image attachments

When the runtime accepts images in an answer, `Types.UserInputRequest.imageAttachmentConstraints`
carries `allowedMimeTypes`, `maxBytesPerImage` and `maxCount`; it is null otherwise. The request also
exposes `requestId` and `meta` (the opaque `_meta` object) when the runtime sends them. Answer with
`new Types.UserInputResponse(answer, wasFreeform, attachments)`, where each
`Types.UserInputImageAttachment` holds base64 `data`, a `mimeType`, and an optional `description` and
`displayName` (the wire `type` is always `"image"`). Attachments are sent only when set; image-only
answers use an empty answer and `wasFreeform = true`. The runtime enforces the limits.

```java
new SessionConfig().onUserInputRequest((request, sessionId) -> {
    Types.UserInputImageAttachmentConstraints constraints = request.imageAttachmentConstraints;
    if (constraints != null && constraints.allowedMimeTypes.contains("image/png")) {
        String data = Base64.getEncoder().encodeToString(Files.readAllBytes(Path.of("screenshot.png")));
        Types.UserInputImageAttachment image = new Types.UserInputImageAttachment(data, "image/png")
            .displayName("screenshot.png");
        return new Types.UserInputResponse("", true, List.of(image));
    }
    return new Types.UserInputResponse("No screenshot available", true);
});
```

## What's New in v3.0.0

This release moves the SDK to **SDK protocol 4** (the protocol of upstream SDK 2.0).

* **Requires Copilot CLI 1.0.113 or newer.** Older CLIs are rejected at `start()` with
  `SDK protocol version mismatch: SDK supports version 4, but server reports version N.`
* **Existing options keep working.** You still configure `SessionConfig` with the same flat
  builders; the SDK translates the payload into the grouped protocol 4 shape (`inference`,
  `tools`, `permissions`, `interaction`, `events`, `agents`, `mcp`, `github`, `state`, ...)
  right before `session.create` and `session.resume`.
* **Legacy BYOK keeps working.** `SessionConfig.provider(ProviderConfig)` becomes a named
  `"default"` connection that is also the default provider, and Copilot API models are disabled
  for that session, exactly as in v1. Explicit `providers`/`defaultProvider`/`capi` win.
* **Protocol 4 tool and permission flow.** Custom tools and `onPermissionRequest` are served
  through the runtime's `external_tool.requested` and `permission.requested` events, answered
  with `session.tools.handlePendingToolCall` and `session.permissions.handlePendingPermissionRequest`.

### New in v3.0.0

```java
// Named BYOK providers and model definitions (select a model as "provider/id")
CopilotSession session = client.createSession(new SessionConfig()
    .model("gateway/chat")
    .providers(List.of(new Types.NamedProviderConfig("gateway", "https://models.example.com/v1")
        .apiKeyCommand("print-model-key")
        .wireApi("responses")))
    .models(List.of(new Types.ProviderModelConfig("gateway", "chat")
        .wireModel("my-deployment")
        .maxOutputTokens(8192)))
    .capi(new Types.CapiConfig(false)));          // BYOK-only catalog

// Use a connection for unqualified model names; defaultProvider(null) clears it on resume
new SessionConfig().model("my-deployment").defaultProvider("gateway").capi(new Types.CapiConfig(false));
```

```java
// Idempotent tools: the runtime may safely retry them
Tool lookup = DefineTool.create("lookup_order")
    .description("Look up an order by ID")
    .idempotent(true)
    .handler((args, inv) -> "shipped")
    .build();

// Client turn IDs are echoed on the turn's events
session.send(new MessageOptions("Summarize the diff").clientTurnId("turn-42"));

// Resume without emitting session.resume
client.resumeSession(sessionId, new SessionConfig().suppressResumeEvent(true));
```

```java
// Client mode: "coding-agent" (default) or "empty" (every session must pick its tools,
// and telemetry, repository hooks, cross-session memory, and similar features default off)
CopilotClient client = new CopilotClient(new CopilotClientOptions().mode(Types.ClientMode.EMPTY));
CopilotSession session = client.createSession(new SessionConfig().availableTools(List.of("view", "grep")));
```

```java
// Live shell output events
session.on(Types.ToolShellOutputData.EVENT_TYPE, event -> {
    Types.ToolShellOutputData out = Types.ToolShellOutputData.fromEvent(event);
    System.out.print(out.text);                    // out.stream is "stdout", "stderr", or null
});

// Shell authentication: answer sudo/ssh prompts from programs that shell tools run
new SessionConfig().onShellAuthenticationRequest((request, sessionId) ->
    "confirm".equals(request.inputKind)
        ? Types.ShellAuthenticationResult.accept("yes")
        : Types.ShellAuthenticationResult.cancel());

// Memory store: receive the memories the agent saves (requires memory enabled)
new SessionConfig().memory(new Types.MemoryConfiguration(true)
    .storeMemory(memory -> myStore.save(memory.subject, memory.fact)));
```

```java
// Network callbacks: proxy resolution and proxy authentication (never log credentials)
CopilotClient client = new CopilotClient(new CopilotClientOptions()
    .network(new Types.NetworkConfig()
        .scope(Types.NetworkScope.CONNECTION)
        .resolveProxy(request -> "PROXY proxy.corp.example:8080; DIRECT")
        .authenticateBasic(request -> new Types.NetworkBasicCredentials(proxyUser, proxyPassword))));
```

```java
// Policy Helper failures on create/resume, and permission-mode refusal reasons
try {
    client.createSession(new SessionConfig().permissionMode(Types.PermissionMode.MANUAL));
} catch (Exception e) {
    Types.PolicyHelperFailure failure = Types.PolicyHelperFailure.fromError(e);
    if (failure != null) System.err.println(failure.category + ": " + failure.message);
}

Types.PermissionModeResult result = session.setPermissionMode(Types.PermissionMode.ALLOW_ALL);
if (!result.success && Types.PermissionModeRefusalReason.MANAGED_POLICY.equals(result.reason)) {
    System.out.println("Blocked by managed policy; mode stays " + result.mode);
}
```

### Also available (shipped in v2.6.0)

* **Client info handshake**: `new CopilotClientOptions().clientInfo(new Types.ClientInfo("acme-portal", "2.4.0").integrationName("acme-plugin"))`.
* **Auto routing tier**: `session.setAutoTier(Types.AutoTier.BALANCE)` (sends `session.model.switchAutoTier`) or `SessionConfig.autoTier(...)`.
* **Ask-user variant**: `new SessionConfig().askUserVariant(Types.AskUserVariant.ELICITATION)`.
* **GitHub token provider**: `new SessionConfig().gitHubTokenProvider(args -> Types.GitHubTokenProviderResult.token(fetchToken(args.host), 3600))`.
* **Sandbox bypass**: `new SessionConfig().sandboxConfig(new Types.SandboxConfig(true).allowBypass(true))`.
* **Feature flag overrides**: `new SessionConfig().featureFlags(Map.of("my_flag", true))`.
* **Session detach**: `session.destroy()` sends `session.detach`, so the session can be resumed later.
* **Message source**: `new MessageOptions("Run the checks").source(Types.MessageSource.agent("planner"))`.
* **Managed settings cache clear**: `client.clearManagedSettingsCache()`.
* **Policy helper recheck**: `Map<String, Object> snapshot = session.recheckPolicyHelper();` (parse `snapshot.get("policyHelperFailure")` with `Types.PolicyHelperFailure.fromMap`).
* **Model allowlist**: `new SessionConfig().allowedModels(List.of("gpt-5", "claude-sonnet-4.5"))`.
* **OAuth client metadata URL**: `new SessionConfig().authClientIdMetadataUrl("https://example.com/oauth/client.json")`.
* **Max output tokens override**: `new SessionConfig().modelCapabilities(Map.of("limits", Map.of("max_output_tokens", 8192)))`.
* **Structured output**: `JsonNode json = session.sendAndWaitStructured("List three colors", schema);` or `new MessageOptions(prompt).responseSchema(schema)`.
* **Agent/hook directories**: `new SessionConfig().agentDirectories(List.of("./agents")).hookDirectories(List.of("./hooks"))`.
* **Working directory trust** (create only): `new SessionConfig().trustWorkingDirectory(Types.WorkingDirectoryTrust.SESSION)`.
* **Transcript recovery** (resume only): `new SessionConfig().allowTranscriptRecovery(true)`.
* **Image generation config**: `new SessionConfig().imageGeneration(new Types.ImageGenerationConfig(true).watermark(true).saveToDisk(false))`.
* **Autopilot options**: `new SessionConfig().autopilot(new Types.AutopilotOptions().continuationEnabled(true).maxContinues(3))`.
* **Subagent lifecycle hooks**: `new SessionHooks().onSubagentStart((input, id) -> Map.of("additionalContext", "Follow the style guide"))` and `.onSubagentStop(...)`.
* **BYOK API key command**: `provider.apiKeyCommand = "print-model-key";` on `ProviderConfig`, or `NamedProviderConfig.apiKeyCommand(...)`.

### Known limitations

* **AHP host**: the runtime-supervised AHP host (`startAhpHost` and the `ahp.*`/`host.*` RPC
  families) is not implemented. It is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
* **In-process hosting**: this SDK keeps spawning the CLI (or connecting over TCP) instead of
  hosting the runtime in-process by default; `CopilotClientOptions.inProcess` is unchanged.
* **Legacy response format**: `MessageOptions.responseFormat(String)` is no longer sent, because
  protocol 4 rejects the string form. Use `responseSchema` for structured output and
  `SessionConfig.imageGeneration` for images.
* **Callback cancellation**: Java callbacks receive no cancellation token. When the runtime
  cancels a shell authentication prompt, the SDK answers `cancel` and interrupts the handler thread.

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
    new CopilotClientOptions().sessionIdleTimeoutSeconds(300)
);
```

### SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```java
SessionFsConfig sessionFs = new SessionFsConfig();
sessionFs.initialCwd = "/repo";
sessionFs.sessionStatePath = "/tmp/state";
sessionFs.conventions = "posix";

CopilotClientOptions opts = new CopilotClientOptions().sessionFs(sessionFs);
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
    .skillDirectories(List.of("./skills"))
    .disabledSkills(List.of("test-skill"))
    .includeSubAgentStreamingEvents(true);
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

Run the Quick Start without setting up a project by using [JBang](https://www.jbang.dev/). Add these lines
to the top of `QuickStart.java`:

```java
///usr/bin/env jbang "$0" "$@" ; exit $?
//JAVA 17+
//DEPS io.github.jeremiahjordanisaacson:copilot-sdk-supercharged:3.1.0
```

Then run it:

```bash
jbang QuickStart.java
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
| `mcpSecretsSave(secretId, value[, configDir])` | Save an MCP credential (returns `McpSecretsSaveResult`) |
| `mcpSecretsDelete(secretId[, configDir])` | Delete an MCP credential |
| `mcpSecretsDeleteServer(serverName[, configDir])` | Delete the MCP credentials indexed for a server |
| `extensionApprovalsGrant(sessionId, requestId)` | Persist a pending plugin extension approval |
| `extensionApprovalsList([sessionId])` | List plugin extension approvals |
| `extensionApprovalsRevoke(extensionName, installationKey[, sessionId])` | Revoke a plugin extension approval |
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

Opt in to image generation for the session with `SessionConfig.imageGeneration`. Per-message
`imageOptions` are still forwarded; the legacy `responseFormat` string is no longer sent because
protocol 4 rejects it (use `responseSchema` for structured output).

```java
CopilotSession session = client.createSession(
    new SessionConfig().imageGeneration(new Types.ImageGenerationConfig(true).saveToDisk(false))
);
SessionEvent response = session.sendAndWait(
    new MessageOptions("Generate a sunset over mountains")
        .imageOptions(new Types.ImageOptions("1024x1024", "hd", "natural"))
);
```

## Recent Features (v2.4–v2.5)

Parity options tracking the upstream `@github/copilot` SDK. All are exposed as fluent
builders on `SessionConfig`, `CopilotClientOptions`, `MessageOptions`, and `SessionHooks`.

### v2.5.0

- **Reasoning effort** — `SessionConfig.reasoningEffort(String)` (`"low"`/`"medium"`/`"high"`/`"xhigh"`; see `Types.ReasoningEffort`).
- **Session rewind** — `SessionConfig.rewindEnabled(boolean)`; roll back to an earlier snapshot (`session.snapshot_rewind` event).
- **Additional directories** — `SessionConfig.additionalDirectories(List<String>)`; extra workspace roots the agent may read.
- **Content exclusion** — `SessionConfig.contentExclusion(boolean)`; honor repository content-exclusion rules.
- **Tool search** — `SessionConfig.toolSearch(Map<String, Object>)`; on-demand tool discovery for large tool sets.
- **Disabled MCP servers** — `SessionConfig.disabledMcpServers(List<String>)`.
- **GitHub MCP tool config** — `SessionConfig.githubMcpToolConfig(Map<String, Object>)`.
- **Canvas provider** — `SessionConfig.canvasProvider(Map<String, Object>)`.
- **Custom agents local-only** — `SessionConfig.customAgentsLocalOnly(boolean)`.
- **Experimental mode** — `SessionConfig.experimentalMode(boolean)`.
- **User-prompt-transformed hook** — `SessionHooks.onUserPromptTransformed(HookHandler)`.
- **Built-in plugin directories** — `CopilotClientOptions.builtinPluginDirectories(List<String>)`.
- **In-process (FFI) transport** — `CopilotClientOptions.inProcess(boolean)`.
- **Agent factory args schema** — `Types.CustomAgentConfig.argsSchema`.
- **Permission decision context** — `Types.PermissionRequestResult.decisionContext`.

### v2.4.0

- **BYOK bearer token provider** — `CopilotClientOptions.bearerTokenProvider(BearerTokenProvider)` (`getBearerToken(ProviderTokenArgs)`).
- **HTTP request handler** — `CopilotClientOptions.requestHandler(CopilotRequestHandler)`.
- **MCP OAuth token handler** — `SessionConfig.onMcpAuthRequest(McpAuthHandler)`.
- **Session citations** — `SessionConfig.enableCitations(boolean)`.
- **Excluded built-in agents** — `SessionConfig.excludedBuiltinAgents(List<String>)`.
- **Session spending limits** — `SessionConfig.sessionLimits(SessionLimitsConfig)` (`maxAiCredits`).
- **Session memory** — `SessionConfig.memory(MemoryConfiguration)`.
- **OTLP protocol** — `SessionConfig.otlpProtocol(String)` (`"grpc"`/`"http/protobuf"`).
- **WebSocket responses** — `SessionConfig.enableWebSocketResponses(boolean)`.
- **Experiment assignments** — `SessionConfig.expAssignments(Map<String, Object>)`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `SessionHooks.onPostToolUse(...)`, `SessionHooks.onPreMcpToolCall(...)`.
- **Per-message agent mode & display prompt** — `MessageOptions.agentMode(String)`, `MessageOptions.displayPrompt(String)`.
- **System-message sections** — `CopilotClient.SystemMessageSection.PREAMBLE` / `.PRESERVE`.
- **Tool defer policy** — `CopilotClient.ToolDefer.AUTO` / `.NEVER` (`Tool.defer`).
- **GitHub attachment variants** — `CopilotClient.GitHubAttachment.GITHUB_COMMIT` / `.GITHUB_REPOSITORY`.

### Examples

```java
// Reasoning effort — trade latency for depth on models that support it
CopilotSession session = client.createSession(
    new SessionConfig()
        .model("gpt-5")
        .reasoningEffort("high")          // "low" | "medium" | "high" | "xhigh"
);
```

```java
// Roll back to an earlier snapshot, expose extra roots, and honor content exclusion
CopilotSession session = client.createSession(
    new SessionConfig()
        .rewindEnabled(true)              // emits session.snapshot_rewind events
        .additionalDirectories(List.of("/repo/shared", "/repo/docs"))
        .contentExclusion(true)
);
```

```java
// On-demand tool discovery for large tool sets
CopilotSession session = client.createSession(
    new SessionConfig().toolSearch(Map.of("enabled", true))   // forwarded to the CLI tool-search config
);
```

```java
// BYOK bearer tokens + in-process FFI transport (client-level)
CopilotClient client = new CopilotClient(
    new CopilotClientOptions()
        .bearerTokenProvider(args -> fetchToken(args.sessionId))
        .builtinPluginDirectories(List.of("/opt/copilot/plugins"))
        .inProcess(true)
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
