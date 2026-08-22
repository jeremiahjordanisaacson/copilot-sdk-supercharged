# Copilot Supercharged SDK for Dart

Dart SDK for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK follows the same architecture and protocol as all other SDKs in the [copilot-sdk-supercharged](https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged) repository (40 languages supported).

## Features

- Spawn and manage the Copilot CLI server process
- Communicate over **stdio** (default) or **TCP** transport
- JSON-RPC 2.0 with Content-Length header framing (LSP-style)
- Create, resume, list, and delete conversation sessions
- Stream session events via Dart `Stream<Map<String, dynamic>>`
- Register custom tools with JSON Schema parameters
- Handle permission requests, user input requests, and lifecycle hooks
- `sendAndWait` convenience method that blocks until `session.idle`
- Protocol version verification on connect
- Models listing with caching
- Full null safety

## Requirements

- Dart SDK >= 3.0.0
- GitHub Copilot CLI installed and accessible

## Installation

Add to your `pubspec.yaml`:

```yaml
dependencies:
  copilot_sdk_supercharged:
    path: ../dart  # or publish to pub.dev
```

Then run:

```bash
dart pub get
```

## Quick Start

```dart
import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

void main() async {
  // Create a client that spawns the CLI server.
  final client = CopilotClient(CopilotClientOptions(
    cliPath: '/path/to/copilot-cli',
  ));

  await client.start();

  // Create a session.
  final session = await client.createSession(SessionConfig(
    model: 'gpt-4o',
  ));

  // Listen for events.
  session.events.listen((event) {
    if (event['type'] == 'assistant.message') {
      print(event['data']['content']);
    }
  });

  // Send a message and wait for the full response.
  final response = await session.sendAndWait(
    MessageOptions(prompt: 'Explain async/await in Dart'),
  );

  print(response?['data']?['content']);

  // Clean up.
  await session.destroy();
  await client.stop();
}
```

## Architecture

```
+------------------+       JSON-RPC 2.0        +------------------+
|                  |  Content-Length framing     |                  |
|  CopilotClient   | <-----------------------> |  Copilot CLI     |
|  (Dart process)  |   stdio or TCP pipes       |  (server)        |
|                  |                            |                  |
+------------------+                            +------------------+
        |
        +-- CopilotSession (1 per conversation)
              |
              +-- events: Stream<Map<String, dynamic>>
              +-- send() / sendAndWait()
              +-- tools, permissions, hooks
```

### Transport

The client communicates with the Copilot CLI server using JSON-RPC 2.0 messages framed with `Content-Length` headers, identical to the Language Server Protocol:

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","id":1,"method":"ping","params":{"message":"hello"}}
```

### Message Flow

1. **Client -> Server (requests):** `ping`, `session.create`, `session.send`, `session.destroy`, `models.list`, etc.
2. **Server -> Client (notifications):** `session.event` (with session events like `assistant.message`, `session.idle`), `session.lifecycle`
3. **Server -> Client (reverse requests):** `tool.call`, `permission.request`, `userInput.request`, `hooks.invoke`

## Custom Tools

Register tools that the Copilot agent can invoke:

```dart
final myTool = defineTool(
  'search_database',
  description: 'Search the product database',
  parameters: {
    'type': 'object',
    'properties': {
      'query': {'type': 'string', 'description': 'Search query'},
      'limit': {'type': 'integer', 'description': 'Max results'},
    },
    'required': ['query'],
  },
  handler: (args, invocation) async {
    final query = (args as Map)['query'] as String;
    final limit = (args as Map)['limit'] as int? ?? 10;
    // Perform search...
    return {'results': [], 'total': 0};
  },
);

final session = await client.createSession(SessionConfig(
  tools: [myTool],
));
```

### Structured Tool Results

For fine-grained control, return a `ToolResultObject`:

```dart
handler: (args, invocation) async {
  return ToolResultObject(
    textResultForLlm: 'Found 3 matching products',
    resultType: ToolResultType.success,
    toolTelemetry: {'queryTimeMs': 42},
  );
},
```

## Permission Handling

Control what operations the agent can perform:

```dart
final session = await client.createSession(SessionConfig(
  onPermissionRequest: (request, context) async {
    // request.kind is one of: shell, write, mcp, read, url
    if (request.kind == 'read') {
      return PermissionRequestResult(kind: 'approved');
    }
    return PermissionRequestResult(
      kind: 'denied-interactively-by-user',
    );
  },
));
```

## User Input

Allow the agent to ask the user questions:

```dart
final session = await client.createSession(SessionConfig(
  onUserInputRequest: (request, context) async {
    print('Agent asks: ${request.question}');
    // Get answer from user...
    return UserInputResponse(answer: 'Yes', wasFreeform: true);
  },
));
```

## Hooks

Intercept session lifecycle events:

```dart
final session = await client.createSession(SessionConfig(
  hooks: SessionHooks(
    onPreToolUse: (input, context) async {
      print('About to run: ${input.toolName}');
      // Return null for default behavior, or modify:
      return PreToolUseHookOutput(permissionDecision: 'allow');
    },
    onPostToolUse: (input, context) async {
      print('Finished: ${input.toolName}');
      return null;
    },
    onSessionStart: (input, context) async {
      print('Session started');
      return null;
    },
    onSessionEnd: (input, context) async {
      print('Session ended: ${input.reason}');
      return null;
    },
  ),
));
```

## Event Types

The `session.events` stream delivers events matching the session event schema. Common event types:

| Event Type | Description |
|---|---|
| `session.start` | Session initialized |
| `session.resume` | Session resumed |
| `session.idle` | Processing complete |
| `session.error` | Error occurred |
| `user.message` | User message sent |
| `assistant.message` | Full assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Model reasoning output |
| `tool.execution_start` | Tool invocation started |
| `tool.execution_complete` | Tool invocation finished |
| `session.shutdown` | Session shutting down |

## Session Management

```dart
// List all sessions.
final sessions = await client.listSessions();

// Resume a previous session.
final session = await client.resumeSession('session-id-here');

// Get the most recent session.
final lastId = await client.getLastSessionId();

// Delete a session permanently.
await client.deleteSession('session-id-here');
```

## Session Idle Timeout

Configure automatic session cleanup after a period of inactivity:

```dart
final client = CopilotClient(options: CopilotClientOptions(
  sessionIdleTimeoutSeconds: 300,
));
```

## SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session, enabling persistent state across compaction boundaries and session resumes.

```dart
final client = CopilotClient(options: CopilotClientOptions(
  sessionFs: SessionFsConfig(
    initialCwd: '/repo',
    sessionStatePath: '/state',
    conventions: 'posix',
  ),
));
```

## Session Metadata

Retrieve metadata about a session (model, creation time, status):

```dart
final meta = await client.getSessionMetadata('session-123');
```

## Skills and Sub-Agent Orchestration

Register skill directories and control sub-agent behavior:

```dart
final session = await client.createSession(config: SessionConfig(
  skillDirectories: ['./skills'],
  disabledSkills: ['test-skill'],
  includeSubAgentStreamingEvents: true,
));
```

- `skillDirectories` - list of directories containing skill definitions to register with the session
- `disabledSkills` - list of skill names to explicitly disable
- `includeSubAgentStreamingEvents` - when true, surfaces streaming events from sub-agents in the event stream

## Custom Providers (BYOK)

Use your own API endpoint:

```dart
final session = await client.createSession(SessionConfig(
  provider: ProviderConfig(
    type: 'openai',
    baseUrl: 'https://api.example.com/v1',
    apiKey: 'your-api-key',
  ),
));
```

## MCP Servers

Connect to Model Context Protocol servers:

```dart
final session = await client.createSession(SessionConfig(
  mcpServers: {
    'my-server': MCPLocalServerConfig(
      tools: ['*'],
      command: 'node',
      args: ['./my-mcp-server.js'],
    ),
  },
));
```

## Connecting to an External Server

Instead of spawning a CLI process, connect to an already-running server:

```dart
final client = CopilotClient(CopilotClientOptions(
  cliUrl: 'localhost:3000',
));

await client.start();
```

## Protocol Version

The SDK verifies protocol compatibility on connect via the `ping` response.
The current SDK protocol version is **2** (matching `sdk-protocol-version.json`).

## API Reference

### CopilotClient

| Method | Description |
|---|---|
| `start()` | Connect to CLI server |
| `stop()` | Graceful shutdown |
| `forceStop()` | Forceful shutdown |
| `createSession([config])` | Create a new session |
| `resumeSession(id, [config])` | Resume existing session |
| `deleteSession(id)` | Delete session from disk |
| `listSessions()` | List all sessions |
| `getLastSessionId()` | Get most recent session ID |
| `ping([message])` | Verify connectivity |
| `getStatus()` | Get CLI version info |
| `getAuthStatus()` | Get auth status |
| `listModels()` | List available models |
| `onLifecycle(handler)` | Subscribe to lifecycle events |

### CopilotSession

| Method / Property | Description |
|---|---|
| `sessionId` | Session identifier |
| `workspacePath` | Workspace directory path |
| `events` | Broadcast stream of all events |
| `on(eventType)` | Filtered event stream |
| `send(options)` | Send a message |
| `sendAndWait(options)` | Send and wait for idle |
| `getMessages()` | Get conversation history |
| `abort()` | Abort current processing |
| `destroy()` | Destroy the session |

## Image Generation

Request image responses using `responseFormat` and `imageOptions`:

```dart
final response = await session.sendAndWait(MessageOptions(
  prompt: 'Generate a sunset over mountains',
  responseFormat: ResponseFormat.image,
  imageOptions: ImageOptions(size: '1024x1024', quality: 'hd', style: 'natural'),
));
```

## Recent Features (v2.4–v2.5)

Recent upstream syncs added a batch of session and client options for parity with `@github/copilot-sdk`. All symbols below are on `SessionConfig`, `MessageOptions`, `CopilotClientOptions`, or `SessionHooks` unless noted.

**v2.5 wave (`SessionConfig` unless noted):**

- **Reasoning effort** — `reasoningEffort` (e.g. `'minimal'`, `'low'`, `'medium'`, `'high'`, `'max'`).
- **Tool search** — `toolSearch` map; discover tools on demand instead of preloading all of them.
- **Session rewind** — `rewindEnabled` roll the conversation back to an earlier turn.
- **Content exclusion** — `contentExclusion` honor content-exclusion rules that hide files from the agent.
- **Additional directories** — `additionalDirectories` extra workspace dirs the session may access.
- **Disabled MCP servers** — `disabledMcpServers` turn off specific MCP servers per session.
- **GitHub MCP tool config** — `githubMcpToolConfig` configure the built-in GitHub MCP toolset.
- **Canvas provider** — `canvasProvider` supply a canvas rendering provider.
- **Custom agents local-only** — `customAgentsLocalOnly` restrict custom agents to local definitions.
- **Experimental mode** — `experimentalMode` opt into experimental CLI behavior.
- **User-prompt-transformed hook** — `SessionHooks.onUserPromptTransformed` fires after a prompt is transformed.
- **Permission decision context** — `PreToolUseHookOutput.decisionContext` opaque context returned with a permission reply.
- **Agent factory args** — `CustomAgentConfig.argsSchema` JSON Schema for a factory's typed arguments.
- **Built-in plugin directories** — `CopilotClientOptions.builtinPluginDirectories`.
- **In-process FFI transport** — `CopilotClientOptions.inProcess` run the CLI in-process instead of spawning a subprocess.

**v2.4 wave:**

- **BYOK bearer token provider** — `bearerTokenProvider` (receives `ProviderTokenArgs`).
- **MCP OAuth token handler** — `onMcpAuthRequest`.
- **Session citations** — `enableCitations`.
- **Excluded built-in agents** — `excludedBuiltinAgents`.
- **Session spending limits** — `sessionLimits` (`SessionLimitsConfig(maxAiCredits: ...)`).
- **OTLP telemetry protocol** — `otlpProtocol` (`'grpc'` or `'http/protobuf'`).
- **WebSocket transport** — `enableWebSocketResponses`.
- **Session memory** — `memory` (`MemoryConfiguration`).
- **Experiment assignments** — `expAssignments`.
- **Post-tool-use / pre-MCP-tool-call hooks** — `SessionHooks.onPostToolUse`, `SessionHooks.onPreMcpToolCall`.
- **Message agent mode / display prompt** — `MessageOptions.agentMode`, `MessageOptions.displayPrompt`.
- **Tool defer loading** — `ToolDefer.auto` / `ToolDefer.never`.
- **System-message sections** — `SystemMessageSection.preamble`, `SystemMessageSection.preserve`.
- **HTTP request handler** — `CopilotRequestHandler` (override `sendRequest`).
- **GitHub attachments** — `GitHubAttachment.gitHubCommit`, `GitHubAttachment.gitHubRepository`, and more.

### Reasoning effort & tool search

```dart
final session = await client.createSession(SessionConfig(
  model: 'gpt-4o',
  reasoningEffort: 'high',
  toolSearch: {'enabled': true},
));
```

### Session rewind & additional directories

```dart
final session = await client.createSession(SessionConfig(
  rewindEnabled: true,
  additionalDirectories: ['/repo/docs', '/repo/vendor'],
));
```

### Content exclusion

```dart
final session = await client.createSession(SessionConfig(
  contentExclusion: true,
));
```

### BYOK bearer token provider

```dart
final session = await client.createSession(SessionConfig(
  provider: ProviderConfig(type: 'openai', baseUrl: 'https://api.example.com/v1'),
  bearerTokenProvider: (ProviderTokenArgs args) async {
    // Resolve a fresh token, scoped to args.sessionId if needed.
    return await fetchToken(args.sessionId);
  },
));
```

### Spending limits & citations

```dart
final session = await client.createSession(SessionConfig(
  sessionLimits: SessionLimitsConfig(maxAiCredits: 5.0),
  enableCitations: true,
));
```

### In-process FFI transport

```dart
final client = CopilotClient(CopilotClientOptions(
  inProcess: true,
  builtinPluginDirectories: ['./plugins'],
));
```

## License

See [LICENSE](../LICENSE) in the repository root.
