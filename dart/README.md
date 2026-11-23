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
- GitHub Copilot CLI 1.0.113 or newer (SDK protocol 4) installed and accessible

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

## What's New in v3.0.0

SDK protocol 4 support (the upstream SDK 2.0 wire format). **Requires Copilot
CLI 1.0.113 or newer**: `start()` rejects older CLIs with
`SDK protocol version mismatch: SDK supports version 4, but server reports version N.`

Existing options keep working. You still pass the flat `SessionConfig` /
`ResumeSessionConfig` options; right before `session.create` /
`session.resume` the client groups them into the protocol 4 payload
(`streaming` becomes `events.streaming`, `onPermissionRequest` becomes
`permissions.request`, `skillDirectories` becomes `skills.directories`, and so
on). A legacy `provider` is sent as a `"default"` named provider with
`inference.defaultProvider = "default"` and Copilot API models disabled
(`github.capi.enabled = false`), so v2.x BYOK sessions behave as before. A
plain-string `systemMessage` is sent as `{mode: 'append', content: ...}`, and
`destroy()` now sends `session.detach` (falling back to `session.destroy`).

```dart
import 'dart:io';

import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

Future<void> main() async {
  final client = CopilotClient(CopilotClientOptions(
    // Client mode: ClientMode.codingAgent (default) or ClientMode.empty;
    // 'copilot-cli' is accepted as an alias of 'coding-agent'.
    mode: ClientMode.codingAgent,
    // Client info: sent as `clientInfo` on the `connect` handshake.
    clientInfo: const ClientInfo(
      editorName: 'acme-portal',
      editorVersion: '2.4.0',
      extensionName: 'acme-copilot',
      extensionVersion: '0.9.0',
    ),
    // Network callbacks: the client calls `network.setProvider` after
    // connecting and answers `network.resolveProxy` with this callback.
    network: NetworkConfig(
      resolveProxy: (url) async =>
          url.contains('corp.example') ? 'PROXY proxy.corp.example:8080' : null,
    ),
  ));
  await client.start();

  final session = await client.createSession(SessionConfig(
    // Named BYOK providers: select a registered model with "<provider>/<id>".
    model: 'local/qwen3',
    providers: const [
      NamedProviderConfig(
        name: 'local',
        type: 'openai',
        baseUrl: 'http://localhost:11434/v1',
        apiKeyCommand: 'op read op://dev/llm/api-key',
      ),
    ],
    models: const [
      ProviderModelConfig(provider: 'local', id: 'qwen3', maxOutputTokens: 8192),
    ],
    defaultProvider: 'local',
    capi: const CapiSessionOptions(enabled: false),
    // Idempotent tools: `idempotent` is sent in tools.definitions[].
    tools: [
      defineTool(
        'lookup_ticket',
        description: 'Look up a ticket by ID',
        idempotent: true,
        handler: (args, invocation) async => {'status': 'open'},
      ),
    ],
    // Shell authentication: answers `shellAuthentication.request` prompts.
    onShellAuthenticationRequest: (request, context) async =>
        request.inputKind == 'confirm'
            ? const ShellAuthenticationResponse.accept('yes')
            : const ShellAuthenticationResponse.cancel(),
    // Memory store: answers `memoryStore.store` (sends state.hasMemoryStore).
    onStoreMemory: (memory, context) async {
      stdout.writeln('remember ${memory.subject}: ${memory.fact}');
    },
    // v2.6.0 options, grouped for protocol 4.
    askUserVariant: AskUserVariant.elicitation,
    sandboxConfig: const SandboxConfig(enabled: true, allowBypass: true),
    featureFlags: const {'someRuntimeFlag': true},
    allowedModels: const ['local/qwen3', 'gpt-5'],
    authClientIdMetadataUrl: 'https://example.com/oauth/client.json',
    agentDirectories: const ['./.github/agents'],
    hookDirectories: const ['./.github/hooks'],
    trustWorkingDirectory: 'session',
    imageGeneration:
        const ImageGenerationConfig(enabled: true, watermark: true, saveToDisk: true),
    autopilot: const AutopilotOptions(continuationEnabled: true, maxContinues: 5),
    modelCapabilities: const ModelCapabilitiesOverride(
      limits: ModelLimitsOverride(maxOutputTokens: 8192),
    ).toJson(),
    hooks: SessionHooks(
      onSubagentStart: (input, context) async => SubagentStartHookOutput(
          additionalContext: 'Focus for ${input.agentName}: security review'),
      onSubagentStop: (input, context) async =>
          SubagentStopHookOutput(modifiedResponse: input.response.trim()),
    ),
  ));

  // Shell output events (`tool.shell_output`).
  final shellSub = session.shellOutput.listen((chunk) {
    (chunk.stream == 'stderr' ? stderr : stdout).write(chunk.text);
  });

  // Client turn ID, message source, and structured output.
  await session.sendAndWait(MessageOptions(
    prompt: 'Summarize open tickets',
    clientTurnId: 'turn-42',
    source: MessageSource.agent('triage'),
    responseSchema: {
      'type': 'object',
      'properties': {
        'count': {'type': 'integer'},
      },
      'required': ['count'],
    },
  ));

  // Auto routing tier, policy helper recheck, permission mode refusal reason.
  await session.switchAutoTier(AutoTier.efficiency);
  await session.recheckPolicyHelper();
  final mode = await session.setPermissionMode('allow-all');
  if (!mode.success && mode.reason == PermissionModeRefusalReason.managedPolicy) {
    stdout.writeln('Managed policy keeps the current permission mode');
  }

  await shellSub.cancel();
  await session.detach(); // session.detach: on-disk state is kept

  // Resume with transcript recovery, without emitting session.resume.
  try {
    final resumed = await client.resumeSession(
      session.sessionId,
      const ResumeSessionConfig(
        suppressResumeEvent: true,
        allowTranscriptRecovery: true,
        clearDefaultProvider: true,
      ),
    );
    await resumed.detach();
  } on JsonRpcError catch (e) {
    // Policy helper failure from `error.data.policyHelperFailure`.
    final failure = e.policyHelperFailure;
    if (failure != null) stderr.writeln('${failure.category}: ${failure.message}');
    rethrow;
  }

  await client.clearManagedSettingsCache(); // managedSettings.clearCache
  await client.stop();
}
```

New in this release:

- **Protocol 4 grouped payloads**: `create`/`resume` send grouped options
  (for example `hooks: {hasHandlers: true}`); `ResumeSessionConfig.suppressResumeEvent`
  (alias `disableResume`) and `allowTranscriptRecovery` stay top level.
- **Named BYOK providers**: `providers`, `models`, `defaultProvider` (or
  `clearDefaultProvider: true` on resume), `capi`.
- **Client mode**: `CopilotClientOptions.mode`. In `ClientMode.empty`, every
  session must set `availableTools` (an `ArgumentError` otherwise) and the
  empty-mode defaults (no experimental mode, telemetry, persistent MCP tokens,
  embeddings, cross-session store, memory, on-demand instructions, repository
  hooks, or host git operations) are applied under your values.
- **Client info**, **network callbacks**, **shell authentication**,
  **memory store**, **shell output events** (`SessionEventTypes.toolShellOutput`,
  `ToolShellOutputData`), **idempotent tools**, **client turn ID**,
  **policy helper failure** (`JsonRpcError.policyHelperFailure`,
  `PolicyHelperFailure`), and **permission mode refusal reason**
  (`PermissionModeSetResult.reason`, `PermissionModeRefusalReason.managedPolicy`).
- **v2.6.0 parity**: `switchAutoTier`, `askUserVariant`, `gitHubTokenProvider`
  (answers `gitHubToken.getToken`, mutually exclusive with `gitHubToken`),
  `SandboxConfig.allowBypass`, `featureFlags`, `detach()`, `MessageSource`,
  `clearManagedSettingsCache()`, `recheckPolicyHelper()`, `allowedModels`,
  `authClientIdMetadataUrl`, `ModelLimitsOverride.maxOutputTokens`
  (`max_output_tokens`), `responseSchema`, `agentDirectories` /
  `hookDirectories`, `trustWorkingDirectory`, `allowTranscriptRecovery`,
  `ImageGenerationConfig.saveToDisk`, `AutopilotOptions.maxContinues`,
  `onSubagentStart` / `onSubagentStop` hooks, and `ProviderConfig.apiKeyCommand`.

GitHub token provider example:

```dart
final session = await client.createSession(SessionConfig(
  gitHubTokenProvider: (request) async => GitHubTokenResult.token(
    accessToken: await mintToken(request.host, request.sessionId),
    expiresIn: 7200,
  ),
));
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC
  families) is an experimental upstream surface that depends on the runtime's
  in-process hosting library; it is not available in this SDK.
- In-process native runtime hosting is not available: the client always
  spawns the CLI (stdio or TCP) or connects to `cliUrl`. The `inProcess`
  option is accepted but has no effect.
- The legacy per-message `MessageOptions.responseFormat` is no longer sent
  (protocol 4 uses `responseFormat` for structured output): use
  `responseSchema`, or the session-level `imageGeneration` option for images.
- `commands`, `onElicitationRequest`, `onExitPlanMode` and `onMcpAuthRequest`
  are accepted on `SessionConfig` but not yet wired to runtime requests.

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
The current SDK protocol version is **4** (matching `sdk-protocol-version.json`);
Copilot CLI 1.0.113 or newer is required.

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
| `clearManagedSettingsCache()` | Re-fetch enterprise managed settings (`managedSettings.clearCache`) |
| `onLifecycle(handler)` | Subscribe to lifecycle events |

### CopilotSession

| Method / Property | Description |
|---|---|
| `sessionId` | Session identifier |
| `workspacePath` | Workspace directory path |
| `events` | Broadcast stream of all events |
| `on(eventType)` | Filtered event stream |
| `shellOutput` | Stream of `tool.shell_output` chunks (`ToolShellOutputData`) |
| `send(options)` | Send a message |
| `sendAndWait(options)` | Send and wait for idle |
| `getMessages()` | Get conversation history |
| `abort()` | Abort current processing |
| `switchAutoTier(tier)` | Change the Auto routing tier (`session.model.switchAutoTier`) |
| `recheckPolicyHelper()` | Re-run the managed-settings policy helper |
| `setPermissionMode(mode)` | Set the permission mode (`PermissionModeSetResult`) |
| `detach()` | Release the session, keeping it resumable (`session.detach`) |
| `destroy()` | Same as `detach()` |

## Image Generation

Opt in to image generation for the session with `imageGeneration`:

```dart
final session = await client.createSession(const SessionConfig(
  imageGeneration: ImageGenerationConfig(enabled: true, saveToDisk: true),
));
```

The legacy per-message `responseFormat: ResponseFormat.image` is no longer sent
(SDK protocol 4 uses `responseFormat` for structured output); `imageOptions`
is still forwarded.

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
