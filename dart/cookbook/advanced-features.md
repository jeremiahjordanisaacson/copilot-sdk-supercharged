# Advanced v2.0 Features in Dart

Recipes for using advanced v2.0 features of the GitHub Copilot SDK in Dart, including per-session auth, SessionFs, commands, system prompts, skills, streaming, and more.

## Scenario

Your application needs fine-grained control over authentication, filesystem access, prompt customization, agent skills, and other advanced capabilities introduced in SDK v2.0.

## 1. Per-Session Authentication

Supply a GitHub token per session instead of globally.

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> main() async {
  final client = CopilotClient(CopilotOptions());
  await client.start();

  final session = await client.createSession(
    config: SessionConfig(
      githubToken: 'ghu_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    ),
  );

  final response = await session.sendAndWait(
    message: 'Who am I authenticated as?',
  );
  print(response.message);

  await client.stop();
}
```

## 2. Session Idle Timeout

Automatically expire sessions after a period of inactivity.

```dart
final session = await client.createSession(
  config: SessionConfig(
    sessionIdleTimeoutSeconds: 600, // 10-minute idle timeout
  ),
);

// Session will be cleaned up automatically if idle for 10 minutes
final response = await session.sendAndWait(
  message: 'This session expires after 10 minutes of inactivity.',
);
```

## 3. SessionFs (Session Filesystem)

Configure a filesystem provider with the 10 required I/O operations.

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

// Implement the 10 required I/O operations
class MySessionFsProvider implements SessionFsProvider {
  @override
  Future<FsResult> readFile(String path) async {
    return FsResult(content: 'file content here');
  }

  @override
  Future<FsResult> writeFile(String path, String content) async {
    return FsResult(success: true);
  }

  @override
  Future<FsResult> deleteFile(String path) async {
    return FsResult(success: true);
  }

  @override
  Future<FsResult> listDirectory(String path) async {
    return FsResult(entries: ['file1.txt', 'file2.txt']);
  }

  @override
  Future<FsResult> createDirectory(String path) async {
    return FsResult(success: true);
  }

  @override
  Future<FsResult> stat(String path) async {
    return FsResult(size: 1024, isDirectory: false);
  }

  @override
  Future<FsResult> rename(String oldPath, String newPath) async {
    return FsResult(success: true);
  }

  @override
  Future<FsResult> copy(String src, String dst) async {
    return FsResult(success: true);
  }

  @override
  Future<FsResult> exists(String path) async {
    return FsResult(exists: true);
  }

  @override
  Future<FsResult> watch(String path, FsWatchCallback callback) async {
    return FsResult(success: true);
  }
}

Future<void> main() async {
  final client = CopilotClient(CopilotOptions());
  await client.start();

  final session = await client.createSession(
    config: SessionConfig(
      sessionFs: SessionFsConfig(
        enabled: true,
        rootPath: '/workspace/project',
        provider: MySessionFsProvider(),
      ),
    ),
  );

  final response = await session.sendAndWait(
    message: 'List the files in the workspace.',
  );
  print(response.message);

  await client.stop();
}
```

## 4. Commands and UI Elicitation

Register slash commands and handle elicitation requests from the agent.

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> main() async {
  final client = CopilotClient(CopilotOptions());
  await client.start();

  final session = await client.createSession(
    config: SessionConfig(
      commands: [
        Command(name: '/deploy', description: 'Deploy the current project'),
        Command(name: '/test', description: 'Run the test suite'),
        Command(name: '/status', description: 'Show project status'),
      ],
      elicitationHandler: (ElicitationRequest req) async {
        print('Agent asks: ${req.message}');
        if (req.type == ElicitationType.confirmation) {
          return ElicitationResponse(confirmed: true);
        }
        return ElicitationResponse(text: 'user-provided-value');
      },
    ),
  );

  final response = await session.sendAndWait(
    message: '/deploy to staging',
  );
  print(response.message);

  await client.stop();
}
```

## 5. System Prompt Customization

Use replace or customize modes with structured sections.

```dart
// Mode 1: Replace the entire system prompt
final session1 = await client.createSession(
  config: SessionConfig(
    systemPrompt: SystemPrompt(
      mode: SystemPromptMode.replace,
      content: 'You are a Dart/Flutter expert. Only discuss Dart topics.',
    ),
  ),
);

// Mode 2: Customize with structured sections
final session2 = await client.createSession(
  config: SessionConfig(
    systemPrompt: SystemPrompt(
      mode: SystemPromptMode.customize,
      sections: [
        PromptSection(
          id: 'role',
          content: 'You are a senior Dart developer.',
        ),
        PromptSection(
          id: 'constraints',
          content: 'Always use null safety. Prefer immutable data classes.',
        ),
        PromptSection(
          id: 'output_format',
          content: 'Format all code with dart format conventions.',
        ),
      ],
    ),
  ),
);
```

## 6. Per-Agent Skills

Configure skill directories and disable specific skills per agent.

```dart
final session = await client.createSession(
  config: SessionConfig(
    skillDirectories: [
      '/home/user/.copilot/skills',
      '/project/.copilot/skills',
    ],
    disabledSkills: ['web-search', 'code-execution'],
  ),
);

final response = await session.sendAndWait(
  message: 'Use the custom project skills to analyze the codebase.',
);
```

## 7. Per-Agent Tool Visibility

Hide specific tools from certain agents using excludedTools.

```dart
// Create a read-only agent that cannot modify files
final reviewer = await client.createSession(
  config: SessionConfig(
    systemPrompt: SystemPrompt(
      mode: SystemPromptMode.replace,
      content: 'You are a code reviewer. Analyze but do not modify.',
    ),
    excludedTools: [
      'write_file',
      'delete_file',
      'execute_command',
      'create_directory',
    ],
  ),
);

// Create a full-access agent
final developer = await client.createSession(
  config: SessionConfig(
    systemPrompt: SystemPrompt(
      mode: SystemPromptMode.replace,
      content: 'You are a developer with full access.',
    ),
  ),
);
```

## 8. Runtime Request Headers

Attach custom headers to individual requests for tracing or auth.

```dart
final response = await session.sendAndWait(
  message: 'Summarize the latest deploy logs.',
  requestHeaders: {
    'X-Request-Id': 'req-abc-123',
    'X-Trace-Id': 'trace-xyz-789',
    'Authorization': 'Bearer custom-token-here',
  },
);
```

## 9. Model Capabilities Override

Override model capabilities for a session to control behavior.

```dart
final session = await client.createSession(
  config: SessionConfig(
    modelCapabilities: ModelCapabilities(
      streaming: true,
      toolCalling: true,
      vision: false,
      maxTokens: 8192,
      contextWindow: 128000,
    ),
  ),
);
```

## 10. Config Discovery

Enable automatic discovery of project-level configuration files.

```dart
final session = await client.createSession(
  config: SessionConfig(
    enableConfigDiscovery: true,
  ),
);

// The SDK will automatically discover and load:
//   .copilot/config.yml
//   .copilot/prompts/*.md
//   .copilot/skills/
// from the project root and parent directories.
final response = await session.sendAndWait(
  message: 'What project configuration did you discover?',
);
```

## 11. Sub-Agent Streaming Events

Receive streaming events from sub-agents during orchestration.

```dart
final session = await client.createSession(
  config: SessionConfig(
    includeSubAgentStreamingEvents: true,
  ),
);

session.onEvent.listen((SessionEvent event) {
  if (event.type == 'assistant.message_delta') {
    stdout.write('[delta] ${event.content}');
  } else if (event.type == 'sub_agent.message_delta') {
    stdout.write('[sub-agent:${event.agentId}] ${event.content}');
  }
});

await session.send(message: 'Coordinate the planner and coder agents.');
```

## 12. Session Metadata

Retrieve metadata about the current session.

```dart
final metadata = await session.getSessionMetadata();
print('Session ID:  ${metadata.sessionId}');
print('Created at:  ${metadata.createdAt}');
print('Turn count:  ${metadata.turnCount}');
print('Model:       ${metadata.model}');
print('Token usage: ${metadata.totalTokens}');
```

## 13. MCP Server Configuration

Configure Model Context Protocol servers using stdio or HTTP transports.

```dart
final session = await client.createSession(
  config: SessionConfig(
    mcpServers: [
      // Stdio transport: launch a local MCP server process
      McpServerConfig(
        name: 'filesystem',
        transport: McpTransport.stdio,
        command: 'npx',
        args: ['-y', '@modelcontextprotocol/server-filesystem', '/workspace'],
      ),
      // HTTP transport: connect to a remote MCP server
      McpServerConfig(
        name: 'remote-db',
        transport: McpTransport.http,
        url: 'https://mcp.example.com/db',
        headers: {'Authorization': 'Bearer token123'},
      ),
    ],
  ),
);

final response = await session.sendAndWait(
  message: 'List the files in the workspace using the MCP server.',
);
```

## 14. Image Generation

Configure the response format to request image generation.

```dart
import 'dart:io';
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> main() async {
  final client = CopilotClient(CopilotOptions());
  await client.start();

  final session = await client.createSession(
    config: SessionConfig(
      responseFormat: ResponseFormat(
        type: ResponseFormatType.imageGeneration,
        imageSize: '1024x1024',
        imageQuality: 'high',
      ),
    ),
  );

  final response = await session.sendAndWait(
    message: 'Generate an image of a futuristic cityscape at sunset.',
  );

  // Save the generated image
  if (response.imageData != null && response.imageData!.isNotEmpty) {
    await File('cityscape.png').writeAsBytes(response.imageData!);
    print('Image saved to cityscape.png');
  }

  await client.stop();
}
```

## Best Practices

- **Scope tokens tightly**: Use per-session `githubToken` with minimal scopes for multi-tenant apps.
- **Use named parameters**: Dart's named parameters make config construction readable and self-documenting.
- **Combine features**: Per-agent skills, tool visibility, and system prompts work together to create specialized agents.
- **Use config discovery in dev**: Enable `enableConfigDiscovery` during development so project-level configs are picked up automatically.
- **Handle streams with listeners**: Use Dart's `Stream.listen` for delta events to process them reactively.
- **Set idle timeouts in server apps**: Use `sessionIdleTimeoutSeconds` to prevent resource leaks from abandoned sessions.
