# Multiple Sessions in Dart

Patterns for managing multiple independent conversations with the GitHub Copilot SDK in Dart.

## Scenario

Your application needs to run several conversations at the same time, each with its own context and history, sharing a single client connection.

## Creating Multiple Sessions

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> main() async {
  final client = CopilotClient(options: CopilotOptions());
  await client.start();

  try {
    // Create separate sessions for different tasks
    final codeSession = await client.createSession(
      config: SessionConfig(systemPrompt: 'You are a Dart expert.'),
    );
    final docsSession = await client.createSession(
      config: SessionConfig(systemPrompt: 'You are a technical writer.'),
    );

    // Each session maintains its own conversation history
    final codeReply = await codeSession.sendAndWait(
      options: SendOptions(message: 'Explain null safety'),
    );
    print('Code: ${codeReply.message}');

    final docsReply = await docsSession.sendAndWait(
      options: SendOptions(message: 'Write a package README'),
    );
    print('Docs: ${docsReply.message}');
  } finally {
    await client.stop();
  }
}
```

## Session Manager Class

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

class SessionManager {
  final CopilotClient _client;
  final Map<String, CopilotSession> _sessions = {};

  SessionManager(this._client);

  Future<CopilotSession> getOrCreate(
    String name, {
    String? systemPrompt,
  }) async {
    if (_sessions.containsKey(name)) {
      return _sessions[name]!;
    }

    final session = await _client.createSession(
      config: SessionConfig(systemPrompt: systemPrompt),
    );
    _sessions[name] = session;
    return session;
  }

  void remove(String name) {
    _sessions.remove(name);
  }

  int get count => _sessions.length;

  Iterable<String> get names => _sessions.keys;
}

// Usage
Future<void> main() async {
  final client = CopilotClient(options: CopilotOptions());
  await client.start();

  final manager = SessionManager(client);

  try {
    final frontend = await manager.getOrCreate(
      'frontend',
      systemPrompt: 'You are a Flutter expert.',
    );
    final backend = await manager.getOrCreate(
      'backend',
      systemPrompt: 'You are a Dart server expert.',
    );

    final r1 = await frontend.sendAndWait(
      options: SendOptions(message: 'Explain widgets'),
    );
    print('Frontend: ${r1.message}');

    final r2 = await backend.sendAndWait(
      options: SendOptions(message: 'Explain shelf middleware'),
    );
    print('Backend: ${r2.message}');
  } finally {
    await client.stop();
  }
}
```

## Parallel Sessions with Future.wait

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> parallelSessions(CopilotClient client) async {
  final tasks = [
    {'name': 'review', 'prompt': 'Review this code', 'persona': 'You review code.'},
    {'name': 'docs', 'prompt': 'Generate API docs', 'persona': 'You write docs.'},
    {'name': 'tests', 'prompt': 'Write widget tests', 'persona': 'You write Dart tests.'},
  ];

  final futures = tasks.map((task) async {
    final session = await client.createSession(
      config: SessionConfig(systemPrompt: task['persona']!),
    );
    final response = await session.sendAndWait(
      options: SendOptions(message: task['prompt']!),
    );
    return MapEntry(task['name']!, response.message);
  });

  final results = await Future.wait(futures);

  for (final entry in results) {
    print('[${entry.key}] ${entry.value}');
  }
}
```

## Best Practices

- **Reuse a single client**: Create one `CopilotClient` and share it across all sessions.
- **Use a manager class**: Encapsulate session creation and lookup behind a clear API.
- **Use `Future.wait` for parallelism**: Run independent sessions concurrently for better throughput.
- **Set distinct system prompts**: Give each session a focused persona for better results.
- **Clean up finished sessions**: Remove sessions from the manager when they are no longer needed.
- **Always stop the client in finally**: Ensure cleanup runs even if session operations fail.
