# Persisting Sessions in Dart

Patterns for saving and resuming Copilot sessions across application restarts in Dart.

## Scenario

Your application needs to save a conversation so users can close the app and return later to continue exactly where they left off.

## Saving and Loading Session IDs

```dart
import 'dart:io';
import 'package:copilot_sdk/copilot_sdk.dart';

const sessionFile = 'session_state.txt';

Future<void> saveSessionId(String sessionId) async {
  await File(sessionFile).writeAsString(sessionId);
}

Future<String?> loadSessionId() async {
  final file = File(sessionFile);
  if (!await file.exists()) return null;

  final id = (await file.readAsString()).trim();
  return id.isNotEmpty ? id : null;
}
```

## Resuming a Session

```dart
import 'dart:io';
import 'package:copilot_sdk/copilot_sdk.dart';

Future<CopilotSession> getOrCreateSession(CopilotClient client) async {
  final savedId = await loadSessionId();

  if (savedId != null) {
    try {
      final session = await client.createSession(
        config: SessionConfig(sessionId: savedId),
      );
      print('Resumed session: $savedId');
      return session;
    } catch (e) {
      print('Could not resume session: $e');
    }
  }

  // Create a new session and save the ID
  final session = await client.createSession(config: SessionConfig());
  await saveSessionId(session.id);
  print('Created new session: ${session.id}');
  return session;
}
```

## Full Example: Persistent Chat

```dart
import 'dart:io';
import 'package:copilot_sdk/copilot_sdk.dart';

const sessionFile = 'session_state.txt';

Future<void> main() async {
  final client = CopilotClient(options: CopilotOptions());
  await client.start();

  try {
    final session = await getOrCreateSession(client);

    print('Chat (type "quit" to exit):');

    while (true) {
      stdout.write('> ');
      final input = stdin.readLineSync();
      if (input == null || input == 'quit') break;

      try {
        final response = await session.sendAndWait(
          options: SendOptions(message: input),
        );
        print('Assistant: ${response.message}');
      } catch (e) {
        print('Error: $e');
      }
    }

    print('Session saved. You can resume later.');
  } finally {
    await client.stop();
  }
}
```

## Storing Multiple Sessions with JSON

```dart
import 'dart:convert';
import 'dart:io';
import 'package:copilot_sdk/copilot_sdk.dart';

const sessionsFile = 'sessions.json';

Future<Map<String, String>> loadSessions() async {
  final file = File(sessionsFile);
  if (!await file.exists()) return {};

  final content = await file.readAsString();
  final decoded = jsonDecode(content) as Map<String, dynamic>;
  return decoded.map((key, value) => MapEntry(key, value as String));
}

Future<void> saveSessions(Map<String, String> sessions) async {
  await File(sessionsFile).writeAsString(jsonEncode(sessions));
}

Future<CopilotSession> getNamedSession(
  CopilotClient client,
  String name, {
  String? systemPrompt,
}) async {
  final sessions = await loadSessions();
  final savedId = sessions[name];

  if (savedId != null) {
    try {
      return await client.createSession(
        config: SessionConfig(sessionId: savedId),
      );
    } catch (_) {
      // Session expired, create a new one
    }
  }

  final session = await client.createSession(
    config: SessionConfig(systemPrompt: systemPrompt),
  );
  sessions[name] = session.id;
  await saveSessions(sessions);
  return session;
}
```

## Best Practices

- **Persist only the session ID**: The SDK and CLI handle conversation state internally. You just need the ID.
- **Try to resume before creating**: Attempt to restore a saved session first, then fall back to a new one.
- **Handle stale sessions**: If resuming fails (session expired), catch the error and create a fresh session.
- **Use `dart:convert` for JSON**: Store multiple session IDs in a JSON file for easy serialization.
- **Use async file I/O**: Dart's async File API avoids blocking the event loop.
- **Save on creation, not on exit**: Write the session ID immediately after creating it to avoid data loss on crashes.
