# Error Handling in Dart

Patterns for handling errors when using the GitHub Copilot SDK in Dart.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors gracefully using Dart's async/await and try/catch patterns.

## Basic Try/Catch

```dart
import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

Future<void> main() async {
  CopilotClient? client;

  try {
    client = CopilotClient(options: CopilotOptions());
    await client.start();
    print('Client started');

    final session = await client.createSession(config: SessionConfig());
    final response = await session.sendAndWait(
      options: SendOptions(message: 'Hello from Dart!'),
    );
    print('Response: ${response.message}');
  } on CopilotConnectionException catch (e) {
    print('Connection error: $e');
  } on CopilotTimeoutException catch (e) {
    print('Timeout: $e');
  } catch (e, stackTrace) {
    print('Unexpected error: $e');
    print('Stack trace: $stackTrace');
  } finally {
    await client?.stop();
  }
}
```

## Retry with Exponential Backoff

```dart
import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

Future<CopilotClient> connectWithRetry({
  required CopilotOptions options,
  int maxRetries = 3,
}) async {
  for (var attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      final client = CopilotClient(options: options);
      await client.start();
      print('Connected on attempt $attempt');
      return client;
    } catch (e) {
      print('Attempt $attempt failed: $e');
      if (attempt == maxRetries) rethrow;

      final delay = Duration(seconds: attempt * 2);
      print('Retrying in ${delay.inSeconds}s...');
      await Future.delayed(delay);
    }
  }

  throw StateError('Unreachable');
}
```

## Timeout Handling

```dart
import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

Future<String> sendWithTimeout(
  CopilotSession session,
  String message, {
  Duration timeout = const Duration(seconds: 30),
}) async {
  try {
    final response = await session
        .sendAndWait(options: SendOptions(message: message))
        .timeout(timeout);
    return response.message;
  } on TimeoutException {
    throw CopilotTimeoutException(
      'Request timed out after ${timeout.inSeconds}s',
    );
  }
}

// Usage
Future<void> example(CopilotSession session) async {
  try {
    final reply = await sendWithTimeout(session, 'Explain null safety in Dart');
    print(reply);
  } on CopilotTimeoutException catch (e) {
    print('Timed out: $e');
  }
}
```

## Safe Session Wrapper

```dart
import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

class SafeSession {
  final CopilotClient _client;
  CopilotSession? _session;

  SafeSession(this._client);

  Future<String> ask(String message) async {
    _session ??= await _client.createSession(config: SessionConfig());

    try {
      final response = await _session!.sendAndWait(
        options: SendOptions(message: message),
      );
      return response.message;
    } catch (e) {
      // Reset the session on error so the next call creates a new one
      _session = null;
      rethrow;
    }
  }
}

// Usage
Future<void> main() async {
  final client = CopilotClient(options: CopilotOptions());
  await client.start();

  final safe = SafeSession(client);

  try {
    final answer = await safe.ask('What is a Future in Dart?');
    print(answer);
  } catch (e) {
    print('Error: $e');
  } finally {
    await client.stop();
  }
}
```

## Handling Stream Errors

```dart
import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';

Future<void> handleStreamErrors(CopilotSession session) async {
  final stream = session.sendStream(
    options: SendOptions(message: 'Stream a long response'),
  );

  await for (final event in stream) {
    try {
      if (event is MessageDelta) {
        stdout.write(event.content);
      } else if (event is MessageComplete) {
        print('\nDone.');
      }
    } catch (e) {
      print('\nStream error: $e');
      break;
    }
  }
}
```

## Best Practices

- **Use typed catch clauses**: Catch `CopilotConnectionException` and `CopilotTimeoutException` before generic `catch`.
- **Always use finally for cleanup**: Ensure `client.stop()` runs even if exceptions occur.
- **Use `.timeout()` on futures**: Dart's built-in timeout mechanism works well with async SDK calls.
- **Reset state on errors**: If a session fails, set it to null and create a fresh one on the next call.
- **Log stack traces**: Include `stackTrace` in catch clauses for debugging async errors.
- **Use null-aware operators**: Use `client?.stop()` in finally blocks to handle cases where the client was never created.
