# Tools and Skills in Dart

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in Dart.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

final timeTool = DefineTool(
  name: 'get_current_time',
  description: 'Returns the current date and time',
  handler: (context) async {
    final now = DateTime.now().toIso8601String();
    return ToolResult(content: now);
  },
);
```

## Defining a Tool with Parameters

```dart
import 'dart:io';
import 'package:copilot_sdk/copilot_sdk.dart';

final readFileTool = DefineTool(
  name: 'read_file',
  description: 'Reads the contents of a file given its path',
  handler: (context) async {
    final path = context.getParam<String>('path');
    if (path == null || path.isEmpty) {
      return ToolResult(error: 'Missing required parameter: path');
    }

    final file = File(path);
    if (!await file.exists()) {
      return ToolResult(error: 'File not found: $path');
    }

    final content = await file.readAsString();
    return ToolResult(content: content);
  },
);
```

## Registering Tools with a Client

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> main() async {
  final client = CopilotClient(
    options: CopilotOptions(),
    tools: [timeTool, readFileTool],
  );
  await client.start();

  try {
    final session = await client.createSession(
      config: SessionConfig(
        systemPrompt: 'You have access to file reading and time tools.',
      ),
    );

    final response = await session.sendAndWait(
      options: SendOptions(message: 'What time is it right now?'),
    );
    print(response.message);
  } finally {
    await client.stop();
  }
}
```

## Defining Multiple Related Tools (Skill Pattern)

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

class KVStoreSkill {
  final Map<String, String> _store = {};

  List<ToolDefinition> get tools => [
    DefineTool(
      name: 'kv_get',
      description: 'Get a value by key',
      handler: (context) async {
        final key = context.getParam<String>('key') ?? '';
        final value = _store[key];
        if (value == null) {
          return ToolResult(content: 'Key not found: $key');
        }
        return ToolResult(content: value);
      },
    ),
    DefineTool(
      name: 'kv_set',
      description: 'Set a key-value pair',
      handler: (context) async {
        final key = context.getParam<String>('key') ?? '';
        final value = context.getParam<String>('value') ?? '';
        _store[key] = value;
        return ToolResult(content: 'Stored: $key');
      },
    ),
    DefineTool(
      name: 'kv_list',
      description: 'List all keys in the store',
      handler: (context) async {
        if (_store.isEmpty) {
          return ToolResult(content: 'Store is empty');
        }
        return ToolResult(content: _store.keys.join('\n'));
      },
    ),
  ];
}

// Usage
Future<void> main() async {
  final kvSkill = KVStoreSkill();

  final client = CopilotClient(
    options: CopilotOptions(),
    tools: kvSkill.tools,
  );
  await client.start();

  try {
    final session = await client.createSession(config: SessionConfig());
    final response = await session.sendAndWait(
      options: SendOptions(
        message: "Store my name as 'Alice' and then retrieve it",
      ),
    );
    print(response.message);
  } finally {
    await client.stop();
  }
}
```

## Sub-Agent Orchestration

```dart
import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> orchestrate(CopilotClient client) async {
  // Create specialized sessions as sub-agents
  final planner = await client.createSession(
    config: SessionConfig(systemPrompt: 'You are a planner. Break tasks into steps.'),
  );
  final coder = await client.createSession(
    config: SessionConfig(systemPrompt: 'You are a Dart developer.'),
  );
  final reviewer = await client.createSession(
    config: SessionConfig(systemPrompt: 'You review Dart code for bugs.'),
  );

  // Step 1: Plan
  final plan = await planner.sendAndWait(
    options: SendOptions(message: 'Plan a REST API client in Dart'),
  );
  print('Plan: ${plan.message}');

  // Step 2: Code
  final code = await coder.sendAndWait(
    options: SendOptions(message: 'Implement this plan:\n${plan.message}'),
  );
  print('Code: ${code.message}');

  // Step 3: Review
  final review = await reviewer.sendAndWait(
    options: SendOptions(message: 'Review this code:\n${code.message}'),
  );
  print('Review: ${review.message}');
}
```

## Best Practices

- **Keep tool handlers async**: Dart tools are naturally async, so use `await` for I/O operations.
- **Validate parameters early**: Check for null or empty values and return a `ToolResult(error:)`.
- **Group related tools into skill classes**: Use a class to encapsulate shared state across related tools.
- **Pass tools in the client constructor**: Register tools when creating the client for clean initialization.
- **Use separate sessions for sub-agents**: Each agent gets its own system prompt and conversation context.
- **Chain agent outputs with string interpolation**: Pass one agent's output as input to the next using Dart's `${}` syntax.
