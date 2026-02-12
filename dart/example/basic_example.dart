// Copyright (c) Microsoft Corporation. All rights reserved.

/// Basic example of using the Copilot Dart SDK.
///
/// This example demonstrates:
/// - Creating a client and connecting to the CLI server
/// - Creating a session with custom tools
/// - Sending messages and receiving responses
/// - Handling events, permissions, and user input
/// - Using hooks for pre/post tool use
/// - Cleaning up resources

import 'dart:async';
import 'dart:io';

import 'package:copilot_sdk/copilot_sdk.dart';

Future<void> main() async {
  // -------------------------------------------------------------------------
  // 1. Create the client
  // -------------------------------------------------------------------------

  final client = CopilotClient(CopilotClientOptions(
    // Path to the Copilot CLI executable.
    // If omitted, defaults to 'copilot' (must be on PATH).
    cliPath: Platform.environment['COPILOT_CLI_PATH'] ?? 'copilot',

    // Use stdio transport (default). Set to false for TCP.
    useStdio: true,

    // Log level for the CLI subprocess.
    logLevel: 'info',
  ));

  // -------------------------------------------------------------------------
  // 2. Define custom tools
  // -------------------------------------------------------------------------

  final weatherTool = defineTool(
    'get_weather',
    description: 'Get current weather for a location',
    parameters: {
      'type': 'object',
      'properties': {
        'location': {
          'type': 'string',
          'description': 'City name or zip code',
        },
      },
      'required': ['location'],
    },
    handler: (args, invocation) async {
      final location = (args as Map<String, dynamic>)['location'] as String;
      print('[Tool] get_weather called for: $location');

      // Return any JSON-serializable value. Strings and Maps both work.
      return {
        'location': location,
        'temperature': 72,
        'unit': 'F',
        'conditions': 'Sunny',
      };
    },
  );

  final timeTool = defineTool(
    'get_time',
    description: 'Get the current date and time',
    handler: (args, invocation) async {
      return DateTime.now().toIso8601String();
    },
  );

  // -------------------------------------------------------------------------
  // 3. Create a session with tools, permissions, and hooks
  // -------------------------------------------------------------------------

  try {
    // Start the client (spawns CLI process, verifies protocol version).
    await client.start();
    print('Client connected. State: ${client.state}');

    // Optionally check models.
    final models = await client.listModels();
    print('Available models: ${models.map((m) => m.id).join(', ')}');

    // Create a session.
    final session = await client.createSession(SessionConfig(
      // model: 'gpt-4o', // Uncomment to specify a model.
      tools: [weatherTool, timeTool],

      // Permission handler: approve all read operations, deny writes.
      onPermissionRequest: (request, context) async {
        print('[Permission] ${request.kind} requested');
        if (request.kind == 'read') {
          return const PermissionRequestResult(kind: 'approved');
        }
        // Deny everything else.
        return const PermissionRequestResult(
          kind: 'denied-interactively-by-user',
        );
      },

      // User input handler: the agent can ask the user questions.
      onUserInputRequest: (request, context) async {
        print('[UserInput] Agent asks: ${request.question}');
        // In a real app you would prompt the user via stdin or UI.
        return const UserInputResponse(
          answer: 'Yes, proceed.',
          wasFreeform: true,
        );
      },

      // Hooks for intercepting tool lifecycle.
      hooks: SessionHooks(
        onPreToolUse: (input, context) async {
          print('[Hook] Pre-tool-use: ${input.toolName}');
          // Return null to use defaults, or return a PreToolUseHookOutput
          // to modify behavior.
          return null;
        },
        onPostToolUse: (input, context) async {
          print('[Hook] Post-tool-use: ${input.toolName}');
          return null;
        },
        onSessionStart: (input, context) async {
          print('[Hook] Session started (source: ${input.source})');
          return null;
        },
      ),
    ));

    print('Session created: ${session.sessionId}');
    if (session.workspacePath != null) {
      print('Workspace: ${session.workspacePath}');
    }

    // -----------------------------------------------------------------------
    // 4. Subscribe to events
    // -----------------------------------------------------------------------

    // Listen to all events (broadcast stream - can have multiple listeners).
    final allEventsSub = session.events.listen((event) {
      final type = event['type'] as String;
      switch (type) {
        case 'assistant.message':
          final content = (event['data'] as Map)['content'] as String?;
          print('\n--- Assistant ---\n$content\n-----------------');
          break;
        case 'assistant.message_delta':
          // Streaming delta (when streaming: true).
          final delta = (event['data'] as Map)['deltaContent'] as String?;
          if (delta != null) stdout.write(delta);
          break;
        case 'tool.execution_start':
          final data = event['data'] as Map;
          print('[Event] Tool execution started: ${data['toolName']}');
          break;
        case 'tool.execution_complete':
          final data = event['data'] as Map;
          print('[Event] Tool execution complete: '
              'success=${data['success']}');
          break;
        case 'session.idle':
          print('[Event] Session idle');
          break;
        case 'session.error':
          final data = event['data'] as Map;
          print('[Event] Session error: ${data['message']}');
          break;
        default:
          // Other event types: session.start, user.message, etc.
          break;
      }
    });

    // You can also listen to a specific event type.
    final idleSub = session.on('session.idle').listen((_) {
      // This fires every time the session becomes idle.
    });

    // -----------------------------------------------------------------------
    // 5. Send a message and wait for the response
    // -----------------------------------------------------------------------

    print('\nSending message...');
    final response = await session.sendAndWait(
      MessageOptions(prompt: 'What is the weather in Seattle?'),
      timeout: const Duration(seconds: 120),
    );

    if (response != null) {
      final content =
          (response['data'] as Map<String, dynamic>?)?['content'] as String?;
      print('\nFinal response: $content');
    } else {
      print('No assistant message received.');
    }

    // -----------------------------------------------------------------------
    // 6. Send another message (session remembers context)
    // -----------------------------------------------------------------------

    final response2 = await session.sendAndWait(
      MessageOptions(prompt: 'And what time is it now?'),
    );

    if (response2 != null) {
      final content =
          (response2['data'] as Map<String, dynamic>?)?['content'] as String?;
      print('\nFinal response 2: $content');
    }

    // -----------------------------------------------------------------------
    // 7. Retrieve full conversation history
    // -----------------------------------------------------------------------

    final history = await session.getMessages();
    print('\nConversation history: ${history.length} events');

    // -----------------------------------------------------------------------
    // 8. Cleanup
    // -----------------------------------------------------------------------

    await allEventsSub.cancel();
    await idleSub.cancel();
    await session.destroy();
    print('Session destroyed.');
  } catch (e, st) {
    print('Error: $e');
    print(st);
  } finally {
    final errors = await client.stop();
    if (errors.isNotEmpty) {
      print('Cleanup errors: $errors');
    }
    print('Client stopped.');
  }
}
