// Copyright (c) Microsoft Corporation. All rights reserved.

/// E2E tests for the Dart SDK session lifecycle, messaging, and configuration.
///
/// These tests use the shared replaying CAPI proxy (test/harness/server.ts) and
/// snapshot YAML files to exercise real SDK code paths without hitting live APIs.

import 'dart:io';

import 'package:copilot_sdk_supercharged/copilot_sdk_supercharged.dart';
import 'package:test/test.dart';

import 'test_harness.dart';

void main() {
  late TestHarness harness;

  setUpAll(() async {
    harness = TestHarness();
    await harness.start();
  });

  tearDownAll(() async {
    await harness.stop();
  });

  /// Helper to configure the proxy for a specific test snapshot.
  Future<void> configureForTest(String snapshotRelPath) async {
    final snapshot = harness.snapshotPathPosix(snapshotRelPath);
    await harness.configure(snapshot, harness.repoRoot);
  }

  // --------------------------------------------------------------------------
  // Test: session_create_disconnect
  // --------------------------------------------------------------------------

  test('session_create_disconnect', () async {
    await configureForTest(
        'test/snapshots/session/should_create_session_with_custom_tool.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession();
      expect(session, isNotNull, reason: 'createSession returned null');
      expect(session.sessionId, isNotEmpty,
          reason: 'session ID should not be empty');

      print('  session ID: ${session.sessionId}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test: send_message
  // --------------------------------------------------------------------------

  test('send_message', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession();
      expect(session, isNotNull);

      final response = await session.sendAndWait(
        MessageOptions(prompt: 'Hello!'),
      );

      expect(response, isNotNull, reason: 'sendAndWait returned null');

      // The response should be an assistant.message event with content
      final data = response!['data'] as Map<String, dynamic>?;
      expect(data, isNotNull, reason: 'response data should not be null');

      final content = data!['content'] as String?;
      expect(content, isNotNull,
          reason: 'response content should not be null');
      expect(content, isNotEmpty,
          reason: 'response content should not be empty');

      final truncated = content!.length > 80
          ? '${content.substring(0, 80)}...'
          : content;
      print('  response (truncated): $truncated');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test: session_fs_config
  // --------------------------------------------------------------------------

  test('session_fs_config', () async {
    await configureForTest(
        'test/snapshots/session/should_create_session_with_custom_tool.yaml');

    // Create client with sessionFs configuration
    const fsConfig = SessionFsConfig(
      initialCwd: '/',
      sessionStatePath: '/session-state',
      conventions: 'posix',
    );

    final options = CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
      sessionFs: fsConfig,
    );

    // Verify the sessionFs config was accepted
    expect(options.sessionFs, isNotNull,
        reason: 'sessionFs should not be null');
    expect(options.sessionFs!.initialCwd, equals('/'),
        reason: 'initialCwd mismatch');
    expect(options.sessionFs!.sessionStatePath, equals('/session-state'),
        reason: 'sessionStatePath mismatch');
    expect(options.sessionFs!.conventions, equals('posix'),
        reason: 'conventions mismatch');

    print('  sessionFs config accepted:'
        ' initialCwd=${options.sessionFs!.initialCwd}'
        ' statePath=${options.sessionFs!.sessionStatePath}'
        ' conventions=${options.sessionFs!.conventions}');

    final client = CopilotClient(options);

    try {
      await client.start();
      // Successfully starting with sessionFs config proves it was accepted
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 4: multi_turn_conversation
  // --------------------------------------------------------------------------

  test('multi_turn_conversation', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();
      final session = await client.createSession();
      expect(session, isNotNull);

      final response1 = await session.sendAndWait(
        MessageOptions(prompt: 'Hello!'),
      );
      expect(response1, isNotNull, reason: 'first response should not be null');
      final data1 = response1!['data'] as Map<String, dynamic>?;
      expect(data1, isNotNull);
      expect(data1!['content'] as String?, isNotEmpty);

      final response2 = await session.sendAndWait(
        MessageOptions(prompt: 'Tell me more.'),
      );
      expect(response2, isNotNull,
          reason: 'second response should not be null');
      final data2 = response2!['data'] as Map<String, dynamic>?;
      expect(data2, isNotNull);
      expect(data2!['content'] as String?, isNotEmpty);

      print('  turn-1 length: ${(data1['content'] as String).length}');
      print('  turn-2 length: ${(data2['content'] as String).length}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 5: session_resume
  // --------------------------------------------------------------------------

  test('session_resume', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client1 = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    late String savedSessionId;

    try {
      await client1.start();
      final session = await client1.createSession();
      savedSessionId = session.sessionId;
      expect(savedSessionId, isNotEmpty);
      await session.destroy();
    } finally {
      await client1.stop();
    }

    // Re-configure proxy for the second client
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client2 = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client2.start();
      final resumed = await client2.resumeSession(savedSessionId);
      expect(resumed, isNotNull,
          reason: 'resumed session should not be null');
      expect(resumed.sessionId, isNotEmpty,
          reason: 'resumed session ID should not be empty');
      print('  resumed session ID: ${resumed.sessionId}');
      await resumed.destroy();
    } finally {
      await client2.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 6: session_list
  // --------------------------------------------------------------------------

  test('session_list', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session1 = await client.createSession();
      final session2 = await client.createSession();
      expect(session1, isNotNull);
      expect(session2, isNotNull);

      final sessions = await client.listSessions();
      expect(sessions, isNotNull, reason: 'listSessions returned null');
      expect(sessions.length, greaterThanOrEqualTo(2),
          reason: 'should have at least 2 sessions');

      print('  listed ${sessions.length} sessions');

      await session1.destroy();
      await session2.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 7: session_metadata
  // --------------------------------------------------------------------------

  test('session_metadata', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession();
      expect(session, isNotNull);

      final metadata = await client.getSessionMetadata(session.sessionId);
      expect(metadata, isNotNull,
          reason: 'session metadata should not be null');

      print('  metadata keys: ${metadata!.keys.toList()}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 8: session_delete
  // --------------------------------------------------------------------------

  test('session_delete', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession();
      expect(session, isNotNull);
      final sessionId = session.sessionId;

      await client.deleteSession(sessionId);

      final sessions = await client.listSessions();
      final ids = sessions.map((s) => s.sessionId).toList();
      expect(ids, isNot(contains(sessionId)),
          reason: 'deleted session should not appear in list');

      print('  session $sessionId deleted and confirmed absent');
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 9: model_list
  // --------------------------------------------------------------------------

  test('model_list', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final models = await client.listModels();
      expect(models, isNotNull, reason: 'listModels returned null');

      print('  models count: ${models.length}');
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 10: ping
  // --------------------------------------------------------------------------

  test('ping', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final result = await client.ping('hello');
      expect(result, isNotNull, reason: 'ping result should not be null');

      print('  ping result: $result');
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 11: auth_status
  // --------------------------------------------------------------------------

  test('auth_status', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final authStatus = await client.getAuthStatus();
      expect(authStatus, isNotNull,
          reason: 'auth status should not be null');

      print('  auth status: $authStatus');
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 12: client_lifecycle
  // --------------------------------------------------------------------------

  test('client_lifecycle', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();
      // If we get here without exception, start succeeded
      print('  client started successfully');
    } finally {
      await client.stop();
      print('  client stopped successfully');
    }
  });

  // --------------------------------------------------------------------------
  // Test 13: foreground_session
  // --------------------------------------------------------------------------

  test('foreground_session', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession();
      expect(session, isNotNull);

      await client.setForegroundSessionId(session.sessionId);
      final fgId = await client.getForegroundSessionId();
      expect(fgId, equals(session.sessionId),
          reason: 'foreground session ID should match');

      print('  foreground session: $fgId');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 14: tools
  // --------------------------------------------------------------------------

  test('tools', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final tool = defineTool(
        'test_tool',
        description: 'A test tool for E2E',
        parameters: {
          'type': 'object',
          'properties': {
            'input': {'type': 'string', 'description': 'test input'},
          },
        },
        handler: (args) async => {'result': 'ok'},
      );

      final session = await client.createSession(
        SessionConfig(tools: [tool]),
      );
      expect(session, isNotNull);
      expect(session.sessionId, isNotEmpty);

      final response = await session.sendAndWait(
        MessageOptions(prompt: 'Use the test_tool with input "hi"'),
      );
      expect(response, isNotNull, reason: 'tool response should not be null');

      print('  session with tool created: ${session.sessionId}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 15: streaming
  // --------------------------------------------------------------------------

  test('streaming', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession(
        SessionConfig(streaming: true),
      );
      expect(session, isNotNull);

      final collectedEvents = <Map<String, dynamic>>[];
      final sub = session.events.listen((event) {
        collectedEvents.add(event);
      });

      final response = await session.sendAndWait(
        MessageOptions(prompt: 'Hello streaming!'),
      );
      expect(response, isNotNull,
          reason: 'streaming response should not be null');

      // Give a moment for any trailing events to arrive
      await Future<void>.delayed(Duration(milliseconds: 200));
      await sub.cancel();

      print('  collected ${collectedEvents.length} events');
      expect(collectedEvents, isNotEmpty,
          reason: 'should have received streaming events');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 16: system_message_customization
  // --------------------------------------------------------------------------

  test('system_message_customization', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession(
        SessionConfig(systemMessage: 'You are a helpful test assistant.'),
      );
      expect(session, isNotNull);
      expect(session.sessionId, isNotEmpty);

      final response = await session.sendAndWait(
        MessageOptions(prompt: 'Who are you?'),
      );
      expect(response, isNotNull,
          reason: 'response with custom system message should not be null');

      print('  session with systemMessage: ${session.sessionId}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 17: session_fs_provider
  // --------------------------------------------------------------------------

  test('session_fs_provider', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final fsConfig = SessionFsConfig(
      initialCwd: '/',
      sessionStatePath: '/session-state',
      conventions: 'posix',
    );

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
      sessionFs: fsConfig,
    ));

    try {
      await client.start();

      final session = await client.createSession();
      expect(session, isNotNull);
      expect(session.sessionId, isNotEmpty);

      print('  session with FS provider: ${session.sessionId}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 18: mcp_servers_config
  // --------------------------------------------------------------------------

  test('mcp_servers_config', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final mcpServer = MCPLocalServerConfig(
        tools: ['*'],
        command: 'echo',
        args: ['hello'],
      );

      final session = await client.createSession(
        SessionConfig(mcpServers: {'test-server': mcpServer}),
      );
      expect(session, isNotNull);
      expect(session.sessionId, isNotEmpty);

      print('  session with MCP server config: ${session.sessionId}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 19: skills_config
  // --------------------------------------------------------------------------

  test('skills_config', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();

      final session = await client.createSession(
        SessionConfig(
          skillDirectories: ['/fake/skills/dir'],
          disabledSkills: ['some-disabled-skill'],
        ),
      );
      expect(session, isNotNull);
      expect(session.sessionId, isNotEmpty);

      print('  session with skills config: ${session.sessionId}');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });

  // --------------------------------------------------------------------------
  // Test 20: compaction
  // --------------------------------------------------------------------------

  test('compaction', () async {
    await configureForTest(
        'test/snapshots/session/sendandwait_blocks_until_session_idle_and_returns_final_assistant_message.yaml');

    final client = CopilotClient(CopilotClientOptions(
      cwd: harness.repoRoot,
      env: {'COPILOT_API_URL': harness.proxyUrl!},
    ));

    try {
      await client.start();
      final session = await client.createSession();
      expect(session, isNotNull);

      // Send multiple messages to exercise conversation handling
      for (var i = 0; i < 3; i++) {
        final response = await session.sendAndWait(
          MessageOptions(prompt: 'Message number ${i + 1}'),
        );
        expect(response, isNotNull,
            reason: 'response $i should not be null');

        final data = response!['data'] as Map<String, dynamic>?;
        expect(data, isNotNull, reason: 'response $i data should not be null');
        expect(data!['content'] as String?, isNotEmpty,
            reason: 'response $i content should not be empty');
      }

      print('  sent 3 messages, all received valid responses');

      await session.destroy();
    } finally {
      await client.stop();
    }
  });
}
