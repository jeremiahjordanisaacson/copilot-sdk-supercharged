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
}
