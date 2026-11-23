// Copyright (c) Microsoft Corporation. All rights reserved.

/// Unit test for the SDK protocol 4 session payload grouping (no CLI needed).

import 'package:copilot_sdk_supercharged/src/session_params.dart';
import 'package:test/test.dart';

void main() {
  test('groups flat session params and translates the legacy provider', () {
    final tool = {'name': 'lookup', 'description': 'Look up a value'};
    expect(
      groupSessionParams({
        'model': 'm',
        'streaming': true,
        'requestPermission': true,
        'tools': [tool],
        'hooks': true,
        'disableResume': true,
        'name': 'create-only',
      }, isResume: true),
      {
        'model': 'm',
        'events': {'streaming': true},
        'permissions': {'request': true},
        'tools': {
          'definitions': [tool]
        },
        'hooks': {'hasHandlers': true},
        'suppressResumeEvent': true,
      },
    );

    expect(
      groupSessionParams({
        'model': 'gpt-4o',
        'provider': {
          'type': 'openai',
          'baseUrl': 'http://localhost:11434/v1',
          'apiKeyCommand': 'op read key',
          'maxOutputTokens': 4096,
        },
        'capi': {'autoTier': 'fast'},
        'mcpServers': {
          'fs': {'command': 'fs-server'}
        },
        'allowTranscriptRecovery': true,
      }, isResume: false),
      {
        'model': 'gpt-4o',
        'inference': {
          'providers': [
            {
              'name': 'default',
              'type': 'openai',
              'baseUrl': 'http://localhost:11434/v1',
              'apiKeyCommand': 'op read key',
            }
          ],
          'defaultProvider': 'default',
          'models': [
            {'provider': 'default', 'id': 'gpt-4o', 'maxOutputTokens': 4096}
          ],
        },
        'github': {
          'capi': {'autoTier': 'fast', 'enabled': false}
        },
        'mcp': {
          'servers': {
            'fs': {'command': 'fs-server'}
          },
          'envValueMode': 'direct',
        },
      },
    );
  });
}
