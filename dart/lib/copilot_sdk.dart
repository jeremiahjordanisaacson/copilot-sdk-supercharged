// Copyright (c) Microsoft Corporation. All rights reserved.

/// Dart SDK for programmatic control of GitHub Copilot CLI.
///
/// This library provides a JSON-RPC 2.0 based client for interacting with the
/// GitHub Copilot CLI server over stdio or TCP transport.
///
/// ## Quick Start
///
/// ```dart
/// import 'package:copilot_sdk/copilot_sdk.dart';
///
/// void main() async {
///   final client = CopilotClient(CopilotClientOptions(
///     cliPath: '/path/to/copilot-cli',
///   ));
///
///   await client.start();
///   final session = await client.createSession();
///
///   session.events.listen((event) {
///     if (event['type'] == 'assistant.message') {
///       print(event['data']['content']);
///     }
///   });
///
///   await session.sendAndWait(MessageOptions(prompt: 'Hello!'));
///   await session.destroy();
///   await client.stop();
/// }
/// ```
library copilot_sdk;

export 'src/sdk_protocol_version.dart';
export 'src/types.dart';
export 'src/json_rpc_client.dart';
export 'src/client.dart';
export 'src/session.dart';
export 'src/define_tool.dart';
