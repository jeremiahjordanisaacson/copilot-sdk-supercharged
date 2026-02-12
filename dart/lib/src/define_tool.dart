// Copyright (c) Microsoft Corporation. All rights reserved.

/// Helper for defining tools with a concise API.

import 'types.dart';

/// Creates a [Tool] definition with the given name and configuration.
///
/// This is a convenience factory that mirrors the `defineTool` helper in the
/// TypeScript SDK.
///
/// ```dart
/// final weatherTool = defineTool(
///   'get_weather',
///   description: 'Get current weather for a location',
///   parameters: {
///     'type': 'object',
///     'properties': {
///       'location': {
///         'type': 'string',
///         'description': 'City name',
///       },
///     },
///     'required': ['location'],
///   },
///   handler: (args, invocation) async {
///     final location = (args as Map<String, dynamic>)['location'] as String;
///     return {'temperature': 72, 'unit': 'F', 'location': location};
///   },
/// );
/// ```
Tool defineTool(
  String name, {
  String? description,
  Map<String, dynamic>? parameters,
  required ToolHandler handler,
}) {
  return Tool(
    name: name,
    description: description,
    parameters: parameters,
    handler: handler,
  );
}
