// Copyright (c) Microsoft Corporation. All rights reserved.

/// Copilot CLI SDK Client - Main entry point for the Copilot Dart SDK.
///
/// This module provides the [CopilotClient] class, which manages the connection
/// to the Copilot CLI server and provides session management capabilities.

import 'dart:async';
import 'dart:io';

import 'json_rpc_client.dart';
import 'sdk_protocol_version.dart';
import 'session.dart';
import 'types.dart';

/// Main client for interacting with the Copilot CLI.
///
/// The [CopilotClient] manages the connection to the Copilot CLI server and
/// provides methods to create and manage conversation sessions. It can either
/// spawn a CLI server process or connect to an existing server.
///
/// ```dart
/// final client = CopilotClient(CopilotClientOptions(
///   cliPath: '/usr/local/bin/copilot',
/// ));
///
/// await client.start();
/// final session = await client.createSession();
///
/// session.events.listen((event) {
///   if (event['type'] == 'assistant.message') {
///     print(event['data']['content']);
///   }
/// });
///
/// await session.sendAndWait(MessageOptions(prompt: 'Hello!'));
/// await session.destroy();
/// await client.stop();
/// ```
class CopilotClient {
  Process? _cliProcess;
  JsonRpcClient? _connection;
  Socket? _socket;
  int? _actualPort;
  String _actualHost = 'localhost';
  ConnectionState _state = ConnectionState.disconnected;
  final Map<String, CopilotSession> _sessions = {};
  bool _isExternalServer = false;
  bool _forceStopping = false;
  List<ModelInfo>? _modelsCache;
  Completer<void>? _modelsCacheLock;
  StreamSubscription<Map<String, dynamic>>? _notificationSub;

  // Lifecycle event handling
  final Set<SessionLifecycleHandler> _lifecycleHandlers = {};
  final Map<String, Set<SessionLifecycleHandler>> _typedLifecycleHandlers = {};

  // Resolved options (with defaults applied).
  late final String _cliPath;
  late final List<String> _cliArgs;
  late final String _cwd;
  late final int _port;
  late final bool _useStdio;
  late final String? _cliUrl;
  late final String _logLevel;
  late final bool _autoStart;
  late final bool _autoRestart;
  late final Map<String, String>? _env;
  late final String? _githubToken;
  late final bool _useLoggedInUser;

  /// Creates a new [CopilotClient].
  ///
  /// [options] configures the CLI path, transport mode, authentication, etc.
  /// Throws if mutually exclusive options are provided.
  CopilotClient([CopilotClientOptions? options]) {
    options ??= const CopilotClientOptions();

    // Validate mutually exclusive options.
    if (options.cliUrl != null &&
        (options.useStdio == true || options.cliPath != null)) {
      throw ArgumentError(
          'cliUrl is mutually exclusive with useStdio and cliPath');
    }
    if (options.cliUrl != null &&
        (options.githubToken != null || options.useLoggedInUser != null)) {
      throw ArgumentError(
        'githubToken and useLoggedInUser cannot be used with cliUrl '
        '(external server manages its own auth)',
      );
    }

    // Parse cliUrl if provided.
    if (options.cliUrl != null) {
      final parsed = _parseCliUrl(options.cliUrl!);
      _actualHost = parsed.$1;
      _actualPort = parsed.$2;
      _isExternalServer = true;
    }

    _cliPath = options.cliPath ?? 'copilot';
    _cliArgs = options.cliArgs ?? const [];
    _cwd = options.cwd ?? Directory.current.path;
    _port = options.port ?? 0;
    _useStdio = options.cliUrl != null ? false : (options.useStdio ?? true);
    _cliUrl = options.cliUrl;
    _logLevel = options.logLevel ?? 'info';
    _autoStart = options.autoStart ?? true;
    _autoRestart = options.autoRestart ?? true;
    _env = options.env;
    _githubToken = options.githubToken;
    _useLoggedInUser =
        options.useLoggedInUser ?? (options.githubToken != null ? false : true);
  }

  /// Current connection state.
  ConnectionState get state => _state;

  /// Starts the CLI server and establishes a connection.
  ///
  /// If connecting to an external server (via cliUrl), only establishes the
  /// connection. Otherwise, spawns the CLI process and connects.
  ///
  /// Called automatically on first use when [CopilotClientOptions.autoStart]
  /// is true (the default).
  Future<void> start() async {
    if (_state == ConnectionState.connected) return;

    _state = ConnectionState.connecting;

    try {
      if (!_isExternalServer) {
        await _startCliServer();
      }
      await _connectToServer();
      await _verifyProtocolVersion();
      _state = ConnectionState.connected;
    } catch (e) {
      _state = ConnectionState.error;
      rethrow;
    }
  }

  /// Stops the CLI server and closes all active sessions.
  ///
  /// Returns a list of errors encountered during cleanup.
  /// An empty list indicates all cleanup succeeded.
  Future<List<Exception>> stop() async {
    final errors = <Exception>[];

    // Destroy all active sessions with retry logic.
    for (final session in _sessions.values) {
      final sessionId = session.sessionId;
      Exception? lastError;

      for (var attempt = 1; attempt <= 3; attempt++) {
        try {
          await session.destroy();
          lastError = null;
          break;
        } catch (e) {
          lastError = e is Exception ? e : Exception(e.toString());
          if (attempt < 3) {
            await Future<void>.delayed(
                Duration(milliseconds: 100 * (1 << (attempt - 1))));
          }
        }
      }

      if (lastError != null) {
        errors.add(Exception(
          'Failed to destroy session $sessionId after 3 attempts: $lastError',
        ));
      }
    }
    _sessions.clear();

    // Close notification subscription.
    await _notificationSub?.cancel();
    _notificationSub = null;

    // Dispose connection.
    if (_connection != null) {
      try {
        _connection!.dispose();
      } catch (e) {
        errors.add(Exception('Failed to dispose connection: $e'));
      }
      _connection = null;
    }

    _modelsCache = null;

    // Close TCP socket.
    if (_socket != null) {
      try {
        _socket!.destroy();
      } catch (e) {
        errors.add(Exception('Failed to close socket: $e'));
      }
      _socket = null;
    }

    // Kill CLI process (only if we spawned it).
    if (_cliProcess != null && !_isExternalServer) {
      try {
        _cliProcess!.kill();
      } catch (e) {
        errors.add(Exception('Failed to kill CLI process: $e'));
      }
      _cliProcess = null;
    }

    _state = ConnectionState.disconnected;
    _actualPort = null;

    return errors;
  }

  /// Forcefully stops without graceful cleanup.
  ///
  /// Use when [stop] fails or takes too long.
  Future<void> forceStop() async {
    _forceStopping = true;
    _sessions.clear();

    await _notificationSub?.cancel();
    _notificationSub = null;

    if (_connection != null) {
      try {
        _connection!.dispose();
      } catch (_) {}
      _connection = null;
    }

    _modelsCache = null;

    if (_socket != null) {
      try {
        _socket!.destroy();
      } catch (_) {}
      _socket = null;
    }

    if (_cliProcess != null && !_isExternalServer) {
      try {
        _cliProcess!.kill(ProcessSignal.sigkill);
      } catch (_) {}
      _cliProcess = null;
    }

    _state = ConnectionState.disconnected;
    _actualPort = null;
  }

  // -------------------------------------------------------------------------
  // Session Management
  // -------------------------------------------------------------------------

  /// Creates a new conversation session.
  ///
  /// If the client is not connected and [autoStart] is enabled, this will
  /// automatically start the connection.
  Future<CopilotSession> createSession([SessionConfig? config]) async {
    config ??= const SessionConfig();
    await _ensureConnected();

    final params = <String, dynamic>{
      if (config.model != null) 'model': config.model,
      if (config.sessionId != null) 'sessionId': config.sessionId,
      if (config.reasoningEffort != null)
        'reasoningEffort': config.reasoningEffort,
      if (config.tools != null)
        'tools': config.tools!.map((t) => t.toJson()).toList(),
      if (config.systemMessage != null)
        'systemMessage': (config.systemMessage as dynamic).toJson(),
      if (config.availableTools != null) 'availableTools': config.availableTools,
      if (config.excludedTools != null) 'excludedTools': config.excludedTools,
      if (config.provider != null) 'provider': config.provider!.toJson(),
      'requestPermission': config.onPermissionRequest != null,
      'requestUserInput': config.onUserInputRequest != null,
      'hooks': config.hooks?.hasAny ?? false,
      if (config.workingDirectory != null)
        'workingDirectory': config.workingDirectory,
      if (config.streaming != null) 'streaming': config.streaming,
      if (config.mcpServers != null)
        'mcpServers':
            config.mcpServers!.map((k, v) => MapEntry(k, v.toJson())),
      if (config.customAgents != null)
        'customAgents': config.customAgents!.map((a) => a.toJson()).toList(),
      if (config.configDir != null) 'configDir': config.configDir,
      if (config.skillDirectories != null)
        'skillDirectories': config.skillDirectories,
      if (config.disabledSkills != null) 'disabledSkills': config.disabledSkills,
      if (config.infiniteSessions != null)
        'infiniteSessions': config.infiniteSessions!.toJson(),
    };

    final response =
        await _connection!.sendRequest('session.create', params) as Map<String, dynamic>;
    final sessionId = response['sessionId'] as String;
    final workspacePath = response['workspacePath'] as String?;

    final session = CopilotSession(
      sessionId: sessionId,
      connection: _connection!,
      workspacePath: workspacePath,
    );

    session.registerTools(config.tools);
    if (config.onPermissionRequest != null) {
      session.registerPermissionHandler(config.onPermissionRequest);
    }
    if (config.onUserInputRequest != null) {
      session.registerUserInputHandler(config.onUserInputRequest);
    }
    if (config.hooks != null) {
      session.registerHooks(config.hooks);
    }

    _sessions[sessionId] = session;
    return session;
  }

  /// Resumes an existing session by its ID.
  Future<CopilotSession> resumeSession(
    String sessionId, [
    ResumeSessionConfig? config,
  ]) async {
    config ??= const ResumeSessionConfig();
    await _ensureConnected();

    final params = <String, dynamic>{
      'sessionId': sessionId,
      if (config.model != null) 'model': config.model,
      if (config.reasoningEffort != null)
        'reasoningEffort': config.reasoningEffort,
      if (config.tools != null)
        'tools': config.tools!.map((t) => t.toJson()).toList(),
      if (config.systemMessage != null)
        'systemMessage': (config.systemMessage as dynamic).toJson(),
      if (config.availableTools != null) 'availableTools': config.availableTools,
      if (config.excludedTools != null) 'excludedTools': config.excludedTools,
      if (config.provider != null) 'provider': config.provider!.toJson(),
      'requestPermission': config.onPermissionRequest != null,
      'requestUserInput': config.onUserInputRequest != null,
      'hooks': config.hooks?.hasAny ?? false,
      if (config.workingDirectory != null)
        'workingDirectory': config.workingDirectory,
      if (config.configDir != null) 'configDir': config.configDir,
      if (config.streaming != null) 'streaming': config.streaming,
      if (config.mcpServers != null)
        'mcpServers':
            config.mcpServers!.map((k, v) => MapEntry(k, v.toJson())),
      if (config.customAgents != null)
        'customAgents': config.customAgents!.map((a) => a.toJson()).toList(),
      if (config.skillDirectories != null)
        'skillDirectories': config.skillDirectories,
      if (config.disabledSkills != null) 'disabledSkills': config.disabledSkills,
      if (config.infiniteSessions != null)
        'infiniteSessions': config.infiniteSessions!.toJson(),
      if (config.disableResume != null) 'disableResume': config.disableResume,
    };

    final response =
        await _connection!.sendRequest('session.resume', params) as Map<String, dynamic>;
    final resumedId = response['sessionId'] as String;
    final workspacePath = response['workspacePath'] as String?;

    final session = CopilotSession(
      sessionId: resumedId,
      connection: _connection!,
      workspacePath: workspacePath,
    );

    session.registerTools(config.tools);
    if (config.onPermissionRequest != null) {
      session.registerPermissionHandler(config.onPermissionRequest);
    }
    if (config.onUserInputRequest != null) {
      session.registerUserInputHandler(config.onUserInputRequest);
    }
    if (config.hooks != null) {
      session.registerHooks(config.hooks);
    }

    _sessions[resumedId] = session;
    return session;
  }

  /// Deletes a session and its data from disk.
  Future<void> deleteSession(String sessionId) async {
    await _ensureConnected();

    final response = await _connection!
            .sendRequest('session.delete', {'sessionId': sessionId})
        as Map<String, dynamic>;

    final success = response['success'] as bool? ?? false;
    if (!success) {
      final error = response['error'] as String? ?? 'Unknown error';
      throw Exception('Failed to delete session $sessionId: $error');
    }

    _sessions.remove(sessionId);
  }

  /// Lists all available sessions.
  Future<List<SessionMetadata>> listSessions() async {
    await _ensureConnected();

    final response = await _connection!.sendRequest('session.list', {})
        as Map<String, dynamic>;
    final sessions = response['sessions'] as List<dynamic>;
    return sessions
        .map((s) => SessionMetadata.fromJson(s as Map<String, dynamic>))
        .toList();
  }

  /// Gets the ID of the most recently updated session.
  Future<String?> getLastSessionId() async {
    await _ensureConnected();
    final response = await _connection!.sendRequest('session.getLastId', {})
        as Map<String, dynamic>;
    return response['sessionId'] as String?;
  }

  // -------------------------------------------------------------------------
  // Ping / Status / Models
  // -------------------------------------------------------------------------

  /// Sends a ping to verify connectivity.
  Future<PingResponse> ping([String? message]) async {
    if (_connection == null) {
      throw StateError('Client not connected');
    }

    final result = await _connection!
        .sendRequest('ping', {'message': message ?? ''}) as Map<String, dynamic>;
    return PingResponse.fromJson(result);
  }

  /// Gets CLI status including version and protocol information.
  Future<GetStatusResponse> getStatus() async {
    await _ensureConnected();
    final result =
        await _connection!.sendRequest('status.get', {}) as Map<String, dynamic>;
    return GetStatusResponse.fromJson(result);
  }

  /// Gets current authentication status.
  Future<GetAuthStatusResponse> getAuthStatus() async {
    await _ensureConnected();
    final result = await _connection!.sendRequest('auth.getStatus', {})
        as Map<String, dynamic>;
    return GetAuthStatusResponse.fromJson(result);
  }

  /// Lists available models with their metadata.
  ///
  /// Results are cached after the first successful call.
  /// Cache is cleared when the client disconnects.
  Future<List<ModelInfo>> listModels() async {
    await _ensureConnected();

    // Simple lock to avoid concurrent fetches.
    if (_modelsCacheLock != null) {
      await _modelsCacheLock!.future;
    }

    if (_modelsCache != null) {
      return List.unmodifiable(_modelsCache!);
    }

    _modelsCacheLock = Completer<void>();

    try {
      final result = await _connection!.sendRequest('models.list', {})
          as Map<String, dynamic>;
      final models = (result['models'] as List<dynamic>)
          .map((m) => ModelInfo.fromJson(m as Map<String, dynamic>))
          .toList();

      _modelsCache = models;
      return List.unmodifiable(models);
    } finally {
      _modelsCacheLock!.complete();
      _modelsCacheLock = null;
    }
  }

  // -------------------------------------------------------------------------
  // TUI + Server Mode
  // -------------------------------------------------------------------------

  /// Gets the foreground session ID in TUI+server mode.
  Future<String?> getForegroundSessionId() async {
    await _ensureConnected();
    final response = await _connection!.sendRequest('session.getForeground', {})
        as Map<String, dynamic>;
    return response['sessionId'] as String?;
  }

  /// Sets the foreground session in TUI+server mode.
  Future<void> setForegroundSessionId(String sessionId) async {
    await _ensureConnected();
    final response = await _connection!
            .sendRequest('session.setForeground', {'sessionId': sessionId})
        as Map<String, dynamic>;
    final success = response['success'] as bool? ?? false;
    if (!success) {
      throw Exception(
          response['error'] as String? ?? 'Failed to set foreground session');
    }
  }

  // -------------------------------------------------------------------------
  // Lifecycle Event Subscriptions
  // -------------------------------------------------------------------------

  /// Subscribes to all session lifecycle events.
  ///
  /// Returns a function that unsubscribes the handler when called.
  void Function() onLifecycle(SessionLifecycleHandler handler) {
    _lifecycleHandlers.add(handler);
    return () => _lifecycleHandlers.remove(handler);
  }

  /// Subscribes to a specific session lifecycle event type.
  ///
  /// [eventType] is one of: session.created, session.deleted,
  /// session.updated, session.foreground, session.background.
  ///
  /// Returns a function that unsubscribes the handler when called.
  void Function() onLifecycleType(
      String eventType, SessionLifecycleHandler handler) {
    _typedLifecycleHandlers.putIfAbsent(eventType, () => {});
    _typedLifecycleHandlers[eventType]!.add(handler);
    return () {
      _typedLifecycleHandlers[eventType]?.remove(handler);
    };
  }

  // -------------------------------------------------------------------------
  // Private Implementation
  // -------------------------------------------------------------------------

  Future<void> _ensureConnected() async {
    if (_connection == null) {
      if (_autoStart) {
        await start();
      } else {
        throw StateError('Client not connected. Call start() first.');
      }
    }
  }

  Future<void> _startCliServer() async {
    final args = <String>[
      ..._cliArgs,
      '--headless',
      '--no-auto-update',
      '--log-level',
      _logLevel,
    ];

    if (_useStdio) {
      args.add('--stdio');
    } else if (_port > 0) {
      args.addAll(['--port', _port.toString()]);
    }

    if (_githubToken != null) {
      args.addAll(['--auth-token-env', 'COPILOT_SDK_AUTH_TOKEN']);
    }
    if (!_useLoggedInUser) {
      args.add('--no-auto-login');
    }

    // Build environment.
    final env = Map<String, String>.from(_env ?? Platform.environment);
    env.remove('NODE_DEBUG');
    if (_githubToken != null) {
      env['COPILOT_SDK_AUTH_TOKEN'] = _githubToken!;
    }

    // Verify CLI exists.
    if (!File(_cliPath).existsSync() && !_cliPath.contains('/')) {
      // Allow bare command names (resolved via PATH).
    } else if (!File(_cliPath).existsSync()) {
      throw Exception(
        'Copilot CLI not found at $_cliPath. '
        'Ensure the CLI is installed and the path is correct.',
      );
    }

    _cliProcess = await Process.start(
      _cliPath,
      args,
      workingDirectory: _cwd,
      environment: env,
    );

    // Forward stderr to host stderr.
    _cliProcess!.stderr.transform(const SystemEncoding().decoder).listen(
      (data) {
        for (final line in data.split('\n')) {
          if (line.trim().isNotEmpty) {
            stderr.writeln('[CLI subprocess] $line');
          }
        }
      },
    );

    if (!_useStdio) {
      // TCP mode: wait for port announcement on stdout.
      final completer = Completer<void>();
      final stdout = StringBuffer();

      late StreamSubscription<String> sub;
      sub = _cliProcess!.stdout
          .transform(const SystemEncoding().decoder)
          .listen((data) {
        stdout.write(data);
        final match =
            RegExp(r'listening on port (\d+)', caseSensitive: false)
                .firstMatch(stdout.toString());
        if (match != null && !completer.isCompleted) {
          _actualPort = int.parse(match.group(1)!);
          sub.cancel();
          completer.complete();
        }
      });

      _cliProcess!.exitCode.then((code) {
        if (!completer.isCompleted) {
          completer.completeError(
              Exception('CLI server exited with code $code'));
        }
      });

      // Timeout waiting for server.
      await completer.future.timeout(
        const Duration(seconds: 10),
        onTimeout: () {
          throw TimeoutException('Timeout waiting for CLI server to start');
        },
      );
    }

    // Handle unexpected exits.
    _cliProcess!.exitCode.then((code) {
      if (_autoRestart &&
          _state == ConnectionState.connected &&
          !_forceStopping) {
        _reconnect();
      }
    });
  }

  Future<void> _connectToServer() async {
    if (_useStdio) {
      await _connectViaStdio();
    } else {
      await _connectViaTcp();
    }
  }

  Future<void> _connectViaStdio() async {
    if (_cliProcess == null) {
      throw StateError('CLI process not started');
    }

    _connection = JsonRpcClient(
      input: _cliProcess!.stdout,
      output: _cliProcess!.stdin,
    );

    _attachConnectionHandlers();
    _connection!.listen();
  }

  Future<void> _connectViaTcp() async {
    if (_actualPort == null) {
      throw StateError('Server port not available');
    }

    _socket = await Socket.connect(_actualHost, _actualPort!);

    _connection = JsonRpcClient(
      input: _socket!,
      output: _socket!,
    );

    _attachConnectionHandlers();
    _connection!.listen();
  }

  void _attachConnectionHandlers() {
    if (_connection == null) return;

    // Listen for notifications.
    _notificationSub = _connection!.notifications.listen((notification) {
      final method = notification['method'] as String?;
      final params = notification['params'];

      switch (method) {
        case 'session.event':
          _handleSessionEventNotification(params);
          break;
        case 'session.lifecycle':
          _handleSessionLifecycleNotification(params);
          break;
      }
    });

    // Register server-to-client request handlers.
    _connection!.onRequest('tool.call', (params) async {
      return await _handleToolCallRequest(params as Map<String, dynamic>);
    });

    _connection!.onRequest('permission.request', (params) async {
      return await _handlePermissionRequest(params as Map<String, dynamic>);
    });

    _connection!.onRequest('userInput.request', (params) async {
      return await _handleUserInputRequest(params as Map<String, dynamic>);
    });

    _connection!.onRequest('hooks.invoke', (params) async {
      return await _handleHooksInvoke(params as Map<String, dynamic>);
    });
  }

  // -------------------------------------------------------------------------
  // Notification Dispatch
  // -------------------------------------------------------------------------

  void _handleSessionEventNotification(dynamic params) {
    if (params is! Map<String, dynamic>) return;
    final sessionId = params['sessionId'] as String?;
    final event = params['event'] as Map<String, dynamic>?;
    if (sessionId == null || event == null) return;

    final session = _sessions[sessionId];
    session?.dispatchEvent(event);
  }

  void _handleSessionLifecycleNotification(dynamic params) {
    if (params is! Map<String, dynamic>) return;
    final type = params['type'] as String?;
    final sessionId = params['sessionId'] as String?;
    if (type == null || sessionId == null) return;

    final event = SessionLifecycleEvent.fromJson(params);

    // Typed handlers.
    final typedHandlers = _typedLifecycleHandlers[type];
    if (typedHandlers != null) {
      for (final handler in typedHandlers) {
        try {
          handler(event);
        } catch (_) {}
      }
    }

    // Wildcard handlers.
    for (final handler in _lifecycleHandlers) {
      try {
        handler(event);
      } catch (_) {}
    }
  }

  // -------------------------------------------------------------------------
  // Server-to-Client Request Handlers
  // -------------------------------------------------------------------------

  Future<Map<String, dynamic>> _handleToolCallRequest(
      Map<String, dynamic> params) async {
    final sessionId = params['sessionId'] as String?;
    final toolCallId = params['toolCallId'] as String?;
    final toolName = params['toolName'] as String?;
    final arguments = params['arguments'];

    if (sessionId == null || toolCallId == null || toolName == null) {
      throw Exception('Invalid tool call payload');
    }

    final session = _sessions[sessionId];
    if (session == null) {
      throw Exception('Unknown session $sessionId');
    }

    final handler = session.getToolHandler(toolName);
    if (handler == null) {
      return {
        'result': {
          'textResultForLlm':
              "Tool '$toolName' is not supported by this client instance.",
          'resultType': 'failure',
          'error': "tool '$toolName' not supported",
          'toolTelemetry': <String, dynamic>{},
        },
      };
    }

    try {
      final invocation = ToolInvocation(
        sessionId: sessionId,
        toolCallId: toolCallId,
        toolName: toolName,
        arguments: arguments,
      );
      final result = await handler(arguments, invocation);
      return {'result': _normalizeToolResult(result)};
    } catch (e) {
      return {
        'result': {
          'textResultForLlm':
              'Invoking this tool produced an error. Detailed information is not available.',
          'resultType': 'failure',
          'error': e.toString(),
          'toolTelemetry': <String, dynamic>{},
        },
      };
    }
  }

  Future<Map<String, dynamic>> _handlePermissionRequest(
      Map<String, dynamic> params) async {
    final sessionId = params['sessionId'] as String?;
    final permissionRequest = params['permissionRequest'];

    if (sessionId == null || permissionRequest == null) {
      throw Exception('Invalid permission request payload');
    }

    final session = _sessions[sessionId];
    if (session == null) {
      throw Exception('Session not found: $sessionId');
    }

    try {
      final result =
          await session.handlePermissionRequest(permissionRequest);
      return {'result': result.toJson()};
    } catch (_) {
      return {
        'result': {
          'kind':
              'denied-no-approval-rule-and-could-not-request-from-user',
        },
      };
    }
  }

  Future<Map<String, dynamic>> _handleUserInputRequest(
      Map<String, dynamic> params) async {
    final sessionId = params['sessionId'] as String?;
    final question = params['question'] as String?;

    if (sessionId == null || question == null) {
      throw Exception('Invalid user input request payload');
    }

    final session = _sessions[sessionId];
    if (session == null) {
      throw Exception('Session not found: $sessionId');
    }

    final result = await session.handleUserInputRequest(
      UserInputRequest(
        question: question,
        choices: (params['choices'] as List<dynamic>?)?.cast<String>(),
        allowFreeform: params['allowFreeform'] as bool? ?? true,
      ),
    );
    return result.toJson();
  }

  Future<Map<String, dynamic>> _handleHooksInvoke(
      Map<String, dynamic> params) async {
    final sessionId = params['sessionId'] as String?;
    final hookType = params['hookType'] as String?;
    final input = params['input'];

    if (sessionId == null || hookType == null) {
      throw Exception('Invalid hooks invoke payload');
    }

    final session = _sessions[sessionId];
    if (session == null) {
      throw Exception('Session not found: $sessionId');
    }

    final output = await session.handleHooksInvoke(hookType, input);
    return {'output': output};
  }

  // -------------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------------

  Map<String, dynamic> _normalizeToolResult(dynamic result) {
    if (result == null) {
      return {
        'textResultForLlm': 'Tool returned no result',
        'resultType': 'failure',
        'error': 'tool returned no result',
        'toolTelemetry': <String, dynamic>{},
      };
    }

    if (ToolResultObject.isToolResultObject(result)) {
      return result as Map<String, dynamic>;
    }

    if (result is ToolResultObject) {
      return result.toJson();
    }

    final text = result is String ? result : result.toString();
    return {
      'textResultForLlm': text,
      'resultType': 'success',
      'toolTelemetry': <String, dynamic>{},
    };
  }

  Future<void> _verifyProtocolVersion() async {
    final expected = getSdkProtocolVersion();
    final pingResult = await ping();
    final serverVersion = pingResult.protocolVersion;

    if (serverVersion == null) {
      throw Exception(
        'SDK protocol version mismatch: SDK expects version $expected, '
        'but server does not report a protocol version. '
        'Please update your server to ensure compatibility.',
      );
    }

    if (serverVersion != expected) {
      throw Exception(
        'SDK protocol version mismatch: SDK expects version $expected, '
        'but server reports version $serverVersion. '
        'Please update your SDK or server to ensure compatibility.',
      );
    }
  }

  (String, int) _parseCliUrl(String url) {
    // Remove protocol if present.
    var clean = url.replaceFirst(RegExp(r'^https?://'), '');

    // Check if it is just a port number.
    if (RegExp(r'^\d+$').hasMatch(clean)) {
      return ('localhost', int.parse(clean));
    }

    final parts = clean.split(':');
    if (parts.length != 2) {
      throw ArgumentError(
        'Invalid cliUrl format: $url. '
        'Expected "host:port", "http://host:port", or "port"',
      );
    }

    final host = parts[0].isEmpty ? 'localhost' : parts[0];
    final port = int.tryParse(parts[1]);

    if (port == null || port <= 0 || port > 65535) {
      throw ArgumentError('Invalid port in cliUrl: $url');
    }

    return (host, port);
  }

  Future<void> _reconnect() async {
    _state = ConnectionState.disconnected;
    try {
      await stop();
      await start();
    } catch (_) {
      // Reconnection failed.
    }
  }
}
