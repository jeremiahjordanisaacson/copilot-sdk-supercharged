// Copyright (c) Microsoft Corporation. All rights reserved.

/// Copilot CLI SDK Client - Main entry point for the Copilot Dart SDK.
///
/// This module provides the [CopilotClient] class, which manages the connection
/// to the Copilot CLI server and provides session management capabilities.

import 'dart:async';
import 'dart:io';
import 'dart:math';

import 'json_rpc_client.dart';
import 'sdk_protocol_version.dart';
import 'session.dart';
import 'session_params.dart';
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
  late final String _logLevel;
  late final bool _autoStart;
  late final bool _autoRestart;
  late final Map<String, String>? _env;
  late final String? _githubToken;
  late final bool _useLoggedInUser;
  late final ClientInfo? _clientInfo;
  late final String _mode;
  late final NetworkConfig? _network;

  // GitHub token providers by registration ID (gitHubToken.getToken).
  final Map<String, GitHubTokenProvider> _gitHubTokenProviders = {};

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
    _logLevel = options.logLevel ?? 'info';
    _autoStart = options.autoStart ?? true;
    _autoRestart = options.autoRestart ?? true;
    _env = options.env;
    _githubToken = options.githubToken;
    _useLoggedInUser =
        options.useLoggedInUser ?? (options.githubToken != null ? false : true);
    _clientInfo = options.clientInfo;
    _mode = _resolveMode(options.mode);
    _network = options.network;
  }

  /// Current connection state.
  ConnectionState get state => _state;

  /// Effective client mode: [ClientMode.codingAgent] (the default) or
  /// [ClientMode.empty].
  String get mode => _mode;

  /// Starts the CLI server and establishes a connection.
  ///
  /// If connecting to an external server (via cliUrl), only establishes the
  /// connection. Otherwise, spawns the CLI process and connects. Then
  /// declares [CopilotClientOptions.clientInfo] on `connect` (when set),
  /// verifies that the server speaks SDK protocol 4 (Copilot CLI 1.0.113 or
  /// newer), and registers the network callbacks (`network.setProvider`).
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
      await _sendClientInfo();
      await _verifyProtocolVersion();
      await _registerNetworkProvider();
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

    // Detach all active sessions with retry logic.
    for (final session in _sessions.values.toList()) {
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
    _gitHubTokenProviders.clear();

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
    _gitHubTokenProviders.clear();

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
  /// automatically start the connection. The flat options are grouped for
  /// SDK protocol 4 right before `session.create` is sent. A failure throws
  /// a [JsonRpcError]; [JsonRpcError.policyHelperFailure] exposes a Policy
  /// Helper failure reported by the runtime.
  Future<CopilotSession> createSession([SessionConfig? config]) async {
    config ??= const SessionConfig();
    _validateSessionOptions(
      gitHubToken: config.gitHubToken,
      gitHubTokenProvider: config.gitHubTokenProvider,
      availableTools: config.availableTools,
    );
    await _ensureConnected();

    final tokenProvider = config.gitHubTokenProvider;
    final registrationId = tokenProvider == null ? null : _newRegistrationId();

    final params = <String, dynamic>{
      if (config.model != null) 'model': config.model,
      if (config.sessionId != null) 'sessionId': config.sessionId,
      if (config.name != null) 'name': config.name,
      if (config.reasoningEffort != null)
        'reasoningEffort': config.reasoningEffort,
      if (config.tools != null)
        'tools': config.tools!.map((t) => t.toJson()).toList(),
      if (config.systemMessage != null)
        'systemMessage': _systemMessageToWire(config.systemMessage),
      if (config.availableTools != null) 'availableTools': config.availableTools,
      if (config.excludedTools != null) 'excludedTools': config.excludedTools,
      if (config.provider != null) 'provider': config.provider!.toJson(),
      'requestPermission': config.onPermissionRequest != null,
      'requestUserInput': config.onUserInputRequest != null,
      'hooks': config.hooks?.hasAny ?? false,
      if (config.onShellAuthenticationRequest != null)
        'requestShellAuthentication': true,
      if (config.onStoreMemory != null) 'hasMemoryStore': true,
      if (config.workingDirectory != null)
        'workingDirectory': config.workingDirectory,
      if (config.streaming != null) 'streaming': config.streaming,
      if (config.includeSubAgentStreamingEvents != null)
        'includeSubAgentStreamingEvents': config.includeSubAgentStreamingEvents,
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
      if (config.modelCapabilities != null)
        'modelCapabilities': config.modelCapabilities,
      if (config.enableConfigDiscovery != null)
        'enableConfigDiscovery': config.enableConfigDiscovery,
      if (config.gitHubToken != null) 'gitHubToken': config.gitHubToken,
      if (registrationId != null)
        'gitHubTokenProviderRegistrationId': registrationId,
      if (config.enableSessionTelemetry != null)
        'enableSessionTelemetry': config.enableSessionTelemetry,
      if (config.instructionDirectories != null)
        'instructionDirectories': config.instructionDirectories,
      // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
      if (config.enableCitations != null)
        'enableCitations': config.enableCitations,
      if (config.excludedBuiltinAgents != null)
        'excludedBuiltinAgents': config.excludedBuiltinAgents,
      if (config.sessionLimits != null)
        'sessionLimits': config.sessionLimits!.toJson(),
      if (config.memory != null)
        'memory': config.memory!.toJson()
      else if (config.onStoreMemory != null)
        // An app-side memory store needs memory enabled.
        'memory': <String, dynamic>{'enabled': true},
      if (config.otlpProtocol != null) 'otlpProtocol': config.otlpProtocol,
      if (config.expAssignments != null)
        'expAssignments': config.expAssignments,
      // --- 2026-08 upstream-sync session options (parity with @github/copilot-sdk) ---
      if (config.rewindEnabled != null) 'rewindEnabled': config.rewindEnabled,
      if (config.additionalDirectories != null)
        'additionalDirectories': config.additionalDirectories,
      if (config.disabledMcpServers != null)
        'disabledMcpServers': config.disabledMcpServers,
      if (config.githubMcpToolConfig != null)
        'githubMcpToolConfig': config.githubMcpToolConfig,
      if (config.canvasProvider != null) 'canvasProvider': config.canvasProvider,
      if (config.customAgentsLocalOnly != null)
        'customAgentsLocalOnly': config.customAgentsLocalOnly,
      if (config.toolSearch != null) 'toolSearch': config.toolSearch,
      if (config.experimentalMode != null)
        'enableExperimentalMode': config.experimentalMode,
      if (config.contentExclusion != null)
        'contentExclusion': config.contentExclusion,
      // --- v2.6.0 upstream-sync session options ---
      if (config.askUserVariant != null) 'askUserVariant': config.askUserVariant,
      if (config.sandboxConfig != null)
        'sandboxConfig': config.sandboxConfig!.toJson(),
      if (config.featureFlags != null) 'featureFlags': config.featureFlags,
      if (config.allowedModels != null) 'allowedModels': config.allowedModels,
      if (config.authClientIdMetadataUrl != null)
        'authClientIdMetadataUrl': config.authClientIdMetadataUrl,
      if (config.agentDirectories != null)
        'agentDirectories': config.agentDirectories,
      if (config.hookDirectories != null)
        'hookDirectories': config.hookDirectories,
      if (config.trustWorkingDirectory != null)
        'trustWorkingDirectory': config.trustWorkingDirectory,
      if (config.imageGeneration != null)
        'imageGeneration': config.imageGeneration!.toJson(),
      if (config.autopilot != null) 'autopilot': config.autopilot!.toJson(),
      // --- v3.0.0 (SDK protocol 4) session options ---
      if (config.providers != null)
        'providers': config.providers!.map((p) => p.toJson()).toList(),
      if (config.models != null)
        'models': config.models!.map((m) => m.toJson()).toList(),
      if (config.defaultProvider != null)
        'defaultProvider': config.defaultProvider,
    };
    final capi = _capiToWire(config.capi, config.enableWebSocketResponses);
    if (capi != null) params['capi'] = capi;

    // BYOK: resolve a per-session bearer token for the provider, if requested.
    if (config.bearerTokenProvider != null && params['provider'] is Map) {
      final token = await config.bearerTokenProvider!(
        ProviderTokenArgs(sessionId: config.sessionId),
      );
      if (token != null) {
        (params['provider'] as Map<String, dynamic>)['bearerToken'] = token;
      }
    }

    final payload = _groupForMode(params, isResume: false);
    if (registrationId != null) {
      _gitHubTokenProviders[registrationId] = tokenProvider!;
    }

    final Map<String, dynamic> response;
    try {
      response = await _connection!.sendRequest('session.create', payload)
          as Map<String, dynamic>;
    } catch (_) {
      if (registrationId != null) _gitHubTokenProviders.remove(registrationId);
      rethrow;
    }
    final sessionId = response['sessionId'] as String;
    final workspacePath = response['workspacePath'] as String?;

    final session = _newSession(sessionId, workspacePath, registrationId);

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
    session.registerShellAuthenticationHandler(
        config.onShellAuthenticationRequest);
    session.registerMemoryStore(config.onStoreMemory);

    _sessions[sessionId] = session;
    return session;
  }

  /// Resumes an existing session by its ID.
  ///
  /// The flat options are grouped for SDK protocol 4 right before
  /// `session.resume` is sent. A failure throws a [JsonRpcError];
  /// [JsonRpcError.policyHelperFailure] exposes a Policy Helper failure.
  Future<CopilotSession> resumeSession(
    String sessionId, [
    ResumeSessionConfig? config,
  ]) async {
    config ??= const ResumeSessionConfig();
    _validateSessionOptions(
      gitHubToken: config.gitHubToken,
      gitHubTokenProvider: config.gitHubTokenProvider,
      availableTools: config.availableTools,
    );
    await _ensureConnected();

    final tokenProvider = config.gitHubTokenProvider;
    final registrationId = tokenProvider == null ? null : _newRegistrationId();
    final suppressResumeEvent =
        config.suppressResumeEvent ?? config.disableResume;

    final params = <String, dynamic>{
      'sessionId': sessionId,
      if (config.model != null) 'model': config.model,
      if (config.reasoningEffort != null)
        'reasoningEffort': config.reasoningEffort,
      if (config.tools != null)
        'tools': config.tools!.map((t) => t.toJson()).toList(),
      if (config.systemMessage != null)
        'systemMessage': _systemMessageToWire(config.systemMessage),
      if (config.availableTools != null) 'availableTools': config.availableTools,
      if (config.excludedTools != null) 'excludedTools': config.excludedTools,
      if (config.provider != null) 'provider': config.provider!.toJson(),
      'requestPermission': config.onPermissionRequest != null,
      'requestUserInput': config.onUserInputRequest != null,
      'hooks': config.hooks?.hasAny ?? false,
      if (config.onShellAuthenticationRequest != null)
        'requestShellAuthentication': true,
      if (config.onStoreMemory != null) 'hasMemoryStore': true,
      if (config.onStoreMemory != null)
        // An app-side memory store needs memory enabled.
        'memory': <String, dynamic>{'enabled': true},
      if (config.workingDirectory != null)
        'workingDirectory': config.workingDirectory,
      if (config.configDir != null) 'configDir': config.configDir,
      if (config.streaming != null) 'streaming': config.streaming,
      if (config.includeSubAgentStreamingEvents != null)
        'includeSubAgentStreamingEvents': config.includeSubAgentStreamingEvents,
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
      if (config.modelCapabilities != null)
        'modelCapabilities': config.modelCapabilities,
      if (config.enableConfigDiscovery != null)
        'enableConfigDiscovery': config.enableConfigDiscovery,
      if (config.gitHubToken != null) 'gitHubToken': config.gitHubToken,
      if (registrationId != null)
        'gitHubTokenProviderRegistrationId': registrationId,
      if (config.enableSessionTelemetry != null)
        'enableSessionTelemetry': config.enableSessionTelemetry,
      if (config.instructionDirectories != null)
        'instructionDirectories': config.instructionDirectories,
      if (suppressResumeEvent != null)
        'suppressResumeEvent': suppressResumeEvent,
      // --- v2.6.0 upstream-sync resume options ---
      if (config.askUserVariant != null) 'askUserVariant': config.askUserVariant,
      if (config.sandboxConfig != null)
        'sandboxConfig': config.sandboxConfig!.toJson(),
      if (config.featureFlags != null) 'featureFlags': config.featureFlags,
      if (config.allowedModels != null) 'allowedModels': config.allowedModels,
      if (config.authClientIdMetadataUrl != null)
        'authClientIdMetadataUrl': config.authClientIdMetadataUrl,
      if (config.agentDirectories != null)
        'agentDirectories': config.agentDirectories,
      if (config.hookDirectories != null)
        'hookDirectories': config.hookDirectories,
      if (config.allowTranscriptRecovery != null)
        'allowTranscriptRecovery': config.allowTranscriptRecovery,
      if (config.imageGeneration != null)
        'imageGeneration': config.imageGeneration!.toJson(),
      if (config.autopilot != null) 'autopilot': config.autopilot!.toJson(),
      // --- v3.0.0 (SDK protocol 4) resume options ---
      if (config.providers != null)
        'providers': config.providers!.map((p) => p.toJson()).toList(),
      if (config.models != null)
        'models': config.models!.map((m) => m.toJson()).toList(),
      if (config.defaultProvider != null)
        'defaultProvider': config.defaultProvider
      else if (config.clearDefaultProvider == true)
        'defaultProvider': null,
    };
    final capi = _capiToWire(config.capi, null);
    if (capi != null) params['capi'] = capi;

    final payload = _groupForMode(params, isResume: true);
    if (registrationId != null) {
      _gitHubTokenProviders[registrationId] = tokenProvider!;
    }

    final Map<String, dynamic> response;
    try {
      response = await _connection!.sendRequest('session.resume', payload)
          as Map<String, dynamic>;
    } catch (_) {
      if (registrationId != null) _gitHubTokenProviders.remove(registrationId);
      rethrow;
    }
    final resumedId = response['sessionId'] as String? ?? sessionId;
    final workspacePath = response['workspacePath'] as String?;

    final session = _newSession(resumedId, workspacePath, registrationId);

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
    session.registerShellAuthenticationHandler(
        config.onShellAuthenticationRequest);
    session.registerMemoryStore(config.onStoreMemory);

    _sessions[resumedId] = session;
    return session;
  }

  /// Validates options shared by [createSession] and [resumeSession].
  void _validateSessionOptions({
    String? gitHubToken,
    GitHubTokenProvider? gitHubTokenProvider,
    List<String>? availableTools,
  }) {
    if (gitHubToken != null && gitHubTokenProvider != null) {
      throw ArgumentError(
          'gitHubToken and gitHubTokenProvider are mutually exclusive');
    }
    if (_mode == ClientMode.empty && availableTools == null) {
      throw ArgumentError(
        "CopilotClient is in mode 'empty' but the session config did not "
        "specify 'availableTools'. Empty mode requires every session to opt "
        'into the tools it wants.',
      );
    }
  }

  /// Groups the flat payload for SDK protocol 4 and applies the empty-mode
  /// defaults under the caller's values.
  Map<String, dynamic> _groupForMode(
    Map<String, dynamic> params, {
    required bool isResume,
  }) {
    final payload = groupSessionParams(params, isResume: isResume);
    return _mode == ClientMode.empty
        ? applyEmptyModeSessionDefaults(payload)
        : payload;
  }

  /// Creates a session object that unregisters itself (and its GitHub token
  /// provider) when it is detached.
  CopilotSession _newSession(
    String sessionId,
    String? workspacePath,
    String? registrationId,
  ) {
    late final CopilotSession session;
    session = CopilotSession(
      sessionId: sessionId,
      connection: _connection!,
      workspacePath: workspacePath,
      onClose: () {
        if (identical(_sessions[sessionId], session)) {
          _sessions.remove(sessionId);
        }
        if (registrationId != null) {
          _gitHubTokenProviders.remove(registrationId);
        }
      },
    );
    return session;
  }

  /// Wire form of the Copilot API options; `enableWebSocketResponses` (a
  /// legacy flat option) moves under `capi`.
  static Map<String, dynamic>? _capiToWire(
    CapiSessionOptions? capi,
    bool? enableWebSocketResponses,
  ) {
    final wire = <String, dynamic>{
      ...?capi?.toJson(),
      if (enableWebSocketResponses != null)
        'enableWebSocketResponses': enableWebSocketResponses,
    };
    return wire.isEmpty ? null : wire;
  }

  /// Wire form of a system message: a plain string is appended to the
  /// default system message.
  static dynamic _systemMessageToWire(dynamic systemMessage) {
    if (systemMessage is String) {
      return <String, dynamic>{'mode': 'append', 'content': systemMessage};
    }
    if (systemMessage is Map) return systemMessage;
    return systemMessage.toJson();
  }

  /// A random (version 4) UUID for a callback registration.
  static String _newRegistrationId() {
    final random = Random.secure();
    final bytes = List<int>.generate(16, (_) => random.nextInt(256));
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;
    final hex = bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
    return '${hex.substring(0, 8)}-${hex.substring(8, 12)}-'
        '${hex.substring(12, 16)}-${hex.substring(16, 20)}-'
        '${hex.substring(20)}';
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

  /// Gets metadata for a session by ID.
  Future<Map<String, dynamic>?> getSessionMetadata(String sessionId) async {
    await _ensureConnected();
    try {
      final response = await _connection!
              .sendRequest('session.getMetadata', {'sessionId': sessionId})
          as Map<String, dynamic>;
      return response;
    } catch (_) {
      return null;
    }
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
  // Managed Settings
  // -------------------------------------------------------------------------

  /// Force-refreshes enterprise managed settings for every account
  /// (`managedSettings.clearCache`). The next session resolution re-fetches
  /// the account's policy instead of serving a cached response.
  Future<void> clearManagedSettingsCache() async {
    await _ensureConnected();
    await _connection!.sendRequest('managedSettings.clearCache', {});
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
  // Session Filesystem Provider
  // -------------------------------------------------------------------------

  /// Sets the session filesystem provider configuration.
  ///
  /// [initialCwd] is the optional initial working directory.
  /// [sessionStatePath] is the optional path for session state persistence.
  /// [conventions] is an optional list of convention strings.
  Future<void> setSessionFsProvider({
    String? initialCwd,
    String? sessionStatePath,
    List<String>? conventions,
  }) async {
    await _ensureConnected();

    final params = <String, dynamic>{
      if (initialCwd != null) 'initialCwd': initialCwd,
      if (sessionStatePath != null) 'sessionStatePath': sessionStatePath,
      if (conventions != null) 'conventions': conventions,
    };

    await _connection!.sendRequest('sessionFs.setProvider', params);
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

    // If cliPath is a .js file, run it with node.
    final executable = _cliPath.endsWith('.js') ? 'node' : _cliPath;
    final execArgs =
        _cliPath.endsWith('.js') ? <String>[_cliPath, ...args] : args;

    _cliProcess = await Process.start(
      executable,
      execArgs,
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

    _connection!.onRequest('gitHubToken.getToken', (params) async {
      return await _handleGitHubTokenRequest(params);
    });

    _connection!.onRequest('shellAuthentication.request', (params) async {
      return await _handleShellAuthenticationRequest(params);
    });

    _connection!.onRequest('shellAuthentication.cancel', (params) async {
      if (params is Map) {
        final sessionId = params['sessionId'];
        final requestId = params['requestId'];
        if (sessionId is String && requestId is String) {
          _sessions[sessionId]?.cancelShellAuthenticationRequest(requestId);
        }
      }
      return <String, dynamic>{};
    });

    _connection!.onRequest('memoryStore.store', (params) async {
      final json = params is Map<String, dynamic> ? params : <String, dynamic>{};
      final session = _sessions[json['sessionId']];
      if (session == null) {
        throw StateError('Memory store owner is unavailable');
      }
      await session.handleMemoryStore(StoredMemory.fromJson(json));
      return null;
    });

    _registerNetworkHandlers();
  }

  /// Registers the `network.*` request handlers for the configured callbacks.
  /// Tokens and passwords are never logged.
  void _registerNetworkHandlers() {
    final network = _network;
    if (network == null) return;

    final resolveProxy = network.resolveProxy;
    if (resolveProxy != null) {
      _connection!.onRequest('network.resolveProxy', (params) async {
        final url = params is Map ? params['url'] as String? ?? '' : '';
        final proxy = await resolveProxy(url);
        return <String, dynamic>{
          if (proxy != null && proxy.isNotEmpty) 'proxy': proxy,
        };
      });
    }

    final authenticateNegotiate = network.authenticateNegotiate;
    if (authenticateNegotiate != null) {
      _connection!.onRequest('network.authenticateNegotiate', (params) async {
        final spn = params is Map ? params['spn'] as String? ?? '' : '';
        final token = await authenticateNegotiate(spn);
        return <String, dynamic>{
          if (token != null && token.isNotEmpty) 'token': token,
        };
      });
    }

    final authenticateBasic = network.authenticateBasic;
    if (authenticateBasic != null) {
      _connection!.onRequest('network.authenticateBasic', (params) async {
        final json = params is Map ? params : const <String, dynamic>{};
        final credentials = await authenticateBasic(
          NetworkAuthenticateBasicRequest(
            proxy: json['proxy'] as String? ?? '',
            realm: json['realm'] as String?,
            previousRejected: json['previousRejected'] as bool? ?? false,
          ),
        );
        if (credentials == null) return <String, dynamic>{};
        final username = credentials.username;
        final password = credentials.password;
        return <String, dynamic>{
          if (username != null) 'username': username,
          if (password != null) 'password': password,
        };
      });
    }
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

  /// Answers `gitHubToken.getToken` with the registered provider's
  /// `{kind: "token", accessToken, tokenType?, expiresIn}` or
  /// `{kind: "cancelled"}`.
  Future<Map<String, dynamic>> _handleGitHubTokenRequest(dynamic params) async {
    final json = params is Map ? params : const <String, dynamic>{};
    final registrationId = json['registrationId'] as String?;
    final provider =
        registrationId == null ? null : _gitHubTokenProviders[registrationId];
    if (provider == null) {
      throw StateError(
          'No GitHub token provider registered for "$registrationId"');
    }
    final result = await provider(GitHubTokenRequest(
      host: json['host'] as String? ?? '',
      sessionId: json['sessionId'] as String?,
      reason: json['reason'] as String? ?? '',
    ));
    return result.toJson();
  }

  /// Answers `shellAuthentication.request` with the session's handler,
  /// or `{action: "cancel"}` when nobody can answer.
  Future<Map<String, dynamic>> _handleShellAuthenticationRequest(
      dynamic params) async {
    if (params is! Map<String, dynamic>) {
      return const ShellAuthenticationResponse.cancel().toJson();
    }
    final request = ShellAuthenticationRequest.fromJson(params);
    final session = _sessions[request.sessionId];
    if (session == null || request.requestId.isEmpty) {
      return const ShellAuthenticationResponse.cancel().toJson();
    }
    final response = await session.handleShellAuthenticationRequest(request);
    return response.toJson();
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

  /// Declares [CopilotClientOptions.clientInfo] on the `connect` handshake.
  /// Runtimes without `connect` are tolerated.
  Future<void> _sendClientInfo() async {
    final wire = _clientInfo?.toWire();
    if (wire == null) return;
    try {
      await _connection!.sendRequest('connect', {'clientInfo': wire});
    } on JsonRpcError catch (e) {
      if (!e.isMethodNotFound) rethrow;
    }
  }

  /// Calls `network.setProvider` when any network callback is configured,
  /// marking each registered callback with an empty object.
  Future<void> _registerNetworkProvider() async {
    final network = _network;
    if (network == null || !network.hasCallbacks) return;
    await _connection!.sendRequest('network.setProvider', {
      if (network.scope != null) 'scope': network.scope,
      if (network.resolveProxy != null) 'resolveProxy': <String, dynamic>{},
      if (network.authenticateNegotiate != null)
        'authenticateNegotiate': <String, dynamic>{},
      if (network.authenticateBasic != null)
        'authenticateBasic': <String, dynamic>{},
    });
  }

  /// Resolves [CopilotClientOptions.mode]; `"copilot-cli"` is an alias of
  /// `"coding-agent"`.
  static String _resolveMode(String? mode) {
    switch (mode) {
      case null:
      case ClientMode.codingAgent:
      case ClientMode.copilotCli:
        return ClientMode.codingAgent;
      case ClientMode.empty:
        return ClientMode.empty;
      default:
        throw ArgumentError(
            "Unsupported client mode '$mode'; use 'coding-agent' or 'empty'");
    }
  }

  Future<void> _verifyProtocolVersion() async {
    final expected = getSdkProtocolVersion();
    final pingResult = await ping();
    final serverVersion = pingResult.protocolVersion;

    if (serverVersion == null) {
      throw Exception(
        'SDK protocol version mismatch: SDK supports version $expected, '
        'but server does not report a protocol version. '
        'Please update your SDK or server to ensure compatibility.',
      );
    }

    if (serverVersion != expected) {
      throw Exception(
        'SDK protocol version mismatch: SDK supports version $expected, '
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
