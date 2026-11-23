// Copyright (c) Microsoft Corporation. All rights reserved.

/// Copilot Session - represents a single conversation session with the
/// Copilot CLI.

import 'dart:async';

import 'json_rpc_client.dart';
import 'types.dart';

/// Represents a single conversation session with the Copilot CLI.
///
/// A session maintains conversation state, handles events, and manages tool
/// execution. Sessions are created via [CopilotClient.createSession] or
/// resumed via [CopilotClient.resumeSession].
///
/// ```dart
/// final session = await client.createSession();
///
/// // Listen to all events.
/// session.events.listen((event) {
///   final type = event['type'] as String;
///   if (type == 'assistant.message') {
///     print(event['data']['content']);
///   }
/// });
///
/// // Send and wait for completion.
/// final response = await session.sendAndWait(MessageOptions(prompt: 'Hello!'));
/// print(response?['data']?['content']);
///
/// await session.destroy();
/// ```
class CopilotSession {
  /// Unique identifier for this session.
  final String sessionId;

  /// Path to the session workspace directory (when infinite sessions enabled).
  final String? workspacePath;

  final JsonRpcClient _connection;

  // Event handling.
  final StreamController<Map<String, dynamic>> _eventController =
      StreamController<Map<String, dynamic>>.broadcast();

  // Tool handling.
  final Map<String, ToolHandler> _toolHandlers = {};

  // Permission / input / hooks handlers.
  PermissionHandler? _permissionHandler;
  UserInputHandler? _userInputHandler;
  SessionHooks? _hooks;
  ExitPlanModeHandler? _exitPlanModeHandler;
  TraceContextProvider? _traceContextProvider;
  ShellAuthenticationHandler? _shellAuthenticationHandler;
  MemoryStoreHandler? _memoryStoreHandler;

  // Pending shell authentication prompts by request ID, completed on cancel.
  final Map<String, Completer<ShellAuthenticationResponse>>
      _pendingShellAuthentications = {};

  // Called once when the session is released (detached or destroyed).
  void Function()? _onClose;
  bool _closed = false;

  /// Creates a new [CopilotSession].
  ///
  /// This constructor is internal. Use [CopilotClient.createSession] or
  /// [CopilotClient.resumeSession] to create sessions.
  CopilotSession({
    required this.sessionId,
    required JsonRpcClient connection,
    this.workspacePath,
    TraceContextProvider? traceContextProvider,
    void Function()? onClose,
  })  : _connection = connection,
        _traceContextProvider = traceContextProvider,
        _onClose = onClose;

  /// Broadcast stream of all session events.
  ///
  /// Each event is a `Map<String, dynamic>` with at least:
  /// - `type`: The event type string (e.g. `assistant.message`, `session.idle`)
  /// - `id`: Event ID
  /// - `timestamp`: ISO 8601 timestamp
  /// - `data`: Event payload
  ///
  /// You can filter by type:
  /// ```dart
  /// session.events
  ///   .where((e) => e['type'] == 'assistant.message')
  ///   .listen((e) => print(e['data']['content']));
  /// ```
  Stream<Map<String, dynamic>> get events => _eventController.stream;

  /// Returns a filtered stream of events matching [eventType].
  ///
  /// Convenience wrapper over [events] with a type filter.
  Stream<Map<String, dynamic>> on(String eventType) {
    return _eventController.stream
        .where((event) => event['type'] == eventType);
  }

  /// Live shell output chunks (`tool.shell_output` events). Chunks are
  /// ephemeral: they are not persisted or replayed to late subscribers.
  Stream<ToolShellOutputData> get shellOutput =>
      on(SessionEventTypes.toolShellOutput).map((event) {
        final data = event['data'];
        return ToolShellOutputData.fromJson(
            data is Map<String, dynamic> ? data : const <String, dynamic>{});
      });

  /// Sends a message to this session.
  ///
  /// The message is enqueued for processing. Subscribe to [events] to receive
  /// streaming responses and other session events.
  ///
  /// Returns the message ID of the enqueued message.
  Future<String> send(MessageOptions options) async {
    final traceCtx = await _getTraceContext();
    final responseSchema = options.responseSchema;
    final response = await _connection.sendRequest('session.send', {
      ...traceCtx,
      'sessionId': sessionId,
      'prompt': options.prompt,
      if (options.attachments != null) 'attachments': options.attachments,
      if (options.mode != null) 'mode': options.mode,
      if (options.agentMode != null) 'agentMode': options.agentMode,
      if (options.displayPrompt != null) 'displayPrompt': options.displayPrompt,
      if (options.requestHeaders != null)
        'requestHeaders': options.requestHeaders,
      if (options.source != null) 'source': options.source,
      if (options.clientTurnId != null) 'clientTurnId': options.clientTurnId,
      if (responseSchema != null)
        'responseFormat': {
          'type': 'json_schema',
          'jsonSchema': {
            'name': 'response',
            'strict': true,
            'schema': responseSchema,
          },
        },
      if (options.imageOptions != null)
        'imageOptions': options.imageOptions!.toJson(),
    }) as Map<String, dynamic>;

    return response['messageId'] as String;
  }

  /// Sends a message and waits until the session becomes idle.
  ///
  /// This combines [send] with waiting for the `session.idle` event.
  /// Events are still delivered to [events] subscribers while waiting.
  ///
  /// Returns the final `assistant.message` event, or `null` if none received.
  ///
  /// [timeout] controls how long to wait (default 60 seconds).
  /// Throws [TimeoutException] if the timeout is reached.
  /// Throws if a `session.error` event is received.
  Future<Map<String, dynamic>?> sendAndWait(
    MessageOptions options, {
    Duration timeout = const Duration(seconds: 60),
  }) async {
    final idleCompleter = Completer<void>();
    Map<String, dynamic>? lastAssistantMessage;

    // Subscribe BEFORE sending to avoid race conditions.
    final subscription = events.listen((event) {
      final type = event['type'] as String?;
      if (type == 'assistant.message') {
        lastAssistantMessage = event;
      } else if (type == 'session.idle') {
        if (!idleCompleter.isCompleted) {
          idleCompleter.complete();
        }
      } else if (type == 'session.error') {
        if (!idleCompleter.isCompleted) {
          final data = event['data'] as Map<String, dynamic>?;
          final message = data?['message'] as String? ?? 'Unknown error';
          idleCompleter.completeError(Exception(message));
        }
      }
    });

    try {
      await send(options);
      await idleCompleter.future.timeout(timeout);
      return lastAssistantMessage;
    } finally {
      await subscription.cancel();
    }
  }

  /// Retrieves all events/messages from this session's history.
  Future<List<Map<String, dynamic>>> getMessages() async {
    final response = await _connection.sendRequest('session.getMessages', {
      'sessionId': sessionId,
    }) as Map<String, dynamic>;

    return (response['events'] as List<dynamic>)
        .cast<Map<String, dynamic>>();
  }

  /// Retrieves metadata for this session.
  Future<Map<String, dynamic>> getMetadata() async {
    final response = await _connection.sendRequest('session.getMetadata', {
      'sessionId': sessionId,
    }) as Map<String, dynamic>;

    return response;
  }

  /// Releases this session without deleting its on-disk state
  /// (`session.detach`), so it can be resumed later with
  /// [CopilotClient.resumeSession]. Falls back to the legacy
  /// `session.destroy` on runtimes without `session.detach`.
  Future<void> detach() async {
    if (_closed) return;
    dynamic response;
    try {
      response = await _connection.sendRequest('session.detach', {
        'sessionId': sessionId,
      });
    } on JsonRpcError catch (e) {
      if (!e.isMethodNotFound) rethrow;
      response = await _connection.sendRequest('session.destroy', {
        'sessionId': sessionId,
      });
    }
    if (response is Map && response['success'] == false) {
      throw Exception('Failed to detach session $sessionId: '
          '${response['error'] ?? 'Unknown error'}');
    }
    await _release();
  }

  /// Destroys this session and releases all resources.
  ///
  /// After calling this, the session can no longer be used. To continue the
  /// conversation, use [CopilotClient.resumeSession] with the session ID.
  /// Equivalent to [detach]: the on-disk state is kept.
  Future<void> destroy() => detach();

  Future<void> _release() async {
    if (_closed) return;
    _closed = true;
    _toolHandlers.clear();
    _permissionHandler = null;
    _userInputHandler = null;
    _hooks = null;
    _exitPlanModeHandler = null;
    _traceContextProvider = null;
    _shellAuthenticationHandler = null;
    _memoryStoreHandler = null;
    for (final pending in _pendingShellAuthentications.values) {
      if (!pending.isCompleted) {
        pending.complete(const ShellAuthenticationResponse.cancel());
      }
    }
    _pendingShellAuthentications.clear();
    final onClose = _onClose;
    _onClose = null;
    onClose?.call();
    await _eventController.close();
  }

  /// Aborts the currently processing message.
  ///
  /// The session remains valid and can continue to be used. Failures arrive
  /// as a [JsonRpcError].
  Future<void> abort() async {
    await _connection.sendRequest('session.abort', {
      'sessionId': sessionId,
    });
  }

  /// Changes the Auto routing preference without changing the session's
  /// model (`session.model.switchAutoTier`). [tier] is one of [AutoTier]
  /// (`fast`, `efficiency`, `balance`, `intelligence`), or null to return to
  /// the provider's default Auto routing. Experimental.
  Future<Map<String, dynamic>> switchAutoTier(String? tier) async {
    final result = await _connection.sendRequest(
        'session.model.switchAutoTier', {
      'sessionId': sessionId,
      'autoTier': tier,
    });
    return result is Map<String, dynamic> ? result : <String, dynamic>{};
  }

  /// Re-runs the managed-settings policy helper for this session
  /// (`session.managedSettings.recheckPolicyHelper`) and returns the
  /// managed settings snapshot (including `policyHelperFailure` when the
  /// helper still fails).
  Future<Map<String, dynamic>> recheckPolicyHelper() async {
    final result = await _connection.sendRequest(
        'session.managedSettings.recheckPolicyHelper', {
      'sessionId': sessionId,
    });
    return result is Map<String, dynamic> ? result : <String, dynamic>{};
  }

  /// Sets the permission mode (`session.permissions.setMode`): `manual`,
  /// `assisted`, or `allow-all`. A refused mode returns `success: false`
  /// with a [PermissionModeSetResult.reason] such as
  /// [PermissionModeRefusalReason.managedPolicy].
  Future<PermissionModeSetResult> setPermissionMode(String mode) async {
    final result = await _connection.sendRequest('session.permissions.setMode', {
      'sessionId': sessionId,
      'mode': mode,
    });
    return PermissionModeSetResult.fromJson(
        result is Map<String, dynamic> ? result : const <String, dynamic>{});
  }

  // -------------------------------------------------------------------------
  // Tool Registration (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Registers tool handlers for this session.
  void registerTools(List<Tool>? tools) {
    _toolHandlers.clear();
    if (tools == null) return;
    for (final tool in tools) {
      _toolHandlers[tool.name] = tool.handler;
    }
  }

  /// Gets a registered tool handler by name.
  ToolHandler? getToolHandler(String name) => _toolHandlers[name];

  /// Registers a permission handler for this session.
  void registerPermissionHandler(PermissionHandler? handler) {
    _permissionHandler = handler;
  }

  /// Registers a user input handler for this session.
  void registerUserInputHandler(UserInputHandler? handler) {
    _userInputHandler = handler;
  }

  /// Registers hook handlers for this session.
  void registerHooks(SessionHooks? hooks) {
    _hooks = hooks;
  }

  /// Registers an exit-plan-mode handler for this session.
  void registerExitPlanModeHandler(ExitPlanModeHandler? handler) {
    _exitPlanModeHandler = handler;
  }

  /// Registers a shell authentication handler for this session.
  void registerShellAuthenticationHandler(ShellAuthenticationHandler? handler) {
    _shellAuthenticationHandler = handler;
  }

  /// Registers an app-side memory store for this session.
  void registerMemoryStore(MemoryStoreHandler? handler) {
    _memoryStoreHandler = handler;
  }

  // -------------------------------------------------------------------------
  // Internal: Shell authentication / memory store (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Handles a `shellAuthentication.request`. Answers `cancel` when no handler
  /// is registered, the handler fails, or the runtime cancels the request
  /// first (see [cancelShellAuthenticationRequest]).
  Future<ShellAuthenticationResponse> handleShellAuthenticationRequest(
      ShellAuthenticationRequest request) async {
    final handler = _shellAuthenticationHandler;
    if (handler == null) return const ShellAuthenticationResponse.cancel();

    final pending = Completer<ShellAuthenticationResponse>();
    _pendingShellAuthentications[request.requestId] = pending;
    handler(request, SessionInvocationContext(sessionId: sessionId)).then(
      (response) {
        if (!pending.isCompleted) pending.complete(response);
      },
      onError: (Object _) {
        if (!pending.isCompleted) {
          pending.complete(const ShellAuthenticationResponse.cancel());
        }
      },
    );
    try {
      return await pending.future;
    } finally {
      _pendingShellAuthentications.remove(request.requestId);
    }
  }

  /// Handles a `shellAuthentication.cancel`: the pending prompt is answered
  /// with `cancel` and the handler's later answer is ignored.
  void cancelShellAuthenticationRequest(String requestId) {
    final pending = _pendingShellAuthentications.remove(requestId);
    if (pending != null && !pending.isCompleted) {
      pending.complete(const ShellAuthenticationResponse.cancel());
    }
  }

  /// Handles a `memoryStore.store` request by invoking the registered store.
  Future<void> handleMemoryStore(StoredMemory memory) async {
    final handler = _memoryStoreHandler;
    if (handler == null) {
      throw StateError('Memory store owner is unavailable');
    }
    await handler(memory, SessionInvocationContext(sessionId: sessionId));
  }

  // -------------------------------------------------------------------------
  // Internal: Event dispatch (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Dispatches a session event to all subscribers.
  ///
  /// This is called internally by [CopilotClient] when a `session.event`
  /// notification is received from the server.
  void dispatchEvent(Map<String, dynamic> event) {
    if (!_eventController.isClosed) {
      _eventController.add(event);
    }
  }

  // -------------------------------------------------------------------------
  // Internal: Permission handling (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Handles a permission request from the CLI server.
  Future<PermissionRequestResult> handlePermissionRequest(
      dynamic rawRequest) async {
    if (_permissionHandler == null) {
      return const PermissionRequestResult(
        kind: 'denied-no-approval-rule-and-could-not-request-from-user',
      );
    }

    try {
      final request = PermissionRequest.fromJson(
          rawRequest as Map<String, dynamic>);
      return await _permissionHandler!(
        request,
        SessionInvocationContext(sessionId: sessionId),
      );
    } catch (_) {
      return const PermissionRequestResult(
        kind: 'denied-no-approval-rule-and-could-not-request-from-user',
      );
    }
  }

  // -------------------------------------------------------------------------
  // Internal: User input handling (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Handles a user input request from the CLI server.
  Future<UserInputResponse> handleUserInputRequest(
      UserInputRequest request) async {
    if (_userInputHandler == null) {
      throw Exception('User input requested but no handler registered');
    }

    return await _userInputHandler!(
      request,
      SessionInvocationContext(sessionId: sessionId),
    );
  }

  // -------------------------------------------------------------------------
  // Internal: Hooks handling (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Handles a hooks invocation from the CLI server.
  Future<dynamic> handleHooksInvoke(String hookType, dynamic input) async {
    if (_hooks == null) return null;

    final context = SessionInvocationContext(sessionId: sessionId);
    final inputMap = input as Map<String, dynamic>? ?? {};

    try {
      switch (hookType) {
        case 'preToolUse':
          if (_hooks!.onPreToolUse == null) return null;
          final result = await _hooks!.onPreToolUse!(
            PreToolUseHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'postToolUse':
          if (_hooks!.onPostToolUse == null) return null;
          final result = await _hooks!.onPostToolUse!(
            PostToolUseHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'preMcpToolCall':
          if (_hooks!.onPreMcpToolCall == null) return null;
          return await _hooks!.onPreMcpToolCall!(inputMap, context);

        case 'userPromptSubmitted':
          if (_hooks!.onUserPromptSubmitted == null) return null;
          final result = await _hooks!.onUserPromptSubmitted!(
            UserPromptSubmittedHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'userPromptTransformed':
          if (_hooks!.onUserPromptTransformed == null) return null;
          final result = await _hooks!.onUserPromptTransformed!(
            UserPromptSubmittedHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'sessionStart':
          if (_hooks!.onSessionStart == null) return null;
          final result = await _hooks!.onSessionStart!(
            SessionStartHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'sessionEnd':
          if (_hooks!.onSessionEnd == null) return null;
          final result = await _hooks!.onSessionEnd!(
            SessionEndHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'errorOccurred':
          if (_hooks!.onErrorOccurred == null) return null;
          final result = await _hooks!.onErrorOccurred!(
            ErrorOccurredHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'subagentStart':
          if (_hooks!.onSubagentStart == null) return null;
          final result = await _hooks!.onSubagentStart!(
            SubagentStartHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        case 'subagentStop':
          if (_hooks!.onSubagentStop == null) return null;
          final result = await _hooks!.onSubagentStop!(
            SubagentStopHookInput.fromJson(inputMap),
            context,
          );
          return result?.toJson();

        default:
          return null;
      }
    } catch (_) {
      return null;
    }
  }

  // -------------------------------------------------------------------------
  // Internal: Exit Plan Mode handling (called by CopilotClient)
  // -------------------------------------------------------------------------

  /// Handles an exit-plan-mode request from the CLI server.
  Future<ExitPlanModeResult> handleExitPlanModeRequest(
      ExitPlanModeRequest request) async {
    if (_exitPlanModeHandler == null) {
      return const ExitPlanModeResult(approved: true);
    }
    return await _exitPlanModeHandler!(
      request,
      SessionInvocationContext(sessionId: sessionId),
    );
  }

  // -------------------------------------------------------------------------
  // Internal: Trace Context helper
  // -------------------------------------------------------------------------

  /// Returns the current W3C Trace Context, or an empty map if unavailable.
  Future<Map<String, dynamic>> _getTraceContext() async {
    if (_traceContextProvider == null) return {};
    try {
      final ctx = await _traceContextProvider!();
      return ctx.toJson();
    } catch (_) {
      return {};
    }
  }
}
