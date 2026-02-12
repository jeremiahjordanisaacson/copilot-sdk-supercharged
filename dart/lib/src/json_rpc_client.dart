// Copyright (c) Microsoft Corporation. All rights reserved.

/// JSON-RPC 2.0 client with Content-Length header framing.
///
/// This implements the JSON-RPC 2.0 protocol over a byte stream (stdio or TCP),
/// using `Content-Length: <n>\r\n\r\n` framing identical to LSP / vscode-jsonrpc.

import 'dart:async';
import 'dart:convert';
import 'dart:io';

/// A single pending JSON-RPC request awaiting a response.
class _PendingRequest {
  final Completer<dynamic> completer;
  final String method;
  final Timer? timeoutTimer;

  _PendingRequest({
    required this.completer,
    required this.method,
    this.timeoutTimer,
  });
}

/// Callback for server-to-client JSON-RPC requests (e.g. tool.call).
/// Returns the result to send back.
typedef RequestHandler = Future<dynamic> Function(dynamic params);

/// JSON-RPC 2.0 client using Content-Length header framing.
///
/// Reads/writes messages of the form:
/// ```
/// Content-Length: <n>\r\n
/// \r\n
/// <n bytes of JSON>
/// ```
class JsonRpcClient {
  final Stream<List<int>> _input;
  final IOSink _output;

  int _nextId = 1;
  final Map<int, _PendingRequest> _pending = {};
  final Map<String, RequestHandler> _requestHandlers = {};

  /// Stream of JSON-RPC notifications from the server.
  /// Each element is a Map with 'method' and 'params'.
  final StreamController<Map<String, dynamic>> _notificationController =
      StreamController<Map<String, dynamic>>.broadcast();

  Stream<Map<String, dynamic>> get notifications =>
      _notificationController.stream;

  /// Buffer for accumulating incoming bytes.
  final List<int> _buffer = [];

  /// Whether the client has been disposed.
  bool _disposed = false;

  StreamSubscription<List<int>>? _inputSubscription;

  /// Default request timeout in milliseconds.
  final int requestTimeoutMs;

  JsonRpcClient({
    required Stream<List<int>> input,
    required IOSink output,
    this.requestTimeoutMs = 120000,
  })  : _input = input,
        _output = output;

  /// Start listening for messages on the input stream.
  void listen() {
    _inputSubscription = _input.listen(
      _onData,
      onError: _onError,
      onDone: _onDone,
    );
  }

  /// Register a handler for server-to-client requests (reverse RPC).
  void onRequest(String method, RequestHandler handler) {
    _requestHandlers[method] = handler;
  }

  /// Send a JSON-RPC request and wait for the response.
  Future<dynamic> sendRequest(String method, [dynamic params]) {
    if (_disposed) {
      return Future.error(StateError('JsonRpcClient has been disposed'));
    }

    final id = _nextId++;
    final completer = Completer<dynamic>();

    Timer? timer;
    if (requestTimeoutMs > 0) {
      timer = Timer(Duration(milliseconds: requestTimeoutMs), () {
        final pending = _pending.remove(id);
        if (pending != null && !pending.completer.isCompleted) {
          pending.completer.completeError(
            TimeoutException(
              'JSON-RPC request "$method" (id=$id) timed out after ${requestTimeoutMs}ms',
            ),
          );
        }
      });
    }

    _pending[id] = _PendingRequest(
      completer: completer,
      method: method,
      timeoutTimer: timer,
    );

    final message = <String, dynamic>{
      'jsonrpc': '2.0',
      'id': id,
      'method': method,
    };
    if (params != null) {
      message['params'] = params;
    }

    _writeMessage(message);
    return completer.future;
  }

  /// Send a JSON-RPC notification (no response expected).
  void sendNotification(String method, [dynamic params]) {
    if (_disposed) return;

    final message = <String, dynamic>{
      'jsonrpc': '2.0',
      'method': method,
    };
    if (params != null) {
      message['params'] = params;
    }

    _writeMessage(message);
  }

  /// Dispose the client: cancel pending requests, close streams.
  void dispose() {
    if (_disposed) return;
    _disposed = true;

    _inputSubscription?.cancel();
    _inputSubscription = null;

    // Complete all pending requests with an error.
    for (final entry in _pending.values) {
      entry.timeoutTimer?.cancel();
      if (!entry.completer.isCompleted) {
        entry.completer.completeError(
          StateError('JsonRpcClient disposed while "${entry.method}" pending'),
        );
      }
    }
    _pending.clear();

    _notificationController.close();
  }

  // ---------------------------------------------------------------------------
  // Private
  // ---------------------------------------------------------------------------

  void _writeMessage(Map<String, dynamic> message) {
    final json = jsonEncode(message);
    final bytes = utf8.encode(json);
    final header = 'Content-Length: ${bytes.length}\r\n\r\n';
    try {
      _output.add(utf8.encode(header));
      _output.add(bytes);
    } catch (_) {
      // Output may be closed already during shutdown.
    }
  }

  void _onData(List<int> chunk) {
    _buffer.addAll(chunk);
    _processBuffer();
  }

  void _processBuffer() {
    while (true) {
      // Look for the header/body separator "\r\n\r\n".
      final separatorIndex = _indexOfSeparator();
      if (separatorIndex == -1) break;

      // Parse Content-Length from header section.
      final headerBytes = _buffer.sublist(0, separatorIndex);
      final headerStr = utf8.decode(headerBytes);
      final contentLength = _parseContentLength(headerStr);
      if (contentLength == null) {
        // Malformed header - skip past separator and try again.
        _buffer.removeRange(0, separatorIndex + 4);
        continue;
      }

      final bodyStart = separatorIndex + 4; // past \r\n\r\n
      final bodyEnd = bodyStart + contentLength;

      if (_buffer.length < bodyEnd) {
        // Not enough data yet.
        break;
      }

      // Extract body.
      final bodyBytes = _buffer.sublist(bodyStart, bodyEnd);
      _buffer.removeRange(0, bodyEnd);

      try {
        final bodyStr = utf8.decode(bodyBytes);
        final message = jsonDecode(bodyStr) as Map<String, dynamic>;
        _handleMessage(message);
      } catch (e) {
        // Malformed JSON - skip.
      }
    }
  }

  int _indexOfSeparator() {
    // Search for \r\n\r\n in _buffer.
    for (var i = 0; i <= _buffer.length - 4; i++) {
      if (_buffer[i] == 0x0D &&
          _buffer[i + 1] == 0x0A &&
          _buffer[i + 2] == 0x0D &&
          _buffer[i + 3] == 0x0A) {
        return i;
      }
    }
    return -1;
  }

  int? _parseContentLength(String header) {
    for (final line in header.split('\r\n')) {
      final trimmed = line.trim();
      if (trimmed.toLowerCase().startsWith('content-length:')) {
        final value = trimmed.substring('content-length:'.length).trim();
        return int.tryParse(value);
      }
    }
    return null;
  }

  void _handleMessage(Map<String, dynamic> message) {
    if (message.containsKey('id') && message.containsKey('method')) {
      // Server-to-client request (reverse RPC).
      _handleServerRequest(message);
    } else if (message.containsKey('id')) {
      // Response to one of our requests.
      _handleResponse(message);
    } else if (message.containsKey('method')) {
      // Notification.
      _handleNotification(message);
    }
  }

  void _handleResponse(Map<String, dynamic> message) {
    final id = message['id'] as int?;
    if (id == null) return;

    final pending = _pending.remove(id);
    if (pending == null) return;

    pending.timeoutTimer?.cancel();

    if (message.containsKey('error')) {
      final error = message['error'] as Map<String, dynamic>;
      final code = error['code'] as int? ?? -1;
      final errorMessage = error['message'] as String? ?? 'Unknown error';
      final data = error['data'];
      pending.completer.completeError(
        JsonRpcError(code: code, message: errorMessage, data: data),
      );
    } else {
      pending.completer.complete(message['result']);
    }
  }

  void _handleNotification(Map<String, dynamic> message) {
    if (!_notificationController.isClosed) {
      _notificationController.add({
        'method': message['method'],
        'params': message['params'],
      });
    }
  }

  Future<void> _handleServerRequest(Map<String, dynamic> message) async {
    final id = message['id'];
    final method = message['method'] as String;
    final params = message['params'];

    final handler = _requestHandlers[method];
    if (handler == null) {
      // Method not found.
      _writeMessage({
        'jsonrpc': '2.0',
        'id': id,
        'error': {
          'code': -32601,
          'message': 'Method not found: $method',
        },
      });
      return;
    }

    try {
      final result = await handler(params);
      _writeMessage({
        'jsonrpc': '2.0',
        'id': id,
        'result': result,
      });
    } catch (e) {
      _writeMessage({
        'jsonrpc': '2.0',
        'id': id,
        'error': {
          'code': -32603,
          'message': e.toString(),
        },
      });
    }
  }

  void _onError(Object error) {
    // Fail all pending requests.
    for (final entry in _pending.values) {
      entry.timeoutTimer?.cancel();
      if (!entry.completer.isCompleted) {
        entry.completer.completeError(error);
      }
    }
    _pending.clear();
  }

  void _onDone() {
    // Input stream closed.
    for (final entry in _pending.values) {
      entry.timeoutTimer?.cancel();
      if (!entry.completer.isCompleted) {
        entry.completer.completeError(
          StateError('Connection closed while "${entry.method}" pending'),
        );
      }
    }
    _pending.clear();

    if (!_notificationController.isClosed) {
      _notificationController.close();
    }
  }
}

// ---------------------------------------------------------------------------
// JSON-RPC Error
// ---------------------------------------------------------------------------

/// Error returned by the JSON-RPC server.
class JsonRpcError implements Exception {
  final int code;
  final String message;
  final dynamic data;

  const JsonRpcError({
    required this.code,
    required this.message,
    this.data,
  });

  @override
  String toString() => 'JsonRpcError($code): $message';
}
