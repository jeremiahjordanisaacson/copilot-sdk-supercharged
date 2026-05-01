// Copyright (c) Microsoft Corporation. All rights reserved.

/// Test harness for Dart E2E tests.
///
/// Manages a replaying CAPI proxy (test/harness/server.ts) that replays
/// snapshot exchanges so SDK tests can run without hitting live APIs.

import 'dart:convert';
import 'dart:io';

/// Manages the replaying proxy server lifecycle for E2E tests.
class TestHarness {
  Process? _process;
  String? _proxyUrl;
  late final String _repoRoot;

  TestHarness() {
    _repoRoot = _computeRepoRoot();
  }

  /// The proxy URL, or null if not started.
  String? get proxyUrl => _proxyUrl;

  /// Absolute path to the repository root.
  String get repoRoot => _repoRoot;

  /// Start the replay proxy server.
  ///
  /// Returns the proxy URL (e.g. "http://127.0.0.1:12345").
  /// Throws if the server fails to start.
  Future<String> start() async {
    if (_proxyUrl != null) return _proxyUrl!;

    final serverPath = '$_repoRoot${Platform.pathSeparator}test'
        '${Platform.pathSeparator}harness${Platform.pathSeparator}server.ts';

    final harnessDir = '$_repoRoot${Platform.pathSeparator}test'
        '${Platform.pathSeparator}harness';

    // On Windows, npx needs shell: true
    _process = await Process.start(
      'npx',
      ['tsx', serverPath],
      workingDirectory: harnessDir,
      runInShell: Platform.isWindows,
    );

    // Forward stderr to the console for debugging
    _process!.stderr.transform(utf8.decoder).listen(stderr.write);

    // Read the first line to get the listening URL
    final stdoutLines = _process!.stdout
        .transform(utf8.decoder)
        .transform(const LineSplitter());

    final firstLine = await stdoutLines.first.timeout(
      const Duration(seconds: 30),
      onTimeout: () {
        _process?.kill();
        throw StateError('Timed out waiting for proxy to start');
      },
    );

    // Parse "Listening: http://..."
    final match = RegExp(r'Listening:\s*(http://\S+)').firstMatch(firstLine);
    if (match == null) {
      _process?.kill();
      throw StateError('Unexpected proxy output: $firstLine');
    }

    _proxyUrl = match.group(1)!;
    print('TestHarness: proxy started at $_proxyUrl');
    return _proxyUrl!;
  }

  /// Configure the proxy for a specific test snapshot.
  ///
  /// [snapshotPath] should be an absolute path to the snapshot YAML file.
  /// [workDir] is the working directory for the test.
  Future<void> configure(String snapshotPath, String workDir) async {
    if (_proxyUrl == null) {
      throw StateError('Proxy not started');
    }

    final client = HttpClient();
    try {
      final request = await client.postUrl(Uri.parse('$_proxyUrl/config'));
      request.headers.contentType = ContentType.json;
      request.write(jsonEncode({
        'filePath': snapshotPath,
        'workDir': workDir,
      }));

      final response = await request.close();
      if (response.statusCode != 200) {
        final body = await response.transform(utf8.decoder).join();
        throw StateError(
            'Proxy config failed (${response.statusCode}): $body');
      }
      await response.drain<void>();
    } finally {
      client.close();
    }
  }

  /// Gracefully stop the proxy server.
  Future<void> stop() async {
    if (_proxyUrl == null) return;

    // Best-effort POST to /stop
    final client = HttpClient();
    try {
      final request = await client
          .postUrl(Uri.parse('$_proxyUrl/stop?skipWritingCache=true'));
      request.headers.contentType = ContentType.json;
      request.write('{}');
      final response = await request.close();
      await response.drain<void>();
    } catch (_) {
      // Best effort
    } finally {
      client.close();
    }

    // Wait for the process to exit
    _process?.kill();
    await _process?.exitCode.timeout(
      const Duration(seconds: 5),
      onTimeout: () {
        _process?.kill(ProcessSignal.sigkill);
        return -1;
      },
    );

    _process = null;
    _proxyUrl = null;
  }

  /// Build an absolute snapshot path from a path relative to the repo root.
  String snapshotPath(String relativePath) {
    return '$_repoRoot${Platform.pathSeparator}$relativePath';
  }

  /// Build a snapshot path using forward slashes (as stored in the repo).
  String snapshotPathPosix(String posixRelativePath) {
    if (Platform.isWindows) {
      return '$_repoRoot\\${posixRelativePath.replaceAll('/', '\\')}';
    }
    return '$_repoRoot/$posixRelativePath';
  }

  /// Compute the repository root.
  ///
  /// Tries COPILOT_REPO_ROOT env var first, then walks up from the script dir.
  static String _computeRepoRoot() {
    final envRoot = Platform.environment['COPILOT_REPO_ROOT'];
    if (envRoot != null && envRoot.isNotEmpty) {
      return envRoot;
    }

    // Walk up from the current script directory:
    // <repo>/dart/e2e/ -> <repo>
    var dir = Directory.current;

    // If we're in dart/e2e, go up twice
    if (dir.path.endsWith('e2e') || dir.path.endsWith('dart')) {
      dir = dir.parent;
      if (dir.path.endsWith('dart')) {
        dir = dir.parent;
      }
    }

    // Check if this looks like the repo root (has test/harness/server.ts)
    final serverTs = File(
        '${dir.path}${Platform.pathSeparator}test${Platform.pathSeparator}harness${Platform.pathSeparator}server.ts');
    if (serverTs.existsSync()) {
      return dir.path;
    }

    // Walk up until we find it
    var current = Directory.current;
    for (var i = 0; i < 10; i++) {
      final check = File(
          '${current.path}${Platform.pathSeparator}test${Platform.pathSeparator}harness${Platform.pathSeparator}server.ts');
      if (check.existsSync()) return current.path;
      final parent = current.parent;
      if (parent.path == current.path) break;
      current = parent;
    }

    return Directory.current.path;
  }
}
