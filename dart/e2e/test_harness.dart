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
  String? _connectProxyUrl;
  String? _caFilePath;
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

    // Parse "Listening: http://..." from the line
    final match = RegExp(r'Listening:\s*(http://\S+)').firstMatch(firstLine);
    if (match == null) {
      _process?.kill();
      throw StateError('Unexpected proxy output: $firstLine');
    }

    _proxyUrl = match.group(1)!;

    // Parse connect proxy metadata JSON from the same line
    final metaMatch = RegExp(r'\{.*\}\s*$').firstMatch(firstLine);
    if (metaMatch != null) {
      try {
        final meta =
            jsonDecode(metaMatch.group(0)!) as Map<String, dynamic>;
        _connectProxyUrl = meta['connectProxyUrl'] as String?;
        _caFilePath = meta['caFilePath'] as String?;
      } catch (_) {
        // Non-fatal — tests can still work without connect proxy metadata
      }
    }

    print('TestHarness: proxy started at $_proxyUrl');
    await _registerDefaultCopilotUsers();
    return _proxyUrl!;
  }

  /// Registers the replay user answered by the proxy's
  /// `/copilot_internal/user` endpoint for [token]. Current CLIs resolve the
  /// Copilot API endpoint from this user (through
  /// `COPILOT_DEBUG_GITHUB_API_URL`), so every token the tests use must be
  /// known.
  Future<void> setCopilotUserByToken(String token) async {
    if (_proxyUrl == null) {
      throw StateError('Proxy not started');
    }

    final client = HttpClient();
    try {
      final request =
          await client.postUrl(Uri.parse('$_proxyUrl/copilot-user-config'));
      request.headers.contentType = ContentType.json;
      request.write(jsonEncode({
        'token': token,
        'response': {
          'login': 'e2e-test-user',
          'id': 12345,
          'copilot_plan': 'individual_pro',
          'endpoints': {
            'api': _proxyUrl,
            'telemetry': 'https://sdk-hydro-collector.example.com',
          },
          'analytics_tracking_id': 'e2e-test-tracking-id',
        },
      }));
      final response = await request.close();
      await response.drain<void>();
      if (response.statusCode != 200) {
        throw StateError(
            'Proxy copilot user config failed (${response.statusCode})');
      }
    } finally {
      client.close();
    }
  }

  /// Registers the tokens the E2E clients authenticate with (see
  /// [getTestEnv]).
  Future<void> _registerDefaultCopilotUsers() async {
    final tokens = <String>{
      for (final token in [
        Platform.environment['GH_TOKEN'],
        Platform.environment['GITHUB_TOKEN'],
        'fake-test-token',
      ])
        if (token != null && token.isNotEmpty) token,
    };
    for (final token in tokens) {
      await setCopilotUserByToken(token);
    }
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

  /// Resolve the path to the Copilot CLI executable.
  ///
  /// Lookup order: `COPILOT_CLI_PATH` (when the file exists), the CLI 1.0.113+
  /// `@github/copilot/npm-loader.js` (run with node), then the legacy
  /// `index.js` entry points. Paths without a `.js` suffix (for example the
  /// `node_modules/.bin/copilot` shim) are spawned directly.
  String getCliPath() {
    final envPath = Platform.environment['COPILOT_CLI_PATH'];
    if (envPath != null && envPath.isNotEmpty && File(envPath).existsSync()) {
      return File(envPath).absolute.path;
    }

    final sep = Platform.pathSeparator;
    final githubModules =
        Directory('$_repoRoot${sep}nodejs${sep}node_modules${sep}@github');

    final npmLoader = File('${githubModules.path}${sep}copilot'
        '${sep}npm-loader.js');
    if (npmLoader.existsSync()) {
      return npmLoader.absolute.path;
    }

    // CLI 1.0.64-1 through 1.0.112 ship a runnable index.js in a
    // platform-specific package (e.g. @github/copilot-linux-x64).
    if (githubModules.existsSync()) {
      for (final entry in githubModules.listSync()) {
        if (entry is Directory) {
          final name = entry.path.split(sep).last;
          if (name.startsWith('copilot-') &&
              !name.contains('language-server')) {
            final idx = File('${entry.path}${sep}index.js');
            if (idx.existsSync()) {
              return idx.absolute.path;
            }
          }
        }
      }
    }

    final nodeCli = File('$_repoRoot${sep}nodejs'
        '${sep}node_modules${sep}'
        '@github${sep}copilot'
        '${sep}index.js');
    if (nodeCli.existsSync()) {
      return nodeCli.absolute.path;
    }

    return 'copilot';
  }

  /// Environment variables for the CLI process, routing traffic through the proxy.
  Map<String, String> getTestEnv(String workDir) {
    final env = Map<String, String>.from(Platform.environment);
    // Signing keys would make the CLI sign requests the replay proxy cannot
    // verify.
    env.remove('COPILOT_HMAC_KEY');
    env.remove('CAPI_HMAC_KEY');
    env['COPILOT_API_URL'] = _proxyUrl ?? '';
    // Route GitHub API calls (including /copilot_internal/user) to the proxy.
    env['COPILOT_DEBUG_GITHUB_API_URL'] = _proxyUrl ?? '';
    env['COPILOT_HOME'] = workDir;
    env['GH_CONFIG_DIR'] = workDir;
    env['XDG_CONFIG_HOME'] = workDir;
    env['XDG_STATE_HOME'] = workDir;
    env['GH_TOKEN'] = env['GH_TOKEN'] ?? 'fake-test-token';
    env['GITHUB_TOKEN'] = env['GITHUB_TOKEN'] ?? 'fake-test-token';

    if (_connectProxyUrl != null && _connectProxyUrl!.isNotEmpty) {
      env['HTTP_PROXY'] = _connectProxyUrl!;
      env['HTTPS_PROXY'] = _connectProxyUrl!;
      env['http_proxy'] = _connectProxyUrl!;
      env['https_proxy'] = _connectProxyUrl!;
      env['NO_PROXY'] = '127.0.0.1,localhost,::1';
      env['no_proxy'] = '127.0.0.1,localhost,::1';
    }
    if (_caFilePath != null && _caFilePath!.isNotEmpty) {
      env['NODE_EXTRA_CA_CERTS'] = _caFilePath!;
      env['SSL_CERT_FILE'] = _caFilePath!;
    }

    return env;
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
