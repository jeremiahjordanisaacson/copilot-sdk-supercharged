/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e

import java.util.regex.Matcher
import java.util.regex.Pattern

/**
 * Manages the shared replaying CAPI proxy for Groovy E2E tests.
 *
 * <p>Spawns {@code npx tsx ../../test/harness/server.ts} and reads stdout
 * to discover the proxy URL. The proxy replays recorded HTTP exchanges
 * so tests run deterministically without a real backend.
 */
class TestHarness {
    private static final Pattern URL_PATTERN = Pattern.compile(/Listening: (http:\/\/[^\s]+)/)
    private static final Pattern METADATA_PATTERN = Pattern.compile(/(\{.*\})\s*$/)

    private Process process
    private String proxyUrl
    private String connectProxyUrl
    private String caFilePath

    /**
     * Starts the replay proxy and returns the URL it is listening on.
     *
     * @return the proxy URL, e.g. {@code http://localhost:12345}
     * @throws RuntimeException if the proxy fails to start or does not
     *         emit the expected "Listening:" line
     */
    String startProxy() {
        if (proxyUrl) return proxyUrl

        // Resolve server.ts relative to this file's expected location (groovy/e2e/)
        File serverScript = new File('../../test/harness/server.ts').canonicalFile
        if (!serverScript.exists()) {
            // Fall back to locating via project structure
            String basePath = System.getProperty('user.dir') ?: '.'
            serverScript = new File(basePath, '../../test/harness/server.ts').canonicalFile
        }
        if (!serverScript.exists()) {
            // Try from groovy/ dir (Gradle project root)
            String basePath = System.getProperty('user.dir') ?: '.'
            serverScript = new File(basePath, '../test/harness/server.ts').canonicalFile
        }
        File workingDir = serverScript.parentFile

        boolean isWindows = System.getProperty('os.name', '').toLowerCase().contains('win')

        List<String> command
        if (isWindows) {
            command = ['cmd', '/c', 'npx', 'tsx', serverScript.absolutePath]
        } else {
            command = ['npx', 'tsx', serverScript.absolutePath]
        }

        ProcessBuilder pb = new ProcessBuilder(command)
            .directory(workingDir)
            .redirectErrorStream(false)

        // Inherit stderr so proxy debug output is visible during test runs
        pb.redirectError(ProcessBuilder.Redirect.INHERIT)

        process = pb.start()

        // Read the first line of stdout to discover the listening URL
        BufferedReader reader = new BufferedReader(new InputStreamReader(process.inputStream))
        String line = reader.readLine()

        if (!line) {
            process.destroyForcibly()
            throw new RuntimeException('Failed to read proxy URL — process produced no output')
        }

        Matcher matcher = URL_PATTERN.matcher(line.trim())
        if (!matcher.find()) {
            process.destroyForcibly()
            throw new RuntimeException("Unexpected proxy output: ${line}")
        }

        proxyUrl = matcher.group(1)

        // Parse connect proxy metadata JSON from the same line
        Matcher metaMatcher = METADATA_PATTERN.matcher(line.trim())
        if (metaMatcher.find()) {
            try {
                def json = new groovy.json.JsonSlurper().parseText(metaMatcher.group(1))
                connectProxyUrl = json.connectProxyUrl
                caFilePath = json.caFilePath
            } catch (Exception ignored) {
                // Non-fatal — tests can still work without connect proxy metadata
            }
        }

        proxyUrl
    }

    /**
     * Resolves the CLI executable path.
     */
    String getCliPath() {
        String envPath = System.getenv('COPILOT_CLI_PATH')
        if (envPath && new File(envPath).exists()) {
            return new File(envPath).absolutePath
        }

        File repoRoot = findRepoRoot()
        File nodeCliPath = new File(repoRoot, 'nodejs/node_modules/@github/copilot/index.js')
        if (nodeCliPath.exists()) {
            return nodeCliPath.absolutePath
        }

        return 'copilot'
    }

    /**
     * Returns environment variables that route CLI traffic through the proxy.
     */
    Map<String, String> getTestEnv(String workDir) {
        Map<String, String> env = [:]
        env.putAll(System.getenv())
        env['COPILOT_API_URL'] = proxyUrl ?: ''
        env['COPILOT_HOME'] = workDir
        env['XDG_CONFIG_HOME'] = workDir
        env['XDG_STATE_HOME'] = workDir
        env['GH_TOKEN'] = System.getenv('GH_TOKEN') ?: 'fake-test-token'
        env['GITHUB_TOKEN'] = System.getenv('GITHUB_TOKEN') ?: 'fake-test-token'

        if (connectProxyUrl) {
            env['HTTP_PROXY'] = connectProxyUrl
            env['HTTPS_PROXY'] = connectProxyUrl
            env['http_proxy'] = connectProxyUrl
            env['https_proxy'] = connectProxyUrl
            env['NO_PROXY'] = '127.0.0.1,localhost,::1'
            env['no_proxy'] = '127.0.0.1,localhost,::1'
        }
        if (caFilePath) {
            env['NODE_EXTRA_CA_CERTS'] = caFilePath
            env['SSL_CERT_FILE'] = caFilePath
        }

        env
    }

    /**
     * Stops the replay proxy process.
     */
    void stopProxy() {
        if (!process) return

        // Best-effort graceful stop via the /stop endpoint
        if (proxyUrl) {
            try {
                URL stopUrl = new URL("${proxyUrl}/stop?skipWritingCache=true")
                HttpURLConnection conn = (HttpURLConnection) stopUrl.openConnection()
                conn.requestMethod = 'POST'
                conn.connectTimeout = 2000
                conn.readTimeout = 2000
                conn.responseCode // trigger the request
                conn.disconnect()
            } catch (Exception ignored) {
                // Best effort — process.destroy below is the backstop
            }
        }

        try {
            process.destroyForcibly()
            process.waitFor()
        } catch (Exception ignored) {}

        process = null
        proxyUrl = null
        connectProxyUrl = null
        caFilePath = null
    }

    /**
     * Find the repo root by walking up from the current directory.
     */
    private static File findRepoRoot() {
        File dir = new File(System.getProperty('user.dir') ?: '.').canonicalFile
        for (int i = 0; i < 10; i++) {
            if (new File(dir, 'test/harness/server.ts').exists()) return dir
            dir = dir.parentFile
            if (!dir) break
        }
        return new File(System.getProperty('user.dir') ?: '.').canonicalFile
    }
}
