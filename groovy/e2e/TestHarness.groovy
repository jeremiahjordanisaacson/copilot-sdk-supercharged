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

    private Process process
    private String proxyUrl

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
        proxyUrl
    }

    /**
     * Extracts the {@code host:port} portion from the proxy URL,
     * suitable for passing as {@code cliUrl} to the SDK client.
     *
     * @return host:port string, e.g. {@code localhost:12345}
     */
    String getCliUrl() {
        if (!proxyUrl) throw new IllegalStateException('Proxy not started')
        URI uri = new URI(proxyUrl)
        "${uri.host}:${uri.port}"
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
    }
}
