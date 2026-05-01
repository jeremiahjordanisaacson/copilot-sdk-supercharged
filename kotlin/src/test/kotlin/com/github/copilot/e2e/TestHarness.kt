/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot.e2e

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

/**
 * Manages a replaying CAPI proxy for E2E tests.
 *
 * Spawns the shared test harness server from `test/harness/server.ts` and provides
 * methods to configure snapshot replay and stop the proxy.
 */
class TestHarness {
    private var process: Process? = null

    /** The URL the proxy is listening on after [start]. */
    var proxyUrl: String? = null
        private set

    /**
     * Start the replay proxy server.
     *
     * @return The proxy URL (e.g. `http://127.0.0.1:PORT`).
     * @throws RuntimeException if the proxy cannot be started or URL cannot be parsed.
     */
    fun start(): String {
        proxyUrl?.let { return it }

        // Resolve path to test/harness/server.ts relative to this source file's package root.
        // In a Gradle project this file is at kotlin/src/test/kotlin/com/github/copilot/e2e/
        // The repo root is 8 levels up from this directory.
        val classResource = TestHarness::class.java.protectionDomain.codeSource?.location
        val repoRoot = findRepoRoot()
        val serverPath = repoRoot.resolve("test/harness/server.ts").absolutePath
        val serverDir = repoRoot.resolve("test/harness")

        val isWindows = System.getProperty("os.name").lowercase().contains("win")

        val command = if (isWindows) {
            listOf("cmd", "/c", "npx", "tsx", serverPath)
        } else {
            listOf("npx", "tsx", serverPath)
        }

        val pb = ProcessBuilder(command)
            .directory(serverDir)
            .redirectErrorStream(false)

        // Inherit stderr for debugging
        pb.redirectError(ProcessBuilder.Redirect.INHERIT)

        val proc = pb.start()
        this.process = proc

        // Read the first line to get the listening URL
        val reader = BufferedReader(InputStreamReader(proc.inputStream))
        val line = reader.readLine()
            ?: run {
                kill()
                throw RuntimeException("Failed to read proxy URL from stdout")
            }

        val regex = Regex("""Listening:\s*(http://\S+)""")
        val match = regex.find(line)
            ?: run {
                kill()
                throw RuntimeException("Unexpected proxy output: $line")
            }

        val url = match.groupValues[1]
        this.proxyUrl = url
        return url
    }

    /**
     * Configure the proxy for a specific test snapshot.
     *
     * @param snapshotPath Absolute path to the YAML snapshot file.
     * @param workDir Working directory for the test.
     */
    fun configure(snapshotPath: String, workDir: String) {
        val url = proxyUrl ?: throw RuntimeException("Proxy not started")

        val connection = URL("$url/config").openConnection() as HttpURLConnection
        connection.requestMethod = "POST"
        connection.setRequestProperty("Content-Type", "application/json")
        connection.doOutput = true
        connection.connectTimeout = 10_000
        connection.readTimeout = 10_000

        val payload = """{"filePath":"$snapshotPath","workDir":"$workDir"}"""
        connection.outputStream.use { it.write(payload.toByteArray()) }

        val status = connection.responseCode
        if (status != 200) {
            throw RuntimeException("Failed to configure replay proxy: HTTP $status")
        }
        connection.disconnect()
    }

    /**
     * Stop the replay proxy server.
     */
    fun stop() {
        proxyUrl?.let { url ->
            try {
                val connection = URL("$url/stop").openConnection() as HttpURLConnection
                connection.requestMethod = "POST"
                connection.setRequestProperty("Content-Type", "application/json")
                connection.doOutput = true
                connection.connectTimeout = 5_000
                connection.readTimeout = 5_000
                connection.outputStream.use { it.write("{}".toByteArray()) }
                connection.responseCode  // trigger the request
                connection.disconnect()
            } catch (_: Exception) {
                // Best effort
            }
        }

        kill()
    }

    private fun kill() {
        process?.let { proc ->
            if (proc.isAlive) {
                proc.destroyForcibly()
                proc.waitFor()
            }
        }
        process = null
        proxyUrl = null
    }

    companion object {
        /**
         * Find the repository root by walking up from the current working directory
         * or known Gradle project structure.
         */
        fun findRepoRoot(): java.io.File {
            // Try from Gradle project dir (kotlin/ is the project dir)
            var dir = java.io.File(System.getProperty("user.dir"))

            // Walk up until we find test/harness/server.ts
            repeat(10) {
                if (dir.resolve("test/harness/server.ts").exists()) {
                    return dir
                }
                dir = dir.parentFile ?: return@repeat
            }

            // Fallback: assume we're in the kotlin/ subdirectory
            val kotlinDir = java.io.File(System.getProperty("user.dir"))
            val candidate = kotlinDir.parentFile
            if (candidate != null && candidate.resolve("test/harness/server.ts").exists()) {
                return candidate
            }

            throw RuntimeException(
                "Cannot find repo root (test/harness/server.ts) from ${System.getProperty("user.dir")}"
            )
        }
    }
}
