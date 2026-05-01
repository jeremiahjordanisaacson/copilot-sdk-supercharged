/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.github.copilot.e2e

import com.github.copilot.*
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*
import java.io.File
import java.nio.file.Files

/**
 * E2E tests for the Kotlin Copilot SDK session lifecycle.
 *
 * These tests use the shared replay proxy and YAML snapshots to exercise
 * session creation, messaging, and filesystem configuration.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class SessionE2ETest {
    private lateinit var harness: TestHarness
    private lateinit var proxyUrl: String
    private lateinit var repoRoot: File
    private lateinit var snapshotsDir: String
    private lateinit var workDir: String

    @BeforeAll
    fun setUpAll() {
        repoRoot = TestHarness.findRepoRoot()
        snapshotsDir = repoRoot.resolve("test/snapshots").absolutePath

        // Create an isolated work directory
        workDir = Files.createTempDirectory("copilot-kotlin-e2e-").toFile().absolutePath

        harness = TestHarness()
        proxyUrl = harness.start()
    }

    @AfterAll
    fun tearDownAll() {
        harness.stop()

        // Clean up work directory
        File(workDir).deleteRecursively()
    }

    // -- Helpers --

    private fun configureSnapshot(category: String, testName: String) {
        val snapshotPath = "$snapshotsDir${File.separator}$category${File.separator}$testName.yaml"
        harness.configure(snapshotPath, workDir)
    }

    private fun getCliPath(): String {
        val envPath = System.getenv("COPILOT_CLI_PATH")
        if (!envPath.isNullOrBlank() && File(envPath).exists()) {
            return File(envPath).absolutePath
        }

        val nodeCliPath = repoRoot
            .resolve("nodejs/node_modules/@github/copilot/index.js")

        if (nodeCliPath.exists()) {
            return nodeCliPath.absolutePath
        }

        return "copilot"
    }

    private fun getTestEnv(): Map<String, String> {
        return mapOf(
            "COPILOT_API_URL" to proxyUrl,
            "COPILOT_HOME" to workDir,
            "XDG_CONFIG_HOME" to workDir,
            "XDG_STATE_HOME" to workDir,
        )
    }

    // -- Tests --

    /**
     * Test that a session can be created and disconnected.
     *
     * Creates a client, starts it, creates a session, verifies the session ID
     * is non-empty, destroys the session, and stops the client.
     */
    @Test
    fun testSessionCreateDisconnect() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val client = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
            )
        )

        client.start()

        try {
            val session = client.createSession(SessionConfig(model = "gpt-4"))

            // Session ID must be non-empty
            assertNotNull(session.sessionId, "Session ID should not be null")
            assertTrue(session.sessionId.isNotEmpty(), "Session ID should be non-empty after creation")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test sending a message and receiving an assistant response.
     *
     * Creates a session, sends a simple arithmetic prompt via sendAndWait,
     * and verifies the response contains an assistant message.
     */
    @Test
    fun testSendMessage() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val client = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
            )
        )

        client.start()

        try {
            val session = client.createSession(SessionConfig(model = "gpt-4"))
            assertTrue(session.sessionId.isNotEmpty())

            val response = session.sendAndWait(MessageOptions(prompt = "What is 1+1?"))

            // Verify we received an assistant message event
            assertNotNull(response, "Expected a non-null assistant response")
            assertEquals("assistant.message", response?.type, "Response should be an assistant.message event")

            val content = response?.data?.toString() ?: ""
            assertTrue(content.isNotEmpty(), "Assistant response content should not be empty")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test that sessionFs configuration is accepted by the client.
     *
     * Creates a client with SessionFsConfig and verifies it starts
     * without error, confirming the config is passed through correctly.
     */
    @Test
    fun testSessionFsConfig() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val fsConfig = SessionFsConfig(
            initialCwd = "/",
            sessionStatePath = "/session-state",
            conventions = "posix",
        )

        val client = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
                sessionFs = fsConfig,
            )
        )

        client.start()

        try {
            // If sessionFs config was invalid, start() or createSession() would throw
            val session = client.createSession(SessionConfig(model = "gpt-4"))
            assertTrue(session.sessionId.isNotEmpty())

            session.destroy()
        } finally {
            client.stop()
        }
    }
}
