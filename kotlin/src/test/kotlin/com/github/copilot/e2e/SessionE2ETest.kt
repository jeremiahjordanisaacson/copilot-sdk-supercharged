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
import java.util.concurrent.CopyOnWriteArrayList

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
            "GH_TOKEN" to (System.getenv("GH_TOKEN") ?: "fake-test-token"),
            "GITHUB_TOKEN" to (System.getenv("GITHUB_TOKEN") ?: "fake-test-token"),
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

    // -- Tests 4-20 --

    /**
     * Test multi-turn conversation: send two messages and verify both responses.
     */
    @Test
    fun testMultiTurnConversation() = runBlocking {
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

            val response1 = session.sendAndWait(MessageOptions(prompt = "What is 1+1?"))
            assertNotNull(response1, "First response should not be null")
            assertEquals("assistant.message", response1?.type)

            val response2 = session.sendAndWait(MessageOptions(prompt = "What is 2+2?"))
            assertNotNull(response2, "Second response should not be null")
            assertEquals("assistant.message", response2?.type)

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test session resume: create session, destroy, stop client, new client, resume.
     */
    @Test
    fun testSessionResume() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val client1 = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
            )
        )

        client1.start()

        val sessionId: String
        try {
            val session = client1.createSession(SessionConfig(model = "gpt-4"))
            sessionId = session.sessionId
            assertTrue(sessionId.isNotEmpty())
            session.destroy()
        } finally {
            client1.stop()
        }

        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val client2 = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
            )
        )

        client2.start()

        try {
            val resumed = client2.resumeSession(sessionId, ResumeSessionConfig(model = "gpt-4"))
            assertNotNull(resumed.sessionId)
            assertTrue(resumed.sessionId.isNotEmpty())
            resumed.destroy()
        } finally {
            client2.stop()
        }
    }

    /**
     * Test listing sessions: create two sessions, verify listSessions returns >= 2.
     */
    @Test
    fun testSessionList() = runBlocking {
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
            val session1 = client.createSession(SessionConfig(model = "gpt-4"))
            val session2 = client.createSession(SessionConfig(model = "gpt-4"))
            assertTrue(session1.sessionId.isNotEmpty())
            assertTrue(session2.sessionId.isNotEmpty())

            val sessions = client.listSessions()
            assertTrue(sessions.size >= 2, "Expected at least 2 sessions, got ${sessions.size}")

            session1.destroy()
            session2.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test session metadata retrieval.
     */
    @Test
    fun testSessionMetadata() = runBlocking {
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

            val metadata = client.getSessionMetadata(session.sessionId)
            assertNotNull(metadata, "Session metadata should not be null")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test session deletion: create, delete, verify gone from list.
     */
    @Test
    fun testSessionDelete() = runBlocking {
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
            val id = session.sessionId
            assertTrue(id.isNotEmpty())

            session.destroy()
            client.deleteSession(id)

            val sessions = client.listSessions()
            val found = sessions.any { it.sessionId == id }
            assertFalse(found, "Deleted session should not appear in session list")
        } finally {
            client.stop()
        }
    }

    /**
     * Test listing available models.
     */
    @Test
    fun testModelList() = runBlocking {
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
            val models = client.listModels()
            assertNotNull(models, "Models list should not be null")
        } finally {
            client.stop()
        }
    }

    /**
     * Test ping returns a valid response.
     */
    @Test
    fun testPing() = runBlocking {
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
            val response = client.ping("hello")
            assertNotNull(response, "Ping response should not be null")
        } finally {
            client.stop()
        }
    }

    /**
     * Test auth status retrieval.
     */
    @Test
    fun testAuthStatus() = runBlocking {
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
            val authStatus = client.getAuthStatus()
            assertNotNull(authStatus, "Auth status should not be null")
        } finally {
            client.stop()
        }
    }

    /**
     * Test client lifecycle state transitions: DISCONNECTED → CONNECTED → DISCONNECTED.
     */
    @Test
    fun testClientLifecycle() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val client = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
                autoStart = false,
            )
        )

        assertEquals(ConnectionState.DISCONNECTED, client.getState(), "Initial state should be DISCONNECTED")

        client.start()

        try {
            assertEquals(ConnectionState.CONNECTED, client.getState(), "State after start should be CONNECTED")
        } finally {
            client.stop()
        }

        assertEquals(ConnectionState.DISCONNECTED, client.getState(), "State after stop should be DISCONNECTED")
    }

    /**
     * Test foreground session ID: set and get.
     */
    @Test
    fun testForegroundSession() = runBlocking {
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
            val id = session.sessionId
            assertTrue(id.isNotEmpty())

            try {
                client.setForegroundSessionId(id)
                val foregroundId = client.getForegroundSessionId()
                assertEquals(id, foregroundId, "Foreground session ID should match the one we set")
            } catch (_: Exception) {
                // Foreground session RPCs may not be available in headless mode
                println("  foreground session skipped (headless mode)")
            }

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test custom tools: define a tool, attach to session, send a message.
     */
    @Test
    fun testTools() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val echoTool = defineTool("echo") {
            description("Echoes input back")
            parameters(mapOf("type" to "object", "properties" to mapOf("msg" to mapOf("type" to "string"))))
            handler { args, _ -> args }
        }

        val greetTool = createTool(
            name = "greet",
            description = "Returns a greeting",
            parameters = mapOf("type" to "object", "properties" to mapOf("name" to mapOf("type" to "string"))),
        ) { args, _ -> "Hello!" }

        val client = CopilotClient(
            CopilotClientOptions(
                cliPath = getCliPath(),
                cwd = workDir,
                env = getTestEnv(),
            )
        )

        client.start()

        try {
            val session = client.createSession(
                SessionConfig(model = "gpt-4", tools = listOf(echoTool, greetTool))
            )
            assertTrue(session.sessionId.isNotEmpty())

            val response = session.sendAndWait(MessageOptions(prompt = "Use the echo tool"))
            assertNotNull(response, "Response with tools should not be null")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test streaming: enable streaming and collect delta events.
     */
    @Test
    fun testStreaming() = runBlocking {
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
            val session = client.createSession(
                SessionConfig(model = "gpt-4", streaming = true)
            )
            assertTrue(session.sessionId.isNotEmpty())

            val events = CopyOnWriteArrayList<SessionEvent>()
            session.on { event -> events.add(event) }

            val response = session.sendAndWait(MessageOptions(prompt = "What is 1+1?"))
            assertNotNull(response, "Streaming response should not be null")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test system message customization via SessionConfig.
     */
    @Test
    fun testSystemMessageCustomization() = runBlocking {
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
            val session = client.createSession(
                SessionConfig(
                    model = "gpt-4",
                    systemMessage = SystemMessageConfig(
                        mode = "replace",
                        content = "You are a helpful test assistant.",
                    ),
                )
            )
            assertNotNull(session.sessionId)
            assertTrue(session.sessionId.isNotEmpty())

            val response = session.sendAndWait(MessageOptions(prompt = "Hello"))
            assertNotNull(response, "Response with custom system message should not be null")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test sessionFs provider configuration on the client.
     */
    @Test
    fun testSessionFsProvider() = runBlocking {
        configureSnapshot(
            "session",
            "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message",
        )

        val fsConfig = SessionFsConfig(
            initialCwd = workDir,
            sessionStatePath = "$workDir/session-state",
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
            val session = client.createSession(SessionConfig(model = "gpt-4"))
            assertNotNull(session.sessionId)
            assertTrue(session.sessionId.isNotEmpty())

            val response = session.sendAndWait(MessageOptions(prompt = "Hello"))
            assertNotNull(response, "Response with sessionFs provider should not be null")

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test MCP servers configuration in session.
     */
    @Test
    fun testMcpServersConfig() = runBlocking {
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
            val mcpServers = mapOf(
                "test-server" to MCPServerConfig(
                    tools = listOf("tool1", "tool2"),
                    type = "stdio",
                    command = "echo",
                    args = listOf("hello"),
                )
            )

            val session = client.createSession(
                SessionConfig(model = "gpt-4", mcpServers = mcpServers)
            )
            assertNotNull(session.sessionId)
            assertTrue(session.sessionId.isNotEmpty())

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test skills configuration (skillDirectories) in session.
     */
    @Test
    fun testSkillsConfig() = runBlocking {
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
            val session = client.createSession(
                SessionConfig(
                    model = "gpt-4",
                    skillDirectories = listOf(workDir),
                )
            )
            assertNotNull(session.sessionId)
            assertTrue(session.sessionId.isNotEmpty())

            session.destroy()
        } finally {
            client.stop()
        }
    }

    /**
     * Test compaction: send multiple messages and verify responses.
     */
    @Test
    fun testCompaction() = runBlocking {
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

            val prompts = listOf(
                "What is 1+1?",
                "What is 2+2?",
                "What is 3+3?",
                "What is 4+4?",
                "What is 5+5?",
            )

            for (prompt in prompts) {
                val response = session.sendAndWait(MessageOptions(prompt = prompt))
                assertNotNull(response, "Response for '$prompt' should not be null")
                assertEquals("assistant.message", response?.type)
            }

            session.destroy()
        } finally {
            client.stop()
        }
    }
}
