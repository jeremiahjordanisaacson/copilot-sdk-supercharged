/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e;

import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotClientOptions;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.SystemMessageConfig;
import com.github.copilot.Types;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * E2E tests for session lifecycle, messaging, and configuration.
 */
class SessionE2ETest extends E2ETestBase {

    /**
     * Verifies that a session can be created and cleanly disconnected.
     */
    @Test
    void test_session_create_disconnect() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session, "Session should not be null");
            assertNotNull(session.getSessionId(), "Session ID should not be null");
            assertFalse(session.getSessionId().isBlank(), "Session ID should not be blank");

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a message can be sent and an assistant response received.
     */
    @Test
    void test_send_message() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session.getSessionId());

            Types.SessionEvent response = session.sendAndWait(new MessageOptions("What is 1+1?"));
            assertNotNull(response, "Should receive an assistant response");
            assertEquals("assistant.message", response.type, "Response should be an assistant message");
            assertNotNull(response.data, "Response data should not be null");

            Object content = response.data.get("content");
            assertNotNull(content, "Response should contain content");
            assertTrue(content.toString().contains("2"),
                    "Response should contain the answer '2', got: " + content);

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a client can be created with sessionFs configuration.
     */
    @Test
    void test_session_fs_config() throws Exception {
        Types.SessionFsConfig fsConfig = new Types.SessionFsConfig();
        fsConfig.initialCwd = "/";
        fsConfig.sessionStatePath = "/session-state";
        fsConfig.conventions = "posix";

        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
                        .sessionFs(fsConfig)
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session, "Session should be created with sessionFs config");
            assertNotNull(session.getSessionId());
            assertFalse(session.getSessionId().isBlank());

            session.destroy();
        } finally {
            client.stop();
        }
    }

    // -------------------------------------------------------
    // Tests 4–20: Additional E2E tests for SDK API surface
    // -------------------------------------------------------

    /**
     * Verifies multi-turn conversation: two sequential sendAndWait calls.
     */
    @Test
    void test_multi_turn_conversation() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session.getSessionId());

            Types.SessionEvent response1 = session.sendAndWait(new MessageOptions("What is 2+2?"));
            assertNotNull(response1, "First response should not be null");
            assertEquals("assistant.message", response1.type, "First response should be assistant.message");
            assertNotNull(response1.data, "First response data should not be null");
            assertNotNull(response1.data.get("content"), "First response should contain content");

            Types.SessionEvent response2 = session.sendAndWait(new MessageOptions("What about 3+3?"));
            assertNotNull(response2, "Second response should not be null");
            assertEquals("assistant.message", response2.type, "Second response should be assistant.message");
            assertNotNull(response2.data, "Second response data should not be null");
            assertNotNull(response2.data.get("content"), "Second response should contain content");

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a session can be resumed by session ID.
     */
    @Test
    void test_session_resume() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        String sessionId;
        try {
            CopilotSession session = client.createSession(new SessionConfig());
            sessionId = session.getSessionId();
            assertNotNull(sessionId);
            session.destroy();
        } finally {
            client.stop();
        }

        // Create a new client and resume the session
        CopilotClient client2 = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession resumed = client2.resumeSession(sessionId);
            assertNotNull(resumed, "Resumed session should not be null");
            assertNotNull(resumed.getSessionId(), "Resumed session should have an ID");
        } finally {
            client2.stop();
        }
    }

    /**
     * Verifies that listSessions returns at least 2 entries after creating 2 sessions.
     */
    @Test
    void test_session_list() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session1 = client.createSession(new SessionConfig());
            assertNotNull(session1.getSessionId());

            CopilotSession session2 = client.createSession(new SessionConfig());
            assertNotNull(session2.getSessionId());

            List<?> sessions = client.listSessions();
            assertNotNull(sessions, "Session list should not be null");
            assertTrue(sessions.size() >= 2, "Should have at least 2 sessions, got: " + sessions.size());

            session1.destroy();
            session2.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that getSessionMetadata returns metadata for a created session.
     */
    @Test
    void test_session_metadata() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session.getSessionId());

            Object metadata = client.getSessionMetadata(session.getSessionId());
            assertNotNull(metadata, "Session metadata should not be null");

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that deleteSession removes a session from the list.
     */
    @Test
    void test_session_delete() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            String sessionId = session.getSessionId();
            assertNotNull(sessionId);
            session.destroy();

            client.deleteSession(sessionId);

            List<?> sessions = client.listSessions();
            if (sessions != null) {
                for (Object entry : sessions) {
                    // Verify the deleted session is not in the list
                    assertFalse(entry.toString().contains(sessionId),
                            "Deleted session should not appear in session list");
                }
            }
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that listModels returns a non-null result.
     */
    @Test
    void test_model_list() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            List<?> models = client.listModels();
            assertNotNull(models, "Model list should not be null");
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that ping returns a non-null response.
     */
    @Test
    void test_ping() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            Object result = client.ping("hello");
            assertNotNull(result, "Ping response should not be null");
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that getAuthStatus returns a non-null response.
     */
    @Test
    void test_auth_status() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            Object result = client.getAuthStatus();
            assertNotNull(result, "Auth status should not be null");
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies client lifecycle: state transitions through start/stop.
     */
    @Test
    void test_client_lifecycle() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
                        .autoStart(false)
        );

        try {
            Types.ConnectionState initialState = client.getState();
            assertNotNull(initialState, "Initial state should not be null");

            client.start();
            Types.ConnectionState startedState = client.getState();
            assertNotNull(startedState, "Started state should not be null");
        } finally {
            client.stop();
            Types.ConnectionState stoppedState = client.getState();
            assertNotNull(stoppedState, "Stopped state should not be null");
        }
    }

    /**
     * Verifies setForegroundSessionId/getForegroundSessionId round-trip.
     */
    @Test
    void test_foreground_session() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            String sessionId = session.getSessionId();
            assertNotNull(sessionId);

            client.setForegroundSessionId(sessionId);
            String foregroundId = client.getForegroundSessionId();
            assertEquals(sessionId, foregroundId,
                    "Foreground session ID should match the one we set");

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a session can be created with a custom tool definition.
     */
    @Test
    void test_tools() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            Map<String, Object> parameters = new HashMap<>();
            parameters.put("type", "object");
            Map<String, Object> props = new HashMap<>();
            Map<String, Object> exprProp = new HashMap<>();
            exprProp.put("type", "string");
            exprProp.put("description", "The math expression to evaluate");
            props.put("expression", exprProp);
            parameters.put("properties", props);
            parameters.put("required", List.of("expression"));

            Types.Tool calculator = new Types.Tool(
                    "calculator",
                    "Evaluates a math expression",
                    parameters,
                    (args, invocation) -> "42"
            );

            SessionConfig config = new SessionConfig()
                    .tools(List.of(calculator));

            CopilotSession session = client.createSession(config);
            assertNotNull(session, "Session with tools should be created");
            assertNotNull(session.getSessionId());

            Types.SessionEvent response = session.sendAndWait(
                    new MessageOptions("Use the calculator tool to evaluate 6*7"));
            assertNotNull(response, "Should receive a response");

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies streaming mode: delta events are emitted during message exchange.
     */
    @Test
    void test_streaming() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            SessionConfig config = new SessionConfig().streaming(true);
            CopilotSession session = client.createSession(config);
            assertNotNull(session.getSessionId());

            CopyOnWriteArrayList<Types.SessionEvent> deltaEvents = new CopyOnWriteArrayList<>();
            CountDownLatch latch = new CountDownLatch(1);

            session.on("assistant.message_delta", event -> {
                deltaEvents.add(event);
            });

            session.on("assistant.message", event -> {
                latch.countDown();
            });

            session.send(new MessageOptions("What is 1+1?"));

            // Wait for the final message (or timeout)
            latch.await(30, TimeUnit.SECONDS);

            // In streaming mode we expect at least one delta event
            assertFalse(deltaEvents.isEmpty(),
                    "Should receive at least one assistant.message_delta event in streaming mode");

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a session can be created with a custom system message (append mode).
     */
    @Test
    void test_system_message_customization() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            SessionConfig config = new SessionConfig()
                    .systemMessage(SystemMessageConfig.append("You are a helpful math tutor."));

            CopilotSession session = client.createSession(config);
            assertNotNull(session, "Session with system message should be created");
            assertNotNull(session.getSessionId());
            assertFalse(session.getSessionId().isBlank());

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a client with SessionFsConfig provider starts without error.
     */
    @Test
    void test_session_fs_provider() throws Exception {
        Types.SessionFsConfig fsConfig = new Types.SessionFsConfig();
        fsConfig.initialCwd = workDir.toAbsolutePath().toString();
        fsConfig.sessionStatePath = workDir.resolve("session-state").toAbsolutePath().toString();
        fsConfig.conventions = "posix";

        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
                        .sessionFs(fsConfig)
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session, "Session with FS provider should be created");
            assertNotNull(session.getSessionId());

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a session config with MCP servers is accepted.
     */
    @Test
    void test_mcp_servers_config() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            Types.MCPServerConfig mcpConfig = new Types.MCPServerConfig();
            mcpConfig.type = "stdio";
            mcpConfig.command = "echo";
            mcpConfig.args = List.of("hello");

            Map<String, Types.MCPServerConfig> mcpServers = new HashMap<>();
            mcpServers.put("test-server", mcpConfig);

            SessionConfig config = new SessionConfig()
                    .mcpServers(mcpServers);

            CopilotSession session = client.createSession(config);
            assertNotNull(session, "Session with MCP servers config should be created");
            assertNotNull(session.getSessionId());

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies that a session config with skill directories is accepted.
     */
    @Test
    void test_skills_config() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            SessionConfig config = new SessionConfig()
                    .skillDirectories(List.of(workDir.toAbsolutePath().toString()));

            CopilotSession session = client.createSession(config);
            assertNotNull(session, "Session with skill directories should be created");
            assertNotNull(session.getSessionId());

            session.destroy();
        } finally {
            client.stop();
        }
    }

    /**
     * Verifies compaction: multiple messages are sent and at least one exchange completes.
     */
    @Test
    void test_compaction() throws Exception {
        CopilotClient client = new CopilotClient(
                new CopilotClientOptions()
                        .cliPath(getCliPath())
                        .cwd(workDir.toAbsolutePath().toString())
                        .env(getTestEnv())
        );

        try {
            CopilotSession session = client.createSession(new SessionConfig());
            assertNotNull(session.getSessionId());

            CopyOnWriteArrayList<Types.SessionEvent> compactionEvents = new CopyOnWriteArrayList<>();
            session.on(event -> {
                if ("session.compaction_start".equals(event.type)
                        || "session.compaction_complete".equals(event.type)) {
                    compactionEvents.add(event);
                }
            });

            // Send multiple messages to potentially trigger compaction
            Types.SessionEvent r1 = session.sendAndWait(new MessageOptions("What is 1+1?"));
            assertNotNull(r1, "First message exchange should complete");

            Types.SessionEvent r2 = session.sendAndWait(new MessageOptions("What is 2+2?"));
            assertNotNull(r2, "Second message exchange should complete");

            Types.SessionEvent r3 = session.sendAndWait(new MessageOptions("What is 3+3?"));
            assertNotNull(r3, "Third message exchange should complete");

            // Compaction is not guaranteed to trigger with only 3 messages,
            // but at least all exchanges should have completed successfully.
            // If compaction did fire, verify the events are well-formed.
            for (Types.SessionEvent ev : compactionEvents) {
                assertNotNull(ev.type, "Compaction event type should not be null");
                assertTrue(ev.type.startsWith("session.compaction"),
                        "Compaction event should have correct type prefix");
            }

            session.destroy();
        } finally {
            client.stop();
        }
    }
}
