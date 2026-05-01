/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e;

import com.github.copilot.CopilotClient;
import com.github.copilot.CopilotClientOptions;
import com.github.copilot.CopilotSession;
import com.github.copilot.MessageOptions;
import com.github.copilot.SessionConfig;
import com.github.copilot.Types;

import org.junit.jupiter.api.Test;

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
}
