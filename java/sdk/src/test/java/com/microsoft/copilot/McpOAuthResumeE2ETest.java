/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.SessionMcpOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.microsoft.copilot.generated.AssistantMessageEvent;
import com.microsoft.copilot.rpc.McpAuthResult;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ResumeSessionOptions;

@Tag("e2e")
class McpOAuthResumeE2ETest {

    private static final String SNAPSHOT = "resumes_a_persisted_session_from_a_new_client_when_an_mcp_oauth_handler_is_configured";

    private static E2ETestContext ctx;

    @BeforeAll
    static void setup() throws Exception {
        ctx = E2ETestContext.create();
    }

    @AfterAll
    static void teardown() throws Exception {
        if (ctx != null) {
            ctx.close();
        }
    }

    @Test
    @Tag("isolated-resume")
    void resumesAPersistedSessionFromANewClientWhenAnMcpOauthHandlerIsConfigured() throws Exception {
        ctx.configureForTest("session", SNAPSHOT);

        String sessionId;
        try (var client = ctx.createClient();
                var session = client
                        .createSession(ctx.newSessionConfig()
                                .setPermissions(ctx.newSessionConfig().getPermissions()
                                        .setOnRequest(PermissionHandler.APPROVE_ALL))
                                .setMcp(ctx.newSessionConfig().getMcp()
                                        .setOnAuthRequest((request, invocation) -> CompletableFuture
                                                .completedFuture(McpAuthResult.cancelled()))))
                        .get(30, TimeUnit.SECONDS)) {
            sessionId = session.getSessionId();

            AssistantMessageEvent response = session.sendAndWait(new MessageOptions().setPrompt("What is 1+1?"), 60_000)
                    .get(90, TimeUnit.SECONDS);
            assertNotNull(response);
            assertTrue(response.getData().content().contains("2"),
                    "Response should contain 2: " + response.getData().content());
        }

        try (var client = ctx.createClient();
                var session = client.resumeSession(sessionId, new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                        .setMcp(new SessionMcpOptions().setOnAuthRequest(
                                (request, invocation) -> CompletableFuture.completedFuture(McpAuthResult.cancelled()))))
                        .get(30, TimeUnit.SECONDS)) {
            assertEquals(sessionId, session.getSessionId());
        }
    }
}
