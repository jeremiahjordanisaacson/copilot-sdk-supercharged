/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Method;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.generated.McpOauthRequestReason;
import com.microsoft.copilot.generated.McpOauthRequiredEvent;
import com.microsoft.copilot.generated.SessionEvent;
import com.microsoft.copilot.rpc.McpAuthHandler;
import com.microsoft.copilot.rpc.McpAuthResult;
import com.microsoft.copilot.rpc.McpAuthToken;

/**
 * Verifies the reply sent back to the runtime for an {@code mcp.oauth_required}
 * request when the session's MCP auth handler fails: the runtime waits for that
 * reply without a deadline, so every non-fatal failure is answered
 * {@code cancelled}.
 */
class McpAuthHandlerFailureReplyTest {

    private static final String SESSION_ID = "mcp-auth-failure-session";

    private final BlockingQueue<ObjectNode> replies = new LinkedBlockingQueue<>();
    private CopilotSession session;

    @BeforeEach
    void setup() {
        var rpc = mock(JsonRpcClient.class);
        when(rpc.invoke(eq("session.mcp.oauth.handlePendingRequest"), any(), any())).thenAnswer(invocation -> {
            replies.add(invocation.getArgument(1));
            return CompletableFuture.completedFuture(null);
        });
        session = new CopilotSession(SESSION_ID, rpc);
    }

    @ParameterizedTest
    @ValueSource(strings = {"throws-error", "throws-exception", "fails-with-error"})
    void failedMcpAuthHandlerIsAnsweredCancelled(String row) throws Exception {
        McpAuthHandler handler = switch (row) {
            case "throws-error" -> (request, invocation) -> {
                throw new AssertionError("auth assertion");
            };
            case "throws-exception" -> (request, invocation) -> {
                throw new IllegalStateException("auth failed");
            };
            default -> (request, invocation) -> CompletableFuture.failedFuture(new AssertionError("auth assertion"));
        };
        session.registerMcpAuthHandler(handler);

        dispatch(row);

        ObjectNode reply = replies.poll(5, TimeUnit.SECONDS);
        assertNotNull(reply, "No reply for " + row);
        assertEquals(SESSION_ID, reply.path("sessionId").asText());
        assertEquals("request-" + row, reply.path("requestId").asText());
        assertEquals("cancelled", reply.path("result").path("kind").asText(), reply.toString());
    }

    @Test
    void mcpAuthHandlerThatReturnsATokenIsAnsweredWithIt() throws Exception {
        session.registerMcpAuthHandler((request, invocation) -> CompletableFuture
                .completedFuture(McpAuthResult.token(new McpAuthToken("access-token", "Bearer", 3600L))));

        dispatch("token");

        ObjectNode reply = replies.poll(5, TimeUnit.SECONDS);
        assertNotNull(reply, "No reply for token");
        assertEquals("request-token", reply.path("requestId").asText());
        assertEquals("token", reply.path("result").path("kind").asText(), reply.toString());
        assertEquals("access-token", reply.path("result").path("accessToken").asText());
    }

    @Test
    void mcpAuthHandlerFatalErrorIsRethrownWithoutAReply() throws Exception {
        var called = new CountDownLatch(1);
        session.registerMcpAuthHandler((request, invocation) -> {
            called.countDown();
            throw new NoClassDefFoundError("class failed to load");
        });

        dispatch("fatal");

        assertTrue(called.await(5, TimeUnit.SECONDS), "handler was not called");
        assertNull(replies.poll(500, TimeUnit.MILLISECONDS), "a fatal error must not be answered");
    }

    private void dispatch(String row) throws Exception {
        var event = new McpOauthRequiredEvent();
        event.setData(McpOauthRequiredEvent.McpOauthRequiredEventData.builder().requestId("request-" + row)
                .serverName("authsrv").serverUrl("https://example.com/mcp").reason(McpOauthRequestReason.INITIAL)
                .build());
        Method dispatchMethod = CopilotSession.class.getDeclaredMethod("dispatchEvent", SessionEvent.class);
        dispatchMethod.setAccessible(true);
        dispatchMethod.invoke(session, event);
    }
}
