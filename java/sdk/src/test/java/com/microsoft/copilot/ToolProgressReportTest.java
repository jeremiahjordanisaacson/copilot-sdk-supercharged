/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.microsoft.copilot.generated.ExternalToolRequestedEvent;
import com.microsoft.copilot.generated.SessionEvent;
import com.microsoft.copilot.rpc.ToolDefinition;
import com.microsoft.copilot.rpc.ToolHandler;

/**
 * Verifies that {@code ToolInvocation.reportProgress} sends
 * {@code session.tools.reportProgress} for the running call, and that a failed
 * send neither fails the handler nor changes the tool result.
 */
class ToolProgressReportTest {

    private static final String SESSION_ID = "tool-progress-session";

    @Test
    void reportProgressSendsTheCallsRequestIdAndExactMessage() throws Exception {
        var rpc = mock(JsonRpcClient.class);
        var progress = new LinkedBlockingQueue<ObjectNode>();
        var replies = new LinkedBlockingQueue<ObjectNode>();
        when(rpc.invoke(eq("session.tools.reportProgress"), any(), any())).thenAnswer(invocation -> {
            progress.add(invocation.getArgument(1));
            return CompletableFuture.failedFuture(new JsonRpcException(-32603, "progress rejected"));
        });
        when(rpc.invoke(eq("session.tools.handlePendingToolCall"), any(), any())).thenAnswer(invocation -> {
            replies.add(invocation.getArgument(1));
            return CompletableFuture.completedFuture(null);
        });
        var session = new CopilotSession(SESSION_ID, rpc);
        String message = "  Step 1/3: fetching \u00e9l\u00e8ves\n";
        ToolHandler handler = invocation -> invocation.reportProgress(message).thenApply(ignored -> "done");
        session.registerTools(
                List.of(ToolDefinition.create("reports", "Reports progress", Map.of("type", "object"), handler)));

        var event = new ExternalToolRequestedEvent();
        event.setData(ExternalToolRequestedEvent.ExternalToolRequestedEventData.builder().requestId("request-progress")
                .sessionId(SESSION_ID).toolCallId("tool-call-progress").toolName("reports").arguments(Map.of())
                .build());
        Method dispatch = CopilotSession.class.getDeclaredMethod("dispatchEvent", SessionEvent.class);
        dispatch.setAccessible(true);
        dispatch.invoke(session, event);

        ObjectNode sent = progress.poll(5, TimeUnit.SECONDS);
        assertNotNull(sent, "No progress was sent");
        assertEquals(SESSION_ID, sent.path("sessionId").asText(null));
        assertEquals("request-progress", sent.path("requestId").asText(null));
        assertEquals(message, sent.path("message").asText(null));

        ObjectNode reply = replies.poll(5, TimeUnit.SECONDS);
        assertNotNull(reply, "The tool call was not answered");
        assertEquals("request-progress", reply.path("requestId").asText(null));
        assertTrue(reply.path("error").isMissingNode() || reply.path("error").isNull(), reply.toString());
        assertEquals("done", reply.path("result").path("textResultForLlm").asText(null));
    }
}
