/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.microsoft.copilot.generated.ToolExecutionProgressEvent;
import com.microsoft.copilot.generated.rpc.SessionToolsExecuteParams;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ToolDefinition;

@Tag("e2e")
public class ExternalToolProgressE2ETest {

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
    void joiningReportProgressOnASingleThreadExecutorDoesNotDeadlock() throws Exception {
        ctx.configureForTest("external_tool_progress",
                "joining_report_progress_on_a_single_thread_executor_does_not_deadlock");

        var handlerReturned = new CompletableFuture<String>();
        ToolDefinition tool = ToolDefinition.create("progress_probe", "Reports progress",
                Map.of("type", "object", "properties", Map.of()), invocation -> {
                    invocation.reportProgress("working").join();
                    handlerReturned.complete(invocation.getToolCallId());
                    return CompletableFuture.completedFuture("PROGRESS_PROBE_DONE");
                });
        var progress = new CompletableFuture<ToolExecutionProgressEvent>();
        ExecutorService singleThread = Executors.newSingleThreadExecutor();
        var options = new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setExecutor(singleThread);
        options.setGitHub(options.getGitHub().setToken("fake-token-for-e2e-tests"));

        try (CopilotClient client = ctx.createClient(options)) {
            CopilotSession session = client
                    .createSession(ctx.newSessionConfig()
                            .setPermissions(
                                    ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                            .setTools(ctx.newSessionConfig().getTools().setDefinitions(List.of(tool))))
                    .get(60, TimeUnit.SECONDS);
            try (var subscription = session.on(ToolExecutionProgressEvent.class, progress::complete)) {
                session.getRpc().tools.initializeAndValidate().get(30, TimeUnit.SECONDS);
                session.getRpc().tools.execute(SessionToolsExecuteParams.builder().name("progress_probe")
                        .arguments(Map.of()).toolCallId("progress-call").build()).get(30, TimeUnit.SECONDS);

                assertEquals("progress-call", handlerReturned.get(30, TimeUnit.SECONDS));
                ToolExecutionProgressEvent reported = progress.get(30, TimeUnit.SECONDS);
                assertNotNull(reported.getData());
                assertEquals("progress-call", reported.getData().toolCallId());
                assertEquals("working", reported.getData().progressMessage());
            } finally {
                session.close();
            }
        } finally {
            singleThread.shutdownNow();
        }
    }
}
