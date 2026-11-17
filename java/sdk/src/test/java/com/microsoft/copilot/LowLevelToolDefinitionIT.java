/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.microsoft.copilot.generated.AssistantMessageEvent;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ToolDefinition;
import com.microsoft.copilot.rpc.ToolSet;

/**
 * Failsafe integration test for explicit (non-ergonomic) tool definition APIs.
 *
 * @see Snapshot: tools/low_level_tool_definition
 */
class LowLevelToolDefinitionIT {

    private static E2ETestContext ctx;
    private String currentPhase;

    record PhaseArgs(String phase) {
    }

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
    void lowLevelToolDefinition() throws Exception {
        ctx.configureForTest("tools", "low_level_tool_definition");

        Map<String, Object> setPhaseSchema = Map.of("type", "object", "properties",
                Map.of("phase", Map.of("type", "string", "enum", List.of("searching", "analyzing", "done"))),
                "required", List.of("phase"));

        ToolDefinition setPhaseTool = ToolDefinition.create("set_current_phase", "Sets the current phase of the agent",
                setPhaseSchema, invocation -> {
                    PhaseArgs args = invocation.getArgumentsAs(PhaseArgs.class);
                    currentPhase = args.phase();
                    return CompletableFuture.completedFuture("Phase set to " + currentPhase);
                });

        Map<String, Object> searchSchema = Map.of("type", "object", "properties",
                Map.of("keyword", Map.of("type", "string")), "required", List.of("keyword"));

        ToolDefinition searchTool = ToolDefinition.create("search_items", "Search for items by keyword", searchSchema,
                invocation -> {
                    Map<String, Object> args = invocation.getArguments();
                    String keyword = (String) args.get("keyword");
                    assertTrue("copilot".equals(keyword), "Expected tool keyword to be 'copilot' but was: " + keyword);
                    return CompletableFuture.completedFuture("Found: item_alpha, item_beta");
                });

        Map<String, Object> grepSchema = Map.of("type", "object", "properties",
                Map.of("query", Map.of("type", "string")), "required", List.of("query"));

        ToolDefinition grepOverrideTool = ToolDefinition.createOverride("grep", "Custom grep override", grepSchema,
                invocation -> {
                    Map<String, Object> args = invocation.getArguments();
                    String query = (String) args.get("query");
                    return CompletableFuture.completedFuture("CUSTOM_GREP: " + query);
                });

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools()
                            .setAvailableTools(new ToolSet().addCustom("*").addBuiltIn("web_fetch"))
                            .setDefinitions(List.of(setPhaseTool, searchTool, grepOverrideTool))))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session.sendAndWait(new MessageOptions().setPrompt(
                        "First, set the current phase to 'analyzing'. Then search for items with keyword 'copilot'. Report the phase and search results."),
                        60_000).get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("analyzing"),
                        "Response should contain the updated phase: " + response.getData().content());
                assertTrue(content.contains("item_alpha") || content.contains("item_beta"),
                        "Response should contain search results: " + response.getData().content());
                assertTrue("analyzing".equals(currentPhase),
                        "Expected currentPhase to be analyzing but was: " + currentPhase);
            } finally {
                session.close();
            }
        }
    }
}
