/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.e2e;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.microsoft.copilot.CopilotClient;
import com.microsoft.copilot.CopilotSession;
import com.microsoft.copilot.E2ETestContext;
import com.microsoft.copilot.generated.AssistantMessageEvent;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ToolDefinition;
import com.microsoft.copilot.rpc.ToolSet;
import com.microsoft.copilot.tool.Param;

/**
 * Failsafe integration test for the ergonomic {@code @CopilotTool} +
 * {@code ToolDefinition.fromObject()} API.
 *
 * <p>
 * This test proves that the ergonomic annotation-based API produces identical
 * wire behavior to the low-level {@code ToolDefinition.create()} API tested in
 * {@code LowLevelToolDefinitionIT}.
 *
 * @see Snapshot: tools/ergonomic_tool_definition
 */
class ErgonomicToolDefinitionIT {

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
    void ergonomicToolDefinition() throws Exception {
        ctx.configureForTest("tools", "ergonomic_tool_definition");

        ErgonomicTestTools tools = new ErgonomicTestTools();
        List<ToolDefinition> toolDefs = ToolDefinition.fromObject(tools);

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools()
                            .setAvailableTools(new ToolSet().addCustom("*").addBuiltIn("web_fetch"))
                            .setDefinitions(toolDefs)))
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
                assertTrue("analyzing".equals(tools.currentPhase),
                        "Expected currentPhase to be 'analyzing' but was: " + tools.currentPhase);
            } finally {
                session.close();
            }
        }
    }

    @Test
    void ergonomicToolArity0() throws Exception {
        ctx.configureForTest("tools", "ergonomic_tool_arity0");

        ErgonomicTestTools tools = new ErgonomicTestTools();
        List<ToolDefinition> toolDefs = ToolDefinition.fromObject(tools);

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools().setAvailableTools(new ToolSet().addCustom("*"))
                            .setDefinitions(toolDefs)))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session
                        .sendAndWait(new MessageOptions().setPrompt("Call get_status and tell me the result."), 60_000)
                        .get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("ok"),
                        "Response should mention the status: " + response.getData().content());
            } finally {
                session.close();
            }
        }
    }

    @Test
    void ergonomicToolArity2() throws Exception {
        ctx.configureForTest("tools", "ergonomic_tool_arity2");

        ErgonomicTestTools tools = new ErgonomicTestTools();
        List<ToolDefinition> toolDefs = ToolDefinition.fromObject(tools);

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools().setAvailableTools(new ToolSet().addCustom("*"))
                            .setDefinitions(toolDefs)))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session.sendAndWait(
                        new MessageOptions().setPrompt(
                                "Call combine_values with 'alpha' and 'beta', then report the combined result."),
                        60_000).get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("alpha") && content.contains("beta"),
                        "Response should contain the combined values: " + response.getData().content());
            } finally {
                session.close();
            }
        }
    }

    @Test
    void lambdaToolArity0() throws Exception {
        ctx.configureForTest("tools", "ergonomic_tool_arity0");

        ToolDefinition getStatus = ToolDefinition.from("get_status", "Returns the current status", () -> "Status: OK");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools().setAvailableTools(new ToolSet().addCustom("*"))
                            .setDefinitions(List.of(getStatus))))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session
                        .sendAndWait(new MessageOptions().setPrompt("Call get_status and tell me the result."), 60_000)
                        .get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("ok"),
                        "Response should mention the status: " + response.getData().content());
            } finally {
                session.close();
            }
        }
    }

    @Test
    void lambdaToolArity2() throws Exception {
        ctx.configureForTest("tools", "ergonomic_tool_arity2");

        ToolDefinition combineValues = ToolDefinition.from("combine_values", "Combines two values into a single string",
                Param.of(String.class, "value1", "First value"), Param.of(String.class, "value2", "Second value"),
                (v1, v2) -> "combined: " + v1 + " + " + v2);

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools().setAvailableTools(new ToolSet().addCustom("*"))
                            .setDefinitions(List.of(combineValues))))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session.sendAndWait(
                        new MessageOptions().setPrompt(
                                "Call combine_values with 'alpha' and 'beta', then report the combined result."),
                        60_000).get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("alpha") && content.contains("beta"),
                        "Response should contain the combined values: " + response.getData().content());
            } finally {
                session.close();
            }
        }
    }

    @Test
    void lambdaToolDefinition() throws Exception {
        ctx.configureForTest("tools", "ergonomic_tool_definition");

        class LambdaTools {
            String currentPhase;
        }
        LambdaTools tools = new LambdaTools();

        ToolDefinition setCurrentPhase = ToolDefinition.from("set_current_phase", "Sets the current phase of the agent",
                Param.of(String.class, "phase", "The phase to transition to"), phase -> {
                    tools.currentPhase = phase;
                    return "Phase set to " + phase;
                });

        ToolDefinition searchItems = ToolDefinition.from("search_items", "Search for items by keyword",
                Param.of(String.class, "keyword", "Search keyword"),
                keyword -> "Found: " + keyword + " -> item_alpha, item_beta");

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client.createSession(ctx.newSessionConfig()
                    .setPermissions(ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL))
                    .setTools(ctx.newSessionConfig().getTools()
                            .setAvailableTools(new ToolSet().addCustom("*").addBuiltIn("web_fetch"))
                            .setDefinitions(List.of(setCurrentPhase, searchItems))))
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
                assertTrue("analyzing".equals(tools.currentPhase),
                        "Expected currentPhase to be 'analyzing' but was: " + tools.currentPhase);
            } finally {
                session.close();
            }
        }
    }
}
