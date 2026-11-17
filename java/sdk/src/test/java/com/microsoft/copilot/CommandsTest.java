/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import com.microsoft.copilot.rpc.SessionInteractionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.microsoft.copilot.rpc.CommandContext;
import com.microsoft.copilot.rpc.CommandDefinition;
import com.microsoft.copilot.rpc.CommandHandler;
import com.microsoft.copilot.rpc.CommandWireDefinition;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;

/**
 * Unit tests for the Commands feature (CommandDefinition, CommandContext,
 * CreateSessionOptions.commands, ResumeSessionOptions.commands, and the wire
 * representation).
 *
 * <p>
 * Ported from {@code CommandsTests.cs} in the reference implementation dotnet
 * SDK.
 * </p>
 */
class CommandsTest {

    @Test
    void commandDefinitionHasRequiredProperties() {
        CommandHandler handler = context -> CompletableFuture.completedFuture(null);
        var cmd = new CommandDefinition().setName("deploy").setDescription("Deploy the app").setHandler(handler);

        assertEquals("deploy", cmd.getName());
        assertEquals("Deploy the app", cmd.getDescription());
        assertNotNull(cmd.getHandler());
    }

    @Test
    void commandContextHasAllProperties() {
        var ctx = new CommandContext().setSessionId("session-1").setCommand("/deploy production")
                .setCommandName("deploy").setArgs("production");

        assertEquals("session-1", ctx.getSessionId());
        assertEquals("/deploy production", ctx.getCommand());
        assertEquals("deploy", ctx.getCommandName());
        assertEquals("production", ctx.getArgs());
    }

    @Test
    void sessionConfigCommandsAreCloned() {
        CommandHandler handler = ctx -> CompletableFuture.completedFuture(null);
        var config = new CreateSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                .setInteraction(new SessionInteractionOptions()
                        .setCommands(List.of(new CommandDefinition().setName("deploy").setHandler(handler))));

        var clone = config.clone();

        assertNotNull(clone.getInteraction().getCommands());
        assertEquals(1, clone.getInteraction().getCommands().size());
        assertEquals("deploy", clone.getInteraction().getCommands().get(0).getName());

        // Collections should be independent — clone list is a copy
        assertNotSame(config.getInteraction().getCommands(), clone.getInteraction().getCommands());
    }

    @Test
    void resumeConfigCommandsAreCloned() {
        CommandHandler handler = ctx -> CompletableFuture.completedFuture(null);
        var config = new ResumeSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                .setInteraction(new SessionInteractionOptions()
                        .setCommands(List.of(new CommandDefinition().setName("deploy").setHandler(handler))));

        var clone = config.clone();

        assertNotNull(clone.getInteraction().getCommands());
        assertEquals(1, clone.getInteraction().getCommands().size());
        assertEquals("deploy", clone.getInteraction().getCommands().get(0).getName());
    }

    @Test
    void buildCreateRequestIncludesCommandWireDefinitions() {
        CommandHandler handler = ctx -> CompletableFuture.completedFuture(null);
        var config = new CreateSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                .setInteraction(new SessionInteractionOptions().setCommands(
                        List.of(new CommandDefinition().setName("deploy").setDescription("Deploy").setHandler(handler),
                                new CommandDefinition().setName("rollback").setHandler(handler))));

        var request = SessionRequestBuilder.buildCreateRequest(config, "session-1");

        assertNotNull(request.getInteraction().getCommands());
        assertEquals(2, request.getInteraction().getCommands().size());
        assertEquals("deploy", request.getInteraction().getCommands().get(0).getName());
        assertEquals("Deploy", request.getInteraction().getCommands().get(0).getDescription());
        assertEquals("rollback", request.getInteraction().getCommands().get(1).getName());
        assertEquals("", request.getInteraction().getCommands().get(1).getDescription());
    }

    @Test
    void buildResumeRequestIncludesCommandWireDefinitions() {
        CommandHandler handler = ctx -> CompletableFuture.completedFuture(null);
        var config = new ResumeSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                .setInteraction(new SessionInteractionOptions().setCommands(List
                        .of(new CommandDefinition().setName("deploy").setDescription("Deploy").setHandler(handler))));

        var request = SessionRequestBuilder.buildResumeRequest("session-1", config);

        assertNotNull(request.getInteraction().getCommands());
        assertEquals(1, request.getInteraction().getCommands().size());
        assertEquals("deploy", request.getInteraction().getCommands().get(0).getName());
        assertEquals("Deploy", request.getInteraction().getCommands().get(0).getDescription());
    }

    @Test
    void buildCreateRequestWithNoCommandsHasNullCommandsList() {
        var config = new CreateSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));

        var request = SessionRequestBuilder.buildCreateRequest(config, "session-1");

        assertNull(request.getInteraction().getCommands());
    }

    @Test
    void buildCreateAndResumeRequestsSendExplicitlyEmptyCommandsList() throws Exception {
        // An explicitly empty list must reach the runtime: on resume it replaces the
        // session's slash commands with none, which omitting the key cannot express.
        var mapper = JsonRpcClient.getObjectMapper();
        var create = SessionRequestBuilder.buildCreateRequest(new CreateSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                .setInteraction(new SessionInteractionOptions().setCommands(List.of())), "session-1");
        var resume = SessionRequestBuilder.buildResumeRequest("session-1",
                new ResumeSessionOptions()
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                        .setInteraction(new SessionInteractionOptions().setCommands(List.of())));

        JsonNode createJson = mapper.valueToTree(create);
        JsonNode resumeJson = mapper.valueToTree(resume);
        for (JsonNode json : List.of(createJson, resumeJson)) {
            assertTrue(json.path("interaction").has("commands"), "commands must be sent: " + json);
            assertTrue(json.path("interaction").get("commands").isArray());
            assertEquals(0, json.path("interaction").get("commands").size());
        }
    }

    @Test
    void buildResumeRequestWithNoCommandsOmitsCommands() {
        var config = new ResumeSessionOptions()
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));

        var request = SessionRequestBuilder.buildResumeRequest("session-1", config);

        assertNull(request.getInteraction().getCommands());
        JsonNode json = JsonRpcClient.getObjectMapper().valueToTree(request);
        assertFalse(json.path("interaction").has("commands"));
    }

    @Test
    void commandWireDefinitionHasNameAndDescription() {
        var wire = new CommandWireDefinition("deploy", "Deploy the app");

        assertEquals("deploy", wire.getName());
        assertEquals("Deploy the app", wire.getDescription());
    }

    @Test
    void commandWireDefinitionNormalizesNullDescription() {
        var wire = new CommandWireDefinition("rollback", null);

        assertEquals("rollback", wire.getName());
        assertEquals("", wire.getDescription());
    }

    @Test
    void commandWireDefinitionFluentSetters() {
        var wire = new CommandWireDefinition();
        wire.setName("status");
        wire.setDescription("Show deployment status");

        assertEquals("status", wire.getName());
        assertEquals("Show deployment status", wire.getDescription());
    }

    @Test
    void commandWireDefinitionFluentSettersChaining() {
        var wire = new CommandWireDefinition().setName("logs").setDescription("View application logs");

        assertEquals("logs", wire.getName());
        assertEquals("View application logs", wire.getDescription());
    }
}
