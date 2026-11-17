/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.microsoft.copilot.generated.AssistantMessageEvent;
import com.microsoft.copilot.rpc.MessageOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.SectionOverride;
import com.microsoft.copilot.rpc.SectionOverrideAction;
import com.microsoft.copilot.rpc.SystemMessageConfig;
import com.microsoft.copilot.rpc.SystemMessageSections;

/**
 * Failsafe integration test that validates {@link SystemMessageSections}
 * constants work correctly with the Copilot CLI via the replay proxy.
 *
 * @see Snapshot:
 *      system_message_transform/should_invoke_transform_callbacks_with_section_content
 */
class SystemMessageSectionsIT {

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

    /**
     * Verifies that transform callbacks on {@link SystemMessageSections#IDENTITY}
     * and {@link SystemMessageSections#TONE} are invoked by the runtime with
     * non-empty section content via the replay proxy.
     *
     * @see Snapshot:
     *      system_message_transform/should_invoke_transform_callbacks_with_section_content
     */
    @Test
    void transformOnIdentitySectionReceivesNonEmptyContent() throws Exception {
        ctx.configureForTest("system_message_transform", "should_invoke_transform_callbacks_with_section_content");

        ConcurrentHashMap<String, String> capturedContent = new ConcurrentHashMap<>();

        var systemMessage = new SystemMessageConfig().setMode(SystemMessageMode.CUSTOMIZE)
                .setSections(Map.of(SystemMessageSections.IDENTITY, new SectionOverride().setTransform(content -> {
                    capturedContent.put("identity", content);
                    return CompletableFuture.completedFuture(content);
                }), SystemMessageSections.TONE, new SectionOverride().setTransform(content -> {
                    capturedContent.put("tone", content);
                    return CompletableFuture.completedFuture(content);
                })));

        try (CopilotClient client = ctx.createClient()) {
            // Create the file the snapshot expects the CLI view tool to read
            Path testFile = ctx.getWorkDir().resolve("test.txt");
            Files.writeString(testFile, "Hello transform!");

            CopilotSession session = client
                    .createSession(ctx.newSessionConfig().setSystemMessage(systemMessage).setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session
                        .sendAndWait(new MessageOptions()
                                .setPrompt("Read the contents of test.txt and tell me what it says"), 60_000)
                        .get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");

                String identityContent = capturedContent.get("identity");
                assertNotNull(identityContent, "Expected identity transform callback to be invoked by the runtime");
                assertTrue(!identityContent.isBlank(),
                        "Expected identity section content to be non-empty but was blank");

                String toneContent = capturedContent.get("tone");
                assertNotNull(toneContent, "Expected tone transform callback to be invoked by the runtime");
                assertTrue(!toneContent.isBlank(), "Expected tone section content to be non-empty but was blank");
            } finally {
                session.close();
            }
        }
    }

    /**
     * Verifies that replacing the {@link SystemMessageSections#IDENTITY} section
     * via {@link SectionOverrideAction#REPLACE} causes the assistant to adopt the
     * custom identity in its response.
     *
     * @see Snapshot:
     *      system_message_sections/should_use_replaced_identity_section_in_response
     */
    @Test
    void shouldUseReplacedIdentitySectionInResponse() throws Exception {
        ctx.configureForTest("system_message_sections", "should_use_replaced_identity_section_in_response");

        var systemMessage = new SystemMessageConfig().setMode(SystemMessageMode.CUSTOMIZE)
                .setSections(Map.of(SystemMessageSections.IDENTITY,
                        new SectionOverride().setAction(SectionOverrideAction.REPLACE)
                                .setContent("You are a helpful gardening assistant called Botanica. "
                                        + "You only answer questions about plants and gardening.")));

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client
                    .createSession(ctx.newSessionConfig().setSystemMessage(systemMessage).setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session
                        .sendAndWait(new MessageOptions().setPrompt("Who are you?"), 60_000).get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("botanica") || content.contains("garden") || content.contains("plant"),
                        "Expected response to reflect the replaced identity section, but got: "
                                + response.getData().content());
            } finally {
                session.close();
            }
        }
    }

    /**
     * Verifies that replacing the {@link SystemMessageSections#PREAMBLE} section
     * via {@link SectionOverrideAction#REPLACE} causes the assistant to adopt the
     * custom identity in its response without affecting sibling sections.
     *
     * @see Snapshot:
     *      system_message_sections/should_use_replaced_preamble_section_in_response
     */
    @Test
    void shouldUseReplacedPreambleSectionInResponse() throws Exception {
        ctx.configureForTest("system_message_sections", "should_use_replaced_preamble_section_in_response");

        var systemMessage = new SystemMessageConfig().setMode(SystemMessageMode.CUSTOMIZE)
                .setSections(Map.of(SystemMessageSections.PREAMBLE,
                        new SectionOverride().setAction(SectionOverrideAction.REPLACE)
                                .setContent("You are a helpful gardening assistant called Botanica. "
                                        + "You only answer questions about plants and gardening.")));

        try (CopilotClient client = ctx.createClient()) {
            CopilotSession session = client
                    .createSession(ctx.newSessionConfig().setSystemMessage(systemMessage).setPermissions(
                            ctx.newSessionConfig().getPermissions().setOnRequest(PermissionHandler.APPROVE_ALL)))
                    .get(30, TimeUnit.SECONDS);

            try {
                AssistantMessageEvent response = session
                        .sendAndWait(new MessageOptions().setPrompt("Who are you?"), 60_000).get(90, TimeUnit.SECONDS);

                assertNotNull(response, "Expected a response from the assistant");
                String content = response.getData().content().toLowerCase();
                assertTrue(content.contains("botanica") || content.contains("garden") || content.contains("plant"),
                        "Expected response to reflect the replaced preamble section, but got: "
                                + response.getData().content());
            } finally {
                session.close();
            }
        }
    }
}
