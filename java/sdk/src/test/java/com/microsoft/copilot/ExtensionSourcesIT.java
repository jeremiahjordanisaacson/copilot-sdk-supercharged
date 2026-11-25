/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.ExtensionLaunchProfile;
import com.microsoft.copilot.generated.rpc.ExtensionLaunchProviderResolveResult;
import com.microsoft.copilot.generated.rpc.ExtensionSources;
import com.microsoft.copilot.generated.rpc.ExtensionStatus;
import com.microsoft.copilot.generated.rpc.SessionCanvasActionInvokeParams;
import com.microsoft.copilot.generated.rpc.SessionCanvasCloseParams;
import com.microsoft.copilot.generated.rpc.SessionCanvasOpenParams;
import com.microsoft.copilot.rpc.ClientExtensionOptions;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.CreateSessionOptions;
import com.microsoft.copilot.rpc.PermissionHandler;
import com.microsoft.copilot.rpc.ResumeSessionOptions;
import com.microsoft.copilot.rpc.SessionExtensionOptions;
import com.microsoft.copilot.rpc.SessionPermissionOptions;
import com.microsoft.copilot.rpc.SessionToolOptions;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

@AllowCopilotExperimental
class ExtensionSourcesIT {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @ParameterizedTest
    @EnumSource(CopilotClientMode.class)
    void createEnablesSelectedSourceWithDiscoveryOff(CopilotClientMode mode) throws Exception {
        exerciseSelection(mode, false);
    }

    @ParameterizedTest
    @EnumSource(CopilotClientMode.class)
    void resumeEnablesSelectedSourceWithDiscoveryOff(CopilotClientMode mode) throws Exception {
        exerciseSelection(mode, true);
    }

    private static void exerciseSelection(CopilotClientMode mode, boolean resume) throws Exception {
        try (var ctx = E2ETestContext.create()) {
            ctx.initializeProxy();
            initRepository(ctx);
            var assets = extensionAssets(Path.of(ctx.getCliPath()));
            var sdk = assets.resolve("copilot-sdk").toString();
            var node = TestUtil.findExecutableInPath("node");
            assertNotNull(node, "The extension launch fixture requires Node.js");
            var projectBoots = writeProbe(ctx, ctx.getWorkDir().resolve(".github/extensions/java-project"), "project");
            var userBoots = writeProbe(ctx, ctx.getHomeDir().resolve("extensions/java-user"), "user");
            var options = new CopilotClientOptions().setMode(mode).setExtensions(new ClientExtensionOptions()
                    .setLaunchProvider(request -> CompletableFuture.completedFuture(ExtensionLaunchProviderResolveResult
                            .builder()
                            .launch(ExtensionLaunchProfile.builder().executable(node)
                                    .args(List.of(assets.resolve("preloads/extension_bootstrap.mjs").toString()))
                                    .env(Map.of("COPILOT_CLI_DIST_DIR", assets.toString(), "EXTENSION_PATH",
                                            request.modulePath()))
                                    .build())
                            .build())));

            var disabled = config(ctx, sdk).setName("Extension source seed")
                    .setExtensionSources(ExtensionSources.builder().user(true).project(true).plugins(false).build());
            disabled.getExtensions().setEnabled(false);
            try (var client = ctx.createClient(options);
                    var seed = client.createSession(disabled).get(60, TimeUnit.SECONDS)) {
                // Reconciliation first joins startup, then reports the disabled controller.
                var inactive = assertInstanceOf(JsonRpcException.class, assertThrows(ExecutionException.class,
                        () -> seed.getRpc().extensions.reconcile().get(60, TimeUnit.SECONDS)).getCause());
                assertNotNull(inactive.getData());
                assertEquals("extension_reconciliation_unavailable", inactive.getData().path("code").asText());
                assertTrue(seed.getRpc().extensions.list().get(30, TimeUnit.SECONDS).extensions().isEmpty());
                assertTrue(seed.getRpc().canvas.list().get(30, TimeUnit.SECONDS).canvases().isEmpty());
                assertFalse(Files.exists(projectBoots));
                assertFalse(Files.exists(userBoots));

                var seedId = seed.getSessionId();
                var sources = ExtensionSources.builder().user(false).project(true).plugins(false).build();
                if (resume) {
                    seed.close();
                }
                var selected = resume
                        ? client.resumeSession(seedId,
                                new ResumeSessionOptions().setWorkingDirectory(ctx.getWorkDir().toString())
                                        .setConfigDirectory(ctx.getHomeDir().toString()).setEnableConfigDiscovery(false)
                                        .setExtensions(new SessionExtensionOptions().setEnabled(true).setSdkPath(sdk))
                                        .setExtensionSources(sources)
                                        .setTools(new SessionToolOptions().setAvailableTools(List.of()))
                                        .setPermissions(new SessionPermissionOptions()
                                                .setOnRequest(PermissionHandler.APPROVE_ALL)))
                                .get(60, TimeUnit.SECONDS)
                        : client.createSession(config(ctx, sdk).setExtensionSources(sources)).get(60, TimeUnit.SECONDS);
                try (var session = selected) {
                    if (resume) {
                        assertEquals(seedId, session.getSessionId());
                    }
                    session.getRpc().extensions.reconcile().get(60, TimeUnit.SECONDS);
                    var extensions = session.getRpc().extensions.list().get(30, TimeUnit.SECONDS).extensions();
                    assertEquals(List.of("project:java-project"), extensions.stream().map(item -> item.id()).toList());
                    var extension = extensions.get(0);
                    assertEquals(ExtensionStatus.RUNNING, extension.status());
                    assertNotNull(extension.pid());
                    var boot = MAPPER.readTree(Files.readString(projectBoots));
                    assertEquals(session.getSessionId(), boot.get("sessionId").asText());
                    assertEquals(extension.pid().longValue(), boot.get("pid").asLong());
                    assertFalse(Files.exists(userBoots));
                    var canvases = session.getRpc().canvas.list().get(30, TimeUnit.SECONDS).canvases();
                    assertEquals(List.of("canvas-project"), canvases.stream().map(item -> item.canvasId()).toList());
                    var instance = "java-source-" + session.getSessionId();
                    var opened = session.getRpc().canvas.open(SessionCanvasOpenParams.builder()
                            .canvasId("canvas-project").instanceId(instance).input(Map.of()).build())
                            .get(30, TimeUnit.SECONDS);
                    assertEquals("Project source opened", opened.title());
                    assertEquals(extension.id(), opened.extensionId());
                    var result = session.getRpc().canvas.action.invoke(SessionCanvasActionInvokeParams.builder()
                            .instanceId(instance).actionName("triple").input(Map.of("value", 17)).build())
                            .get(30, TimeUnit.SECONDS);
                    assertEquals(Map.of("tripled", 51), result.result());
                    session.getRpc().canvas.close(SessionCanvasCloseParams.builder().instanceId(instance).build())
                            .get(30, TimeUnit.SECONDS);
                }
            }
        }
    }

    private static CreateSessionOptions config(E2ETestContext ctx, String sdk) {
        return ctx.newSessionConfig().setConfigDirectory(ctx.getHomeDir().toString()).setEnableConfigDiscovery(false)
                .setExtensions(new SessionExtensionOptions().setEnabled(true).setSdkPath(sdk))
                .setTools(new SessionToolOptions().setAvailableTools(List.of()))
                .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL));
    }

    private static Path writeProbe(E2ETestContext ctx, Path directory, String name) throws Exception {
        Files.createDirectories(directory);
        var boots = ctx.getWorkDir().resolve(name + ".boots");
        Files.writeString(directory.resolve("extension.mjs"),
                """
                        import { writeFileSync } from "node:fs";
                        import { joinSession, createCanvas } from "@microsoft/copilot-sdk/extension";
                        writeFileSync(%s, JSON.stringify({ sessionId: process.env.SESSION_ID, pid: process.pid }));
                        await joinSession({ canvases: { definitions: [createCanvas({
                            id: %s, displayName: "Java source probe", description: "Exercises a real extension.",
                            open: () => ({ title: "Project source opened" }),
                            actions: [{
                                name: "triple", description: "Triples the supplied integer.",
                                inputSchema: { type: "object", properties: { value: { type: "integer" } }, required: ["value"] },
                                handler: ({ input }) => ({ tripled: input.value * 3 }),
                            }],
                        })] } });
                        """
                        .formatted(MAPPER.writeValueAsString(boots.toString()),
                                MAPPER.writeValueAsString("canvas-" + name)));
        return boots;
    }

    private static Path extensionAssets(Path cli) {
        for (var directory = cli.toAbsolutePath().getParent(); directory != null; directory = directory.getParent()) {
            if (Files.isRegularFile(directory.resolve("preloads/extension_bootstrap.mjs"))
                    && Files.isRegularFile(directory.resolve("copilot-sdk/extension.js"))) {
                return directory;
            }
        }
        throw new IllegalStateException("Missing extension assets beside the selected runtime: " + cli);
    }

    private static void initRepository(E2ETestContext ctx) throws Exception {
        var builder = new ProcessBuilder("git", "-c", "init.templateDir=", "init", "--quiet")
                .directory(ctx.getWorkDir().toFile()).redirectErrorStream(true);
        builder.environment().keySet().removeAll(List.of("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"));
        builder.environment().put("GIT_CONFIG_NOSYSTEM", "1");
        builder.environment().put("GIT_CONFIG_GLOBAL", ctx.getHomeDir().resolve("empty-gitconfig").toString());
        var process = builder.start();
        try {
            assertTrue(process.waitFor(30, TimeUnit.SECONDS), "git init did not finish");
            assertEquals(0, process.exitValue(), new String(process.getInputStream().readAllBytes()));
        } finally {
            if (process.isAlive()) {
                process.destroyForcibly().waitFor();
            }
        }
    }
}
