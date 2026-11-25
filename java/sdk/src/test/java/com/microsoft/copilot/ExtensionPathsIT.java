/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.copilot.generated.rpc.ExtensionLaunchProfile;
import com.microsoft.copilot.generated.rpc.ExtensionLaunchProviderResolveResult;
import com.microsoft.copilot.generated.rpc.ExtensionSource;
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

@AllowCopilotExperimental
class ExtensionPathsIT {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @ParameterizedTest
    @EnumSource(CopilotClientMode.class)
    void clonedPathListsKeepCreateAndResumeSelectionIndependent(CopilotClientMode mode) throws Exception {
        try (var ctx = E2ETestContext.create()) {
            ctx.initializeProxy();
            initRepository(ctx);
            var assets = extensionAssets(Path.of(ctx.getCliPath()));
            var sdk = assets.resolve("copilot-sdk").toString();
            var selected = ctx.getWorkDir().resolve("selected");
            var excluded = ctx.getWorkDir().resolve("excluded");
            writeProbe(ctx, selected, "selected");
            var excludedBoots = writeProbe(ctx, excluded, "excluded");
            var entry = selected.resolve("extension.mjs").toString();
            var paths = new ArrayList<>(List.of(entry));
            var create = config(ctx, sdk).setExtensionPaths(paths).clone();
            paths.set(0, excluded.resolve("extension.mjs").toString());
            try (var client = ctx.createClient(extensionClientOptions(mode, assets));
                    var session = client.createSession(create).get(60, TimeUnit.SECONDS)) {
                assertSelectedPath(session, entry);
                assertFalse(Files.exists(excludedBoots));
                var resumePaths = new ArrayList<>(List.of(entry));
                var resume = new ResumeSessionOptions().setWorkingDirectory(ctx.getWorkDir().toString())
                        .setConfigDirectory(ctx.getHomeDir().toString()).setEnableConfigDiscovery(false)
                        .setExtensions(new SessionExtensionOptions().setEnabled(true).setSdkPath(sdk))
                        .setExtensionPaths(resumePaths).setTools(new SessionToolOptions().setAvailableTools(List.of()))
                        .setPermissions(new SessionPermissionOptions().setOnRequest(PermissionHandler.APPROVE_ALL))
                        .clone();
                resumePaths.clear();
                resumePaths.add(excluded.resolve("extension.mjs").toString());
                try (var resumed = client.resumeSession(session.getSessionId(), resume).get(60, TimeUnit.SECONDS)) {
                    assertEquals(session.getSessionId(), resumed.getSessionId());
                    assertSelectedPath(resumed, entry);
                    assertFalse(Files.exists(excludedBoots));
                }
            }
        }
    }

    private static void assertSelectedPath(CopilotSession session, String entry) throws Exception {
        session.getRpc().tools.initializeAndValidate().get(60, TimeUnit.SECONDS);
        var extensions = session.getRpc().extensions.reconcile().get(60, TimeUnit.SECONDS).extensions();
        assertEquals(1, extensions.size());
        var extension = extensions.get(0);
        assertEquals(entry, extension.modulePath());
        assertEquals(ExtensionSource.EXPLICIT, extension.source());
        assertEquals(ExtensionStatus.RUNNING, extension.status());
        var instance = "java-path-" + session.getSessionId();
        var opened = session.getRpc().canvas.open(SessionCanvasOpenParams.builder().canvasId("canvas-selected")
                .instanceId(instance).input(Map.of()).build()).get(30, TimeUnit.SECONDS);
        assertEquals(extension.id(), opened.extensionId());
        var result = session.getRpc().canvas.action.invoke(SessionCanvasActionInvokeParams.builder()
                .instanceId(instance).actionName("triple").input(Map.of("value", 17)).build())
                .get(30, TimeUnit.SECONDS);
        assertEquals(Map.of("tripled", 51), result.result());
        session.getRpc().canvas.close(SessionCanvasCloseParams.builder().instanceId(instance).build()).get(30,
                TimeUnit.SECONDS);
    }

    private static CopilotClientOptions extensionClientOptions(CopilotClientMode mode, Path assets) {
        var node = TestUtil.findExecutableInPath("node");
        assertNotNull(node, "The extension launch fixture requires Node.js");
        return new CopilotClientOptions().setMode(mode)
                .setExtensions(new ClientExtensionOptions().setLaunchProvider(
                        request -> CompletableFuture.completedFuture(ExtensionLaunchProviderResolveResult.builder()
                                .launch(ExtensionLaunchProfile.builder().executable(node)
                                        .args(List.of(assets.resolve("preloads/extension_bootstrap.mjs").toString()))
                                        .env(Map.of("COPILOT_CLI_DIST_DIR", assets.toString(), "EXTENSION_PATH",
                                                request.modulePath()))
                                        .build())
                                .build())));
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
