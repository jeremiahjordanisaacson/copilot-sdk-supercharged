/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInfo;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Base class for E2E tests.
 *
 * <p>Manages the test harness lifecycle and provides helper methods
 * for creating SDK clients configured to use the replaying proxy.
 */
public abstract class E2ETestBase {
    protected static TestHarness harness;
    protected static Path workDir;
    protected static Path homeDir;

    @BeforeAll
    static void startHarness() throws Exception {
        harness = new TestHarness();
        harness.start();

        workDir = Files.createTempDirectory("copilot-e2e-work-");
        homeDir = Files.createTempDirectory("copilot-e2e-home-");
    }

    @AfterAll
    static void stopHarness() {
        if (harness != null) {
            harness.stop();
            harness = null;
        }
        deleteDirQuietly(workDir);
        deleteDirQuietly(homeDir);
    }

    @BeforeEach
    void configureProxy(TestInfo testInfo) throws Exception {
        // Derive snapshot path from test method name
        // e.g. "test_session_create_disconnect" -> snapshot "session/session_create_disconnect.yaml"
        String testName = testInfo.getTestMethod().orElseThrow().getName();
        if (testName.startsWith("test_")) {
            testName = testName.substring(5);
        }

        Path snapshotPath = TestHarness.getSnapshotsDir()
                .resolve("session")
                .resolve(testName + ".yaml");

        harness.configure(snapshotPath.toAbsolutePath().toString(), workDir.toAbsolutePath().toString());

        // Clean work directory between tests
        cleanDir(workDir);
    }

    /**
     * Returns environment variables configured for the proxy.
     */
    protected static Map<String, String> getTestEnv() {
        Map<String, String> env = new HashMap<>(System.getenv());
        env.put("COPILOT_API_URL", harness.getProxyUrl());
        env.put("COPILOT_HOME", homeDir.toAbsolutePath().toString());
        env.put("XDG_CONFIG_HOME", homeDir.toAbsolutePath().toString());
        env.put("XDG_STATE_HOME", homeDir.toAbsolutePath().toString());
        return env;
    }

    /**
     * Returns the CLI path for tests — uses COPILOT_CLI_PATH env var
     * or falls back to the nodejs node_modules installation.
     */
    protected static String getCliPath() {
        String envPath = System.getenv("COPILOT_CLI_PATH");
        if (envPath != null && !envPath.isBlank()) {
            Path p = Path.of(envPath);
            if (Files.exists(p)) {
                return p.toAbsolutePath().toString();
            }
        }

        Path repoRoot = TestHarness.getRepoRoot();
        Path cliPath = repoRoot.resolve("nodejs")
                .resolve("node_modules")
                .resolve("@github")
                .resolve("copilot")
                .resolve("index.js");
        if (Files.exists(cliPath)) {
            return cliPath.toAbsolutePath().toString();
        }

        throw new RuntimeException(
                "CLI not found for tests. Set COPILOT_CLI_PATH or run 'npm install' in the nodejs directory.");
    }

    private static void cleanDir(Path dir) {
        if (dir == null || !Files.exists(dir)) return;
        try (Stream<Path> walk = Files.walk(dir)) {
            walk.filter(p -> !p.equals(dir))
                    .sorted(Comparator.reverseOrder())
                    .forEach(p -> {
                        try { Files.deleteIfExists(p); } catch (IOException ignored) { }
                    });
        } catch (IOException ignored) { }
    }

    private static void deleteDirQuietly(Path dir) {
        if (dir == null || !Files.exists(dir)) return;
        try (Stream<Path> walk = Files.walk(dir)) {
            walk.sorted(Comparator.reverseOrder())
                    .forEach(p -> {
                        try { Files.deleteIfExists(p); } catch (IOException ignored) { }
                    });
        } catch (IOException ignored) { }
    }
}
