/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/
package com.github.copilot.e2e;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Test harness that manages a replaying CAPI proxy for E2E tests.
 *
 * <p>Spawns the shared test harness server from test/harness/server.ts and
 * provides methods to configure and stop the proxy.
 */
public class TestHarness {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final Pattern LISTENING_PATTERN = Pattern.compile("Listening: (http://[^\\s]+)");

    private Process process;
    private String proxyUrl;

    /** The repo root directory, computed relative to this file's expected location. */
    private static final Path REPO_ROOT;

    static {
        // java/e2e/src/test/java/com/github/copilot/e2e/ -> repo root is 9 levels up,
        // but at runtime we use the working directory approach instead.
        // Compute from the e2e module root (java/e2e/) which is 2 levels below repo root.
        String userDir = System.getProperty("user.dir");
        Path cwd = Paths.get(userDir).toAbsolutePath();
        // If running from java/e2e, go up 2 levels; if from java, go up 1; if from repo root, stay.
        if (cwd.endsWith("e2e")) {
            REPO_ROOT = cwd.getParent().getParent();
        } else if (cwd.endsWith("java")) {
            REPO_ROOT = cwd.getParent();
        } else {
            REPO_ROOT = cwd;
        }
    }

    /**
     * Returns the absolute path to the repo root.
     */
    public static Path getRepoRoot() {
        return REPO_ROOT;
    }

    /**
     * Returns the absolute path to the snapshots directory.
     */
    public static Path getSnapshotsDir() {
        return REPO_ROOT.resolve("test").resolve("snapshots");
    }

    /**
     * Starts the replaying proxy server.
     *
     * @throws Exception if the server fails to start
     */
    public void start() throws Exception {
        if (proxyUrl != null) {
            return;
        }

        Path serverPath = REPO_ROOT.resolve("test").resolve("harness").resolve("server.ts");
        Path harnessDir = serverPath.getParent();

        List<String> command = new ArrayList<>();
        boolean isWindows = System.getProperty("os.name").toLowerCase().contains("win");
        if (isWindows) {
            command.add("cmd");
            command.add("/c");
        }
        command.add("npx");
        command.add("tsx");
        command.add(serverPath.toString());

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.directory(harnessDir.toFile());
        pb.redirectErrorStream(false);

        process = pb.start();

        // Read the first line from stdout to get the listening URL
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
            String line = reader.readLine();
            if (line == null) {
                process.destroyForcibly();
                throw new RuntimeException("Failed to read proxy URL — server produced no output");
            }

            Matcher matcher = LISTENING_PATTERN.matcher(line.trim());
            if (!matcher.find()) {
                process.destroyForcibly();
                throw new RuntimeException("Unexpected proxy output: " + line);
            }

            proxyUrl = matcher.group(1);
        }
    }

    /**
     * Configures the proxy for a specific test snapshot.
     *
     * @param snapshotPath absolute path to the YAML snapshot file
     * @param workDir      working directory for the test
     * @throws Exception if the configuration request fails
     */
    public void configure(String snapshotPath, String workDir) throws Exception {
        if (proxyUrl == null) {
            throw new IllegalStateException("Proxy not started");
        }

        URL url = new URL(proxyUrl + "/config");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);

        String body = MAPPER.writeValueAsString(Map.of(
                "filePath", snapshotPath,
                "workDir", workDir
        ));

        try (OutputStream os = conn.getOutputStream()) {
            os.write(body.getBytes(StandardCharsets.UTF_8));
        }

        int status = conn.getResponseCode();
        if (status != 200) {
            throw new RuntimeException("Proxy config failed with status " + status);
        }
        conn.disconnect();
    }

    /**
     * Stops the proxy server gracefully.
     */
    public void stop() {
        if (process == null) {
            return;
        }

        if (proxyUrl != null) {
            try {
                URL url = new URL(proxyUrl + "/stop?skipWritingCache=true");
                HttpURLConnection conn = (HttpURLConnection) url.openConnection();
                conn.setRequestMethod("POST");
                conn.setRequestProperty("Content-Length", "0");
                conn.setDoOutput(true);
                conn.getResponseCode();
                conn.disconnect();
            } catch (Exception e) {
                // Best effort
            }
        }

        process.destroyForcibly();
        try {
            process.waitFor();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        process = null;
        proxyUrl = null;
    }

    /**
     * Returns the proxy URL, or null if not started.
     */
    public String getProxyUrl() {
        return proxyUrl;
    }
}
