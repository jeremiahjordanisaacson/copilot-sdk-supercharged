/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.microsoft.copilot.ffi.NativeRuntimeLoader;
import com.microsoft.copilot.rpc.CopilotClientMode;
import com.microsoft.copilot.rpc.CopilotClientOptions;
import com.microsoft.copilot.rpc.RuntimeConnection;
import com.microsoft.copilot.rpc.SpawnedStdioRuntimeConnection;
import com.microsoft.copilot.rpc.SpawnedTcpRuntimeConnection;

/**
 * Manages the lifecycle of the Copilot CLI server process.
 * <p>
 * This class handles spawning the CLI server process, building command lines,
 * detecting the listening port, and establishing connections.
 */
final class CliServerManager {

    private static final Logger LOG = Logger.getLogger(CliServerManager.class.getName());
    private static final int STDERR_READER_JOIN_TIMEOUT_MS = 5000;
    static final int FORCE_KILL_TIMEOUT_SECONDS = 10;

    private final CopilotClientOptions options;
    private final RuntimeConnection runtimeConnection;
    private final SpawnOptions spawn;
    private final StringBuilder stderrBuffer = new StringBuilder();
    private volatile Thread stderrThread;
    private String connectionToken;

    CliServerManager(CopilotClientOptions options, RuntimeConnection connection) {
        this.options = options;
        this.runtimeConnection = connection;
        if (connection instanceof SpawnedStdioRuntimeConnection stdio) {
            this.spawn = new SpawnOptions(stdio.getPath(), stdio.getArgs(), stdio.getEnvironment(),
                    stdio.getWorkingDirectory(), true, 0);
        } else if (connection instanceof SpawnedTcpRuntimeConnection tcp) {
            this.spawn = new SpawnOptions(tcp.getPath(), tcp.getArgs(), tcp.getEnvironment(), tcp.getWorkingDirectory(),
                    false, tcp.getPort());
        } else {
            this.spawn = null;
        }
    }

    /**
     * Sets the connection token to pass to the CLI process via environment
     * variable.
     *
     * @param connectionToken
     *            the token, or {@code null} if not applicable
     */
    void setConnectionToken(String connectionToken) {
        this.connectionToken = connectionToken;
    }

    /**
     * Starts the CLI server process.
     *
     * @return information about the started process including detected port
     * @throws IOException
     *             if the process cannot be started
     * @throws InterruptedException
     *             if interrupted while waiting for port detection
     */
    ProcessInfo startCliServer() throws IOException, InterruptedException {
        if (spawn == null) {
            throw new IllegalStateException("Starting a CLI process requires a spawned runtime connection");
        }
        clearStderrBuffer();

        RuntimeLaunch launch = resolveCliLaunch();
        var args = new ArrayList<String>();

        if (spawn.args() != null) {
            args.addAll(spawn.args());
        }

        args.add("--server");
        args.add("--no-auto-update");
        if (options.getObservability().getLogLevel() != null) {
            args.add("--log-level");
            args.add(options.getObservability().getLogLevel());
        }

        if (spawn.stdio()) {
            args.add("--stdio");
        } else if (spawn.port() > 0) {
            args.add("--port");
            args.add(String.valueOf(spawn.port()));
        }

        // Add auth-related flags
        if (options.getGitHub().getToken() != null && !options.getGitHub().getToken().isEmpty()) {
            args.add("--auth-token-env");
            args.add("COPILOT_SDK_AUTH_TOKEN");
        }

        // Default UseLoggedInUser to false when GitHubToken is provided
        boolean useLoggedInUser = options.getGitHub().getUseLoggedInUser()
                .orElse(options.getGitHub().getToken() == null || options.getGitHub().getToken().isEmpty());
        if (!useLoggedInUser) {
            args.add("--no-auto-login");
        }

        if (RuntimeSettings.idleTimeout(runtimeConnection).isPresent()
                && RuntimeSettings.idleTimeout(runtimeConnection).getAsInt() > 0) {
            args.add("--session-idle-timeout");
            args.add(String.valueOf(RuntimeSettings.idleTimeout(runtimeConnection).getAsInt()));
        }

        if (options.isRemote()) {
            args.add("--remote");
        }

        List<String> command = resolveCliCommand(launch.executable(), args);

        var pb = new ProcessBuilder(command);
        pb.redirectErrorStream(false);

        // Note: On Windows, console window visibility depends on how the parent Java
        // process was launched. GUI applications started with 'javaw' will not create
        // visible console windows for subprocesses. Console applications started with
        // 'java' will share their console with subprocesses. Java's ProcessBuilder
        // doesn't provide explicit CREATE_NO_WINDOW flags like native Windows APIs,
        // but the default behavior is appropriate for most use cases.

        if (spawn.workingDirectory() != null) {
            pb.directory(new File(spawn.workingDirectory()));
        }

        configureProcessEnvironment(pb);

        Process process = pb.start();

        // Forward stderr to logger in background
        startStderrReader(process);

        try {
            Integer detectedPort = null;
            if (!spawn.stdio()) {
                detectedPort = waitForPortAnnouncement(process);
            }
            return new ProcessInfo(process, detectedPort);
        } catch (IOException | RuntimeException error) {
            try {
                cleanupCliProcess(process, captureDescendants(process), true);
            } catch (RuntimeException cleanupError) {
                error.addSuppressed(cleanupError);
            }
            throw error;
        }
    }

    /**
     * Connects to a running Copilot server.
     *
     * @param process
     *            the CLI process (null if connecting to external server)
     * @param tcpHost
     *            the host to connect to (null for stdio mode)
     * @param tcpPort
     *            the port to connect to (null for stdio mode)
     * @return the JSON-RPC client connected to the server
     * @throws IOException
     *             if connection fails
     */
    JsonRpcClient connectToServer(Process process, String tcpHost, Integer tcpPort) throws IOException {
        if (tcpHost != null && tcpPort != null) {
            // TCP mode: external server or child process with explicit port
            Socket socket = new Socket(tcpHost, tcpPort);
            return JsonRpcClient.fromSocket(socket);
        } else if (process != null) {
            // Stdio mode: child process
            return JsonRpcClient.fromProcess(process);
        } else {
            throw new IllegalStateException("Cannot connect: no process for stdio and no host:port for TCP");
        }
    }

    private void startStderrReader(Process process) {
        var thread = new Thread(() -> {
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getErrorStream(), StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    synchronized (stderrBuffer) {
                        stderrBuffer.append(line).append('\n');
                    }
                    LOG.fine("[CLI] " + line);
                }
            } catch (IOException e) {
                LOG.log(Level.FINE, "Error reading stderr", e);
            }
        }, "cli-stderr-reader");
        thread.setDaemon(true);
        thread.start();
        this.stderrThread = thread;
    }

    private Integer waitForPortAnnouncement(Process process) throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
            Pattern portPattern = Pattern.compile("listening on port (\\d+)", Pattern.CASE_INSENSITIVE);
            long deadline = System.currentTimeMillis() + 30000;

            while (System.currentTimeMillis() < deadline) {
                String line = reader.readLine();
                if (line == null) {
                    awaitStderrReader();
                    String stderr = getStderrOutput();
                    throw new IOException(formatCliExitedMessage("CLI process exited unexpectedly.", stderr));
                }

                Matcher matcher = portPattern.matcher(line);
                if (matcher.find()) {
                    return Integer.parseInt(matcher.group(1));
                }
            }

            throw new IOException("Timeout waiting for CLI to announce port");
        }
    }

    static void cleanupCliProcess(Process process, List<ProcessHandle> descendants, boolean forceImmediately) {
        if (!process.isAlive() && descendants.stream().noneMatch(ProcessHandle::isAlive)) {
            return;
        }
        try {
            if (terminateProcessTree(process, descendants, forceImmediately)) {
                return;
            }
            if (!forceImmediately) {
                if (terminateProcessTree(process, descendants, true)) {
                    return;
                }
            }
            throw new IllegalStateException("CLI process tree did not terminate after shutdown: " + process.pid());
        } catch (InterruptedException error) {
            Thread.currentThread().interrupt();
            throw new CompletionException("Interrupted while terminating the CLI process tree", error);
        } catch (ExecutionException error) {
            throw new CompletionException("Failed to await CLI process tree termination", error.getCause());
        }
    }

    static List<ProcessHandle> captureDescendants(Process process) {
        return deepestFirst(process.descendants());
    }

    /**
     * Both snapshots of a process tree, deepest first by current ancestry, so a
     * process that started after {@code retained} was captured still ends before
     * its parent.
     */
    static List<ProcessHandle> mergeDescendants(List<ProcessHandle> retained, List<ProcessHandle> captured) {
        if (retained.isEmpty()) {
            return captured;
        }
        return deepestFirst(Stream.concat(retained.stream(), captured.stream()).distinct());
    }

    private static List<ProcessHandle> deepestFirst(Stream<ProcessHandle> processes) {
        return processes.map(process -> Map.entry(process, processDepth(process)))
                .sorted(Map.Entry.<ProcessHandle, Integer>comparingByValue().reversed()).map(Map.Entry::getKey)
                .toList();
    }

    private static int processDepth(ProcessHandle process) {
        int depth = 0;
        var parent = process.parent();
        while (parent.isPresent()) {
            depth++;
            parent = parent.get().parent();
        }
        return depth;
    }

    private static boolean terminateProcessTree(Process process, List<ProcessHandle> descendants, boolean force)
            throws InterruptedException, ExecutionException {
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(FORCE_KILL_TIMEOUT_SECONDS);
        // Reap deepest children while their parents can still collect their exit.
        // Killing the launcher first can orphan zombies in containers without an init.
        for (var child : descendants) {
            if (child.isAlive()) {
                if (force) {
                    child.destroyForcibly();
                } else {
                    child.destroy();
                }
                waitForProcessExit(child, deadline);
            }
        }
        if (process.isAlive()) {
            if (force) {
                process.destroyForcibly();
            } else {
                process.destroy();
            }
        }
        return waitForProcessTreeExit(process, descendants, deadline);
    }

    private static boolean waitForProcessTreeExit(Process process, List<ProcessHandle> descendants, long deadline)
            throws InterruptedException, ExecutionException {
        if (!process.waitFor(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS)) {
            return false;
        }
        for (var child : descendants) {
            if (!waitForProcessExit(child, deadline)) {
                return false;
            }
        }
        return true;
    }

    private static boolean waitForProcessExit(ProcessHandle process, long deadline)
            throws InterruptedException, ExecutionException {
        if (!process.isAlive()) {
            return true;
        }
        try {
            process.onExit().get(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
            return true;
        } catch (TimeoutException error) {
            return false;
        }
    }

    String getStderrOutput() {
        synchronized (stderrBuffer) {
            return stderrBuffer.toString().trim();
        }
    }

    void awaitStderrReader() {
        Thread t = this.stderrThread;
        if (t != null) {
            try {
                t.join(STDERR_READER_JOIN_TIMEOUT_MS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private void clearStderrBuffer() {
        synchronized (stderrBuffer) {
            stderrBuffer.setLength(0);
        }
    }

    static String formatCliExitedMessage(String message, String stderrOutput) {
        if (stderrOutput == null || stderrOutput.isEmpty()) {
            return message;
        }
        return message + "\nstderr: " + stderrOutput;
    }

    private List<String> resolveCliCommand(String cliPath, List<String> args) throws IOException {
        boolean isJsFile = cliPath.toLowerCase().endsWith(".js");
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");

        if (isJsFile) {
            String node = windows ? resolveWindowsExecutable("node") : resolveUnixExecutable("node");
            if (node == null) {
                throw new IOException("Node.js was not found on the configured PATH");
            }
            var result = new ArrayList<String>();
            result.add(node);
            result.add(cliPath);
            result.addAll(args);
            return result;
        }

        // Own native executables directly so terminating the process cannot orphan
        // a child behind cmd.exe. Batch shims still require the shell.
        if (windows && !new File(cliPath).isAbsolute()) {
            String executable = resolveWindowsExecutable(cliPath);
            if (executable != null) {
                var result = new ArrayList<String>();
                result.add(executable);
                result.addAll(args);
                return result;
            }
            var result = new ArrayList<String>();
            result.add("cmd");
            result.add("/c");
            result.add(cliPath);
            result.addAll(args);
            return result;
        }

        var result = new ArrayList<String>();
        result.add(windows ? cliPath : resolveUnixExecutable(cliPath));
        result.addAll(args);
        return result;
    }

    private String resolveUnixExecutable(String command) throws IOException {
        if (command.contains("/")) {
            return command;
        }
        var environment = spawn.environment() != null ? spawn.environment() : System.getenv();
        Path cwd = Path.of(spawn.workingDirectory() != null ? spawn.workingDirectory() : System.getProperty("user.dir"))
                .toAbsolutePath();
        String path = environment.get("PATH");
        if (path != null) {
            for (String directory : path.split(File.pathSeparator, -1)) {
                Path candidate = cwd.resolve(directory).resolve(command);
                if (Files.isRegularFile(candidate) && Files.isExecutable(candidate)) {
                    return candidate.toAbsolutePath().toString();
                }
            }
        }
        throw new IOException("Executable was not found on the configured PATH: " + command);
    }

    private String resolveWindowsExecutable(String cliPath) {
        Path cwd = Path.of(spawn.workingDirectory() != null ? spawn.workingDirectory() : System.getProperty("user.dir"))
                .toAbsolutePath();
        var directories = new ArrayList<String>();
        var environment = spawn.environment() != null ? spawn.environment() : System.getenv();
        boolean hasDirectory = cliPath.contains("\\") || cliPath.contains("/");
        if (hasDirectory || environment.keySet().stream()
                .noneMatch(key -> key.equalsIgnoreCase("NoDefaultCurrentDirectoryInExePath"))) {
            directories.add(cwd.toString());
        }
        if (!hasDirectory) {
            String pathValue = environment.entrySet().stream().filter(entry -> entry.getKey().equalsIgnoreCase("PATH"))
                    .map(java.util.Map.Entry::getValue).findFirst().orElse("");
            directories.addAll(Arrays.asList(pathValue.split(File.pathSeparator)));
        }
        String pathExtensions = environment.entrySet().stream()
                .filter(entry -> entry.getKey().equalsIgnoreCase("PATHEXT")).map(java.util.Map.Entry::getValue)
                .findFirst().orElse(".COM;.EXE;.BAT;.CMD");
        String[] names = new File(cliPath).getName().contains(".")
                ? new String[]{cliPath}
                : Arrays.stream(pathExtensions.split(";")).map(extension -> cliPath + extension).toArray(String[]::new);
        // Preserve invalid-command diagnostics even when implicit cwd search is
        // disabled.
        if (names.length > 0) {
            Path.of(names[0]);
        }
        for (String directory : directories) {
            if (directory.isBlank()) {
                continue;
            }
            if (directory.startsWith("\"") && directory.endsWith("\"")) {
                directory = directory.substring(1, directory.length() - 1);
            }
            Path searchDirectory;
            try {
                searchDirectory = cwd.resolve(directory);
            } catch (InvalidPathException error) {
                LOG.log(Level.FINE, "Skipping invalid PATH entry while resolving CLI executable", error);
                continue;
            }
            for (String name : names) {
                Path candidate = searchDirectory.resolve(name);
                if (Files.isRegularFile(candidate)) {
                    String lowerName = name.toLowerCase(java.util.Locale.ROOT);
                    return lowerName.endsWith(".exe") || lowerName.endsWith(".com")
                            ? candidate.toAbsolutePath().toString()
                            : null;
                }
            }
        }
        return null;
    }

    void configureProcessEnvironment(ProcessBuilder pb) {
        var environment = normalizeEnvironment(spawn.environment() != null ? spawn.environment() : pb.environment());
        pb.environment().clear();
        pb.environment().putAll(environment);
        pb.environment().remove("NODE_DEBUG");
        if (options.getInternalTestOptions() != null) {
            pb.environment().putAll(options.getInternalTestOptions().toEnvironment());
        }
        if (options.getMode() != CopilotClientMode.EMPTY) {
            pb.environment().put("COPILOT_RUNTIME_PROCESS_FILE_LOGGING", "1");
        } else {
            pb.environment().put("COPILOT_DISABLE_KEYTAR", "1");
        }

        // Set auth token in environment if provided
        if (options.getGitHub().getToken() != null && !options.getGitHub().getToken().isEmpty()) {
            pb.environment().put("COPILOT_SDK_AUTH_TOKEN", options.getGitHub().getToken());
        }

        // Set Copilot home directory if configured
        if (RuntimeSettings.baseDirectory(runtimeConnection) != null
                && !RuntimeSettings.baseDirectory(runtimeConnection).isEmpty()) {
            pb.environment().put("COPILOT_HOME", RuntimeSettings.baseDirectory(runtimeConnection));
        }

        // Set connection token for TCP mode
        if (connectionToken != null && !connectionToken.isEmpty()) {
            pb.environment().put("COPILOT_CONNECTION_TOKEN", connectionToken);
        }

        // Set telemetry environment variables if configured
        if (options.getObservability().getTelemetry() != null) {
            pb.environment().putAll(options.getObservability().getTelemetry().toEnvironment());
        }
    }

    RuntimeLaunch resolveCliLaunch() throws IOException {
        return resolveCliLaunch(System.getenv(NativeRuntimeLoader.COPILOT_CLI_PATH_ENV));
    }

    RuntimeLaunch resolveCliLaunch(String inheritedCliPath) throws IOException {
        if (spawn.path() != null) {
            return new RuntimeLaunch(spawn.path());
        }

        var environment = spawn.environment();
        String envCliPath = environment != null
                ? environment.get(NativeRuntimeLoader.COPILOT_CLI_PATH_ENV)
                : inheritedCliPath;
        if (envCliPath != null && !envCliPath.isBlank()) {
            return new RuntimeLaunch(envCliPath);
        }

        Path wrapper = NativeRuntimeLoader.resolveRuntimeWrapper();
        return new RuntimeLaunch(wrapper.toString());
    }

    /**
     * Information about a started CLI server process.
     *
     * @param process
     *            the CLI process
     * @param port
     *            the detected TCP port (null for stdio mode)
     */
    record ProcessInfo(Process process, Integer port) {
    }

    record RuntimeLaunch(String executable) {
    }

    private static Map<String, String> normalizeEnvironment(Map<String, String> environment) {
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).startsWith("windows");
        var normalized = new LinkedHashMap<String, String>();
        environment.forEach((key, value) -> normalized.put(windows ? key.toUpperCase(Locale.ROOT) : key, value));
        return normalized;
    }

    private record SpawnOptions(String path, List<String> args, Map<String, String> environment,
            String workingDirectory, boolean stdio, int port) {
        private SpawnOptions {
            args = args == null ? null : new ArrayList<>(args);
            environment = environment == null ? null : normalizeEnvironment(environment);
        }
    }
}
