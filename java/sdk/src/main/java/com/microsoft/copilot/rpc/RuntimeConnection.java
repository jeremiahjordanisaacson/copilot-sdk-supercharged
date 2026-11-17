/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

/**
 * Configures how a {@link com.microsoft.copilot.CopilotClient} connects to the
 * Copilot runtime.
 * <p>
 * Instances are created through the factory methods on this class and assigned
 * with {@link CopilotClientOptions#setRuntime(RuntimeConnection)}:
 *
 * <pre>{@code
 * // Host the native runtime in this process (the default).
 * new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(RuntimeConnection.forInProcess());
 *
 * // Spawn a runtime child process and talk over stdin/stdout.
 * new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT).setRuntime(RuntimeConnection.forSpawnedStdio());
 *
 * // Spawn a runtime child process listening on a TCP socket.
 * new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
 * 		.setRuntime(RuntimeConnection.forSpawnedTcp().setPath("/usr/local/bin/copilot"));
 *
 * // Connect to an already-running runtime.
 * new CopilotClientOptions().setMode(CopilotClientMode.CODING_AGENT)
 * 		.setRuntime(RuntimeConnection.forTcp("localhost", 3000));
 * }</pre>
 *
 * @since 1.0.0
 */
public abstract sealed class RuntimeConnection implements Cloneable
        permits OwnedRuntimeConnection, TcpRuntimeConnection {

    RuntimeConnection() {
    }

    /**
     * Copies this configuration without starting a runtime.
     *
     * @return the independent configuration
     */
    @Override
    public RuntimeConnection clone() {
        try {
            return (RuntimeConnection) super.clone();
        } catch (CloneNotSupportedException impossible) {
            throw new AssertionError(impossible);
        }
    }

    /**
     * Spawns a runtime child process and communicates over its stdin/stdout.
     *
     * @return a new stdio connection
     */
    public static SpawnedStdioRuntimeConnection forSpawnedStdio() {
        return new SpawnedStdioRuntimeConnection();
    }

    /**
     * Spawns a runtime child process at the given path and communicates over its
     * stdin/stdout.
     *
     * @param path
     *            path to the runtime executable, or {@code null} to use
     *            {@code COPILOT_CLI_PATH} or the bundled runtime wrapper
     * @return a new stdio connection
     */
    public static SpawnedStdioRuntimeConnection forSpawnedStdio(String path) {
        return new SpawnedStdioRuntimeConnection().setPath(path);
    }

    /**
     * Spawns a runtime child process that listens on a TCP socket and connects to
     * it.
     *
     * @return a new TCP connection
     */
    public static SpawnedTcpRuntimeConnection forSpawnedTcp() {
        return new SpawnedTcpRuntimeConnection();
    }

    /**
     * Connects to an already-running runtime over plaintext TCP.
     *
     * @param host
     *            hostname or IP address without a scheme, port, path, or IPv6
     *            brackets
     * @param port
     *            TCP port, from 1 through 65535
     * @return a new TCP connection that does not own the runtime's lifetime
     * @throws IllegalArgumentException
     *             if the host or port is invalid
     */
    public static TcpRuntimeConnection forTcp(String host, int port) {
        return new TcpRuntimeConnection(host, port);
    }

    /**
     * Hosts the runtime in-process by loading its native library and communicating
     * over the C ABI — no child process is spawned by the SDK for JSON-RPC
     * transport. This is the default when no runtime or
     * {@code COPILOT_SDK_DEFAULT_CONNECTION} environment override is supplied.
     *
     * @return a new in-process connection
     */
    public static InProcessRuntimeConnection forInProcess() {
        return new InProcessRuntimeConnection();
    }

    /**
     * Resolves the installed SDK default synchronously at call time without
     * starting a runtime. Honors COPILOT_SDK_DEFAULT_CONNECTION (stdio or
     * inprocess, case-insensitive). Empty or unset uses in-process hosting. Other
     * values throw.
     *
     * @return the selected owned-runtime connection
     */
    public static OwnedRuntimeConnection<?> forDefault() {
        return forDefault(null, null);
    }
    /**
     * Resolves the default with settings shared by both hosting modes.
     *
     * @param baseDirectory
     *            runtime data home
     * @param sessionIdleTimeoutSeconds
     *            cleanup timeout; zero or null disables cleanup
     * @return the selected owned-runtime connection
     */
    public static OwnedRuntimeConnection<?> forDefault(String baseDirectory, Integer sessionIdleTimeoutSeconds) {
        String value = System.getenv("COPILOT_SDK_DEFAULT_CONNECTION");
        OwnedRuntimeConnection<?> result;
        if ("stdio".equalsIgnoreCase(value))
            result = forSpawnedStdio();
        else if (value == null || value.isEmpty() || value.equalsIgnoreCase("inprocess"))
            result = forInProcess();
        else
            throw new IllegalArgumentException("Invalid COPILOT_SDK_DEFAULT_CONNECTION value '" + value
                    + "'. Expected 'inprocess', 'stdio', or unset.");
        return result.setBaseDirectory(baseDirectory).setSessionIdleTimeoutSeconds(sessionIdleTimeoutSeconds);
    }
}
