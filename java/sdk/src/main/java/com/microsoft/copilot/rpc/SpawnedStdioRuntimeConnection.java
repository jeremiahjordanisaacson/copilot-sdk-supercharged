/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import java.util.ArrayList;
import java.util.List;

/**
 * Spawns a runtime child process and communicates over its stdin/stdout.
 * Construct with {@link RuntimeConnection#forSpawnedStdio()} or
 * {@link RuntimeConnection#forSpawnedStdio(String)}.
 *
 * @since 1.0.0
 */
public final class SpawnedStdioRuntimeConnection extends ChildProcessRuntimeConnection<SpawnedStdioRuntimeConnection> {

    private String path;
    private List<String> args;

    SpawnedStdioRuntimeConnection() {
    }

    /**
     * Returns the path to the runtime executable.
     *
     * @return the path, or {@code null} to use {@code COPILOT_CLI_PATH} or the
     *         bundled runtime wrapper
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the path to the runtime executable.
     *
     * @param path
     *            the path, or {@code null} to use {@code COPILOT_CLI_PATH} or the
     *            bundled runtime wrapper
     * @return this instance for method chaining
     */
    public SpawnedStdioRuntimeConnection setPath(String path) {
        this.path = path;
        return this;
    }

    /**
     * Returns the extra command-line arguments passed to the runtime process.
     *
     * @return the arguments, or {@code null} if none are configured
     */
    public List<String> getArgs() {
        return args;
    }

    /**
     * Sets extra command-line arguments prepended before SDK-managed flags.
     *
     * @param args
     *            the arguments, or {@code null} for none
     * @return this instance for method chaining
     */
    public SpawnedStdioRuntimeConnection setArgs(List<String> args) {
        this.args = args == null ? null : new ArrayList<>(args);
        return this;
    }

    @Override
    public SpawnedStdioRuntimeConnection clone() {
        return super.clone().setArgs(args);
    }
}
