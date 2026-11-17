/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
/**
 * Startup settings applicable only to child-process transports.
 *
 * @param <T>
 *            concrete child-process transport
 */
public abstract sealed class ChildProcessRuntimeConnection<T extends ChildProcessRuntimeConnection<T>>
        extends
            OwnedRuntimeConnection<T>
        permits SpawnedStdioRuntimeConnection, SpawnedTcpRuntimeConnection {
    private Map<String, String> environment;
    private String workingDirectory;
    ChildProcessRuntimeConnection() {
    }
    /**
     * Returns the executable path.
     *
     * @return path, or null for the bundled runtime
     */
    public abstract String getPath();
    /**
     * Returns extra command-line arguments.
     *
     * @return arguments, or null
     */
    public abstract List<String> getArgs();
    /**
     * Returns the replacement process environment.
     *
     * @return environment, or null to inherit
     */
    public Map<String, String> getEnvironment() {
        return environment == null ? null : new HashMap<>(environment);
    }
    /**
     * Replaces the inherited environment. A null map inherits; an empty map starts
     * with no inherited variables. SDK-managed variables are applied afterward.
     *
     * @param value
     *            replacement environment
     * @return this connection
     */
    @SuppressWarnings("unchecked")
    public T setEnvironment(Map<String, String> value) {
        environment = value == null ? null : new HashMap<>(value);
        return (T) this;
    }
    /**
     * Returns the process working directory.
     *
     * @return working directory, or null
     */
    public String getWorkingDirectory() {
        return workingDirectory;
    }
    /**
     * Sets the process working directory. Session working directories are
     * configured separately with
     * {@link CreateSessionOptions#setWorkingDirectory(String)}.
     *
     * @param value
     *            working directory
     * @return this connection
     */
    @SuppressWarnings("unchecked")
    public T setWorkingDirectory(String value) {
        workingDirectory = value;
        return (T) this;
    }

    @Override
    @SuppressWarnings("unchecked")
    public T clone() {
        T copy = (T) super.clone();
        copy.setEnvironment(environment);
        return copy;
    }
}
