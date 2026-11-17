/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot.rpc;
/**
 * Settings for runtimes owned by this client.
 *
 * @param <T>
 *            concrete runtime transport
 */
public abstract sealed class OwnedRuntimeConnection<T extends OwnedRuntimeConnection<T>> extends RuntimeConnection
        permits ChildProcessRuntimeConnection, InProcessRuntimeConnection {
    private String baseDirectory;
    private Integer sessionIdleTimeoutSeconds;
    OwnedRuntimeConnection() {
    }
    /**
     * Returns the runtime data home.
     *
     * @return data home, or null for the runtime default
     */
    public String getBaseDirectory() {
        return baseDirectory;
    }
    /**
     * Sets the runtime data home, including the in-process cache.
     *
     * @param value
     *            data home
     * @return this connection
     */
    @SuppressWarnings("unchecked")
    public T setBaseDirectory(String value) {
        baseDirectory = value;
        return (T) this;
    }
    /**
     * Returns idle-session cleanup in seconds.
     *
     * @return timeout, or null to disable cleanup
     */
    public Integer getSessionIdleTimeoutSeconds() {
        return sessionIdleTimeoutSeconds;
    }
    /**
     * Sets idle-session cleanup. Zero or null disables cleanup.
     *
     * @param value
     *            timeout in seconds
     * @return this connection
     */
    @SuppressWarnings("unchecked")
    public T setSessionIdleTimeoutSeconds(Integer value) {
        sessionIdleTimeoutSeconds = value;
        return (T) this;
    }
}
