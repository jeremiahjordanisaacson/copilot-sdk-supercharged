/* Copyright (c) Microsoft Corporation. All rights reserved. */
package com.microsoft.copilot;
import com.microsoft.copilot.rpc.OwnedRuntimeConnection;
import com.microsoft.copilot.rpc.RuntimeConnection;
/**
 * Internal runtime startup projection.
 *
 * @hidden
 */
public final class RuntimeSettings {
    private RuntimeSettings() {
    }
    /**
     * Projects the baseDirectory startup setting.
     *
     * @param r
     *            the selected runtime
     * @return the effective baseDirectory setting
     */
    public static String baseDirectory(RuntimeConnection r) {
        return r instanceof OwnedRuntimeConnection<?> c ? c.getBaseDirectory() : null;
    }
    /**
     * Projects the idleTimeout startup setting.
     *
     * @param r
     *            the selected runtime
     * @return the effective idleTimeout setting
     */
    public static java.util.OptionalInt idleTimeout(RuntimeConnection r) {
        return r instanceof OwnedRuntimeConnection<?> c && c.getSessionIdleTimeoutSeconds() != null
                ? java.util.OptionalInt.of(c.getSessionIdleTimeoutSeconds())
                : java.util.OptionalInt.empty();
    }
}
