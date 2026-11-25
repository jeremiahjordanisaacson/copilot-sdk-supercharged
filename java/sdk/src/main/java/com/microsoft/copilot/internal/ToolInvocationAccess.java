/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.internal;

import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Function;

import com.microsoft.copilot.rpc.ToolInvocation;

/**
 * SDK-internal access to {@link ToolInvocation} state that has no public
 * setter. This package is not exported from the module.
 */
public final class ToolInvocationAccess {

    private static volatile BiConsumer<ToolInvocation, Function<String, CompletableFuture<Void>>> progressReporterBinder;

    private ToolInvocationAccess() {
    }

    /**
     * Registers the binder. Called once from {@link ToolInvocation}'s static
     * initializer.
     *
     * @param binder
     *            sets an invocation's progress reporter
     */
    public static void register(BiConsumer<ToolInvocation, Function<String, CompletableFuture<Void>>> binder) {
        progressReporterBinder = binder;
    }

    /**
     * Binds the function that delivers an invocation's progress reports.
     *
     * @param invocation
     *            the invocation the SDK dispatches to a tool handler
     * @param reporter
     *            sends one progress message, or {@code null} to release it
     */
    public static void setProgressReporter(ToolInvocation invocation,
            Function<String, CompletableFuture<Void>> reporter) {
        // Creating the invocation ran ToolInvocation's static initializer.
        progressReporterBinder.accept(invocation, reporter);
    }
}
