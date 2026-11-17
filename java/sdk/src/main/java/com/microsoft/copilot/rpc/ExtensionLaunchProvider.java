/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

package com.microsoft.copilot.rpc;

import java.util.concurrent.CompletableFuture;

import com.microsoft.copilot.CopilotExperimental;
import com.microsoft.copilot.generated.rpc.ExtensionLaunchProviderResolveRequest;
import com.microsoft.copilot.generated.rpc.ExtensionLaunchProviderResolveResult;

/**
 * Resolves launch profiles for extension entrypoints discovered by the runtime.
 *
 * @since 1.0.0
 */
@FunctionalInterface
@CopilotExperimental
public interface ExtensionLaunchProvider {

    /**
     * Resolves an optional launch profile for a discovered extension entrypoint.
     *
     * @param request
     *            the discovered extension entrypoint
     * @return a future containing the launch resolution
     */
    CompletableFuture<ExtensionLaunchProviderResolveResult> resolve(ExtensionLaunchProviderResolveRequest request);
}
