/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code plugins.policy} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerPluginsPolicyApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerPluginsPolicyApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Resolves plugin policy through the original connection's credential owner and retains the effective managed settings layers.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<PluginPolicyResolveInvocationResult> resolveForInvocation(PluginsPolicyResolveForInvocationParams params) {
        return caller.invoke("plugins.policy.resolveForInvocation", params, PluginPolicyResolveInvocationResult.class);
    }

}
