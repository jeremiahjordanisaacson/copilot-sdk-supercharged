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
 * API methods for the {@code plugins.sources} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerPluginsSourcesApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerPluginsSourcesApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Resolves persisted, explicit, and trusted live plugins with runtime-owned managed policy. Live records remain separate from persisted installations.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<PluginSourcesResolveResult> resolve(PluginsSourcesResolveParams params) {
        return caller.invoke("plugins.sources.resolve", params, PluginSourcesResolveResult.class);
    }

}
