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
 * API methods for the {@code acp} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerAcpApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerAcpApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Serves ACP through the original trusted SDK host's engine until its transport ends. Closes ACP sessions before completion. Refuses other peers and unrelated engines.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> serve(AcpServeParams params) {
        return caller.invoke("acp.serve", params, Void.class);
    }

    /**
     * Stops the original trusted SDK host's ACP transport and waits for its sessions to close.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> shutdown() {
        return caller.invoke("acp.shutdown", java.util.Map.of(), Void.class);
    }

}
