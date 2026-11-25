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
 * API methods for the {@code contextBoard} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerContextBoardApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerContextBoardApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Returns the runtime-owned instructions for a rem-agent that consolidates a session context board. Hosts that launch a consolidation agent use this prompt rather than copy runtime instructions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<ContextBoardGetConsolidationPromptResult> getConsolidationPrompt() {
        return caller.invoke("contextBoard.getConsolidationPrompt", java.util.Map.of(), ContextBoardGetConsolidationPromptResult.class);
    }

}
