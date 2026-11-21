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
 * API methods for the {@code memories} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerMemoriesApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerMemoriesApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Validates and imports semantic memory JSONL through original-owner credentials and scoped runtime sessions. Preserves account policy and conflicts. Dry runs perform no authentication, network calls, or writes.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<MemoriesImportSemanticResult> importSemantic(MemoriesImportSemanticParams params) {
        return caller.invoke("memories.importSemantic", params, MemoriesImportSemanticResult.class);
    }

    /**
     * Validates semantic memory JSONL and stores records under the server's account and repository policy. Dry runs submit no records.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<MemoriesImportResult> import_(MemoriesImportParams params) {
        return caller.invoke("memories.import", params, MemoriesImportResult.class);
    }

}
