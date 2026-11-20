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
 * API methods for the {@code ahp.connectionLogs} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerAhpConnectionLogsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerAhpConnectionLogsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Opens a redacted diagnostic log owned by this admitted SDK connection, before any relay authentication starts.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionLogRequest> open(AhpConnectionLogsOpenParams params) {
        return caller.invoke("ahp.connectionLogs.open", params, AhpConnectionLogRequest.class);
    }

    /**
     * Appends diagnostic text through the runtime's live secret filter to an owned connection log.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> write(AhpConnectionLogsWriteParams params) {
        return caller.invoke("ahp.connectionLogs.write", params, Void.class);
    }

    /**
     * Waits for an owned connection log's admitted writes to reach its file.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> flush(AhpConnectionLogsFlushParams params) {
        return caller.invoke("ahp.connectionLogs.flush", params, Void.class);
    }

    /**
     * Flushes and closes an owned connection log. The log lifetime is independent of relay authentication and retries.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> close(AhpConnectionLogsCloseParams params) {
        return caller.invoke("ahp.connectionLogs.close", params, Void.class);
    }

}
