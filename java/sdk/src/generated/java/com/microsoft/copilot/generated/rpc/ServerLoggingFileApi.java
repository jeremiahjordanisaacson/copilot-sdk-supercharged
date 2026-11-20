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
 * API methods for the {@code logging.file} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerLoggingFileApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerLoggingFileApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Opens the exact file target granted out of band to this SDK connection by its native host. Requests cannot select paths. The resource closes on disconnect.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<LoggingFileOpenResult> open(LoggingFileOpenParams params) {
        return caller.invoke("logging.file.open", params, LoggingFileOpenResult.class);
    }

    /**
     * Appends a redacted record through the runtime logger. Await writes in order before flushing or closing.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> write(LoggingFileWriteParams params) {
        return caller.invoke("logging.file.write", params, Void.class);
    }

    /**
     * Waits for preceding writes to reach the owned file and reports writer failures.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> flush(LoggingFileFlushParams params) {
        return caller.invoke("logging.file.flush", params, Void.class);
    }

    /**
     * Flushes and closes the owned file logger, reporting flush errors even when closing succeeds. The identifier cannot be reused.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> close(LoggingFileCloseParams params) {
        return caller.invoke("logging.file.close", params, Void.class);
    }

}
