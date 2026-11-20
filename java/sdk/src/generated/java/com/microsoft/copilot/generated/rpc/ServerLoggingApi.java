/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code logging} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerLoggingApi {

    private final RpcCaller caller;

    /** API methods for the {@code logging.file} sub-namespace. */
    public final ServerLoggingFileApi file;

    /** @param caller the RPC transport function */
    ServerLoggingApi(RpcCaller caller) {
        this.caller = caller;
        this.file = new ServerLoggingFileApi(caller);
    }

}
