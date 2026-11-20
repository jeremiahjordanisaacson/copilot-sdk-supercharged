/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code ahp} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerAhpApi {

    private final RpcCaller caller;

    /** API methods for the {@code ahp.connections} sub-namespace. */
    final ServerAhpConnectionsApi connections;
    /** API methods for the {@code ahp.connectionLogs} sub-namespace. */
    final ServerAhpConnectionLogsApi connectionLogs;

    /** @param caller the RPC transport function */
    ServerAhpApi(RpcCaller caller) {
        this.caller = caller;
        this.connections = new ServerAhpConnectionsApi(caller);
        this.connectionLogs = new ServerAhpConnectionLogsApi(caller);
    }

}
