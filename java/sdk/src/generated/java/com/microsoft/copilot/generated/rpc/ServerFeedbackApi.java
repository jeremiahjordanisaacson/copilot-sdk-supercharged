/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code feedback} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerFeedbackApi {

    private final RpcCaller caller;

    /** API methods for the {@code feedback.upload} sub-namespace. */
    final ServerFeedbackUploadApi upload;

    /** @param caller the RPC transport function */
    ServerFeedbackApi(RpcCaller caller) {
        this.caller = caller;
        this.upload = new ServerFeedbackUploadApi(caller);
    }

}
