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
 * API methods for the {@code feedback.upload} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerFeedbackUploadApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerFeedbackUploadApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Starts the single saved desktop archive upload authorized at native host launch after fresh affirmative consent. Only its launch-bound owner may call this method; no file path, token, or destination is accepted over RPC.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<FeedbackUploadSnapshot> start(FeedbackUploadStartParams params) {
        return caller.invoke("feedback.upload.start", params, FeedbackUploadSnapshot.class);
    }

    /**
     * Reads connection-owned local upload progress, optionally waiting for a changed revision. Terminal outcomes return immediately. Canceling a read does not cancel the upload.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<FeedbackUploadSnapshot> read(FeedbackUploadReadParams params) {
        return caller.invoke("feedback.upload.read", params, FeedbackUploadSnapshot.class);
    }

    /**
     * Requests cancellation of the owner-bound upload. Local archive remains available; remote cancellation is best effort, not erasure.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<FeedbackUploadCancelResult> cancel(FeedbackUploadCancelParams params) {
        return caller.invoke("feedback.upload.cancel", params, FeedbackUploadCancelResult.class);
    }

}
