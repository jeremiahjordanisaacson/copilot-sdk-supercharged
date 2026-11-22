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
 * API methods for the {@code account.deviceCodeLogin} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerAccountDeviceCodeLoginApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerAccountDeviceCodeLoginApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Starts device authorization through the original account owner without creating a session. The calling connection owns the flow; connection teardown cancels polling and pending persistence.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AuthLoginBegun> begin(AccountDeviceCodeLoginBeginParams params) {
        return caller.invoke("account.deviceCodeLogin.begin", params, AuthLoginBegun.class);
    }

    /**
     * Awaits device authorization or answers a previous plaintext-storage request. Never saves plaintext without a later explicit decision. Rejects another connection's flow and a changed original account owner.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AuthLoginStep> advance(AccountDeviceCodeLoginAdvanceParams params) {
        return caller.invoke("account.deviceCodeLogin.advance", params, AuthLoginStep.class);
    }

    /**
     * Opens the verification URL for a device authorization flow owned by this connection.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> openBrowser(AccountDeviceCodeLoginOpenBrowserParams params) {
        return caller.invoke("account.deviceCodeLogin.openBrowser", params, Void.class);
    }

    /**
     * Cancels this connection's device authorization flow and discards any pending plaintext-storage decision.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> cancel(AccountDeviceCodeLoginCancelParams params) {
        return caller.invoke("account.deviceCodeLogin.cancel", params, Void.class);
    }

}
