/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import javax.annotation.processing.Generated;

/**
 * API methods for the {@code ahp.connections} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerAhpConnectionsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerAhpConnectionsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Creates a session at the remote AHP source and returns its refreshed catalog state. This does not create a local SDK attachment.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionCreateSessionResult> createSession(AhpConnectionsCreateSessionParams params) {
        return caller.invoke("ahp.connections.createSession", params, AhpConnectionCreateSessionResult.class);
    }

    /**
     * Selects a source catalog session without subscribing to its complete state. A null summary means no session matched.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionSelectSessionResult> selectSession(AhpConnectionsSelectSessionParams params) {
        return caller.invoke("ahp.connections.selectSession", params, AhpConnectionSelectSessionResult.class);
    }

    /**
     * Disposes a session at the remote source for every attached client. This is not a local SDK detach.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> disposeSession(AhpConnectionsDisposeSessionParams params) {
        return caller.invoke("ahp.connections.disposeSession", params, Void.class);
    }

    /**
     * Creates an inert outgoing AHP connection owned by this admitted SDK connection. Uses its existing original auth owner and never accepts a setup bearer.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionStateResult> create(AhpConnectionsCreateParams params) {
        return caller.invoke("ahp.connections.create", params, AhpConnectionStateResult.class);
    }

    /**
     * Attaches a runtime-owned SDK session facade to a session on the caller's outgoing AHP connection. Session state, history, approvals, and turn lifetimes stay in the runtime.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpSessionAttachResult> attachSession(AhpConnectionsAttachSessionParams params) {
        return caller.invoke("ahp.connections.attachSession", params, AhpSessionAttachResult.class);
    }

    /**
     * Releases the caller's AHP session facade, settles pending requests, and releases its subscription without disposing the host session.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> detachSession(AhpConnectionsDetachSessionParams params) {
        return caller.invoke("ahp.connections.detachSession", params, Void.class);
    }

    /**
     * Connects and initializes an owned outgoing AHP connection. Connection errors remain in the returned state.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionStateResult> connect(AhpConnectionsConnectParams params) {
        return caller.invoke("ahp.connections.connect", params, AhpConnectionStateResult.class);
    }

    /**
     * Reconnects the same outgoing client and restores its active subscriptions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionStateResult> reconnect(AhpConnectionsReconnectParams params) {
        return caller.invoke("ahp.connections.reconnect", params, AhpConnectionStateResult.class);
    }

    /**
     * Refreshes the outgoing connection's session catalog.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionStateResult> refresh(AhpConnectionsRefreshParams params) {
        return caller.invoke("ahp.connections.refresh", params, AhpConnectionStateResult.class);
    }

    /**
     * Authenticates pending protected resources with this connection's renewable account-bound credential. Reports failures per resource.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<List<AhpConnectionResourceAuthResult>> authenticate(AhpConnectionsAuthenticateParams params) {
        return caller.invoke("ahp.connections.authenticate", params, RpcMapper.INSTANCE.getTypeFactory().constructCollectionType(List.class, AhpConnectionResourceAuthResult.class));
    }

    /**
     * Waits for the next connection-owned event batch. Only one reader may wait at a time. Disposal releases the waiter.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<AhpConnectionReadEventsResult> readEvents(AhpConnectionsReadEventsParams params) {
        return caller.invoke("ahp.connections.readEvents", params, AhpConnectionReadEventsResult.class);
    }

    /**
     * Disposes an owned outgoing connection, cancels its protocol work, and releases its subscriptions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> dispose(AhpConnectionsDisposeParams params) {
        return caller.invoke("ahp.connections.dispose", params, Void.class);
    }

}
