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
 * API methods for the {@code remoteHosts} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerRemoteHostsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerRemoteHostsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Executes a closed native relay operation through an outgoing engine owned by this SDK connection. Does not accept arbitrary JSON-RPC methods or copy canonical session state.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostCommandResult> command(RemoteHostsCommandParams params) {
        return caller.invoke("remoteHosts.command", params, RemoteHostCommandResult.class);
    }

    /**
     * Allocates an inert outgoing AHP connection owned by this SDK connection. Direct credential sharing requires explicit permission; credentials never cross this interface.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostCreateResult> create(RemoteHostsCreateParams params) {
        return caller.invoke("remoteHosts.create", params, RemoteHostCreateResult.class);
    }

    /**
     * Connects an owned outgoing AHP engine using its original transport and renewable account credential.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostStateResult> connect(RemoteHostsConnectParams params) {
        return caller.invoke("remoteHosts.connect", params, RemoteHostStateResult.class);
    }

    /**
     * Reconnects the same outgoing AHP engine and restores its admitted subscriptions without changing account identity.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostStateResult> reconnect(RemoteHostsReconnectParams params) {
        return caller.invoke("remoteHosts.reconnect", params, RemoteHostStateResult.class);
    }

    /**
     * Reads the existing outgoing engine snapshot without network access.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostStateResult> getState(RemoteHostsGetStateParams params) {
        return caller.invoke("remoteHosts.getState", params, RemoteHostStateResult.class);
    }

    /**
     * Refreshes the owned remote host's session inventory without attaching to its sessions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostStateResult> listSessions(RemoteHostsListSessionsParams params) {
        return caller.invoke("remoteHosts.listSessions", params, RemoteHostStateResult.class);
    }

    /**
     * Cancels pending allocation and disposes an outgoing connection. Safe before create completes and after prior disposal.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> dispose(RemoteHostsDisposeParams params) {
        return caller.invoke("remoteHosts.dispose", params, Void.class);
    }

    /**
     * Answers advertised and explicitly requested protected resources with this connection's renewable GitHub credential. Reports independent outcomes.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostAuthenticateResourcesResult> authenticateResources(RemoteHostsAuthenticateResourcesParams params) {
        return caller.invoke("remoteHosts.authenticateResources", params, RemoteHostAuthenticateResourcesResult.class);
    }

    /**
     * Creates a remote session, including native project provisioning when requested, and returns the engine's existing post-creation refresh.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<RemoteHostCreateSessionResult> createSession(RemoteHostsCreateSessionParams params) {
        return caller.invoke("remoteHosts.createSession", params, RemoteHostCreateSessionResult.class);
    }

    /**
     * Deletes a session through its existing outgoing host engine without creating a local session model.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> deleteSession(RemoteHostsDeleteSessionParams params) {
        return caller.invoke("remoteHosts.deleteSession", params, Void.class);
    }

    /**
     * Retains a native refcounted remote session subscription and delivers projected actions to the owning SDK connection.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> subscribeSession(RemoteHostsSubscribeSessionParams params) {
        return caller.invoke("remoteHosts.subscribeSession", params, Void.class);
    }

    /**
     * Releases one native remote session subscription without deleting or stopping the remote session.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> unsubscribeSession(RemoteHostsUnsubscribeSessionParams params) {
        return caller.invoke("remoteHosts.unsubscribeSession", params, Void.class);
    }

}
