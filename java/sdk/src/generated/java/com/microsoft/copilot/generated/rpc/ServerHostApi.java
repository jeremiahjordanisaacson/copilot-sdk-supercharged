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
 * API methods for the {@code host} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerHostApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerHostApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Publishes an attached resident session for this listener's lifetime without copying it.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<HostPublishSessionResult> publishSession(HostPublishSessionParams params) {
        return caller.invoke("host.publishSession", params, HostPublishSessionResult.class);
    }

    /**
     * Withdraws a live or dormant application publication from an owned live host and revokes AHP attachments without deleting local session history, worktrees, or ongoing work. Returns false when already absent; rejects nonapplication publications.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<HostUnpublishSessionResult> unpublishSession(HostUnpublishSessionParams params) {
        return caller.invoke("host.unpublishSession", params, HostUnpublishSessionResult.class);
    }

    /**
     * Reads the complete advertised live and dormant session catalog of an existing host owned by this connection. Does not start hosting, subscribe to sessions, or guarantee resumability.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<HostListSessionsResult> listSessions(HostListSessionsParams params) {
        return caller.invoke("host.listSessions", params, HostListSessionsResult.class);
    }

    /**
     * Requests app-owned materialization over the owning SDK participant.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<HostSessionCreateResult> createSession(HostCreateSessionParams params) {
        return caller.invoke("host.createSession", params, HostSessionCreateResult.class);
    }

    /**
     * Releases app ownership retention after AHP detaches.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> releaseSession(HostReleaseSessionParams params) {
        return caller.invoke("host.releaseSession", params, Void.class);
    }

    /**
     * Starts a connection-owned AHP host with explicit localServer and/or githubEnvironment transports as a supervised SDK participant.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<HostStartResult> start(HostStartParams params) {
        return caller.invoke("host.start", params, HostStartResult.class);
    }

    /**
     * Stops a listener owned by this SDK connection and joins its cleanup without deleting sessions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> dispose(HostDisposeParams params) {
        return caller.invoke("host.dispose", params, Void.class);
    }

    /**
     * Changes remote creation admission on a host owned by this SDK connection without disconnecting clients. Acknowledgement means subsequent admissions use the new policy; already-admitted creation may complete.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<Void> setSessionCreationPolicy(HostSetSessionCreationPolicyParams params) {
        return caller.invoke("host.setSessionCreationPolicy", params, Void.class);
    }

    /**
     * Resolves current authenticated credentials and remote-control policy only for the runtime-owned Mission Control hosting participant.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<HostEnvironmentCredentials> getEnvironmentCredentials() {
        return caller.invoke("host.getEnvironmentCredentials", java.util.Map.of(), HostEnvironmentCredentials.class);
    }

    /**
     * Returns listener settings only to the supervised hosting participant over its SDK connection.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<HostConfiguration> getConfiguration() {
        return caller.invoke("host.getConfiguration", java.util.Map.of(), HostConfiguration.class);
    }

    /**
     * Resolves the host account credential without applying Mission Control transport policy, only for its supervised GitHub hosting participant.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<HostEnvironmentCredentials> getAuthenticationCredentials() {
        return caller.invoke("host.getAuthenticationCredentials", java.util.Map.of(), HostEnvironmentCredentials.class);
    }

    /**
     * Validates a decrypted admission credential through the owning application, only for its supervised custom-authentication participant.
     *
     * @return a future that completes with the {@code HostAuthenticationResult} value,
     *     or {@code null} when the result is absent. Callers must handle the
     *     {@code null} completion value.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<HostAuthenticationResult> authenticate(HostAuthenticateParams params) {
        return caller.invoke("host.authenticate", params, HostAuthenticationResult.class);
    }

    /**
     * Cancels a supervised participant's pending admission validation.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> cancelAuthentication(HostCancelAuthenticationParams params) {
        return caller.invoke("host.cancelAuthentication", params, Void.class);
    }

    /**
     * Reports a supervised hosting participant's bound AHP endpoint after its SDK handshake.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<Void> ready(HostReadyParams params) {
        return caller.invoke("host.ready", params, Void.class);
    }

}
