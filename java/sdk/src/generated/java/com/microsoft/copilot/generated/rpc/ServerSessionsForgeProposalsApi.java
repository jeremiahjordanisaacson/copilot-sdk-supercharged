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
 * API methods for the {@code sessions.forgeProposals} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class ServerSessionsForgeProposalsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerSessionsForgeProposalsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Reads a Forge proposal's before-generation capture from the canonical store owned by the configured runtime host. The request cannot select a database.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<ForgeProposalWorkspaceBeforeResult> workspaceBefore(SessionsForgeProposalsWorkspaceBeforeParams params) {
        return caller.invoke("sessions.forgeProposals.workspaceBefore", params, ForgeProposalWorkspaceBeforeResult.class);
    }

    /**
     * Reconciles a generated artifact with canonical Forge proposals. Reuses a ready or reviewing fingerprint, completes an eligible generation, or creates and completes an on-demand generation. Existing store transitions retain their transaction and persisted compatibility semantics. SDK skill-review clients can use the same canonical lifecycle without implementing another store.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<ForgeProposalReconcileResult> reconcile(SessionsForgeProposalsReconcileParams params) {
        return caller.invoke("sessions.forgeProposals.reconcile", params, ForgeProposalReconcileResult.class);
    }

    /**
     * Marks stale generating proposals failed in the configured canonical store. The runtime clock and existing store transition own the cutoff and persisted update.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    CompletableFuture<ForgeProposalFailStaleResult> failStale(SessionsForgeProposalsFailStaleParams params) {
        return caller.invoke("sessions.forgeProposals.failStale", params, ForgeProposalFailStaleResult.class);
    }

}
