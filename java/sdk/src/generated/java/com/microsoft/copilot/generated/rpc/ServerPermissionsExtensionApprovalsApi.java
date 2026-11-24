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
 * API methods for the {@code permissions.extensionApprovals} namespace.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public final class ServerPermissionsExtensionApprovalsApi {

    private final RpcCaller caller;

    /** @param caller the RPC transport function */
    ServerPermissionsExtensionApprovalsApi(RpcCaller caller) {
        this.caller = caller;
    }

    /**
     * Persists an eligible live plugin extension permission request for the user before the host answers that request normally.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<PermissionsExtensionApprovalsGrantResult> grant(PermissionsExtensionApprovalsGrantParams params) {
        return caller.invoke("permissions.extensionApprovals.grant", params, PermissionsExtensionApprovalsGrantResult.class);
    }

    /**
     * Lists user-scoped plugin extension approvals across sessions.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<PermissionsExtensionApprovalsListResult> list(PermissionsExtensionApprovalsListParams params) {
        return caller.invoke("permissions.extensionApprovals.list", params, PermissionsExtensionApprovalsListResult.class);
    }

    /**
     * Revokes one user-scoped plugin extension approval by extension and installation identity.
     *
     * @apiNote This method is experimental and may change in a future version.
     * @since 1.0.0
     */
    @CopilotExperimental
    public CompletableFuture<PermissionsExtensionApprovalsRevokeResult> revoke(PermissionsExtensionApprovalsRevokeParams params) {
        return caller.invoke("permissions.extensionApprovals.revoke", params, PermissionsExtensionApprovalsRevokeResult.class);
    }

}
